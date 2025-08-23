//! Based on `tools/gen_stubs.zig` in ziglang/zig.

const builtin = @import("builtin");
const std = @import("std");

const arches = [_][]const u8{
    "aarch64",
    "amd64",
    "armv7",
    "i386",
    "powerpc",
    "powerpc64",
    "riscv64",
};

const Library = struct {
    name: []const u8,
    dir: []const u8,
    sover: u32,
};

/// `pthread` is only an alias for `thr`.
/// `xnet` is only an alias for `c`.
const libs = [_]Library{
    .{ .name = "libm", .dir = "/lib", .sover = 5 },
    .{ .name = "libstdthreads", .dir = "/usr/lib", .sover = 0 },
    .{ .name = "libthr", .dir = "/lib", .sover = 3 },
    .{ .name = "libc", .dir = "/lib", .sover = 7 },
    .{ .name = "libdl", .dir = "/usr/lib", .sover = 1 },
    .{ .name = "librt", .dir = "/lib", .sover = 1 },
    .{ .name = "ld", .dir = "/libexec", .sover = 1 },
    .{ .name = "libutil", .dir = "/lib", .sover = 9 },
    .{ .name = "libexecinfo", .dir = "/usr/lib", .sover = 1 },
};

const blacklist = [_][]const u8{
    "_init",
    "_fini",

    // RISC-V
    "__global_pointer$",
};

const Symbol = struct {
    name: []const u8,
    /// Null if the library isn't using symbol versioning, or for special unversioned symbols.
    version: ?[]const u8,
    kind: union(Tag) {
        func,
        object: u64,
        tls: u64,

        const Tag = enum(u8) {
            func = 'F',
            object = 'D',
            tls = 'T',
        };
    },
    weak: bool,
};

const SymbolVersion = union(enum) {
    private,
    normal: []const u8,
    future,
};

const Parse = struct {
    arena: std.mem.Allocator,
    version: std.SemanticVersion,
    arch: []const u8,
    lib: []const u8,
    header: std.elf.Header,
    elf_bytes: []align(@alignOf(std.elf.Elf64_Ehdr)) u8,
    symbols: std.ArrayList(Symbol),
};

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);
    const freebsd_sysroot_path = args[1];
    const fbsd_libc_version = try std.SemanticVersion.parse(try std.fmt.allocPrint(arena, "{s}.0", .{args[2]}));

    var sysroots_dir = try std.fs.cwd().openDir(freebsd_sysroot_path, .{});
    defer sysroots_dir.close();

    for (arches) |arch| {
        const abilist_dir_path = try std.fmt.allocPrint(arena, "{d}.{d}/{s}", .{
            fbsd_libc_version.major,
            fbsd_libc_version.minor,
            arch,
        });
        var dest_dir = try std.fs.cwd().makeOpenPath(abilist_dir_path, .{});
        defer dest_dir.close();

        for (libs) |lib| {
            const elf_bytes = try sysroots_dir.readFileAllocOptions(
                arena,
                try std.fmt.allocPrint(arena, "{s}{s}/{s}.so.{d}", .{
                    arch,
                    lib.dir,
                    if (std.mem.eql(u8, lib.name, "ld")) "ld-elf" else lib.name,
                    lib.sover,
                }),
                100 * 1024 * 1024,
                3 * 1024 * 1024,
                .of(std.elf.Elf64_Ehdr),
                null,
            );
            var reader: std.Io.Reader = .fixed(elf_bytes);
            const header = try std.elf.Header.read(&reader);

            var parse: Parse = .{
                .arena = arena,
                .version = fbsd_libc_version,
                .arch = arch,
                .lib = lib.name,
                .header = header,
                .elf_bytes = elf_bytes,
                .symbols = .{},
            };

            try if (header.is_64) switch (header.endian) {
                .big => parseElf(&parse, true, .big),
                .little => parseElf(&parse, true, .little),
            } else switch (header.endian) {
                .big => parseElf(&parse, false, .big),
                .little => parseElf(&parse, false, .little),
            };

            // Sort the symbols for deterministic output.
            std.mem.sort(Symbol, parse.symbols.items, {}, struct {
                pub fn lessThan(ctx: void, a: Symbol, b: Symbol) bool {
                    _ = ctx;

                    if (a.version == null) {
                        if (b.version != null) return true;

                        return switch (std.mem.order(u8, a.name, b.name)) {
                            .gt, .eq => false,
                            .lt => true,
                        };
                    } else if (b.version == null) return false;

                    return switch (std.mem.order(u8, a.version.?, b.version.?)) {
                        .gt => false,
                        .lt => true,
                        .eq => switch (std.mem.order(u8, a.name, b.name)) {
                            .gt, .eq => false,
                            .lt => true,
                        },
                    };
                }
            }.lessThan);

            var af = try dest_dir.atomicFile(try std.fmt.allocPrint(arena, "{s}.abilist", .{lib.name}), .{});
            defer af.deinit();

            var bw = std.io.bufferedWriter(af.file.writer());
            const w = bw.writer();

            for (parse.symbols.items) |sym| {
                try w.print("{s} {s} ", .{ sym.version orelse "", sym.name });

                if (sym.weak) {
                    try w.writeAll("W ");
                }

                switch (sym.kind) {
                    inline else => |size, tag| {
                        try w.writeByte(@intFromEnum(tag));

                        if (tag != .func) try w.print(" 0x{X}", .{size});
                    },
                }

                try w.writeByte('\n');
            }

            try bw.flush();
            try af.finish();
        }
    }
}

fn parseElf(parse: *Parse, comptime is_64: bool, comptime endian: std.builtin.Endian) !void {
    const Elf_Shdr = if (is_64) std.elf.Elf64_Shdr else std.elf.Elf32_Shdr;
    const Elf_Dyn = if (is_64) std.elf.Elf64_Dyn else std.elf.Elf32_Dyn;
    const Elf_Sym = if (is_64) std.elf.Elf64_Sym else std.elf.Elf32_Sym;

    const Fns = struct {
        fn swap(x: anytype) @TypeOf(x) {
            if (endian != comptime builtin.cpu.arch.endian()) {
                return @byteSwap(x);
            } else {
                return x;
            }
        }
    };

    const shdrs = std.mem.bytesAsSlice(Elf_Shdr, parse.elf_bytes[parse.header.shoff..])[0..parse.header.shnum];

    const shstrtab_offset = Fns.swap(shdrs[parse.header.shstrndx].sh_offset);
    const shstrtab = parse.elf_bytes[shstrtab_offset..];

    var dynamic_index: u16 = 0;
    var dynsym_index: u16 = 0;
    var gnu_version_index: u16 = 0;
    var gnu_version_d_index: u16 = 0;
    for (shdrs, 0..) |shdr, i| {
        const sh_name = std.mem.sliceTo(shstrtab[Fns.swap(shdr.sh_name)..], 0);

        if (std.mem.eql(u8, sh_name, ".dynamic")) dynamic_index = @as(u16, @intCast(i));
        if (std.mem.eql(u8, sh_name, ".dynsym")) dynsym_index = @as(u16, @intCast(i));
        if (std.mem.eql(u8, sh_name, ".gnu.version")) gnu_version_index = @as(u16, @intCast(i));
        if (std.mem.eql(u8, sh_name, ".gnu.version_d")) gnu_version_d_index = @as(u16, @intCast(i));
    }
    if (dynamic_index == 0) @panic("did not find the .dynamic section");
    if (dynsym_index == 0) @panic("did not find the .dynsym section");
    if (gnu_version_index == 0) @panic("did not find the .gnu.version section");

    const dynv_off = Fns.swap(shdrs[dynamic_index].sh_offset);
    const dynv_size = Fns.swap(shdrs[dynamic_index].sh_size);
    const dynv = std.mem.bytesAsSlice(Elf_Dyn, parse.elf_bytes[dynv_off..][0..dynv_size]);

    var verdef_num: u64 = 0;
    for (dynv) |dyn| {
        const tag = Fns.swap(dyn.d_tag);

        if (tag == std.elf.DT_NULL) break;
        if (tag == std.elf.DT_VERDEFNUM) verdef_num = Fns.swap(dyn.d_val);
    }

    const gnu_verstab_off = Fns.swap(shdrs[gnu_version_index].sh_offset);
    const gnu_verstab_size = Fns.swap(shdrs[gnu_version_index].sh_size);
    const gnu_verstab = std.mem.bytesAsSlice(std.elf.Half, parse.elf_bytes[gnu_verstab_off..][0..gnu_verstab_size]);

    var versions: std.AutoHashMap(u16, SymbolVersion) = .init(parse.arena);
    if (gnu_version_d_index != 0) {
        if (verdef_num == 0) @panic("unexpected zero DT_VERDEFNUM");

        const gnu_verdefs_off = Fns.swap(shdrs[gnu_version_d_index].sh_offset);
        const gnu_verdefs_size = Fns.swap(shdrs[gnu_version_d_index].sh_size);
        const gnu_verdefs_ptr: [*]u8 = @ptrCast(parse.elf_bytes[gnu_verdefs_off..][0..gnu_verdefs_size]);

        const gnu_verdef_strs_offset = Fns.swap(shdrs[Fns.swap(shdrs[gnu_version_d_index].sh_link)].sh_offset);
        const gnu_verdef_strs = parse.elf_bytes[gnu_verdef_strs_offset..];

        var next_verdef: [*]u8 = @ptrCast(gnu_verdefs_ptr);
        for (0..Fns.swap(shdrs[gnu_version_d_index].sh_info)) |_| {
            const cur_verdef: *align(1) std.elf.Verdef = @ptrCast(next_verdef);
            next_verdef += Fns.swap(cur_verdef.next);

            const ndx: std.elf.VER_NDX = @enumFromInt(Fns.swap(@intFromEnum(cur_verdef.ndx)));

            switch (ndx) {
                .LOCAL, .GLOBAL => continue,
                else => {},
            }

            if (Fns.swap(cur_verdef.cnt) != 1) @panic("encountered multiple verdaux entries");

            const verdaux: *align(1) std.elf.Verdaux = @ptrCast(@as([*]u8, @ptrCast(cur_verdef)) + Fns.swap(cur_verdef.aux));
            const name = try parse.arena.dupe(u8, std.mem.sliceTo(gnu_verdef_strs[Fns.swap(verdaux.name)..], 0));

            // https://wiki.freebsd.org/SymbolVersioning

            if (std.mem.eql(u8, name, "FBSDprivate_1.0")) {
                try versions.put(@intFromEnum(ndx), .private);
                continue;
            }

            // This can happen when the FreeBSD developers backport stuff to a previous release, especially if the
            // libraries being parsed were built from a point release such as 14.1.0 (= FBSD_1.7).
            if (std.mem.startsWith(u8, name, "FBSD_")) {
                const fbsd_ver = try std.SemanticVersion.parse(try std.fmt.allocPrint(parse.arena, "{s}.0", .{name["FBSD_".len..]}));
                if (fbsd_ver.order(parse.version) == .gt) {
                    try versions.put(@intFromEnum(ndx), .future);
                    continue;
                }
            }

            try versions.put(@intFromEnum(ndx), .{ .normal = name });
        }
    }

    const dyn_syms_off = Fns.swap(shdrs[dynsym_index].sh_offset);
    const dyn_syms_size = Fns.swap(shdrs[dynsym_index].sh_size);
    const dyn_syms = std.mem.bytesAsSlice(Elf_Sym, parse.elf_bytes[dyn_syms_off..][0..dyn_syms_size]);

    const dynstr_offset = Fns.swap(shdrs[Fns.swap(shdrs[dynsym_index].sh_link)].sh_offset);
    const dynstr = parse.elf_bytes[dynstr_offset..];

    syms: for (dyn_syms, 0..) |sym, i| {
        const name = try parse.arena.dupe(u8, std.mem.sliceTo(dynstr[Fns.swap(sym.st_name)..], 0));
        const ty = @as(u4, @truncate(sym.st_info));
        const binding = @as(u4, @truncate(sym.st_info >> 4));
        const visib = @as(std.elf.STV, @enumFromInt(@as(u2, @truncate(sym.st_other))));
        const size = Fns.swap(sym.st_size);
        const ver_ndx = Fns.swap(gnu_verstab[i]) & ~@as(u16, 1 << 15);

        for (blacklist) |bl_name| {
            if (std.mem.eql(u8, name, bl_name)) continue :syms;
        }

        if (sym.st_shndx == std.elf.SHN_UNDEF) continue;

        switch (visib) {
            .DEFAULT => {},
            .INTERNAL, .HIDDEN, .PROTECTED => continue,
        }

        switch (@as(std.elf.VER_NDX, @enumFromInt(ver_ndx))) {
            .LOCAL => continue,
            else => {},
        }

        switch (binding) {
            std.elf.STB_GLOBAL, std.elf.STB_WEAK => {},
            std.elf.STB_LOCAL => continue,
            else => {
                std.log.warn("{s} {s}: skipping '{s}' due to it having binding '{d}'", .{
                    parse.arch, parse.lib, name, binding,
                });
                continue;
            },
        }

        switch (ty) {
            std.elf.STT_FUNC, std.elf.STT_GNU_IFUNC => {},
            std.elf.STT_OBJECT, std.elf.STT_TLS => if (size == 0) @panic("missing size for data symbol"),
            else => {
                std.log.warn("{s} {s}: skipping '{s}' due to it having type '{d}'", .{
                    parse.arch, parse.lib, name, ty,
                });
                continue;
            },
        }

        const ver: ?[]const u8 = if (versions.get(ver_ndx)) |v| switch (v) {
            .private, .future => continue,
            .normal => |n| n,
        } else null;

        try parse.symbols.append(parse.arena, .{
            .name = name,
            .version = ver,
            .kind = switch (ty) {
                std.elf.STT_FUNC, std.elf.STT_GNU_IFUNC => .func,
                std.elf.STT_OBJECT => .{ .object = size },
                std.elf.STT_TLS => .{ .tls = size },
                else => unreachable,
            },
            .weak = binding == std.elf.STB_WEAK,
        });
    }
}
