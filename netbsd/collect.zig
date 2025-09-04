//! Based on `tools/gen_stubs.zig` in ziglang/zig.

const builtin = @import("builtin");
const std = @import("std");

const arches = [_][]const u8{
    "alpha",
    "amd64",
    "evbarm64",
    "evbarmv7",
    "evbarmv7hf",
    "evbmips",
    "evbmips64",
    "evbmipsn64",
    "evbmipssf",
    "evbppc",
    "evbppcsf",
    "evbsh3",
    "evbsh3sf",
    "hppa",
    "i386",
    "mac68k",
    "mac68ksf",
    "sparc",
    "sparc64",
};

const Library = struct {
    name: []const u8,
    dir: []const u8,
    sover: u32,
};

const libs = [_]Library{
    .{ .name = "libm", .dir = "/usr/lib", .sover = 0 },
    .{ .name = "libpthread", .dir = "/usr/lib", .sover = 1 },
    .{ .name = "libc", .dir = "/usr/lib", .sover = 12 },
    .{ .name = "librt", .dir = "/usr/lib", .sover = 1 },
    .{ .name = "ld", .dir = "/libexec", .sover = 0 },
    .{ .name = "libutil", .dir = "/usr/lib", .sover = 7 },
    .{ .name = "libexecinfo", .dir = "/usr/lib", .sover = 0 },
};

const blacklist = [_][]const u8{
    "_GLOBAL_OFFSET_TABLE_",

    "__end__",
    "_end",

    "__bss_start__",
    "__bss_end__",
    "_bss_end__",

    "_init",
    "_fini",

    "___ctors",
    "___ctors_end",
    "__ctors",
    "__ctors_end",
    "___dtors",
    "___dtors_end",
    "__dtors",
    "__dtors_end",

    "__prof_data_sect_data",
    "__prof_cnts_sect_data",
    "__prof_nms_sect_data",

    "__curbrk",
    "curbrk",
    "__minbrk",

    "_membar_producer_end",
    "_membar_sync_end",

    "_atomic_cas_ras_start",
    "_atomic_cas_ras_end",
    "_atomic_cas_8_ras_start",
    "_atomic_cas_8_ras_end",
    "_atomic_cas_16_ras_start",
    "_atomic_cas_16_ras_end",

    "pthread__lock_ras_start",
    "pthread__lock_ras_end",

    "cerror",
};

fn fixType(name: []const u8, ty: u4) u4 {
    // NetBSD is sloppy about including `.type` directives for routines implemented in assembly.

    if (std.mem.eql(u8, name, "__mcount")) return std.elf.STT_FUNC;
    if (std.mem.eql(u8, name, "_mcount")) return std.elf.STT_FUNC;

    if (std.mem.eql(u8, name, "ffsl")) return std.elf.STT_FUNC;

    if (std.mem.eql(u8, name, "htonl")) return std.elf.STT_FUNC;
    if (std.mem.eql(u8, name, "htons")) return std.elf.STT_FUNC;
    if (std.mem.eql(u8, name, "ntohl")) return std.elf.STT_FUNC;
    if (std.mem.eql(u8, name, "ntohs")) return std.elf.STT_FUNC;

    if (std.mem.eql(u8, name, "index")) return std.elf.STT_FUNC;
    if (std.mem.eql(u8, name, "rindex")) return std.elf.STT_FUNC;

    return ty;
}

fn fixSize(name: []const u8, size: u64) u64 {
    _ = name;

    // If `fixType` returns `std.elf.STT_OBJECT` for a symbol, then this function needs to return
    // the size for that symbol.

    return size;
}

const Symbol = struct {
    name: []const u8,
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

const Parse = struct {
    arena: std.mem.Allocator,
    arch: []const u8,
    lib: []const u8,
    header: std.elf.Header,
    elf_bytes: []align(@alignOf(std.elf.Elf64_Ehdr)) u8,
    symbols: std.Arraylist(Symbol),
};

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);
    const netbsd_sysroot_path = args[1];
    const netbsd_version = try std.SemanticVersion.parse(try std.fmt.allocPrint(arena, "{s}.0", .{args[2]}));

    var sysroots_dir = try std.fs.cwd().openDir(netbsd_sysroot_path, .{});
    defer sysroots_dir.close();

    for (arches) |arch| {
        const abilist_dir_path = try std.fmt.allocPrint(arena, "{d}.{d}/{s}", .{
            netbsd_version.major,
            netbsd_version.minor,
            arch,
        });
        var dest_dir = try std.fs.cwd().makeOpenPath(abilist_dir_path, .{});
        defer dest_dir.close();

        for (libs) |lib| {
            const elf_bytes = try sysroots_dir.readFileAllocOptions(
                arena,
                try std.fmt.allocPrint(arena, "{s}{s}/{s}", .{
                    arch,
                    lib.dir,
                    if (std.mem.eql(u8, lib.name, "ld"))
                        "ld.elf_so"
                    else
                        try std.fmt.allocPrint(arena, "{s}.so.{d}", .{ lib.name, lib.sover }),
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

                    return switch (std.mem.order(u8, a.name, b.name)) {
                        .gt, .eq => false,
                        .lt => true,
                    };
                }
            }.lessThan);

            var buffer: [4096]u8 = undefined;
            var af = try dest_dir.atomicFile(try std.fmt.allocPrint(arena, "{s}.abilist", .{lib.name}), .{ .write_buffer = &buffer });
            defer af.deinit();

            const w = &af.file_writer.interface;

            for (parse.symbols.items) |sym| {
                try w.print(" {s} ", .{sym.name});

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

            try af.finish();
        }
    }
}

fn parseElf(parse: *Parse, comptime is_64: bool, comptime endian: std.builtin.Endian) !void {
    const Elf_Shdr = if (is_64) std.elf.Elf64_Shdr else std.elf.Elf32_Shdr;
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
    for (shdrs, 0..) |shdr, i| {
        const sh_name = std.mem.sliceTo(shstrtab[Fns.swap(shdr.sh_name)..], 0);

        if (std.mem.eql(u8, sh_name, ".dynamic")) dynamic_index = @as(u16, @intCast(i));
        if (std.mem.eql(u8, sh_name, ".dynsym")) dynsym_index = @as(u16, @intCast(i));
    }
    if (dynamic_index == 0) @panic("did not find the .dynamic section");
    if (dynsym_index == 0) @panic("did not find the .dynsym section");

    const dyn_syms_off = Fns.swap(shdrs[dynsym_index].sh_offset);
    const dyn_syms_size = Fns.swap(shdrs[dynsym_index].sh_size);
    const dyn_syms = std.mem.bytesAsSlice(Elf_Sym, parse.elf_bytes[dyn_syms_off..][0..dyn_syms_size]);

    const dynstr_offset = Fns.swap(shdrs[Fns.swap(shdrs[dynsym_index].sh_link)].sh_offset);
    const dynstr = parse.elf_bytes[dynstr_offset..];

    syms: for (dyn_syms) |sym| {
        const name = try parse.arena.dupe(u8, std.mem.sliceTo(dynstr[Fns.swap(sym.st_name)..], 0));
        const ty = fixType(name, @as(u4, @truncate(sym.st_info)));
        const binding = @as(u4, @truncate(sym.st_info >> 4));
        const visib = @as(std.elf.STV, @enumFromInt(@as(u3, @truncate(sym.st_other))));
        const size = fixSize(name, Fns.swap(sym.st_size));

        for (blacklist) |bl_name| {
            if (std.mem.eql(u8, name, bl_name)) continue :syms;
        }

        if (sym.st_shndx == std.elf.SHN_UNDEF) continue;

        switch (visib) {
            .DEFAULT => {},
            .INTERNAL, .HIDDEN, .PROTECTED => continue,
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

        try parse.symbols.append(parse.arena, .{
            .name = name,
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
