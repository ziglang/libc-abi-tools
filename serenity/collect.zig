const builtin = @import("builtin");
const std = @import("std");

const arches = [_][]const u8{
    "aarch64",
    "riscv64",
    "x86_64",
};

const blacklist = [_][]const u8{
    "_init",
    "_fini",
};

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
    header: std.elf.Header,
    elf_bytes: []align(@alignOf(std.elf.Elf64_Ehdr)) u8,
    symbols: std.Arraylist(Symbol),
};

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);
    const serenity_sysroot_path = args[1];

    var sysroots_dir = try std.fs.cwd().openDir(serenity_sysroot_path, .{});
    defer sysroots_dir.close();

    for (arches) |arch| {
        const abilist_dir_path = try std.fmt.allocPrint(arena, "master/{s}", .{arch});
        var dest_dir = try std.fs.cwd().makeOpenPath(abilist_dir_path, .{});
        defer dest_dir.close();

        const elf_bytes = try sysroots_dir.readFileAllocOptions(
            arena,
            try std.fmt.allocPrint(arena, "{s}/Root/usr/lib/libc.so", .{arch}),
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
        var af = try dest_dir.atomicFile("libc.abilist", .{ .write_buffer = &buffer });
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
        const ty = @as(u4, @truncate(sym.st_info));
        const binding = @as(u4, @truncate(sym.st_info >> 4));
        const visib = @as(std.elf.STV, @enumFromInt(@as(u3, @truncate(sym.st_other))));
        const size = Fns.swap(sym.st_size);

        // Skip C++ nonsense.
        if (std.mem.startsWith(u8, name, "_Z")) continue :syms;

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
                std.log.warn("{s} libc: skipping '{s}' due to it having binding '{d}'", .{
                    parse.arch, name, binding,
                });
                continue;
            },
        }

        switch (ty) {
            std.elf.STT_FUNC, std.elf.STT_GNU_IFUNC => {},
            std.elf.STT_OBJECT, std.elf.STT_TLS => if (size == 0) @panic("missing size for data symbol"),
            else => {
                std.log.warn("{s} libc: skipping '{s}' due to it having type '{d}'", .{
                    parse.arch, name, ty,
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
