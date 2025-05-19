const std = @import("std");
const Target = std.Target;
const Version = @import("Version.zig");
const mem = std.mem;
const log = std.log;
const fs = std.fs;
const fmt = std.fmt;
const assert = std.debug.assert;

// Example abilist path:
// ./sysdeps/unix/sysv/linux/aarch64/libc.abilist
const AbiList = struct {
    targets: []const ZigTarget,
    path: []const u8,
};
const ZigTarget = struct {
    arch: std.Target.Cpu.Arch,
    abi: std.Target.Abi,

    fn getIndex(zt: ZigTarget) u16 {
        for (zig_targets, 0..) |other, i| {
            if (zt.eql(other)) {
                return @intCast(i);
            }
        }
        unreachable;
    }

    fn eql(zt: ZigTarget, other: ZigTarget) bool {
        return zt.arch == other.arch and zt.abi == other.abi;
    }
};

const lib_names = [_][]const u8{
    "m",
    "pthread",
    "c",
    "dl",
    "rt",
    "ld",
    "util",
    "resolv",
};

/// This is organized by grouping together at the beginning,
/// targets most likely to share the same symbol information.
const zig_targets = [_]ZigTarget{
    // zig fmt: off
    .{ .arch = .arm        , .abi = .gnueabi },
    .{ .arch = .armeb      , .abi = .gnueabi },
    .{ .arch = .arm        , .abi = .gnueabihf },
    .{ .arch = .armeb      , .abi = .gnueabihf },
    .{ .arch = .arc        , .abi = .gnu },
    .{ .arch = .csky       , .abi = .gnueabi },
    .{ .arch = .csky       , .abi = .gnueabihf },
    .{ .arch = .m68k       , .abi = .gnu },
    .{ .arch = .mipsel     , .abi = .gnueabihf },
    .{ .arch = .mips       , .abi = .gnueabihf },
    .{ .arch = .mipsel     , .abi = .gnueabi },
    .{ .arch = .mips       , .abi = .gnueabi },
    .{ .arch = .x86        , .abi = .gnu },
    .{ .arch = .riscv32    , .abi = .gnu },
    .{ .arch = .sparc      , .abi = .gnu },
    .{ .arch = .powerpc    , .abi = .gnueabi },
    .{ .arch = .powerpc    , .abi = .gnueabihf },

    .{ .arch = .powerpc64le, .abi = .gnu },
    .{ .arch = .powerpc64  , .abi = .gnu },
    .{ .arch = .mips64el   , .abi = .gnuabi64 },
    .{ .arch = .mips64     , .abi = .gnuabi64 },
    .{ .arch = .mips64el   , .abi = .gnuabin32 },
    .{ .arch = .mips64     , .abi = .gnuabin32 },
    .{ .arch = .aarch64    , .abi = .gnu },
    .{ .arch = .aarch64_be , .abi = .gnu },
    .{ .arch = .x86_64     , .abi = .gnu },
    .{ .arch = .x86_64     , .abi = .gnux32 },
    .{ .arch = .riscv64    , .abi = .gnu },
    .{ .arch = .sparc64    , .abi = .gnu },
    .{ .arch = .loongarch64, .abi = .gnu },
    .{ .arch = .loongarch64, .abi = .gnusf },
    .{ .arch = .s390x      , .abi = .gnu },
    // zig fmt: on
};

comptime {
    assert(zig_targets.len <= @bitSizeOf(std.meta.FieldType(Inclusion, .targets)));
}

const versions = [_]Version{
    .{.major = 2, .minor = 0},
    .{.major = 2, .minor = 1},
    .{.major = 2, .minor = 1, .patch = 1},
    .{.major = 2, .minor = 1, .patch = 2},
    .{.major = 2, .minor = 1, .patch = 3},
    .{.major = 2, .minor = 2},
    .{.major = 2, .minor = 2, .patch = 1},
    .{.major = 2, .minor = 2, .patch = 2},
    .{.major = 2, .minor = 2, .patch = 3},
    .{.major = 2, .minor = 2, .patch = 4},
    .{.major = 2, .minor = 2, .patch = 5},
    .{.major = 2, .minor = 2, .patch = 6},
    .{.major = 2, .minor = 3},
    .{.major = 2, .minor = 3, .patch = 2},
    .{.major = 2, .minor = 3, .patch = 3},
    .{.major = 2, .minor = 3, .patch = 4},
    .{.major = 2, .minor = 4},
    .{.major = 2, .minor = 5},
    .{.major = 2, .minor = 6},
    .{.major = 2, .minor = 7},
    .{.major = 2, .minor = 8},
    .{.major = 2, .minor = 9},
    .{.major = 2, .minor = 10},
    .{.major = 2, .minor = 11},
    .{.major = 2, .minor = 12},
    .{.major = 2, .minor = 13},
    .{.major = 2, .minor = 14},
    .{.major = 2, .minor = 15},
    .{.major = 2, .minor = 16},
    .{.major = 2, .minor = 17},
    .{.major = 2, .minor = 18},
    .{.major = 2, .minor = 19},
    .{.major = 2, .minor = 20},
    .{.major = 2, .minor = 21},
    .{.major = 2, .minor = 22},
    .{.major = 2, .minor = 23},
    .{.major = 2, .minor = 24},
    .{.major = 2, .minor = 25},
    .{.major = 2, .minor = 26},
    .{.major = 2, .minor = 27},
    .{.major = 2, .minor = 28},
    .{.major = 2, .minor = 29},
    .{.major = 2, .minor = 30},
    .{.major = 2, .minor = 31},
    .{.major = 2, .minor = 32},
    .{.major = 2, .minor = 33},
    .{.major = 2, .minor = 34},
    .{.major = 2, .minor = 35},
    .{.major = 2, .minor = 36},
    .{.major = 2, .minor = 37},
    .{.major = 2, .minor = 38},
    .{.major = 2, .minor = 39},
    .{.major = 2, .minor = 40},
    .{.major = 2, .minor = 41},
};

comptime {
    assert(versions.len <= @bitSizeOf(std.meta.FieldType(Inclusion, .versions)));
}

// fpu/nofpu are hardcoded elsewhere, based on .gnueabi/.gnueabihf with an exception for .arm
// n64/n32 are hardcoded elsewhere, based on .gnuabi64/.gnuabin32
const abi_lists = [_]AbiList{
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .aarch64, .abi = .gnu },
            ZigTarget{ .arch = .aarch64_be, .abi = .gnu },
        },
        .path = "aarch64",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .s390x, .abi = .gnu }},
        .path = "s390/s390-64",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .arm, .abi = .gnueabi },
            ZigTarget{ .arch = .armeb, .abi = .gnueabi },
            ZigTarget{ .arch = .arm, .abi = .gnueabihf },
            ZigTarget{ .arch = .armeb, .abi = .gnueabihf },
        },
        .path = "arm",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .arc, .abi = .gnu }},
        .path = "arc",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .csky, .abi = .gnueabi },
            ZigTarget{ .arch = .csky, .abi = .gnueabihf },
        },
        .path = "csky",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .sparc, .abi = .gnu },
        },
        .path = "sparc/sparc32",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .sparc64, .abi = .gnu }},
        .path = "sparc/sparc64",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .m68k, .abi = .gnu }},
        .path = "m68k/m680x0",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .mips64el, .abi = .gnuabi64 },
            ZigTarget{ .arch = .mips64, .abi = .gnuabi64 },
        },
        .path = "mips/mips64",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .mips64el, .abi = .gnuabin32 },
            ZigTarget{ .arch = .mips64, .abi = .gnuabin32 },
        },
        .path = "mips/mips64",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .mipsel, .abi = .gnueabihf },
            ZigTarget{ .arch = .mips, .abi = .gnueabihf },
        },
        .path = "mips/mips32",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .mipsel, .abi = .gnueabi },
            ZigTarget{ .arch = .mips, .abi = .gnueabi },
        },
        .path = "mips/mips32",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .x86_64, .abi = .gnu }},
        .path = "x86_64/64",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .x86_64, .abi = .gnux32 }},
        .path = "x86_64/x32",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .x86, .abi = .gnu }},
        .path = "i386",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .powerpc64le, .abi = .gnu }},
        .path = "powerpc/powerpc64",
    },
    AbiList{
        .targets = &[_]ZigTarget{ZigTarget{ .arch = .powerpc64, .abi = .gnu }},
        .path = "powerpc/powerpc64",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .powerpc, .abi = .gnueabi },
            ZigTarget{ .arch = .powerpc, .abi = .gnueabihf },
        },
        .path = "powerpc/powerpc32",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .riscv32, .abi = .gnu },
        },
        .path = "riscv/rv32",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .riscv64, .abi = .gnu },
        },
        .path = "riscv/rv64",
    },
    AbiList{
        .targets = &[_]ZigTarget{
            ZigTarget{ .arch = .loongarch64, .abi = .gnu },
            ZigTarget{ .arch = .loongarch64, .abi = .gnusf },
        },
        .path = "loongarch/lp64",
    },
};

/// This is the first version that has loongarch support.
const ver36 = Version{
    .major = 2,
    .minor = 36,
};

/// After glibc 2.33, mips64 put some files inside n64 and n32 directories.
/// This is also the first version that has riscv32 support.
const ver33 = Version{
    .major = 2,
    .minor = 33,
};

/// This is the first version that has arc support.
const ver32 = Version{
    .major = 2,
    .minor = 32,
};

/// glibc 2.31 added sysdeps/unix/sysv/linux/arm/le and sysdeps/unix/sysv/linux/arm/be
/// Before these directories did not exist.
const ver30 = Version{
    .major = 2,
    .minor = 30,
};

/// This is the first version that has csky support.
const ver29 = Version{
    .major = 2,
    .minor = 29,
};

/// Similarly, powerpc64 le and be were introduced in glibc 2.29
const ver28 = Version{
    .major = 2,
    .minor = 28,
};

/// This is the first version that has riscv64 support.
const ver27 = Version{
    .major = 2,
    .minor = 27,
};

/// Before this version the abilist files had a different structure.
const ver23 = Version{
    .major = 2,
    .minor = 23,
};

const Symbol = struct {
    type: [lib_names.len][zig_targets.len][versions.len]Type = empty_type,
    is_fn: bool = undefined,

    const empty_row = [1]Type{.absent} ** versions.len;
    const empty_row2 = [1]@TypeOf(empty_row){empty_row} ** zig_targets.len;
    const empty_type = [1]@TypeOf(empty_row2){empty_row2} ** lib_names.len;

    const Type = union(enum) {
        absent,
        function,
        object: u16,

        fn eql(ty: Type, other: Type) bool {
            return switch (ty) {
                .absent => unreachable,
                .function => other == .function,
                .object => |ty_size| switch (other) {
                    .absent => unreachable,
                    .function => false,
                    .object => |other_size| ty_size == other_size,
                },
            };
        }
    };

    /// Return true if and only if the inclusion has no false positives.
    fn testInclusion(symbol: Symbol, inc: Inclusion, lib_i: u8) bool {
        for (symbol.type[lib_i], 0..) |versions_row, targets_i| {
            for (versions_row, 0..) |ty, versions_i| {
                if ((inc.targets & (@as(u64, 1) << @intCast(targets_i))) == 0 or
                    (inc.versions & (@as(u64, 1) << @intCast(versions_i))) == 0)
                    continue;

                switch (ty) {
                    .absent => return false,
                    .function => {},
                    .object => |size| if (inc.size != size) return false,
                }
            }
        }

        return true;
    }
};

const Inclusion = struct {
    versions: u64,
    targets: u64,
    lib: u8,
    size: u16,
};

const NamedInclusion = struct {
    name: []const u8,
    inc: Inclusion,
};

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const arena = arena_instance.allocator();

    var glibc_dir = try fs.cwd().openDir(".", .{.iterate = true});
    defer glibc_dir.close();

    const fs_versions = v: {
        var fs_versions = std.ArrayList(Version).init(arena);

        var version_dir_it = glibc_dir.iterate();
        while (try version_dir_it.next()) |entry| {
            if (entry.kind != .directory) continue;
            try fs_versions.append(try Version.parse(entry.name));
        }

        break :v fs_versions.items;
    };
    std.mem.sort(Version, fs_versions, {}, versionAscending);

    var symbols = std.StringHashMap(Symbol).init(arena);

    // Before this version the abilist files had a different structure.
    const first_fs_ver = Version{
        .major = 2,
        .minor = 23,
    };

    for (fs_versions) |fs_ver| {
        if (fs_ver.order(first_fs_ver) == .lt) {
            log.warn("skipping glibc version {} because the abilist files have a different format", .{fs_ver});
            continue;
        }
        log.info("scanning abilist files for glibc version: {}", .{fs_ver});

        const prefix = try fmt.allocPrint(arena, "{d}.{d}/sysdeps/unix/sysv/linux", .{
            fs_ver.major, fs_ver.minor,
        });
        for (&abi_lists) |*abi_list| {
            if (abi_list.targets[0].arch == .riscv64 and fs_ver.order(ver27) == .lt) {
                continue;
            }
            if (abi_list.targets[0].arch == .csky and fs_ver.order(ver29) == .lt) {
                continue;
            }
            if (abi_list.targets[0].arch == .arc and fs_ver.order(ver32) == .lt) {
                continue;
            }
            if (abi_list.targets[0].arch == .riscv32 and fs_ver.order(ver33) == .lt) {
                continue;
            }
            if (abi_list.targets[0].arch == .loongarch64 and fs_ver.order(ver36) == .lt) {
                continue;
            }

            for (lib_names, 0..) |lib_name, lib_i| {
                const lib_prefix = if (std.mem.eql(u8, lib_name, "ld")) "" else "lib";
                const basename = try fmt.allocPrint(arena, "{s}{s}.abilist", .{ lib_prefix, lib_name });
                const abi_list_filename = blk: {
                    const is_c = std.mem.eql(u8, lib_name, "c");
                    const is_dl = std.mem.eql(u8, lib_name, "dl");
                    const is_m = std.mem.eql(u8, lib_name, "m");
                    const is_ld = std.mem.eql(u8, lib_name, "ld");
                    const is_rt = std.mem.eql(u8, lib_name, "rt");
                    const is_resolv = std.mem.eql(u8, lib_name, "resolv");
                    const is_util = std.mem.eql(u8, lib_name, "util");

                    if ((abi_list.targets[0].arch == .mips64 or
                        abi_list.targets[0].arch == .mips64el) and
                        fs_ver.order(ver33) == .gt and (is_rt or is_c or is_ld))
                    {
                        if (abi_list.targets[0].abi == .gnuabi64) {
                            break :blk try fs.path.join(arena, &.{
                                prefix, abi_list.path, "n64", basename,
                            });
                        } else if (abi_list.targets[0].abi == .gnuabin32) {
                            break :blk try fs.path.join(arena, &.{
                                prefix, abi_list.path, "n32", basename,
                            });
                        } else {
                            unreachable;
                        }
                    } else if (abi_list.targets[0].abi == .gnuabi64 and (is_c or is_ld or is_resolv)) {
                        break :blk try fs.path.join(arena, &.{
                            prefix, abi_list.path, "n64", basename,
                        });
                    } else if (abi_list.targets[0].abi == .gnuabin32 and (is_c or is_ld or is_resolv)) {
                        break :blk try fs.path.join(arena, &.{
                            prefix, abi_list.path, "n32", basename,
                        });
                    } else if ((abi_list.targets[0].arch != .arm and abi_list.targets[0].arch != .csky) and
                        abi_list.targets[0].abi == .gnueabihf and
                        (is_c or (is_m and abi_list.targets[0].arch == .powerpc)))
                    {
                        break :blk try fs.path.join(arena, &.{
                            prefix, abi_list.path, "fpu", basename,
                        });
                    } else if ((abi_list.targets[0].arch != .arm and abi_list.targets[0].arch != .csky) and
                        abi_list.targets[0].abi == .gnueabi and
                        (is_c or (is_m and abi_list.targets[0].arch == .powerpc)))
                    {
                        break :blk try fs.path.join(arena, &.{
                            prefix, abi_list.path, "nofpu", basename,
                        });
                    } else if ((abi_list.targets[0].arch == .armeb or
                        abi_list.targets[0].arch == .arm) and fs_ver.order(ver30) == .gt)
                    {
                        const endian_suffix = switch (abi_list.targets[0].arch) {
                            .armeb => "be",
                            else => "le",
                        };
                        break :blk try fs.path.join(arena, &.{
                            prefix, abi_list.path, endian_suffix, basename,
                        });
                    } else if ((abi_list.targets[0].arch == .powerpc64le or
                        abi_list.targets[0].arch == .powerpc64))
                    {
                        if (fs_ver.order(ver28) == .gt) {
                            const endian_suffix = switch (abi_list.targets[0].arch) {
                                .powerpc64le => "le",
                                else => "be",
                            };
                            break :blk try fs.path.join(arena, &.{
                                prefix, abi_list.path, endian_suffix, basename,
                            });
                        }
                        // 2.28 and earlier, the files looked like this:
                        // libc.abilist
                        // libc-le.abilist
                        const endian_suffix = switch (abi_list.targets[0].arch) {
                            .powerpc64le => "-le",
                            else => "",
                        };
                        break :blk try fmt.allocPrint(arena, "{s}/{s}/{s}{s}{s}.abilist", .{
                            prefix, abi_list.path, lib_prefix, lib_name, endian_suffix,
                        });
                    } else if (abi_list.targets[0].arch == .loongarch64 and (is_dl or is_util)) {
                        continue;
                    }

                    break :blk try fs.path.join(arena, &.{ prefix, abi_list.path, basename });
                };

                const max_bytes = 10 * 1024 * 1024;
                const contents = glibc_dir.readFileAlloc(arena, abi_list_filename, max_bytes) catch |err| {
                    fatal("unable to open {s}: {}", .{ abi_list_filename, err });
                };
                var lines_it = std.mem.tokenizeScalar(u8, contents, '\n');
                while (lines_it.next()) |line| {
                    var tok_it = std.mem.tokenizeScalar(u8, line, ' ');
                    const ver_text = tok_it.next().?;
                    if (mem.startsWith(u8, ver_text, "GCC_")) continue;
                    if (mem.startsWith(u8, ver_text, "_gp_disp")) continue;
                    if (!mem.startsWith(u8, ver_text, "GLIBC_")) {
                        fatal("line did not start with 'GLIBC_': '{s}'", .{line});
                    }
                    const ver = try Version.parse(ver_text["GLIBC_".len..]);
                    const name = tok_it.next() orelse {
                        fatal("symbol name not found in {s} on line '{s}'", .{
                            abi_list_filename, line,
                        });
                    };
                    const category = tok_it.next().?;
                    const ty: Symbol.Type = if (mem.eql(u8, category, "F"))
                        .{ .function = {} }
                    else if (mem.eql(u8, category, "D"))
                        .{ .object = try fmt.parseInt(u16, tok_it.next().?, 0) }
                    else if (mem.eql(u8, category, "A"))
                        continue
                    else
                        fatal("unrecognized symbol type '{s}' on line '{s}'", .{category, line});

                    // Detect incorrect information when a symbol migrates from one library
                    // to another.
                    if (ver.order(fs_ver) == .lt and fs_ver.order(first_fs_ver) != .eq) {
                        // This abilist is claiming that this version is found in this
                        // library. However if that was true, we would have already
                        // noted it in the previous set of abilists.
                        continue;
                    }

                    const gop = try symbols.getOrPut(name);
                    if (!gop.found_existing) {
                        gop.value_ptr.* = .{};
                    }
                    for (abi_list.targets) |t| {
                        gop.value_ptr.type[lib_i][t.getIndex()][verIndex(ver)] = ty;
                    }
                }
            }
        }
    }

    // Our data format depends on the type of a symbol being consistently a function or an object
    // and not switching depending on target or version. Here we verify that premise.
    {
        var it = symbols.iterator();
        while (it.next()) |entry| {
            const name = entry.key_ptr.*;
            var prev_ty: @typeInfo(Symbol.Type).@"union".tag_type.? = .absent;
            for (entry.value_ptr.type) |targets_row| {
                for (targets_row) |versions_row| {
                    for (versions_row) |ty| {
                        switch (ty) {
                            .absent => continue,
                            .function => switch (prev_ty) {
                                .absent => prev_ty = ty,
                                .function => continue,
                                .object => fatal("symbol {s} switches types", .{name}),
                            },
                            .object => switch (prev_ty) {
                                .absent => prev_ty = ty,
                                .function => fatal("symbol {s} switches types", .{name}),
                                .object => continue,
                            },
                        }
                    }
                }
            }
            entry.value_ptr.is_fn = switch (prev_ty) {
                .absent => unreachable,
                .function => true,
                .object => false,
            };
        }
        log.info("confirmed that every symbol is consistently either an object or a function", .{});
    }

    // Now we have all the data and we want to emit the fewest number of inclusions as possible.
    // The first split is functions vs objects.
    // For functions, the only type possibilities are `absent` or `function`.
    // We use a greedy algorithm, "spreading" the inclusion from a single point to
    // as many targets as possible, then to as many versions as possible.
    var fn_inclusions = std.ArrayList(NamedInclusion).init(arena);
    var fn_count: usize = 0;
    var fn_target_popcount: usize = 0;
    var fn_version_popcount: usize = 0;
    const none_handled = blk: {
        const empty_row = [1]bool{false} ** versions.len;
        const empty_row2 = [1]@TypeOf(empty_row){empty_row} ** zig_targets.len;
        const empty_row3 = [1]@TypeOf(empty_row2){empty_row2} ** lib_names.len;
        break :blk empty_row3;
    };
    {
        var it = symbols.iterator();
        while (it.next()) |entry| {
            if (!entry.value_ptr.is_fn) continue;
            fn_count += 1;

            // Find missing inclusions. We can't move on from this symbol until
            // all the present symbols have been handled.
            var handled = none_handled;
            var libs_handled = [1]bool{false} ** lib_names.len;
            var lib_i: u8 = 0;
            while (lib_i < lib_names.len) {
                if (libs_handled[lib_i]) {
                    lib_i += 1;
                    continue;
                }
                const targets_row = entry.value_ptr.type[lib_i];

                var wanted_targets: u64 = 0;
                var wanted_versions_multi = [1]u64{0} ** zig_targets.len;

                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |ty, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;

                        switch (ty) {
                            .absent => continue,
                            .function => {
                                wanted_targets |= @as(u64, 1) << @intCast(targets_i);
                                wanted_versions_multi[targets_i] |=
                                    @as(u64, 1) << @intCast(versions_i);
                            },
                            .object => unreachable,
                        }
                    }
                }
                if (wanted_targets == 0) {
                    // This library is done.
                    libs_handled[lib_i] = true;
                    continue;
                }

                // Put one target and one version into the inclusion.
                const first_targ_index = @ctz(wanted_targets);
                var wanted_versions = wanted_versions_multi[first_targ_index];
                const first_ver_index = @ctz(wanted_versions);
                var inc: Inclusion = .{
                    .versions = @as(u64, 1) << @intCast(first_ver_index),
                    .targets = @as(u64, 1) << @intCast(first_targ_index),
                    .lib = @intCast(lib_i),
                    .size = 0,
                };
                wanted_targets &= ~(@as(u64, 1) << @intCast(first_targ_index));
                wanted_versions &= ~(@as(u64, 1) << @intCast(first_ver_index));
                assert(entry.value_ptr.testInclusion(inc, lib_i));

                // Expand the inclusion one at a time to include as many
                // of the rest of the versions as possible.
                while (wanted_versions != 0) {
                    const test_ver_index = @ctz(wanted_versions);
                    const new_inc: Inclusion = .{
                        .versions = inc.versions | (@as(u64, 1) << @intCast(test_ver_index)),
                        .targets = inc.targets,
                        .lib = inc.lib,
                        .size = 0,
                    };
                    if (entry.value_ptr.testInclusion(new_inc, lib_i)) {
                        inc = new_inc;
                    }
                    wanted_versions &= ~(@as(u64, 1) << @intCast(test_ver_index));
                }

                // Expand the inclusion one at a time to include as many
                // of the rest of the targets as possible.
                while (wanted_targets != 0) {
                    const test_targ_index = @ctz(wanted_targets);
                    const new_inc: Inclusion = .{
                        .versions = inc.versions,
                        .targets = inc.targets | (@as(u64, 1) << @intCast(test_targ_index)),
                        .lib = inc.lib,
                        .size = 0,
                    };
                    if (entry.value_ptr.testInclusion(new_inc, lib_i)) {
                        inc = new_inc;
                    }
                    wanted_targets &= ~(@as(u64, 1) << @intCast(test_targ_index));
                }

                fn_target_popcount += @popCount(inc.targets);
                fn_version_popcount += @popCount(inc.versions);

                try fn_inclusions.append(.{
                    .name = entry.key_ptr.*,
                    .inc = inc,
                });

                // Mark stuff as handled by this inclusion.
                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |_, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;
                        if ((inc.targets & (@as(u64, 1) << @intCast(targets_i)) ) != 0 and
                            (inc.versions & (@as(u64, 1) << @intCast(versions_i)) ) != 0)
                        {
                            handled[lib_i][targets_i][versions_i] = true;
                        }
                    }
                }
            }
        }
    }

    log.info("total function inclusions: {d}", .{fn_inclusions.items.len});
    log.info("average inclusions per function: {d}", .{
        @as(f64, @floatFromInt(fn_inclusions.items.len)) / @as(f64, @floatFromInt(fn_count)),
    });
    log.info("average function targets bits set: {d}", .{
        @as(f64, @floatFromInt(fn_target_popcount)) / @as(f64, @floatFromInt(fn_inclusions.items.len)),
    });
    log.info("average function versions bits set: {d}", .{
        @as(f64, @floatFromInt(fn_version_popcount)) / @as(f64, @floatFromInt(fn_inclusions.items.len)),
    });

    var obj_inclusions = std.ArrayList(NamedInclusion).init(arena);
    var obj_count: usize = 0;
    var obj_target_popcount: usize = 0;
    var obj_version_popcount: usize = 0;
    {
        var it = symbols.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.is_fn) continue;
            obj_count += 1;

            // Find missing inclusions. We can't move on from this symbol until
            // all the present symbols have been handled.
            var handled = none_handled;
            var libs_handled = [1]bool{false} ** lib_names.len;
            var lib_i: u8 = 0;
            while (lib_i < lib_names.len) {
                if (libs_handled[lib_i]) {
                    lib_i += 1;
                    continue;
                }
                const targets_row = entry.value_ptr.type[lib_i];

                var wanted_targets: u64 = 0;
                var wanted_versions_multi = [1]u64{0} ** zig_targets.len;
                var wanted_sizes_multi = [1]u16{0} ** zig_targets.len;

                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |ty, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;

                        switch (ty) {
                            .absent => continue,
                            .object => |size| {
                                wanted_targets |= @as(u64, 1) << @intCast(targets_i);

                                var ok = false;
                                if (wanted_sizes_multi[targets_i] == 0) {
                                    wanted_sizes_multi[targets_i] = size;
                                    ok = true;
                                } else if (wanted_sizes_multi[targets_i] == size) {
                                    ok = true;
                                }
                                if (ok) {
                                    wanted_versions_multi[targets_i] |=
                                        @as(u64, 1) << @intCast(versions_i);
                                }
                            },
                            .function => unreachable,
                        }
                    }
                }
                if (wanted_targets == 0) {
                    // This library is done.
                    libs_handled[lib_i] = true;
                    continue;
                }

                // Put one target and one version into the inclusion.
                const first_targ_index = @ctz(wanted_targets);
                var wanted_versions = wanted_versions_multi[first_targ_index];
                const wanted_size = wanted_sizes_multi[first_targ_index];
                const first_ver_index = @ctz(wanted_versions);
                var inc: Inclusion = .{
                    .versions = @as(u64, 1) << @intCast(first_ver_index),
                    .targets = @as(u64, 1) << @intCast(first_targ_index),
                    .lib = @intCast(lib_i),
                    .size = wanted_size,
                };
                wanted_targets &= ~(@as(u64, 1) << @intCast(first_targ_index));
                wanted_versions &= ~(@as(u64, 1) << @intCast(first_ver_index));
                assert(entry.value_ptr.testInclusion(inc, lib_i));

                // Expand the inclusion one at a time to include as many
                // of the rest of the versions as possible.
                while (wanted_versions != 0) {
                    const test_ver_index = @ctz(wanted_versions);
                    const new_inc: Inclusion = .{
                        .versions = inc.versions | (@as(u64, 1) << @intCast(test_ver_index)),
                        .targets = inc.targets,
                        .lib = inc.lib,
                        .size = wanted_size,
                    };
                    if (entry.value_ptr.testInclusion(new_inc, lib_i)) {
                        inc = new_inc;
                    }
                    wanted_versions &= ~(@as(u64, 1) << @intCast(test_ver_index));
                }

                // Expand the inclusion one at a time to include as many
                // of the rest of the targets as possible.
                while (wanted_targets != 0) {
                    const test_targ_index = @ctz(wanted_targets);
                    if (wanted_sizes_multi[test_targ_index] == wanted_size) {
                        const new_inc: Inclusion = .{
                            .versions = inc.versions,
                            .targets = inc.targets | (@as(u64, 1) << @intCast(test_targ_index)),
                            .lib = inc.lib,
                            .size = wanted_size,
                        };
                        if (entry.value_ptr.testInclusion(new_inc, lib_i)) {
                            inc = new_inc;
                        }
                    }
                    wanted_targets &= ~(@as(u64, 1) << @intCast(test_targ_index));
                }

                obj_target_popcount += @popCount(inc.targets);
                obj_version_popcount += @popCount(inc.versions);

                try obj_inclusions.append(.{
                    .name = entry.key_ptr.*,
                    .inc = inc,
                });

                // Mark stuff as handled by this inclusion.
                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |_, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;
                        if ((inc.targets & (@as(u64, 1) << @intCast(targets_i)) ) != 0 and
                            (inc.versions & (@as(u64, 1) << @intCast(versions_i)) ) != 0)
                        {
                            handled[lib_i][targets_i][versions_i] = true;
                        }
                    }
                }
            }
        }
    }

    log.info("total object inclusions: {d}", .{obj_inclusions.items.len});
    log.info("average inclusions per object: {d}", .{
        @as(f32, @floatFromInt(obj_inclusions.items.len)) / @as(f32, @floatFromInt(obj_count)),
    });
    log.info("average objects targets bits set: {d}", .{
        @as(f64, @floatFromInt(obj_target_popcount)) / @as(f64, @floatFromInt(obj_inclusions.items.len)),
    });
    log.info("average objects versions bits set: {d}", .{
        @as(f64, @floatFromInt(obj_version_popcount)) / @as(f64, @floatFromInt(obj_inclusions.items.len)),
    });

    // Serialize to the output file.
    var af = try fs.cwd().atomicFile("abilists", .{});
    defer af.deinit();

    var bw = std.io.bufferedWriter(af.file.writer());
    const w = bw.writer();

    // Libraries
    try w.writeByte(lib_names.len);
    for (lib_names) |lib_name| {
        try w.writeAll(lib_name);
        try w.writeByte(0);
    }

    // Versions
    try w.writeByte(versions.len);
    for (versions) |ver| {
        try w.writeByte(@intCast(ver.major));
        try w.writeByte(@intCast(ver.minor));
        try w.writeByte(@intCast(ver.patch));
    }

    // Targets
    try w.writeByte(zig_targets.len);
    for (zig_targets) |zt| {
        try w.print("{s}-linux-{s}\x00", .{@tagName(zt.arch), @tagName(zt.abi)});
    }

    {
        // Function Inclusions
        try w.writeInt(u16, @intCast(fn_inclusions.items.len), .little);
        var i: usize = 0;
        while (i < fn_inclusions.items.len) {
            const name = fn_inclusions.items[i].name;
            try w.writeAll(name);
            try w.writeByte(0);
            while (true) {
                const inc = fn_inclusions.items[i].inc;
                i += 1;
                try std.leb.writeUleb128(w, inc.targets);
                const set_terminal_bit = i >= fn_inclusions.items.len or
                    !mem.eql(u8, name, fn_inclusions.items[i].name);
                var lib = inc.lib;
                // Currently no unversioned symbols, so bit 1 << 5 is unset.
                // Currently no weak symbols, so bit 1 << 6 is unset.
                if (set_terminal_bit) {
                    lib |= 1 << 7;
                }
                try w.writeByte(lib);

                var buf: [versions.len]u8 = undefined;
                var buf_index: usize = 0;
                for (versions, 0..) |_, ver_i| {
                    if ((inc.versions & (@as(u64, 1) << @intCast(ver_i))) != 0) {
                        buf[buf_index] = @intCast(ver_i);
                        buf_index += 1;
                    }
                }
                buf[buf_index - 1] |= 0b1000_0000;
                try w.writeAll(buf[0..buf_index]);

                if (set_terminal_bit) break;
            }
        }
    }

    {
        // Object Inclusions
        try w.writeInt(u16, @intCast(obj_inclusions.items.len), .little);
        var i: usize = 0;
        while (i < obj_inclusions.items.len) {
            const name = obj_inclusions.items[i].name;
            try w.writeAll(name);
            try w.writeByte(0);
            while (true) {
                const inc = obj_inclusions.items[i].inc;
                i += 1;
                try std.leb.writeUleb128(w, inc.targets);
                try std.leb.writeUleb128(w, inc.size);
                const set_terminal_bit = i >= obj_inclusions.items.len or
                    !mem.eql(u8, name, obj_inclusions.items[i].name);
                var lib = inc.lib;
                // Currently no unversioned symbols, so bit 1 << 5 is unset.
                // Currently no weak symbols, so bit 1 << 6 is unset.
                if (set_terminal_bit) {
                    lib |= 1 << 7;
                }
                try w.writeByte(lib);

                var buf: [versions.len]u8 = undefined;
                var buf_index: usize = 0;
                for (versions, 0..) |_, ver_i| {
                    if ((inc.versions & (@as(u64, 1) << @intCast(ver_i))) != 0) {
                        buf[buf_index] = @intCast(ver_i);
                        buf_index += 1;
                    }
                }
                buf[buf_index - 1] |= 0b1000_0000;
                try w.writeAll(buf[0..buf_index]);

                if (set_terminal_bit) break;
            }
        }
    }

    {
        // TLS Inclusions (currently none)
        try w.writeInt(u16, 0, .little);
    }

    try bw.flush();
    try af.finish();
}

fn versionAscending(context: void, a: Version, b: Version) bool {
    _ = context;
    return a.order(b) == .lt;
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    log.err(format, args);
    std.process.exit(1);
}

fn verIndex(ver: Version) u6 {
    for (versions, 0..) |v, i| {
        if (v.order(ver) == .eq) {
            return @intCast(i);
        }
    }
    unreachable;
}
