const std = @import("std");
const Target = std.Target;
const mem = std.mem;
const log = std.log;
const fs = std.fs;
const fmt = std.fmt;
const assert = std.debug.assert;

const ZigTarget = struct {
    name: []const u8,
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

/// `pthread` is only an alias for `thr`.
/// `xnet` is only an alias for `c`.
const lib_names = [_][]const u8{
    "m",
    "stdthreads",
    "thr",
    "c",
    "dl",
    "rt",
    "ld",
    "util",
    "execinfo",
};

/// This is organized by grouping together at the beginning,
/// targets most likely to share the same symbol information.
const zig_targets = [_]ZigTarget{
    .{ .name = "armv7", .arch = .arm, .abi = .eabihf },
    .{ .name = "powerpc", .arch = .powerpc, .abi = .eabihf },
    .{ .name = "i386", .arch = .x86, .abi = .none },

    .{ .name = "aarch64", .arch = .aarch64, .abi = .none },
    .{ .name = "powerpc64", .arch = .powerpc64, .abi = .none },
    .{ .name = "powerpc64", .arch = .powerpc64le, .abi = .none },
    .{ .name = "riscv64", .arch = .riscv64, .abi = .none },
    .{ .name = "amd64", .arch = .x86_64, .abi = .none },
};

comptime {
    assert(zig_targets.len <= @bitSizeOf(std.meta.FieldType(Inclusion, .targets)));
}

const first_fs_ver: std.SemanticVersion = .{
    .major = 1,
    .minor = 7,
    .patch = 0,
};

const versions = [_]std.SemanticVersion{
    .{ .major = 1, .minor = 0, .patch = 0 },
    .{ .major = 1, .minor = 1, .patch = 0 },
    .{ .major = 1, .minor = 2, .patch = 0 },
    .{ .major = 1, .minor = 3, .patch = 0 },
    .{ .major = 1, .minor = 4, .patch = 0 },
    .{ .major = 1, .minor = 5, .patch = 0 },
    .{ .major = 1, .minor = 6, .patch = 0 },
    .{ .major = 1, .minor = 7, .patch = 0 },
};

fn verIndex(ver: std.SemanticVersion) u6 {
    for (versions, 0..) |v, i| {
        if (v.order(ver) == .eq) {
            return @intCast(i);
        }
    }

    unreachable;
}

comptime {
    assert(versions.len <= @bitSizeOf(std.meta.FieldType(Inclusion, .versions)));
}

const Symbol = struct {
    type: [lib_names.len][zig_targets.len][versions.len]Type = empty_type,
    confirmed_type: enum {
        function,
        object,
        tls,
    } = undefined,

    const empty_row = [1]Type{.absent} ** versions.len;
    const empty_row2 = [1]@TypeOf(empty_row){empty_row} ** zig_targets.len;
    const empty_type = [1]@TypeOf(empty_row2){empty_row2} ** lib_names.len;

    const Type = union(enum) {
        absent,
        function: struct { unversioned: bool, weak: bool },
        object: struct { unversioned: bool, weak: bool, size: u16 },
        tls: struct { unversioned: bool, weak: bool, size: u16 },

        fn eql(ty: Type, other: Type) bool {
            return switch (ty) {
                .absent => unreachable,
                .function => |ty_fn| other == .function and
                    ty_fn.unversioned == other.function.unversioned and
                    ty_fn.weak == other.function.weak,
                inline .object, .tls => |ty_info, ty_tag| switch (other) {
                    .absent => unreachable,
                    .function => false,
                    inline .object, .tls => |other_info, other_tag| ty_tag == other_tag and
                        ty_info.unversioned == other_info.unversioned and
                        ty_info.weak == other_info.weak and
                        ty_info.size == other_info.size,
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
                    .function => |f| if (inc.unversioned != f.unversioned or inc.weak != f.weak) return false,
                    inline .object, .tls => |o| if (inc.unversioned != o.unversioned or inc.weak != o.weak or inc.size != o.size) return false,
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
    unversioned: bool,
    weak: bool,
    size: u16,
};

const NamedInclusion = struct {
    name: []const u8,
    inc: Inclusion,
};

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const arena = arena_instance.allocator();

    var version_dir = try fs.cwd().openDir(".", .{ .iterate = true });
    defer version_dir.close();

    const fs_versions = v: {
        var fs_versions = std.ArrayList(std.SemanticVersion).init(arena);

        var version_dir_it = version_dir.iterate();
        while (try version_dir_it.next()) |entry| {
            if (entry.kind != .directory) continue;
            try fs_versions.append(try std.SemanticVersion.parse(try std.fmt.allocPrint(arena, "{s}.0", .{entry.name})));
        }

        break :v fs_versions.items;
    };

    std.mem.sort(std.SemanticVersion, fs_versions, {}, struct {
        fn lessThan(ctx: void, a: std.SemanticVersion, b: std.SemanticVersion) bool {
            _ = ctx;

            return a.order(b) == .lt;
        }
    }.lessThan);

    var symbols = std.StringHashMap(Symbol).init(arena);

    for (fs_versions) |fs_ver| {
        log.info("scanning abilist files for FreeBSD libc version: {}", .{fs_ver});

        const prefix = try fmt.allocPrint(arena, "{d}.{d}", .{
            fs_ver.major, fs_ver.minor,
        });

        for (zig_targets) |target| {
            for (lib_names, 0..) |lib_name, lib_i| {
                const abilist_path = try fs.path.join(arena, &.{
                    prefix,
                    target.name,
                    try fmt.allocPrint(arena, "{s}{s}.abilist", .{
                        if (std.mem.eql(u8, lib_name, "ld")) "" else "lib",
                        lib_name,
                    }),
                });

                const contents = try version_dir.readFileAlloc(arena, abilist_path, 10 * 1024 * 1024);

                var lines_it = std.mem.tokenizeScalar(u8, contents, '\n');
                while (lines_it.next()) |line| {
                    var tok_it = std.mem.tokenizeScalar(u8, line, ' ');
                    const unversioned: bool, const ver: std.SemanticVersion = blk: {
                        // This means the symbol is unversioned; there is no version token. In this case, we just
                        // go by the first version on disk that we saw the symbol in, and pray that it's mostly correct.
                        if (std.mem.startsWith(u8, line, " ")) break :blk .{ true, fs_ver };

                        const ver_text = tok_it.next().?;
                        break :blk .{ false, try std.SemanticVersion.parse(
                            try std.fmt.allocPrint(arena, "{s}.0", .{ver_text[std.mem.indexOfAny(u8, ver_text, "0123456789").?..]}),
                        ) };
                    };
                    const name = tok_it.next().?;
                    const weak = if (std.mem.eql(u8, tok_it.peek().?, "W")) blk: {
                        _ = tok_it.next().?;
                        break :blk true;
                    } else false;
                    const category = tok_it.next().?;
                    const ty: Symbol.Type = if (mem.eql(u8, category, "F"))
                        .{ .function = .{
                            .unversioned = unversioned,
                            .weak = weak,
                        } }
                    else if (mem.eql(u8, category, "D"))
                        .{ .object = .{
                            .unversioned = unversioned,
                            .weak = weak,
                            .size = try fmt.parseInt(u16, tok_it.next().?, 0),
                        } }
                    else if (mem.eql(u8, category, "T"))
                        .{ .tls = .{
                            .unversioned = unversioned,
                            .weak = weak,
                            .size = try fmt.parseInt(u16, tok_it.next().?, 0),
                        } }
                    else
                        unreachable;

                    const gop = try symbols.getOrPut(name);
                    if (!gop.found_existing) gop.value_ptr.* = .{};

                    gop.value_ptr.type[lib_i][target.getIndex()][verIndex(ver)] = ty;
                }
            }
        }
    }

    // Our data format depends on the type of a symbol being consistently a function, object, or
    // TLS, and not switching depending on target or version. Here we verify that premise.
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
                                .object, .tls => fatal("symbol {s} switches types", .{name}),
                            },
                            .object => switch (prev_ty) {
                                .absent => prev_ty = ty,
                                .function, .tls => fatal("symbol {s} switches types", .{name}),
                                .object => continue,
                            },
                            .tls => switch (prev_ty) {
                                .absent => prev_ty = ty,
                                .function, .object => fatal("symbol {s} switches types", .{name}),
                                .tls => continue,
                            },
                        }
                    }
                }
            }
            entry.value_ptr.confirmed_type = switch (prev_ty) {
                .absent => unreachable,
                .function => .function,
                .object => .object,
                .tls => .tls,
            };
        }
        log.info("confirmed that every symbol is consistently either a function, object, or tls", .{});
    }

    // Now we have all the data and we want to emit the fewest number of inclusions as possible.
    // The first split is functions vs objects vs TLS.
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
            if (entry.value_ptr.confirmed_type != .function) continue;
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
                var wanted_unversioned_multi = [1]?bool{null} ** zig_targets.len;
                var wanted_weak_multi = [1]?bool{null} ** zig_targets.len;

                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |ty, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;

                        switch (ty) {
                            .absent => continue,
                            .function => |info| {
                                wanted_targets |= @as(u64, 1) << @intCast(targets_i);

                                var unversioned_ok = false;
                                if (wanted_unversioned_multi[targets_i] == null) {
                                    wanted_unversioned_multi[targets_i] = info.unversioned;
                                    unversioned_ok = true;
                                } else if (wanted_unversioned_multi[targets_i].? == info.unversioned) {
                                    unversioned_ok = true;
                                }

                                var weak_ok = false;
                                if (wanted_weak_multi[targets_i] == null) {
                                    wanted_weak_multi[targets_i] = info.weak;
                                    weak_ok = true;
                                } else if (wanted_weak_multi[targets_i].? == info.weak) {
                                    weak_ok = true;
                                }

                                if (unversioned_ok and weak_ok) {
                                    wanted_versions_multi[targets_i] |=
                                        @as(u64, 1) << @intCast(versions_i);
                                }
                            },
                            .object, .tls => unreachable,
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
                const wanted_unversioned = wanted_unversioned_multi[first_targ_index].?;
                const wanted_weak = wanted_weak_multi[first_targ_index].?;
                const first_ver_index = @ctz(wanted_versions);
                var inc: Inclusion = .{
                    .versions = @as(u64, 1) << @intCast(first_ver_index),
                    .targets = @as(u64, 1) << @intCast(first_targ_index),
                    .lib = @intCast(lib_i),
                    .unversioned = wanted_unversioned,
                    .weak = wanted_weak,
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
                        .unversioned = wanted_unversioned,
                        .weak = wanted_weak,
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
                    if (wanted_unversioned_multi[test_targ_index] == wanted_unversioned and
                        wanted_weak_multi[test_targ_index] == wanted_weak)
                    {
                        const new_inc: Inclusion = .{
                            .versions = inc.versions,
                            .targets = inc.targets | (@as(u64, 1) << @intCast(test_targ_index)),
                            .lib = inc.lib,
                            .unversioned = wanted_unversioned,
                            .weak = wanted_weak,
                            .size = 0,
                        };
                        if (entry.value_ptr.testInclusion(new_inc, lib_i)) {
                            inc = new_inc;
                        }
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
                        if ((inc.targets & (@as(u64, 1) << @intCast(targets_i))) != 0 and
                            (inc.versions & (@as(u64, 1) << @intCast(versions_i))) != 0)
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
            if (entry.value_ptr.confirmed_type != .object) continue;
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
                var wanted_unversioned_multi = [1]?bool{null} ** zig_targets.len;
                var wanted_weak_multi = [1]?bool{null} ** zig_targets.len;
                var wanted_sizes_multi = [1]u16{0} ** zig_targets.len;

                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |ty, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;

                        switch (ty) {
                            .absent => continue,
                            .object => |info| {
                                wanted_targets |= @as(u64, 1) << @intCast(targets_i);

                                var size_ok = false;
                                if (wanted_sizes_multi[targets_i] == 0) {
                                    wanted_sizes_multi[targets_i] = info.size;
                                    size_ok = true;
                                } else if (wanted_sizes_multi[targets_i] == info.size) {
                                    size_ok = true;
                                }

                                var unversioned_ok = false;
                                if (wanted_unversioned_multi[targets_i] == null) {
                                    wanted_unversioned_multi[targets_i] = info.unversioned;
                                    unversioned_ok = true;
                                } else if (wanted_unversioned_multi[targets_i].? == info.unversioned) {
                                    unversioned_ok = true;
                                }

                                var weak_ok = false;
                                if (wanted_weak_multi[targets_i] == null) {
                                    wanted_weak_multi[targets_i] = info.weak;
                                    weak_ok = true;
                                } else if (wanted_weak_multi[targets_i].? == info.weak) {
                                    weak_ok = true;
                                }

                                if (size_ok and unversioned_ok and weak_ok) {
                                    wanted_versions_multi[targets_i] |=
                                        @as(u64, 1) << @intCast(versions_i);
                                }
                            },
                            .function, .tls => unreachable,
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
                const wanted_unversioned = wanted_unversioned_multi[first_targ_index].?;
                const wanted_weak = wanted_weak_multi[first_targ_index].?;
                const wanted_size = wanted_sizes_multi[first_targ_index];
                const first_ver_index = @ctz(wanted_versions);
                var inc: Inclusion = .{
                    .versions = @as(u64, 1) << @intCast(first_ver_index),
                    .targets = @as(u64, 1) << @intCast(first_targ_index),
                    .lib = @intCast(lib_i),
                    .unversioned = wanted_unversioned,
                    .weak = wanted_weak,
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
                        .unversioned = wanted_unversioned,
                        .weak = wanted_weak,
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
                    if (wanted_unversioned_multi[test_targ_index] == wanted_unversioned and
                        wanted_weak_multi[test_targ_index] == wanted_weak and
                        wanted_sizes_multi[test_targ_index] == wanted_size)
                    {
                        const new_inc: Inclusion = .{
                            .versions = inc.versions,
                            .targets = inc.targets | (@as(u64, 1) << @intCast(test_targ_index)),
                            .lib = inc.lib,
                            .unversioned = wanted_unversioned,
                            .weak = wanted_weak,
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
                        if ((inc.targets & (@as(u64, 1) << @intCast(targets_i))) != 0 and
                            (inc.versions & (@as(u64, 1) << @intCast(versions_i))) != 0)
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

    var tls_inclusions = std.ArrayList(NamedInclusion).init(arena);
    var tls_count: usize = 0;
    var tls_target_popcount: usize = 0;
    var tls_version_popcount: usize = 0;
    {
        var it = symbols.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.confirmed_type != .tls) continue;
            tls_count += 1;

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
                var wanted_unversioned_multi = [1]?bool{null} ** zig_targets.len;
                var wanted_weak_multi = [1]?bool{null} ** zig_targets.len;
                var wanted_sizes_multi = [1]u16{0} ** zig_targets.len;

                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |ty, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;

                        switch (ty) {
                            .absent => continue,
                            .tls => |info| {
                                wanted_targets |= @as(u64, 1) << @intCast(targets_i);

                                var size_ok = false;
                                if (wanted_sizes_multi[targets_i] == 0) {
                                    wanted_sizes_multi[targets_i] = info.size;
                                    size_ok = true;
                                } else if (wanted_sizes_multi[targets_i] == info.size) {
                                    size_ok = true;
                                }

                                var unversioned_ok = false;
                                if (wanted_unversioned_multi[targets_i] == null) {
                                    wanted_unversioned_multi[targets_i] = info.unversioned;
                                    unversioned_ok = true;
                                } else if (wanted_unversioned_multi[targets_i].? == info.unversioned) {
                                    unversioned_ok = true;
                                }

                                var weak_ok = false;
                                if (wanted_weak_multi[targets_i] == null) {
                                    wanted_weak_multi[targets_i] = info.weak;
                                    weak_ok = true;
                                } else if (wanted_weak_multi[targets_i].? == info.weak) {
                                    weak_ok = true;
                                }

                                if (size_ok and unversioned_ok and weak_ok) {
                                    wanted_versions_multi[targets_i] |=
                                        @as(u64, 1) << @intCast(versions_i);
                                }
                            },
                            .function, .object => unreachable,
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
                const wanted_unversioned = wanted_unversioned_multi[first_targ_index].?;
                const wanted_weak = wanted_weak_multi[first_targ_index].?;
                const wanted_size = wanted_sizes_multi[first_targ_index];
                const first_ver_index = @ctz(wanted_versions);
                var inc: Inclusion = .{
                    .versions = @as(u64, 1) << @intCast(first_ver_index),
                    .targets = @as(u64, 1) << @intCast(first_targ_index),
                    .lib = @intCast(lib_i),
                    .unversioned = wanted_unversioned,
                    .weak = wanted_weak,
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
                        .unversioned = wanted_unversioned,
                        .weak = wanted_weak,
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
                    if (wanted_unversioned_multi[test_targ_index] == wanted_unversioned and
                        wanted_weak_multi[test_targ_index] == wanted_weak and
                        wanted_sizes_multi[test_targ_index] == wanted_size)
                    {
                        const new_inc: Inclusion = .{
                            .versions = inc.versions,
                            .targets = inc.targets | (@as(u64, 1) << @intCast(test_targ_index)),
                            .lib = inc.lib,
                            .unversioned = wanted_unversioned,
                            .weak = wanted_weak,
                            .size = wanted_size,
                        };
                        if (entry.value_ptr.testInclusion(new_inc, lib_i)) {
                            inc = new_inc;
                        }
                    }
                    wanted_targets &= ~(@as(u64, 1) << @intCast(test_targ_index));
                }

                tls_target_popcount += @popCount(inc.targets);
                tls_version_popcount += @popCount(inc.versions);

                try tls_inclusions.append(.{
                    .name = entry.key_ptr.*,
                    .inc = inc,
                });

                // Mark stuff as handled by this inclusion.
                for (targets_row, 0..) |versions_row, targets_i| {
                    for (versions_row, 0..) |_, versions_i| {
                        if (handled[lib_i][targets_i][versions_i]) continue;
                        if ((inc.targets & (@as(u64, 1) << @intCast(targets_i))) != 0 and
                            (inc.versions & (@as(u64, 1) << @intCast(versions_i))) != 0)
                        {
                            handled[lib_i][targets_i][versions_i] = true;
                        }
                    }
                }
            }
        }
    }

    log.info("total tls inclusions: {d}", .{tls_inclusions.items.len});
    log.info("average inclusions per tls: {d}", .{
        @as(f32, @floatFromInt(tls_inclusions.items.len)) / @as(f32, @floatFromInt(tls_count)),
    });
    log.info("average tls targets bits set: {d}", .{
        @as(f64, @floatFromInt(tls_target_popcount)) / @as(f64, @floatFromInt(tls_inclusions.items.len)),
    });
    log.info("average tls versions bits set: {d}", .{
        @as(f64, @floatFromInt(tls_version_popcount)) / @as(f64, @floatFromInt(tls_inclusions.items.len)),
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
        try w.print("{s}-freebsd-{s}\x00", .{ @tagName(zt.arch), @tagName(zt.abi) });
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
                if (inc.unversioned) {
                    lib |= 1 << 5;
                }
                if (inc.weak) {
                    lib |= 1 << 6;
                }
                if (set_terminal_bit) {
                    lib |= 1 << 7;
                }
                try w.writeByte(lib);

                // For unversioned inclusions, we only need to write the earliest version.
                if (inc.unversioned) {
                    try w.writeByte(@as(u8, @ctz(inc.versions)) | 0b1000_0000);
                } else {
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
                }

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
                if (inc.unversioned) {
                    lib |= 1 << 5;
                }
                if (inc.weak) {
                    lib |= 1 << 6;
                }
                if (set_terminal_bit) {
                    lib |= 1 << 7;
                }
                try w.writeByte(lib);

                // For unversioned inclusions, we only need to write the earliest version.
                if (inc.unversioned) {
                    try w.writeByte(@as(u8, @ctz(inc.versions)) | 0b1000_0000);
                } else {
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
                }

                if (set_terminal_bit) break;
            }
        }
    }

    {
        // TLS Inclusions
        try w.writeInt(u16, @intCast(tls_inclusions.items.len), .little);
        var i: usize = 0;
        while (i < tls_inclusions.items.len) {
            const name = tls_inclusions.items[i].name;
            try w.writeAll(name);
            try w.writeByte(0);
            while (true) {
                const inc = tls_inclusions.items[i].inc;
                i += 1;
                try std.leb.writeUleb128(w, inc.targets);
                try std.leb.writeUleb128(w, inc.size);
                const set_terminal_bit = i >= tls_inclusions.items.len or
                    !mem.eql(u8, name, tls_inclusions.items[i].name);
                var lib = inc.lib;
                if (inc.unversioned) {
                    lib |= 1 << 5;
                }
                if (inc.weak) {
                    lib |= 1 << 6;
                }
                if (set_terminal_bit) {
                    lib |= 1 << 7;
                }
                try w.writeByte(lib);

                // For unversioned inclusions, we only need to write the earliest version.
                if (inc.unversioned) {
                    try w.writeByte(@as(u8, @ctz(inc.versions)) | 0b1000_0000);
                } else {
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
                }

                if (set_terminal_bit) break;
            }
        }
    }

    try bw.flush();
    try af.finish();
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    log.err(format, args);
    std.process.exit(1);
}
