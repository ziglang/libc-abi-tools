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

/// This is organized by grouping together at the beginning,
/// targets most likely to share the same symbol information.
const zig_targets = [_]ZigTarget{
    .{ .name = "arm", .arch = .arm, .abi = .musleabi },
    .{ .name = "arm", .arch = .arm, .abi = .musleabihf },
    .{ .name = "arm", .arch = .armeb, .abi = .musleabi },
    .{ .name = "arm", .arch = .armeb, .abi = .musleabihf },
    .{ .name = "arm", .arch = .thumb, .abi = .musleabi },
    .{ .name = "arm", .arch = .thumb, .abi = .musleabihf },
    .{ .name = "arm", .arch = .thumbeb, .abi = .musleabi },
    .{ .name = "arm", .arch = .thumbeb, .abi = .musleabihf },
    .{ .name = "mips", .arch = .mips, .abi = .musleabi },
    .{ .name = "mips", .arch = .mips, .abi = .musleabihf },
    .{ .name = "mips", .arch = .mipsel, .abi = .musleabi },
    .{ .name = "mips", .arch = .mipsel, .abi = .musleabihf },
    .{ .name = "powerpc", .arch = .powerpc, .abi = .musleabi },
    .{ .name = "powerpc", .arch = .powerpc, .abi = .musleabihf },
    .{ .name = "i386", .arch = .x86, .abi = .musl },

    .{ .name = "aarch64", .arch = .aarch64, .abi = .musl },
    .{ .name = "aarch64", .arch = .aarch64_be, .abi = .musl },
    .{ .name = "loongarch64", .arch = .loongarch64, .abi = .musl },
    .{ .name = "loongarch64", .arch = .loongarch64, .abi = .muslsf },
    .{ .name = "mips64", .arch = .mips64, .abi = .muslabi64 },
    .{ .name = "mips64", .arch = .mips64, .abi = .muslabin32 },
    .{ .name = "mips64", .arch = .mips64el, .abi = .muslabi64 },
    .{ .name = "mips64", .arch = .mips64el, .abi = .muslabin32 },
    .{ .name = "powerpc64", .arch = .powerpc64, .abi = .musl },
    .{ .name = "powerpc64", .arch = .powerpc64le, .abi = .musl },
    .{ .name = "riscv64", .arch = .riscv64, .abi = .musl },
    .{ .name = "s390x", .arch = .s390x, .abi = .musl },
    .{ .name = "x86_64", .arch = .x86_64, .abi = .musl },
};

comptime {
    assert(zig_targets.len <= @bitSizeOf(@FieldType(Inclusion, "targets")));
}

const first_fs_ver: std.SemanticVersion = .{
    .major = 10,
    .minor = 1,
    .patch = 0,
};

/// Introduced loongarch64 and riscv32 support.
const ver1_2_5: std.SemanticVersion = .{
    .major = 1,
    .minor = 2,
    .patch = 5,
};

const versions = [_]std.SemanticVersion{
    .{ .major = 1, .minor = 2, .patch = 0 },
    .{ .major = 1, .minor = 2, .patch = 1 },
    .{ .major = 1, .minor = 2, .patch = 2 },
    .{ .major = 1, .minor = 2, .patch = 3 },
    .{ .major = 1, .minor = 2, .patch = 4 },
    .{ .major = 1, .minor = 2, .patch = 5 },
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
    assert(versions.len <= @bitSizeOf(@FieldType(Inclusion, "versions")));
}

const Symbol = struct {
    type: [zig_targets.len][versions.len]Type = empty_type,
    confirmed_type: enum {
        function,
        object,
    } = undefined,

    const empty_row = [1]Type{.absent} ** versions.len;
    const empty_type = [1]@TypeOf(empty_row){empty_row} ** zig_targets.len;

    const Type = union(enum) {
        absent,
        function: struct { weak: bool },
        object: struct { weak: bool, size: u16 },

        fn eql(ty: Type, other: Type) bool {
            return switch (ty) {
                .absent => unreachable,
                .function => |ty_fn| other == .function and
                    ty_fn.weak == other.function.weak,
                .object => |ty_info| switch (other) {
                    .absent => unreachable,
                    .function => false,
                    .object => |other_info| ty_info.weak == other_info.weak and
                        ty_info.size == other_info.size,
                },
            };
        }
    };

    /// Return true if and only if the inclusion has no false positives.
    fn testInclusion(symbol: Symbol, inc: Inclusion) bool {
        for (symbol.type, 0..) |versions_row, targets_i| {
            for (versions_row, 0..) |ty, versions_i| {
                if ((inc.targets & (@as(u64, 1) << @intCast(targets_i))) == 0 or
                    (inc.versions & (@as(u64, 1) << @intCast(versions_i))) == 0)
                    continue;

                switch (ty) {
                    .absent => return false,
                    .function => |f| if (inc.weak != f.weak) return false,
                    .object => |o| if (inc.weak != o.weak or inc.size != o.size) return false,
                }
            }
        }

        return true;
    }
};

const Inclusion = struct {
    versions: u64,
    targets: u64,
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

    var musl_dir = try fs.cwd().openDir(".", .{ .iterate = true });
    defer musl_dir.close();

    const fs_versions = v: {
        var fs_versions: std.ArrayList(std.SemanticVersion) = .{};

        var version_dir_it = musl_dir.iterate();
        while (try version_dir_it.next()) |entry| {
            if (entry.kind != .directory) continue;
            try fs_versions.append(arena, try std.SemanticVersion.parse(entry.name));
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
        log.info("scanning abilist files for musl version: {f}", .{fs_ver});

        const prefix = try fmt.allocPrint(arena, "{d}.{d}.{d}", .{
            fs_ver.major, fs_ver.minor, fs_ver.patch,
        });

        for (zig_targets) |target| {
            if ((target.arch == .loongarch64 or target.arch == .riscv32) and fs_ver.order(ver1_2_5) == .lt)
                continue;

            const abilist_path = try fs.path.join(arena, &.{
                prefix,
                target.name,
                "libc.abilist",
            });

            const contents = try musl_dir.readFileAlloc(arena, abilist_path, 10 * 1024 * 1024);

            var lines_it = std.mem.tokenizeScalar(u8, contents, '\n');
            while (lines_it.next()) |line| {
                var tok_it = std.mem.tokenizeScalar(u8, line, ' ');
                const name = tok_it.next().?;
                const weak = if (std.mem.eql(u8, tok_it.peek().?, "W")) blk: {
                    _ = tok_it.next().?;
                    break :blk true;
                } else false;
                const category = tok_it.next().?;
                const ty: Symbol.Type = if (mem.eql(u8, category, "F"))
                    .{ .function = .{
                        .weak = weak,
                    } }
                else if (mem.eql(u8, category, "D"))
                    .{ .object = .{
                        .weak = weak,
                        .size = try fmt.parseInt(u16, tok_it.next().?, 0),
                    } }
                else
                    unreachable;

                const gop = try symbols.getOrPut(name);
                if (!gop.found_existing) gop.value_ptr.* = .{};

                gop.value_ptr.type[target.getIndex()][verIndex(fs_ver)] = ty;
            }
        }
    }

    // Our data format depends on the type of a symbol being consistently a function or object, and
    // not switching depending on target or version. Here we verify that premise.
    {
        var it = symbols.iterator();
        while (it.next()) |entry| {
            const name = entry.key_ptr.*;
            var prev_ty: @typeInfo(Symbol.Type).@"union".tag_type.? = .absent;
            for (entry.value_ptr.type) |versions_row| {
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
            entry.value_ptr.confirmed_type = switch (prev_ty) {
                .absent => unreachable,
                .function => .function,
                .object => .object,
            };
        }
        log.info("confirmed that every symbol is consistently either a function or object", .{});
    }

    // Now we have all the data and we want to emit the fewest number of inclusions as possible.
    // The first split is functions vs objects.
    // For functions, the only type possibilities are `absent` or `function`.
    // We use a greedy algorithm, "spreading" the inclusion from a single point to
    // as many targets as possible, then to as many versions as possible.
    var fn_inclusions: std.ArrayList(NamedInclusion) = .{};
    var fn_count: usize = 0;
    var fn_target_popcount: usize = 0;
    var fn_version_popcount: usize = 0;
    const none_handled = blk: {
        const empty_row = [1]bool{false} ** versions.len;
        const empty_row2 = [1]@TypeOf(empty_row){empty_row} ** zig_targets.len;
        break :blk empty_row2;
    };
    {
        var it = symbols.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.confirmed_type != .function) continue;
            fn_count += 1;

            // Find missing inclusions. We can't move on from this symbol until
            // all the present symbols have been handled.
            var handled = none_handled;

            const targets_row = entry.value_ptr.type;

            var wanted_targets: u64 = 0;
            var wanted_versions_multi = [1]u64{0} ** zig_targets.len;
            var wanted_weak_multi = [1]?bool{null} ** zig_targets.len;

            for (targets_row, 0..) |versions_row, targets_i| {
                for (versions_row, 0..) |ty, versions_i| {
                    if (handled[targets_i][versions_i]) continue;

                    switch (ty) {
                        .absent => continue,
                        .function => |info| {
                            wanted_targets |= @as(u64, 1) << @intCast(targets_i);

                            var weak_ok = false;
                            if (wanted_weak_multi[targets_i] == null) {
                                wanted_weak_multi[targets_i] = info.weak;
                                weak_ok = true;
                            } else if (wanted_weak_multi[targets_i].? == info.weak) {
                                weak_ok = true;
                            }

                            if (weak_ok) {
                                wanted_versions_multi[targets_i] |=
                                    @as(u64, 1) << @intCast(versions_i);
                            }
                        },
                        .object => unreachable,
                    }
                }
            }

            // Put one target and one version into the inclusion.
            const first_targ_index = @ctz(wanted_targets);
            var wanted_versions = wanted_versions_multi[first_targ_index];
            const wanted_weak = wanted_weak_multi[first_targ_index].?;
            const first_ver_index = @ctz(wanted_versions);
            var inc: Inclusion = .{
                .versions = @as(u64, 1) << @intCast(first_ver_index),
                .targets = @as(u64, 1) << @intCast(first_targ_index),
                .weak = wanted_weak,
                .size = 0,
            };
            wanted_targets &= ~(@as(u64, 1) << @intCast(first_targ_index));
            wanted_versions &= ~(@as(u64, 1) << @intCast(first_ver_index));
            assert(entry.value_ptr.testInclusion(inc));

            // Expand the inclusion one at a time to include as many
            // of the rest of the versions as possible.
            while (wanted_versions != 0) {
                const test_ver_index = @ctz(wanted_versions);
                const new_inc: Inclusion = .{
                    .versions = inc.versions | (@as(u64, 1) << @intCast(test_ver_index)),
                    .targets = inc.targets,
                    .weak = wanted_weak,
                    .size = 0,
                };
                if (entry.value_ptr.testInclusion(new_inc)) {
                    inc = new_inc;
                }
                wanted_versions &= ~(@as(u64, 1) << @intCast(test_ver_index));
            }

            // Expand the inclusion one at a time to include as many
            // of the rest of the targets as possible.
            while (wanted_targets != 0) {
                const test_targ_index = @ctz(wanted_targets);
                if (wanted_weak_multi[test_targ_index] == wanted_weak) {
                    const new_inc: Inclusion = .{
                        .versions = inc.versions,
                        .targets = inc.targets | (@as(u64, 1) << @intCast(test_targ_index)),
                        .weak = wanted_weak,
                        .size = 0,
                    };
                    if (entry.value_ptr.testInclusion(new_inc)) {
                        inc = new_inc;
                    }
                }
                wanted_targets &= ~(@as(u64, 1) << @intCast(test_targ_index));
            }

            fn_target_popcount += @popCount(inc.targets);
            fn_version_popcount += @popCount(inc.versions);

            try fn_inclusions.append(arena, .{
                .name = entry.key_ptr.*,
                .inc = inc,
            });

            // Mark stuff as handled by this inclusion.
            for (targets_row, 0..) |versions_row, targets_i| {
                for (versions_row, 0..) |_, versions_i| {
                    if (handled[targets_i][versions_i]) continue;
                    if ((inc.targets & (@as(u64, 1) << @intCast(targets_i))) != 0 and
                        (inc.versions & (@as(u64, 1) << @intCast(versions_i))) != 0)
                    {
                        handled[targets_i][versions_i] = true;
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

    var obj_inclusions: std.ArrayList(NamedInclusion) = .{};
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

            const targets_row = entry.value_ptr.type;

            var wanted_targets: u64 = 0;
            var wanted_versions_multi = [1]u64{0} ** zig_targets.len;
            var wanted_weak_multi = [1]?bool{null} ** zig_targets.len;
            var wanted_sizes_multi = [1]u16{0} ** zig_targets.len;

            for (targets_row, 0..) |versions_row, targets_i| {
                for (versions_row, 0..) |ty, versions_i| {
                    if (handled[targets_i][versions_i]) continue;

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

                            var weak_ok = false;
                            if (wanted_weak_multi[targets_i] == null) {
                                wanted_weak_multi[targets_i] = info.weak;
                                weak_ok = true;
                            } else if (wanted_weak_multi[targets_i].? == info.weak) {
                                weak_ok = true;
                            }

                            if (size_ok and weak_ok) {
                                wanted_versions_multi[targets_i] |=
                                    @as(u64, 1) << @intCast(versions_i);
                            }
                        },
                        .function => unreachable,
                    }
                }
            }

            // Put one target and one version into the inclusion.
            const first_targ_index = @ctz(wanted_targets);
            var wanted_versions = wanted_versions_multi[first_targ_index];
            const wanted_weak = wanted_weak_multi[first_targ_index].?;
            const wanted_size = wanted_sizes_multi[first_targ_index];
            const first_ver_index = @ctz(wanted_versions);
            var inc: Inclusion = .{
                .versions = @as(u64, 1) << @intCast(first_ver_index),
                .targets = @as(u64, 1) << @intCast(first_targ_index),
                .weak = wanted_weak,
                .size = wanted_size,
            };
            wanted_targets &= ~(@as(u64, 1) << @intCast(first_targ_index));
            wanted_versions &= ~(@as(u64, 1) << @intCast(first_ver_index));
            assert(entry.value_ptr.testInclusion(inc));

            // Expand the inclusion one at a time to include as many
            // of the rest of the versions as possible.
            while (wanted_versions != 0) {
                const test_ver_index = @ctz(wanted_versions);
                const new_inc: Inclusion = .{
                    .versions = inc.versions | (@as(u64, 1) << @intCast(test_ver_index)),
                    .targets = inc.targets,
                    .weak = wanted_weak,
                    .size = wanted_size,
                };
                if (entry.value_ptr.testInclusion(new_inc)) {
                    inc = new_inc;
                }
                wanted_versions &= ~(@as(u64, 1) << @intCast(test_ver_index));
            }

            // Expand the inclusion one at a time to include as many
            // of the rest of the targets as possible.
            while (wanted_targets != 0) {
                const test_targ_index = @ctz(wanted_targets);
                if (wanted_weak_multi[test_targ_index] == wanted_weak and
                    wanted_sizes_multi[test_targ_index] == wanted_size)
                {
                    const new_inc: Inclusion = .{
                        .versions = inc.versions,
                        .targets = inc.targets | (@as(u64, 1) << @intCast(test_targ_index)),
                        .weak = wanted_weak,
                        .size = wanted_size,
                    };
                    if (entry.value_ptr.testInclusion(new_inc)) {
                        inc = new_inc;
                    }
                }
                wanted_targets &= ~(@as(u64, 1) << @intCast(test_targ_index));
            }

            obj_target_popcount += @popCount(inc.targets);
            obj_version_popcount += @popCount(inc.versions);

            try obj_inclusions.append(arena, .{
                .name = entry.key_ptr.*,
                .inc = inc,
            });

            // Mark stuff as handled by this inclusion.
            for (targets_row, 0..) |versions_row, targets_i| {
                for (versions_row, 0..) |_, versions_i| {
                    if (handled[targets_i][versions_i]) continue;
                    if ((inc.targets & (@as(u64, 1) << @intCast(targets_i))) != 0 and
                        (inc.versions & (@as(u64, 1) << @intCast(versions_i))) != 0)
                    {
                        handled[targets_i][versions_i] = true;
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
    var buffer: [4096]u8 = undefined;
    var af = try fs.cwd().atomicFile("abilists", .{ .write_buffer = &buffer });
    defer af.deinit();

    const w = &af.file_writer.interface;

    // Libraries
    try w.writeByte(1);
    try w.writeAll("c\x00");

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
        try w.print("{s}-linux-{s}\x00", .{ @tagName(zt.arch), @tagName(zt.abi) });
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
                var lib: u8 = 0;
                lib |= 1 << 5; // musl is always unversioned.
                if (inc.weak) {
                    lib |= 1 << 6;
                }
                if (set_terminal_bit) {
                    lib |= 1 << 7;
                }
                try w.writeByte(lib);

                // For unversioned inclusions, we only need to write the earliest version.
                try w.writeByte(@as(u8, @ctz(inc.versions)) | 0b1000_0000);

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
                var lib: u8 = 0;
                lib |= 1 << 5; // musl is always unversioned.
                if (inc.weak) {
                    lib |= 1 << 6;
                }
                if (set_terminal_bit) {
                    lib |= 1 << 7;
                }
                try w.writeByte(lib);

                // For unversioned inclusions, we only need to write the earliest version.
                try w.writeByte(@as(u8, @ctz(inc.versions)) | 0b1000_0000);

                if (set_terminal_bit) break;
            }
        }
    }

    {
        // TLS Inclusions (currently none)
        try w.writeInt(u16, 0, .little);
    }

    try af.finish();
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    log.err(format, args);
    std.process.exit(1);
}
