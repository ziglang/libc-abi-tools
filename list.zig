const std = @import("std");
const Version = std.SemanticVersion;
const mem = std.mem;
const log = std.log;
const fs = std.fs;
const fmt = std.fmt;
const assert = std.debug.assert;

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);
    const abilists_file_path = args[1];

    var lib_names_buffer: [8][]const u8 = undefined;
    var target_names_buffer: [32][]const u8 = undefined;
    var versions_buffer: [64]Version = undefined;

    var file = try std.fs.cwd().openFile(abilists_file_path, .{});
    defer file.close();

    const stdout = std.io.getStdOut();
    var bw = std.io.bufferedWriter(stdout.writer());
    const w = bw.writer();

    var br = std.io.bufferedReader(file.reader());
    const r = br.reader();

    const all_libs = b: {
        try w.writeAll("Libraries:\n");
        const libs_len = try r.readByte();
        var i: u8 = 0;
        while (i < libs_len) : (i += 1) {
            const lib_name = try r.readUntilDelimiterAlloc(arena, 0, 100);
            try w.print(" {d} lib{s}.so\n", .{ i, lib_name });
            lib_names_buffer[i] = lib_name;
        }
        break :b lib_names_buffer[0..libs_len];
    };

    const all_versions = b: {
        try w.writeAll("Versions:\n");
        const versions_len = try r.readByte();
        var i: u8 = 0;
        while (i < versions_len) : (i += 1) {
            const major = try r.readByte();
            const minor = try r.readByte();
            const patch = try r.readByte();
            if (patch == 0) {
                try w.print(" {d} GLIBC_{d}.{d}\n", .{ i, major, minor });
            } else {
                try w.print(" {d} GLIBC_{d}.{d}.{d}\n", .{ i, major, minor, patch });
            }
            versions_buffer[i] = .{ .major = major, .minor = minor, .patch = patch };
        }
        break :b versions_buffer[0..versions_len];
    };

    const all_targets = b: {
        try w.writeAll("Targets:\n");
        const targets_len = try r.readByte();
        var i: u8 = 0;
        while (i < targets_len) : (i += 1) {
            const target_name = try r.readUntilDelimiterAlloc(arena, 0, 100);
            try w.print(" {d} {s}\n", .{ i, target_name });
            target_names_buffer[i] = target_name;
        }
        break :b target_names_buffer[0..targets_len];
    };

    {
        try w.writeAll("Functions:\n");
        const fns_len = try r.readInt(u16, .little);
        var i: u16 = 0;
        var opt_symbol_name: ?[]const u8 = null;
        while (i < fns_len) : (i += 1) {
            const symbol_name = opt_symbol_name orelse n: {
                const name = try r.readUntilDelimiterAlloc(arena, 0, 100);
                opt_symbol_name = name;
                break :n name;
            };
            try w.print(" {s}:\n", .{symbol_name});
            const targets = try std.leb.readUleb128(u64, r);
            var lib_index = try r.readByte();
            const is_terminal = (lib_index & (1 << 7)) != 0;
            if (is_terminal) {
                lib_index &= ~@as(u8, 1 << 7);
                opt_symbol_name = null;
            }

            var ver_buf: [50]u8 = undefined;
            var ver_buf_index: usize = 0;
            while (true) {
                const byte = try r.readByte();
                const last = (byte & 0b1000_0000) != 0;
                ver_buf[ver_buf_index] = @as(u7, @truncate(byte));
                ver_buf_index += 1;
                if (last) break;
            }
            const versions = ver_buf[0..ver_buf_index];

            try w.print("  library: lib{s}.so\n", .{all_libs[lib_index]});
            try w.writeAll("  versions:");
            for (versions) |ver_index| {
                const ver = all_versions[ver_index];
                if (ver.patch == 0) {
                    try w.print(" {d}.{d}", .{ ver.major, ver.minor });
                } else {
                    try w.print(" {d}.{d}.{d}", .{ ver.major, ver.minor, ver.patch });
                }
            }
            try w.writeAll("\n");

            try w.writeAll("  targets:");
            for (all_targets, 0..) |target, target_i| {
                if ((targets & (@as(u64, 1) << @as(u6, @intCast(target_i)))) != 0) {
                    try w.print(" {s}", .{target});
                }
            }
            try w.writeAll("\n");
        }
    }

    {
        try w.writeAll("Objects:\n");
        const objects_len = try r.readInt(u16, .little);
        var i: u16 = 0;
        var opt_symbol_name: ?[]const u8 = null;
        while (i < objects_len) : (i += 1) {
            const symbol_name = opt_symbol_name orelse n: {
                const name = try r.readUntilDelimiterAlloc(arena, 0, 100);
                opt_symbol_name = name;
                break :n name;
            };
            try w.print(" {s}:\n", .{symbol_name});
            const targets = try std.leb.readUleb128(u64, r);
            const size = try std.leb.readUleb128(u16, r);
            var lib_index = try r.readByte();
            const is_terminal = (lib_index & (1 << 7)) != 0;
            if (is_terminal) {
                lib_index &= ~@as(u8, 1 << 7);
                opt_symbol_name = null;
            }

            var ver_buf: [50]u8 = undefined;
            var ver_buf_index: usize = 0;
            while (true) {
                const byte = try r.readByte();
                const last = (byte & 0b1000_0000) != 0;
                ver_buf[ver_buf_index] = @as(u7, @truncate(byte));
                ver_buf_index += 1;
                if (last) break;
            }
            const versions = ver_buf[0..ver_buf_index];

            try w.print("  size: {d}\n", .{size});
            try w.print("  library: lib{s}.so\n", .{all_libs[lib_index]});
            try w.writeAll("  versions:");
            for (versions) |ver_index| {
                const ver = all_versions[ver_index];
                if (ver.patch == 0) {
                    try w.print(" {d}.{d}", .{ ver.major, ver.minor });
                } else {
                    try w.print(" {d}.{d}.{d}", .{ ver.major, ver.minor, ver.patch });
                }
            }
            try w.writeAll("\n");

            try w.writeAll("  targets:");
            for (all_targets, 0..) |target, target_i| {
                if ((targets & (@as(u64, 1) << @as(u6, @intCast(target_i)))) != 0) {
                    try w.print(" {s}", .{target});
                }
            }
            try w.writeAll("\n");
        }
    }

    try bw.flush();
}
