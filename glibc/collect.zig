const std = @import("std");
const Version = @import("Version.zig");
const Target = std.Target;
const mem = std.mem;
const log = std.log;
const fs = std.fs;

pub fn main() !void {
    var arena_instance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);
    const glibc_src_path = args[1];

    const stdout = try exec(arena, &.{ "git", "-C", glibc_src_path, "describe", "--tags" });
    if (!mem.startsWith(u8, stdout, "glibc-")) {
        fatal("the git repository provided does not have an official glibc release tag checked out. git describe returned '{s}'", .{stdout});
    }
    const ver_text = mem.trimRight(u8, stdout["glibc-".len..], " \n\r");
    const ver = Version.parse(ver_text) catch |err| {
        fatal("unable to parse '{s}': {s}", .{ ver_text, @errorName(err) });
    };
    const search_dir_path = try fs.path.join(arena, &.{ glibc_src_path, "sysdeps" });
    const dest_dir_path = try std.fmt.allocPrint(arena, "{d}.{d}/sysdeps", .{
        ver.major, ver.minor,
    });

    var dest_dir = try std.fs.cwd().makeOpenPath(dest_dir_path, .{});
    defer dest_dir.close();

    var glibc_src_dir = try std.fs.cwd().openDir(search_dir_path, .{ .iterate = true });
    defer glibc_src_dir.close();

    var walker = try glibc_src_dir.walk(arena);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (!std.mem.endsWith(u8, entry.basename, ".abilist"))
            continue;

        if (fs.path.dirname(entry.path)) |dirname| {
            try dest_dir.makePath(dirname);
        }
        try glibc_src_dir.copyFile(entry.path, dest_dir, entry.path, .{});
    }
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    log.err(format, args);
    std.process.exit(1);
}

fn exec(arena: mem.Allocator, argv: []const []const u8) ![]const u8 {
    const child_result = try std.process.Child.run(.{
        .allocator = arena,
        .argv = argv,
        .max_output_bytes = 200 * 1024 * 1024,
    });
    if (child_result.stderr.len != 0) {
        fatal("{s}", .{child_result.stderr});
    }

    switch (child_result.term) {
        .Exited => |code| if (code == 0) return child_result.stdout else {
            fatal("{s} exited with code {d}", .{ argv[0], code });
        },
        else => {
            fatal("{s} crashed", .{argv[0]});
        },
    }
}
