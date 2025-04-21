major: u32,
minor: u32,
patch: u32 = 0,

pub const Range = struct {
    min: Version,
    max: Version,

    pub fn includesVersion(self: Range, ver: Version) bool {
        if (self.min.order(ver) == .gt) return false;
        if (self.max.order(ver) == .lt) return false;
        return true;
    }

    /// Checks if system is guaranteed to be at least `version` or older than `version`.
    /// Returns `null` if a runtime check is required.
    pub fn isAtLeast(self: Range, ver: Version) ?bool {
        if (self.min.order(ver) != .lt) return true;
        if (self.max.order(ver) == .lt) return false;
        return null;
    }
};

pub fn order(lhs: Version, rhs: Version) std.math.Order {
    if (lhs.major < rhs.major) return .lt;
    if (lhs.major > rhs.major) return .gt;
    if (lhs.minor < rhs.minor) return .lt;
    if (lhs.minor > rhs.minor) return .gt;
    if (lhs.patch < rhs.patch) return .lt;
    if (lhs.patch > rhs.patch) return .gt;
    return .eq;
}

pub fn parse(text: []const u8) !Version {
    var end: usize = 0;
    while (end < text.len) : (end += 1) {
        const c = text[end];
        if (!std.ascii.isDigit(c) and c != '.') break;
    }
    // found no digits or '.' before unexpected character
    if (end == 0) return error.InvalidVersion;

    var it = std.mem.splitScalar(u8, text[0..end], '.');
    // substring is not empty, first call will succeed
    const major = it.first();
    if (major.len == 0) return error.InvalidVersion;
    const minor = it.next() orelse "0";
    // ignore 'patch' if 'minor' is invalid
    const patch = if (minor.len == 0) "0" else (it.next() orelse "0");

    return Version{
        .major = try std.fmt.parseUnsigned(u32, major, 10),
        .minor = try std.fmt.parseUnsigned(u32, if (minor.len == 0) "0" else minor, 10),
        .patch = try std.fmt.parseUnsigned(u32, if (patch.len == 0) "0" else patch, 10),
    };
}

pub fn format(
    self: Version,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    out_stream: anytype,
) !void {
    _ = options;
    if (fmt.len == 0) {
        if (self.patch == 0) {
            if (self.minor == 0) {
                return std.fmt.format(out_stream, "{d}", .{self.major});
            } else {
                return std.fmt.format(out_stream, "{d}.{d}", .{ self.major, self.minor });
            }
        } else {
            return std.fmt.format(out_stream, "{d}.{d}.{d}", .{ self.major, self.minor, self.patch });
        }
    } else {
        std.fmt.invalidFmtError(fmt, self);
    }
}

const Version = @This();
const std = @import("std");
