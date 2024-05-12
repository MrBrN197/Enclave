const std = @import("std");

pub fn eql(
    a: []const u8,
    b: []const u8,
) bool {
    return std.mem.eql(u8, a, b);
}

pub fn isEmpty(str: []const u8) bool {
    if (str.len == 0) return true;

    for (str) |c| if (c != ' ') return false; // FIX:
    return true;
}
