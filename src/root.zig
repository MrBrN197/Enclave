const std = @import("std");
const testing = std.testing;

pub const eprint = std.debug.print;

pub fn eprintln(comptime str: []const u8, args: anytype) void {
    return std.debug.print(str ++ "\n", args);
}

pub fn eql(
    a: []const u8,
    b: []const u8,
) bool {
    return std.mem.eql(u8, a, b);
}

// export fn add(a: i32, b: i32) i32 {
//     return a + b;
// }

// test "basic add functionality" {
//     try testing.expect(add(3, 7) == 10);
// }
