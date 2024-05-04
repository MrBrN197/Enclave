const c = @import("./c.zig");
const eprint = @import("./root.zig").eprint;
const eprintln = @import("./root.zig").eprintln;
const eql = @import("./root.zig").eql;
const Node = @import("./node.zig").Node;
const Parser = @import("./parser.zig").Parser;
const std = @import("std");
const tsnode = @import("./node.zig");

const assert = std.debug.assert;
const mem = std.mem;
const log = std.log;

var gpa_allocator = std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = true,
}){};
const gpa = gpa_allocator.allocator();

pub const std_options = std.Options{ .log_level = .debug };

pub fn main() void {
    var argv = std.process.args();
    assert(argv.skip());

    var count: usize = 0;
    while (argv.next()) |filepath| {
        count += 1;
        eprintln("=" ** 20 ++ "'{s}'" ++ "=" ** 20, .{filepath});
        convert_file(filepath) catch |e| {
            switch (e) {
                error.FileNotFound => eprintln("File Path Not Found: {s}", .{filepath}),
                error.IsDir => std.log.warn("TODO: handle directories \"{s}\"", .{filepath}),

                else => {
                    eprintln("Failed to Read File {s}", .{filepath});
                    eprintln("{s}", .{@errorName(e)});
                    std.process.exit(@truncate(@intFromError(e))); // FIX:
                },
            }
        };
    }

    if (count == 0) {
        eprintln("error: filepath required", .{});
        std.process.exit(1);
    }
}

pub fn convert_file(filepath: []const u8) !void {
    const as_kb: usize = @shlExact(1, 20);
    const as_mb: usize = @shlExact(1, 30);

    gpa_allocator.setRequestedMemoryLimit(as_mb * 256);

    const buffer = try gpa.alloc(u8, as_kb * 64);
    const read: []const u8 = try std.fs.cwd().readFile(filepath, buffer);

    assert(read.len < buffer.len); // FIX:

    var wrapper = Parser.init(read, gpa);

    const items = wrapper.parse();
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();

    for (items) |item| item.serialize(writer) catch unreachable;

    try wrapper.deinit();
}
