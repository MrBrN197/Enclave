const c = @import("./c.zig");
const eprint = @import("./root.zig").eprint;
const eprintln = @import("./root.zig").eprintln;
const eql = @import("./root.zig").eql;
const Node = @import("./node.zig").Node;
const NodeItem = @import("./node.zig").NodeItem;

const Parser = @import("./parser.zig").Parser;
const std = @import("std");
const tsnode = @import("./node.zig");

const assert = std.debug.assert;
const mem = std.mem;
const log = std.log;

const as_kb: usize = @shlExact(1, 20);
const as_mb: usize = @shlExact(1, 30);

var gpa_allocator = std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = true,
}){};

const gpa = gpa_allocator.allocator();

pub const std_options = std.Options{ .log_level = .info };

pub fn main() void {
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();

    gpa_allocator.setRequestedMemoryLimit(as_mb * 512);

    var argv = std.process.args();
    assert(argv.skip());

    var count: usize = 0;
    while (argv.next()) |filepath| {
        std.log.info("{s:>50}", .{filepath});
        const result = extract_items_from_file(filepath) catch |e| {
            switch (e) {
                error.FileNotFound => std.log.warn("file path not found: {s}", .{filepath}),
                error.IsDir => std.log.warn("skipping directory name \"{s}\"", .{filepath}),

                else => {
                    std.log.err("failed to read file {s}", .{filepath});
                    std.log.err("{s}", .{@errorName(e)});
                    std.process.exit(@truncate(@intFromError(e))); // FIX:
                },
            }
            continue;
        };

        for (result.node_items) |item| item.serialize(writer) catch unreachable;

        count += 1;
    }

    if (count == 0) {
        std.log.err("error: filepath required", .{});
        std.process.exit(1);
    }
    std.log.info("processed {} files", .{count});
}

const Items = struct {
    node_items: []const NodeItem,
};

pub fn extract_items_from_file(filepath: []const u8) !Items {
    const filesize = (try std.fs.cwd().statFile(filepath)).size;

    const buffer = try gpa.alloc(u8, filesize + 1);
    defer gpa.free(buffer);

    const read: []const u8 = try std.fs.cwd().readFile(filepath, buffer);

    assert(read.len == (buffer.len - 1));

    var wrapper = Parser.init(read, gpa);
    defer wrapper.deinit();

    const items = wrapper.parse();

    return .{ .node_items = items };
}
