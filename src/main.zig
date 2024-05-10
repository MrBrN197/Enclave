const c = @import("./c.zig");
const eprint = @import("./root.zig").eprint;
const eprintln = @import("./root.zig").eprintln;
const eql = @import("./root.zig").eql;
const Node = @import("./node.zig").Node;
const NodeItem = @import("./node.zig").NodeItem;
const File = std.fs.File;

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

pub const std_options = std.Options{ .log_level = .warn };

pub fn main() void {
    gpa_allocator.setRequestedMemoryLimit(as_mb * 256);

    var argv = std.process.args();
    assert(argv.skip());

    var count: usize = 0;

    var filepaths = std.ArrayList([]const u8).init(gpa);

    while (argv.next()) |filepath| {
        filepaths.append(filepath) catch unreachable;

        count += 1;
    }

    convert_file(filepaths.items, (std.io.getStdOut().writer()));

    if (count == 0) {
        std.log.err("error: filepath required", .{});
        std.process.exit(1);
    }

    std.log.info("processed {} files", .{count});
}

pub fn convert_file(filepaths: []const []const u8, writer: anytype) void {
    const parse_results = Parser.parseFiles(gpa, filepaths) catch |e| {
        switch (e) {
            error.FileNotFound => std.log.warn("file path not found: {s}", .{filepaths}),
            error.IsDir => std.log.warn("skipping directory name \"{s}\"", .{filepaths}),

            else => {
                std.log.err("{s}: failed to read file {s}", .{ filepaths, @errorName(e) });
                std.process.exit(@truncate(@intFromError(e))); // FIX:
            },
        }
        return;
    };

    const suffix = ".zig";
    for (parse_results.items) |parse_result| {
        std.log.info("Item: filepath: {s}", .{parse_result.filepath});

        const filepath = parse_result.filepath;
        const filepath_len = filepath.len;
        const buffer = gpa.alloc(u8, filepath_len + suffix.len) catch unreachable;
        const outfilepath = std.fmt.bufPrint(
            buffer,
            "{s}{s}",
            .{ filepath, suffix },
        ) catch unreachable;

        std.log.info("{s:>50}\n=> {s:>50}", .{ filepath, outfilepath });

        const outfile = std.fs.cwd().createFile(
            outfilepath,
            .{ .exclusive = false },
        ) catch unreachable; // FIX:

        for (parse_result.parserd_module.module.items.items) |item| {
            item.serialize(outfile.writer()) catch unreachable;
            item.serialize(writer) catch unreachable;
        }
    }
}
