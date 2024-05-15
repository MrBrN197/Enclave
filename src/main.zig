const c = @import("./c.zig");
const eprint = @import("./root.zig").eprint;
const eprintln = @import("./root.zig").eprintln;
const eql = @import("./root.zig").eql;
const log = std.log;
const mem = std.mem;
const std = @import("std");
const tsnode = @import("./node.zig");
const debug = std.debug;

const as_kb: usize = @shlExact(1, 20);
const as_mb: usize = @shlExact(1, 30);

const Parser = @import("./parser.zig").Parser;
const Node = @import("./node.zig").Node;
const NodeItem = @import("./node.zig").NodeItem;
const SerializeContext = @import("./node/items/item.zig").SerializeContext;
const Impl = @import("./node/items/item.zig").Impl;
const File = std.fs.File;

pub const std_options = std.Options{ .log_level = .warn };

var gpa_allocator = std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = true,
}){};

const gpa = gpa_allocator.allocator();

pub fn main() !void {
    gpa_allocator.setRequestedMemoryLimit(as_mb * 256);

    var argv = std.process.args();
    if (!argv.skip()) return error.InvalidArg;

    var count: usize = 0;

    var filepaths = std.ArrayList([]const u8).init(gpa);

    while (argv.next()) |filepath| {
        filepaths.append(filepath) catch return error.InvalidArgPath;
        count += 1;
    }

    try convert_files(filepaths.items, (std.io.getStdOut().writer()));

    if (count == 0) {
        std.log.err("error: filepath required", .{});
        std.process.exit(1);
    }

    std.log.info("processed {} files", .{count});
}

pub fn convert_files(filepaths: []const []const u8, writer: anytype) !void {
    _ = writer; // autofix
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

    var ctx_items = std.StringHashMap(NodeItem).init(gpa); //FIX;
    var implementations = std.ArrayList(Impl).init(gpa); //FIX;

    for (parse_results.items) |parse_result| {
        const module = parse_result.parserd_module.module;
        for (module.node_items.items) |node_item| {
            switch (node_item.data) {
                .impl_item => |impl| try implementations.append(impl),
                else => try ctx_items.putNoClobber(node_item.name.?, node_item),
            }
        }
    }

    const ctx = SerializeContext{
        .items = ctx_items,
        .impls = implementations.items,
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

        for (parse_result.parserd_module.module.node_items.items) |item| {
            if (item.data == .impl_item) continue;

            item.serialize(outfile.writer(), ctx) catch unreachable;
            // item.serialize(writer, ctx) catch unreachable;
        }
    }
}
