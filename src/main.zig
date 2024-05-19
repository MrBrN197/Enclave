const c = @import("./c.zig");
const debug = std.debug;
const eprint = @import("./root.zig").eprint;
const eprintln = @import("./root.zig").eprintln;
const eql = @import("./root.zig").eql;
const items = @import("./node/items/item.zig");
const log = std.log;
const mem = std.mem;
const std = @import("std");
const tsnode = @import("./node.zig");

const byte_per_kb: usize = @shlExact(1, 20);
const byte_per_mb: usize = @shlExact(1, 30);

const File = std.fs.File;
const Identifier = items.Identifier;
const Impl = items.Impl;
const Import = items.Import;
const Node = @import("./node.zig").Node;
const NodeItem = @import("./node.zig").NodeItem;
const Parser = @import("./parser.zig").Parser;
const SerializeContext = @import("./node/items/item.zig").SerializeContext;

pub const std_options = std.Options{
    .log_level = .info,
};

var gpa_allocator = std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = true,
}){};

const gpa = gpa_allocator.allocator();

pub fn main() !void {
    gpa_allocator.setRequestedMemoryLimit(byte_per_mb * 256);

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
    const parse_results = try Parser.parseFiles(gpa, filepaths);

    var ctx_items = SerializeContext.ItemsMap.init(gpa); //FIX;

    var implementations = std.ArrayList(Impl).init(gpa); //FIX;
    var imports = std.ArrayList(Import).init(gpa);

    var count: usize = 0;
    for (parse_results.items) |parse_result| {
        const module = parse_result.parserd_module.module;
        for (module.node_items.items) |node_item| {
            switch (node_item.data) {
                .impl_item => |impl| try implementations.append(impl),
                .import_item => |import| try imports.append(import),
                else => {
                    count += 1;

                    std.log.err("\n\nItem Name =>  {s} {}", .{ node_item.name.?, count });

                    try ctx_items.putNoClobber(node_item.name.?, node_item);
                },
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
            item.serialize(writer, ctx) catch unreachable;
        }
    }
}
