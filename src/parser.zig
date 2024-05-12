const c = @import("./c.zig");
const root = @import("./root.zig");
const std = @import("std");
const tsnode = @import("./node.zig");

const assert = std.debug.assert;
const eprintln = root.eprintln;
const eprint = root.eprint;
const mem = std.mem;

const Module = @import("./node/item.zig").Module;
const NodeItem = @import("./node/types.zig").NodeItem;
const Node = tsnode.Node;
const TypeItem = @import("./node/types.zig").NodeItem.Data.TypeItem;

pub const Parser = struct {
    allocator: mem.Allocator,
    lines_iter: Lines,
    parser: ?*c.TSParser,
    tree: ?*c.TSTree,

    const Lines = mem.SplitIterator(u8, .scalar);
    const Self = @This();

    pub fn init(source_code: []const u8, allocator: mem.Allocator) @This() {
        const parser = c.ts_parser_new();

        assert(c.ts_parser_set_language(parser, c.tree_sitter_rust()));

        const tree = c.ts_parser_parse_string(
            parser,
            null,
            source_code.ptr,
            @intCast(source_code.len),
        );

        return Parser{
            .allocator = allocator,
            .lines_iter = mem.splitScalar(u8, source_code, '\n'),
            .parser = parser,
            .tree = tree,
        };
    }

    pub fn deinit(self: Parser) void {
        // Free all of the heap-allocated memory.
        // c.free(string);
        c.ts_tree_delete(self.tree);
        c.ts_parser_delete(self.parser);

        return;
    }

    pub fn print_source(self: *const Parser, node: c.TSNode) void {
        const start = c.ts_node_start_point(node);
        const source = self.node_to_string_alloc(node, self.allocator); // FIX: clear

        const row = start.row + 1;
        const column = start.column + 1;

        std.debug.print(
            "=" ** 20 ++ " .{[tag]s} " ++ "=" ** 20 ++
                \\ {[row]}:{[column]}
                \\ {[source]s}
                \\
                \\
            ,

            .{ .row = row, .column = column, .source = source, .tag = c.ts_node_type(node) },
        );

        return;
    }

    const ParseResult = struct {
        filepath: []const u8,
        parserd_module: ParsedModule,
    };

    pub fn parseFiles(allocator: std.mem.Allocator, filepaths: []const []const u8) !std.ArrayList(ParseResult) {
        var results = std.ArrayList(ParseResult).init(allocator);

        for (filepaths) |filepath| {
            std.log.info("filePath: {s}", .{filepath});
            const filesize = (std.fs.cwd().statFile(filepath) catch unreachable).size;

            const buffer = allocator.alloc(u8, filesize + 1) catch unreachable;
            defer allocator.free(buffer);

            const read: []const u8 = std.fs.cwd().readFile(filepath, buffer) catch unreachable;

            assert(read.len == (buffer.len - 1));
            const parser = Parser.init(read, allocator);

            const parsed_module = parser.parse();
            // defer parser.deinit(); // FIX:

            results.append(ParseResult{
                .parserd_module = parsed_module.*,
                .filepath = filepath,
            }) catch unreachable;
        }

        return results;
    }

    const ParsedModule = struct {
        module: Module,
    };

    pub fn parse(
        self: *const Self,
    ) *ParsedModule {
        var lines = self.lines_iter;
        lines.reset();

        const root_node = c.ts_tree_root_node(self.tree);
        assert(!c.ts_node_is_null(root_node));

        var collect = std.ArrayList(NodeItem).init(self.allocator);
        const ctx = .{
            .items = &collect,
            .modules = &[_]Module{},
            .parser = self,
        };

        var current_node = Node.init_with_context(self.allocator, root_node, ctx);

        // const module = current_node.extract_node_items();
        const result = current_node.extract_module();

        const ptr = self.allocator.create(ParsedModule) catch unreachable;
        ptr.* = .{
            .module = result,
        };
        return ptr;
    }

    pub fn node_to_string_alloc(self: *const Parser, node: c.TSNode, allocator: mem.Allocator) []const u8 {
        const start = c.ts_node_start_point(node);
        const end = c.ts_node_end_point(node);
        const lines = self.get_text_at(start, end);

        var size: usize = 0;
        for (lines) |slice| {
            size += slice.len;
        }
        const buffer = allocator.alloc(u8, size) catch unreachable;

        var idx: usize = 0;
        for (lines) |slice| {
            std.mem.copyForwards(u8, buffer[idx..], slice);
            idx += slice.len;
        }

        assert(idx == size);
        assert(!mem.eql(u8, "", buffer[0..idx]));

        return buffer[0..idx];
    }
    pub fn node_to_string_buf(self: *const Parser, node: c.TSNode, buffer: []u8) []const u8 {
        const start = c.ts_node_start_point(node);
        const end = c.ts_node_end_point(node);
        const lines = self.get_text_at(start, end);

        var size: usize = 0;
        for (lines) |line| size += line.len;

        var idx: usize = 0;
        for (lines) |slice| {
            std.mem.copyForwards(u8, buffer[idx..], slice);
            idx += slice.len;
        }

        return buffer[0..idx];
    }

    pub fn node_to_string(self: *const Parser, node: c.TSNode) []const u8 {
        const start = c.ts_node_start_point(node);
        const end = c.ts_node_end_point(node);
        const lines = self.get_text_at(start, end);

        var size: usize = 0;
        for (lines) |slice| {
            size += slice.len;
        }
        const buffer = self.allocator.alloc(u8, size) catch unreachable;

        var idx: usize = 0;
        for (lines) |slice| {
            std.mem.copyForwards(u8, buffer[idx..], slice);
            idx += slice.len;
        }

        assert(idx == size);
        assert(!mem.eql(u8, "", buffer[0..idx]));

        return buffer[0..idx];
    }

    pub fn get_text_at(self: *const Parser, start_pt: c.TSPoint, end_pt: c.TSPoint) []const []const u8 {
        var lines = self.lines_iter;

        var skip_num_rows = start_pt.row;

        while (skip_num_rows > 0) : (skip_num_rows -= 1) {
            _ = lines.next() orelse unreachable;
        }
        var selected_rows = std.ArrayList([]const u8).init(self.allocator);

        assert(start_pt.row <= end_pt.row);
        var num_rows = (end_pt.row - start_pt.row) + 1;

        while (num_rows > 0) : (num_rows -= 1) {
            selected_rows.append(lines.next() orelse unreachable) catch unreachable;
        }

        const items = selected_rows.items;

        if (start_pt.row == end_pt.row) {
            assert(start_pt.column < end_pt.column);
            items[0] = items[0][start_pt.column..end_pt.column];
        } else {
            const len = items.len;
            items[0] = items[0][start_pt.column..];
            items[len - 1] = items[len - 1][0..end_pt.column];
        }

        return items;
    }
};
