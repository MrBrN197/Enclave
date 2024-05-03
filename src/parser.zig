const c = @import("./c.zig");
const NodeItem = @import("./node/types.zig").NodeItem;
const TypeItem = @import("./node/types.zig").NodeItem.ItemData.TypeItem;
const root = @import("./root.zig");
const std = @import("std");
const tsnode = @import("./node.zig");

const assert = std.debug.assert;
const eprintln = root.eprintln;
const eprint = root.eprint;
const mem = std.mem;
const Node = tsnode.Node;

pub const Parser = struct {
    lines_iter: Lines,
    parser: ?*c.TSParser,
    tree: ?*c.TSTree,
    allocator: mem.Allocator,

    const Lines = mem.SplitIterator(u8, .scalar);

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

    pub fn deinit(self: *Parser) !void {
        // Free all of the heap-allocated memory.
        // c.free(string);
        c.ts_tree_delete(self.tree);
        c.ts_parser_delete(self.parser);

        return;
    }

    pub fn print_syntax_tree(self: *const Parser) void {
        _ = self; // autofix
        // var lines = self.lines_iter;
        // lines.reset(); // TODO:

        // const root_node = c.ts_tree_root_node(self.tree);
        // const current_node = Node.init(root_node, self.allocator);

        // eprint("\n" ++ "=" ** 20, .{});
        // eprint("{s}", .{current_node.sym});
        // eprintln("=" ** 20, .{});

        // const stdout = std.io.getStdOut();
        // const writer = stdout.writer();

        // current_node.write_to(self, writer);
    }

    const Self = @This();
    pub fn parse(self: *const Self) [](NodeItem) {
        var lines = self.lines_iter;
        lines.reset(); //TODO:

        var result = std.ArrayList(NodeItem).init(self.allocator);
        const root_node = c.ts_tree_root_node(self.tree);
        assert(!c.ts_node_is_null(root_node));

        const current_node = Node.init(root_node, self.allocator);

        const type_items = current_node.extract_type_items(self);
        _ = type_items; // autofix

        current_node.extract_node_items(self, &result);

        return result.items;
    }

    pub fn node_to_string(self: *const Parser, node: c.TSNode, allocator: mem.Allocator) []const u8 {
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
