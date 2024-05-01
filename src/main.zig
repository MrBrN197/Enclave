const std = @import("std");

const c = @import("./c.zig");

const Node = @import("./node.zig").Node;
const tsnode = @import("./node.zig");

const assert = std.debug.assert;
const mem = std.mem;
const log = std.log;

const eprintln = @import("./root.zig").eprintln;
const eprint = @import("./root.zig").eprint;

const eql = @import("./root.zig").eql;

pub fn out(comptime str: []const u8) void {
    eprint("{s}", .{str});
}

pub const std_options = std.Options{ .log_level = .info };

pub fn main() void {
    const filepath = "./examples/example.rs";
    _ = convert_file(filepath) catch |e| {
        switch (e) {
            error.FileNotFound => eprintln("File Path Not Found: {s}", .{filepath}),

            else => {
                eprintln("Failed to Read File {s}", .{filepath});
                eprintln("{s}", .{@errorName(e)});
            },
        }
    };
}

var gpa_allocator = std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = true,
}){};

const gpa = gpa_allocator.allocator();

pub fn convert_file(filepath: []const u8) !void {
    const as_kb: usize = @shlExact(1, 20);
    const as_mb: usize = @shlExact(1, 30);

    gpa_allocator.setRequestedMemoryLimit(as_mb * 256);

    const buffer = try gpa.alloc(u8, as_kb * 64);
    const read: []const u8 = try std.fs.cwd().readFile(filepath, buffer);

    assert(read.len < buffer.len);

    var wrapper = Parser.init(read);

    wrapper.print_syntax_tree();

    try wrapper.free();
}

pub const Parser = struct {
    tree: ?*c.TSTree,
    parser: ?*c.TSParser,
    lines_iter: Lines,

    const Lines = mem.SplitIterator(u8, .scalar);

    fn init(source_code: []const u8) @This() {
        const parser = c.ts_parser_new();

        assert(c.ts_parser_set_language(parser, c.tree_sitter_rust()));

        const tree = c.ts_parser_parse_string(
            parser,
            null,
            source_code.ptr,
            @intCast(source_code.len),
        );

        return Parser{
            .lines_iter = mem.splitScalar(u8, source_code, '\n'),
            .tree = tree,
            .parser = parser,
        };
    }

    fn free(self: *Parser) !void {
        // Free all of the heap-allocated memory.
        // c.free(string);
        c.ts_tree_delete(self.tree);
        c.ts_parser_delete(self.parser);

        return;
    }

    fn print_syntax_tree(self: *const Parser) void {
        var lines = self.lines_iter;

        lines.reset();

        const root_node = c.ts_tree_root_node(self.tree);
        const current_node = Node.init(root_node, gpa);
        // if (true) continue;

        eprint("\n" ++ "=" ** 20, .{});
        eprint("{s}", .{current_node.sym});
        eprintln("=" ** 20, .{});

        const stdout = std.io.getStdOut();
        const writer = stdout.writer();

        current_node.write_to(self, writer);
    }

    pub fn node_to_string(self: *const Parser, node: c.TSNode, allocator: std.mem.Allocator) []const u8 {
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

        return buffer[0..idx];
    }

    fn get_text_at(self: *const Parser, start_pt: c.TSPoint, end_pt: c.TSPoint) []const []const u8 {
        var lines = self.lines_iter;

        var skip_num_rows = start_pt.row;

        while (skip_num_rows > 0) : (skip_num_rows -= 1) {
            _ = lines.next() orelse unreachable;
        }
        var selected_rows = std.ArrayList([]const u8).init(gpa);

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

    fn qualified_type(self: *const Parser, node: c.TSNode) void {
        const type_field = tsnode.get_field(node, "type") orelse unreachable; // $_type
        self.type_str(type_field);
        const alias_field = tsnode.get_field(node, "alias") orelse unreachable; // $_type
        self.type_str(alias_field);
    }
};
