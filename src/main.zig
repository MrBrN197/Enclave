const std = @import("std");

const c = @import("./c.zig");

const Node = @import("./node.zig").Node;
const tsnode = @import("./node.zig");

const assert = std.debug.assert;
const mem = std.mem;
const log = std.log;

const eprintln = @import("./root.zig").eprintln;
const eprint = @import("./root.zig").eprint;

pub fn eql(
    a: []const u8,
    b: []const u8,
) bool {
    return std.mem.eql(u8, a, b);
}

pub fn out(comptime str: []const u8) void {
    eprint("{s}", .{str});
}

pub const std_options = std.Options{ .log_level = .info };

pub fn main() !void {
    _ = try convert_file("./example.rs");
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

    wrapper.loop_tree();

    try wrapper.free();
}

fn log_unhandled_type(node: c.TSNode) void {
    const name = tsnode.get_sym_name(node, gpa);

    // if (eql(name, "identifier")) return; // TODO:
    // if (eql(name, "scoped_identifier")) return; // TODO:
    // if (eql(name, "scoped_use_list")) return; // TODO:
    // if (eql(name, "use_list")) return; // TODO:

    eprintln("Unhandled sym: {{{s}}}", .{name});
}

pub const Parser = struct {
    tree: ?*c.TSTree,
    parser: ?*c.TSParser,
    lines_iter: Lines,

    const Lines = mem.SplitIterator(u8, .scalar);
    const TextSlice = []const []const u8;

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

    fn loop_tree(self: *const Parser) void {
        var i: usize = 0;
        var lines = self.lines_iter;

        lines.reset();

        const root_node = c.ts_tree_root_node(self.tree);

        var queue = std.ArrayList(c.TSNode).init(gpa);
        queue.append(root_node) catch unreachable;

        while (queue.items.len > 0) : (i += 1) {
            const node = queue.pop();

            const current_node = Node.init(node, gpa);

            eprint("\n" ++ "=" ** 20, .{});
            eprint("{s}", .{current_node.sym});
            eprintln("=" ** 20, .{});

            const stdout = std.io.getStdOut();
            const writer = stdout.writer();

            current_node.write_to(self, writer);
            // if (eql(current_node.sym, "function_item")) {
            //     self.function_item_str(current_node);
            // } else if (eql(current_node.sym, "use_declaration")) {
            //     // self.use_declaration_str(current_node);
            // } else if (eql(current_node.sym, "enum_item")) {
            //     self.enum_item_str(current_node);
            // } else if (eql(current_node.sym, "extern_crate_declaration")) {
            //     self.extern_crate_declaration_str(current_node);
            // } else if (eql(current_node.sym, "type_item")) {
            //     self.type_item_str(current_node);
            // } else if (eql(current_node.sym, "call_expression")) {
            //     self.call_expression_str(current_node);
            // } else if (eql(current_node.sym, "struct_item")) {
            //     self.struct_item_str(current_node);
            // } else if (eql(current_node.sym, "attribute_item")) {
            //     // self.attribute_item_str(current_node);
            // } else {
            //     log_unhandled_type(current_node);

            //     var children = tsnode.get_children_named(current_node);
            //     defer children.clearAndFree();

            //     for (children.items) |child| {
            //         assert(!c.ts_node_is_null(child)); // TODO: remove
            //         queue.append(child) catch unreachable;
            //     }
            // }
        }
    }

    pub fn node_to_string(self: *const Parser, node: c.TSNode) TextSlice {
        const start = c.ts_node_start_point(node);
        const end = c.ts_node_end_point(node);
        const name = self.get_text_at(start, end);
        return name;
    }

    fn get_text_at(self: *const Parser, start_pt: c.TSPoint, end_pt: c.TSPoint) TextSlice {
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

    /// Convert Type to string
    /// Convert Abstract Type to string
    /// Convert Reference Type to string
    /// Convert Metavariable Type string
    const A = struct { i32, i32 };
};

// fn node_type_is_one_of(node: c.TSNode, filter: []const []const u8) bool {
//     const _node_name = c.ts_node_type(node);
//     const node_name = _node_name[0..mem.len(_node_name)];

//     eprintln("len: {}", .{node_name.len});
//     for (filter) |name| {
//         eprintln("name: {s}", .{node_name});
//         if (mem.eql(name, node_name)) return true;
//     }

//     return false;
// }

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
