const std = @import("std");

const c = @cImport({
    @cInclude("string.h");
    @cInclude("tree_sitter/api.h");
    @cInclude("tree_sitter/tree-sitter-rust.h");
});

const assert = std.debug.assert;
const eprint = std.debug.print;
const eql = std.mem.eql;
const mem = std.mem;
const log = std.log;

fn eprintln(comptime str: []const u8, args: anytype) void {
    return std.debug.print(str ++ "\n", args);
}

pub fn main() !void {
    _ = convert_file("./example.rs");
}

var gpa_allocator = std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = true,
}){};

const gpa = gpa_allocator.allocator();

pub fn convert_file(filepath: []const u8) void {
    const as_kb: usize = @shlExact(1, 20);

    gpa_allocator.setRequestedMemoryLimit(as_kb * 128);

    const buffer = gpa.alloc(u8, as_kb * 64) catch unreachable;
    const read: []const u8 = std.fs.cwd().readFile(filepath, buffer) catch unreachable;

    const lines = mem.splitScalar(u8, read, '\n');

    assert(read.len < buffer.len);

    var wrapper = tree_sitter_parser(read);

    loop_tree(&wrapper, lines);

    // const fn_node = c.ts_node_named_child(root_node, 0);
    // const root_node_name = c.ts_node_type(root_node);

    // eprintln("root_node: {s}\n", .{root_node_name});

    // const source_file_ = c.ts_node_type(root_node);
    // const source_file = source_file_[0..std.mem.len(source_file_)];
    // assert(std.mem.eql(u8, source_file, "source_file"));

    // const function_item_ = c.ts_node_type(fn_node);
    // const function_item = function_item_[0..std.mem.len(function_item_)];
    // assert(std.mem.eql(u8, function_item, "function_item"));

    // eprintln("root_node count:           {d}", .{c.ts_node_child_count(root_node)});
    // eprintln("fn_node count:             {d}", .{c.ts_node_child_count(fn_node)});
    // eprintln("fn_node named child count: {d}", .{c.ts_node_named_child_count(fn_node)});

    // const string = c.ts_node_string(root_node);
    // eprintln("Syntax tree: {s}", .{string});
    try wrapper.free();
}

const TSTreeWrapper = struct {
    tree: ?*c.TSTree,
    parser: ?*c.TSParser,

    fn free(self: *TSTreeWrapper) !void {
        // Free all of the heap-allocated memory.
        // c.free(string);
        c.ts_tree_delete(self.tree);
        c.ts_parser_delete(self.parser);

        return;
    }
};

fn get_node_name(node: c.TSNode) []const u8 {
    const node_name = c.ts_node_type(node);
    return node_name[0..mem.len(node_name)];
}

fn is_of_type(node: c.TSNode, name: []const u8) bool {
    const c_node_name = c.ts_node_type(node);
    const node_name = c_node_name[0..mem.len(c_node_name)];
    return eql(u8, node_name, name);
}

const Lines = mem.SplitIterator(u8, .scalar);

fn get_text_at(start_pt: c.TSPoint, end_pt: c.TSPoint, lines_iter: Lines) []const []const u8 {
    var lines = lines_iter;

    var skip_num_rows = start_pt.row;

    while (skip_num_rows > 0) : (skip_num_rows -= 1) {
        _ = lines.next() orelse unreachable;
    }
    var selected_rows = std.ArrayList([]const u8).init(gpa);

    assert(start_pt.row <= end_pt.row);
    var num_rows = (end_pt.row - start_pt.row) + 1;

    eprintln(
        "NumRows: {}",
        .{num_rows},
    );
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

fn loop_tree(wrapper: *const TSTreeWrapper, buffer: Lines) void {
    var i: usize = 0;
    var lines = buffer;
    lines.reset();

    const root_node = c.ts_tree_root_node(wrapper.tree);

    var queue = std.ArrayList(c.TSNode).init(gpa);
    queue.append(root_node) catch unreachable;

    while (queue.items.len > 0) : (i += 1) {
        const current_node = queue.pop();

        var children = std.ArrayList(c.TSNode).init(gpa); // TODO: move
        defer children.clearAndFree();

        const child_count = c.ts_node_named_child_count(current_node);

        for (0..child_count) |idx| {
            eprintln("Getting Child: {}", .{idx});
            const child_node = c.ts_node_child(current_node, @intCast(idx));
            assert(!c.ts_node_is_null(child_node));
            children.append(child_node) catch unreachable;
        }

        for (children.items, 0..) |child, idx| {
            const child_name = c.ts_node_type(child);

            const name = child_name[0..std.mem.len(child_name)];
            if (eql(u8, "function_item", name)) {
                const field_name = "name";
                const name_field = c.ts_node_child_by_field_name(child, field_name, field_name.len);

                const point = c.ts_node_start_point(name_field);
                const end_point = c.ts_node_end_point(name_field);

                const text = get_text_at(point, end_point, lines);

                log.debug("loc {}:{} - {}:{} ==> ", .{
                    point.row + 1,
                    point.column + 1,
                    end_point.row + 1,
                    end_point.column + 1,
                });

                std.log.debug("[function_item]:", .{});
                for (text) |line| {
                    eprintln("\t\"{s}\"", .{line});
                }
            } else {
                eprintln("Appending Child: {:>2} {s}", .{ idx, child_name });
            }

            queue.append(child) catch unreachable;
        }
    }
}

fn tree_sitter_parser(source_code: []const u8) TSTreeWrapper {
    const parser = c.ts_parser_new();

    assert(c.ts_parser_set_language(parser, c.tree_sitter_rust()));

    const tree = c.ts_parser_parse_string(
        parser,
        null,
        source_code.ptr,
        @intCast(source_code.len),
    );

    return TSTreeWrapper{
        .tree = tree,
        .parser = parser,
    };
}
test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
