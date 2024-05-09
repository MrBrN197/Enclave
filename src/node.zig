const c = @import("./c.zig");
const node_types = @import("./node/types.zig");
const parse = @import("./parser.zig"); // TODO:
const root = @import("./root.zig");
const std = @import("std");
const path = @import("./node/path.zig");

const assert = std.debug.assert;
const eprintln = root.eprintln;
const eprint = root.eprint;
const eql = root.eql;
const is_empty = root.is_empty;
const mem = std.mem;
const Parser = parse.Parser;
const Writer = @TypeOf(std.io.getStdOut().writer());
const Allocator = std.mem.Allocator;

pub const IdentifierKind = node_types.IdentifierKind;
pub const NodeItem = node_types.NodeItem;
pub const NodeType = node_types.NodeType;
pub const ImportPath = node_types.ImportPath;
pub const PathParser = path.PathParser;
pub const ImportPathParser = path.ImportPathParser;
pub const TypeKind = NodeItem.Data.TypeKind;

fn out(writer: Writer, comptime str: []const u8, args: anytype) void {
    return std.fmt.format(writer, str, args) catch unreachable;
}

pub fn get_child_named(node: c.TSNode, idx: u32) ?c.TSNode {
    const result = c.ts_node_named_child(node, idx);
    return if (c.ts_node_is_null(node)) null else result;
}

pub fn is_of_type(node: c.TSNode, name: []const u8) bool {
    const c_node_name = c.ts_node_type(node);
    const node_name = c_node_name[0..mem.len(c_node_name)];
    return eql(node_name, name);
}

pub fn get_type(node: c.TSNode, allocator: std.mem.Allocator) []const u8 { // TODO: remove
    const node_name = c.ts_node_type(node);
    assert(mem.len(node_name) < 1024); // TODO: remove
    const len = std.mem.len(node_name);
    const copy = allocator.alloc(u8, len) catch unreachable; // TODO:
    mem.copyForwards(u8, copy, node_name[0..mem.len(node_name)]);
    return copy[0..mem.len(node_name)];
}

pub const Node = struct {
    node_type: NodeType,
    node: c.TSNode,
    sym: []const u8,
    row_start: struct { usize, usize },
    row_end: struct { usize, usize },
    allocator: Allocator,

    const Self = @This();

    pub fn init(node: c.TSNode, allocator: std.mem.Allocator) Self {
        const point_start = c.ts_node_start_point(node);
        const point_end = c.ts_node_end_point(node);

        const sym_name = get_type(node, allocator);
        const node_type = NodeType.from_string(sym_name);

        return Self{
            .allocator = allocator,
            .node = node,
            .node_type = node_type,
            .row_end = .{ point_end.row, point_end.column },
            .row_start = .{ point_start.row, point_start.column },
            .sym = sym_name,
        };
    }

    pub fn deinit(_: *Self) void {
        // FIX:
        // self.allocator.free(self.sym);
    }

    pub fn extract_type_item(self: *const Node, parser: *const Parser) NodeItem.Data.TypeItem {
        // TODO: $visibility_modifier

        const name_field = self.get_field_unchecked("name"); //  $_type_identifier
        const typename = parser.node_to_string_alloc(name_field.node, self.allocator);
        // TODO: const type_parameters_field = if (self.get_field("type_parameters")) //  $type_parameters

        const type_field = self.get_field_unchecked("type"); // $_type
        const type_kind = type_field.extract_type_ref(parser);
        // TODO: $where_clause,

        return NodeItem.Data.TypeItem{
            .name = typename,
            .kind = type_kind,
        };
    }

    pub fn extract_node_items(
        self: *const Node,
        parser: *const Parser,
        collect: *std.ArrayList(NodeItem),
    ) void {
        switch (self.node_type) {
            .declaration_list, .source_file => {
                var children = self.get_children_named();
                defer children.clearAndFree();
                defer for (children.items) |*child| child.deinit();

                for (children.items) |child| {
                    child.extract_node_items(parser, collect);
                }
            },

            .associated_type => {

                // FIX:
                // const name_field = self.get_field_unchecked("name");
                // const typekind = name_field.extract_type_ref(parser);
                // assert(typekind == .identifier);

                // // TODO: if (self.get_field("type_parameters"));
                // // TODO: if (self.get_field("bounds"));

                // // TODO: $where_clause;

                // const item_data = NodeItem.Data{
                //     .type_item = .{
                //         .name = typekind.identifier,
                //         .kind = typekind,
                //     },
                // };

                // const item = NodeItem.init(item_data, typekind.identifier);
                // collect.append(item) catch unreachable;
            },
            .type_item => {
                const type_item = self.extract_type_item(parser);
                const typename = type_item.name;
                const item_data = NodeItem.Data{
                    .type_item = type_item,
                };

                const item = NodeItem.init(item_data, typename);
                collect.append(item) catch unreachable;
            },

            .function_item => {
                const item = self.extract_function_item(parser);
                collect.append(item) catch unreachable;
            },
            .struct_item => {
                const item = self.extract_struct_item(parser);
                collect.append(item) catch unreachable;
            },
            .impl_item => {
                const item = self.extract_impl_item(parser);
                collect.append(item) catch unreachable;
            },
            .enum_item => {
                const item = self.extract_enum_item(parser);
                collect.append(item) catch unreachable;
            },
            .mod_item => {
                const item = self.extract_mod_item(parser);
                collect.append(item) catch unreachable;
            },
            .const_item => {
                const item = self.extract_const_item(parser);
                collect.append(item) catch unreachable;
            },
            .static_item => {
                const item = self.extract_static_item(parser);
                collect.append(item) catch unreachable;
            },
            .trait_item => {
                const item = self.extract_trait_item(parser);
                collect.append(item) catch unreachable;
            },

            .extern_crate_declaration => {
                const item = self.extract_extern_crate_declaration(parser);
                collect.append(item) catch unreachable;
            },

            .use_declaration => {
                const item = self.extract_use_declaration(parser);
                collect.append(item) catch unreachable;
            },

            .function_signature_item => {}, // FIX:  comment

            .attribute_item => {}, // TODO
            .expression_statement => {}, // TODO:

            .block_comment, .line_comment => {},

            .macro_invocation, .macro_definition => {},
            .empty_statement => {},

            else => |tag| {
                const gray_open = "\u{001b}[38;5;8m\n";
                const gray_close = "\u{001b}[m\n";

                eprintln(gray_open ++ "// TODO: {s}", .{@tagName(tag)});

                parser.print_source(self.node);
                eprintln(gray_close, .{});
            },
        }
    }

    pub fn get_field_unchecked(self: *const Node, field_name: []const u8) Node {
        const result = c.ts_node_child_by_field_name(self.node, field_name.ptr, @truncate(field_name.len));

        if (c.ts_node_is_null(result)) {
            eprintln(
                "\u{001B}[38:5:1m" ++
                    \\ ===============[ERROR]======================
                    \\ Field:  
                    \\   "{s}"
                    \\ Name:
                    \\   "{s}"
                    \\ Syntax Tree:
                    \\   {s}
                    \\ ===============[ERROR]======================
                ++ "\u{001B}[m",
                .{
                    field_name,
                    self.sym,
                    c.ts_node_string(self.node)[0..100],
                },
            );
            unreachable;
        }

        return Node.init(result, self.allocator);
    }

    pub fn get_field(self: *const Node, field_name: []const u8) ?Node {
        const result = c.ts_node_child_by_field_name(self.node, field_name.ptr, @truncate(field_name.len));

        if (c.ts_node_is_null(result))
            return null;

        return Node.init(result, self.allocator);
    }

    pub fn get_children_named(self: *const Node) std.ArrayList(Node) {
        // FIX clear
        const ts_node = self.node;

        var children = std.ArrayList(Node).init(self.allocator);
        const named_child_count = c.ts_node_named_child_count(ts_node);

        for (0..named_child_count) |idx| {
            const child = get_child_named(ts_node, @truncate(idx)) orelse unreachable;

            var buffer = [1]u8{0} ** 128;
            var fba = std.heap.FixedBufferAllocator.init(&buffer);
            const allocator = fba.allocator();

            // NOTE: skip line/block comments
            if (eql(get_type(child, allocator), "line_comment")) continue; // FIX:

            children.append(
                Node.init(
                    child,
                    self.allocator,
                ),
            ) catch unreachable;
        }

        return children;
    }

    pub fn get_next_siblings(self: *const Node) std.ArrayList(Node) {
        const ts_node = self.node;
        var children = std.ArrayList(Node).init(self.allocator);

        var next_sibling = ts_node;

        while (true) {
            next_sibling = c.ts_node_next_named_sibling(next_sibling);
            if (c.ts_node_is_null(next_sibling)) break;

            var buffer = [1]u8{0} ** 128;
            var fba = std.heap.FixedBufferAllocator.init(&buffer);
            const allocator = fba.allocator();

            // NOTE: skip line/block comments
            if (eql(get_type(next_sibling, allocator), "line_comment")) continue; // FIX:

            children.append(
                Node.init(
                    next_sibling,
                    self.allocator,
                ),
            ) catch unreachable;
        }

        return children;
    }

    fn extract_type_parameters(self: *const Node, parser: *const Parser) ?std.ArrayList(TypeKind) {
        var params = std.ArrayList(TypeKind).init(self.allocator);

        const children = self.get_children_named();

        for (children.items) |child| {
            if (child.node_type == .attribute_item) @panic("Todo");

            switch (child.node_type) {
                .lifetime => std.log.info("skipping: lifetime", .{}),
                .metavariable => @panic("todo:"),
                .type_identifier => {
                    const typename = child.extract_type_ref(parser);
                    params.append(typename) catch unreachable;
                },
                .constrained_type_parameter => {
                    const typename_field = child.get_field_unchecked("left");

                    if (typename_field.node_type == .lifetime) {
                        @panic("todo");
                    } else {
                        assert(typename_field.node_type == .type_identifier); // _type_identifier
                        const typename = typename_field.extract_type_ref(parser);
                        params.append(typename) catch unreachable;
                    }
                    // field('bounds', $.trait_bounds),
                },
                .optional_type_parameter => @panic("todo:"),
                .const_parameter => @panic("todo:"),
                else => unreachable,
            }
        }

        if (params.items.len == 0) {
            params.clearAndFree();
            return null;
        }
        return params;
    }

    fn extract_struct_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: if self.get_field("visibility_modifier") ||

        assert(self.node_type == .struct_item);

        const id_field = self.get_field_unchecked("name"); // $.identifier,
        const id = parser.node_to_string_alloc(id_field.node, self.allocator);

        const generics: ?std.ArrayList(TypeKind) = blk: {
            if (self.get_field("type_parameters")) |type_parameters| {
                const params = type_parameters.extract_type_parameters(parser);

                break :blk params;
            } else break :blk null;
        };

        var fields = std.ArrayList(NodeItem.Data.Object.Field).init(self.allocator);
        var ordered = false;

        if (self.get_field("body")) |body_field| {
            // TODO: if get_field(body_field, "where_clause") |where_clause field| {}

            if (body_field.node_type == .field_declaration_list) {
                const decls = body_field.get_children_named();
                for (decls.items) |decl| {
                    switch (decl.node_type) {
                        .field_declaration => {
                            // $visibility_modifier?,
                            const name_field = decl.get_field_unchecked("name"); //  $_field_identifier
                            const name = parser.node_to_string_alloc(name_field.node, self.allocator);
                            const type_field = decl.get_field_unchecked("type"); //  $_type
                            const type_kind = type_field.extract_type_ref(parser);

                            const field = NodeItem.Data.Object.Field{ .field = .{
                                .name = name,
                                .type_kind = type_kind,
                            } };

                            fields.append(field) catch unreachable;
                        },
                        .attribute_item => continue, // FIX:
                        else => |_| {
                            parser.print_source(decl.node);

                            @panic("todo");
                        },
                    }
                }
            } else if (body_field.node_type == .ordered_field_declaration_list) {
                ordered = true;

                const children = body_field.get_children_named();

                for (children.items) |child| {
                    // TODO: vis, attr
                    if (child.node_type != .attribute_item) continue; // FIX:
                    if (child.node_type != .visibility_modifier) continue; // FIX:

                    const type_field = child.get_field_unchecked("type");
                    const type_kind = type_field.extract_type_ref(parser);
                    fields.append(NodeItem.Data.Object.Field{ .tuple = type_kind }) catch unreachable;
                }
            } else unreachable;
        }

        const object = NodeItem.Data{
            .object_item = .{
                .fields = fields.items,
                .generics = generics,
                .ordered = ordered,
            },
        };

        assert(!eql(id, ""));
        const result = NodeItem.init(object, id);
        return result;
    }

    fn extract_body(self: *const Self, parser: *const Parser) std.ArrayList(NodeItem) {
        var node_items = std.ArrayList(NodeItem).init(parser.allocator); // TODO:
        self.extract_node_items(parser, &node_items);

        return node_items;

        //     if (eql("field_declaration_list", node_name)) {
        //         const children = get_children_named(self);

        //         for (children.items) |field_decl| {
        //             const field_name = get_type(field_decl);
        //             if (eql("field_declaration", field_name)) {
        //                 const name_field = self.get_field("name") orelse unreachable;
        //                 const type_field = self.get_field("type") orelse unreachable;
        //                 const name = parser.node_to_string(name_field.node, self.allocator);
        //                 _ = name; // autofix
        //                 const _type = parser.node_to_string(type_field.node, self.allocator);
        //                 _ = _type; // autofix
        //             }
        //         }
        //     } else {
        //         assert(eql("ordered_field_declaration_list", node_name));
        //         const _type_field = self.get_field("type") orelse unreachable;
        //         const _type = parser.node_to_string(_type_field.node, self.allocator);

        //         // TODO: convert type;
        //         for (_type) |str| {
        //             _ = str; // autofix
        //         }
        //     }
    }

    fn extract_const_item(self: *const @This(), parser: *const Parser) NodeItem {
        const name_field = self.get_field_unchecked("name");

        const name_ = name_field.extract_type_ref(parser);
        assert(name_ == .identifier);
        const name = name_.identifier.text;

        const type_field = self.get_field_unchecked("type");
        const type_kind = type_field.extract_type_ref(parser);

        const value_expr = blk: {
            if (self.get_field("value")) |expr_field| { // TODO: _expr
                const text = parser.node_to_string_alloc(expr_field.node, self.allocator);
                break :blk text;
            } else break :blk null;
        };

        const item_data = NodeItem.Data{ .const_item = .{
            .name = IdentifierKind{ .text = name },
            .type_kind = type_kind,
            .value_expr = value_expr,
        } };
        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_static_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: $visibility_modifier?
        // TODO: $mutable_specifier?

        const name_field = self.get_field_unchecked("name");

        const name_ = name_field.extract_type_ref(parser);
        assert(name_ == .identifier);
        const name = name_.identifier.text;

        const type_field = self.get_field_unchecked("type");
        const type_kind = type_field.extract_type_ref(parser);

        const value_expr = blk: {
            if (self.get_field("value")) |expr_field| { // TODO: _expr
                const text = parser.node_to_string_alloc(expr_field.node, self.allocator);
                break :blk text;
            } else break :blk null;
        };

        const item_data = NodeItem.Data{ .const_item = .{
            .name = IdentifierKind{ .text = name },
            .type_kind = type_kind,
            .value_expr = value_expr,
        } };
        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_trait_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: $visibility_modifier,

        const name_field = self.get_field_unchecked("name"); // $_type_identifier
        const name = name_field.extract_type_ref(parser);

        var type_constraints = std.ArrayList([]const u8).init(self.allocator);

        if (self.get_field("type_parameters")) |field| { // $type_parameters
            const text = parser.node_to_string_alloc(field.node, self.allocator);
            type_constraints.append(text) catch unreachable; // TODO: FIX:
        }

        var annotations = std.ArrayList([]const u8).init(parser.allocator); // FIX:
        if (self.get_field("bounds")) |r| { // $trait_bounds
            const text = parser.node_to_string(r.node);
            annotations.append(text) catch unreachable;
        }

        // TODO: $where_clause

        const body_field = self.get_field_unchecked("body");
        var items = std.ArrayList(NodeItem).init(self.allocator);

        body_field.extract_node_items(parser, &items);

        assert(name == .identifier);

        const item_data = NodeItem.Data{ .trait_item = .{
            .name = name.identifier.text,
            .items = items,
            .constraints = type_constraints,
        } };
        var result = NodeItem.init(item_data, name.identifier.text);
        result.annotations = annotations; // FIX:
        return result;
    }

    fn extract_attribute_item(self: *const @This(), parser: *const Parser) void {
        _ = self; // autofix
        _ = parser; // autofix
        // out(writer, "//", .{});
        // out(writer, "{s}", .{parser.node_to_string(self.node, self.allocator)});
    }

    fn extract_mod_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: $visibility_modifier

        const name_field = self.get_field_unchecked("name");
        const name = parser.node_to_string_alloc(name_field.node, self.allocator);

        const node_items = if (self.get_field("body")) |body_field| blk: { // $declaration_list);
            break :blk body_field.extract_body(parser);
        } else null;

        // const item_data = NodeItem.ItemData{
        //     .module_item = module,
        // };

        const module = NodeItem.Data{
            .module_item = .{ .contents = node_items },
        };
        const result = NodeItem.init(module, name);

        return result;
    }

    fn extract_foreign_mod_item(_: *const @This(), _: *const Parser) void {
        @panic("todo");
    }

    fn extract_union_item(_: *const @This(), _: *const Parser) void {
        @panic("todo");
    }

    fn extract_extern_crate_declaration(self: *const @This(), parser: *const Parser) NodeItem {
        // todo: $visibility_modifier
        const children = self.get_children_named();
        _ = children; // autofix

        const name_field = self.get_field_unchecked("name");
        const name = name_field.extract_type_ref(parser);
        assert(name == .identifier);

        if (self.get_field("alias")) |field| {
            const alias = field.extract_type_ref(parser);
            assert(alias == .identifier);
            @panic("todo");
        }

        const result = NodeItem.init(
            .{ .module_item = .{ .contents = null } },
            name.identifier.text,
        );
        return result;
    }

    pub fn extract_type_ref(self: *const @This(), parser: *const Parser) NodeItem.Data.TypeKind {
        switch (self.node_type) {
            .abstract_type => {
                const result = self.extract_abstract_type(parser);
                return result;
            },
            .reference_type => {
                // TODO: $lifetime,
                // TODO: $mutable_specifier,
                const type_field = self.get_field_unchecked("type");
                const child_type = type_field.extract_type_ref(parser);

                const child = self.allocator.create(@TypeOf(child_type)) catch unreachable; // FIX: remove

                child.* = child_type;

                return TypeKind{
                    .ref = .{ .child = child },
                };
            },
            .metavariable => {
                unreachable;
            },
            .pointer_type => {
                unreachable;
            },
            .generic_type => { // FIX:

                const type_field = self.get_field_unchecked("type");
                const name = type_field.extract_type_ref(parser);
                assert(name == .identifier);

                const type_args = self.get_field_unchecked("type_arguments"); // $type_arguments
                var constraints = std.ArrayList(struct {
                    name: ?[]const u8,
                    type_kind: ?TypeKind,
                }).init(self.allocator); // FIX:
                constraints = constraints; // FIX:

                const children = type_args.get_children_named();
                for (children.items) |child| {
                    if (child.node_type == .trait_bounds) {
                        @panic("todo");
                    } else {
                        switch (child.node_type) {
                            .type_binding => {
                                const name_field = child.get_field_unchecked("name");
                                const child_name = name_field.extract_type_ref(parser);

                                assert(child_name == .identifier);

                                if (child.get_field("type_arguments")) |_| @panic("todo"); // $type_arguments

                                const child_type_field = child.get_field_unchecked("type");
                                const child_type = child_type_field.extract_type_ref(parser);
                                constraints.append(.{
                                    .name = child_name.identifier.text,
                                    .type_kind = child_type,
                                }) catch unreachable;
                            },
                            .lifetime => {
                                std.log.debug("ignored: lifetime", .{});
                            },
                            .block => @panic("todo"), // FIX:

                            // TODO: $_type
                            // TODO: $_literal,
                            else => {
                                const any = child.extract_type_ref(parser);

                                constraints.append(.{
                                    .name = null,
                                    .type_kind = any,
                                }) catch unreachable;
                            },
                        }
                    }
                }

                const result = TypeKind{
                    .generic = .{ .name = name.identifier },
                };
                return result;
            },
            .scoped_identifier, .scoped_type_identifier => {
                const name_field = self.get_field_unchecked("name");
                const type_kind = name_field.extract_type_ref(parser);
                assert(type_kind == .identifier);
                assert(type_kind.identifier == .text);

                if (self.get_field("path")) |path_field| {
                    const result = PathParser.init(parser, path_field, self.allocator);
                    const scope = result.parse();

                    return TypeKind{ .identifier = .{ .scoped = .{
                        .name = type_kind.identifier.text,
                        .path = scope,
                    } } };
                }

                return type_kind;
            },
            .tuple_type => {
                var tuples_types = std.ArrayList(TypeKind).init(self.allocator); // FIX:

                const children = self.get_children_named();
                for (children.items) |child| {
                    tuples_types.append(child.extract_type_ref(parser)) catch unreachable;
                }

                return TypeKind{ .tuple = tuples_types };
            },
            .unit_type => {
                return .none;
            },
            .array_type => {
                const result = self.extract_array_type(parser);
                return result;
            },
            .function_type => {
                const result = self.extract_function_type(parser);
                return result; // autofix
            },
            .macro_invocation => {
                unreachable;
            },
            .never_type => {
                return .no_return;
            },
            .dynamic_type => {
                return .dynamic;
            },
            .bounded_type => {
                std.log.warn("bounded_type ignored", .{});
                return .none; // FIX:
            },
            .removed_trait_bound => {
                @panic("todo");
            },
            .primitive_type => {
                // FIX: test

                const text = parser.node_to_string_alloc(self.node, self.allocator);

                if (eql(text, "u16")) return .{ .primitive = .u16 };
                if (eql(text, "u32")) return .{ .primitive = .u32 };
                if (eql(text, "u64")) return .{ .primitive = .u64 };
                if (eql(text, "u8")) return .{ .primitive = .u8 };
                if (eql(text, "i16")) return .{ .primitive = .i16 };
                if (eql(text, "i32")) return .{ .primitive = .i32 };
                if (eql(text, "i64")) return .{ .primitive = .i64 };
                if (eql(text, "i8")) return .{ .primitive = .i8 };
                if (eql(text, "char")) return .{ .primitive = .char };
                if (eql(text, "str")) return .{ .primitive = .str };
                if (eql(text, "bool")) return .{ .primitive = .bool };
                if (eql(text, "u128")) return .{ .primitive = .u128 };
                if (eql(text, "i128")) return .{ .primitive = .i128 };
                if (eql(text, "isize")) return .{ .primitive = .isize };
                if (eql(text, "usize")) return .{ .primitive = .usize };
                if (eql(text, "f32")) return .{ .primitive = .f32 };
                if (eql(text, "f64")) return .{ .primitive = .f64 };

                unreachable;
            },
            else => |tag| { // _type_identifier
                assert(tag == .identifier or
                    tag == .shorthand_field_identifier or
                    tag == .type_identifier);

                const text = parser.node_to_string_alloc(self.node, self.allocator);
                return TypeKind{ .identifier = .{ .text = text } }; // FIX: copy
            },
        }
    }

    fn extract_function_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: self.get_field("visibility_modifier")
        // TODO: self.get_field("function_modifiers")

        const name_field = self.get_field_unchecked("name");
        assert(eql(name_field.sym, "identifier")); // TODO: $metavariable
        const name = parser.node_to_string_alloc(name_field.node, self.allocator);

        const generics = blk: {
            if (self.get_field("type_parameters")) |field| {
                break :blk field.extract_type_parameters(parser);
            } else break :blk null;
        };

        const parameters_field = self.get_field_unchecked("parameters");
        const params = parameters_field.extract_parameters(parser);

        // TODO: get_field("where_clause"),

        const return_type_kind = blk: {
            if (self.get_field("return_type")) |return_type| {
                const type_kind = return_type.extract_type_ref(parser);

                break :blk type_kind;
            } else break :blk .none;
        };

        // TODO: field('body') //  $.block;

        const item_data = NodeItem.Data{
            .procedure_item = .{
                .params = params.items,
                .return_type = return_type_kind,
                .generics = generics,
            },
        };

        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_parameters(
        self: *const @This(),
        parser: *const Parser,
    ) std.ArrayList(NodeItem.Data.Procedure.Param) {
        var result = std.ArrayList(NodeItem.Data.Procedure.Param).init(self.allocator);

        const children = self.get_children_named();
        for (children.items) |child| {
            if (child.node_type == .attribute_item) unreachable;

            const NameType = struct { pname: ?IdentifierKind, ptype: ?NodeItem.Data.TypeKind };
            const name_type: NameType = blk: {
                if (child.node_type == .parameter) {

                    // TODO: $mutable_specifier,

                    const field_name = "pattern";
                    const tsnode = c.ts_node_child_by_field_name(child.node, field_name.ptr, @truncate(field_name.len));
                    const text = parser.node_to_string_alloc(tsnode, self.allocator);

                    const name: IdentifierKind = blk_name: {
                        if (eql(text, "_")) break :blk_name .discarded; //FIX:

                        const pattern_field = Node.init(tsnode, self.allocator); // ( $_pattern | $self )

                        if (pattern_field.node_type == .self) {
                            break :blk_name IdentifierKind{ .text = "self" }; // TODO EnumLiteral
                        } else {
                            const name = pattern_field.extract_pattern(parser);
                            break :blk_name name;
                        }
                    };

                    const type_field = child.get_field_unchecked("type"); //  $_type
                    const typename = type_field.extract_type_ref(parser);
                    break :blk .{ .pname = name, .ptype = typename };
                } else if (child.node_type == .self_parameter) {
                    const name: IdentifierKind = .self;
                    break :blk .{ .pname = name, .ptype = null };
                } else if (child.node_type == .variadic_parameter) {
                    @panic("todo");
                } else { // _type
                    const text = parser.node_to_string_alloc(child.node, self.allocator);

                    if (eql(text, "_")) break :blk .{
                        .pname = IdentifierKind{ .text = text },
                        .ptype = null,
                    } else {
                        const typekind = child.extract_type_ref(parser);
                        break :blk .{
                            .ptype = typekind,
                            .pname = null,
                        };
                    }
                }
            };

            const param_name = name_type.pname;
            const param_kind = name_type.ptype;

            result.append(NodeItem.Data.Procedure.Param{
                .name = param_name,
                .typekind = param_kind,
            }) catch unreachable; // FIX:
        }
        return result;
    }

    fn extract_pattern(
        self: *const Node,
        parser: *const Parser,
    ) IdentifierKind {
        switch (self.node_type) {
            .identifier => {
                const source = parser.node_to_string_alloc(self.node, self.allocator);
                return IdentifierKind{ .text = source };
            },
            // _literal_pattern,
            // $.string_literal,
            // $.raw_string_literal,
            // $.char_literal,
            // $.boolean_literal,
            // $.integer_literal,
            // $.float_literal,
            // $.negative_literal,

            // $.identifier,
            // $.scoped_identifier,
            // $.tuple_pattern,
            // $.tuple_struct_pattern,
            .struct_pattern => {
                const type_field = self.get_field_unchecked("type");
                const typekind = type_field.extract_type_ref(parser);
                assert(typekind == .identifier);

                // fields
                const next_siblings = type_field.get_next_siblings();
                for (next_siblings.items) |field| {
                    if (field.node_type == .field_pattern) {

                        // TODO: 'ref'?
                        // TODO: $mutable_specifier?

                        const name_field = field.get_field_unchecked("name");
                        const name_identifier = name_field.extract_type_ref(parser);
                        assert(name_identifier == .identifier);

                        if (field.get_field("pattern")) |pattern_field| {
                            const inner = pattern_field.extract_pattern(parser);
                            _ = inner;
                            @panic("todo");
                        }

                        const text = parser.node_to_string(field.node);

                        const result = IdentifierKind{
                            .matched = text, // FIX:
                        };
                        return result;
                    } else {
                        parser.print_source(field.node);
                        assert(field.node_type == .remaining_field_pattern);
                        @panic("todo");
                    }
                }
                unreachable;
            },
            // $.ref_pattern,
            // $.slice_pattern,
            // $.captured_pattern,
            // $.reference_pattern,
            // $.remaining_field_pattern,
            // $.mut_pattern,
            // $.range_pattern,
            // $.or_pattern,
            // $.const_block,
            // $.macro_invocation,
            // '_',
            else => {
                eprintln("NodeType: {s}", .{@tagName(self.node_type)});
                unreachable;
            },
        }
    }

    fn extract_enum_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: $visibility_modifier;

        const name_field = self.get_field_unchecked("name");
        const name = parser.node_to_string_alloc(name_field.node, self.allocator);

        // TODO: if (get_field("type_parameters") $type_parameters;
        // TODO: $where_clause;

        const body = self.get_field_unchecked("body"); // $enum_variant_list;

        const variants = body.extract_enum_variants(parser);

        const item_data = NodeItem.Data{ .enum_item = .{
            .variants = variants.items,
        } };
        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_enum_variants(self: *const @This(), parser: *const Parser) std.ArrayList(NodeItem.Data.Enum.Variant) {
        const children = self.get_children_named(); // FIX:

        var variants = std.ArrayList(NodeItem.Data.Enum.Variant).init(self.allocator); // FIX:

        for (children.items) |child| {
            // TODO: $attribute_item;

            if (child.node_type == .attribute_item) continue; // FIX:

            assert(child.node_type == .enum_variant);

            // TODO: $?visibility_modifier
            const name_field = child.get_field_unchecked("name"); // identifier
            const name = parser.node_to_string_alloc(name_field.node, self.allocator);
            // TODO: if child.get_field("body" // $field_declaration_list | $ordered_field_declaration_list

            // TODO: if child.get_field("value", $_expression?

            const variant = NodeItem.Data.Enum.Variant{
                .name = name,
            };
            variants.append(variant) catch unreachable;
        }
        return variants;
    }

    /// FIX:
    fn extract_impl_item(self: *const @This(), parser: *const Parser) NodeItem {
        if (self.get_field("type_parameters")) |field| { //$type_parameters
            _ = field; // autofix

        }

        if (self.get_field("trait")) |_| {
            // TODO: for trait
            // ($_type_identifier | $scoped_type_identifier | $generic_type)

        }

        const type_field = self.get_field_unchecked("type");
        const type_ref = type_field.extract_type_ref(parser);

        // TODO: $where_clause?
        // TODO: if(self.get_field("body") || // $declaration_list

        const item_data = NodeItem.Data{
            .impl_item = .{
                .procedures = null, // TODO:
                .item_ref = type_ref,
            }, // TODO:
        };
        const result = NodeItem.init(item_data, null);
        return result;
    }

    fn extract_scoped_identifier(_: *const @This(), _: *const Parser) void {
        @panic("todo");

        // assert(eql(get_type(self), "scoped_identifier"));

        // if (self.get_field("path")) |field| {
        //     const field_sym = get_type(field);

        //     if (eql(field_sym, "self")) {
        //         field.write_to(parser);
        //     } else if (eql(field_sym, "identifier")) {
        //         field.write_to(parser);
        //     } else if (eql(field_sym, "metavariable")) {
        //         //         out(writer, "{s}", .{parser.node_to_string(field_sym.node, self.allocator)});
        //         field.write_to(parser);
        //     } else if (eql(field_sym, "super")) {
        //         field.write_to(parser);
        //     } else if (eql(field_sym, "crate")) {
        //         field.write_to(parser);
        //     } else if (eql(field_sym, "scoped_identifier")) {
        //         field.write_to(parser);
        //     } else if (eql(field_sym, "bracketed_type")) {
        //         field.write_to(parser);
        //     } else if (eql(field_sym, "generic_type")) {
        //         field.write_to(parser);
        //     } else {
        //         unreachable;
        //     }
        // }

        // const name_field = self.get_field("name") orelse unreachable; // choice($.identifier, $.super)),

        // const name_sym = get_type(name_field);
        // if (eql(name_sym, "identifier")) {
        //     name_field.write_to(parser);
        // } else if (eql(name_sym, "super")) {
        //     name_field.write_to(parser);
        // } else unreachable;
    }

    fn extract_use_declaration(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: visibility_modifier

        const argument = self.get_field_unchecked("argument");

        const importPath = ImportPathParser.init(parser, argument, self.allocator)
            .parse();

        const result = NodeItem.init(
            .{
                .import_item = importPath,
            },
            null,
        );

        return result;
    }

    fn extract_abstract_type(self: *const @This(), parser: *const Parser) TypeKind {
        if (self.get_field("type_parameters")) |_| @panic("todo");

        const trait_field = self.get_field_unchecked("trait");
        return switch (trait_field.node_type) {
            .scoped_type_identifier => @panic("todo"),
            .removed_trait_bound => @panic("todo"),

            .generic_type => {
                const type_kind = trait_field.extract_type_ref(parser);
                assert(type_kind == .generic);
                return type_kind;
            },
            .function_type => {
                const typekind = trait_field.extract_function_type(parser);
                return typekind;
            },
            .tuple_type => unreachable,
            else => {
                // ._type_identifier
                const type_kind = trait_field.extract_type_ref(parser);
                assert(type_kind == .identifier);
                return type_kind;
            },
        };
    }

    fn extract_literal_type(_: *const @This(), _: *const Parser) void {
        @panic("todo");
    }

    fn extract_array_type(self: *const @This(), parser: *const Parser) TypeKind {
        const type_field = self.get_field("element") orelse unreachable;
        const typekind = type_field.extract_type_ref(parser);

        const child = self.allocator.create(@TypeOf(typekind)) catch unreachable; // FIX: remove
        child.* = typekind;

        if (self.get_field("length")) |length_field| {
            const length_expression = parser.node_to_string_alloc(length_field.node, self.allocator); // FIX:
            return TypeKind{
                .array = .{
                    .length_expr = length_expression,
                    .child = child,
                },
            }; // FIX: parseInt
        } else {
            return TypeKind{
                .array = .{
                    .length_expr = null,
                    .child = child,
                },
            };
        }
    }

    fn extract_function_type(self: *const @This(), parser: *const Parser) TypeKind {
        assert(self.node_type == .function_type); // todo: remove

        // TODO: $for_lifetimes?
        // var params = std.ArrayList(NodeItem.ItemData.Procedure.Param).init(self.allocator);

        const parameters_field = self.get_field_unchecked("parameters"); // TODO: $parameters
        const params = parameters_field.extract_parameters(parser);

        const return_type = blk: {
            if (self.get_field("return_type")) |return_type_field| {
                const typekind = return_type_field.extract_type_ref(parser);
                const return_type_ptr = self.allocator.create(@TypeOf(typekind)) catch unreachable;
                return_type_ptr.* = typekind;

                break :blk return_type_ptr;

                // FIX: $_type
            } else break :blk null;
        };

        if (self.get_field("trait")) |field| {
            _ = {
                if (field.node_type == .scoped_type_identifier) {
                    // const scoped_path = PathParser.init(parser, field, self.allocator);
                    @panic("todo"); // FIX:
                } else {
                    assert(field.node_type == .type_identifier);
                    const typekind = field.extract_type_ref(parser);
                    assert(typekind == .identifier);

                    assert(eql(typekind.identifier.text, "Fn") or
                        eql(typekind.identifier.text, "FnOnce") or
                        eql(typekind.identifier.text, "FnMut"));
                }
            };

            const result = TypeKind{
                .proc = .{
                    .params = params,
                    .return_type = return_type,
                },
            };
            return result;
        } else {
            const children = self.get_children_named();
            for (children.items) |child| {
                if (child.node_type != .function_modifiers) continue;
                var buffer = [_]u8{0} ** 10;
                var fba = std.heap.FixedBufferAllocator.init(&buffer);
                const allocator = fba.allocator();

                const text = parser.node_to_string_alloc(child.node, allocator);

                if (eql(text, "unsafe")) continue;

                // TODO:
                // 'async',
                // 'default',
                // 'const',
                // 'unsafe',
                // $extern_modifier,

                unreachable;
            }
            const result = TypeKind{
                .proc = .{
                    .params = params, // FIX:
                    .return_type = return_type,
                },
            };
            return result;
        }
    }
};
