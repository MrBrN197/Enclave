const c = @import("./c.zig");
const mem = @import("std").mem;
const node_types = @import("./node/types.zig");
const parse = @import("./parser.zig"); // TODO:
const path = @import("./node/path.zig");
const procedure = @import("./node/items/procedure.zig");
const root = @import("./root.zig");
const std = @import("std");
const str = @import("./str.zig");
const items = @import("./node/items/item.zig");
const object = @import("./node/items/object.zig");

const assert = std.debug.assert;
const eprintln = root.eprintln;
const eprint = root.eprint;

const Allocator = std.mem.Allocator;
const Buf = path.Buf;
const Parser = parse.Parser;
const Writer = @TypeOf(std.io.getStdOut().writer());
pub const IdentifierKind = node_types.IdentifierKind;
pub const Import = @import("./node/items/item.zig").Import;
pub const ImportPath = node_types.ImportPath;
pub const ImportPathParser = path.ImportPathParser;
pub const Module = items.Module;
pub const NodeItem = node_types.NodeItem;
pub const NodeType = node_types.NodeType;
pub const PathParser = path.PathParser;
pub const TypeKind = node_types.TypeKind;

pub fn get_child_named(node: c.TSNode, idx: u32) ?c.TSNode {
    const result = c.ts_node_named_child(node, idx);
    return if (c.ts_node_is_null(node)) null else result;
}

pub fn is_of_type(node: c.TSNode, name: []const u8) bool {
    const c_node_name = c.ts_node_type(node);
    const node_name = c_node_name[0..mem.len(c_node_name)];
    return str.eql(node_name, name);
}

pub fn get_type(node: c.TSNode, allocator: std.mem.Allocator) []const u8 { // TODO: remove
    const node_name = c.ts_node_type(node);
    assert(mem.len(node_name) < 1024); // TODO: remove
    const len = std.mem.len(node_name);
    const copy = allocator.alloc(u8, len) catch unreachable; // TODO:
    mem.copyForwards(u8, copy, node_name[0..mem.len(node_name)]);
    return copy[0..mem.len(node_name)];
}

pub const Ctx = struct {
    parser: *const Parser,
    modules: []const Module,
    items: *std.ArrayList(NodeItem),
};

pub const Node = struct {
    node_type: NodeType,
    node: c.TSNode,
    row_start: struct { usize, usize },
    row_end: struct { usize, usize },
    allocator: Allocator,
    ctx: Ctx,

    const Self = @This();

    pub fn init_with_context(allocator: std.mem.Allocator, node: c.TSNode, ctx: Ctx) *Self {
        const point_start = c.ts_node_start_point(node);
        const point_end = c.ts_node_end_point(node);

        const sym_name = get_type(node, allocator);
        const node_type = NodeType.from_string(sym_name);

        const ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self{
            .allocator = allocator,
            .ctx = ctx,
            .node = node,
            .node_type = node_type,
            .row_end = .{ point_end.row, point_end.column },
            .row_start = .{ point_start.row, point_start.column },
        };
        return ptr;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);

        std.log.warn("todo: deinit", .{});
        // FIX:
    }

    pub fn extract_type_item(self: *Node) NodeItem {
        // TODO: $visibility_modifier

        const name_field = self.get_field_unchecked("name"); //  $_type_identifier
        const typename = self.ctx.parser.node_to_string_alloc(name_field.node, self.allocator);
        // TODO: const type_parameters_field = if (self.get_field("type_parameters")) //  $type_parameters

        const type_field = self.get_field_unchecked("type"); // $_type
        const type_kind = type_field.extract_type_ref();
        // TODO: $where_clause,

        const item_data = NodeItem.Data{
            .type_item = .{
                .kind = type_kind,
            },
        };

        const type_item = NodeItem.init(item_data, typename);
        return type_item;
    }

    pub fn extract_module(self: *const Self) Module {
        assert(self.node_type == .declaration_list or
            self.node_type == .source_file);

        const children: std.ArrayList(*Node) = self.get_children_named();
        // defer children.clearAndFree(); //FIX:
        // defer for (children.items) |child| child.deinit(); // FIX:

        var result = Module.init(self.allocator); // FIX:

        for (children.items) |child| {
            child.extract_node_items();
        }

        for (self.ctx.items.items) |item| result.insertItem(item);

        return result;
    }

    pub fn extract_node_items(self: *Node) void {
        switch (self.node_type) {
            .source_file, .declaration_list => unreachable, // TODO: remove
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
                const item = self.extract_type_item();
                self.ctx.items.append(item) catch unreachable;
            },

            .function_item => {
                const item = self.extract_function_item();
                self.ctx.items.append(item) catch unreachable;
            },
            .struct_item => {
                const item = self.extract_struct_item();
                self.ctx.items.append(item) catch unreachable;
            },
            .impl_item => {
                const item = self.extract_impl_item();

                self.ctx.items.append(item) catch unreachable;
            },
            .enum_item => {
                const item = self.extract_enum_item();

                self.ctx.items.append(item) catch unreachable;
            },
            .mod_item => {
                const item = self.extract_mod_item();
                self.ctx.items.append(item) catch unreachable;
            },
            .const_item => {
                const item = self.extract_const_item();

                self.ctx.items.append(item) catch unreachable;
            },
            .static_item => {
                const item = self.extract_static_item();

                self.ctx.items.append(item) catch unreachable;
            },
            .trait_item => {
                const item = self.extract_trait_item();

                self.ctx.items.append(item) catch unreachable;
            },

            .extern_crate_declaration => {
                const item = self.extract_extern_crate_declaration();

                self.ctx.items.append(item) catch unreachable;
            },

            .use_declaration => {
                const item = self.extract_use_declaration();
                self.ctx.items.append(item) catch unreachable;
            },

            .function_signature_item => {
                // TODO: vis
                // TODO: function_modifiers

                const name_field = self.get_field_unchecked("name");
                const name = name_field.extract_type_ref().identifier;

                const params_field = self.get_field_unchecked("parameters");
                const params = params_field.extract_parameters();

                var bounds = BoundsMap.init(self.allocator);

                const type_params = ty: {
                    if (self.get_field("type_parameters")) |field| {
                        break :ty field.extract_type_parameters(&bounds);
                    } else break :ty null;
                };

                const return_type: ?TypeKind = ret: {
                    if (self.get_field("return_type")) |field| {
                        const return_type: TypeKind = field.extract_type_ref();
                        break :ret return_type; // autofix
                    } else break :ret null;
                };

                for (self.get_children_named().items) |child| {
                    if (child.node_type == .where_clause) child.extract_where_clause(&bounds);
                }

                const function_signature_data = NodeItem.Data{
                    .function_signature_item = .{
                        .bounds = bounds,
                        .generics = type_params,
                        .params = params,
                        .return_type = return_type,
                    },
                };

                const item = NodeItem.init(function_signature_data, name.text);
                self.ctx.items.append(item) catch unreachable;
            },

            .attribute_item => {}, // TODO
            .expression_statement => {}, // TODO:

            .block_comment, .line_comment => {},

            .macro_invocation, .macro_definition => {},
            .empty_statement => {},

            else => |tag| {
                const gray_open = "\u{001b}[38;5;8m\n";
                const gray_close = "\u{001b}[m\n";

                eprintln(gray_open ++ "// TODO: {s}", .{@tagName(tag)});

                self.ctx.parser.print_source(self.node);
                eprintln(gray_close, .{});
            },
        }
    }

    pub fn get_field_unchecked(self: *const Node, field_name: []const u8) *Node {
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
                    @tagName(self.node_type),
                    c.ts_node_string(self.node)[0..100],
                },
            );
            unreachable;
        }

        return Node.init_with_context(self.allocator, result, self.ctx);
    }

    pub fn get_field(self: *const Node, field_name: []const u8) ?*Node {
        const result = c.ts_node_child_by_field_name(self.node, field_name.ptr, @truncate(field_name.len));

        if (c.ts_node_is_null(result))
            return null;

        return Node.init_with_context(self.allocator, result, self.ctx);
    }

    pub fn get_children_named(self: *const Node) std.ArrayList(*Node) {
        // FIX clear
        const ts_node = self.node;

        var children = std.ArrayList(*Node).init(self.allocator);
        const named_child_count = c.ts_node_named_child_count(ts_node);

        for (0..named_child_count) |idx| {
            const child = get_child_named(ts_node, @truncate(idx)) orelse unreachable;

            var buffer = [1]u8{0} ** 128;
            var fba = std.heap.FixedBufferAllocator.init(&buffer);
            const allocator = fba.allocator();

            // NOTE: skip line/block comments
            if (str.eql(get_type(child, allocator), "line_comment")) continue; // FIX:

            children.append(
                Node.init_with_context(
                    self.allocator,
                    child,
                    self.ctx,
                ),
            ) catch unreachable;
        }

        return children;
    }

    pub fn get_next_siblings(self: *const Node) std.ArrayList(*Node) {
        const ts_node = self.node;
        var children = std.ArrayList(*Node).init(self.allocator);

        var next_sibling = ts_node;

        while (true) {
            next_sibling = c.ts_node_next_named_sibling(next_sibling);
            if (c.ts_node_is_null(next_sibling)) break;

            var buffer = [1]u8{0} ** 128;
            var fba = std.heap.FixedBufferAllocator.init(&buffer);
            const allocator = fba.allocator();

            // NOTE: skip line/block comments
            if (str.eql(get_type(next_sibling, allocator), "line_comment")) continue; // FIX:

            children.append(
                Node.init_with_context(self.allocator, next_sibling, self.ctx),
            ) catch unreachable;
        }

        return children;
    }

    fn extract_type_parameters(self: *const Node, collect_bounds: *BoundsMap) ?std.ArrayList(TypeKind) {
        var params = std.ArrayList(TypeKind).init(self.allocator);

        const children = self.get_children_named();

        for (children.items) |child| {
            if (child.node_type == .attribute_item) @panic("Todo");

            switch (child.node_type) {
                .lifetime => std.log.info("skipping: lifetime", .{}),
                .metavariable => @panic("todo:"),
                .type_identifier => {
                    const typename = child.extract_type_ref();

                    params.append(typename) catch unreachable;
                },
                .constrained_type_parameter => {
                    const typename_field = child.get_field_unchecked("left");

                    if (typename_field.node_type == .lifetime) {
                        std.log.info("skipping lifetime", .{});
                        continue;
                    }

                    assert(typename_field.node_type == .type_identifier);

                    const typename = typename_field.extract_type_ref();

                    params.append(typename) catch unreachable;

                    const bounds = child.get_field_unchecked("bounds").get_children_named();
                    var typekinds = std.ArrayList(TypeKind).init(self.allocator);

                    for (bounds.items) |bound| {
                        switch (bound.node_type) {
                            .lifetime => std.log.info("skipping lifetime", .{}),
                            .higher_ranked_trait_bound => @panic("todo: higher_ranked_trait_bound "),
                            else => {
                                const typekind = bound.extract_type_ref();
                                typekinds.append(typekind) catch unreachable;
                            },
                        }
                    }
                    collect_bounds.putNoClobber(typename.identifier.text, typekinds) catch unreachable;
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

    fn extract_struct_item(self: *const @This()) NodeItem {
        const parser = self.ctx.parser;
        _ = parser; // autofix
        // TODO: if self.get_field("visibility_modifier") ||

        assert(self.node_type == .struct_item);

        const struct_name_field = self.get_field_unchecked("name"); // $.identifier,
        const struct_name = struct_name_field.extract_type_ref().identifier;

        var bounds = BoundsMap.init(self.allocator);
        const generics: ?std.ArrayList(TypeKind) = blk: {
            if (self.get_field("type_parameters")) |type_parameters| {
                const params = type_parameters.extract_type_parameters(&bounds);

                break :blk params;
            } else break :blk null;
        };

        var fields = std.ArrayList(object.Field).init(self.allocator);
        var ordered = false;

        if (self.get_field("body")) |body_field| {
            // TODO: if get_field(body_field, "where_clause") |where_clause field| {}

            if (body_field.node_type == .field_declaration_list) {
                const decls = body_field.get_children_named();
                for (decls.items) |decl| {
                    switch (decl.node_type) {
                        .field_declaration => {
                            // $visibility_modifier?,
                            const struct_field_name_field = decl.get_field_unchecked("name"); //  $_field_identifier
                            const struct_field_name = self.ctx.parser.node_to_string_alloc(struct_field_name_field.node, self.allocator);
                            const struct_field_type_field = decl.get_field_unchecked("type"); //  $_type
                            const struct_field_type = struct_field_type_field.extract_type_ref();

                            fields.append(object.Field{ .field = .{
                                .name = struct_field_name,
                                .type_kind = struct_field_type,
                            } }) catch unreachable;
                        },
                        .attribute_item => continue, // FIX:
                        else => |_| {
                            self.ctx.parser.print_source(decl.node);

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
                    const type_kind = type_field.extract_type_ref();

                    fields.append(object.Field{ .tuple = type_kind }) catch unreachable;
                }
            } else unreachable;
        }

        const object_data = NodeItem.Data{
            .object_item = .{
                .fields = fields.items,
                .generics = generics,
                .ordered = ordered,
                .bounds = bounds,
            },
        };

        const result = NodeItem.init(object_data, struct_name.text);
        return result;
    }

    fn extract_body(self: *Self) void {
        self.extract_node_items();
    }

    fn extract_const_item(self: *const Self) NodeItem {
        const parser = self.ctx.parser;
        const name_field = self.get_field_unchecked("name");

        const name_ = name_field.extract_type_ref();

        assert(name_ == .identifier);
        const name = name_.identifier.text;

        const type_field = self.get_field_unchecked("type");
        const type_kind = type_field.extract_type_ref();

        const value_expr = blk: {
            if (self.get_field("value")) |expr_field| { // TODO: _expr
                const text = parser.node_to_string_alloc(expr_field.node, self.allocator);
                break :blk text;
            } else break :blk null;
        };

        const item_data = NodeItem.Data{
            .const_item = .{
                .type_kind = type_kind,
                .value_expr = value_expr,
            },
        };
        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_static_item(self: *const @This()) NodeItem {
        const parser = self.ctx.parser;
        // TODO: $visibility_modifier?
        // TODO: $mutable_specifier?

        const name_field = self.get_field_unchecked("name");

        const name_ = name_field.extract_type_ref();

        assert(name_ == .identifier);
        const name = name_.identifier.text;

        const type_field = self.get_field_unchecked("type");
        const type_kind = type_field.extract_type_ref();

        const value_expr = blk: {
            if (self.get_field("value")) |expr_field| { // TODO: _expr
                const text = parser.node_to_string_alloc(expr_field.node, self.allocator);
                break :blk text;
            } else break :blk null;
        };

        const item_data = NodeItem.Data{
            .const_item = .{
                .type_kind = type_kind,
                .value_expr = value_expr,
            },
        };
        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_trait_item(self: *const @This()) NodeItem {
        const parser = self.ctx.parser;
        // TODO: $visibility_modifier,

        const name_field = self.get_field_unchecked("name"); // $_type_identifier
        const name = name_field.extract_type_ref();
        assert(name == .identifier);

        var bounds = BoundsMap.init(self.allocator);
        const generics = blk: {
            if (self.get_field("type_parameters")) |field| {
                break :blk field.extract_type_parameters(&bounds);
            } else break :blk null;
        };

        var annotations = std.ArrayList([]const u8).init(parser.allocator); // FIX:
        if (self.get_field("bounds")) |r| { // $trait_bounds
            const text = parser.node_to_string(r.node);
            annotations.append(text) catch unreachable;
        }

        const children = self.get_children_named();
        for (children.items) |child| {
            if (child.node_type == .where_clause) child.extract_where_clause(&bounds);
        }

        const body_field = self.get_field_unchecked("body");

        // FIX:
        var collect = std.ArrayList(NodeItem).init(self.allocator);
        const ctx = Ctx{
            .modules = &[_]Module{},
            .parser = self.ctx.parser,
            .items = &collect,
        };

        const body = Node.init_with_context(
            self.allocator,
            body_field.node,
            ctx,
        );
        const module = body.extract_module();

        const item_data = NodeItem.Data{
            .trait_item = .{
                .body = module,
                .bounds = bounds,
                .generics = generics,
            },
        };
        var result = NodeItem.init(item_data, name.identifier.text);
        result.annotations = annotations; // FIX:
        return result;
    }

    fn extract_attribute_item(self: *const @This()) void {
        const parser = self.ctx.parser;
        _ = parser; // autofix
        // out(writer, "//", .{});
        // out(writer, "{s}", .{parser.node_to_string(self.node, self.allocator)});
    }

    fn extract_mod_item(self: *const @This()) NodeItem {
        // TODO: $visibility_modifier

        const name_field = self.get_field_unchecked("name");
        const name = self.ctx.parser.node_to_string_alloc(name_field.node, self.allocator);

        const module: Module = if (self.get_field("body")) |body_field| blk: { // $declaration_list);
            var collect = std.ArrayList(NodeItem).init(self.allocator);
            const ctx = .{
                .modules = &[_]Module{},
                .parser = self.ctx.parser,
                .items = &collect,
            };

            const body = Node.init_with_context(
                self.allocator,
                body_field.node,
                ctx,
            );

            const module = body.extract_module();
            // var module = Module.init(self.allocator);
            // for (body.ctx.items.items) |item| module.insertItem(item);

            break :blk module;
        } else Module.init(self.allocator);

        const data = NodeItem.Data{
            .module_item = module,
        };
        const result = NodeItem.init(data, name);

        return result;
    }

    fn extract_foreign_mod_item(_: *const Self, _: *const Parser) void {
        @panic("todo");
    }

    fn extract_union_item(_: *const Self, _: *const Parser) void {
        @panic("todo");
    }

    fn extract_extern_crate_declaration(self: *const @This()) NodeItem {
        const parser = self.ctx.parser;
        _ = parser; // autofix
        // todo: $visibility_modifier
        const children = self.get_children_named();
        _ = children; // autofix

        const name_field = self.get_field_unchecked("name");
        const name = name_field.extract_type_ref();

        assert(name == .identifier);

        if (self.get_field("alias")) |field| {
            const alias = field.extract_type_ref();

            assert(alias == .identifier);
            @panic("todo");
        }

        const module = Module.init(self.allocator);

        const result = NodeItem.init(
            .{
                .module_item = module,
            },
            name.identifier.text,
        );
        return result;
    }

    pub fn extract_type_ref(self: *const Self) TypeKind {
        switch (self.node_type) {
            .abstract_type => {
                const result = self.extract_abstract_type();
                return result;
            },
            .reference_type => {
                // TODO: $lifetime,
                // TODO: $mutable_specifier,
                const type_field = self.get_field_unchecked("type");
                const child_type = type_field.extract_type_ref();

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
                const name = type_field.extract_type_ref();
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
                                const child_name = name_field.extract_type_ref();

                                assert(child_name == .identifier);

                                if (child.get_field("type_arguments")) |_| @panic("todo"); // $type_arguments

                                const child_type_field = child.get_field_unchecked("type");
                                const child_type = child_type_field.extract_type_ref();
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
                                const any = child.extract_type_ref();

                                constraints.append(.{
                                    .name = null,
                                    .type_kind = any,
                                }) catch unreachable;
                            },
                        }
                    }
                }

                const result = TypeKind{
                    .identifier = name.identifier,
                };
                return result;
            },
            .scoped_identifier, .scoped_type_identifier => {
                const name_field = self.get_field_unchecked("name");
                const type_kind = name_field.extract_type_ref();
                assert(type_kind == .identifier);
                assert(type_kind.identifier == .text);

                if (self.get_field("path")) |path_field| {
                    const path_parser = PathParser.init(self.allocator, self.ctx.parser, path_field.*);
                    var scope_buf = path_parser.parse();
                    scope_buf.append(".");
                    scope_buf.append(type_kind.identifier.text);

                    return TypeKind{
                        .identifier = .{
                            .scoped = scope_buf,
                        },
                    };
                }

                return type_kind;
            },
            .tuple_type => {
                var tuples_types = std.ArrayList(TypeKind).init(self.allocator); // FIX:

                const children = self.get_children_named();
                for (children.items) |child| {
                    tuples_types.append(child.extract_type_ref()) catch unreachable;
                }

                return TypeKind{ .tuple = tuples_types };
            },
            .unit_type => {
                return .none;
            },
            .array_type => {
                const result = self.extract_array_type();
                return result;
            },
            .function_type => {
                const result = self.extract_function_type();
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

                const text = self.ctx.parser.node_to_string_alloc(self.node, self.allocator);

                if (str.eql(text, "u16")) return .{ .primitive = .u16 };
                if (str.eql(text, "u32")) return .{ .primitive = .u32 };
                if (str.eql(text, "u64")) return .{ .primitive = .u64 };
                if (str.eql(text, "u8")) return .{ .primitive = .u8 };
                if (str.eql(text, "i16")) return .{ .primitive = .i16 };
                if (str.eql(text, "i32")) return .{ .primitive = .i32 };
                if (str.eql(text, "i64")) return .{ .primitive = .i64 };
                if (str.eql(text, "i8")) return .{ .primitive = .i8 };
                if (str.eql(text, "char")) return .{ .primitive = .char };
                if (str.eql(text, "str")) return .{ .primitive = .str };
                if (str.eql(text, "bool")) return .{ .primitive = .bool };
                if (str.eql(text, "u128")) return .{ .primitive = .u128 };
                if (str.eql(text, "i128")) return .{ .primitive = .i128 };
                if (str.eql(text, "isize")) return .{ .primitive = .isize };
                if (str.eql(text, "usize")) return .{ .primitive = .usize };
                if (str.eql(text, "f32")) return .{ .primitive = .f32 };
                if (str.eql(text, "f64")) return .{ .primitive = .f64 };

                unreachable;
            },
            else => |tag| { // _type_identifier
                assert(tag == .identifier or
                    tag == .shorthand_field_identifier or
                    tag == .type_identifier);

                const text = self.ctx.parser.node_to_string_alloc(self.node, self.allocator);
                return TypeKind{ .identifier = .{ .text = text } }; // FIX: copy
            },
        }
    }

    fn extract_function_item(self: *const @This()) NodeItem {
        const parser = self.ctx.parser;
        // TODO: self.get_field("visibility_modifier")
        // TODO: self.get_field("function_modifiers")

        const name_field = self.get_field_unchecked("name");
        assert((name_field.node_type == .identifier)); // TODO: $metavariable
        const name = parser.node_to_string_alloc(name_field.node, self.allocator);

        var bounds = BoundsMap.init(self.allocator);
        const generics = blk: {
            if (self.get_field("type_parameters")) |field| {
                break :blk field.extract_type_parameters(&bounds);
            } else break :blk null;
        };

        const parameters_field = self.get_field_unchecked("parameters");
        const params = parameters_field.extract_parameters();

        const return_type_kind = blk: {
            if (self.get_field("return_type")) |return_type| {
                const type_kind = return_type.extract_type_ref();

                break :blk type_kind;
            } else break :blk .none;
        };

        // TODO: field('body') //  $.block;

        const children = self.get_children_named();
        for (children.items) |child| {
            if (child.node_type == .where_clause) {
                child.extract_where_clause(&bounds);
            }
        }

        const item_data = NodeItem.Data{
            .procedure_item = .{
                .params = params.items,
                .return_type = return_type_kind,
                .generics = generics,
                .bounds = bounds,
            },
        };

        const result = NodeItem.init(item_data, name);
        return result;
    }

    const BoundsMap = std.StringArrayHashMap(std.ArrayList(TypeKind));

    fn extract_where_clause(self: *const Self, collect_bounds: *BoundsMap) void {
        const children = self.get_children_named();

        assert(children.items.len > 0);

        for (children.items) |child| {
            assert(child.node_type == .where_predicate);

            const predicate = child;
            const left_field = predicate.get_field_unchecked("left");

            var identifier: IdentifierKind = undefined;

            switch (left_field.node_type) {
                .lifetime => @panic("todo: lifetime "),
                .scoped_type_identifier => @panic("todo: scoped_type_identifier "),
                .generic_type => @panic("todo: generic_type "),
                .reference_type => @panic("todo: reference_type "),
                .pointer_type => @panic("todo: pointer_type "),
                .tuple_type => @panic("todo: tuple_type "),
                .array_type => @panic("todo: array_type "),
                .higher_ranked_trait_bound => @panic("todo: higher_ranked_trait_bound "),

                else => {
                    const typekind = left_field.extract_type_ref();
                    assert(typekind == .identifier);
                    identifier = typekind.identifier;
                },
            }

            const bounds = child.get_field_unchecked("bounds").get_children_named();
            var bounds_list = std.ArrayList(TypeKind).init(self.allocator);

            for (bounds.items) |bound| {
                switch (bound.node_type) {
                    .lifetime => @panic("todo: lifetime "),
                    .higher_ranked_trait_bound => @panic("todo: higher_ranked_trait_bound "),
                    else => {
                        const typekind = bound.extract_type_ref();
                        bounds_list.append(typekind) catch unreachable;
                    },
                }
            }
            collect_bounds.putNoClobber(identifier.text, bounds_list) catch unreachable;
        }
    }

    fn extract_parameters(
        self: *const Self,
    ) std.ArrayList(procedure.Param) {
        const parser = self.ctx.parser;
        var result = std.ArrayList(procedure.Param).init(self.allocator);

        const children = self.get_children_named();
        for (children.items) |child| {
            if (child.node_type == .attribute_item) unreachable;

            const name_type = blk: {
                if (child.node_type == .parameter) {

                    // TODO: $mutable_specifier,

                    const field_name = "pattern";
                    const tsnode = c.ts_node_child_by_field_name(child.node, field_name.ptr, @truncate(field_name.len));
                    const text = parser.node_to_string_alloc(tsnode, self.allocator);

                    const name: IdentifierKind = blk_name: {
                        if (str.eql(text, "_")) break :blk_name .discarded; //FIX:

                        const pattern_field = Node.init_with_context(self.allocator, tsnode, self.ctx); // ( $_pattern | $self )

                        if (pattern_field.node_type == .self) {
                            break :blk_name IdentifierKind{ .text = "self" }; // TODO EnumLiteral
                        } else {
                            const name = pattern_field.extract_pattern();

                            break :blk_name name;
                        }
                    };

                    const type_field = child.get_field_unchecked("type"); //  $_type
                    const typename = type_field.extract_type_ref();

                    break :blk .{ name, typename };
                } else if (child.node_type == .self_parameter) {
                    const name: IdentifierKind = .self;
                    parser.print_source(child.node);
                    break :blk .{ name, .self };
                } else if (child.node_type == .variadic_parameter) {
                    @panic("todo");
                } else { // _type
                    const text = parser.node_to_string_alloc(child.node, self.allocator);

                    if (str.eql(text, "_")) {
                        break :blk .{ IdentifierKind{ .text = text }, null };
                    } else {
                        break :blk .{
                            null,
                            child.extract_type_ref(),
                        };
                    }
                }
            };

            const param_name, const param_type = name_type;

            result.append(procedure.Param{
                .name = param_name,
                .typekind = param_type,
            }) catch unreachable; // FIX:
        }
        return result;
    }

    fn extract_pattern(
        self: *const Node,
    ) IdentifierKind {
        switch (self.node_type) {
            .identifier => {
                const source = self.ctx.parser.node_to_string_alloc(self.node, self.allocator);
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
                var type_field = self.get_field_unchecked("type");
                const typekind = type_field.extract_type_ref();

                assert(typekind == .identifier);

                // fields
                const next_siblings = type_field.get_next_siblings();
                for (next_siblings.items) |field| {
                    if (field.node_type == .field_pattern) {

                        // TODO: 'ref'?
                        // TODO: $mutable_specifier?

                        const name_field = field.get_field_unchecked("name");
                        const name_identifier = name_field.extract_type_ref();

                        assert(name_identifier == .identifier);

                        if (field.get_field("pattern")) |pattern_field| {
                            const inner = pattern_field.extract_pattern();

                            _ = inner;
                            @panic("todo");
                        }

                        const text = self.ctx.parser.node_to_string(field.node);

                        const result = IdentifierKind{
                            .matched = text, // FIX:
                        };
                        return result;
                    } else {
                        self.ctx.parser.print_source(field.node);
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

    fn extract_enum_item(self: *const @This()) NodeItem {
        const parser = self.ctx.parser;
        // TODO: $visibility_modifier;

        const name_field = self.get_field_unchecked("name");
        const name = parser.node_to_string_alloc(name_field.node, self.allocator);

        // TODO: if (get_field("type_parameters") $type_parameters;
        // TODO: $where_clause;

        const body = self.get_field_unchecked("body"); // $enum_variant_list;

        const variants = body.extract_enum_variants();

        const item_data = NodeItem.Data{
            .enum_item = .{
                .variants = variants.items,
            },
        };
        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_enum_variants(self: *const @This()) std.ArrayList(object.Enum.Variant) {
        const parser = self.ctx.parser;
        const children = self.get_children_named(); // FIX:

        var variants = std.ArrayList(object.Enum.Variant).init(self.allocator); // FIX:

        for (children.items) |child| {
            // TODO: $attribute_item;

            if (child.node_type == .attribute_item) continue; // FIX:

            assert(child.node_type == .enum_variant);

            // TODO: $?visibility_modifier
            const name_field = child.get_field_unchecked("name"); // identifier
            const name = parser.node_to_string_alloc(name_field.node, self.allocator);
            // TODO: if child.get_field("body" // $field_declaration_list | $ordered_field_declaration_list

            // TODO: if child.get_field("value", $_expression?

            const variant = object.Enum.Variant{
                .name = name,
            };
            variants.append(variant) catch unreachable;
        }
        return variants;
    }

    /// FIX:
    fn extract_impl_item(self: *const @This()) NodeItem {
        var bounds = BoundsMap.init(self.allocator);

        const type_parameters = params: {
            if (self.get_field("type_parameters")) |field| { //$type_parameters
                break :params field.extract_type_parameters(&bounds);
            } else break :params null;
        };

        const trait_identifier = trait: {
            if (self.get_field("trait")) |field| {
                break :trait field.extract_type_ref().identifier;
            } else break :trait null;
        };

        const type_field = self.get_field_unchecked("type");
        const implementor_identifier = type_field.extract_type_ref().identifier;

        const children = self.get_children_named();
        for (children.items) |child| {
            if (child.node_type == .where_clause) child.extract_where_clause(&bounds);
        }

        const body: ?Module = body: {
            if (self.get_field("body")) |body_field| {
                var collect = std.ArrayList(NodeItem).init(self.allocator); // FIX:
                const ctx = Ctx{
                    .modules = &[_]Module{}, //FIX:
                    .parser = self.ctx.parser,
                    .items = &collect,
                };

                const body = Node.init_with_context(
                    self.allocator,
                    body_field.node,
                    ctx,
                );
                const module = body.extract_module();

                break :body module;
            } else break :body null;
        };

        const item_data = NodeItem.Data{
            .impl_item = .{
                .body = body,
                .bounds = bounds,
                .generics = type_parameters,
                .implementor = implementor_identifier,
                .for_interface = trait_identifier,
            },
        };
        const result = NodeItem.init(item_data, null);
        return result;
    }

    fn extract_scoped_identifier(_: *const Self, _: *const Parser) void {
        @panic("todo");

        // assert(str.eql(get_type(self), "scoped_identifier"));

        // if (self.get_field("path")) |field| {
        //     const field_sym = get_type(field);

        //     if (str.eql(field_sym, "self")) {
        //         field.write_to(parser);
        //     } else if (str.eql(field_sym, "identifier")) {
        //         field.write_to(parser);
        //     } else if (str.eql(field_sym, "metavariable")) {
        //         //         out(writer, "{s}", .{parser.node_to_string(field_sym.node, self.allocator)});
        //         field.write_to(parser);
        //     } else if (str.eql(field_sym, "super")) {
        //         field.write_to(parser);
        //     } else if (str.eql(field_sym, "crate")) {
        //         field.write_to(parser);
        //     } else if (str.eql(field_sym, "scoped_identifier")) {
        //         field.write_to(parser);
        //     } else if (str.eql(field_sym, "bracketed_type")) {
        //         field.write_to(parser);
        //     } else if (str.eql(field_sym, "generic_type")) {
        //         field.write_to(parser);
        //     } else {
        //         unreachable;
        //     }
        // }

        // const name_field = self.get_field("name") orelse unreachable; // choice($.identifier, $.super)),

        // const name_sym = get_type(name_field);
        // if (str.eql(name_sym, "identifier")) {
        //     name_field.write_to(parser);
        // } else if (str.eql(name_sym, "super")) {
        //     name_field.write_to(parser);
        // } else unreachable;
    }

    fn extract_use_declaration(self: *const @This()) NodeItem {
        // TODO: visibility_modifier
        const ctx = self.ctx;

        const argument = self.get_field_unchecked("argument");

        var import_paths = std.ArrayList(Buf).init(self.allocator);
        ImportPathParser.init(
            self.allocator,
            ctx.parser,
            argument.*,
        ).parse(&import_paths);

        const import_item = Import.init(import_paths);

        const result = NodeItem.init(
            .{ .import_item = import_item },
            null,
        );

        return result;
    }

    fn extract_abstract_type(self: *const Self) TypeKind {
        if (self.get_field("type_parameters")) |_| @panic("todo");

        var trait_field = self.get_field_unchecked("trait");
        return switch (trait_field.node_type) {
            .scoped_type_identifier => @panic("todo"),
            .removed_trait_bound => @panic("todo"),

            .generic_type => {
                const type_kind = trait_field.extract_type_ref();

                assert(type_kind == .identifier);
                return type_kind;
            },
            .function_type => {
                const typekind = trait_field.extract_function_type();

                return typekind;
            },
            .tuple_type => unreachable,
            else => {
                // ._type_identifier
                const type_kind = trait_field.extract_type_ref();

                assert(type_kind == .identifier);
                return type_kind;
            },
        };
    }

    fn extract_literal_type(_: *const Self) void {
        @panic("todo");
    }

    fn extract_array_type(self: *const Self) TypeKind {
        const parser = self.ctx.parser;

        const type_field = self.get_field("element") orelse unreachable;
        const typekind = type_field.extract_type_ref();

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

    fn extract_function_type(self: *const @This()) TypeKind {
        const parser = self.ctx.parser;
        assert(self.node_type == .function_type); // todo: remove

        // TODO: $for_lifetimes?
        // var params = std.ArrayList(procedure.Param).init(self.allocator);

        const parameters_field = self.get_field_unchecked("parameters"); // TODO: $parameters
        const params = parameters_field.extract_parameters();

        const return_type = blk: {
            if (self.get_field("return_type")) |return_type_field| {
                const typekind = return_type_field.extract_type_ref();

                const return_type_ptr = self.allocator.create(@TypeOf(typekind)) catch unreachable;
                return_type_ptr.* = typekind;

                break :blk return_type_ptr;

                // FIX: $_type
            } else break :blk null;
        };

        if (self.get_field("trait")) |field| {
            _ = {
                if (field.node_type == .scoped_type_identifier) {
                    // const scoped_path = PathParser.init(self.allocator, parser, field);
                    @panic("todo"); // FIX:
                } else {
                    assert(field.node_type == .type_identifier);
                    const typekind = field.extract_type_ref();

                    assert(typekind == .identifier);

                    assert(str.eql(typekind.identifier.text, "Fn") or
                        str.eql(typekind.identifier.text, "FnOnce") or
                        str.eql(typekind.identifier.text, "FnMut"));
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

                if (str.eql(text, "unsafe")) continue;

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
