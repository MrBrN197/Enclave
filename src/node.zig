const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;

const c = @import("./c.zig");

const eprintln = @import("./root.zig").eprintln;
const eprint = @import("./root.zig").eprint;
const eql = @import("./root.zig").eql;

const Parser = @import("./parser.zig").Parser; // TODO:

const node_types = @import("./node/types.zig");
const NodeType = node_types.NodeType;
const NodeItem = node_types.NodeItem;

const Writer = @TypeOf(std.io.getStdOut().writer());
const Allocator = std.mem.Allocator;

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
    assert(mem.len(node_name) < 1024);
    const copy = allocator.alloc(u8, 1024) catch unreachable; // TODO:
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

    pub fn init(node: c.TSNode, allocator: std.mem.Allocator) @This() {
        const point_start = c.ts_node_start_point(node);
        const point_end = c.ts_node_end_point(node);

        const sym_name = get_type(node, allocator);
        const node_type = NodeType.from_string(sym_name);

        return @This(){
            .node_type = node_type,
            .node = node,
            .sym = sym_name,
            .row_start = .{ point_start.row, point_start.column },
            .row_end = .{ point_end.row, point_end.column },
            .allocator = allocator,
        };
    }

    pub fn deinit(_: *Node) void {
        // FIX:
    }

    pub fn extract_type_item(self: *const Node, parser: *const Parser) NodeItem {
        // TODO: $visibility_modifier

        const name_field = self.get_field_unchecked("name"); //  $_type_identifier
        const typename = parser.node_to_string(name_field.node, self.allocator);
        // TODO: const type_parameters_field = if (self.get_field("type_parameters")) //  $type_parameters

        // TODO: const type_field = self.get_field("type"); // $_type
        // TODO: $where_clause,

        const item_data = NodeItem.ItemData{
            .type_item = .{
                .name = typename,
                .definition = null, // TODO:
            },
        };
        const result = NodeItem.init(item_data, typename);
        return result;
    }

    pub fn extract_node_items(self: *const Node, parser: *const Parser, collect: *std.ArrayList(NodeItem)) void {
        switch (self.node_type) {
            .source_file => {
                var children = self.get_children_named();
                defer children.clearAndFree();
                defer for (children.items) |*child| child.deinit();

                for (children.items) |child| {
                    child.extract_node_items(parser, collect);
                }
            },

            .type_item => {
                const item = self.extract_type_item(parser);
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
            .line_comment, .use_declaration => return,

            else => |tag| {
                const gray_open = "\u{001b}[38;5;8m\n";
                const gray_close = "\u{001b}[m\n";
                eprintln(gray_open ++ "// TODO: {s}" ++ gray_close, .{@tagName(tag)});
            },
        }
    }

    pub fn get_source_text(self: *const Node) []const []const u8 {
        _ = self; // autofix

        const slice = [1][]const u8{""};
        return &slice;
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
                    \\ Source:
                    \\   {s}
                    \\ ===============[ERROR]======================
                ++ "\u{001B}[m",
                .{
                    field_name,
                    self.sym,
                    c.ts_node_string(self.node)[0..100],
                    self.get_source_text()[0],
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
        const ts_node = self.node;

        var children = std.ArrayList(Node).init(self.allocator);
        const named_child_count = c.ts_node_named_child_count(ts_node);

        for (0..named_child_count) |idx| {
            children.append(
                Node.init(
                    get_child_named(ts_node, @truncate(idx)) orelse unreachable,
                    self.allocator,
                ),
            ) catch unreachable;
        }

        return children;
    }

    fn extract_type_identifier(self: *const @This(), parser: *const Parser) void {
        const text = parser.node_to_string(self.node, self.allocator);
        _ = text; // autofix
        // out(writer, "{s}", .{text});
    }

    fn extract_arguments(self: *const @This(), parser: *const Parser) void {
        _ = self; // autofix
        _ = parser; // autofix
    }

    fn extract_struct_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: if get_field(self,"visibility_modifier") ||

        assert(self.node_type == .struct_item);

        const id_field = self.get_field_unchecked("name"); // $.identifier,
        const id = parser.node_to_string(id_field.node, self.allocator);
        // if (self.get_field(self, "type_parameters")) ||  // TODO:

        var fields = std.ArrayList(NodeItem.ItemData.Object.Field).init(self.allocator);

        if (self.get_field("body")) |body_field| {
            // TODO: if get_field(body_field, "where_clause") |where_clause field| {}
            if (eql(body_field.sym, "field_declaration_list")) {
                const decls = body_field.get_children_named();
                for (decls.items) |decl| {
                    if (decl.node_type == .field_declaration) {
                        // $visibility_modifier?,
                        const name_field = decl.get_field_unchecked("name"); //  $_field_identifier
                        const name = parser.node_to_string(name_field.node, self.allocator);
                        const type_field = decl.get_field_unchecked("type"); //  $_type
                        _ = type_field; // autofix
                        // TODO: extract_type_ref(type_field);

                        const field = NodeItem.ItemData.Object.Field{
                            .name = name,
                            .type_ref = null,
                        };
                        fields.append(field) catch unreachable;
                    } else if (decl.node_type == .attribute_item) {
                        unreachable;
                    } else unreachable;
                }
            } else if (eql(body_field.sym, "ordered_field_declaration_list")) {
                unreachable; // TODO:
            } else unreachable;
        }

        const data = NodeItem.ItemData{
            .object_item = .{
                .fields = fields.items,
                .procedures = null, // TODO:
            },
        };

        assert(!eql(id, ""));
        const result = NodeItem.init(data, id);
        return result;
    }

    fn extract_body(self: *const @This(), parser: *const Parser) void {
        const node_name = get_type(self);

        if (eql("field_declaration_list", node_name)) {
            const children = get_children_named(self);
            //     out(writer, " = struct {{\n", .{});

            for (children.items) |field_decl| {
                const field_name = get_type(field_decl);
                if (eql("field_declaration", field_name)) {
                    const name_field = self.get_field("name") orelse unreachable;
                    const type_field = self.get_field("type") orelse unreachable;
                    const name = parser.node_to_string(name_field.node, self.allocator);
                    _ = name; // autofix
                    const _type = parser.node_to_string(type_field.node, self.allocator);
                    _ = _type; // autofix
                    //             out(writer, "\t{s}: {s},\n", .{ name[0], _type[0] });
                }
            }
            //     out(writer, "}};", .{});
        } else {
            //     out(writer, " = ", .{});
            assert(eql("ordered_field_declaration_list", node_name));
            const _type_field = self.get_field("type") orelse unreachable;
            const _type = parser.node_to_string(_type_field.node, self.allocator);

            // TODO: convert type;
            for (_type) |str| {
                _ = str; // autofix
                //         out(writer, "{s}", .{str});
            }
            //     out(writer, ";", .{});
        }
    }

    fn extract_const_item(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_macro_definition(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_empty_statement(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_attribute_item(self: *const @This(), parser: *const Parser) void {
        _ = self; // autofix
        _ = parser; // autofix
        // out(writer, "//", .{});
        // out(writer, "{s}", .{parser.node_to_string(self.node, self.allocator)});
    }
    fn extract_inner_attribute_item(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_mod_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: $visibility_modifier

        const name_field = self.get_field_unchecked("name");
        const name = parser.node_to_string(name_field.node, self.allocator);

        if (self.get_field("body")) |_| { // $declaration_list);
            unreachable;
        }

        const item_data = NodeItem.ItemData{
            .module_item = .{ .content = null }, // TODO:
        };

        const result = NodeItem.init(item_data, name);
        return result;
    }
    fn extract_foreign_mod_item(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_union_item(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }

    fn extract_extern_crate_declaration(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;
        const name = parser.node_to_string(name_field.node, self.allocator);

        if (self.get_field("alias")) |alias_field| {
            const alias = parser.node_to_string(alias_field.node, self.allocator);
            _ = alias; // autofix
            //     out(writer, "\nconst ", .{});

            //     out(writer, "{s}", .{alias});
            //     out(writer, " = @import(\"", .{});
            //     out(writer, "{s}", .{name});
            //     out(writer, "\");", .{});
        } else {
            //     out(writer, "const std = @import(\"", .{});
            for (name) |str| {
                _ = str; // autofix
                //         out(writer, "{s}", .{str});
            }
            //     out(writer, "\");", .{});
        }

        // out(writer, "\n", .{});
    }

    fn extract_type_ref(self: *const @This(), parser: *const Parser) ?NodeItem.ItemData.Type {
        const name = blk: {
            switch (self.node_type) {
                .abstract_type => {
                    unreachable;
                },
                .reference_type => {
                    unreachable;
                },
                .metavariable => {
                    unreachable;
                },
                .pointer_type => {
                    unreachable;
                },
                .generic_type => {
                    unreachable;
                },
                .scoped_type_identifier => {
                    unreachable;
                },
                .tuple_type => {
                    unreachable;
                },
                .unit_type => {
                    return null;
                },
                .array_type => {
                    unreachable;
                },
                .function_type => {
                    unreachable;
                },
                .macro_invocation => {
                    unreachable;
                },
                .never_type => {
                    break :blk "noreturn";
                },
                .dynamic_type => {
                    unreachable;
                },
                .bounded_type => {
                    unreachable;
                },
                .removed_trait_bound => {
                    unreachable;
                },
                .primitive_type => {
                    // TODO: test

                    const text = parser.node_to_string(self.node, self.allocator);
                    break :blk text;
                },
                else => { // _type_identifier
                    const text = parser.node_to_string(self.node, self.allocator);
                    break :blk text;
                },
            }
        };

        const result = NodeItem.ItemData.Type{
            .name = name,
            .definition = null, // TODO:
        };
        return result;
    }

    fn extract_function_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: self.get_field("visibility_modifier")
        // TODO: self.get_field("function_modifiers")

        const name_field = self.get_field_unchecked("name");
        assert(eql(name_field.sym, "identifier")); // TODO: $metavariable
        const name = parser.node_to_string(name_field.node, self.allocator);

        // TODO: if (self.get_field("type_parameters"))
        const parameters_field = self.get_field_unchecked("parameters");
        const params = parameters_field.extract_parameters(parser);

        // TODO: get_field("where_clause"),

        const return_type = blk: {
            if (self.get_field("return_type")) |return_type| {
                const _type = return_type.extract_type_ref(parser);
                break :blk _type;
            } else break :blk null;
        };

        // TODO: field('body') //  $.block;

        const item_data = NodeItem.ItemData{
            .procedure_item = .{
                .params = params.items,
                .return_type = return_type,
            },
        };

        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_parameters(
        self: *const @This(),
        parser: *const Parser,
    ) std.ArrayList(NodeItem.ItemData.Procedure.Param) {
        var result = std.ArrayList(NodeItem.ItemData.Procedure.Param).init(self.allocator);

        const children = self.get_children_named();
        for (children.items) |child| {
            if (child.node_type == .attribute_item) {
                unreachable;
            }

            const name_type = blk: {
                if (child.node_type == .parameter) {
                    unreachable;
                } else if (child.node_type == .self_parameter) {
                    unreachable;
                } else if (child.node_type == .variadic_parameter) {
                    unreachable;
                } else { // _type
                    const text = parser.node_to_string(child.node, self.allocator);
                    if (eql(text, "_")) break :blk .{ .name = text, .type = null };
                    // TODO: return extract_type()
                    unreachable;
                }
            };

            const name = name_type.name;
            const typename = name_type.type;

            result.append(NodeItem.ItemData.Procedure.Param{
                .name = name,
                .typename = typename,
            }) catch unreachable; // FIX:
        }
        return result;
    }
    fn extract_call_expression(self: *const @This(), parser: *const Parser) void { //TODO: writer

        const function = self.get_field("function") orelse unreachable;
        const args = self.get_field("arguments") orelse unreachable;

        const function_value = parser.node_to_string(function.node, self.allocator);
        const args_value = parser.node_to_string(args.node, self.allocator);

        for (function_value) |str| {
            _ = str; // autofix
            //     out(writer, "{s}", .{str});
        }
        for (args_value) |str| {
            _ = str; // autofix
            //     out(writer, "{s}", .{str});
        }
        // out(writer, "\n", .{});
    }
    fn extract_enum_item(self: *const @This(), parser: *const Parser) NodeItem {
        // TODO: $visibility_modifier;

        const name_field = self.get_field_unchecked("name");
        const name = parser.node_to_string(name_field.node, self.allocator);

        // TODO: if (get_field("type_parameters") $type_parameters;
        // TODO: $where_clause;

        const body = self.get_field_unchecked("body"); // $enum_variant_list;

        const variants = body.extract_enum_variants(parser);

        const item_data = NodeItem.ItemData{ .enum_item = .{
            .procedures = null,
            .variants = variants.items,
        } };
        const result = NodeItem.init(item_data, name);
        return result;
    }

    fn extract_enum_variants(self: *const @This(), parser: *const Parser) std.ArrayList(NodeItem.ItemData.Enum.Variant) {
        const children = self.get_children_named(); // FIX:

        var variants = std.ArrayList(NodeItem.ItemData.Enum.Variant).init(self.allocator); // FIX:

        for (children.items) |child| {
            // TODO: $attribute_item;

            assert(child.node_type == .enum_variant);

            // TODO: $?visibility_modifier
            const name_field = child.get_field_unchecked("name"); // identifier
            const name = parser.node_to_string(name_field.node, self.allocator);
            // TODO: if child.get_field("body" // $field_declaration_list | $ordered_field_declaration_list

            // TODO: if child.get_field("value", $_expression?

            const variant = NodeItem.ItemData.Enum.Variant{
                .name = name,
            };
            variants.append(variant) catch unreachable;
        }
        return variants;
    }

    fn extract_function_signature_item(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_impl_item(self: *const @This(), parser: *const Parser) NodeItem {
        _ = parser; // autofix
        if (self.get_field("type_parameters")) |field| { //$type_parameters
            _ = field; // autofix

        }

        if (self.get_field("trait")) |_| {
            // TODO: for trait
            // ($_type_identifier | $scoped_type_identifier | $generic_type)

        }

        // TODO: const type = extract_type_ref(get_field_unchecked(field("type"))); // $_type

        // TODO: $where_clause?
        // TODO: if(self.get_field("body") || // $declaration_list

        const item_data = NodeItem.ItemData{
            .impl_item = .{ .procedures = null }, // TODO:
        };
        const result = NodeItem.init(item_data, null);
        return result;
    }

    fn extract_trait_item(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;
        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_associated_type(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_let_declaration(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_self(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        //
    }
    fn extract_identifier(self: *const @This(), parser: *const Parser) void {
        assert(eql("identifier", get_type(self)) or
            eql("type_identifier", get_type(self)) or
            eql("field_identifier", get_type(self)));

        const source = parser.node_to_string(self.node, self.allocator);
        _ = source; // autofix
        // out(writer, "{s}", .{source});
    }
    fn extract_super(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
    }
    fn extract_crate(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
    }
    fn extract_scoped_identifier(self: *const @This(), parser: *const Parser) void {
        assert(eql(get_type(self), "scoped_identifier"));

        if (self.get_field("path")) |field| {
            const field_sym = get_type(field);

            if (eql(field_sym, "self")) {
                field.write_to(parser);
            } else if (eql(field_sym, "identifier")) {
                field.write_to(parser);
            } else if (eql(field_sym, "metavariable")) {
                //         out(writer, "{s}", .{parser.node_to_string(field_sym.node, self.allocator)});
                field.write_to(parser);
            } else if (eql(field_sym, "super")) {
                field.write_to(parser);
            } else if (eql(field_sym, "crate")) {
                field.write_to(parser);
            } else if (eql(field_sym, "scoped_identifier")) {
                field.write_to(parser);
            } else if (eql(field_sym, "bracketed_type")) {
                field.write_to(parser);
            } else if (eql(field_sym, "generic_type")) {
                field.write_to(parser);
            } else {
                unreachable;
            }
        }

        const name_field = self.get_field("name") orelse unreachable; // choice($.identifier, $.super)),

        const name_sym = get_type(name_field);
        if (eql(name_sym, "identifier")) {
            name_field.write_to(parser);
        } else if (eql(name_sym, "super")) {
            name_field.write_to(parser);
        } else unreachable;
    }
    fn extract_bracketed_type(self: *const @This(), parser: *const Parser) void {
        const sym = get_type(self);

        // TODO: bracketed type
        if (eql(sym, "qualified_type")) {
            self.write_to(parser);
        } else { // $_type
            self.write_to(parser);
        }
    }
    fn extract_generic_type_with_turbofish(self: *const @This(), parser: *const Parser) void {
        const type_field = self.get_field_unchecked("type");

        assert(eql(type_field.sym, "scoped_identifier") or eql(type_field.sym, "type_identifier"));

        const type_arguments = self
            .get_field_unchecked("type_arguments")
            .get_children_named();

        // out(writer, "<", .{});
        for (type_arguments.items) |ty_arg| ty_arg.write_to(parser);
        // out(writer, ">", .{});
    }
    fn extract__reserved_identifier(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        //
    }
    fn extract_use_as_clause(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        //
    }
    fn extract_use_list(self: *const @This(), parser: *const Parser) void {
        const claues = get_children_named(self); // _use_clause

        for (claues.items) |clause| {
            const sym = get_type(clause);
            if (eql(sym, "use_as_clause")) {
                clause.write_to(parser);
            } else if (eql(sym, "use_list")) {
                clause.write_to(parser);
            } else if (eql(sym, "scoped_use_list")) {
                clause.write_to(parser);
            } else if (eql(sym, "use_wildcard")) {
                clause.write_to(parser);
            } else { // _path
                clause.write_to(parser);
            }
        }
    }
    fn extract__path(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        //     $.self,
        // alias(choice(...primitiveTypes), $.identifier),
        // $.metavariable,
        // $.super,
        // $.crate,
        // $.identifier,
        // $.scoped_identifier,
        // $._reserved_identifier,
    }
    fn extract_scoped_use_list(self: *const @This(), parser: *const Parser) void {
        assert(eql(get_type(self), "scoped_use_list"));

        if (self.get_field("path")) |path_field| { // $._path
            path_field.write_to(parser);
        }
        const list_field = self.get_field("list") orelse unreachable; // $.use_list),
        list_field.write_to(parser);
    }
    fn extract_use_wildcard(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        assert(eql(get_type(self), "use_wildcard"));
        //
    }
    fn extract_use_declaration(self: *const @This(), parser: *const Parser) void {
        const argument_field = self.get_field("argument") orelse unreachable;

        const use_clause_sym = get_type(argument_field);

        const string = c.ts_node_string(self);
        _ = string; // autofix
        // out(writer, "Syntax tree: {s}\n", .{string});
        // out(writer, "// const ", .{});

        for ([_][]const u8{
            "crate",
            "identifier",
            "metavariable",
            "scoped_identifier",
            "scoped_use_list",
            "self",
            "super",
            "use_as_clause",
            "use_list",
            "use_wildcard",
        }) |sym| {
            assert(eql(sym, use_clause_sym));
        }

        argument_field.write_to(parser);

        // out(writer, "\n", .{});
    }
    fn extract_static_item(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            _ = type_parameters; // autofix
            //     out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            _ = parameters; // autofix
            // out(
            //     writer,
            //     "Parameters:\n\t{s}\n",
            //     .{parameters},
            // );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            _ = return_type; // autofix
            // out(
            //     writer,
            //     "Return_type:\n\t{s}\n",
            //     .{return_type},
            // );
        }
    }
    fn extract_abstract_type(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        // out(writer, "TODO: -> {{{{ABSTRACT_TYPE}}}}", .{});
    }
    fn extract_reference_type(self: *const @This(), parser: *const Parser) void {
        // NOTE: lifetime ignored

        // out(writer, "*", .{});
        if (self.get_field("mutable_specifier")) |_| { // mut
            //     out(writer, " ", .{});
        } else { // const
            //     out(writer, "const ", .{});
        }

        const ty_field = self.get_field("type") orelse unreachable;
        ty_field.write_to(parser);
    }

    fn extract_pointer_type(self: *const @This(), parser: *const Parser) void {
        const tyfield = self.get_field("type") orelse unreachable;

        // out(writer, "*", .{});
        if (self.get_field("mutable_specifier")) |_| { // mut
            //     out(writer, " ", .{});
        } else { // const
            //     out(writer, "const ", .{});
        }

        tyfield.write_to(parser);
    }
    fn extract_generic_type(self: *const @This(), parser: *const Parser) void {
        const type_field = self.get_field_unchecked("type");

        assert((eql(type_field.sym, "type_identifier") or
            eql(type_field.sym, "reserved_identifier") or
            eql(type_field.sym, "scoped_type_identifier")));

        type_field.write_to(parser);

        const type_args = self.get_field_unchecked("type_arguments").get_children_named();

        // out(writer, "<", .{});
        for (type_args.items) |ty_arg| ty_arg.write_to(parser);
        // out(writer, ">", .{});
    }

    fn extract_scoped_type_identifier(self: *const @This(), parser: *const Parser) void {
        const name_field = self.get_field("name") orelse unreachable;
        const path_field = self.get_field("path") orelse unreachable;

        const path = parser.node_to_string(path_field.node, self.allocator);

        var split = mem.splitSequence(u8, path, "::");

        var parts = std.ArrayList([]const u8).init(self.allocator);

        while (split.next()) |part| {
            parts.append(part) catch unreachable;
        }

        const joined = std.mem.join(self.allocator, "::", parts.items) catch unreachable;
        _ = joined; // autofix

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix
        // out(writer, "{s}", .{joined});
        // out(writer, ".", .{});
        // out(writer, "{s}", .{name});
    }
    fn extract_tuple_type(self: *const @This(), parser: *const Parser) void {
        const types = get_children_named(self);
        // out(writer, "struct {{", .{});
        for (types.items) |ty| {
            ty.write_to(parser);
            //     out(writer, ",", .{});
        }
        // out(writer, "}}", .{});
    }

    fn extract_unit_type(_: *const @This(), _: *const Parser) void {
        // out(writer, "void", .{});
    }

    fn extract_array_type(self: *const @This(), parser: *const Parser) void {
        const element = self.get_field("element") orelse unreachable;

        if (self.get_field("length")) |length_field| {
            const length_expression = parser.node_to_string(length_field.node, self.allocator);
            _ = length_expression; // autofix
            //     out(writer, "[", .{});
            //     out(writer, "{s}", .{length_expression});
            //     out(writer, "]", .{});
        } else {
            //     out(writer, "[_]", .{});
        }

        element.write_to(parser);
    }
    fn extract_function_type(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        // out(writer, "TODO:  -> {{{{FUNCTION_TYPE}}}}", .{});
    }
    fn extract_macro_invocation(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix

        _ = self; // autofix
        // out(writer, "TODO:  -> {{{{MACRO_INVOCATION}}}}", .{});
    }
    fn extract_never_type(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        // out(writer, "noreturn", .{});
    }
    fn extract_dynamic_type(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        // out(writer, "TODO:  -> {{{{DYNAMIC_TYPE}}}}", .{});
    }
    fn extract_bounded_type(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        // out(writer, "TODO:  -> {{{{BOUNDED_TYPE}}}}", .{});
    }
    fn extract_removed_trait_bound(self: *const @This(), parser: *const Parser) void {
        _ = parser; // autofix
        _ = self; // autofix
        // out(writer, "TODO:  -> {{{{REMOVED_TRAIT_BOUND}}}}", .{});
    }
    fn extract_primitive_type(self: *const @This(), parser: *const Parser) void {
        // TODO: assert type ==  $.primitive_type;
        const source = parser.node_to_string(self.node, self.allocator);
        _ = source; // autofix
        // out(writer, "{s}", source, .{});
    }
};
