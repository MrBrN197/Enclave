const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;

const c = @import("./c.zig");

const eprintln = @import("./root.zig").eprintln;
const eprint = @import("./root.zig").eprint;
const eql = @import("./root.zig").eql;

const main = @import("./main.zig"); // TODO:

const NodeType = @import("./node/types.zig").NodeType;

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

    pub fn write_to(self: *const Node, parser: *const main.Parser, writer: Writer) void {
        switch (self.node_type) {
            .source_file => {
                var children = self.get_children_named();
                defer children.clearAndFree();
                defer for (children.items) |*child| child.deinit();

                for (children.items) |child| {
                    child.write_to(parser, writer);
                }
            },
            .function_item => self.function_item_str(parser, writer),
            .generic_type => self.generic_type_str(parser, writer),
            .parameters => self.parameters_str(parser, writer),
            .scoped_type_identifier => self.scoped_type_identifier_str(parser, writer),
            .struct_item => self.struct_item_str(parser, writer),
            .type_identifier => self.type_identifier_str(parser, writer),
            .unit_type => self.unit_type_str(parser, writer),

            .type_item => {
                self.type_item_str(parser, writer);
            },

            else => |tag| {
                const gray_open = "\u{001b}[38;5;8m\n";
                const gray_close = "\u{001b}[m\n";
                out(writer, gray_open ++ "// TODO: {s}" ++ gray_close, .{@tagName(tag)});
            },
        }
    }

    fn type_identifier_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const text = parser.node_to_string(self.node, self.allocator);
        out(writer, "{s}", .{text});
    }

    fn arguments_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = self; // autofix
        _ = parser; // autofix
        _ = writer; // autofix
    }

    fn struct_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable; // $._type_identifier),

        if (self.get_field("type_parameters")) |field| field.write_to(parser, writer);

        const name = parser.node_to_string(name_field.node, self.allocator);
        out(writer, "const {s}", .{name});

        if (self.get_field("body")) |field| {
            field.write_to(parser, writer);
        }
        out(writer, "\n", .{});
    }

    fn body_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const node_name = get_type(self);

        if (eql("field_declaration_list", node_name)) {
            const children = get_children_named(self);
            out(writer, " = struct {{\n", .{});

            for (children.items) |field_decl| {
                const field_name = get_type(field_decl);
                if (eql("field_declaration", field_name)) {
                    const name_field = self.get_field("name") orelse unreachable;
                    const type_field = self.get_field("type") orelse unreachable;
                    const name = parser.node_to_string(name_field.node, self.allocator);
                    const _type = parser.node_to_string(type_field.node, self.allocator);
                    out(writer, "\t{s}: {s},\n", .{ name[0], _type[0] });
                }
            }
            out(writer, "}};", .{});
        } else {
            out(writer, " = ", .{});
            assert(eql("ordered_field_declaration_list", node_name));
            const _type_field = self.get_field("type") orelse unreachable;
            const _type = parser.node_to_string(_type_field.node, self.allocator);

            // TODO: convert type;
            for (_type) |str| {
                out(writer, "{s}", .{str});
            }
            out(writer, ";", .{});
        }
    }

    fn const_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn macro_definition_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn empty_statement_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn attribute_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        out(writer, "//", .{});
        out(writer, "{s}", .{parser.node_to_string(self.node, self.allocator)});
    }
    fn inner_attribute_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn mod_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn foreign_mod_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn union_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn type_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field_unchecked("name"); // $._type_identifier),

        const name = parser.node_to_string(name_field.node, self.allocator);
        if (self.get_field("type_parameters")) |type_parameters_field| {
            _ = type_parameters_field; // autofix
        } // ('type_parameters', optional($.type_parameters)),

        const type_field = self.get_field("type") orelse unreachable; // ('type', $._type),

        out(writer, "const {s}", .{name});
        out(writer, " = ", .{});
        type_field.write_to(parser, writer);
        out(writer, ";\n", .{});
    }
    fn extern_crate_declaration_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;
        const name = parser.node_to_string(name_field.node, self.allocator);

        if (self.get_field("alias")) |alias_field| {
            const alias = parser.node_to_string(alias_field.node, self.allocator);
            out(writer, "\nconst ", .{});

            out(writer, "{s}", .{alias});
            out(writer, " = @import(\"", .{});
            out(writer, "{s}", .{name});
            out(writer, "\");", .{});
        } else {
            out(writer, "const std = @import(\"", .{});
            for (name) |str| {
                out(writer, "{s}", .{str});
            }
            out(writer, "\");", .{});
        }

        out(writer, "\n", .{});
    }
    fn function_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {

        // optional($.visibility_modifier),
        // optional($.function_modifiers),

        out(writer, "fn ", .{});
        if (self.get_field("name")) |name_field| {
            const id = parser.node_to_string(name_field.node, self.allocator);
            out(writer, "{s}", .{id});
        }

        out(writer, "(", .{});
        if (self.get_field("type_parameters")) |type_parameters_field| type_parameters_field.write_to(parser, writer);
        out(writer, ") ", .{});

        if (self.get_field("parameters")) |parameters_field| parameters_field.write_to(parser, writer);
        if (self.get_field("return_type")) |return_type| { // _type
            return_type.write_to(parser, writer);
        } else {
            self.unit_type_str(parser, writer);
        }

        out(writer, " {{\n", .{});
        // optional($.where_clause),
        // field('body', $.block),
        out(writer, "}}\n", .{});
    }

    fn parameters_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void { //TODO: writer

        const children = self.get_children_named();
        for (children.items) |child| {
            if (eql(child.sym, "attribute_item")) {
                child.write_to(parser, writer);
            }

            if (eql(child.sym, "parameter")) {
                child.write_to(parser, writer);
            } else if (eql(child.sym, "self_parameter")) {
                child.write_to(parser, writer);
            } else if (eql(child.sym, "variadic_parameter")) {
                child.write_to(parser, writer);
            } else if (eql(child.sym, "_type")) {
                child.write_to(parser, writer);
            } else {
                out(writer, " _ ", .{});
            }

            out(writer, ",", .{});
        }
    }
    fn call_expression_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void { //TODO: writer

        const function = self.get_field("function") orelse unreachable;
        const args = self.get_field("arguments") orelse unreachable;

        const function_value = parser.node_to_string(function.node, self.allocator);
        const args_value = parser.node_to_string(args.node, self.allocator);

        for (function_value) |str| {
            out(writer, "{s}", .{str});
        }
        for (args_value) |str| {
            out(writer, "{s}", .{str});
        }
        out(writer, "\n", .{});
    }
    fn enum_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn function_signature_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn impl_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn trait_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;
        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn associated_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn let_declaration_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn self_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = writer; // autofix
        _ = parser; // autofix
        _ = self; // autofix
        //
    }
    fn identifier_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        assert(eql("identifier", get_type(self)) or
            eql("type_identifier", get_type(self)) or
            eql("field_identifier", get_type(self)));

        const source = parser.node_to_string(self.node, self.allocator);
        out(writer, "{s}", .{source});
    }
    fn super_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = writer; // autofix
        _ = parser; // autofix
        _ = self; // autofix
    }
    fn crate_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = writer; // autofix
        _ = parser; // autofix
        _ = self; // autofix
    }
    fn scoped_identifier_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        assert(eql(get_type(self), "scoped_identifier"));

        if (self.get_field("path")) |field| {
            const field_sym = get_type(field);

            if (eql(field_sym, "self")) {
                field.write_to(parser, writer);
            } else if (eql(field_sym, "identifier")) {
                field.write_to(parser, writer);
            } else if (eql(field_sym, "metavariable")) {
                out(writer, "{s}", .{parser.node_to_string(field_sym.node, self.allocator)});
                field.write_to(parser, writer);
            } else if (eql(field_sym, "super")) {
                field.write_to(parser, writer);
            } else if (eql(field_sym, "crate")) {
                field.write_to(parser, writer);
            } else if (eql(field_sym, "scoped_identifier")) {
                field.write_to(parser, writer);
            } else if (eql(field_sym, "bracketed_type")) {
                field.write_to(parser, writer);
            } else if (eql(field_sym, "generic_type")) {
                field.write_to(parser, writer);
            } else {
                unreachable;
            }
        }

        const name_field = self.get_field("name") orelse unreachable; // choice($.identifier, $.super)),

        const name_sym = get_type(name_field);
        if (eql(name_sym, "identifier")) {
            name_field.write_to(parser, writer);
        } else if (eql(name_sym, "super")) {
            name_field.write_to(parser, writer);
        } else unreachable;
    }
    fn bracketed_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const sym = get_type(self);

        // TODO: bracketed type
        if (eql(sym, "qualified_type")) {
            self.write_to(parser, writer);
        } else { // $_type
            self.write_to(parser, writer);
        }
    }
    fn generic_type_with_turbofish_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const type_field = self.get_field_unchecked("type");

        assert(eql(type_field.sym, "scoped_identifier") or eql(type_field.sym, "type_identifier"));

        const type_arguments = self
            .get_field_unchecked("type_arguments")
            .get_children_named();

        out(writer, "<", .{});
        for (type_arguments.items) |ty_arg| ty_arg.write_to(parser, writer);
        out(writer, ">", .{});
    }
    fn _reserved_identifier_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = writer; // autofix
        _ = parser; // autofix
        _ = self; // autofix
        //
    }
    fn use_as_clause_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = writer; // autofix
        _ = parser; // autofix
        _ = self; // autofix
        //
    }
    fn use_list_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const claues = get_children_named(self); // _use_clause

        for (claues.items) |clause| {
            const sym = get_type(clause);
            if (eql(sym, "use_as_clause")) {
                clause.write_to(parser, writer);
            } else if (eql(sym, "use_list")) {
                clause.write_to(parser, writer);
            } else if (eql(sym, "scoped_use_list")) {
                clause.write_to(parser, writer);
            } else if (eql(sym, "use_wildcard")) {
                clause.write_to(parser, writer);
            } else { // _path
                clause.write_to(parser, writer);
            }
        }
    }
    fn _path_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = writer; // autofix
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
    fn scoped_use_list_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        assert(eql(get_type(self), "scoped_use_list"));

        if (self.get_field("path")) |path_field| { // $._path
            path_field.write_to(parser, writer);
        }
        const list_field = self.get_field("list") orelse unreachable; // $.use_list),
        list_field.write_to(parser, writer);
    }
    fn use_wildcard_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = writer; // autofix
        _ = parser; // autofix
        assert(eql(get_type(self), "use_wildcard"));
        //
    }
    fn use_declaration_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const argument_field = self.get_field("argument") orelse unreachable;

        const use_clause_sym = get_type(argument_field);

        const string = c.ts_node_string(self);
        out(writer, "Syntax tree: {s}\n", .{string});
        out(writer, "// const ", .{});

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

        argument_field.write_to(parser, writer);

        out(writer, "\n", .{});
    }
    fn static_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node, self.allocator);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node, self.allocator);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node, self.allocator);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn abstract_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix
        _ = self; // autofix
        out(writer, "TODO: -> {{{{ABSTRACT_TYPE}}}}", .{});
    }
    fn reference_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        // NOTE: lifetime ignored

        out(writer, "*", .{});
        if (self.get_field("mutable_specifier")) |_| { // mut
            out(writer, " ", .{});
        } else { // const
            out(writer, "const ", .{});
        }

        const ty_field = self.get_field("type") orelse unreachable;
        ty_field.write_to(parser, writer);
    }

    fn pointer_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const tyfield = self.get_field("type") orelse unreachable;

        out(writer, "*", .{});
        if (self.get_field("mutable_specifier")) |_| { // mut
            out(writer, " ", .{});
        } else { // const
            out(writer, "const ", .{});
        }

        tyfield.write_to(parser, writer);
    }
    fn generic_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const type_field = self.get_field_unchecked("type");

        assert((eql(type_field.sym, "type_identifier") or
            eql(type_field.sym, "reserved_identifier") or
            eql(type_field.sym, "scoped_type_identifier")));

        type_field.write_to(parser, writer);

        const type_args = self.get_field_unchecked("type_arguments").get_children_named();

        out(writer, "<", .{});
        for (type_args.items) |ty_arg| ty_arg.write_to(parser, writer);
        out(writer, ">", .{});
    }

    fn scoped_type_identifier_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;
        const path_field = self.get_field("path") orelse unreachable;

        const path = parser.node_to_string(path_field.node, self.allocator);

        var split = mem.splitSequence(u8, path, "::");

        var parts = std.ArrayList([]const u8).init(self.allocator);

        while (split.next()) |part| {
            parts.append(part) catch unreachable;
        }

        const joined = std.mem.join(self.allocator, "::", parts.items) catch unreachable;

        const name = parser.node_to_string(name_field.node, self.allocator);
        out(writer, "{s}", .{joined});
        out(writer, ".", .{});
        out(writer, "{s}", .{name});
    }
    fn tuple_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const types = get_children_named(self);
        out(writer, "struct {{", .{});
        for (types.items) |ty| {
            ty.write_to(parser, writer);
            out(writer, ",", .{});
        }
        out(writer, "}}", .{});
    }

    fn unit_type_str(_: *const @This(), _: *const main.Parser, writer: Writer) void {
        out(writer, "void", .{});
    }

    fn array_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const element = self.get_field("element") orelse unreachable;

        if (self.get_field("length")) |length_field| {
            const length_expression = parser.node_to_string(length_field.node, self.allocator);
            out(writer, "[", .{});
            out(writer, "{s}", .{length_expression});
            out(writer, "]", .{});
        } else {
            out(writer, "[_]", .{});
        }

        element.write_to(parser, writer);
    }
    fn function_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix
        _ = self; // autofix
        out(writer, "TODO:  -> {{{{FUNCTION_TYPE}}}}", .{});
    }
    fn macro_invocation_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix

        _ = self; // autofix
        out(writer, "TODO:  -> {{{{MACRO_INVOCATION}}}}", .{});
    }
    fn never_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix
        _ = self; // autofix
        out(writer, "noreturn", .{});
    }
    fn dynamic_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix
        _ = self; // autofix
        out(writer, "TODO:  -> {{{{DYNAMIC_TYPE}}}}", .{});
    }
    fn bounded_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix
        _ = self; // autofix
        out(writer, "TODO:  -> {{{{BOUNDED_TYPE}}}}", .{});
    }
    fn removed_trait_bound_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix
        _ = self; // autofix
        out(writer, "TODO:  -> {{{{REMOVED_TRAIT_BOUND}}}}", .{});
    }
    fn primitive_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        // TODO: assert type ==  $.primitive_type;
        const source = parser.node_to_string(self.node, self.allocator);
        out(writer, "{s}", source, .{});
    }
};
