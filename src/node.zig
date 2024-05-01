const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;

const c = @import("./c.zig");

const eprintln = @import("./root.zig").eprintln;
const eprint = @import("./root.zig").eprint;

const main = @import("./main.zig"); // TODO:
const eql = main.eql;

const Writer = @TypeOf(std.io.getStdOut().writer());
const Allocator = std.mem.Allocator;

fn out(writer: Writer, comptime str: []const u8, args: anytype) void {
    return std.fmt.format(writer, str ++ "\n", args) catch unreachable;
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

pub fn get_children_named(node: c.TSNode, allocator: std.mem.Allocator) std.ArrayList(c.TSNode) {
    var array_list = std.ArrayList(c.TSNode).init(allocator);
    const child_count = c.ts_node_named_child_count(node);

    for (0..child_count) |idx| {
        const child = get_child_named(node, @truncate(idx)) orelse unreachable;
        array_list.append(child) catch unreachable;
    }

    return array_list;
}

pub fn get_type(node: c.TSNode, allocator: std.mem.Allocator) []const u8 {
    const node_name = c.ts_node_type(node);
    assert(mem.len(node_name) < 1024);
    const copy = allocator.alloc(u8, 1024) catch unreachable; // TODO:
    mem.copyForwards(u8, copy, node_name[0..mem.len(node_name)]);
    return copy[0..mem.len(node_name)];
}

pub const Node = struct {
    const NodeType = enum {
        abstract_type,
        arguments,
        array_expression,
        array_type,
        assignment_expression,
        associated_type,
        async_block,
        attribute,
        attribute_item,
        await_expression,
        base_field_initializer,
        binary_expression,
        block,
        block_comment,
        bounded_type,
        bracketed_type,
        break_expression,
        call_expression,
        closure_expression,
        compound_assignment_expr,
        const_block,
        const_item,
        const_parameter,
        constrained_type_parameter,
        continue_expression,
        crate,
        declaration_list,
        delim_token_tree,
        dynamic_type,
        else_clause,
        empty_statement,
        enum_item,
        enum_variant,
        enum_variant_list,
        expression_statement,
        extern_crate_declaration,
        extern_modifier,
        field_declaration,
        field_declaration_list,
        field_expression,
        field_initializer,
        field_initializer_list,
        float_literal,
        foreign_mod_item,
        for_expression,
        for_lifetimes,
        fragment_specifier,
        function_item,
        function_modifiers,
        function_signature_item,
        function_type,
        generic_function,
        generic_type,
        generic_type_with_turbofish,
        higher_ranked_trait_bound,
        identifier,
        if_expression,
        impl_item,
        index_expression,
        inner_attribute_item,
        integer_literal,
        label,
        last_match_arm,
        let_chain,
        let_condition,
        let_declaration,
        lifetime,
        line_comment,
        loop_expression,
        macro_definition,
        macro_invocation,
        macro_rule,
        match_arm,
        match_block,
        match_expression,
        match_pattern,
        metavariable,
        mod_item,
        mutable_specifier,
        never_type,
        optional_type_parameter,
        ordered_field_declaration_list,
        parameter,
        parameters,
        parenthesized_expression,
        pointer_type,
        primitive_type,
        qualified_type,
        range_expression,
        raw_string_literal_content,
        reference_expression,
        reference_type,
        removed_trait_bound,
        return_expression,
        scoped_identifier,
        scoped_type_identifier,
        scoped_type_identifier_in_expression_position,
        scoped_use_list,
        self,
        self_parameter,
        shebang,
        shorthand_field_initializer,
        static_item,
        string_content,
        string_literal,
        struct_expression,
        struct_item,
        super,
        token_binding_pattern,
        token_repetition,
        token_repetition_pattern,
        token_tree,
        token_tree_pattern,
        trait_bounds,
        trait_item,
        try_block,
        try_expression,
        tuple_expression,
        tuple_pattern,
        tuple_struct_pattern,
        tuple_type,
        type_arguments,
        type_binding,
        type_cast_expression,
        type_item,
        type_parameters,
        unary_expression,
        union_item,
        unit_expression,
        unit_type,
        unsafe_block,
        use_as_clause,
        use_declaration,
        use_list,
        use_wildcard,
        variadic_parameter,
        visibility_modifier,
        where_clause,
        where_predicate,
        while_expression,
        yield_expression,

        pub fn from_string(str: []const u8) NodeType {
            if (eql(str, "abstract_type")) {
                return .abstract_type;
            } else if (eql(str, "arguments")) {
                return .arguments;
            } else if (eql(str, "array_expression")) {
                return .array_expression;
            } else if (eql(str, "array_type")) {
                return .array_type;
            } else if (eql(str, "assignment_expression")) {
                return .assignment_expression;
            } else if (eql(str, "associated_type")) {
                return .associated_type;
            } else if (eql(str, "async_block")) {
                return .async_block;
            } else if (eql(str, "attribute")) {
                return .attribute;
            } else if (eql(str, "attribute_item")) {
                return .attribute_item;
            } else if (eql(str, "await_expression")) {
                return .await_expression;
            } else if (eql(str, "base_field_initializer")) {
                return .base_field_initializer;
            } else if (eql(str, "binary_expression")) {
                return .binary_expression;
            } else if (eql(str, "block")) {
                return .block;
            } else if (eql(str, "block_comment")) {
                return .block_comment;
            } else if (eql(str, "bounded_type")) {
                return .bounded_type;
            } else if (eql(str, "bracketed_type")) {
                return .bracketed_type;
            } else if (eql(str, "break_expression")) {
                return .break_expression;
            } else if (eql(str, "call_expression")) {
                return .call_expression;
            } else if (eql(str, "closure_expression")) {
                return .closure_expression;
            } else if (eql(str, "compound_assignment_expr")) {
                return .compound_assignment_expr;
            } else if (eql(str, "const_block")) {
                return .const_block;
            } else if (eql(str, "const_item")) {
                return .const_item;
            } else if (eql(str, "const_parameter")) {
                return .const_parameter;
            } else if (eql(str, "constrained_type_parameter")) {
                return .constrained_type_parameter;
            } else if (eql(str, "continue_expression")) {
                return .continue_expression;
            } else if (eql(str, "crate")) {
                return .crate;
            } else if (eql(str, "declaration_list")) {
                return .declaration_list;
            } else if (eql(str, "delim_token_tree")) {
                return .delim_token_tree;
            } else if (eql(str, "dynamic_type")) {
                return .dynamic_type;
            } else if (eql(str, "else_clause")) {
                return .else_clause;
            } else if (eql(str, "empty_statement")) {
                return .empty_statement;
            } else if (eql(str, "enum_item")) {
                return .enum_item;
            } else if (eql(str, "enum_variant")) {
                return .enum_variant;
            } else if (eql(str, "enum_variant_list")) {
                return .enum_variant_list;
            } else if (eql(str, "expression_statement")) {
                return .expression_statement;
            } else if (eql(str, "extern_crate_declaration")) {
                return .extern_crate_declaration;
            } else if (eql(str, "extern_modifier")) {
                return .extern_modifier;
            } else if (eql(str, "field_declaration")) {
                return .field_declaration;
            } else if (eql(str, "field_declaration_list")) {
                return .field_declaration_list;
            } else if (eql(str, "field_expression")) {
                return .field_expression;
            } else if (eql(str, "field_initializer")) {
                return .field_initializer;
            } else if (eql(str, "field_initializer_list")) {
                return .field_initializer_list;
            } else if (eql(str, "float_literal")) {
                return .float_literal;
            } else if (eql(str, "foreign_mod_item")) {
                return .foreign_mod_item;
            } else if (eql(str, "for_expression")) {
                return .for_expression;
            } else if (eql(str, "for_lifetimes")) {
                return .for_lifetimes;
            } else if (eql(str, "fragment_specifier")) {
                return .fragment_specifier;
            } else if (eql(str, "function_item")) {
                return .function_item;
            } else if (eql(str, "function_modifiers")) {
                return .function_modifiers;
            } else if (eql(str, "function_signature_item")) {
                return .function_signature_item;
            } else if (eql(str, "function_type")) {
                return .function_type;
            } else if (eql(str, "generic_function")) {
                return .generic_function;
            } else if (eql(str, "generic_type")) {
                return .generic_type;
            } else if (eql(str, "generic_type_with_turbofish")) {
                return .generic_type_with_turbofish;
            } else if (eql(str, "higher_ranked_trait_bound")) {
                return .higher_ranked_trait_bound;
            } else if (eql(str, "identifier")) {
                return .identifier;
            } else if (eql(str, "if_expression")) {
                return .if_expression;
            } else if (eql(str, "impl_item")) {
                return .impl_item;
            } else if (eql(str, "index_expression")) {
                return .index_expression;
            } else if (eql(str, "inner_attribute_item")) {
                return .inner_attribute_item;
            } else if (eql(str, "integer_literal")) {
                return .integer_literal;
            } else if (eql(str, "label")) {
                return .label;
            } else if (eql(str, "last_match_arm")) {
                return .last_match_arm;
            } else if (eql(str, "let_chain")) {
                return .let_chain;
            } else if (eql(str, "let_condition")) {
                return .let_condition;
            } else if (eql(str, "let_declaration")) {
                return .let_declaration;
            } else if (eql(str, "lifetime")) {
                return .lifetime;
            } else if (eql(str, "line_comment")) {
                return .line_comment;
            } else if (eql(str, "loop_expression")) {
                return .loop_expression;
            } else if (eql(str, "macro_definition")) {
                return .macro_definition;
            } else if (eql(str, "macro_invocation")) {
                return .macro_invocation;
            } else if (eql(str, "macro_rule")) {
                return .macro_rule;
            } else if (eql(str, "match_arm")) {
                return .match_arm;
            } else if (eql(str, "match_block")) {
                return .match_block;
            } else if (eql(str, "match_expression")) {
                return .match_expression;
            } else if (eql(str, "match_pattern")) {
                return .match_pattern;
            } else if (eql(str, "metavariable")) {
                return .metavariable;
            } else if (eql(str, "mod_item")) {
                return .mod_item;
            } else if (eql(str, "mutable_specifier")) {
                return .mutable_specifier;
            } else if (eql(str, "never_type")) {
                return .never_type;
            } else if (eql(str, "optional_type_parameter")) {
                return .optional_type_parameter;
            } else if (eql(str, "ordered_field_declaration_list")) {
                return .ordered_field_declaration_list;
            } else if (eql(str, "parameter")) {
                return .parameter;
            } else if (eql(str, "parameters")) {
                return .parameters;
            } else if (eql(str, "parenthesized_expression")) {
                return .parenthesized_expression;
            } else if (eql(str, "pointer_type")) {
                return .pointer_type;
            } else if (eql(str, "primitive_type")) {
                return .primitive_type;
            } else if (eql(str, "qualified_type")) {
                return .qualified_type;
            } else if (eql(str, "range_expression")) {
                return .range_expression;
            } else if (eql(str, "raw_string_literal_content")) {
                return .raw_string_literal_content;
            } else if (eql(str, "reference_expression")) {
                return .reference_expression;
            } else if (eql(str, "reference_type")) {
                return .reference_type;
            } else if (eql(str, "removed_trait_bound")) {
                return .removed_trait_bound;
            } else if (eql(str, "return_expression")) {
                return .return_expression;
            } else if (eql(str, "scoped_identifier")) {
                return .scoped_identifier;
            } else if (eql(str, "scoped_type_identifier")) {
                return .scoped_type_identifier;
            } else if (eql(str, "scoped_type_identifier_in_expression_position")) {
                return .scoped_type_identifier_in_expression_position;
            } else if (eql(str, "scoped_use_list")) {
                return .scoped_use_list;
            } else if (eql(str, "self")) {
                return .self;
            } else if (eql(str, "self_parameter")) {
                return .self_parameter;
            } else if (eql(str, "shebang")) {
                return .shebang;
            } else if (eql(str, "shorthand_field_initializer")) {
                return .shorthand_field_initializer;
            } else if (eql(str, "static_item")) {
                return .static_item;
            } else if (eql(str, "string_content")) {
                return .string_content;
            } else if (eql(str, "string_literal")) {
                return .string_literal;
            } else if (eql(str, "struct_expression")) {
                return .struct_expression;
            } else if (eql(str, "struct_item")) {
                return .struct_item;
            } else if (eql(str, "super")) {
                return .super;
            } else if (eql(str, "token_binding_pattern")) {
                return .token_binding_pattern;
            } else if (eql(str, "token_repetition")) {
                return .token_repetition;
            } else if (eql(str, "token_repetition_pattern")) {
                return .token_repetition_pattern;
            } else if (eql(str, "token_tree")) {
                return .token_tree;
            } else if (eql(str, "token_tree_pattern")) {
                return .token_tree_pattern;
            } else if (eql(str, "trait_bounds")) {
                return .trait_bounds;
            } else if (eql(str, "trait_item")) {
                return .trait_item;
            } else if (eql(str, "try_block")) {
                return .try_block;
            } else if (eql(str, "try_expression")) {
                return .try_expression;
            } else if (eql(str, "tuple_expression")) {
                return .tuple_expression;
            } else if (eql(str, "tuple_pattern")) {
                return .tuple_pattern;
            } else if (eql(str, "tuple_struct_pattern")) {
                return .tuple_struct_pattern;
            } else if (eql(str, "tuple_type")) {
                return .tuple_type;
            } else if (eql(str, "type_arguments")) {
                return .type_arguments;
            } else if (eql(str, "type_binding")) {
                return .type_binding;
            } else if (eql(str, "type_cast_expression")) {
                return .type_cast_expression;
            } else if (eql(str, "type_item")) {
                return .type_item;
            } else if (eql(str, "type_parameters")) {
                return .type_parameters;
            } else if (eql(str, "unary_expression")) {
                return .unary_expression;
            } else if (eql(str, "union_item")) {
                return .union_item;
            } else if (eql(str, "unit_expression")) {
                return .unit_expression;
            } else if (eql(str, "unit_type")) {
                return .unit_type;
            } else if (eql(str, "unsafe_block")) {
                return .unsafe_block;
            } else if (eql(str, "use_as_clause")) {
                return .use_as_clause;
            } else if (eql(str, "use_declaration")) {
                return .use_declaration;
            } else if (eql(str, "use_list")) {
                return .use_list;
            } else if (eql(str, "use_wildcard")) {
                return .use_wildcard;
            } else if (eql(str, "variadic_parameter")) {
                return .variadic_parameter;
            } else if (eql(str, "visibility_modifier")) {
                return .visibility_modifier;
            } else if (eql(str, "where_clause")) {
                return .where_clause;
            } else if (eql(str, "where_predicate")) {
                return .where_predicate;
            } else if (eql(str, "while_expression")) {
                return .while_expression;
            } else if (eql(str, "yield_expression")) {
                return .yield_expression;
            }

            return .type_item;
        }
    };

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

    // TODO: pub fn deinit() void {}

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

    pub fn write_to(self: *const Node, parser: *const main.Parser, writer: Writer) void {
        switch (self.node_type) {
            .type_item => {
                self.type_item_str(parser, writer);
            },
            else => |tag| {
                _ = tag; // autofix
                out(writer, "// TODO: {s}", .{"@tagName"});
            },
        }
    }

    fn struct_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable; // $._type_identifier),
        const name = parser.node_to_string(name_field.node);

        // if (self.get_field( "type_parameters")) |field| {
        //     const type_parameters = self.get_text_for(field);

        //     for (type_parameters) |str| {
        //         out(writer, "{s}", .{str});
        //     }
        // }

        out(writer, "const ", .{});
        for (name) |str| {
            out(writer, "{s}", .{str});
        }
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
                    const name = parser.node_to_string(name_field.node);
                    const _type = parser.node_to_string(type_field.node);
                    out(writer, "\t{s}: {s},\n", .{ name[0], _type[0] });
                }
            }
            out(writer, "}};", .{});
        } else {
            out(writer, " = ", .{});
            assert(eql("ordered_field_declaration_list", node_name));
            const _type_field = self.get_field("type") orelse unreachable;
            const _type = parser.node_to_string(_type_field.node);

            // TODO: convert type;
            for (_type) |str| {
                out(writer, "{s}", .{str});
            }
            out(writer, ";", .{});
        }
    }

    fn const_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn macro_definition_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn empty_statement_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn attribute_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        out(writer, "//", .{});
        out("{s}", parser, parser.node_to_string(self.node));
    }
    fn inner_attribute_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn mod_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn foreign_mod_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn union_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn type_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field_unchecked("name"); // $._type_identifier),

        const name = parser.node_to_string(name_field.node);
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
        const name = parser.node_to_string(name_field.node);

        if (self.get_field("alias")) |alias_field| {
            const alias = parser.node_to_string(alias_field.node);
            out(writer, "\nconst ", .{});

            out(writer, "{s}", alias, .{});
            out(writer, " = @import(\"", .{});
            out(writer, "{s}", name, .{});
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
        _ = writer; // autofix
        _ = parser; // autofix
        _ = self; // autofix
    }
    fn call_expression_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void { //TODO: writer

        const function = self.get_field("function") orelse unreachable;
        const args = self.get_field("arguments") orelse unreachable;

        const function_value = parser.node_to_string(function.node);
        const args_value = parser.node_to_string(args.node);

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

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn function_signature_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn impl_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn trait_item_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;
        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn associated_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
            out(
                writer,
                "Return_type:\n\t{s}\n",
                .{return_type},
            );
        }
    }
    fn let_declaration_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
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

        const source = parser.node_to_string(self.node);
        out(writer, "{s}", source, .{});
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
        const type_field = self.get_field("type") orelse unreachable;
        const type_sym = get_type(type_field);

        if (eql(type_sym, "type_identifier")) {
            type_field.write_to(parser, writer);
        } else if (eql(type_sym, "scoped_identifier")) {
            type_field.write_to(parser, writer);
        }

        const type_arguments_field = self.get_field("type_arguments") orelse unreachable;

        const type_arguments = get_children_named(type_arguments_field);

        out(writer, "<", .{});
        for (type_arguments.items) |ty_arg| {
            ty_arg.write_to(parser, writer);
        }
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

        const name = parser.node_to_string(name_field.node);
        _ = name; // autofix

        if (self.get_field("type_parameters")) |type_parameters_field| {
            const type_parameters = parser.node_to_string(type_parameters_field.node);
            out(writer, "Params:\n\t{s}\n", .{type_parameters});
        }

        if (self.get_field("parameters")) |parameters_field| {
            const parameters = parser.node_to_string(parameters_field.node);
            out(
                writer,
                "Parameters:\n\t{s}\n",
                .{parameters},
            );
        }
        if (self.get_field("return_type")) |return_type_field| {
            const return_type = parser.node_to_string(return_type_field.node);
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
    fn metavariable_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = self; // autofix
        _ = parser; // autofix
        out(writer, "TODO:  -> {{{{METAVARIABLE}}}}", .{});
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
        // $._type_identifier,
        // $._reserved_identifier,
        // $.scoped_type_identifier,

        const type_field = self.get_field("type") orelse unreachable;
        const ty_sym_name = get_type(type_field);
        if (eql(ty_sym_name, "type_identifier")) {
            type_field.write_to(parser, writer);
        } else if (eql(ty_sym_name, "reserved_identifier")) {
            type_field.write_to(parser, writer);
        } else if (eql(ty_sym_name, "scoped_type_identifier")) {
            type_field.write_to(parser, writer);
        } else {
            assert(false);
        }

        const type_arguments_field = self.get_field("type_arguments") orelse unreachable;

        const type_arguments = get_children_named(type_arguments_field);

        out(writer, "<", .{});
        for (type_arguments.items) |ty_arg| {
            ty_arg.write_to(parser, writer);
        }
        out(writer, ">", .{});
    }
    fn scoped_type_identifier_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const name_field = self.get_field("name") orelse unreachable;
        const path_field = self.get_field("path") orelse unreachable;

        const source = parser.node_to_string(path_field.node);
        assert(source.len == 1);
        const path = source[0];

        var split = mem.splitSequence(u8, path, "::");

        const buffer = self.allocator.alloc(u8, 4096) catch unreachable; // TODO:
        _ = buffer; // autofix

        var parts = std.ArrayList([]const u8).init(self.allocator);

        while (split.next()) |part| {
            parts.append(part) catch unreachable;
        }

        const joined = std.mem.join(self.allocator, "::", parts.items) catch unreachable;

        const name = parser.node_to_string(name_field.node);
        out(writer, "{s}", .{joined});
        out(writer, ".", .{});
        out(writer, "{s}", name, .{});
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
    fn unit_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        _ = parser; // autofix
        _ = self; // autofix
        out(writer, "void", .{});
    }
    fn array_type_str(self: *const @This(), parser: *const main.Parser, writer: Writer) void {
        const element = self.get_field("element") orelse unreachable;

        if (self.get_field("length")) |length_field| {
            const length_expression = parser.node_to_string(length_field.node);
            out(writer, "[", .{});
            out(writer, "{s}", length_expression, .{});
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
        const source = parser.node_to_string(self.node);
        out(writer, "{s}", source, .{});
    }
};
