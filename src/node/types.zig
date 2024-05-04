const eql = @import("../root.zig").eql;
const eprintln = @import("../root.zig").eprintln;
const eprint = @import("../root.zig").eprint;

const std = @import("std");
const Writer = @TypeOf(std.io.getStdOut().writer());
const assert = std.debug.assert;

const is_empty = @import("../root.zig").is_empty;

pub const IdentifierKind = union {
    named: []const u8,
    matched: Pattern,

    pub const Pattern = struct {
        kind: NodeItem.ItemData.TypeKind,
    };
};

pub const NodeItem = struct {
    // TODO: contents: {}
    name: ?[]const u8,
    path: ?[]const u8,
    data: ItemData,

    pub const ItemData = union(enum) {
        procedure_item: Procedure,
        object_item: Object,
        type_item: TypeItem,
        impl_item: Impl,
        enum_item: Enum,
        module_item: Module,

        pub const Impl = struct {
            procedures: ?[]const Procedure,
        };

        pub const Module = struct {
            contents: ?std.ArrayList(NodeItem), // TODO:
        };

        pub const Enum = struct {
            procedures: ?[]const Procedure,
            variants: []const Variant,

            pub const Variant = struct {
                name: []const u8,
            };
        };

        pub const Procedure = struct {
            params: []const Param,
            return_type: ?TypeKind,

            pub const Param = struct {
                name: IdentifierKind,
                typekind: ?TypeKind,

                const Self = @This();

                pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: Writer) !void {
                    _ = self; // autofix
                    _ = writer; // autofix
                    // FIX:
                    // if (self.typekind) |ty| {
                    //     switch (ty) {
                    //         .tuple => |tuple_types| {
                    //             for (tuple_types.items, 0) |tuple_type, idx| {
                    //                 try std.fmt.format(writer, "{s}: {{{s}}}", .{tuple_type});
                    //                 if (idx != (tuple_types.items.len - 1)) try std.fmt.format(writer, " | ", .{});
                    //             }
                    //         },
                    //         // FIX: unreachable
                    //         else => return std.fmt.format(writer, "{s}: {{{s}}}", .{ self.name, @tagName(ty) }), // FIX:
                    //     }
                    // } else { // FIX: unreachable
                    //     return std.fmt.format(writer, "{s}", .{self.name});
                    // }
                }
            };
        };

        pub const Object = struct {
            pub const Field = struct { name: []const u8, type_kind: TypeKind };
            fields: []Field,
            procedures: ?[]const Procedure,
        };

        pub const TypeKind = union(enum) {
            array: struct { length_expr: ?[]const u8 },
            generic: struct { name: []const u8 },
            identifier: []const u8,
            no_return: void,
            primitive: enum { none }, // FIX:
            proc: struct { params: ?std.ArrayList(Procedure.Param), trait_type: ?enum { once, mut } },
            ref: struct { child: ?*const TypeKind },
            tuple: std.ArrayList(TypeKind),

            const Self = @This();
            pub fn format(
                self: Self,
                comptime _: []const u8,
                _: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                switch (self) {
                    .tuple => |tuple_items| {
                        assert(tuple_items.items.len > 0);
                        try std.fmt.format(writer, "(", .{});
                        for (tuple_items.items, 0..) |ty_kind, idx| {
                            try std.fmt.format(writer, "{s}", .{ty_kind});
                            if (idx != (tuple_items.items.len - 1)) try std.fmt.format(writer, " | ", .{});
                        }
                        try std.fmt.format(writer, ")", .{});
                    },
                    else => return std.fmt.format(writer, "{{{{ {s} }}}}", .{@tagName(self)}),
                }
            }
        };

        pub const TypeItem = struct {
            name: []const u8,
            kind: ?TypeKind, // TODO:

            const Self = @This();
            pub fn format(
                self: Self,
                comptime _: []const u8,
                _: std.fmt.FormatOptions,
                writer: Writer,
            ) !void {
                return std.fmt.format(writer, "{s}", .{self.name});
            }
        };
    };

    pub fn init(data: ItemData, name: ?[]const u8) NodeItem {
        if (name) |str| assert(!is_empty(str));

        return NodeItem{
            .data = data,
            .name = name,
            .path = null, // TODO:

        };
    }

    pub fn serialize(self: *const NodeItem, writer: Writer) !void {
        switch (self.data) {
            .module_item => |mod| {
                assert(self.name != null);
                const name = self.name.?;

                if (mod.contents) |contents| {
                    try std.fmt.format(writer, "const {s} = module {{\n", .{name});
                    for (contents.items) |item| {
                        try item.serialize(writer);
                    }
                    try std.fmt.format(writer, "\n}};", .{});
                } else {
                    try std.fmt.format(writer, "mod {s};", .{name});
                }

                try std.fmt.format(writer, "\n", .{});
            },
            .impl_item => {
                try std.fmt.format(writer, "// TODO: serialize impl_item\n", .{});
            },
            .procedure_item => |data| {
                const name = self.name orelse unreachable;
                // TODO: visibility
                try std.fmt.format(writer, "fn {s}", .{name});
                try std.fmt.format(writer, "(", .{});
                const param_len = data.params.len;
                for (data.params, 0..) |param, idx| {
                    try std.fmt.format(writer, "{}", .{param});
                    if (idx != (param_len - 1)) try std.fmt.format(writer, ",", .{});
                }

                try std.fmt.format(writer, ")", .{});

                if (data.return_type) |return_type| {
                    try std.fmt.format(writer, "{s}", .{return_type});
                } else {
                    try std.fmt.format(writer, "void", .{});
                }

                // self.to_str(writer); // TODO:
                try std.fmt.format(writer, "\n", .{});
            },

            else => |tag| {
                const name = self.name orelse unreachable;
                const item_type = blk: {
                    switch (tag) {
                        ItemData.object_item, ItemData.module_item => break :blk "struct",
                        ItemData.enum_item => break :blk "enum",
                        else => {
                            eprintln("unable to serialize '{s}'", .{@tagName(tag)});
                            unreachable;
                        },
                    }
                };
                try std.fmt.format(writer, "const {s} = {s}", .{ name, item_type });
                try std.fmt.format(writer, "\n", .{});
            },
        }
    }
};

pub const NodeType = enum {
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
    boolean_literal,
    bounded_type,
    bracketed_type,
    break_expression,
    call_expression,
    captured_pattern,
    char_literal,
    closure_expression,
    closure_parameters,
    comment,
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
    escape_sequence,
    expression_statement,
    extern_crate_declaration,
    extern_modifier,
    field_declaration,
    field_declaration_list,
    field_expression,
    field_identifier,
    field_initializer,
    field_initializer_list,
    field_pattern,
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
    mut_pattern,
    negative_literal,
    never_type,
    optional_type_parameter,
    ordered_field_declaration_list,
    or_pattern,
    parameter,
    parameters,
    parenthesized_expression,
    pointer_type,
    primitive_type,
    qualified_type,
    range_expression,
    range_pattern,
    raw_string_literal,
    reference_expression,
    reference_pattern,
    reference_type,
    ref_pattern,
    remaining_field_pattern,
    removed_trait_bound,
    return_expression,
    scoped_identifier,
    scoped_type_identifier,
    scoped_type_identifier_in_expression_position,
    scoped_use_list,
    self,
    self_parameter,
    shorthand_field_identifier,
    shorthand_field_initializer,
    slice_pattern,
    source_file,
    static_item,
    string_literal,
    struct_expression,
    struct_item,
    struct_pattern,
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
    type_identifier,
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
        if (eql(str, "abstract_type")) return .abstract_type;
        if (eql(str, "arguments")) return .arguments;
        if (eql(str, "array_expression")) return .array_expression;
        if (eql(str, "array_type")) return .array_type;
        if (eql(str, "assignment_expression")) return .assignment_expression;
        if (eql(str, "associated_type")) return .associated_type;
        if (eql(str, "async_block")) return .async_block;
        if (eql(str, "attribute_item")) return .attribute_item;
        if (eql(str, "attribute")) return .attribute;
        if (eql(str, "await_expression")) return .await_expression;
        if (eql(str, "base_field_initializer")) return .base_field_initializer;
        if (eql(str, "binary_expression")) return .binary_expression;
        if (eql(str, "block")) return .block;
        if (eql(str, "boolean_literal")) return .boolean_literal;
        if (eql(str, "bounded_type")) return .bounded_type;
        if (eql(str, "bracketed_type")) return .bracketed_type;
        if (eql(str, "break_expression")) return .break_expression;
        if (eql(str, "call_expression")) return .call_expression;
        if (eql(str, "captured_pattern")) return .captured_pattern;
        if (eql(str, "char_literal")) return .char_literal;
        if (eql(str, "closure_expression")) return .closure_expression;
        if (eql(str, "closure_parameters")) return .closure_parameters;
        if (eql(str, "comment")) return .comment;
        if (eql(str, "compound_assignment_expr")) return .compound_assignment_expr;
        if (eql(str, "const_block")) return .const_block;
        if (eql(str, "const_item")) return .const_item;
        if (eql(str, "const_parameter")) return .const_parameter;
        if (eql(str, "constrained_type_parameter")) return .constrained_type_parameter;
        if (eql(str, "continue_expression")) return .continue_expression;
        if (eql(str, "crate")) return .crate;
        if (eql(str, "declaration_list")) return .declaration_list;
        if (eql(str, "delim_token_tree")) return .delim_token_tree;
        if (eql(str, "dynamic_type")) return .dynamic_type;
        if (eql(str, "else_clause")) return .else_clause;
        if (eql(str, "empty_statement")) return .empty_statement;
        if (eql(str, "enum_item")) return .enum_item;
        if (eql(str, "enum_variant_list")) return .enum_variant_list;
        if (eql(str, "enum_variant")) return .enum_variant;
        if (eql(str, "escape_sequence")) return .escape_sequence;
        if (eql(str, "expression_statement")) return .expression_statement;
        if (eql(str, "extern_crate_declaration")) return .extern_crate_declaration;
        if (eql(str, "extern_modifier")) return .extern_modifier;
        if (eql(str, "field_declaration_list")) return .field_declaration_list;
        if (eql(str, "field_declaration")) return .field_declaration;
        if (eql(str, "field_expression")) return .field_expression;
        if (eql(str, "field_identifier")) return .field_identifier;
        if (eql(str, "field_initializer_list")) return .field_initializer_list;
        if (eql(str, "field_initializer")) return .field_initializer;
        if (eql(str, "field_pattern")) return .field_pattern;
        if (eql(str, "foreign_mod_item")) return .foreign_mod_item;
        if (eql(str, "for_expression")) return .for_expression;
        if (eql(str, "for_lifetimes")) return .for_lifetimes;
        if (eql(str, "fragment_specifier")) return .fragment_specifier;
        if (eql(str, "function_item")) return .function_item;
        if (eql(str, "function_modifiers")) return .function_modifiers;
        if (eql(str, "function_signature_item")) return .function_signature_item;
        if (eql(str, "function_type")) return .function_type;
        if (eql(str, "generic_function")) return .generic_function;
        if (eql(str, "generic_type")) return .generic_type;
        if (eql(str, "generic_type_with_turbofish")) return .generic_type_with_turbofish;
        if (eql(str, "higher_ranked_trait_bound")) return .higher_ranked_trait_bound;
        if (eql(str, "identifier")) return .identifier;
        if (eql(str, "if_expression")) return .if_expression;
        if (eql(str, "impl_item")) return .impl_item;
        if (eql(str, "index_expression")) return .index_expression;
        if (eql(str, "inner_attribute_item")) return .inner_attribute_item;
        if (eql(str, "integer_literal")) return .integer_literal;
        if (eql(str, "label")) return .label;
        if (eql(str, "last_match_arm")) return .last_match_arm;
        if (eql(str, "let_condition")) return .let_condition;
        if (eql(str, "let_declaration")) return .let_declaration;
        if (eql(str, "lifetime")) return .lifetime;
        if (eql(str, "line_comment")) return .line_comment;
        if (eql(str, "loop_expression")) return .loop_expression;
        if (eql(str, "macro_definition")) return .macro_definition;
        if (eql(str, "macro_invocation")) return .macro_invocation;
        if (eql(str, "macro_rule")) return .macro_rule;
        if (eql(str, "match_arm")) return .match_arm;
        if (eql(str, "match_block")) return .match_block;
        if (eql(str, "match_expression")) return .match_expression;
        if (eql(str, "match_pattern")) return .match_pattern;
        if (eql(str, "metavariable")) return .metavariable;
        if (eql(str, "mod_item")) return .mod_item;
        if (eql(str, "mutable_specifier")) return .mutable_specifier;
        if (eql(str, "mut_pattern")) return .mut_pattern;
        if (eql(str, "negative_literal")) return .negative_literal;
        if (eql(str, "never_type")) return .never_type;
        if (eql(str, "optional_type_parameter")) return .optional_type_parameter;
        if (eql(str, "ordered_field_declaration_list")) return .ordered_field_declaration_list;
        if (eql(str, "or_pattern")) return .or_pattern;
        if (eql(str, "parameter")) return .parameter;
        if (eql(str, "parameters")) return .parameters;
        if (eql(str, "parenthesized_expression")) return .parenthesized_expression;
        if (eql(str, "pointer_type")) return .pointer_type;
        if (eql(str, "primitive_type")) return .primitive_type;
        if (eql(str, "qualified_type")) return .qualified_type;
        if (eql(str, "range_expression")) return .range_expression;
        if (eql(str, "range_pattern")) return .range_pattern;
        if (eql(str, "raw_string_literal")) return .raw_string_literal;
        if (eql(str, "reference_expression")) return .reference_expression;
        if (eql(str, "reference_pattern")) return .reference_pattern;
        if (eql(str, "reference_type")) return .reference_type;
        if (eql(str, "ref_pattern")) return .ref_pattern;
        if (eql(str, "remaining_field_pattern")) return .remaining_field_pattern;
        if (eql(str, "removed_trait_bound")) return .removed_trait_bound;
        if (eql(str, "return_expression")) return .return_expression;
        if (eql(str, "scoped_identifier")) return .scoped_identifier;
        if (eql(str, "scoped_type_identifier_in_expression_position")) return .scoped_type_identifier_in_expression_position;
        if (eql(str, "scoped_type_identifier")) return .scoped_type_identifier;
        if (eql(str, "scoped_use_list")) return .scoped_use_list;
        if (eql(str, "self_parameter")) return .self_parameter;
        if (eql(str, "self")) return .self;
        if (eql(str, "shorthand_field_identifier")) return .shorthand_field_identifier;
        if (eql(str, "shorthand_field_initializer")) return .shorthand_field_initializer;
        if (eql(str, "slice_pattern")) return .slice_pattern;
        if (eql(str, "source_file")) return .source_file;
        if (eql(str, "static_item")) return .static_item;
        if (eql(str, "string_literal")) return .string_literal;
        if (eql(str, "struct_expression")) return .struct_expression;
        if (eql(str, "struct_item")) return .struct_item;
        if (eql(str, "struct_pattern")) return .struct_pattern;
        if (eql(str, "super")) return .super;
        if (eql(str, "token_binding_pattern")) return .token_binding_pattern;
        if (eql(str, "token_repetition_pattern")) return .token_repetition_pattern;
        if (eql(str, "token_repetition")) return .token_repetition;
        if (eql(str, "token_tree_pattern")) return .token_tree_pattern;
        if (eql(str, "token_tree")) return .token_tree;
        if (eql(str, "trait_bounds")) return .trait_bounds;
        if (eql(str, "trait_item")) return .trait_item;
        if (eql(str, "try_block")) return .try_block;
        if (eql(str, "try_expression")) return .try_expression;
        if (eql(str, "tuple_expression")) return .tuple_expression;
        if (eql(str, "tuple_pattern")) return .tuple_pattern;
        if (eql(str, "tuple_struct_pattern")) return .tuple_struct_pattern;
        if (eql(str, "tuple_type")) return .tuple_type;
        if (eql(str, "type_arguments")) return .type_arguments;
        if (eql(str, "type_binding")) return .type_binding;
        if (eql(str, "type_cast_expression")) return .type_cast_expression;
        if (eql(str, "type_identifier")) return .type_identifier;
        if (eql(str, "type_item")) return .type_item;
        if (eql(str, "type_parameters")) return .type_parameters;
        if (eql(str, "unary_expression")) return .unary_expression;
        if (eql(str, "union_item")) return .union_item;
        if (eql(str, "unit_expression")) return .unit_expression;
        if (eql(str, "unit_type")) return .unit_type;
        if (eql(str, "unsafe_block")) return .unsafe_block;
        if (eql(str, "use_as_clause")) return .use_as_clause;
        if (eql(str, "use_declaration")) return .use_declaration;
        if (eql(str, "use_list")) return .use_list;
        if (eql(str, "use_wildcard")) return .use_wildcard;
        if (eql(str, "variadic_parameter")) return .variadic_parameter;
        if (eql(str, "visibility_modifier")) return .visibility_modifier;
        if (eql(str, "where_clause")) return .where_clause;
        if (eql(str, "where_predicate")) return .where_predicate;
        if (eql(str, "while_expression")) return .while_expression;
        if (eql(str, "yield_expression")) return .yield_expression;

        eprintln("Invalid Type '{s}'", .{str});
        unreachable;
    }
};
