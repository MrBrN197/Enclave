const eprint = @import("../root.zig").eprint;
const eprintln = @import("../root.zig").eprintln;
const std = @import("std");
const str = @import("../str.zig");

const assert = std.debug.assert;
const fmt = std.fmt;
const mem = std.mem;

pub const NodeItem = @import("./items/item.zig").NodeItem;
pub const ImportPath = @import("./path.zig").ImportPath;
pub const IdentifierKind = @import("./items/item.zig").IdentifierKind;
const procedure = @import("./items/procedure.zig");

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
    block_comment,
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

    pub fn from_string(string: []const u8) NodeType {
        if (str.eql(string, "abstract_type")) return .abstract_type;
        if (str.eql(string, "arguments")) return .arguments;
        if (str.eql(string, "array_expression")) return .array_expression;
        if (str.eql(string, "array_type")) return .array_type;
        if (str.eql(string, "assignment_expression")) return .assignment_expression;
        if (str.eql(string, "associated_type")) return .associated_type;
        if (str.eql(string, "async_block")) return .async_block;
        if (str.eql(string, "attribute_item")) return .attribute_item;
        if (str.eql(string, "attribute")) return .attribute;
        if (str.eql(string, "await_expression")) return .await_expression;
        if (str.eql(string, "base_field_initializer")) return .base_field_initializer;
        if (str.eql(string, "binary_expression")) return .binary_expression;
        if (str.eql(string, "block_comment")) return .block_comment;
        if (str.eql(string, "block")) return .block;
        if (str.eql(string, "boolean_literal")) return .boolean_literal;
        if (str.eql(string, "bounded_type")) return .bounded_type;
        if (str.eql(string, "bracketed_type")) return .bracketed_type;
        if (str.eql(string, "break_expression")) return .break_expression;
        if (str.eql(string, "call_expression")) return .call_expression;
        if (str.eql(string, "captured_pattern")) return .captured_pattern;
        if (str.eql(string, "char_literal")) return .char_literal;
        if (str.eql(string, "closure_expression")) return .closure_expression;
        if (str.eql(string, "closure_parameters")) return .closure_parameters;
        if (str.eql(string, "comment")) return .comment;
        if (str.eql(string, "compound_assignment_expr")) return .compound_assignment_expr;
        if (str.eql(string, "const_block")) return .const_block;
        if (str.eql(string, "const_item")) return .const_item;
        if (str.eql(string, "const_parameter")) return .const_parameter;
        if (str.eql(string, "constrained_type_parameter")) return .constrained_type_parameter;
        if (str.eql(string, "continue_expression")) return .continue_expression;
        if (str.eql(string, "crate")) return .crate;
        if (str.eql(string, "declaration_list")) return .declaration_list;
        if (str.eql(string, "delim_token_tree")) return .delim_token_tree;
        if (str.eql(string, "dynamic_type")) return .dynamic_type;
        if (str.eql(string, "else_clause")) return .else_clause;
        if (str.eql(string, "empty_statement")) return .empty_statement;
        if (str.eql(string, "enum_item")) return .enum_item;
        if (str.eql(string, "enum_variant_list")) return .enum_variant_list;
        if (str.eql(string, "enum_variant")) return .enum_variant;
        if (str.eql(string, "escape_sequence")) return .escape_sequence;
        if (str.eql(string, "expression_statement")) return .expression_statement;
        if (str.eql(string, "extern_crate_declaration")) return .extern_crate_declaration;
        if (str.eql(string, "extern_modifier")) return .extern_modifier;
        if (str.eql(string, "field_declaration_list")) return .field_declaration_list;
        if (str.eql(string, "field_declaration")) return .field_declaration;
        if (str.eql(string, "field_expression")) return .field_expression;
        if (str.eql(string, "field_identifier")) return .field_identifier;
        if (str.eql(string, "field_initializer_list")) return .field_initializer_list;
        if (str.eql(string, "field_initializer")) return .field_initializer;
        if (str.eql(string, "field_pattern")) return .field_pattern;
        if (str.eql(string, "foreign_mod_item")) return .foreign_mod_item;
        if (str.eql(string, "for_expression")) return .for_expression;
        if (str.eql(string, "for_lifetimes")) return .for_lifetimes;
        if (str.eql(string, "fragment_specifier")) return .fragment_specifier;
        if (str.eql(string, "function_item")) return .function_item;
        if (str.eql(string, "function_modifiers")) return .function_modifiers;
        if (str.eql(string, "function_signature_item")) return .function_signature_item;
        if (str.eql(string, "function_type")) return .function_type;
        if (str.eql(string, "generic_function")) return .generic_function;
        if (str.eql(string, "generic_type")) return .generic_type;
        if (str.eql(string, "generic_type_with_turbofish")) return .generic_type_with_turbofish;
        if (str.eql(string, "higher_ranked_trait_bound")) return .higher_ranked_trait_bound;
        if (str.eql(string, "identifier")) return .identifier;
        if (str.eql(string, "if_expression")) return .if_expression;
        if (str.eql(string, "impl_item")) return .impl_item;
        if (str.eql(string, "index_expression")) return .index_expression;
        if (str.eql(string, "inner_attribute_item")) return .inner_attribute_item;
        if (str.eql(string, "integer_literal")) return .integer_literal;
        if (str.eql(string, "label")) return .label;
        if (str.eql(string, "last_match_arm")) return .last_match_arm;
        if (str.eql(string, "let_condition")) return .let_condition;
        if (str.eql(string, "let_declaration")) return .let_declaration;
        if (str.eql(string, "lifetime")) return .lifetime;
        if (str.eql(string, "line_comment")) return .line_comment;
        if (str.eql(string, "loop_expression")) return .loop_expression;
        if (str.eql(string, "macro_definition")) return .macro_definition;
        if (str.eql(string, "macro_invocation")) return .macro_invocation;
        if (str.eql(string, "macro_rule")) return .macro_rule;
        if (str.eql(string, "match_arm")) return .match_arm;
        if (str.eql(string, "match_block")) return .match_block;
        if (str.eql(string, "match_expression")) return .match_expression;
        if (str.eql(string, "match_pattern")) return .match_pattern;
        if (str.eql(string, "metavariable")) return .metavariable;
        if (str.eql(string, "mod_item")) return .mod_item;
        if (str.eql(string, "mutable_specifier")) return .mutable_specifier;
        if (str.eql(string, "mut_pattern")) return .mut_pattern;
        if (str.eql(string, "negative_literal")) return .negative_literal;
        if (str.eql(string, "never_type")) return .never_type;
        if (str.eql(string, "optional_type_parameter")) return .optional_type_parameter;
        if (str.eql(string, "ordered_field_declaration_list")) return .ordered_field_declaration_list;
        if (str.eql(string, "or_pattern")) return .or_pattern;
        if (str.eql(string, "parameter")) return .parameter;
        if (str.eql(string, "parameters")) return .parameters;
        if (str.eql(string, "parenthesized_expression")) return .parenthesized_expression;
        if (str.eql(string, "pointer_type")) return .pointer_type;
        if (str.eql(string, "primitive_type")) return .primitive_type;
        if (str.eql(string, "qualified_type")) return .qualified_type;
        if (str.eql(string, "range_expression")) return .range_expression;
        if (str.eql(string, "range_pattern")) return .range_pattern;
        if (str.eql(string, "raw_string_literal")) return .raw_string_literal;
        if (str.eql(string, "reference_expression")) return .reference_expression;
        if (str.eql(string, "reference_pattern")) return .reference_pattern;
        if (str.eql(string, "reference_type")) return .reference_type;
        if (str.eql(string, "ref_pattern")) return .ref_pattern;
        if (str.eql(string, "remaining_field_pattern")) return .remaining_field_pattern;
        if (str.eql(string, "removed_trait_bound")) return .removed_trait_bound;
        if (str.eql(string, "return_expression")) return .return_expression;
        if (str.eql(string, "scoped_identifier")) return .scoped_identifier;
        if (str.eql(string, "scoped_type_identifier_in_expression_position")) return .scoped_type_identifier_in_expression_position;
        if (str.eql(string, "scoped_type_identifier")) return .scoped_type_identifier;
        if (str.eql(string, "scoped_use_list")) return .scoped_use_list;
        if (str.eql(string, "self_parameter")) return .self_parameter;
        if (str.eql(string, "self")) return .self;
        if (str.eql(string, "shorthand_field_identifier")) return .shorthand_field_identifier;
        if (str.eql(string, "shorthand_field_initializer")) return .shorthand_field_initializer;
        if (str.eql(string, "slice_pattern")) return .slice_pattern;
        if (str.eql(string, "source_file")) return .source_file;
        if (str.eql(string, "static_item")) return .static_item;
        if (str.eql(string, "string_literal")) return .string_literal;
        if (str.eql(string, "struct_expression")) return .struct_expression;
        if (str.eql(string, "struct_item")) return .struct_item;
        if (str.eql(string, "struct_pattern")) return .struct_pattern;
        if (str.eql(string, "super")) return .super;
        if (str.eql(string, "token_binding_pattern")) return .token_binding_pattern;
        if (str.eql(string, "token_repetition_pattern")) return .token_repetition_pattern;
        if (str.eql(string, "token_repetition")) return .token_repetition;
        if (str.eql(string, "token_tree_pattern")) return .token_tree_pattern;
        if (str.eql(string, "token_tree")) return .token_tree;
        if (str.eql(string, "trait_bounds")) return .trait_bounds;
        if (str.eql(string, "trait_item")) return .trait_item;
        if (str.eql(string, "try_block")) return .try_block;
        if (str.eql(string, "try_expression")) return .try_expression;
        if (str.eql(string, "tuple_expression")) return .tuple_expression;
        if (str.eql(string, "tuple_pattern")) return .tuple_pattern;
        if (str.eql(string, "tuple_struct_pattern")) return .tuple_struct_pattern;
        if (str.eql(string, "tuple_type")) return .tuple_type;
        if (str.eql(string, "type_arguments")) return .type_arguments;
        if (str.eql(string, "type_binding")) return .type_binding;
        if (str.eql(string, "type_cast_expression")) return .type_cast_expression;
        if (str.eql(string, "type_identifier")) return .type_identifier;
        if (str.eql(string, "type_item")) return .type_item;
        if (str.eql(string, "type_parameters")) return .type_parameters;
        if (str.eql(string, "unary_expression")) return .unary_expression;
        if (str.eql(string, "union_item")) return .union_item;
        if (str.eql(string, "unit_expression")) return .unit_expression;
        if (str.eql(string, "unit_type")) return .unit_type;
        if (str.eql(string, "unsafe_block")) return .unsafe_block;
        if (str.eql(string, "use_as_clause")) return .use_as_clause;
        if (str.eql(string, "use_declaration")) return .use_declaration;
        if (str.eql(string, "use_list")) return .use_list;
        if (str.eql(string, "use_wildcard")) return .use_wildcard;
        if (str.eql(string, "variadic_parameter")) return .variadic_parameter;
        if (str.eql(string, "visibility_modifier")) return .visibility_modifier;
        if (str.eql(string, "where_clause")) return .where_clause;
        if (str.eql(string, "where_predicate")) return .where_predicate;
        if (str.eql(string, "while_expression")) return .while_expression;
        if (str.eql(string, "yield_expression")) return .yield_expression;

        eprintln("Invalid Type '{s}'", .{string});

        unreachable;
    }
};

pub const TypeKind = union(enum) {
    array: struct { length_expr: ?[]const u8, child: *const TypeKind },
    dynamic,
    generic: struct {
        name: IdentifierKind,
    },
    identifier: IdentifierKind,
    none,
    no_return: void,
    primitive: enum { u16, u32, u64, u8, u128, usize, i16, i32, i64, i8, i128, isize, f32, f64, char, str, bool }, // FIX:
    proc: struct {
        params: ?std.ArrayList(procedure.Param),
        return_type: ?*const TypeKind,
    },
    ref: struct { child: *const TypeKind }, // TODO: is_mut: bool
    tuple: std.ArrayList(TypeKind),
    self,

    pub fn format(
        self: TypeKind,
        comptime _: []const u8,
        _: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .identifier => |id| try fmt.format(writer, "{s}", .{id}),
            .tuple => |tuple_items| {
                assert(tuple_items.items.len > 0);
                try fmt.format(writer, "struct {{", .{});
                for (tuple_items.items, 0..) |ty_kind, idx| {
                    try fmt.format(writer, "{s}", .{ty_kind});
                    if (idx != (tuple_items.items.len - 1)) try fmt.format(writer, ", ", .{});
                }
                try fmt.format(writer, "}} ", .{});
            },
            .primitive => |prim| {
                switch (prim) {
                    .char => try fmt.format(writer, "u8", .{}),
                    .str => try fmt.format(writer, "[]const u8", .{}),
                    else => try fmt.format(writer, "{s}", .{@tagName(prim)}),
                }
            },
            .array => |array| {
                const brackets = array.length_expr orelse "[]";

                const typename = array.child;

                try fmt.format(writer, "{s}{}", .{ brackets, typename.* });
            },
            .ref => |ref| {
                try fmt.format(writer, "*const {}", .{ref.child});
            },

            .generic => |generic| try fmt.format(writer, "{s}", .{generic.name}),
            .none => try fmt.format(writer, "void", .{}),
            .no_return => @panic("todo"),
            .proc => |proc| {
                try fmt.format(writer, "fn(", .{});
                const args = proc.params.?;
                for (args.items) |buf| try fmt.format(writer, "{},", .{buf});
                try fmt.format(writer, ") ", .{});
                if (proc.return_type) |rt| {
                    try fmt.format(writer, "{}", .{rt});
                } else {
                    try fmt.format(writer, "void", .{});
                }
            },
            .self => try fmt.format(writer, "Self", .{}),
            .dynamic => @panic("todo:"),
        }
    }
};
