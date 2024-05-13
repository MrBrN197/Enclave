const assert = std.debug.assert;
const eprintln = root.eprintln;
const fmt = std.fmt;
const mem = std.mem;
const path = @import("./path.zig");
const procedure = @import("./items/procedure.zig");
const root = @import("../root.zig");
const std = @import("std");
const str = @import("../str.zig");

const Buf = path.Buf;
const ImportPath = path.ImportPath;
pub const Enum = @import("./items/object.zig").Enum;
pub const Import = @import("./items/import.zig").Import;
pub const Module = @import("./items/module.zig").Module;
pub const Object = @import("./items/object.zig").Object;
pub const Procedure = @import("./items/procedure.zig").Procedure;
pub const TypeKind = @import("./types.zig").TypeKind;

pub const IdentifierKind = union(enum) {
    discarded, // FIX: '_'
    matched: []const u8, // TODO:
    scoped: Buf,
    self,
    text: []const u8, // TODO: remove

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .discarded => try fmt.format(writer, "_", .{}),
            .matched => |matched| try fmt.format(writer, "{s}", .{matched}),
            .scoped => |pathbuf| try fmt.format(writer, "{s}", .{pathbuf.str()}),
            .self => try fmt.format(writer, "self", .{}),
            .text => |name| return fmt.format(writer, "{s}", .{name}),
        }
    }
};

pub const TypeItem = struct {
    kind: TypeKind,

    pub fn serialize(self: *const @This(), writer: anytype, name: []const u8) !void {
        try fmt.format(writer, "//type\nconst {s} = {};\n", .{ name, self.kind });
    }
};

pub const NodeItem = struct {
    // TODO: visibility,

    annotations: ?std.ArrayList([]const u8),
    data: Data,
    name: ?[]const u8,

    pub const Data = union(enum) {
        const_item: Constant,
        enum_item: Enum,
        function_signature_item: FnSignature,
        impl_item: Impl,
        import_item: Import,
        module_item: Module,
        object_item: Object,
        procedure_item: Procedure,
        trait_item: Trait,
        type_item: TypeItem,

        const Trait = struct {
            body: Module,
            bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
            generics: ?std.ArrayList(TypeKind),

            pub fn serialize(self: *const @This(), writer: anytype, name: []const u8) @TypeOf(writer).Error!void {
                try fmt.format(writer, "// {s} Interface\n", .{name});
                try fmt.format(writer, "const {s} = struct {{\n", .{name});
                try fmt.format(writer, "const Self = @This();\n", .{});
                for (self.body.node_items.items) |item| {
                    try item.serialize(writer); //FIX
                    @panic("fix: inferred error");
                }

                try fmt.format(writer, "}};\n", .{});
            }

            pub fn format(self: @This(), comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) anyerror!void {
                _ = writer; // autofix
                _ = self; // autofix
            }
        };

        pub const FnSignature = struct {
            bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
            generics: ?std.ArrayList(TypeKind),
            params: std.ArrayList(procedure.Param),
            return_type: ?TypeKind,

            pub fn serialize(self: *const @This(), writer: anytype, name: []const u8) !void {
                // TODO: bounds

                try fmt.format(writer, "{s}Fn: *const fn(", .{name});
                for (self.params.items) |param| try fmt.format(writer, "{},", .{param.typekind.?});
                try fmt.format(writer, ") ", .{});
                if (self.return_type) |rt| {
                    try fmt.format(writer, "{}", .{rt});
                } else {
                    try fmt.format(writer, "void", .{});
                }
                try fmt.format(writer, ",\n", .{});
            }
        };

        pub const Constant = struct {
            type_kind: TypeKind,
            value_expr: ?[]const u8,

            pub fn serialize(self: *const @This(), writer: anytype, name: []const u8) @TypeOf(writer).Error!void {
                assert(self.value_expr != null);

                const val = self.value_expr.?;

                try fmt.format(writer, "const {s}: {} = undefined; // FIX: \n// {s};", .{
                    name,
                    self.type_kind,
                    val,
                });
                try fmt.format(writer, "\n", .{});
            }
        };

        pub const Impl = struct {
            body: ?Module,
            bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
            generics: ?std.ArrayList(TypeKind),
            implementor: IdentifierKind,
            for_interface: ?IdentifierKind,

            pub fn serialize(self: *const Impl, writer: anytype) !void {
                _ = self; // autofix
                _ = writer; // autofix
                @panic("todo:impl");
            }
        };
    };

    pub fn init(data: Data, name: ?[]const u8) NodeItem {
        if (name) |s| assert(!str.isEmpty(s));

        return NodeItem{
            .data = data,
            .name = name,
            .annotations = null, // TODO:
        };
    }

    pub fn initAlloc(allocator: std.mem.Allocator, data: Data, name: ?[]const u8) std.mem.Allocator.Error!*NodeItem {
        const node = NodeItem.init(data, name);

        const ptr = try allocator.create(NodeItem);
        ptr.* = node;
        return ptr;
    }

    pub fn serialize(
        self: *const @This(),
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self.data) {
            .const_item => |cnst| try cnst.serialize(writer, self.name.?),
            .enum_item => |enm| try enm.serialize(writer, self.name.?),
            .function_signature_item => |fntype| try fntype.serialize(writer, self.name.?),
            .impl_item => |impl| try impl.serialize(writer),
            .import_item => |import| try import.serialize(writer),
            .module_item => |mod| try mod.serialize(writer, self.name.?),
            .object_item => |obj| try obj.serialize(writer, self.name.?),
            .procedure_item => |proc| try proc.serialize(writer, self.name.?),
            .trait_item => |trait| try trait.serialize(writer, self.name.?),
            .type_item => |typ| try typ.serialize(writer, self.name.?),
        }
    }
};
