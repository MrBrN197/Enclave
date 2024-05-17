const assert = std.debug.assert;
const fmt = std.fmt;
const mem = std.mem;
const path = @import("../path.zig");
const procedure = @import("./procedure.zig");
const std = @import("std");
const str = @import("../../str.zig");
const log = std.log;

const Buf = path.Buf;
const ImportPath = path.ImportPath;
pub const Enum = @import("./object.zig").Enum;
pub const Import = @import("./import.zig").Import;
pub const Module = @import("./module.zig").Module;
pub const Object = @import("./object.zig").Object;
pub const Procedure = @import("./procedure.zig").Procedure;
pub const TypeKind = @import("../types.zig").TypeKind;
pub const FnSignature = @import("./fn.zig").FnSignature;
pub const Trait = interface.Trait;

pub const interface = @import("./interface.zig");

pub const TypeParam = struct {
    type: TypeKind,
    constraints: ?std.ArrayList(TypeBound),

    pub const TypeBound = union(enum) {
        type: TypeKind,
        constant: PrimitiveKind,
    };

    pub fn format(self: *const @This(), comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        _ = self; // autofix
        _ = writer; // autofix
    }
};

pub const Impl = struct {
    body: ?Module,
    generics: ?std.ArrayList(TypeParam),
    implementor: TypeKind,
    for_interface: ?IdentifierKind,
};

pub const MatchedIdentifier = union(enum) {
    field_pattern: []const u8,
    tuple_pattern: []const u8,
    reference_pattern: []const u8,

    pub fn format(self: *const @This(), comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            .field_pattern => |text| {
                try fmt.format(writer, "{s}", .{text});
            },
            else => @panic("todo:"),
        }
    }
};

pub const PrimitiveKind = enum { u16, u32, u64, u8, u128, usize, i16, i32, i64, i8, i128, isize, f32, f64, char, str, bool };

pub const IdentifierKind = union(enum) {
    discarded, // FIX: '_'
    generic: struct { name: []const u8 },
    matched: MatchedIdentifier,
    scoped: Buf,
    primitive: PrimitiveKind, // FIX:

    self,
    text: []const u8, // TODO: remove

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .discarded => try fmt.format(writer, "_", .{}),
            .generic => |generic| try fmt.format(writer, "{s}", .{generic.name}),
            .matched => |matched| try fmt.format(writer, "{}", .{matched}),
            .primitive => |prim| {
                switch (prim) {
                    .char => try fmt.format(writer, "u8", .{}),
                    .str => try fmt.format(writer, "[]const u8", .{}),
                    else => try fmt.format(writer, "{s}", .{@tagName(prim)}),
                }
            },
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

pub const SerializeContext = struct {
    items: std.StringHashMap(NodeItem),
    impls: []const Impl,

    pub fn getImpls(
        self: *const @This(),
        object_name: []const u8,
        collect: *std.ArrayList(*const Impl),
    ) void {
        for (self.impls) |*item| {
            if (item.for_interface) |_| {
                if (str.eql(item.implementor.identifier.text, object_name)) {
                    collect.append(item) catch unreachable;
                }
            }
        }
    }

    const Template = struct { name: []const u8 };

    const FnSignatureItem = struct {
        data: *const FnSignature,
        name: []const u8,
    };
    const Interface = struct {
        procedures: std.ArrayList(FnSignatureItem),
    };

    pub fn getInterface(
        self: *const @This(),
        impl_name: []const u8,
    ) !Interface {
        var it = self.items.iterator();

        while (it.next()) |value| {
            const item = value.value_ptr;

            switch (item.data) {
                .trait_item => |*iface| {
                    var procedures = std.ArrayList(FnSignatureItem).init(std.heap.page_allocator);
                    for (iface.body.node_items.items) |*i| {
                        switch (i.data) {
                            .function_signature_item => |*proc| {
                                procedures.append(.{
                                    .data = proc,
                                    .name = i.name.?,
                                }) catch unreachable;
                            },
                            else => {},
                        }
                    }

                    if (str.eql(item.name.?, impl_name)) {
                        const result = Interface{
                            .procedures = procedures,
                        };
                        return result;
                    }
                },
                else => {},
            }
        }
        return error.MissingInterface;
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
        ctx: SerializeContext,
    ) @TypeOf(writer).Error!void {
        switch (self.data) {
            .const_item => |cnst| try cnst.serialize(writer, self.name.?),
            .enum_item => |enm| try enm.serialize(writer, self.name.?),
            .function_signature_item => |fntype| try fntype.serialize(writer, self.name.?),
            .import_item => |import| try import.serialize(writer),
            .module_item => |mod| try mod.serialize(writer, self.name.?, ctx),
            .object_item => |obj| try obj.serialize(writer, self.name.?, ctx),
            .procedure_item => |proc| try proc.serialize(writer, self.name.?),
            .type_item => |typ| try typ.serialize(writer, self.name.?),
            .trait_item => |trait| try trait.serialize(writer, self.name.?, ctx),
            else => unreachable,
        }
    }
};
