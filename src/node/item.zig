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
        trait_item: struct {
            body: Module,
            bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
            generics: ?std.ArrayList(TypeKind),
        },
        type_item: TypeItem,

        pub const FnSignature = struct {
            bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
            generics: ?std.ArrayList(TypeKind),
            params: std.ArrayList(procedure.Param),
            return_type: ?TypeKind,
        };

        pub const Constant = struct {
            type_kind: TypeKind,
            value_expr: ?[]const u8,
        };

        pub const Impl = struct {
            body: ?Module,
            bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
            generics: ?std.ArrayList(TypeKind),
            implementor: IdentifierKind,
            for_interface: ?IdentifierKind,
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

    pub fn serialize(self: *const NodeItem, writer: anytype) !void {
        switch (self.data) {
            .import_item => |import| {
                for (import.import_paths.items) |p| {
                    const basename = ImportPath.basename(p.str());
                    if (str.eql(basename, "*")) {
                        std.log.warn("todo: path {s}", .{p.str()});
                        continue;
                    }
                    try fmt.format(writer, "const {s} = {s};\n", .{ basename, p.str() });
                }
            },
            .module_item => |mod| {
                assert(self.name != null);

                const name = self.name.?;

                if (mod.items.items.len > 0) {
                    try fmt.format(writer, "const {s} = struct {{ // TODO: Module  \n", .{name}); // FIX:
                    for (mod.items.items) |item| try item.serialize(writer);
                    try fmt.format(writer, "\n}};", .{});
                } else {
                    try fmt.format(writer, "const {s} = @import(\"{s}\");", .{ name, name });
                }

                try fmt.format(writer, "\n", .{});
            },
            .impl_item => |_| @panic("todo"),
            .const_item => |item_data| {
                assert(item_data.value_expr != null);

                const val = item_data.value_expr.?;

                try fmt.format(writer, "const {s}: {} = undefined; // FIX: \n// {s};", .{
                    self.name.?,
                    item_data.type_kind,
                    val,
                });
                try fmt.format(writer, "\n", .{});
            },
            .procedure_item => |data| {
                const name = self.name.?;
                // TODO: visibility

                if (data.generics) |generics| {
                    for (generics.items) |generic| {
                        try fmt.format(writer, "// ", .{});
                        assert(generic == .identifier);
                        try fmt.format(writer, "{s}: ", .{generic});

                        if (data.get_bounds(generic.identifier.text)) |bounds|
                            for (bounds) |bound| try fmt.format(writer, "{}, ", .{bound});

                        try fmt.format(writer, "\n", .{});
                    }
                }
                try fmt.format(
                    writer,
                    "fn {s}", // FIX:
                    .{name},
                );
                try fmt.format(writer, "(", .{});

                for (data.params) |param| try fmt.format(writer, "{},", .{param});

                try fmt.format(writer, ") ", .{});

                try fmt.format(writer, "{} {{\n", .{data.return_type});

                for (data.params) |param| {
                    if (param.name) |n| {
                        try fmt.format(
                            writer,
                            \\    _ = {}; // autofix todo:
                            \\
                        ,
                            .{n},
                        );
                    }
                }
                try fmt.format(
                    writer,

                    \\    @panic("todo: {s}");
                    \\}}
                    \\
                ,
                    .{name},
                );
            },
            .object_item => |data| {
                const name = self.name.?;

                if (data.generics) |generics| {
                    try fmt.format(writer, "// ", .{});
                    for (generics.items) |generic| try fmt.format(writer, "{s}: type,", .{generic});
                    try fmt.format(writer, "\n", .{});
                }

                var buffer = [_]u8{0} ** 4096;
                var idx: usize = 0;

                for (data.fields, 1..) |fld, i| {
                    const written = try fmt.bufPrint(
                        buffer[idx..],
                        "\t{}",
                        .{fld},
                    );
                    idx += written.len;

                    if (i != data.fields.len) {
                        idx += (try fmt.bufPrint(buffer[idx..], "\n", .{})).len;
                    }
                }

                const fields_str = buffer[0..idx];

                try fmt.format(
                    writer,
                    "" ++
                        \\const {s} = struct {{
                        \\{s}
                        \\}};
                        \\
                    ,

                    .{ name, fields_str },
                );

                try fmt.format(writer, "\n", .{});
            },
            .enum_item => |data| {
                try fmt.format(writer, "const {s} = enum {{\n", .{self.name.?});
                const variants = data.variants;

                for (variants) |variant| try fmt.format(writer, "\t{},", .{variant});
                try fmt.format(writer, "\n}};\n", .{});
                // std.log.warn("unabled to serialize {s}\n", .{@tagName(tag)});
            },
            .type_item => |data| {
                try fmt.format(writer, "//type\nconst {s} = {};\n", .{ self.name.?, data.kind });
            },
            .trait_item => |data| {
                const item_name = self.name.?;

                try fmt.format(writer, "// {s} Interface\n", .{item_name});
                try fmt.format(writer, "const {s} = struct {{\n", .{item_name});
                try fmt.format(writer, "const Self = @This();\n", .{});
                for (data.body.items.items) |item| try item.serialize(writer);

                try fmt.format(writer, "}};\n", .{});
            },
            .function_signature_item => |itemdata| {
                const item_name = self.name.?;

                // TODO: bounds

                try fmt.format(writer, "{s}Fn: *const fn(", .{item_name});
                for (itemdata.params.items) |param| try fmt.format(writer, "{},", .{param.typekind.?});
                try fmt.format(writer, ") ", .{});
                if (itemdata.return_type) |rt| {
                    try fmt.format(writer, "{}", .{rt});
                } else {
                    try fmt.format(writer, "void", .{});
                }
                try fmt.format(writer, ",\n", .{});
            },
        }
    }
};
