const path = @import("./path.zig");
const root = @import("../root.zig");
const std = @import("std");

const assert = std.debug.assert;
const fmt = std.fmt;
const is_empty = root.is_empty;
const mem = std.mem;
const eprintln = root.eprintln;
const PathBuf = path.PathBuf;

pub const IdentifierKind = union(enum) {
    discarded, // FIX: '_'
    matched: []const u8, // FIX:
    scoped: struct {
        path: PathBuf,
        name: []const u8,
    },
    self,
    text: []const u8,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .text => |name| {
                return fmt.format(
                    writer,
                    "{s}", //FIX:
                    .{name},
                );
            },
            .scoped => |scoped| {
                try fmt.format(writer, "{s}.{s}", .{
                    scoped.path.buffer[0..scoped.path.len],
                    scoped.name,
                });
            },

            .discarded => try fmt.format(writer, "_", .{}),
            .matched => |matched| try fmt.format(writer, "{s}", .{matched}),
            else => |tag| {
                eprintln("\n\ntext: `{s}`\n\n", .{@tagName(tag)});
                @panic("todo");
            },
        }
    }
};

pub const NodeItem = struct {
    // TODO: visibility,

    // TODO: contents: {}
    annotations: ?std.ArrayList([]const u8),
    data: Data,
    name: ?[]const u8, // TODO: remove
    path: ?[]const u8,

    pub const Data = union(enum) {
        const_item: Constant,
        enum_item: Enum,
        impl_item: Impl,
        module_item: Module,
        object_item: Object,
        procedure_item: Procedure,
        trait_item: struct {
            name: []const u8,
            items: std.ArrayList(NodeItem),
            constraints: std.ArrayList([]const u8),
        },
        type_item: TypeItem,
        import_item: path.ImportPath,

        pub const Constant = struct {
            name: IdentifierKind,
            value_expr: ?[]const u8,
            type_kind: TypeKind,
        };

        pub const Impl = struct {
            procedures: ?[]const Procedure,
            item_ref: TypeKind, // TODO: remove '?'
        };

        pub const Module = struct {
            contents: ?std.ArrayList(NodeItem), // TODO:
        };

        pub const Enum = struct {
            variants: []const Variant,

            pub const Variant = struct {
                name: []const u8,
            };
        };

        pub const Procedure = struct {
            params: []const Param,
            return_type: TypeKind,
            generics: ?std.ArrayList(TypeKind),

            pub const Param = struct {
                name: ?IdentifierKind,
                typekind: ?TypeKind,

                const Self = @This();
                pub fn format(
                    self: Self,
                    comptime _: []const u8,
                    _: fmt.FormatOptions,
                    writer: anytype,
                ) !void {
                    assert(self.typekind != null);
                    const ty = self.typekind.?;

                    if (self.name) |name| {
                        return fmt.format(writer, "{}: {}", .{ name, ty });
                    } else {
                        return fmt.format(writer, "{}", .{ty});
                    }
                }
            };
        };

        pub const Object = struct {
            fields: []Field,
            ordered: bool,
            generics: ?std.ArrayList(TypeKind),

            pub const Field = union(enum) {
                tuple: TypeKind,
                field: struct {
                    name: []const u8,
                    type_kind: TypeKind,
                },

                pub fn format(
                    self: Field,
                    comptime _: []const u8,
                    _: fmt.FormatOptions,
                    writer: anytype,
                ) !void {
                    switch (self) {
                        .tuple => @panic("todo"),
                        .field => |f| {
                            try fmt.format(
                                writer,
                                "\t{s}: {},", // FIX:
                                .{ f.name, f.type_kind },
                            );
                        },
                    }
                }
            };
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
                params: ?std.ArrayList(Procedure.Param),
                return_type: ?*const TypeKind,
            },
            ref: struct { child: ?*const TypeKind },
            tuple: std.ArrayList(TypeKind),

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
                        const brackets = array.length_expr orelse "[_]";

                        const typename = array.child;

                        try fmt.format(writer, "{s}{}", .{ brackets, typename.* });
                    },
                    .ref => |ref| {
                        // FIX:
                        assert(ref.child != null);

                        try fmt.format(writer, "*const {}", .{ref.child.?.*});
                    },

                    // .dynamic => @panic("todo"),
                    .generic => |generic| try fmt.format(writer, "{s}", .{generic.name}),
                    .none => try fmt.format(writer, "void", .{}),
                    .no_return => @panic("todo"),
                    .proc => try fmt.format(writer, "fn(void) void", .{}), // FIX:

                    else => return fmt.format(writer, "__{s}_type", .{@tagName(self)}),
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
                _: fmt.FormatOptions,
                writer: anytype,
            ) !void {
                return fmt.format(writer, "{s}", .{self.name});
            }
        };
    };

    pub fn init(data: Data, name: ?[]const u8) NodeItem {
        if (name) |str| assert(!is_empty(str));

        return NodeItem{
            .data = data,
            .name = name,
            .path = null, // TODO:
            .annotations = null, // TODO:
        };
    }

    pub fn serialize(self: *const NodeItem, writer: anytype) !void {
        switch (self.data) {
            .import_item => |import_path| {
                var collect = std.ArrayList([]const u8)
                    .init(std.heap.page_allocator); //FIX:
                defer collect.clearAndFree();

                import_path.collect_paths("", &collect);

                for (collect.items) |p| {
                    const basename = path.ImportPath.basename(p);
                    try fmt.format(writer, "const {s} = {s};\n", .{ basename, p });
                }
            },
            .module_item => |mod| {
                assert(self.name != null);

                const name = self.name.?;

                if (mod.contents) |contents| {
                    try fmt.format(writer, "const {s} = struct {{ // TODO: Module  \n", .{name}); // FIX:
                    for (contents.items) |item| {
                        try item.serialize(writer);
                    }
                    try fmt.format(writer, "\n}};", .{});
                } else {
                    try fmt.format(writer, "const {s} = @import(\"______\");", .{name});
                }

                try fmt.format(writer, "\n", .{});
            },
            .impl_item => |_| {
                try fmt.format(writer, "", .{});
            },
            .const_item => |item_data| {
                assert(item_data.name == .text);
                assert(item_data.value_expr != null);

                const val = item_data.value_expr.?;

                try fmt.format(writer, "// const {s}: {} = {s};", .{
                    item_data.name.text,
                    item_data.type_kind,
                    val,
                });
                try fmt.format(writer, "\n", .{});
            },
            .procedure_item => |data| {
                const name = self.name orelse unreachable;
                // TODO: visibility

                if (data.generics) |generics| {
                    try fmt.format(writer, "// ", .{});
                    for (generics.items) |generic| try fmt.format(writer, "{s}: type,", .{generic});
                    try fmt.format(writer, "\n", .{});
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
                const name = self.name orelse unreachable;

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
                        \\const {s} =  struct {{
                        \\{s}
                        \\}};
                        \\
                    ,

                    .{ name, fields_str },
                );

                try fmt.format(writer, "\n", .{});
            },

            else => |tag| {
                if (true) return; // FIX:
                const name = self.name orelse unreachable;
                const item_type = blk: {
                    switch (tag) {
                        Data.object_item, Data.module_item => break :blk "struct",
                        Data.enum_item => break :blk "enum",
                        else => {
                            eprintln("unable to serialize '{s}'", .{@tagName(tag)});
                            unreachable;
                        },
                    }
                };
                try fmt.format(writer, "const {s} = {s}", .{ name, item_type });
                try fmt.format(writer, "\n", .{});
            },
        }
    }
};
