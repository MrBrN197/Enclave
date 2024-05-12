const std = @import("std");
const TypeKind = @import("../item.zig").TypeKind;

pub const Object = struct {
    bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
    fields: []Field,
    ordered: bool,
    generics: ?std.ArrayList(TypeKind),
};

pub const Field = union(enum) {
    tuple: TypeKind,
    field: struct {
        name: []const u8,
        type_kind: TypeKind,
    },

    pub fn format(
        self: Field,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .tuple => @panic("todo"),
            .field => |f| {
                try std.fmt.format(
                    writer,
                    "\t{s}: {},", // FIX:
                    .{ f.name, f.type_kind },
                );
            },
        }
    }
};

pub const Enum = struct {
    variants: []const Variant,

    pub const Variant = struct {
        name: []const u8,
        // typekind: TypeKind, //TODO:

        const Self = @This();

        pub fn format(
            self: Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            return std.fmt.format(writer, "{s}", .{self.name}); //FIX:
        }
    };
};
