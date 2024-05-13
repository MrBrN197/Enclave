const std = @import("std");
const TypeKind = @import("../item.zig").TypeKind;

const fmt = std.fmt;

pub const Object = struct {
    bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
    fields: []Field,
    ordered: bool,
    generics: ?std.ArrayList(TypeKind),

    const Self = @This();

    pub fn serialize(self: *const Self, writer: anytype, name: []const u8) !void {
        if (self.generics) |generics| {
            try fmt.format(writer, "// ", .{});
            for (generics.items) |generic| try fmt.format(writer, "{s}: type,", .{generic});
            try fmt.format(writer, "\n", .{});
        }

        var buffer = [_]u8{0} ** 4096;
        var idx: usize = 0;

        for (self.fields, 1..) |fld, i| {
            const written = try fmt.bufPrint(
                buffer[idx..],
                "\t{}",
                .{fld},
            );
            idx += written.len;

            if (i != self.fields.len) {
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
    }
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
        _: fmt.FormatOptions,
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

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: fmt.FormatOptions,
            writer: anytype,
        ) !void {
            return std.fmt.format(writer, "{s}", .{self.name}); //FIX:
        }
    };

    pub fn serialize(self: *const @This(), writer: anytype, name: []const u8) !void {
        try fmt.format(writer, "const {s} = enum {{\n", .{name});
        const variants = self.variants;

        for (variants) |variant| try fmt.format(writer, "\t{},", .{variant});
        try fmt.format(writer, "\n}};\n", .{});
        // std.log.warn("unabled to serialize {s}\n", .{@tagName(tag)});
    }
};
