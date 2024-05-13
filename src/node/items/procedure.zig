const IdentifierKind = @import("../item.zig").IdentifierKind;
const TypeKind = @import("../types.zig").TypeKind;

const std = @import("std");
const fmt = std.fmt;
const assert = std.debug.assert;

pub const Procedure = struct {
    bounds: ?std.StringArrayHashMap(std.ArrayList(TypeKind)),
    generics: ?std.ArrayList(TypeKind),
    params: []const Param,
    return_type: TypeKind,

    pub fn get_bounds(self: *const @This(), type_param_identifier: []const u8) ?[]const TypeKind {
        if (self.bounds) |bounds| {
            const result = bounds.get(type_param_identifier) orelse return null;
            return result.items;
        } else return null;
    }

    pub fn serialize(self: *const @This(), writer: anytype, name: []const u8) !void {

        // TODO: visibility

        if (self.generics) |generics| {
            for (generics.items) |generic| {
                try fmt.format(writer, "// ", .{});
                assert(generic == .identifier);
                try fmt.format(writer, "{s}: ", .{generic});

                if (self.get_bounds(generic.identifier.text)) |bounds|
                    for (bounds) |bound| fmt.format(writer, "{}, ", .{bound}) catch unreachable; //FIX: infered error

                try fmt.format(writer, "\n", .{});
            }
        }
        try fmt.format(writer, "fn {s}", .{name});
        try fmt.format(writer, "(", .{});

        for (self.params) |param| try fmt.format(writer, "{},", .{param});

        try fmt.format(writer, ") ", .{});

        try fmt.format(writer, "{} {{\n", .{self.return_type});

        for (self.params) |param| {
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
    }
};

pub const Param = struct {
    name: ?IdentifierKind,
    typekind: ?TypeKind,

    const Self = @This();
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        std.debug.assert(self.typekind != null);
        const ty = self.typekind.?;

        if (self.name) |name| {
            return std.fmt.format(writer, "{}: {}", .{ name, ty });
        } else {
            return std.fmt.format(writer, "{}", .{ty});
        }
    }
};
