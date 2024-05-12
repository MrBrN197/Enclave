const IdentifierKind = @import("../item.zig").IdentifierKind;
const TypeKind = @import("../types.zig").TypeKind;

const std = @import("std");

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
