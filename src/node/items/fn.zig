const std = @import("std");

const TypeKind = @import("./item.zig").TypeKind;
const procedure = @import("./procedure.zig");
const TypeParam = @import("./item.zig").TypeParam;

pub const FnSignature = struct {
    generics: ?std.ArrayList(TypeParam),
    params: std.ArrayList(procedure.Param),
    return_type: ?TypeKind,

    pub fn serialize(self: *const @This(), writer: anytype, name: []const u8) !void {
        // TODO: bounds

        try std.fmt.format(writer, "{s}Fn: *const fn(", .{name});
        for (self.params.items) |param| try std.fmt.format(writer, "{},", .{param.typekind.?});
        try std.fmt.format(writer, ") ", .{});
        if (self.return_type) |rt| {
            try std.fmt.format(writer, "{}", .{rt});
        } else {
            try std.fmt.format(writer, "void", .{});
        }
        try std.fmt.format(writer, ",\n", .{});
    }
};
