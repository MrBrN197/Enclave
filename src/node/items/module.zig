const NodeItem = @import("../item.zig").NodeItem;

const std = @import("std");

pub const Module = struct {
    node_items: std.ArrayList(NodeItem),

    const Item = NodeItem;
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .node_items = std.ArrayList(Item).init(allocator),
        };
    }

    pub fn insertItem(self: *Self, item: Item) void {
        self.node_items.append(item) catch unreachable;
    }

    pub fn serialize(self: Self, writer: anytype, name: []const u8) @TypeOf(writer).Error!void {
        if (self.node_items.items.len > 0) {
            try std.fmt.format(writer, "const {s} = struct {{ // TODO: Module  \n", .{name}); // FIX:
            for (self.node_items.items) |item| try item.serialize(writer);
            try std.fmt.format(writer, "\n}};", .{});
        } else {
            try std.fmt.format(writer, "const {s} = @import(\"{s}\");", .{ name, name });
        }

        try std.fmt.format(writer, "\n", .{});
    }
};
