const NodeItem = @import("../item.zig").NodeItem;

const std = @import("std");

pub const Module = struct {
    items: std.ArrayList(NodeItem),

    const Item = NodeItem;
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .items = std.ArrayList(Item).init(allocator),
        };
    }

    pub fn insertItem(self: *Self, item: Item) void {
        self.items.append(item) catch unreachable;
    }
};
