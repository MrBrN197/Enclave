const NodeItem = @import("./item.zig").NodeItem;

const std = @import("std");
const SerializeContext = @import("./item.zig").SerializeContext;
const Identifier = @import("./item.zig").Identifier;

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

    pub fn serialize(self: Self, writer: anytype, name: Identifier, ctx: SerializeContext) @TypeOf(writer).Error!void {
        if (self.node_items.items.len > 0) {
            try std.fmt.format(writer, "const {s} = struct {{ // TODO: Module  \n", .{name}); // FIX:
            for (self.node_items.items) |item| {
                if (item.data == .impl_item) continue;
                try item.serialize(writer, ctx);
            }
            try std.fmt.format(writer, "\n}};", .{});
        } else {
            try std.fmt.format(writer, "const {s} = @import(\"{s}\");", .{ name, name });
        }

        try std.fmt.format(writer, "\n", .{});
    }
};
