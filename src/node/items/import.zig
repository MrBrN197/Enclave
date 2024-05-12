const std = @import("std");
const path = @import("../path.zig");

pub const Import = struct {
    import_paths: std.ArrayList(path.Buf),

    const Self = @This();

    pub fn init(import_paths: std.ArrayList(path.Buf)) Self {
        return Self{ .import_paths = import_paths };
    }
};
