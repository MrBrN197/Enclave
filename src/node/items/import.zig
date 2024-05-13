const std = @import("std");
const fmt = @import("std").fmt;
const str = @import("../../str.zig");
const path = @import("../path.zig");

pub const Import = struct {
    import_paths: std.ArrayList(path.Buf),

    const Self = @This();

    pub fn init(import_paths: std.ArrayList(path.Buf)) Self {
        return Self{ .import_paths = import_paths };
    }

    pub fn serialize(self: *const @This(), writer: anytype) !void {
        for (self.import_paths.items) |p| {
            const basename = path.ImportPath.basename(p.str());
            if (str.eql(basename, "*")) {
                std.log.warn("todo: path {s}", .{p.str()});
                continue;
            }
            try fmt.format(writer, "const {s} = {s};\n", .{ basename, p.str() });
        }
    }
};
