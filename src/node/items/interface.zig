const std = @import("std");
const fmt = std.fmt;
const Module = @import("./module.zig").Module;
const SerializeContext = @import("./item.zig").SerializeContext;
const TypeKind = @import("../types.zig").TypeKind;

pub const Trait = struct {
    body: Module,
    bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
    generics: ?std.ArrayList(TypeKind),

    pub fn serialize(
        self: *const @This(),
        writer: anytype,
        name: []const u8,
        ctx: SerializeContext,
    ) @TypeOf(writer).Error!void {
        try fmt.format(writer,
            \\
            \\// '{[name]s}' interface
            \\pub const Any{[name]s} = struct {{
            \\context: *anyopaque,
            \\
            \\
            \\
        , .{ .name = name });

        for (self.body.node_items.items) |item| {
            switch (item.data) {
                .function_signature_item => |fnsig| {
                    try fmt.format(writer, "{s}Fn: *const fn(*anyopaque", .{item.name.?});

                    for (fnsig.params.items) |param| try fmt.format(writer, ",{}", .{param.typekind.?});

                    try fmt.format(writer, ") ", .{});

                    if (fnsig.return_type) |rt| {
                        try fmt.format(writer, "{},\n", .{rt});
                    } else {
                        try fmt.format(writer, "void,\n", .{});
                    }
                },
                else => {},
            }
        }
        try fmt.format(writer, "\n" ** 3, .{});

        for (self.body.node_items.items) |item| {
            switch (item.data) {
                .function_signature_item => |fnsig| {
                    try fmt.format(writer, "pub fn {s}(self: *@This()", .{item.name.?});

                    for (fnsig.params.items) |param| try fmt.format(writer, ", {}", .{param});

                    try fmt.format(writer, ") ", .{});

                    if (fnsig.return_type) |rt| {
                        try fmt.format(writer, "{} {{\n", .{rt});
                    } else {
                        try fmt.format(writer, "void {{\n", .{});
                    }
                    try fmt.format(writer, "\treturn self.{s}Fn(self.context", .{item.name.?});

                    for (fnsig.params.items) |param| try fmt.format(writer, ",{}", .{param.name.?});

                    try fmt.format(writer, ");\n}}\n", .{});
                },
                else => {},
            }
        }

        try fmt.format(writer, "\n" ** 3, .{});

        for (self.body.node_items.items) |item| {
            if (item.data == .function_signature_item) continue;
            try item.serialize(writer, ctx);
        }

        // for (self.body.node_items.items) |item| {
        //     switch (item.data) {
        //         .function_signature_item => |fnsig| {
        //             try fmt.format(writer, "pub fn {s}(self: @This() ", .{item.name.?});

        //             for (fnsig.params.items) |param| {
        //                 try fmt.format(
        //                     writer,
        //                     ",{}",
        //                     .{param},
        //                 );
        //             }

        //             if (fnsig.return_type) |rt| {
        //                 try fmt.format(writer, ") {}{{\n", .{rt});
        //             } else {
        //                 try fmt.format(writer, ") void{{\n", .{});
        //             }

        //             try fmt.format(
        //                 writer,
        //                 "return {s}Fn(self.context\n",
        //                 .{item.name.?},
        //             );
        //             for (fnsig.params.items) |param| {
        //                 try fmt.format(
        //                     writer,
        //                     ",{s}",
        //                     .{param.name.?},
        //                 );
        //             }
        //             try fmt.format(
        //                 writer,
        //                 ");\n",
        //                 .{},
        //             );

        //             try fmt.format(writer, "}}\n", .{});
        //         },
        //         else => {},
        //     }
        // }

        try fmt.format(writer, "}};\n", .{});
    }
};
