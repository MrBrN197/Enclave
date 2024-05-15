const std = @import("std");
const TypeKind = @import("./item.zig").TypeKind;
const Impl = @import("./item.zig").Impl;
const SerializeContext = @import("./item.zig").SerializeContext;

const fmt = std.fmt;

pub const Object = struct {
    bounds: std.StringArrayHashMap(std.ArrayList(TypeKind)),
    fields: []Field,
    ordered: bool,
    generics: ?std.ArrayList(TypeKind),

    const Self = @This();

    pub fn serialize(self: *const Self, writer: anytype, name: []const u8, ctx: SerializeContext) !void {
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

        try fmt.format(writer, "" ++
            \\pub const {s} = struct {{
            \\{s}
            \\
        , .{ name, fields_str });

        try fmt.format(writer, "\n", .{});

        var impls = std.ArrayList(*const Impl).init(std.heap.page_allocator);
        defer impls.clearAndFree();
        ctx.getImpls(name, &impls);

        for (impls.items) |impl| {
            const interface_name = impl.for_interface.?.text;

            try fmt.format(
                writer,
                "// {s} interface implementation for {s}\n",
                .{ interface_name, name },
            );

            const interface = ctx.getInterface(interface_name) catch unreachable; // TODO:

            for (interface.procedures.items) |proc| {
                try fmt.format(writer, "fn {s}_{s}(_: *anyopaque", .{
                    interface_name,
                    proc.name,
                });
                for (proc.data.params.items) |fnsignature| try fmt.format(writer, ",_: {}", .{fnsignature.typekind.?});

                if (proc.data.return_type) |return_type| {
                    try fmt.format(writer, ") {s} {{\n", .{return_type});
                } else {
                    try fmt.format(writer, ") void {{\n", .{});
                }
                try fmt.format(writer,
                    \\// TODO: 
                    \\@import("std").debug.print("TODO: '{s}'", .{{}});
                    \\return 0;
                    \\}}
                    \\
                , .{proc.name});
            }

            try fmt.format(writer,
                \\pub fn get{s} (self: *@This(),) 
                \\ Any{s} {{
            , .{ interface_name, interface_name });

            try fmt.format(writer,
                \\
                \\    return .{{
                \\        .context = self,
                \\
            , .{});

            for (interface.procedures.items) |proc| {
                try fmt.format(writer,
                    \\        .{s}Fn = @ptrCast(&{s}_{s}),
                    \\
                , .{ proc.name, interface_name, proc.name });
            }

            try fmt.format(writer, "}};\n", .{});
            try fmt.format(writer, "}}\n", .{});
        }

        try fmt.format(writer, "}};", .{});
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
