const root = @import("../root.zig");
const std = @import("std");

const eql = root.eql;
const fmt = std.fmt;
const mem = std.mem;

pub const Path = union(enum) {
    const Self = @This();

    scope: struct {
        name: []const u8,
        alias: ?[]const u8,
        next: ?*Path,
    },
    components: std.ArrayList(Path),

    const PathData = struct { path: []const u8, last: []const u8 };
    pub fn collect_paths(
        self: *const Self,
        prefix: []const u8,
        collect: *std.ArrayList(PathData),
    ) void {
        switch (self.*) {
            .scope => |scope| {
                const name = scope.name;
                const buffer = collect.allocator.alloc(u8, prefix.len + name.len + 1) catch unreachable;
                mem.copyForwards(u8, buffer[0..], prefix);
                mem.copyForwards(u8, buffer[prefix.len..], name);
                const new_prefix_len = prefix.len + name.len;

                var new_prefix = buffer[0..new_prefix_len];

                if (scope.next) |next| {
                    if (!eql(new_prefix, "")) {
                        mem.copyForwards(u8, buffer[new_prefix_len..], ".");
                        new_prefix = buffer[0..(new_prefix_len + 1)];
                    }
                    next.collect_paths(new_prefix, collect);
                } else {
                    collect.append(.{
                        .path = new_prefix,
                        .last = name,
                    }) catch unreachable;
                }
            },
            .components => |components| {
                for (components.items) |*comp| {
                    comp.collect_paths(prefix, collect);
                }
            },
        }
    }

    pub fn is_singluar(self: *const Self) bool {
        var next: ?*const Self = self;
        while (next) |n| {
            if (n.* != .scope) return false;
            next = n.scope.next;
        }
        return true;
    }

    pub fn format(
        self: *const Self,
        _: anytype,
        _: anytype,
        writer: anytype,
    ) !void {
        var paths = std.ArrayList(PathData).init(std.heap.page_allocator); //FIX: HACK: REMOVE

        defer paths.clearAndFree();
        self.collect_paths("", &paths);
        for (paths.items) |p| {
            const last = p.last;
            const path = p.path;
            try fmt.format(writer, "const {s} = {s}; \n", .{ last, path });
        }
    }
};

pub const PathParser = struct {
    const Self = @This();
    const Node = @import("../node.zig").Node;
    const Parser = @import("../parser.zig").Parser;

    parser: *const Parser,
    root: Node,

    allocator: std.mem.Allocator,

    pub fn init(
        parser_: *const Parser,
        root_node: Node,
        allocator: std.mem.Allocator,
    ) Self {
        return .{
            .allocator = allocator,
            .parser = parser_,
            .root = root_node,
        };
    }

    pub fn parse(s: Self) Path {
        const component = s.path(s.root);
        const ptr = s.allocator.create(@TypeOf(component)) catch unreachable;

        ptr.* = component;

        const result = Path{
            .scope = .{
                .name = "", // FIX:
                .next = ptr,
                .alias = null,
            },
        };

        return result;
    }

    pub fn path(s: @This(), node: Node) Path {
        switch (node.node_type) {
            .self => {
                return Path{ .scope = .{
                    .alias = null,
                    .name = "self",
                    .next = null,
                } };
            }, //FIX:
            .crate => {
                return Path{ .scope = .{
                    .alias = null,
                    .name = "crate",
                    .next = null,
                } };
            }, //FIX:
            .super => {
                return Path{ .scope = .{
                    .name = "super",
                    .alias = null,
                    .next = null,
                } };
            }, // FIX:

            .scoped_identifier => {
                return s.scoped_identifier(node);
            },
            .use_as_clause => {
                return s.use_as_clause(node);
            },
            .use_list => {
                return s.use_list(node);
            },
            .scoped_use_list => {
                return s.scoped_use_list(node);
            },
            .use_wildcard => {
                return s.use_wildcard(node);
            },
            .identifier => {
                // $metavariable | $identifier
                const typekind = node.extract_type_ref(s.parser);
                std.debug.assert(typekind == .identifier);
                std.debug.assert(typekind.identifier == .text);

                return Path{ .scope = .{
                    .name = typekind.identifier.text,
                    .next = null,
                    .alias = null,
                } };
            },
            else => {
                s.parser.print_source(node.node);
                @panic("todo");
            },
        }
    }

    fn scoped_identifier(s: @This(), node: Node) Path {
        const name_field = node.get_field_unchecked("name");

        const last_component = blk: {
            if (name_field.node_type == .super) {
                break :blk Path{ .scope = .{
                    .name = "super",
                    .next = null,
                    .alias = null,
                } };
            } else {
                std.debug.assert(name_field.node_type == .identifier);
                const typekind = name_field.extract_type_ref(s.parser);

                break :blk Path{ .scope = .{
                    .name = typekind.identifier.text,
                    .next = null,
                    .alias = null,
                } };
            }
        };

        if (node.get_field("path")) |field| {
            var path_component = blk: {
                switch (field.node_type) {
                    .bracketed_type => @panic("todo"), // FIX:
                    .generic_type => @panic("todo"),
                    else => {
                        break :blk s.path(field);
                    },
                }
            };

            var next: *Path = &path_component;
            while (next.scope.next) |n| next = n;

            const ptr = s.allocator.create(@TypeOf(last_component)) catch unreachable;
            ptr.* = last_component;

            next.scope.next = ptr;
            return path_component;
        } else return last_component;
    }

    pub fn use_as_clause(s: @This(), node: Node) Path {
        const path_field = node.get_field_unchecked("path");
        var path_component = s.path(path_field);

        var last_component: *Path = &path_component;

        while (last_component.scope.next) |n| {
            last_component = n;
        }

        const alias_field = node.get_field_unchecked("alias");
        const alias_identifier = alias_field.extract_type_ref(s.parser).identifier;

        last_component.scope.alias = alias_identifier.text;
        return path_component;
    }

    pub fn use_list(s: @This(), node: Node) Path {
        const children = node.get_children_named();
        var collect = std.ArrayList(Path).init(s.allocator);

        for (children.items) |child| {
            switch (child.node_type) {
                .use_as_clause => {
                    const path_component = s.use_as_clause(child);
                    collect.append(path_component) catch unreachable;
                },
                .use_list => {
                    const path_component = s.use_list(child);
                    collect.append(path_component) catch unreachable;
                },

                .scoped_use_list => {
                    collect.append(s.scoped_use_list(child)) catch unreachable;
                },
                .use_wildcard => {
                    collect.append(s.use_wildcard(child)) catch unreachable;
                },
                .identifier => {
                    const name = child.extract_type_ref(s.parser);
                    collect.append(Path{ .scope = .{
                        .name = name.identifier.text,
                        .next = null,
                        .alias = null,
                    } }) catch unreachable;
                },
                else => {
                    const x = s.path(child);
                    collect.append(x) catch unreachable;
                },
            }
        }
        return Path{
            .components = collect,
        };
    }

    pub fn scoped_use_list(s: @This(), node: Node) Path {
        const use_list_field = node.get_field_unchecked("list");
        const last_components = s.use_list(
            use_list_field,
        );

        if (node.get_field("path")) |field| {
            var path_components = s.path(field);
            var next = &path_components;

            while (next.scope.next) |n| next = n;

            const ptr = s.allocator.create(@TypeOf(last_components)) catch unreachable;
            ptr.* = last_components;
            next.scope.next = ptr;

            return path_components;
        }
        return last_components;
    }

    pub fn use_wildcard(s: @This(), node: Node) Path {
        const children = node.get_children_named();
        std.debug.assert(children.items.len == 1);
        const child = children.items[0];

        var path_component = s.path(child);

        var next: *Path = &path_component;

        while (next.scope.next) |n| {
            next = n;
        }

        const last = Path{
            .scope = .{
                .name = "*", // FIX:
                .next = null,
                .alias = null,
            },
        };

        const ptr = s.allocator.create(@TypeOf(last)) catch unreachable;

        ptr.* = last;
        return path_component;
    }

    fn extract_bracketed_type(_: *const @This(), _: *const Parser) void {
        @panic("todo");
    }
};
