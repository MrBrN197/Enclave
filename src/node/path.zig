const root = @import("../root.zig");
const std = @import("std");

const eql = root.eql;
const fmt = std.fmt;

const mem = std.mem;

pub const PathBuf = struct {
    const Self = @This();
    const SIZE = 4096;

    buffer: [SIZE]u8,
    len: usize,

    pub fn from(string: []const u8) PathBuf {
        var buffer = [_]u8{0} ** SIZE;
        mem.copyForwards(u8, buffer[0..], string);
        return .{
            .buffer = buffer,
            .len = string.len,
        };
    }

    pub fn append(self: *Self, string: []const u8) void {
        std.debug.assert(self.len + string.len < self.buffer.len);
        std.mem.copyForwards(u8, self.buffer[self.len..], string);
        self.len += string.len;
    }

    pub fn str(self: *const Self) []const u8 {
        return self.buffer[0..self.len];
    }
};

pub const ImportPath = union(enum) {
    const Self = @This();

    scope: struct {
        name: PathBuf,
        alias: ?[]const u8,
        next: ?*ImportPath,
    },
    components: std.ArrayList(ImportPath),

    const PathData = struct { path: []const u8, last: []const u8 };
    pub fn collect_paths(self: *const Self, prefix: []const u8, collect: *std.ArrayList([]const u8)) void {
        switch (self.*) {
            .scope => |scope| {
                const name = scope.name;
                const buffer = collect.allocator.alloc(u8, prefix.len + name.len + 1) catch unreachable;
                mem.copyForwards(u8, buffer[0..], prefix);
                mem.copyForwards(u8, buffer[prefix.len..], name.str());
                const new_prefix_len = prefix.len + name.len;

                var new_prefix = buffer[0..new_prefix_len];

                if (scope.next) |next| {
                    if (!eql(new_prefix, "")) {
                        mem.copyForwards(u8, buffer[new_prefix_len..], ".");
                        new_prefix = buffer[0..(new_prefix_len + 1)];
                    }
                    next.collect_paths(new_prefix, collect);
                } else {
                    collect.append(new_prefix) catch unreachable;
                }
            },
            .components => |components| {
                for (components.items) |*comp| {
                    comp.collect_paths(prefix, collect);
                }
            },
        }
    }

    pub fn basename(string: []const u8) []const u8 { //FIX: remove
        var last: @TypeOf(string) = undefined;
        var iter = mem.splitScalar(u8, string, '.');
        while (iter.next()) |next| last = next;
        return last;
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

    pub fn parse(s: Self) PathBuf {
        var collect = std.ArrayList(PathBuf).init(s.allocator);
        defer {
            collect.clearAndFree();
        }

        s.path(s.root, &collect);

        var result = PathBuf.from("");
        const end = collect.items.len;

        for (collect.items, 0..) |pathbuf, i| {
            result.append(pathbuf.buffer[0..pathbuf.len]);

            if (i != (end - 1)) result.append(".");
        }

        return result;
    }

    pub fn path(s: @This(), node: Node, collect: *std.ArrayList(PathBuf)) void {
        switch (node.node_type) {
            .self, .crate, .super => |tag| {
                const source = @tagName(tag);
                const pathbuf = PathBuf.from(source);
                collect.append(pathbuf) catch unreachable;
            }, //FIX:

            .scoped_identifier => {
                s.scoped_identifier(node, collect);
            },
            .identifier => {
                // $metavariable | $identifier
                const typekind = node.extract_type_ref(s.parser);
                std.debug.assert(typekind == .identifier);
                std.debug.assert(typekind.identifier == .text);

                const p = PathBuf.from(typekind.identifier.text);
                collect.append(p) catch unreachable;
            },
            .use_as_clause, .use_list, .scoped_use_list, .use_wildcard => unreachable,

            else => {
                s.parser.print_source(node.node);
                @panic("todo");
            },
        }
    }

    fn scoped_identifier(s: @This(), node: Node, collect: *std.ArrayList(PathBuf)) void {
        const name_field = node.get_field_unchecked("name");

        const last_component: PathBuf = blk: {
            if (name_field.node_type == .super) {
                const p = PathBuf.from("super");
                break :blk p;
            } else {
                std.debug.assert(name_field.node_type == .identifier);
                const typekind = name_field.extract_type_ref(s.parser);
                const p = PathBuf.from(typekind.identifier.text);
                break :blk p;
            }
        };

        if (node.get_field("path")) |field| switch (field.node_type) {
            .bracketed_type => @panic("todo"), // FIX:
            .generic_type => @panic("todo"),
            else => s.path(field, collect),
        };

        collect.append(last_component) catch unreachable;
    }

    fn extract_bracketed_type(_: *const @This(), _: *const Parser) void {
        @panic("todo");
    }
};

pub const ImportPathParser = struct {
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

    pub fn parse(s: Self) ImportPath {
        const component = s.path(s.root);
        const ptr = s.allocator.create(@TypeOf(component)) catch unreachable;

        ptr.* = component;

        const result = ImportPath{
            .scope = .{
                .name = PathBuf.from(""), // FIX:
                .next = ptr,
                .alias = null,
            },
        };

        return result;
    }

    pub fn path(s: Self, node: Node) ImportPath {
        switch (node.node_type) {
            .self, .crate, .super, .scoped_identifier, .identifier => {
                const result = PathParser.init(s.parser, node, s.allocator)
                    .parse();
                const r = ImportPath{ .scope = .{
                    .name = result,
                    .alias = null,
                    .next = null,
                } };
                return r;
            }, // FIX:

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

            else => {
                s.parser.print_source(node.node);
                @panic("todo");
            },
        }
    }

    fn scoped_identifier(s: @This(), node: Node) ImportPath {
        const name_field = node.get_field_unchecked("name");

        const last_component = blk: {
            if (name_field.node_type == .super) {
                break :blk ImportPath{ .scope = .{
                    .name = "super",
                    .next = null,
                    .alias = null,
                } };
            } else {
                std.debug.assert(name_field.node_type == .identifier);
                const typekind = name_field.extract_type_ref(s.parser);

                break :blk ImportPath{ .scope = .{
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

            var next: *ImportPath = &path_component;
            while (next.scope.next) |n| next = n;

            const ptr = s.allocator.create(@TypeOf(last_component)) catch unreachable;
            ptr.* = last_component;

            next.scope.next = ptr;
            return path_component;
        } else return last_component;
    }

    pub fn use_as_clause(s: Self, node: Node) ImportPath {
        const path_field = node.get_field_unchecked("path");
        var path_component = s.path(path_field);

        var last_component: *ImportPath = &path_component;

        while (last_component.scope.next) |n| {
            last_component = n;
        }

        const alias_field = node.get_field_unchecked("alias");
        const alias_identifier = alias_field.extract_type_ref(s.parser).identifier;

        last_component.scope.alias = alias_identifier.text;
        return path_component;
    }

    pub fn scoped_use_list(s: Self, node: Node) ImportPath {
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

    pub fn use_wildcard(s: Self, node: Node) ImportPath {
        const children = node.get_children_named();
        std.debug.assert(children.items.len == 1);
        const child = children.items[0];

        var path_component = s.path(child);

        var next: *ImportPath = &path_component;

        while (next.scope.next) |n| {
            next = n;
        }

        const last = ImportPath{
            .scope = .{
                .name = PathBuf.from("*"), // FIX:
                .next = null,
                .alias = null,
            },
        };

        const ptr = s.allocator.create(@TypeOf(last)) catch unreachable;

        ptr.* = last;
        return path_component;
    }

    pub fn use_list(s: Self, node: Node) ImportPath {
        const children = node.get_children_named();
        var collect = std.ArrayList(ImportPath).init(s.allocator);

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
                    collect.append(ImportPath{ .scope = .{
                        .name = PathBuf.from(name.identifier.text),
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

        return ImportPath{
            .components = collect,
        };
    }

    fn extract_bracketed_type(_: *const @This(), _: *const Parser) void {
        @panic("todo");
    }
};
