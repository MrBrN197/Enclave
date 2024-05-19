const fmt = std.fmt;
const mem = std.mem;
const std = @import("std");

pub const Buf = struct {
    const Self = @This();
    const SIZE = 4096;

    allocator: std.mem.Allocator,
    buffer: [SIZE]u8,
    len: usize,

    pub fn from(string: []const u8) Buf {
        var buffer = [_]u8{0} ** SIZE;
        mem.copyForwards(u8, buffer[0..], string);
        return .{
            .allocator = std.heap.page_allocator, // FIX:
            .buffer = buffer,
            .len = string.len,
        };
    }

    fn deinit(self: *Self) void {
        self.allocator.free(self.buffer);
        @panic("todo: ");
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
    multiple: std.ArrayList(ImportPath),
    singular: struct {
        name: Buf, // FIX: rename
        alias: ?[]const u8,
        next: ?*ImportPath,
    },

    const Identifier = @import("./types.zig").Identifier;
    const PathData = struct { path: []const u8, last: []const u8 };
    const Self = @This();

    pub fn collect_paths(self: *const Self, prefix: []const u8, collect: *std.ArrayList(Buf)) void {
        switch (self.*) {
            .singular => |comp| {
                var new_prefix = Buf.from(prefix);
                new_prefix.append(comp.name.str());

                if (comp.next) |next| {
                    if (new_prefix.len != 0) new_prefix.append(".");
                    next.collect_paths(new_prefix.str(), collect);
                } else {
                    collect.append(new_prefix) catch unreachable;
                }
            },
            .multiple => |components| {
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
        allocator: std.mem.Allocator,
        parser_: *const Parser,
        root_node: Node,
    ) Self {
        return .{
            .allocator = allocator,
            .parser = parser_,
            .root = root_node,
        };
    }

    pub fn parse(self: Self) Buf {
        var collect = std.ArrayList(Buf).init(self.allocator);
        defer {
            collect.clearAndFree();
        }

        self.path(self.root, &collect);

        var result = Buf.from("");
        const end = collect.items.len;

        for (collect.items, 0..) |pathbuf, i| {
            result.append(pathbuf.buffer[0..pathbuf.len]);

            if (i != (end - 1)) result.append(".");
        }

        return result;
    }

    pub fn path(self: Self, node: Node, collect: *std.ArrayList(Buf)) void {
        switch (node.node_type) {
            .self, .crate, .super => |tag| {
                const source = @tagName(tag);
                const pathbuf = Buf.from(source);
                collect.append(pathbuf) catch unreachable;
            }, //FIX:

            .scoped_identifier => {
                self.scoped_identifier(node, collect);
            },
            .identifier => {
                // $metavariable | $identifier
                const typekind = node.extract_type_ref();
                std.debug.assert(typekind == .identifier);
                std.debug.assert(typekind.identifier == .text);

                const p = Buf.from(typekind.identifier.text);
                collect.append(p) catch unreachable;
            },
            .use_as_clause, .use_list, .scoped_use_list, .use_wildcard => unreachable,
            .bracketed_type => {
                if (Node.getFirstChildNamed(node, .qualified_type)) |child| {
                    const type_field = child.get_field_unchecked("type"); // , $._type),
                    const lhs = type_field.extract_type_ref();
                    _ = lhs; // autofix

                    const alias_field = child.get_field_unchecked("alias"); // , $._type),
                    const rhs = alias_field.extract_type_ref();

                    var p = Buf.from("Any");
                    p.append(rhs.identifier.text);
                    collect.append(p) catch unreachable;
                } else {
                    var children = node.getNamedChildren();
                    defer children.clearAndFree();
                    std.debug.assert(children.items.len == 1);

                    const child = children.items[0];
                    self.path(child.*, collect);
                }
            },
            .generic_type => @panic("todo: generic_type_with_turbofish"),
            else => {
                self.parser.print_source(node.node);
                const c = @import("../c.zig");
                var parent = c.ts_node_parent(node.node);
                parent = c.ts_node_parent(parent);

                const tree = c.ts_node_string(parent);
                std.debug.print("Tree: {s}\n", .{tree});
                const name = @import("../node.zig").get_type(parent, self.allocator);

                std.debug.print("Parent Name: {s}\n", .{name});

                @panic("invalid node type");
            },
        }
    }

    fn scoped_identifier(self: @This(), node: Node, collect: *std.ArrayList(Buf)) void {
        var name_field = node.get_field_unchecked("name");

        // FIX:
        const last_component: Buf = blk: {
            if (name_field.node_type == .super) {
                const p = Buf.from("super");
                break :blk p;
            } else {
                std.debug.assert(name_field.node_type == .identifier);
                const typekind = name_field.extract_type_ref();
                const p = Buf.from(typekind.identifier.text);
                break :blk p;
            }
        };

        if (node.get_field("path")) |field| switch (field.node_type) {
            .bracketed_type => unreachable,
            .generic_type => @panic("todo"),
            else => self.path(field.*, collect),
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

    parser: *const Parser, //FIX: remove
    root: Node,

    allocator: std.mem.Allocator,

    pub fn init(
        allocator: std.mem.Allocator,
        parser_: *const Parser,
        root_node: Node,
    ) Self {
        return .{
            .allocator = allocator,
            .parser = parser_,
            .root = root_node,
        };
    }

    pub fn parse(self: Self, collect: *std.ArrayList(Buf)) void {
        const component = self.path(self.root);

        const ptr = self.allocator.create(@TypeOf(component)) catch unreachable;

        ptr.* = component;

        const import_path = ImportPath{
            .singular = .{
                .name = Buf.from(""), // FIX:
                .next = ptr,
                .alias = null,
            },
        };

        import_path.collect_paths("", collect);
    }

    pub fn path(self: Self, node: Node) ImportPath { // FIX: remove self
        switch (node.node_type) {
            .self, .crate, .super, .scoped_identifier, .identifier => {
                const result = PathParser.init(self.allocator, self.parser, node)
                    .parse();

                const r = ImportPath{ .singular = .{
                    .name = result,
                    .alias = null,
                    .next = null,
                } };
                return r;
            }, // FIX:

            .use_as_clause => {
                return self.use_as_clause(node);
            },
            .use_list => {
                return self.use_list(node);
            },
            .scoped_use_list => {
                return self.scoped_use_list(node);
            },
            .use_wildcard => {
                return self.use_wildcard(node);
            },

            else => {
                self.parser.print_source(node.node);
                @panic("invalid node type");
            },
        }
    }

    fn scoped_identifier(self: @This(), node: Node) ImportPath {
        var name_field = node.get_field_unchecked("name");

        // FIX:
        const last_component = blk: {
            if (name_field.node_type == .super) {
                break :blk ImportPath{ .singular = .{
                    .name = "super",
                    .next = null,
                    .alias = null,
                } };
            } else {
                std.debug.assert(name_field.node_type == .identifier);
                const typekind = name_field.extract_type_ref();

                break :blk ImportPath{ .singular = .{
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
                        break :blk self.path(field);
                    },
                }
            };

            var next: *ImportPath = &path_component;
            while (next.singular.next) |n| next = n;

            const ptr = self.allocator.create(@TypeOf(last_component)) catch unreachable;
            ptr.* = last_component;

            next.singular.next = ptr;
            return path_component;
        } else return last_component;
    }

    pub fn use_as_clause(self: Self, node: Node) ImportPath {
        const path_field = node.get_field_unchecked("path");
        var path_component = self.path(path_field.*);

        // FIX:
        var last_component: *ImportPath = &path_component;

        while (last_component.singular.next) |n| {
            last_component = n;
        }

        var alias_field = node.get_field_unchecked("alias");
        const alias_identifier = alias_field.extract_type_ref().identifier;

        last_component.singular.alias = alias_identifier.text;
        return path_component;
    }

    pub fn scoped_use_list(self: Self, node: Node) ImportPath {
        const use_list_field = node.get_field_unchecked("list");
        const last_components = self.use_list(use_list_field.*);

        // FIX:
        if (node.get_field("path")) |field| {
            var path_components = self.path(field.*);
            var next = &path_components;

            while (next.singular.next) |n| next = n;

            const ptr = self.allocator.create(@TypeOf(last_components)) catch unreachable;
            ptr.* = last_components;
            next.singular.next = ptr;

            return path_components;
        }
        return last_components;
    }

    pub fn use_wildcard(self: Self, node: Node) ImportPath {
        const children = node.getNamedChildren();
        std.debug.assert(children.items.len == 1);
        const child = children.items[0];
        var pathbuf = PathParser.init(self.allocator, self.parser, child.*).parse();

        pathbuf.append(".*"); // FIX:

        const result: ImportPath = .{
            .singular = .{
                .name = pathbuf,
                .alias = null,
                .next = null,
            },
        };
        return result;
    }

    pub fn use_list(self: Self, node: Node) ImportPath {
        const children = node.getNamedChildren();
        var collect = std.ArrayList(ImportPath).init(self.allocator);

        for (children.items) |child| {
            switch (child.node_type) {
                .use_as_clause => {
                    const path_component = self.use_as_clause(child.*);
                    collect.append(path_component) catch unreachable;
                },
                .use_list => {
                    const path_component = self.use_list(child.*);
                    collect.append(path_component) catch unreachable;
                },

                .scoped_use_list => {
                    collect.append(self.scoped_use_list(child.*)) catch unreachable;
                },
                .use_wildcard => {
                    collect.append(self.use_wildcard(child.*)) catch unreachable;
                },
                .identifier => {
                    const name = child.extract_type_ref();
                    collect.append(ImportPath{ .singular = .{
                        .name = Buf.from(name.identifier.text),
                        .next = null,
                        .alias = null,
                    } }) catch unreachable;
                },
                else => {
                    const x = self.path(child.*);
                    collect.append(x) catch unreachable;
                },
            }
        }

        return ImportPath{ .multiple = collect };
    }

    fn extract_bracketed_type(_: *const @This(), _: *const Parser) void {
        @panic("todo");
    }
};
