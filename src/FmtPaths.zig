const std = @import("std");

// All stored paths belong to the caller's traversal arena.
const Self = @This();
io: std.Io,
allocator: std.mem.Allocator,
stderr: *std.Io.Writer,
exclusions: []const []const u8,
base: []const u8,
files: std.ArrayList([]const u8) = .empty,
visited: std.StringHashMapUnmanaged(void) = .empty,
failed: bool = false,

pub fn lessThan(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

pub fn excluded(self: *const Self, path: []const u8) bool {
    for (self.exclusions) |prefix| {
        if (std.mem.eql(u8, path, prefix)) return true;
        if (std.mem.startsWith(u8, path, prefix) and
            (std.fs.path.isSep(prefix[prefix.len - 1]) or
                (path.len > prefix.len and std.fs.path.isSep(path[prefix.len])))) return true;
    }
    return false;
}

pub fn collect(self: *Self, inputs: []const []const u8) !void {
    const roots = try self.allocator.alloc([]const u8, inputs.len);
    for (inputs, roots) |input, *root| root.* = try std.fs.path.resolve(self.allocator, &.{ self.base, input });
    std.mem.sort([]const u8, roots, {}, lessThan);
    for (roots) |root| try self.visit(root, true);
    std.mem.sort([]const u8, self.files.items, {}, lessThan);
}

fn report(self: *Self, path: []const u8, err: anyerror) !void {
    self.failed = true;
    try self.stderr.print("{s}: error: {s}\n", .{ path, @errorName(err) });
}

fn visit(self: *Self, path: []const u8, explicit: bool) anyerror!void {
    if (self.excluded(path) or self.visited.contains(path)) return;
    const cwd = std.Io.Dir.cwd();
    const stat = cwd.statFile(self.io, path, .{ .follow_symlinks = false }) catch |err| {
        try self.visited.put(self.allocator, path, {});
        return self.report(path, err);
    };
    if (stat.kind != .directory and !explicit and !std.mem.endsWith(u8, path, ".nov")) return;
    const entry = try self.visited.getOrPut(self.allocator, path);
    if (entry.found_existing) return;
    switch (stat.kind) {
        .directory => {
            // Do not follow directory links, including links swapped in after stat.
            var dir = cwd.openDir(self.io, path, .{ .iterate = true, .follow_symlinks = false }) catch |err| return self.report(path, err);
            defer dir.close(self.io);
            var children: std.ArrayList([]const u8) = .empty;
            var iterator = dir.iterate();
            while (iterator.next(self.io) catch |err| failure: {
                try self.report(path, err);
                break :failure null;
            }) |child| {
                const child_path = try std.fs.path.join(self.allocator, &.{ path, child.name });
                try children.append(self.allocator, child_path);
            }
            std.mem.sort([]const u8, children.items, {}, lessThan);
            for (children.items) |child| try self.visit(child, false);
        },
        .file, .sym_link => {
            if (!explicit and !std.mem.endsWith(u8, path, ".nov")) return;
            const resolved = cwd.realPathFileAlloc(self.io, path, self.allocator) catch |err| return self.report(path, err);
            if (self.excluded(resolved)) return;
            const target = cwd.statFile(self.io, resolved, .{}) catch |err| return self.report(path, err);
            if (target.kind == .directory) return;
            if (target.kind != .file) return self.report(path, error.UnsupportedFileType);
            try self.files.append(self.allocator, path);
        },
        else => if (explicit) try self.report(path, error.UnsupportedFileType),
    }
}
