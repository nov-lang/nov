const std = @import("std");
const builtin = @import("builtin");

pub const Discovery = struct {
    cc: ?[]const u8 = null,
    nov_cc: ?[]const u8 = null,
    env_cc: ?[]const u8 = null,
    fallback_cc: []const u8 = "cc",
    fallback_zig: []const u8 = "zig",
};

/// Values are executable paths, never shell command lines. Only a missing
/// executable advances discovery; a broken compiler must not be hidden.
pub fn compile(io: std.Io, discovery: Discovery, source: []const u8, output: []const u8, stderr: *std.Io.Writer) !bool {
    const candidates = [_]?[]const u8{ discovery.cc, discovery.nov_cc, discovery.env_cc, discovery.fallback_cc, discovery.fallback_zig };
    var tried = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    defer tried.deinit();
    for (candidates, 0..) |candidate, index| {
        const executable = candidate orelse continue;
        if (executable.len == 0) continue;
        const zig = index == candidates.len - 1;
        if (tried.written().len != 0) try tried.writer.writeAll(", ");
        try tried.writer.print("'{s}{s}'", .{ executable, if (zig) " cc" else "" });
        const argv = [_][]const u8{ executable, "cc", "-std=c11", "-O2", source, "-o", output };
        const plain_argv = [_][]const u8{ executable, "-std=c11", "-O2", source, "-o", output };
        try stderr.flush();
        var child = std.process.spawn(io, .{
            .argv = if (zig) &argv else &plain_argv,
            .stdin = .ignore,
            // Keep compiler chatter off the program's stdout.
            .stdout = .{ .file = std.Io.File.stderr() },
        }) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => {
                try stderr.print("error: cannot start C compiler '{s}': {s}\n", .{ executable, @errorName(err) });
                return false;
            },
        };
        defer child.kill(io);
        const term = try child.wait(io);
        if (term == .exited and term.exited == 0) return true;
        try stderr.print("error: C compiler '{s}' failed ({s}", .{ executable, @tagName(term) });
        if (term == .exited) try stderr.print(" {d}", .{term.exited});
        try stderr.writeAll(")\n");
        return false;
    }
    try stderr.print("error: no C compiler found; tried: {s}\nuse 'nov emit-c <file>' to emit C without a compiler\n", .{tried.written()});
    return false;
}

pub fn run(io: std.Io, executable: []const u8, stderr: *std.Io.Writer) !u8 {
    try stderr.flush();
    // Workspace paths contain a directory component, so PATH is not searched.
    // Zig 0.16's threaded backend does not implement spawnPath yet.
    var child = try std.process.spawn(io, .{ .argv = &.{executable} });
    defer child.kill(io);
    const term = try child.wait(io);
    return switch (term) {
        .exited => |code| code,
        .signal => |signal| blk: {
            try stderr.print("error: program terminated by signal {d}\n", .{@intFromEnum(signal)});
            break :blk @intCast(@min(255, 128 + @as(u32, @intFromEnum(signal))));
        },
        else => blk: {
            try stderr.print("error: program terminated abnormally ({s})\n", .{@tagName(term)});
            break :blk 1;
        },
    };
}

pub const Workspace = struct {
    allocator: std.mem.Allocator,
    path: []const u8,

    pub fn init(io: std.Io, allocator: std.mem.Allocator, root: []const u8) !Workspace {
        for (0..16) |_| {
            var random: [16]u8 = undefined;
            io.random(&random);
            var name: [36]u8 = undefined;
            name[0..4].* = "nov-".*;
            name[4..].* = std.fmt.bytesToHex(random, .lower);
            const path = try std.fs.path.join(allocator, &.{ root, &name });
            const permissions: std.Io.Dir.Permissions = if (builtin.os.tag == .windows) .default_dir else .fromMode(0o700);
            std.Io.Dir.cwd().createDir(io, path, permissions) catch |err| {
                allocator.free(path);
                if (err == error.PathAlreadyExists) continue;
                return err;
            };
            return .{ .allocator = allocator, .path = path };
        }
        return error.PathAlreadyExists;
    }

    pub fn deinit(self: Workspace, io: std.Io) void {
        std.Io.Dir.cwd().deleteTree(io, self.path) catch {};
        self.allocator.free(self.path);
    }

    pub fn file(self: Workspace, name: []const u8) ![]const u8 {
        return std.fs.path.join(self.allocator, &.{ self.path, name });
    }
};

pub fn defaultOutput(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}{s}", .{ std.fs.path.stem(source), if (builtin.os.tag == .windows) ".exe" else "" });
}

fn fakeCompiler(dir: std.Io.Dir, name: []const u8, script: []const u8) !void {
    const io = std.testing.io;
    try dir.writeFile(io, .{ .sub_path = name, .data = script });
    try dir.setFilePermissions(io, name, .fromMode(0o700), .{});
}

test "compiler discovery uses literal executables in approved order and exact flags" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const good = try std.fs.path.join(allocator, &.{ root, "compiler with spaces;literal" });
    defer allocator.free(good);
    const missing = try std.fs.path.join(allocator, &.{ root, "absent" });
    defer allocator.free(missing);
    try fakeCompiler(tmp.dir, "compiler with spaces;literal", "#!/bin/sh\n[ \"$#\" = 5 ] && [ \"$1\" = -std=c11 ] && [ \"$2\" = -O2 ] && [ \"$3\" = 'source with spaces.c' ] && [ \"$4\" = -o ] && [ \"$5\" = 'output;literal' ]\n");
    for (0..4) |selected| {
        const candidates: [4][]const u8 = blk: {
            var values = [_][]const u8{missing} ** 4;
            values[selected] = good;
            break :blk values;
        };
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expect(try compile(io, .{ .cc = candidates[0], .nov_cc = candidates[1], .env_cc = candidates[2], .fallback_cc = candidates[3], .fallback_zig = missing }, "source with spaces.c", "output;literal", &diagnostics.writer));
        try std.testing.expectEqualStrings("", diagnostics.written());
    }
    try fakeCompiler(tmp.dir, "zig", "#!/bin/sh\n[ \"$#\" = 6 ] && [ \"$1\" = cc ] && [ \"$2\" = -std=c11 ] && [ \"$3\" = -O2 ]\n");
    const zig = try std.fs.path.join(allocator, &.{ root, "zig" });
    defer allocator.free(zig);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(try compile(io, .{ .cc = "", .nov_cc = "", .fallback_cc = missing, .fallback_zig = zig }, "source", "output", &diagnostics.writer));
}

test "missing compiler lists all attempts and suggests emit-c" {
    const allocator = std.testing.allocator;
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(!try compile(std.testing.io, .{ .cc = "/nov-missing/explicit", .nov_cc = "/nov-missing/nov-env", .env_cc = "/nov-missing/cc-env", .fallback_cc = "/nov-missing/cc", .fallback_zig = "/nov-missing/zig" }, "source", "output", &diagnostics.writer));
    try std.testing.expectEqualStrings("error: no C compiler found; tried: '/nov-missing/explicit', '/nov-missing/nov-env', '/nov-missing/cc-env', '/nov-missing/cc', '/nov-missing/zig cc'\nuse 'nov emit-c <file>' to emit C without a compiler\n", diagnostics.written());
}

test "compiler failure stops discovery and run preserves nonzero exit status" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try fakeCompiler(tmp.dir, "failure", "#!/bin/sh\nexit 37\n");
    const path = try tmp.dir.realPathFileAlloc(io, "failure", allocator);
    defer allocator.free(path);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(!try compile(io, .{ .cc = path }, "source", "output", &diagnostics.writer));
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "failed (exited 37)") != null);
    try std.testing.expectEqual(@as(u8, 37), try run(io, path, &diagnostics.writer));
}

test "nonexecutable compiler stops discovery rather than silently falling back" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "not-executable", .data = "#!/bin/sh\nexit 0\n" });
    try tmp.dir.setFilePermissions(io, "not-executable", .fromMode(0o600), .{});
    const path = try tmp.dir.realPathFileAlloc(io, "not-executable", allocator);
    defer allocator.free(path);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expect(!try compile(io, .{ .cc = path }, "source", "output", &diagnostics.writer));
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "cannot start C compiler") != null);
    try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "no C compiler found") == null);
}

test "default output uses the source basename and native executable suffix" {
    const output = try defaultOutput(std.testing.allocator, "directory/program.nov");
    defer std.testing.allocator.free(output);
    try std.testing.expectEqualStrings(if (builtin.os.tag == .windows) "program.exe" else "program", output);
}

test "workspace removes compiler artifacts on success and failure" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const workspace = try Workspace.init(io, allocator, root);
    const path = try workspace.file("program.c");
    defer allocator.free(path);
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = "source" });
    workspace.deinit(io);
    try std.testing.expectError(error.FileNotFound, std.Io.Dir.cwd().openFile(io, path, .{}));
}
