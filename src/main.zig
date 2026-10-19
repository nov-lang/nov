const std = @import("std");
const build_options = @import("build_options");
const clap = @import("clap");
const Parser = @import("Parser.zig");
const Diagnostics = @import("Diagnostics.zig");
const Formatter = @import("Formatter.zig");

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-v, --version          Display the version and exit.
    \\--check                Check formatting without changing files (fmt only).
    \\<arg>...
    \\
);

fn usage(writer: *std.Io.Writer) !void {
    try writer.writeAll(
        \\Usage: nov check <file>...
        \\       nov fmt [--check] <file>...
        \\
        \\Parse Nov files and report syntax errors. Compilation is not implemented yet.
        \\
        \\Options:
        \\
    );
    try clap.help(writer, clap.Help, &params, .{
        .description_on_new_line = false,
        .description_indent = 2,
        .spacing_between_parameters = 0,
        .indent = 2,
    });
}

pub fn main(init: std.process.Init) !u8 {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_file = std.Io.File.stderr().writer(init.io, &stderr_buffer);
    const stderr = &stderr_file.interface;
    defer stderr.flush() catch {};

    var diag: clap.Diagnostic = .{};
    var res = clap.parse(clap.Help, &params, .{ .arg = clap.parsers.string }, init.minimal.args, .{
        .allocator = init.gpa,
        .diagnostic = &diag,
    }) catch |err| {
        try diag.report(stderr, err);
        try usage(stderr);
        return 1;
    };
    defer res.deinit();

    if (res.args.version != 0) {
        var buffer: [128]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        try stdout.interface.print("nov {f}\n", .{build_options.version});
        try stdout.interface.flush();
        return 0;
    }
    if (res.args.help != 0) {
        try usage(stderr);
        return 0;
    }

    const args = res.positionals[0];
    const is_fmt = args.len != 0 and std.mem.eql(u8, args[0], "fmt");
    if (res.args.check != 0 and !is_fmt) {
        try stderr.writeAll("error: --check requires the fmt command\n");
        return 1;
    }
    if (args.len < 2 or (!is_fmt and !std.mem.eql(u8, args[0], "check"))) {
        if (args.len != 0) try stderr.writeAll("error: expected 'check' or 'fmt' followed by one or more files\n");
        try usage(stderr);
        return 1;
    }

    var failed = false;
    for (args[1..]) |path| {
        const valid = processFile(init.io, init.gpa, path, stderr, is_fmt, res.args.check != 0) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ path, @errorName(err) });
            failed = true;
            continue;
        };
        failed = failed or !valid;
    }
    return if (failed) 1 else 0;
}

fn processFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, stderr: *std.Io.Writer, is_fmt: bool, check_only: bool) !bool {
    const cwd = std.Io.Dir.cwd();
    // Resolve symlinks so atomic replacement updates the target, not the link.
    const resolved = try cwd.realPathFileAlloc(io, path, allocator);
    defer allocator.free(resolved);
    const source = try cwd.readFileAllocOptions(io, resolved, allocator, .limited(1024 * 1024), .of(u8), 0);
    defer allocator.free(source);
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;
    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);
    try Diagnostics.render(ast, path, stderr);
    if (ast.errors.len != 0) return false;
    if (!is_fmt) return true;

    const formatted = Formatter.format(allocator, source) catch |err| switch (err) {
        error.UnsupportedSyntax => {
            try stderr.print("{s}: error: formatting this syntax is not supported yet; file unchanged\n", .{path});
            return false;
        },
        else => return err,
    };
    defer allocator.free(formatted);
    if (std.mem.eql(u8, source, formatted)) return true;
    if (check_only) {
        try stderr.print("{s}: error: file is not formatted\n", .{path});
        return false;
    }

    try replaceFormattedFile(io, allocator, resolved, source, formatted);
    return true;
}

fn replaceFormattedFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, original: []const u8, formatted: []const u8) !void {
    const cwd = std.Io.Dir.cwd();
    const file = try cwd.openFile(io, path, .{});
    const stat = file.stat(io) catch |err| {
        file.close(io);
        return err;
    };
    file.close(io);
    var atomic = try cwd.createFileAtomic(io, path, .{ .replace = true, .permissions = stat.permissions });
    defer atomic.deinit(io);
    try atomic.file.setPermissions(io, stat.permissions);
    var buffer: [4096]u8 = undefined;
    var writer = atomic.file.writer(io, &buffer);
    try writer.interface.writeAll(formatted);
    try writer.interface.flush();
    // Detect edits made while parsing/rendering or preparing the replacement.
    // There is no portable compare-and-swap rename; a final rename race remains.
    const current = try cwd.readFileAlloc(io, path, allocator, .limited(1024 * 1024));
    defer allocator.free(current);
    if (!std.mem.eql(u8, original, current)) return error.SourceChanged;
    try atomic.replace(io);
}

test "formatting refuses to overwrite a changed source" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const updated = "; edited concurrently\nlet x = 2\n";
    try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = updated });
    const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
    defer allocator.free(path);
    try std.testing.expectError(error.SourceChanged, replaceFormattedFile(io, allocator, path, "let x=1\n", "let x = 1\n"));
    const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(updated, actual);
}

test "format files atomically and leave rejected input unchanged" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cases = [_]struct { source: []const u8, check_only: bool = false, valid: bool, expected: []const u8 }{
        .{ .source = "let x=1+2\n", .valid = true, .expected = "let x = 1 + 2\n" },
        .{ .source = "let x=1+2\n", .check_only = true, .valid = false, .expected = "let x=1+2\n" },
        .{ .source = "let x =\n", .valid = false, .expected = "let x =\n" },
        .{ .source = "let x = foo(1)\n", .valid = false, .expected = "let x = foo(1)\n" },
    };
    for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
        defer allocator.free(path);
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(case.valid, try processFile(io, allocator, path, &diagnostics.writer, true, case.check_only));
        const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
        defer allocator.free(actual);
        try std.testing.expectEqualStrings(case.expected, actual);
    }
}

test {
    _ = @import("Ast.zig");
    _ = @import("Ir.zig");
    _ = @import("Module.zig");
    _ = @import("Dependencies.zig");
    _ = @import("TypeResolver.zig");
    _ = @import("Sema.zig");
    _ = @import("Literal.zig");
    _ = @import("BodyChecker.zig");
    _ = @import("Lowering.zig");
    _ = @import("Tokenizer.zig");
    _ = @import("string_literal.zig");
    _ = Diagnostics;
    _ = Formatter;
}
