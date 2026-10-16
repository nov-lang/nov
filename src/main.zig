const std = @import("std");
const build_options = @import("build_options");
const clap = @import("clap");
const Parser = @import("Parser.zig");
const Diagnostics = @import("Diagnostics.zig");

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-v, --version          Display the version and exit.
    \\<arg>...
    \\
);

fn usage(writer: *std.Io.Writer) !void {
    try writer.writeAll(
        \\Usage: nov check <file>...
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
    if (args.len < 2 or !std.mem.eql(u8, args[0], "check")) {
        if (args.len != 0) try stderr.print("error: expected 'check' followed by one or more files\n", .{});
        try usage(stderr);
        return 1;
    }

    var failed = false;
    for (args[1..]) |path| {
        const valid = checkFile(init.io, init.gpa, path, stderr) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ path, @errorName(err) });
            failed = true;
            continue;
        };
        failed = failed or !valid;
    }
    return if (failed) 1 else 0;
}

fn checkFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, stderr: *std.Io.Writer) !bool {
    const source = try std.Io.Dir.cwd().readFileAllocOptions(io, path, allocator, .limited(1024 * 1024), .of(u8), 0);
    defer allocator.free(source);
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;
    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);
    try Diagnostics.render(ast, path, stderr);
    return ast.errors.len == 0;
}

test {
    _ = @import("Ast.zig");
    _ = @import("Tokenizer.zig");
    _ = @import("string_literal.zig");
    _ = Diagnostics;
}
