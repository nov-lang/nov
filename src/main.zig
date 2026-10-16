const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const clap = @import("clap");
const kf = @import("known-folders");
const ic = @import("isocline");
const Axe = @import("axe").Axe(.{
    .scope_format = "@%",
    .mutex = .default,
});

pub const std_options: std.Options = .{
    .logFn = Axe.log,
};

fn usage(io: std.Io, comptime params: anytype) !void {
    var buffer: [4096]u8 = undefined;
    var writer = std.Io.File.stderr().writer(io, &buffer);
    const stderr = &writer.interface;
    try stderr.writeAll("Usage: nov ");
    try clap.usage(stderr, clap.Help, params);
    try stderr.writeAll("\n\nOptions:\n");
    try clap.help(stderr, clap.Help, params, .{
        .description_on_new_line = false,
        .description_indent = 2,
        .spacing_between_parameters = 0,
        .indent = 2,
    });
    try stderr.flush();
}

fn runFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8) !void {
    const bytes = try std.Io.Dir.cwd().readFileAllocOptions(io, path, allocator, .limited(1024 * 1024), .of(u8), 0);
    defer allocator.free(bytes);
    // TODO
    // try vm.interpret(allocator, bytes);
}

fn repl(io: std.Io, allocator: std.mem.Allocator, environ: *const std.process.Environ.Map) !void {
    if (try kf.getPath(io, allocator, environ, .logs)) |state_dir| {
        defer allocator.free(state_dir);
        const history_path = try std.fs.path.joinZ(allocator, &.{ state_dir, "nov_history" });
        defer allocator.free(history_path);
        ic.setHistory(history_path, 1000);
    } else {
        ic.setHistory(null, -1);
    }
    _ = ic.enableColor(true);

    // TODO for prompt do:
    //> while (false) {
    //while> i += 1
    //while> }
    //> print(i)
    while (ic.readline(null)) |bytes| {
        defer ic.free(bytes);
        const line = std.mem.span(bytes);
        // TODO
        _ = line;
        // vm.interpret(allocator, line) catch {};
    }
}

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    try Axe.init(io, &.{}, init.environ_map);
    defer Axe.deinit();

    // TODO
    if (true) {
        const exit_status = try testMain(io, allocator);
        if (exit_status != 0) {
            std.log.err("Test failed with exit status {}", .{exit_status});
        }
        return;
    }

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-v, --version          Display the version and exit.
        \\<file>...
        \\
    );

    const parsers = comptime .{
        .file = clap.parsers.string,
    };

    var diag: clap.Diagnostic = .{};
    var res = clap.parse(clap.Help, &params, parsers, init.minimal.args, .{
        .allocator = allocator,
        .diagnostic = &diag,
    }) catch |err| {
        diag.reportToFile(io, .stderr(), err) catch {};
        usage(io, &params) catch {};
        std.process.exit(1);
    };
    defer res.deinit();

    if (res.args.version != 0) {
        var buffer: [128]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(io, &buffer);
        try stdout.interface.print("nov {f}", .{build_options.version});
        try stdout.interface.flush();
        return;
    }

    if (res.args.help != 0) {
        try usage(io, &params);
        return;
    }

    if (res.positionals[0].len == 0) {
        try repl(io, allocator, init.environ_map);
    } else {
        for (res.positionals[0]) |path| {
            try runFile(io, allocator, path);
            break; // TODO: support multiple files
        }
    }

    if (builtin.mode == .Debug) {
        std.log.debug("clean exit", .{});
    } else {
        // let the kernel free memory
        std.process.exit(0);
    }
}

fn testMain(io: std.Io, allocator: std.mem.Allocator) !u8 {
    const Parser = @import("Parser.zig");
    // const AstGen = @import("AstGen.zig");
    // const Nir = @import("Nir.zig");

    const source =
        \\let x =
        \\    #3 + 1
        \\    + 1
        \\let x: *mut int = 1 + 1
        // \\let main: () = {
        // \\    @println(x)
        // \\}
        //TODO: change parseArrayLiteral to allow that
        // \\let z = [ 1, 2, 3, 4 ].len
        \\
    ;

    std.log.debug(
        \\Running parser with source:
        \\```
        \\{s}
        \\```
    , .{source});

    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);

    // std.debug.print("\nNodes:", .{});
    // for (ast.nodes.items(.tag)) |tag| {
    //     std.debug.print(" {s}", .{@tagName(tag)});
    // }
    // std.debug.print("\n", .{});

    // for (ast.rootDecls()) |stmt| {
    //     for (ast.firstToken(stmt)..ast.lastToken(stmt) + 1) |token| {
    //         std.debug.print("{s} ", .{ast.tokenSlice(@intCast(token))});
    //     }
    //     std.debug.print("\n", .{});
    // }

    if (ast.errors.len > 0) {
        var stderr_buffer: [4096]u8 = undefined;
        var stderr_writer = std.Io.File.stderr().writer(io, &stderr_buffer);
        const stderr = &stderr_writer.interface;
        try stderr.writeAll("\n");
        for (ast.errors) |parse_error| {
            var loc = ast.tokenLocation(0, parse_error.token);
            // TODO
            loc.line += 1;
            loc.column += 1;
            // TODO: use chameleon?
            const bold = "\x1b[1m";
            const red = "\x1b[31m";
            const green = "\x1b[32m";
            const reset = "\x1b[m";
            try stderr.print(bold ++ "{s}:{d}:{d}: " ++ red ++ "error: " ++ reset ++ bold, .{
                "source",
                loc.line,
                loc.column,
            });
            try ast.renderError(parse_error, stderr);
            try stderr.print(reset ++ "\n{s}\n", .{ast.source[loc.line_start..loc.line_end]});
            try stderr.splatByteAll(' ', loc.column - 1);
            try stderr.writeAll(green);
            switch (ast.tokenSlice(parse_error.token).len) {
                1 => try stderr.writeAll("^"),
                else => |len| try stderr.splatByteAll('~', len),
            }
            try stderr.writeAll("\n" ++ reset);
        }
        try stderr.flush();
        return 1;
    }

    // const fmt_source = try ast.render(allocator);
    // defer allocator.free(fmt_source);
    // std.log.debug(
    //     \\Formatted source:
    //     \\```
    //     \\{s}
    //     \\```
    // , .{fmt_source});

    // var nir = try AstGen.generate(allocator, &ast);
    // defer nir.deinit(allocator);
    // for (0..nir.instructions.len) |i| {
    //     const inst = nir.instructions.get(i);
    //     const data_tag = Nir.Inst.data_tags[@intFromEnum(inst.tag)];
    //     switch (data_tag) {
    //         inline else => |tag| {
    //             const data = @field(inst.data, @tagName(tag));
    //             std.debug.print("Instruction {}: " ++ @tagName(tag) ++ ": {}\n", .{ i, data });
    //         },
    //     }
    //     // std.debug.print("Instruction {}: {}\n", .{ i, nir.instructions.get(i) });
    // }

    return 0;
}

test {
    _ = @import("Ast.zig");
    _ = @import("Tokenizer.zig");
    _ = @import("string_literal.zig");
}
