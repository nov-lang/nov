const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const clap = @import("clap");
const kf = @import("known-folders");
const ic = @import("isocline");

pub const std_options: std.Options = .{
    .logFn = coloredLog,
};

// TODO: move this to a separate file
const Color = enum(u8) {
    black = 30,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    default,
    bright_black = 90,
    bright_red,
    bright_green,
    bright_yellow,
    bright_blue,
    bright_magenta,
    bright_cyan,
    bright_white,

    const csi = "\x1b[";
    const reset = csi ++ "0m";
    const bold = csi ++ "1m";

    fn toSeq(comptime fg: Color) []const u8 {
        return comptime csi ++ std.fmt.digits2(@intFromEnum(fg)) ++ "m";
    }
};

fn coloredLog(
    comptime message_level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const level_txt = comptime switch (message_level) {
        .err => Color.bold ++ Color.red.toSeq() ++ "error" ++ Color.reset,
        .warn => Color.bold ++ Color.yellow.toSeq() ++ "warning" ++ Color.reset,
        .info => Color.bold ++ Color.blue.toSeq() ++ "info" ++ Color.reset,
        .debug => Color.bold ++ Color.cyan.toSeq() ++ "debug" ++ Color.reset,
    };
    const scope_prefix = (if (scope != .default) "@" ++ @tagName(scope) else "") ++ ": ";
    const stderr = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stderr);
    const writer = bw.writer();

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    nosuspend {
        writer.print(level_txt ++ scope_prefix ++ format ++ "\n", args) catch return;
        bw.flush() catch return;
    }
}

// TODO: use this variable for logging
pub var enable_color = false;

fn usage(comptime params: anytype) !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.writeAll("Usage: nov ");
    try clap.usage(stderr, clap.Help, params);
    try stderr.writeAll("\n\nOptions:\n");
    try clap.help(stderr, clap.Help, params, .{
        .description_on_new_line = false,
        .description_indent = 2,
        .spacing_between_parameters = 0,
        .indent = 2,
    });
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const cwd = std.fs.cwd();
    const file = try cwd.openFile(path, .{});
    defer file.close();
    var array_list = try std.ArrayList(u8).initCapacity(allocator, 4096);
    defer array_list.deinit();
    try file.reader().readAllArrayList(&array_list, 1024 * 1024);
    try array_list.append(0);
    const bytes = array_list.items[0 .. array_list.items.len - 1 :0];
    // TODO
    _ = bytes;
    // try vm.interpret(allocator, bytes);
}

fn repl(allocator: std.mem.Allocator) !void {
    if (try kf.getPath(allocator, .state)) |state_dir| {
        defer allocator.free(state_dir);
        const history_path = try std.fs.path.joinZ(allocator, &.{ state_dir, "nov_history" });
        defer allocator.free(history_path);
        ic.setHistory(history_path, 1000);
    } else {
        ic.setHistory(null, -1);
    }

    _ = ic.enableColor(enable_color);

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

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{ .verbose_log = false }) = .{};
    const use_gpa = builtin.mode == .Debug;
    const allocator = if (builtin.os.tag == .wasi)
        std.heap.wasm_allocator
    else if (use_gpa)
        gpa.allocator()
        // We would prefer to use raw libc allocator here, but cannot
        // use it if it won't support the alignment we need.
    else if (@alignOf(std.c.max_align_t) < @max(@alignOf(i128), std.atomic.cache_line))
        std.heap.c_allocator
    else
        std.heap.raw_c_allocator;
    defer if (use_gpa) {
        _ = gpa.deinit();
    };

    // TODO
    if (true) {
        const exit_status = try testMain(allocator);
        if (exit_status != 0) {
            std.log.err("Test failed with exit status {}", .{exit_status});
        }
        return;
    }

    if (builtin.os.tag != .windows and builtin.os.tag != .wasi) {
        const no_color = std.posix.getenv("NO_COLOR");
        enable_color = no_color == null or no_color.?.len == 0;
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
    var res = clap.parse(clap.Help, &params, parsers, .{
        .allocator = allocator,
        .diagnostic = &diag,
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        usage(&params) catch {};
        std.process.exit(1);
    };
    defer res.deinit();

    if (res.args.version != 0) {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("nov {}", .{build_options.version});
        return;
    }

    if (res.args.help != 0) {
        try usage(&params);
        return;
    }

    if (res.positionals.len == 0) {
        try repl(allocator);
    } else {
        for (res.positionals) |path| {
            try runFile(allocator, path);
            break; // TODO: support multiple files
        }
    }
}

fn testMain(allocator: std.mem.Allocator) !u8 {
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
        const stderr = std.io.getStdErr().writer();
        try stderr.writeAll("\n");
        for (ast.errors) |parse_error| {
            var loc = ast.tokenLocation(0, parse_error.token);
            // TODO
            loc.line += 1;
            loc.column += 1;
            try stderr.print(Color.bold ++ "{s}:{d}:{d}: " ++
                Color.red.toSeq() ++ "error: " ++ Color.reset ++ Color.bold, .{
                "source",
                loc.line,
                loc.column,
            });
            try ast.renderError(parse_error, stderr);
            try stderr.print(Color.reset ++ "\n{s}\n", .{ast.source[loc.line_start..loc.line_end]});
            try stderr.writeByteNTimes(' ', loc.column - 1);
            try stderr.writeAll(Color.green.toSeq());
            switch (ast.tokenSlice(parse_error.token).len) {
                1 => try stderr.writeAll("^"),
                else => |len| try stderr.writeByteNTimes('~', len),
            }
            try stderr.writeAll("\n" ++ Color.reset);
        }
        return 1;
    }

    const fmt_source = try ast.render(allocator);
    defer allocator.free(fmt_source);
    std.log.debug(
        \\Formatted source:
        \\```
        \\{s}
        \\```
    , .{fmt_source});

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
    _ = @import("Tokenizer.zig");
    _ = @import("string_literal.zig");
}
