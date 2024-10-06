const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const clap = @import("clap");
const kf = @import("known-folders");
const ic = @import("isocline");
const Scanner = @import("Scanner.zig");
const Chunk = @import("Chunk.zig");
const VM = @import("vm.zig").VM;
const debug = @import("debug.zig");

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
    comptime scope: @Type(.EnumLiteral),
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

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{ .verbose_log = false }) = .{};
    defer if (builtin.mode == .Debug) std.debug.assert(gpa.deinit() == .ok);
    const allocator = if (builtin.mode == .Debug) gpa.allocator() else std.heap.c_allocator;

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

    var vm = VM.init(allocator);
    defer vm.deinit();

    if (res.positionals.len == 0) {
        try repl(allocator, &vm);
    } else {
        for (res.positionals) |path| {
            try runFile(allocator, &vm, path);
            break; // TODO: support multiple files
        }
    }
}

fn runFile(allocator: std.mem.Allocator, vm: *VM, path: []const u8) !void {
    const cwd = std.fs.cwd();
    const file = try cwd.openFile(path, .{});
    defer file.close();
    var array_list = try std.ArrayList(u8).initCapacity(allocator, 4096);
    defer array_list.deinit();
    try file.reader().readAllArrayList(&array_list, 1024 * 1024);
    try array_list.append(0);
    const bytes = array_list.items[0 .. array_list.items.len - 1 :0];
    try vm.interpret(allocator, bytes);
}

fn repl(allocator: std.mem.Allocator, vm: *VM) !void {
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
        vm.interpret(allocator, line) catch {};
    }
}

test {
    // std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("Tokenizer.zig"));
    std.testing.refAllDecls(@import("Value.zig"));
}
