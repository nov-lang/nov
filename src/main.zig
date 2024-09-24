const std = @import("std");
const builtin = @import("builtin");
const clap = @import("clap");
const kf = @import("known-folders");
const ic = @import("isocline");
const Scanner = @import("Scanner.zig");
const Chunk = @import("Chunk.zig");
const VM = @import("vm.zig").VM;
const debug = @import("debug.zig");

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
        try stdout.print("nov 0.0.0", .{});
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
}
