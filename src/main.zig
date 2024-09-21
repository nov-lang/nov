const std = @import("std");
const Scanner = @import("Scanner.zig");
const Chunk = @import("Chunk.zig");
const VM = @import("vm.zig").VM;
const debug = @import("debug.zig");
const ln = @import("linenoise.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var vm = VM.init(allocator);
    defer vm.deinit();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    const progname = args.next().?;

    if (args.next()) |arg| {
        if (args.next()) |_| {
            std.zig.fatal("Usage: {s} [script]", .{progname});
        }
        try runFile(allocator, &vm, arg);
    } else {
        try repl(allocator, &vm);
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
    const history_path = if (std.posix.getenv("XDG_STATE_HOME")) |xdg_state|
        try std.fmt.allocPrintZ(allocator, "{s}/nov-history", .{xdg_state})
    else if (std.posix.getenv("HOME")) |home|
        try std.fmt.allocPrintZ(allocator, "{s}/.local/state/nov-history", .{home})
    else
        try allocator.dupeZ(u8, "./nov-history");
    defer allocator.free(history_path);

    _ = ln.linenoiseHistorySetMaxLen(1000);
    _ = ln.linenoiseHistoryLoad(history_path);

    while (true) {
        if (ln.linenoise("> ")) |bytes| {
            const line = std.mem.span(bytes);
            vm.interpret(allocator, line) catch {};
            _ = ln.linenoiseHistoryAdd(bytes);
            _ = ln.linenoiseHistorySave(history_path);
        } else {
            break;
        }
    }
}
