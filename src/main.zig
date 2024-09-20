const std = @import("std");
const Scanner = @import("Scanner.zig");

var had_error = false;

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    const progname = args.next().?;

    if (args.next()) |arg| {
        if (args.next()) |_| {
            std.zig.fatal("Usage: {s} [script]", .{progname});
        }
        try runFile(allocator, arg);
    } else {
        try runPrompt(allocator);
    }
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
    try run(allocator, bytes);
    if (had_error) {
        std.process.exit(1);
    }
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    var br = std.io.bufferedReader(stdin);
    const reader = br.reader();
    const stdout = std.io.getStdOut().writer();
    var array_list = try std.ArrayList(u8).initCapacity(allocator, 4096);
    defer array_list.deinit();

    while (true) {
        try stdout.writeAll("> ");
        reader.streamUntilDelimiter(array_list.writer(), '\n', null) catch |e| switch (e) {
            error.EndOfStream => if (array_list.items.len == 0) {
                try stdout.writeAll("\n");
                break;
            },
            else => return e,
        };
        try array_list.append(0);
        const bytes = array_list.items[0 .. array_list.items.len - 1 :0];
        try run(allocator, bytes);
        had_error = false;
        array_list.clearRetainingCapacity();
    }
}

fn run(allocator: std.mem.Allocator, source: [:0]const u8) !void {
    _ = allocator;
    var scanner = Scanner.init(source);
    const stdout = std.io.getStdOut().writer();
    while (scanner.next()) |token| {
        try stdout.print("{}\n", .{token});
    }
}

pub fn err(line: usize, comptime fmt: []const u8, args: anytype) void {
    report(line, "", fmt, args);
}

fn report(line: usize, where: []const u8, comptime fmt: []const u8, args: anytype) void {
    const stderr = std.io.getStdErr().writer();
    stderr.print("[line {d}] Error {s}: ", .{ line, where }) catch {};
    stderr.print(fmt, args) catch {};
    stderr.writeAll("\n") catch {};
    had_error = true;
}
