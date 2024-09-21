const std = @import("std");
const Scanner = @import("Scanner.zig");
const Chunk = @import("Chunk.zig");
const VM = @import("vm.zig").VM;
const debug = @import("debug.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var vm = VM.init(allocator);
    defer vm.deinit();

    // var chunk = Chunk.init(allocator);
    // defer chunk.deinit();
    // var constant = try chunk.addConstant(1.2);
    // try chunk.write(@intFromEnum(Chunk.OpCode.constant), 123);
    // try chunk.write(@intCast(constant), 123);

    // constant = try chunk.addConstant(3.4);
    // try chunk.write(@intFromEnum(Chunk.OpCode.constant), 123);
    // try chunk.write(@intCast(constant), 123);

    // try chunk.write(@intFromEnum(Chunk.OpCode.add), 123);

    // constant = try chunk.addConstant(5.6);
    // try chunk.write(@intFromEnum(Chunk.OpCode.constant), 123);
    // try chunk.write(@intCast(constant), 123);
    // try chunk.write(@intFromEnum(Chunk.OpCode.negation), 123);

    // try chunk.write(@intFromEnum(Chunk.OpCode.@"return"), 123);
    // debug.disassembleChunk(&chunk, "test chunk");
    // try vm.interpret(&chunk);

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
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var array_list = try std.ArrayList(u8).initCapacity(allocator, 4096);
    defer array_list.deinit();

    while (true) {
        try stdout.writeAll("> ");
        stdin.streamUntilDelimiter(array_list.writer(), '\n', null) catch |err| switch (err) {
            error.EndOfStream => if (array_list.items.len == 0) {
                try stdout.writeAll("\n");
                break;
            },
            else => return err,
        };
        try array_list.append(0);
        const bytes = array_list.items[0 .. array_list.items.len - 1 :0];
        vm.interpret(allocator, bytes) catch {};
        array_list.clearRetainingCapacity();
    }
}

// fn run(allocator: std.mem.Allocator, source: [:0]const u8) !void {
//     _ = allocator;
//     var scanner = Scanner.init(source);
//     const stdout = std.io.getStdOut().writer();
//     while (true) {
//         const token = scanner.next();
//         try stdout.print("{}\n", .{token});
//         if (token.tag == .eof) {
//             break;
//         }
//     }
// }

// pub fn err(line: usize, comptime fmt: []const u8, args: anytype) void {
//     report(line, "", fmt, args);
// }

// fn report(line: usize, where: []const u8, comptime fmt: []const u8, args: anytype) void {
//     const stderr = std.io.getStdErr().writer();
//     stderr.print("[line {d}] Error {s}: ", .{ line, where }) catch {};
//     stderr.print(fmt, args) catch {};
//     stderr.writeAll("\n") catch {};
// }
