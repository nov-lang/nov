const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("Chunk.zig");
const Value = @import("value.zig").Value;
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");

pub const Error = std.mem.Allocator.Error || error{ CompileError, RuntimeError };

pub const VM = struct {
    chunk: *Chunk,
    idx: usize, // instruction pointer
    stack: std.BoundedArray(Value, stack_max), // make the stack dynamic?
    arena: std.heap.ArenaAllocator, // TODO: remove

    const stack_max = 256;

    pub fn init(allocator: std.mem.Allocator) VM {
        return .{
            .chunk = undefined,
            .idx = 0,
            .stack = .{},
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.arena.deinit();
    }

    pub fn interpret(self: *VM, allocator: std.mem.Allocator, source: [:0]const u8) Error!void {
        var chunk = Chunk.init(allocator);
        defer chunk.deinit();
        if (!compiler.compile(source, &chunk)) {
            return error.CompileError;
        }
        self.chunk = &chunk;
        self.idx = 0;
        try self.run();
    }

    pub fn push(self: *VM, value: Value) Error!void {
        self.stack.append(value) catch return error.RuntimeError;
    }

    pub fn pop(self: *VM) Error!Value {
        return self.stack.popOrNull() orelse return error.RuntimeError;
    }

    // fn peek(self: *VM, distance: usize) Value {
    //     return self.stack.items[self.stack.len - 1 - distance];
    // }

    fn run(self: *VM) Error!void {
        while (true) {
            if (builtin.mode == .Debug) {
                debug.print("          ", .{});
                var it = std.mem.reverseIterator(self.stack.constSlice());
                while (it.next()) |value| {
                    debug.print("[ {} ]", .{value});
                }
                debug.print("\n", .{});
                _ = debug.disassembleInstruction(self.chunk, self.idx);
            }
            const instruction: Chunk.OpCode = @enumFromInt(self.chunk.code.items[self.idx]);
            self.idx += 1;
            switch (instruction) {
                .constant => {
                    const constant = self.chunk.constants.items[self.chunk.code.items[self.idx]];
                    self.idx += 1;
                    try self.push(constant);
                },
                .equal => {
                    const b = try self.pop();
                    const a = try self.pop();
                    if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
                        try self.push(.{ .bool = false });
                    } else {
                        try self.push(.{ .bool = switch (a) {
                            .number => a.number == b.number,
                            .bool => a.bool == b.bool,
                            .string => std.mem.eql(u8, a.string, b.string),
                        } });
                    }
                },
                .greater => {
                    const b = (try self.pop()).number;
                    const a = (try self.pop()).number;
                    try self.push(.{ .bool = a > b });
                },
                .less => {
                    const b = (try self.pop()).number;
                    const a = (try self.pop()).number;
                    try self.push(.{ .bool = a < b });
                },
                .@"or" => {
                    const b = (try self.pop()).bool;
                    const a = (try self.pop()).bool;
                    try self.push(.{ .bool = a or b });
                },
                .add => {
                    const b = try self.pop();
                    const a = try self.pop();
                    if (a == .number and b == .number) {
                        try self.push(.{ .number = a.number + b.number });
                    } else if (a == .string and b == .string) {
                        const len = a.string.len + b.string.len;
                        const buf = try self.arena.allocator().alloc(u8, len);
                        @memcpy(buf[0..a.string.len], a.string);
                        @memcpy(buf[a.string.len..], b.string);
                        try self.push(.{ .string = buf });
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings", .{});
                        return error.RuntimeError;
                    }
                },
                .sub => {
                    const b = (try self.pop()).number;
                    const a = (try self.pop()).number;
                    try self.push(.{ .number = a - b });
                },
                .mul => {
                    const b = (try self.pop()).number;
                    const a = (try self.pop()).number;
                    try self.push(.{ .number = a * b });
                },
                .div => {
                    const b = (try self.pop()).number;
                    const a = (try self.pop()).number;
                    try self.push(.{ .number = a / b });
                },
                .not => {
                    const value = (try self.pop()).bool;
                    try self.push(.{ .bool = !value });
                },
                .negation => {
                    const value = (try self.pop()).number;
                    // if (value != .number) {
                    //     self.runtimeError("Operand must be a number", .{});
                    //     return error.RuntimeError;
                    // }
                    try self.push(.{ .number = -value });
                },
                .@"return" => {
                    debug.print("{}\n", .{try self.pop()});
                    return;
                },
            }
        }
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) void {
        const stderr = std.io.getStdErr().writer();
        stderr.print("Runtime Error: " ++ fmt ++ "\n", args) catch {};
        const line = self.chunk.lines.items[self.idx - 1];
        stderr.print("[line {d}] in script\n", .{line}) catch {};
        self.stack.resize(0) catch unreachable;
    }
};
