const std = @import("std");
const builtin = @import("builtin");
const Chunk = @import("Chunk.zig");
const v = @import("value.zig");
const Value = v.Value;
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");
pub const Error = error{ CompileError, RuntimeError, StackOverflow } || std.mem.Allocator.Error;

const Oceania = @This();

chunk: *Chunk,
ip: usize, // instruction pointer
stack: std.BoundedArray(Value, stack_size), // make the stack dynamic?
arena: std.heap.ArenaAllocator, // TODO: remove
globals: std.StringHashMapUnmanaged(Value),
allocator: std.mem.Allocator,

const stack_size = 16 * 1024;

pub fn init(allocator: std.mem.Allocator) Oceania {
    return .{
        .chunk = undefined,
        .ip = 0,
        .stack = .{},
        .globals = .{},
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
}

pub fn deinit(self: *Oceania) void {
    self.arena.deinit();

    var it = self.globals.iterator();
    while (it.next()) |entry| {
        self.allocator.free(entry.key_ptr.*);
        entry.value_ptr.*.destroy(self.allocator);
    }
    self.globals.deinit(self.allocator);
}

pub fn interpret(self: *Oceania, allocator: std.mem.Allocator, source: [:0]const u8) Error!void {
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    if (!compiler.compile(self.arena.allocator(), source, &chunk)) {
        return error.CompileError;
    }
    self.chunk = &chunk;
    self.ip = 0;
    try self.run();
}

pub fn push(self: *Oceania, value: Value) Error!void {
    self.stack.append(value) catch return error.RuntimeError;
}

pub fn pop(self: *Oceania) Error!Value {
    return self.stack.popOrNull() orelse error.RuntimeError;
}

fn peek(self: *Oceania, distance: usize) Value {
    return self.stack.get(self.stack.len - 1 - distance);
}

inline fn readConstant(self: *Oceania, arg: u24) Value {
    return self.chunk.constants.items[arg];
}

// debugLoop
fn run(self: *Oceania) Error!void {
    while (true) {
        if (builtin.mode == .Debug) {
            debug.print("          ", .{});
            var it = std.mem.reverseIterator(self.stack.constSlice());
            while (it.next()) |value| {
                debug.print("[ {} ]", .{value});
            }
            debug.print("\n", .{});
            _ = debug.disassembleInstruction(self.chunk, self.ip);
        }
        const code = self.chunk.code.items[self.ip];
        self.ip += 1;
        const instruction: Chunk.OpCode = @enumFromInt(@as(u8, @truncate(code)));
        switch (instruction) {
            .constant => {
                const arg: u24 = @intCast(code >> 8);
                try self.push(self.readConstant(arg));
            },
            .equal => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(Value.create(a.eql(b)));
            },
            .greater => {
                const b = (try self.pop()).number;
                const a = (try self.pop()).number;
                try self.push(Value.create(a > b));
            },
            .less => {
                const b = (try self.pop()).number;
                const a = (try self.pop()).number;
                try self.push(Value.create(a < b));
            },
            .add => {
                const b = try self.pop();
                const a = try self.pop();
                if (a == .number and b == .number) {
                    try self.push(Value.create(a.number + b.number));
                } else if (a == .string and b == .string) {
                    const allocator = self.arena.allocator();
                    const len = a.string.data.len + b.string.data.len;
                    const buf = try allocator.alloc(u8, len);
                    errdefer allocator.free(buf);
                    @memcpy(buf[0..a.string.data.len], a.string.data);
                    @memcpy(buf[a.string.data.len..], b.string.data);
                    try self.push(try Value.createObject(allocator, buf));
                } else {
                    return self.runtimeError("Operands must be two numbers or two strings", .{});
                }
            },
            .sub => {
                const b = (try self.pop()).number;
                const a = (try self.pop()).number;
                try self.push(Value.create(a - b));
            },
            .mul => {
                const b = try self.pop();
                const a = try self.pop();
                if (a == .number and b == .number) {
                    try self.push(Value.create(a.number * b.number));
                } else if ((a == .string and b == .number) or (a == .number and b == .string)) {
                    const allocator = self.arena.allocator();
                    const n: usize, const s = if (a == .number)
                        .{ @intFromFloat(a.number), b.string.data }
                    else
                        .{ @intFromFloat(b.number), a.string.data };
                    const buf = try allocator.alloc(u8, s.len * n);
                    errdefer allocator.free(buf);
                    for (0..n) |i| {
                        @memcpy(buf[i * s.len .. (i + 1) * s.len], s);
                    }
                    try self.push(try Value.createObject(allocator, buf));
                } else {
                    return self.runtimeError("Operands must be two numbers or a number and a string", .{});
                }
            },
            .div => {
                const b = (try self.pop()).number;
                const a = (try self.pop()).number;
                try self.push(Value.create(a / b));
            },
            .mod => {
                const b = (try self.pop()).number;
                const a = (try self.pop()).number;
                try self.push(Value.create(@mod(a, b)));
            },
            .not => {
                const value = (try self.pop()).bool;
                try self.push(Value.create(!value));
            },
            .negation => {
                const value = (try self.pop()).number;
                try self.push(Value.create(-value));
            },
            .pop => {
                _ = try self.pop();
            },
            .get_local => {
                const slot: u24 = @intCast(code >> 8);
                try self.push(self.stack.get(slot));
            },
            // TODO: handle string for get/set local
            .set_local => {
                const slot: u24 = @intCast(code >> 8);
                self.stack.set(slot, self.peek(0));
            },
            .get_global => {
                const arg: u24 = @intCast(code >> 8);
                const name = self.readConstant(arg).string.data;
                if (self.globals.get(name)) |value| {
                    try self.push(value);
                } else {
                    return self.runtimeError("Undefined variable '{s}'", .{name});
                }
            },
            .set_global => {
                const arg: u24 = @intCast(code >> 8);
                const name = self.readConstant(arg).string.data;
                const new_value = try Value.dupe(self.peek(0), self.allocator);
                errdefer new_value.destroy(self.allocator);

                if (try self.globals.fetchPut(self.allocator, name, new_value)) |old_kv| {
                    const old_value = old_kv.value;
                    if (std.meta.activeTag(new_value) != std.meta.activeTag(old_value)) {
                        self.globals.put(self.allocator, name, old_value) catch {};
                        return self.runtimeError(
                            "Cannot assign a value of type '{s}' to a variable of type '{s}'",
                            .{ @tagName(new_value), @tagName(old_value) },
                        );
                    }
                    old_value.destroy(self.allocator);
                    // TODO: freeing old value produce crash in debug build for
                    // let x = "aaa"
                    // x = x
                } else {
                    std.debug.assert(self.globals.remove(name) == true);
                    return self.runtimeError("Undefined variable '{s}'", .{name});
                }
            },
            .define_global => {
                const arg: u24 = @intCast(code >> 8);
                const name = try self.allocator.dupe(u8, self.readConstant(arg).string.data);
                errdefer self.allocator.free(name);
                const gop = try self.globals.getOrPut(self.allocator, name);
                if (gop.found_existing) {
                    return self.runtimeError("Variable '{s}' already defined", .{name});
                } else {
                    const value = try self.pop();
                    gop.value_ptr.* = try Value.dupe(value, self.allocator);
                }
            },
            // TODO: move to stdlib -> when import and call are implemented
            .print => {
                const stdout = std.io.getStdOut().writer();
                stdout.print("{}\n", .{try self.pop()}) catch {};
            },
            .jump => {
                const offset: u24 = @intCast(code >> 8);
                self.ip += offset;
            },
            .jump_if_false => {
                const offset: u24 = @intCast(code >> 8);
                if (!self.peek(0).bool) {
                    self.ip += offset;
                }
            },
            .loop => {
                const offset: u24 = @intCast(code >> 8);
                self.ip -= offset;
            },
            .@"return" => {
                return;
            },
        }
    }
}

// TODO: releaseLoop with labeled switch/continue

fn runtimeError(self: *Oceania, comptime fmt: []const u8, args: anytype) Error {
    const stderr = std.io.getStdErr().writer();
    stderr.print("Runtime Error: " ++ fmt ++ "\n", args) catch {};
    const line = self.chunk.lines.items[self.ip - 1];
    stderr.print("[line {d}] in script\n", .{line}) catch {};
    self.stack.resize(0) catch unreachable;
    return error.RuntimeError;
}

fn fadd(a: Value, b: Value) Value {
    return .{ .float = a.float + b.float };
}

// fneg is fsub(0, a)
fn fsub(a: Value, b: Value) Value {
    return .{ .float = a.float - b.float };
}

fn fmul(a: Value, b: Value) Value {
    return .{ .float = a.float * b.float };
}

fn fdiv(a: Value, b: Value) Value {
    return .{ .float = a.float / b.float };
}

fn iadd(a: Value, b: Value) Value {
    return .{ .int = a.int +% b.int };
}

fn isub(a: Value, b: Value) Value {
    return .{ .int = a.int -% b.int };
}

fn imul(a: Value, b: Value) Value {
    return .{ .int = a.int *% b.int };
}

fn idiv(a: Value, b: Value) Value {
    return .{ .int = @divTrunc(a.int, b.int) };
}

fn udiv(a: Value, b: Value) Value {
    return .{ .int = @bitCast(@as(u63, @bitCast(a.int)) / @as(u63, @bitCast(b.int))) };
}

fn irem(a: Value, b: Value) Value {
    return .{ .int = @rem(a.int, b.int) };
}

fn urem(a: Value, b: Value) Value {
    return .{ .int = @bitCast(@as(u63, @bitCast(a.int)) % @as(u63, @bitCast(b.int))) };
}
