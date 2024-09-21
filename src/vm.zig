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
    globals: std.StringHashMapUnmanaged(Value),
    allocator: std.mem.Allocator,

    const stack_max = 256;

    pub fn init(allocator: std.mem.Allocator) VM {
        return .{
            .chunk = undefined,
            .idx = 0,
            .stack = .{},
            .globals = .{},
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *VM) void {
        self.arena.deinit();

        var it = self.globals.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            switch (entry.value_ptr.*) {
                .number, .bool => {},
                .string => self.allocator.free(entry.value_ptr.string),
            }
        }
        self.globals.deinit(self.allocator);
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

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.get(self.stack.len - 1 - distance);
    }

    inline fn readConstant(self: *VM) Value {
        const value = self.chunk.constants.items[self.chunk.code.items[self.idx]];
        self.idx += 1;
        return value;
    }

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
                    try self.push(self.readConstant());
                },
                .equal => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .bool = a.eql(b) });
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
                .add => {
                    const b = try self.pop();
                    const a = try self.pop();
                    if (a == .number and b == .number) {
                        try self.push(.{ .number = a.number + b.number });
                    } else if (a == .string and b == .string) {
                        const len = a.string.len + b.string.len;
                        const buf = try self.arena.allocator().alloc(u8, len);
                        errdefer self.arena.allocator().free(buf);
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
                    const b = try self.pop();
                    const a = try self.pop();
                    if (a == .number and b == .number) {
                        try self.push(.{ .number = a.number * b.number });
                    } else if ((a == .string and b == .number) or (a == .number and b == .string)) {
                        const n: usize, const s = if (a == .number)
                            .{ @intFromFloat(a.number), b.string }
                        else
                            .{ @intFromFloat(b.number), a.string };
                        const buf = try self.arena.allocator().alloc(u8, s.len * n);
                        errdefer self.arena.allocator().free(buf);
                        for (0..n) |i| {
                            @memcpy(buf[i * s.len .. (i + 1) * s.len], s);
                        }
                        try self.push(.{ .string = buf });
                    } else {
                        self.runtimeError("Operands must be two numbers or a number and a string", .{});
                        return error.RuntimeError;
                    }
                },
                .div => {
                    const b = (try self.pop()).number;
                    const a = (try self.pop()).number;
                    try self.push(.{ .number = a / b });
                },
                .mod => {
                    const b = (try self.pop()).number;
                    const a = (try self.pop()).number;
                    try self.push(.{ .number = @mod(a, b) });
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
                .pop => {
                    _ = try self.pop();
                },
                .get_local => {
                    const slot = self.chunk.code.items[self.idx];
                    self.idx += 1;
                    try self.push(self.stack.get(slot));
                },
                // TODO: handle string for get/set local
                .set_local => {
                    const slot = self.chunk.code.items[self.idx];
                    self.idx += 1;
                    self.stack.set(slot, self.peek(0));
                },
                .get_global => {
                    const name = self.readConstant().string;
                    if (self.globals.get(name)) |value| {
                        try self.push(value);
                    } else {
                        self.runtimeError("Undefined variable '{s}'", .{name});
                        return error.RuntimeError;
                    }
                },
                .set_global => {
                    const name = self.readConstant().string;
                    const raw_new_value = self.peek(0);
                    const new_value: Value = switch (raw_new_value) {
                        .string => blk: {
                            const dupe = try self.allocator.dupe(u8, raw_new_value.string);
                            break :blk .{ .string = dupe };
                        },
                        .number, .bool => raw_new_value,
                    };
                    errdefer if (new_value == .string) self.allocator.free(new_value.string);

                    if (try self.globals.fetchPut(self.allocator, name, new_value)) |old_kv| {
                        const old_value = old_kv.value;
                        if (std.meta.activeTag(new_value) != std.meta.activeTag(old_value)) {
                            self.globals.put(self.allocator, name, old_value) catch {};
                            self.runtimeError(
                                "Cannot assign a value of type '{s}' to a variable of type '{s}'",
                                .{ @tagName(new_value), @tagName(old_value) },
                            );
                            return error.RuntimeError;
                        }
                        switch (old_value) {
                            .string => self.allocator.free(old_value.string),
                            .number, .bool => {},
                        }
                    } else {
                        std.debug.assert(self.globals.remove(name) == true);
                        self.runtimeError("Undefined variable '{s}'", .{name});
                        return error.RuntimeError;
                    }
                },
                .define_global => {
                    const name = try self.allocator.dupe(u8, self.readConstant().string);
                    errdefer self.allocator.free(name);
                    const gop = try self.globals.getOrPut(self.allocator, name);
                    if (gop.found_existing) {
                        self.runtimeError("Variable '{s}' already defined", .{name});
                        return error.RuntimeError;
                    } else {
                        const value = try self.pop();
                        switch (value) {
                            .string => {
                                const dupe = try self.allocator.dupe(u8, value.string);
                                gop.value_ptr.* = .{ .string = dupe };
                            },
                            .number, .bool => gop.value_ptr.* = value,
                        }
                    }
                },
                // TODO: move to stdlib -> when import and call are implemented
                .print => {
                    const stdout = std.io.getStdOut().writer();
                    stdout.print("{}\n", .{try self.pop()}) catch {};
                },
                .jump => {
                    const offset = std.mem.readInt(
                        u16,
                        @ptrCast(self.chunk.code.items[self.idx .. self.idx + @sizeOf(u16)]),
                        .big,
                    );
                    self.idx += @sizeOf(u16);
                    self.idx += offset;
                },
                .jump_if_false => {
                    const offset = std.mem.readInt(
                        u16,
                        @ptrCast(self.chunk.code.items[self.idx .. self.idx + @sizeOf(u16)]),
                        .big,
                    );
                    self.idx += @sizeOf(u16);
                    if (!self.peek(0).bool) {
                        self.idx += offset;
                    }
                },
                .loop => {
                    const offset = std.mem.readInt(
                        u16,
                        @ptrCast(self.chunk.code.items[self.idx .. self.idx + @sizeOf(u16)]),
                        .big,
                    );
                    self.idx += @sizeOf(u16);
                    self.idx -= offset;
                },
                .@"return" => {
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
