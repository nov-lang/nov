const std = @import("std");

// > NOTE: in my VM, i don't use an `enum`.
// >  this is just for simplicity.
// well I use an enum but I might even use an union :EYES:
// no because it adds to much overhead but a packed struct(u32) could be useful
pub const Instruction = union(enum) {
    load_int: struct {
        dst: u8,
        value: i16,
    },
    copy: struct {
        dst: u8,
        src: u8,
    },
    add: struct {
        dst: u8,
        src1: u8,
        src2: u8,
    },
    sub: struct {
        dst: u8,
        src1: u8,
        src2: u8,
    },
    call: struct {
        function: u8,
        arg: u8,
        dst: u8,
    },
    ret: struct {
        src: u8,
    },
    jump_not_less: struct {
        target: u8,
        src1: u8,
        src2: u8,
    },

    // comptime {
    //     if (std.debug.runtime_safety == false) {
    //         std.debug.assert(@sizeOf(Instruction) == @sizeOf(u32));
    //     }
    // }
};

pub const Function = struct {
    code: std.ArrayListUnmanaged(Instruction) = .{},
    num_registers: usize = 8,

    pub fn deinit(self: *Function, allocator: std.mem.Allocator) void {
        self.code.deinit(allocator);
    }
};

pub const Vm = struct {
    functions: std.ArrayListUnmanaged(Function),
    stack: std.ArrayListUnmanaged(i64),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Vm {
        return .{
            .functions = .{},
            .stack = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Vm) void {
        for (self.functions.items) |*func| {
            func.deinit(self.allocator);
        }
        self.functions.deinit(self.allocator);
        self.stack.deinit(self.allocator);
    }

    // func must be allocated with the vm's allocator
    pub fn addFunc(self: *Vm, func: Function) !void {
        return self.functions.append(self.allocator, func);
    }

    pub fn call(self: *Vm, func: u8, arg: i64) !i64 {
        const base_pointer = self.stack.items.len;
        {
            const function = &self.functions.items[func];
            // const stack_top = base_pointer + function.num_registers;

            // initialize registers to zero
            try self.stack.appendNTimes(self.allocator, 0, function.num_registers);
            // try self.stack.resize(self.allocator, stack_top);
            // @memset(self.stack.items, 0);

            // write the argument to register `0`
            self.stack.items[base_pointer] = arg;
        }
        // NOTE: this function's registers are
        //  `self.stack.items[base_pointer..]`
        //  see the `reg` utility function.

        const bp = base_pointer;

        var program_counter: usize = 0;

        while (true) {
            const function = &self.functions.items[func];
            const instr: Instruction = function.code.items[program_counter];
            program_counter += 1;

            // std.debug.print("executing {}\n", .{instr});

            switch (instr) {
                .load_int => |load_int| {
                    self.stack.items[bp + load_int.dst] = load_int.value;
                },
                .copy => |copy| {
                    self.stack.items[bp + copy.dst] = self.stack.items[bp + copy.src];
                },
                .add => |add| {
                    self.stack.items[bp + add.dst] = self.stack.items[bp + add.src1] + self.stack.items[bp + add.src2];
                },
                .sub => |sub| {
                    self.stack.items[bp + sub.dst] = self.stack.items[bp + sub.src1] - self.stack.items[bp + sub.src2];
                },
                .call => |_call| {
                    const _arg = self.stack.items[bp + _call.arg];
                    const result = try self.call(_call.function, _arg);
                    self.stack.items[bp + _call.dst] = result;
                },
                .ret => |ret| {
                    const result = self.stack.items[bp + ret.src];
                    self.stack.shrinkRetainingCapacity(base_pointer);
                    return result;
                },
                .jump_not_less => |jump_not_less| {
                    const src1 = self.stack.items[bp + jump_not_less.src1];
                    const src2 = self.stack.items[bp + jump_not_less.src2];
                    if (!(src1 < src2)) {
                        program_counter = jump_not_less.target;
                    }
                },
            }
        }
    }

    fn reg(self: *Vm, base_pointer: usize, index: u8) *i64 {
        return &self.stack.items[base_pointer + index];
    }
};

// fib(n) = n if (n < 2) else fib(n-2) + fib(n-1)
fn makeFibo(allocator: std.mem.Allocator) !Function {
    var func: Function = .{ .num_registers = 7 };
    errdefer func.deinit(allocator);

    const fib = 0; // function index `0`
    // registers
    const n = 0;
    const two = 1;
    const one = 2;
    const temp = 3;
    const recursive_1 = 4;
    const recursive_2 = 5;
    const result = 6;

    // 0
    try func.code.append(allocator, .{ .load_int = .{ .dst = two, .value = 2 } });
    // 1: if !(n < 2) then jump to else
    try func.code.append(allocator, .{ .jump_not_less = .{ .target = 3, .src1 = n, .src2 = two } });
    // 2: then
    try func.code.append(allocator, .{ .ret = .{ .src = n } });
    // 3: else, recursive_1 = fib(n-2)
    try func.code.append(allocator, .{ .sub = .{ .dst = temp, .src1 = n, .src2 = two } });
    try func.code.append(allocator, .{ .call = .{ .function = fib, .arg = temp, .dst = recursive_1 } });
    // recursive_2 = fib(n-1)
    try func.code.append(allocator, .{ .load_int = .{ .dst = one, .value = 1 } });
    try func.code.append(allocator, .{ .sub = .{ .dst = temp, .src1 = n, .src2 = one } });
    try func.code.append(allocator, .{ .call = .{ .function = fib, .arg = temp, .dst = recursive_2 } });
    // return recursive_1 + recursive_2
    try func.code.append(allocator, .{ .add = .{ .dst = result, .src1 = recursive_1, .src2 = recursive_2 } });
    try func.code.append(allocator, .{ .ret = .{ .src = result } });

    return func;
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = Vm.init(allocator);
    defer vm.deinit();

    try vm.addFunc(try makeFibo(vm.allocator));

    for (0..10) |i| {
        std.debug.print("fib({}) = {}\n", .{ i, try vm.call(0, @intCast(i)) });
    }
}
