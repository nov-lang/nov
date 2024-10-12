const std = @import("std");
const Value = @import("value.zig").Value;
const Ast = @import("../Ast.zig");

const Chunk = @This();

code: std.ArrayListUnmanaged(u32),
tokens: std.ArrayListUnmanaged(Ast.TokenIndex),
constants: std.ArrayListUnmanaged(Value),
allocator: std.mem.Allocator,

// TODO: reduce the number of opcodes
// TODO: add higher-level opcodes
// TODO: add an instruction to optimize for loops?
// TODO: look at
// https://treeniks.github.io/x86-64-simplified/instructions/README.html
// https://en.wikipedia.org/wiki/List_of_Java_bytecode_instructions
pub const OpCode = enum(u8) {
    nop = 1,
    @"unreachable" = 2,

    copy = 3,
    swap = 4,

    lodab = 5,
    loadi = 6,
    loadu = 7,
    loadf = 8,
    loado = 9,

    iadd,
    isub,
    imul,
    idiv,
    udiv,
    // imod,
    // irem,

    fadd,
    fsub,
    fmul,
    fdiv,
    // fmod,
    // frem,

    not,

    jmp,
    // jmp_true,
    jmp_false,
    // jmp_nil,
    // jmp_not_nil,

    call,
    ret,

    constant,
    equal,
    greater,
    less,
    add,
    sub,
    mul,
    div,
    mod,
    bool_not,
    negation,
    pop,
    get_local,
    set_local,
    get_global,
    set_global,
    define_global,
    print,
    jump,
    jump_if_false,
    loop,
    @"return",
};

pub fn init(allocator: std.mem.Allocator) Chunk {
    return .{
        .code = .{},
        .locations = .{},
        .constants = .{},
        .allocator = allocator,
    };
}

pub fn deinit(self: *Chunk) void {
    self.code.deinit(self.allocator);
    self.tokens.deinit(self.allocator);
    self.constants.deinit(self.allocator);
}

pub fn write(self: *Chunk, code: u32, token: Ast.TokenIndex) !void {
    try self.code.append(self.allocator, code);
    try self.tokens.append(self.allocator, token);
}

pub fn addConstant(self: *Chunk, value: Value) !u24 {
    try self.constants.append(self.allocator, value);
    return @intCast(self.constants.items.len - 1);
}