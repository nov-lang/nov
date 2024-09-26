const std = @import("std");
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const _value = @import("value.zig");
const Value = _value.Value;
const Function = _value.Function;
const Chunk = @import("Chunk.zig");
const isPrimitive = @import("primitives.zig").isPrimitive;

const Codegen = @This();

pub const Error = std.mem.Allocator.Error;

current: *Frame,
ast: *const Ast,
allocator: std.mem.Allocator,
arena: std.mem.Allocator,

pub fn generate(allocator: std.mem.Allocator, ast: Ast) Error!*Function {
    std.debug.assert(ast.errors.len == 0);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var codegen: Codegen = .{
        .current = undefined,
        .allocator = allocator,
        .arena = arena.allocator(),
        .ast = &ast,
    };

    return (try codegen.generateNode(0)).?;
}

pub const Frame = struct {
    enclosing: ?*Frame = null,
    function_node: Node.Index,
    function: *Function,
    // return_counts: bool = false,
    // return_emitted: bool = false,
};

const Nodegen = *const fn (*Codegen, Node.Index) Error!void;
const generators = std.enums.directEnumArray(Node.Tag, Nodegen, 0, .{
    // .negation = generateNegation,
    .int_literal = generateInt,
});

fn generateNode(self: *Codegen, node: Node.Index) Error!?*Function {
    // synchronize

    if (generators[@intFromEnum(self.ast.nodes.items(.tag)[node])]) |gen| {
        return gen(self, node);
    }

    return null;
}

fn emit(self: *Codegen, code: u32) Error!void {
    return self.chunk.write(code, self.previous.line);
}

fn emitOpCode(self: *Codegen, code: Chunk.OpCode) Error!void {
    return self.emit(@intFromEnum(code));
}

fn emitCodeArg(self: *Codegen, code: Chunk.OpCode, arg: u24) Error!void {
    return self.emit(@as(u32, @intFromEnum(code)) | (@as(u32, @intCast(arg)) << 8));
}

fn makeConstant(self: *Codegen, value: Value) Error!u24 {
    return self.chunk.addConstant(value);
}

fn emitConstant(self: *Codegen, value: Value) Error!void {
    return self.emitCodeArg(.constant, self.makeConstant(value));
}

// fn generateNegation(self: *Codegen, node: Node.Index) !void {
//     const operand = self.ast.nodes.items(.negation)[node];
//     try self.generateNode(operand);
//     try self.chunk.write(Chunk.OpCode.negation, self.ast.nodes.items(.line)[node]);
// }

fn generateInt(self: *Codegen, node: Node.Index) Error!void {
    const literal = self.ast.nodes.items(.main_token)[node];
    const value = Value.create(try std.fmt.parseInt(i64, literal, 0));
    try self.emitConstant(value);
}
