const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");
const Air = @import("Air.zig");
pub const Error = std.mem.Allocator.Error;

const Sema = @This();

instructions: std.MultiArrayList(Air.Inst),
ast: *const Ast,
allocator: std.mem.Allocator,
arena: std.mem.Allocator,

// TODO: testing
pub var root: []Air.Inst.Ref = undefined;

pub fn generate(allocator: std.mem.Allocator, ast: *const Ast) Error!Air {
    var arena = std.heap.ArenaAllocator.init(allocator);
    // defer arena.deinit();

    var self: Sema = .{
        .instructions = .{},
        .ast = ast,
        .allocator = allocator,
        .arena = arena.allocator(),
    };

    var root_list = std.ArrayList(Air.Inst.Ref).init(allocator);
    defer root_list.deinit();

    const node_tags: []const Ast.Node.Tag = ast.nodes.items(.tag);
    for (ast.rootStmts()) |stmt| {
        switch (node_tags[stmt]) {
            .decl => {}, // TODO
            else => {
                try root_list.append(try self.expr(stmt));
            },
        }
    }

    root = try root_list.toOwnedSlice();

    return .{
        .instructions = self.instructions.toOwnedSlice(),
    };
}

fn expr(self: *Sema, node: Ast.Node.Index) Error!Air.Inst.Ref {
    const ast = self.ast;
    // const main_tokens = ast.nodes.items(.main_token);
    // const token_tags = ast.tokens.items(.tag);
    // const node_datas = ast.nodes.items(.data);
    const node_tags: []const Ast.Node.Tag = ast.nodes.items(.tag);

    switch (node_tags[node]) {
        .root => unreachable,

        .add => return self.simpleBinOp(node, .add),

        .int_literal => return self.intLiteral(node),

        else => {
            std.log.debug("Unsupported expression: {}", .{node_tags[node]});
            unreachable;
        },
    }
}

fn simpleBinOp(self: *Sema, node: Ast.Node.Index, op_tag: Air.Inst.Tag) Error!Air.Inst.Ref {
    const ast = self.ast;
    const node_datas = ast.nodes.items(.data);

    const lhs = node_datas[node].lhs;
    const rhs = node_datas[node].rhs;

    try self.instructions.append(self.allocator, .{
        .tag = op_tag,
        .data = .{
            .bin_op = .{
                .lhs = try self.expr(lhs),
                .rhs = try self.expr(rhs),
            },
        },
    });

    return .{ .index = self.instructions.len - 1 };
}

fn intLiteral(self: *Sema, node: Ast.Node.Index) Error!Air.Inst.Ref {
    const raw = self.ast.tokenSlice(self.ast.nodes.items(.main_token)[node]);
    const n = std.fmt.parseUnsigned(u64, raw, 0) catch |err| switch (err) {
        error.Overflow => unreachable, // TODO
        error.InvalidCharacter => unreachable, // validated by tokenizer
    };
    return Air.Inst.Ref{ .value = .{ .int = n } };
}
