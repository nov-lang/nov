const std = @import("std");
// const _value = @import("value.zig");
// const Value = _value.Value;
const Tokenizer = @import("Scanner.zig");
const Token = Tokenizer.Token;

const Ast = @This();

allocator: std.mem.Allocator,
tokens: std.ArrayListUnmanaged(Token),
nodes: std.ArrayListUnmanaged(Node),

pub const Node = struct {
    expr: Expr,

    pub const Expr = union(enum) {
        binary: Binary,
        grouping: Grouping,
        unary: Unary,
        number: Number,
        // string: String,
        // string_literal: StringLiteral,
        // bool: Bool,
        block: Block,
        expression: *const Node,
    };

    pub fn format(
        self: *const Node,
        comptime fmt: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (fmt.len != 0) {
            std.fmt.invalidFmtError(fmt, self);
        }
        switch (self.expr) {
            inline else => |expr| try expr.format(writer),
            .expression => |expr| try expr.format("", .{}, writer),
        }
    }
};

pub fn init(allocator: std.mem.Allocator) Ast {
    return .{
        .allocator = allocator,
        .tokens = .{},
        .nodes = .{},
    };
}

pub fn deinit(self: *Ast) void {
    self.tokens.deinit(self.allocator);
    self.nodes.deinit(self.allocator);
}

pub fn appendNode(self: *Ast, node: Node) !*Node {
    try self.nodes.append(self.allocator, node);
    return &self.nodes.items[self.nodes.items.len - 1];
}

pub fn appendToken(self: *Ast, token: Token) !*Token {
    try self.tokens.append(self.allocator, token);
    return &self.tokens.items[self.tokens.items.len - 1];
}

pub fn format(
    self: *const Ast,
    comptime fmt: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (fmt.len != 0) {
        std.fmt.invalidFmtError(fmt, self);
    }
    try writer.print("{}", .{self.nodes.getLast()});
}

pub const Binary = struct {
    left: *const Node,
    operator: Token.Tag,
    right: *const Node,

    fn format(self: Binary, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("({s} {} {})", .{ self.operator.lexeme().?, self.left, self.right });
    }
};

pub const Grouping = struct {
    expression: *const Node,

    fn format(self: Grouping, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("(group {})", .{self.expression});
    }
};

pub const Number = struct {
    value: f64,

    fn format(self: Number, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{d}", .{self.value});
    }
};

// pub const String = struct {
//     value: []const u8,

//     fn format(self: String, writer: anytype) @TypeOf(writer).Error!void {
//         try writer.writeAll(self.value);
//     }
// };

// pub const StringLiteral = struct {
//     value: Token,

//     fn format(self: StringLiteral, writer: anytype) @TypeOf(writer).Error!void {
//         try writer.writeAll(self.value.literal.string);
//     }
// };

pub const Unary = struct {
    operator: Token.Tag,
    right: *const Node,

    fn format(self: Unary, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("({s} {})", .{ self.operator.lexeme().?, self.right });
    }
};

pub const Block = struct {
    statements: []const *const Node,

    fn format(self: Block, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll("(block");
        for (self.statements) |statement| {
            try writer.print(" {}", .{statement});
        }
        try writer.writeAll(")");
    }
};
