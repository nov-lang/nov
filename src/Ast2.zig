const std = @import("std");
const v = @import("value.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const assert = std.debug.assert;

const Ast = @This();

source: [:0]const u8,
tokens: TokenList.Slice,
nodes: NodeList.Slice,
errors: []const Error,

pub const TokenIndex = u32;
pub const TokenList = std.MultiArrayList(Token); // only use tag and start?
pub const NodeList = std.MultiArrayList(Node);
pub const root_i = 0;

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) Parser.Error!Ast {
    const tokens = blk: {
        var list: TokenList = .{};
        errdefer list.deinit(allocator);

        // TODO
        // const estimated_token_count = source.len / 8;
        // try tokens.ensureTotalCapacity(allocator, estimated_token_count);

        var tokenizer = Tokenizer.init(source);
        while (true) {
            const token = tokenizer.next();
            try list.append(allocator, token);
            if (token.tag == .eof) {
                break;
            }
        }
        break :blk list.toOwnedSlice();
    };

    var parser: Parser = .{
        .source = source,
        .allocator = allocator,
        .tokens = tokens,
        .tok_i = 0,
        .nodes = .{},
        .errors = .{},
        .depth = 0,
    };
    defer parser.nodes.deinit(allocator);
    defer parser.errors.deinit(allocator);

    // TODO
    // const estimated_node_count = (tokens.len + 2) / 2;
    // try parser.nodes.ensureTotalCapacity(allocator, estimated_node_count);

    try parser.parse();

    return .{
        .source = source,
        .tokens = tokens,
        .nodes = parser.nodes.toOwnedSlice(),
        .errors = try parser.errors.toOwnedSlice(allocator),
    };
}

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    self.tokens.deinit(allocator);
    self.nodes.deinit(allocator); // TODO: some nodes have values that need to be deinitialized (use an arena?)
    allocator.free(self.errors);
}

pub fn rootNodes(self: Ast) []const Node.Index {
    return self.nodes.get(root_i).root.nodes;
}

pub fn format(
    self: *const Ast,
    comptime fmt: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "nodes")) {
        for (self.nodes.get(root_i).root.nodes) |node| {
            try Node.format(self, node, writer);
            try writer.writeAll("\n");
        }
    } else if (comptime std.mem.eql(u8, fmt, "tokens")) {
        const tags = self.tokens.items(.tag);
        if (tags.len == 0) {
            return writer.writeAll("no tokens");
        }
        try writer.print("{s}", .{tags[0].symbol()});
        for (tags[1..]) |tag| {
            try writer.print(" {s}", .{tag.symbol()});
        }
    } else if (comptime std.mem.eql(u8, fmt, "errors")) {
        unreachable;
    } else {
        std.fmt.invalidFmtError(fmt, self);
    }
}

// TODO: optimize to use as little memory as possible
// TODO: missing tags:
//   multi_assign for `=` (and maybe for other op)
//   slice_type: `[]lhs`
//   slice_open: `lhs[rhs..]`
//   slice: `lhs[b..c]`. rhs is index into Slice
//   slice_access: `lhs[rhs]`.
//   slice_init_dot: `.{a, b}`. `sub_list[lhs..rhs]`. (do not implement?)
//   slice_init: `lhs{a, b}`. `sub_range_list[rhs]`.
//   match: `match (lhs) {}`. `SubRange[rhs]`.
//   match_case_one: `lhs => rhs`. If lhs is omitted it means `_`.
//   match_case: `a, b, c => rhs`. `SubRange[lhs]`.
//   match_range: `lhs..rhs`.
//   identifier: all identifiers should be consumed by the parser
pub const Node = union(enum) {
    root: Root,

    binary: Binary,
    block: Block,
    boolean: Boolean,
    @"break": Break,
    call: Call,
    @"continue": Continue,
    dot: Dot,
    @"enum": Enum,
    expression: Expression,
    float: Float,
    @"for": For,
    force_unwrap: ForceUnwrap,
    function: Function,
    function_type: FunctionType,
    grouping: Grouping,
    @"if": If,
    import: Import,
    int_literal: IntLiteral,
    loop: Loop,
    named_variable: NamedVariable,
    nil: Nil,
    @"return": Return,
    string: String,
    type_expr: TypeExpr,
    type_of_expr: TypeOfExpr,
    unary: Unary,
    unwrap: Unwrap,
    declaration: Declaration,
    identifier: Identifier,

    pub const Index = u32;

    fn format(ast: *const Ast, index: Index, writer: anytype) @TypeOf(writer).Error!void {
        const node = ast.nodes.get(index);
        switch (node) {
            .root => unreachable,
            inline else => |self| try self.format(ast, writer),
        }
    }
};

pub const Slot = u32;
pub const SlotType = enum(u8) { local, global };

pub const Root = struct {
    nodes: []const Node.Index,
};

/// Binary operation
/// `{left} {operator} {right}`
/// Operators:
/// `==`, `!=`, `<`, `>`, `<=`, `>=`, `*=`, `/=`, `%=`, `+=`, `-=`, `<<=`,
/// `>>=`, `&=`, `|=`, `^=`, `|=` `=`, `*`, `/`, `%`, `+`, `-`, `<<`, `>>`,
/// `&`, `|`, `^`, `??`, `and`, `or`, `|>`
pub const Binary = struct {
    /// operator is ast.tokens.items(.tag)[token]
    token: TokenIndex,
    left: Node.Index,
    right: Node.Index,

    /// "({op} {left} {right})"
    fn format(self: *const Binary, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("({s} ", .{ast.tokens.items(.tag)[self.token].lexeme().?});
        try Node.format(ast, self.left, writer);
        try writer.writeAll(" ");
        try Node.format(ast, self.right, writer);
        try writer.writeAll(")");
    }
};

/// `LBRACE {statements} RBRACE`
/// The last statement is returned as the value of the block
pub const Block = struct {
    token: TokenIndex,
    statements: []const Node.Index,

    /// "(block {statements})"
    fn format(self: *const Block, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll("(block ");
        for (self.statements) |stmt| {
            try Node.format(ast, stmt, writer);
        }
        try writer.writeAll(")");
    }
};

/// Boolean literal
/// `true` or `false`
pub const Boolean = struct {
    token: TokenIndex,
    value: bool,

    /// "{value}"
    fn format(self: *const Boolean, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = ast;
        try writer.print("{}", .{self.value});
    }
};

/// Break statement
/// `break`
/// `break {value}`
pub const Break = struct {
    token: TokenIndex,
    value: ?Node.Index,

    /// "(break {?node})"
    fn format(self: *const Break, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll("(break");
        if (self.value) |value| {
            try writer.writeAll(" ");
            try Node.format(ast, value, writer);
        }
        try writer.writeAll(")");
    }
};

/// Function call
/// `{callee}({arguments})`
/// `{arguments} |> {callee}` (handled in binary)
pub const Call = struct {
    token: TokenIndex,
    callee: Node.Index,
    // callee_type_def: *v.TypeDef,
    arguments: []const Argument,
    catch_default: ?Node.Index,
    tail_call: bool = false,

    pub const Argument = struct {
        name: ?TokenIndex,
        value: Node.Index,
    };

    // TODO
    fn format(self: *const Call, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

/// Continue statement
/// `continue`
pub const Continue = struct {
    token: TokenIndex,

    // TODO
    fn format(self: *const Continue, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

/// Field access
/// `{callee}.{identifier}`
pub const Dot = struct {
    token: TokenIndex,
    // TODO: idk about all
    callee: Node.Index,
    identifier: TokenIndex,
    member: Member,
    generic_resolve: ?Node.Index,
    // member_type_def: *v.TypeDef,

    pub const Member = union(enum) {
        ref: void,
        value: Node.Index,
        call: Node.Index,
        enum_member: u32,
    };

    // TODO
    fn format(self: *const Dot, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

/// Enum declaration
/// TODO: idk how to represent yet
pub const Enum = struct {
    token: TokenIndex,
    name: TokenIndex,
    member_type: ?Node.Index,
    slot: Slot,
    members: []const Member,

    pub const Member = struct {
        name: TokenIndex,
        value: ?Node.Index,
    };

    // TODO
    fn format(self: *const Enum, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

pub const Expression = struct {
    token: TokenIndex,
    expr: Node.Index,

    /// "{expr}"
    fn format(self: *const Expression, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try Node.format(ast, self.expr, writer);
    }
};

/// Float literal
pub const Float = struct {
    token: TokenIndex,
    value: f64,

    /// "{value}"
    fn format(self: *const Float, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = ast;
        try writer.print("{d}", .{self.value});
    }
};

pub const For = struct {
    token: TokenIndex,
    // TODO: this is a foreach

    // TODO
    fn format(self: *const For, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

/// Force unwrap
/// `{expr}.?`
pub const ForceUnwrap = Unwrap;

// TODO:
// should be
// let {?mut} {?rec} {name} = ({args}) -> {body} -> {?return_type}
// I don't know if it's a good idea to let it be `mut`
// I don't know about the `rec` either
// args can be empty but there must be ()
// arg: {name}: {?type} = {?default}
// if type is not specified it's `any` which is a type that can be anything
/// Function literal
/// LPAREN Param* RPAREN ARROW Statement? (ARROW TypeExpr)?
/// if Statement is not present it's a FunctionType
pub const Function = struct {
    token: TokenIndex,
    params: []const Node.Index,
    body: ?Node.Index,
    return_type: ?Node.Index,

    // TODO
    fn format(self: *const Function, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

/// Function type
/// ({arguments}) -> {return_type}
/// Examples:
/// `() -> ()` (no arguments, no return type)
/// `(a: int, b: int) -> int` (two arguments, return type is int)
/// `() -> (int, int)` (no arguments, return type is a tuple)
/// Note: a tuple is an immutable list
pub const FunctionType = struct {
    token: TokenIndex,
    name: ?TokenIndex,
    return_type: ?Node.Index,
    arguments: []const Argument,
    // generic_types: []const TokenIndex,

    pub const Argument = struct {
        name: TokenIndex,
        type: ?Node.Index,
        default: ?Node.Index,
    };

    // TODO
    fn format(self: *const FunctionType, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

// TODO: Generic

/// Grouped expression
/// `({expr})`
pub const Grouping = struct {
    token: TokenIndex,
    expr: Node.Index,

    /// "(group {expr})"
    fn format(self: *const Grouping, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll("(group ");
        try Node.format(ast, self.expr, writer);
        try writer.writeAll(")");
    }
};

/// If statement
/// `if {condition} {body}`
/// `if {condition} {body} else {else_body}`
/// TODO
/// `if (lhs) |a| rhs`.
/// `if (lhs) |x| a else b`. `If[rhs]`.
/// `if (lhs) |x| a else |y| b`. `If[rhs]`.
pub const If = struct {
    token: TokenIndex,
    condition: Node.Index,
    // unwrapped_identifier: ?TokenIndex,
    // casted_type: ?Node.Index,
    then_expr: Node.Index,
    else_expr: ?Node.Index = null,
    // is_statement: bool,

    // TODO
    fn format(self: *const If, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

pub const Import = struct {
    token: TokenIndex,
    // TODO

    // TODO
    fn format(self: *const Import, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

/// Integer literal
pub const IntLiteral = struct {
    token: TokenIndex,
    value: u64,

    /// "{value}"
    fn format(self: *const IntLiteral, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = ast;
        try writer.print("{d}", .{self.value});
    }
};

// TODO: List

/// Infinite Loop
/// `loop LBRACE {body} RBRACE`
pub const Loop = struct {
    token: TokenIndex,
    body: Node.Index,

    /// "(loop {body})"
    fn format(self: *const Loop, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll("(loop ");
        try Node.format(ast, self.body, writer);
        try writer.writeAll(")");
    }
};

// TODO: Map

pub const NamedVariable = struct {
    token: TokenIndex,
    name: []const TokenIndex,
    value: ?Node.Index,
    slot: Slot,
    slot_type: SlotType,
    slot_constant: bool,

    // TODO
    fn format(self: *const NamedVariable, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

pub const Nil = struct {
    token: TokenIndex,
    /// "nil"
    fn format(self: *const Nil, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
        try writer.writeAll("nil");
    }
};

// TODO: Pattern, ProtocolDeclaration?
// TODO: Range
// TODO: Resolve

/// Return statement
/// `return`
/// `return {value}`
pub const Return = struct {
    token: TokenIndex,
    value: ?Node.Index,

    /// "(return {?value})"
    fn format(self: *const Return, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll("(return");
        if (self.value) |value| {
            try writer.writeAll(" ");
            try Node.format(ast, value, writer);
        }
        try writer.writeAll(")");
    }
};

/// String literal
/// `"value"`
/// `\\multiline
///  \\value
///  \\`
/// TODO: should support interpolation
pub const String = struct {
    /// value: ast.source[ast.tokens.items(.start)[self.token]..ast.tokens.items(.end)[self.token]]
    /// or use tokenSlice
    token: TokenIndex,

    /// "\"{value}\""
    fn format(self: *const String, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("\"{s}\"", .{ast.tokenSlice(self.token)});
    }
};

// TODO: Subscript? Throw? Try?

pub const TypeExpr = struct {
    token: TokenIndex,
    node: Node.Index,
    optional: bool = false,
    slice: bool = false,

    // TODO
    fn format(self: *const TypeExpr, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

pub const TypeOfExpr = struct {
    token: TokenIndex,
    node: Node.Index,

    // TODO
    fn format(self: *const TypeOfExpr, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

/// Unary operation
/// `{operator}{expr}`
/// Operators:
/// `!`, `-`, `~`
/// `?`
pub const Unary = struct {
    /// operator is ast.tokens.items(.tag)[token]
    token: TokenIndex,
    expr: Node.Index,

    /// "({op} {expr})"
    fn format(self: *const Unary, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("({s} ", .{ast.tokens.items(.tag)[self.token].lexeme().?});
        try Node.format(ast, self.expr, writer);
        try writer.writeAll(")");
    }
};

/// Unwrap
pub const Unwrap = struct {
    token: TokenIndex,
    unwrapped: Node.Index,
    // original_type: *v.TypeDef,

    // TODO
    fn format(self: *const Unwrap, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        _ = self;
        _ = ast;
    }
};

// TODO: UserType

/// Variable declaration
/// KEYWORD_let KEYWORD_priv? KEYWORD_mut? IDENTIFIER (COLON TYPE)? (EQUALS VALUE)?
/// full mut:   `let mut {name}: {type} = {value}`
/// full:       `let {name}: {type} = {value}`
/// only value: `let {name} = {value}`
/// only type:  `let {name}: {type}`
/// for functions `value` would be `({args}) -> {body} -> {?return_type}`
pub const Declaration = struct {
    token: TokenIndex,
    name: TokenIndex,
    value: ?Node.Index,
    type: ?Node.Index,
    mutable: bool = false,
    public: bool = true,
    // recursive: bool = false,
    // slot: Slot,
    // slot_type: SlotType,

    // TODO
    fn format(self: *const Declaration, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("(let {s}", .{ast.tokenSlice(self.name)});
        if (self.type) |_type| {
            try writer.writeAll(": ");
            try Node.format(ast, _type, writer);
        }
        if (self.value) |value| {
            try writer.writeAll(" = ");
            try Node.format(ast, value, writer);
        }
        try writer.writeAll(")");
    }
};

/// Identifier
pub const Identifier = struct {
    /// value: ast.source[ast.tokens.items(.start)[self.token]..ast.tokens.items(.end)[self.token]]
    token: TokenIndex,

    // TODO
    fn format(self: *const Identifier, ast: *const Ast, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{s}", .{ast.tokenSlice(self.token)});
    }
};

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub fn tokenLocation(self: Ast, start_offset: u32, token_index: u32) Location {
    var loc: Location = .{
        .line = 0,
        .column = 0,
        .line_start = start_offset,
        .line_end = self.source.len,
    };
    const token_start = self.tokens.items(.start)[token_index];

    // Scan to by line until we go past the token start
    while (std.mem.indexOfScalarPos(u8, self.source, loc.line_start, '\n')) |i| {
        if (i >= token_start) {
            break; // Went past
        }
        loc.line += 1;
        loc.line_start = i + 1;
    }

    const offset = loc.line_start;
    for (self.source[offset..], 0..) |c, i| {
        if (i + offset == token_start) {
            loc.line_end = i + offset;
            while (loc.line_end < self.source.len and self.source[loc.line_end] != '\n') {
                loc.line_end += 1;
            }
            return loc;
        }
        if (c == '\n') {
            loc.line += 1;
            loc.column = 0;
            loc.line_start = i + 1;
        } else {
            loc.column += 1;
        }
    }
    return loc;
}

pub fn tokenSlice(self: Ast, token_index: TokenIndex) []const u8 {
    const token_starts: []const u32 = self.tokens.items(.start);
    const token_tags: []const Token.Tag = self.tokens.items(.tag);
    const token_tag = token_tags[token_index];

    // Many tokens can be determined entirely by their tag.
    if (token_tag.lexeme()) |lexeme| {
        return lexeme;
    }

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: Tokenizer = .{
        .buffer = self.source,
        .index = token_starts[token_index],
    };
    const token = tokenizer.next();
    assert(token.tag == token_tag);
    return self.source[token.start..token.end];
}

pub const Error = struct {
    tag: Tag,
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },

    pub const Tag = enum {
        chained_comparison_operators,
        wrong_equal_var_decl,
        expected_expr,
        expected_expr_or_assignment,
        expected_expr_or_var_decl,
        expected_type_expr,
        expected_prefix_expr,
        expected_newline_or_else,
        expected_newline_or_lbrace,
        expected_newline_after_decl,
        expected_newline_after_stmt,
        expected_comma_after_arg,
        expected_comma_after_match_prong,
        expected_block,
        expected_block_or_assignment,
        expected_block_or_expr,
        expected_statement,

        /// `expected_tag` is populated.
        expected_token,
    };
};

pub fn renderError(self: Ast, parse_error: Error, writer: anytype) !void {
    const token_tags: []const Token.Tag = self.tokens.items(.tag);
    switch (parse_error.tag) {
        .chained_comparison_operators => {
            return writer.writeAll("comparison operators cannot be chained");
        },
        // .decl_between_fields => {
        //     return writer.writeAll("declarations are not allowed between container fields");
        // },
        .expected_block => {
            return writer.print("expected block, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_block_or_assignment => {
            return writer.print("expected block or assignment, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_block_or_expr => {
            return writer.print("expected block or expression, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_block_or_field => {
        //     return writer.print("expected block or field, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .expected_container_members => {
        //     return writer.print("expected test, comptime, var decl, or container field, found '{s}'", .{
        //         token_tags[parse_error.token].symbol(),
        //     });
        // },
        .expected_expr => {
            return writer.print("expected expression, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_expr_or_assignment => {
            return writer.print("expected expression or assignment, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        .expected_expr_or_var_decl => {
            return writer.print("expected expression or var decl, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_labelable => {
        //     return writer.print("expected 'while', 'for', 'inline', or '{{', found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .expected_param_list => {
        //     return writer.print("expected parameter list, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        .expected_prefix_expr => {
            return writer.print("expected prefix expression, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_primary_type_expr => {
        //     return writer.print("expected primary type expression, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        .expected_newline_or_else => {
            return writer.writeAll("expected new line or 'else' after statement");
        },
        .expected_newline_or_lbrace => {
            return writer.writeAll("expected new line or block after function prototype");
        },
        .expected_statement => {
            return writer.print("expected statement, found '{s}'", .{
                token_tags[parse_error.token].symbol(),
            });
        },
        // .expected_suffix_op => {
        //     return writer.print("expected pointer dereference, optional unwrap, or field access, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        .expected_type_expr => {
            return writer.print("expected type expression, found '{s}'", .{
                token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
            });
        },
        // .expected_loop_payload => {
        //     return writer.print("expected loop payload, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .expected_container => {
        //     return writer.print("expected a struct, enum or union, found '{s}'", .{
        //         token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)].symbol(),
        //     });
        // },
        // .extern_fn_body => {
        //     return writer.writeAll("extern functions have no body");
        // },
        // .extra_addrspace_qualifier => {
        //     return writer.writeAll("extra addrspace qualifier");
        // },
        // .extra_align_qualifier => {
        //     return writer.writeAll("extra align qualifier");
        // },
        // .extra_allowzero_qualifier => {
        //     return writer.writeAll("extra allowzero qualifier");
        // },
        // .extra_const_qualifier => {
        //     return writer.writeAll("extra const qualifier");
        // },
        // .extra_volatile_qualifier => {
        //     return writer.writeAll("extra volatile qualifier");
        // },
        // .ptr_mod_on_array_child_type => {
        //     return writer.print("pointer modifier '{s}' not allowed on array child type", .{
        //         token_tags[parse_error.token].symbol(),
        //     });
        // },
        // .invalid_bit_range => {
        //     return writer.writeAll("bit range not allowed on slices and arrays");
        // },
        // .same_line_doc_comment => {
        //     return writer.writeAll("same line documentation comment");
        // },
        // .unattached_doc_comment => {
        //     return writer.writeAll("unattached documentation comment");
        // },
        // .test_doc_comment => {
        //     return writer.writeAll("documentation comments cannot be attached to tests");
        // },
        // .comptime_doc_comment => {
        //     return writer.writeAll("documentation comments cannot be attached to comptime blocks");
        // },
        // .varargs_nonfinal => {
        //     return writer.writeAll("function prototype has parameter after varargs");
        // },
        // .expected_continue_expr => {
        //     return writer.writeAll("expected ':' before while continue expression");
        // },

        .expected_newline_after_decl => {
            return writer.writeAll("expected new line after declaration");
        },
        .expected_newline_after_stmt => {
            return writer.writeAll("expected new line after statement");
        },
        // .expected_comma_after_field => {
        //     return writer.writeAll("expected ',' after field");
        // },
        .expected_comma_after_arg => {
            return writer.writeAll("expected ',' after argument");
        },
        // .expected_comma_after_param => {
        //     return writer.writeAll("expected ',' after parameter");
        // },
        // .expected_comma_after_initializer => {
        //     return writer.writeAll("expected ',' after initializer");
        // },
        .expected_comma_after_match_prong => {
            return writer.writeAll("expected ',' after match prong");
        },
        // .expected_comma_after_for_operand => {
        //     return writer.writeAll("expected ',' after for operand");
        // },
        // .expected_comma_after_capture => {
        //     return writer.writeAll("expected ',' after for capture");
        // },
        // .expected_initializer => {
        //     return writer.writeAll("expected field initializer");
        // },
        // .mismatched_binary_op_whitespace => {
        //     return writer.print("binary operator `{s}` has whitespace on one side, but not the other.", .{token_tags[parse_error.token].lexeme().?});
        // },
        // .zig_style_container => {
        //     return writer.print("to declare a container do 'const {s} = {s}'", .{
        //         self.tokenSlice(parse_error.token), parse_error.extra.expected_tag.symbol(),
        //     });
        // },
        // .previous_field => {
        //     return writer.writeAll("field before declarations here");
        // },
        // .next_field => {
        //     return writer.writeAll("field after declarations here");
        // },
        // .expected_var_const => {
        //     return writer.writeAll("expected 'var' or 'const' before variable declaration");
        // },
        .wrong_equal_var_decl => {
            return writer.writeAll("variable initialized with '==' instead of '='");
        },
        // .var_const_decl => {
        //     return writer.writeAll("use 'var' or 'const' to declare variable");
        // },
        // .extra_for_capture => {
        //     return writer.writeAll("extra capture in for loop");
        // },
        // .for_input_not_captured => {
        //     return writer.writeAll("for input is not captured");
        // },
        .expected_token => {
            const found_tag = token_tags[parse_error.token + @intFromBool(parse_error.token_is_prev)];
            const expected_symbol = parse_error.extra.expected_tag.symbol();
            return writer.print("expected '{s}', found '{s}'", .{
                expected_symbol, found_tag.symbol(),
            });
        },
    }
}
