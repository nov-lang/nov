// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/render.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details
// TODO: this doesn't work atm because newlines are treated like a newline **and** like a semicolon

const std = @import("std");
const Ast = @import("Ast.zig");
const Token = @import("Tokenizer.zig").Token;
const primitives = @import("primitives.zig");
const string_literal = @import("string_literal.zig");
const assert = std.debug.assert;
pub const Error = std.mem.Allocator.Error;
const Ais = AutoIndentingStream(std.ArrayList(u8).Writer);
const indent_delta = 4; // TODO: replace with tab?

const Render = @This();

allocator: std.mem.Allocator,
ais: *Ais,
ast: Ast,
fixups: Fixups,

pub const Fixups = struct {
    /// The key is the `let` token of the variable declaration
    /// that should have a `_ = foo` inserted afterwards.
    unused_var_decls: std.AutoHashMapUnmanaged(Ast.TokenIndex, void) = .empty,
    /// The functions in this unordered set of AST fn decl nodes will render
    /// with a function body of `@trap()` instead, with all parameters
    /// discarded.
    gut_functions: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .empty,
    /// These global declarations will be omitted.
    omit_nodes: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .empty,
    /// These expressions will be replaced with the string value.
    replace_nodes_with_string: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .empty,
    /// The string value will be inserted directly after the node.
    append_string_after_node: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .empty,
    /// These nodes will be replaced with a different node.
    replace_nodes_with_node: std.AutoHashMapUnmanaged(Ast.Node.Index, Ast.Node.Index) = .empty,
    /// Change all identifier names matching the key to be value instead.
    rename_identifiers: std.StringArrayHashMapUnmanaged([]const u8) = .empty,

    /// All `@import` builtin calls which refer to a file path will be prefixed
    /// with this path.
    rebase_imported_paths: ?[]const u8 = null,

    pub fn count(f: Fixups) usize {
        return f.unused_var_decls.count() +
            f.gut_functions.count() +
            f.omit_nodes.count() +
            f.replace_nodes_with_string.count() +
            f.append_string_after_node.count() +
            f.replace_nodes_with_node.count() +
            f.rename_identifiers.count() +
            @intFromBool(f.rebase_imported_paths != null);
    }

    pub fn clearRetainingCapacity(f: *Fixups) void {
        f.unused_var_decls.clearRetainingCapacity();
        f.gut_functions.clearRetainingCapacity();
        f.omit_nodes.clearRetainingCapacity();
        f.replace_nodes_with_string.clearRetainingCapacity();
        f.append_string_after_node.clearRetainingCapacity();
        f.replace_nodes_with_node.clearRetainingCapacity();
        f.rename_identifiers.clearRetainingCapacity();

        f.rebase_imported_paths = null;
    }

    pub fn deinit(f: *Fixups, allocator: std.mem.Allocator) void {
        f.unused_var_decls.deinit(allocator);
        f.gut_functions.deinit(allocator);
        f.omit_nodes.deinit(allocator);
        f.replace_nodes_with_string.deinit(allocator);
        f.append_string_after_node.deinit(allocator);
        f.replace_nodes_with_node.deinit(allocator);
        f.rename_identifiers.deinit(allocator);
        f.* = undefined;
    }
};

pub fn renderTree(buffer: *std.ArrayList(u8), ast: Ast, fixups: Fixups) Error!void {
    assert(ast.errors.len == 0); // Cannot render an invalid AST.
    var auto_indenting_stream = Ais.init(buffer, indent_delta);
    defer auto_indenting_stream.deinit();
    var r: Render = .{
        .allocator = buffer.allocator,
        .ais = &auto_indenting_stream,
        .ast = ast,
        .fixups = fixups,
    };

    // Render all the line comments at the beginning of the file.
    const comment_end_loc = ast.tokens.items(.start)[0];
    _ = try r.renderComments(0, comment_end_loc);

    try r.renderMembers(ast.rootDecls());

    if (auto_indenting_stream.disabled_offset) |disabled_offset| {
        try writeFixingWhitespace(auto_indenting_stream.underlying_writer, ast.source[disabled_offset..]);
    }
}

const Container = enum {
    @"enum",
    tuple,
    other,
};

/// Render all members in the given slice, keeping empty lines where appropriate
fn renderMembers(self: *Render, members: []const Ast.Node.Index) Error!void {
    // const ast = r.ast;
    if (members.len == 0) return;
    // const container: Container = for (members) |member| {
    //     if (ast.fullContainerField(member)) |field| if (!field.ast.tuple_like) break .other;
    // } else .tuple;
    const container: Container = .other;
    try renderMember(self, container, members[0], .newline);
    for (members[1..]) |member| {
        try self.renderExtraNewline(member);
        try self.renderMember(container, member, .newline);
    }
}

fn renderMember(
    self: *Render,
    container: Container,
    decl: Ast.Node.Index,
    space: Space,
) Error!void {
    _ = container;
    _ = space;
    const ast = self.ast;
    const ais = self.ais;
    const node_tags: []const Ast.Node.Tag = ast.nodes.items(.tag);
    // const token_tags = ast.tokens.items(.tag);
    // const main_tokens = ast.nodes.items(.main_token);
    // const datas = ast.nodes.items(.data);
    if (self.fixups.omit_nodes.contains(decl)) return;
    try self.renderDocComments(ast.firstToken(decl));
    switch (node_tags[decl]) {
        .attr_decl_one => {
            @panic("TODO");
        },

        .attr_decl => {
            @panic("TODO");
        },

        .decl => {
            try ais.pushSpace(.endline);
            try self.renderDecl(ast.fullDecl(decl).?, .endline);
            ais.popSpace();
        },

        .container_field => {
            @panic("TODO");
        },

        else => unreachable,
    }
}

/// Render all expressions in the slice, keeping empty lines where appropriate
fn renderExpressions(
    self: *Render,
    expressions: []const Ast.Node.Index,
    space: Space,
) Error!void {
    if (expressions.len == 0) return;
    try renderExpression(self, expressions[0], space);
    for (expressions[1..]) |expression| {
        try self.renderExtraNewline(expression);
        try self.renderExpression(expression, space);
    }
}

fn renderExpression(self: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const ast = self.ast;
    const ais = self.ais;
    const token_tags = ast.tokens.items(.tag);
    const main_tokens = ast.nodes.items(.main_token);
    const node_tags: []const Ast.Node.Tag = ast.nodes.items(.tag);
    const datas = ast.nodes.items(.data);
    if (self.fixups.replace_nodes_with_string.get(node)) |replacement| {
        try ais.writer().writeAll(replacement);
        try renderOnlySpace(self, space);
        return;
    } else if (self.fixups.replace_nodes_with_node.get(node)) |replacement| {
        return self.renderExpression(replacement, space);
    }
    switch (node_tags[node]) {
        .identifier => {
            const token_index = main_tokens[node];
            return self.renderIdentifier(token_index, space, .preserve_when_shadowing);
        },

        .number_literal,
        .char_literal,
        .string_literal,
        => return self.renderToken(main_tokens[node], space),

        .builtin_literal => {
            @panic("TODO");
        },

        .enum_literal => {
            try self.renderToken(datas[node].lhs, .none); // .
            return self.renderIdentifier(main_tokens[node], space, .eagerly_unquote); // name
        },

        .block_two,
        .block_two_newline,
        => {
            @panic("TODO");
            // const statements = [2]Ast.Node.Index{ datas[node].lhs, datas[node].rhs };
            // if (datas[node].lhs == 0) {
            //     return renderBlock(r, node, statements[0..0], space);
            // } else if (datas[node].rhs == 0) {
            //     return renderBlock(r, node, statements[0..1], space);
            // } else {
            //     return renderBlock(r, node, statements[0..2], space);
            // }
        },
        .block,
        .block_newline,
        => {
            @panic("TODO");
            // const statements = ast.extra_data[datas[node].lhs..datas[node].rhs];
            // return renderBlock(r, node, statements, space);
        },

        .@"defer" => {
            @panic("TODO");
        },

        .@"comptime" => {
            const comptime_token = main_tokens[node];
            const block = datas[node].rhs;
            try self.renderToken(comptime_token, .none);
            return self.renderExpression(block, space);
        },

        .field_access => {
            @panic("TODO");
        },

        .range => {
            const infix = datas[node];
            try self.renderExpression(infix.lhs, .none);
            if (infix.rhs != 0) {
                try self.renderToken(main_tokens[node], .none);
                return self.renderExpression(infix.rhs, space);
            } else {
                return self.renderToken(main_tokens[node], space);
            }
        },

        .assign,
        .assign_div,
        .assign_sub,
        .assign_mod,
        .assign_add,
        .assign_mul,
        => {
            const infix = datas[node];
            try self.renderExpression(infix.lhs, .space);
            const op_token = main_tokens[node];
            try ais.pushIndent(.after_equals);
            if (token_tags[op_token + 1] == .newline) {
                try self.renderToken(op_token, .newline);
            } else {
                try self.renderToken(op_token, .space);
            }
            try self.renderExpression(infix.rhs, space);
            ais.popIndent();
        },

        .add,
        .bang_equal,
        .bit_and,
        .bit_or,
        .shl,
        .shr,
        .bit_xor,
        .bool_and,
        .bool_or,
        .div,
        .equal_equal,
        .greater_or_equal,
        .greater_than,
        .less_or_equal,
        .less_than,
        .mod,
        .mul,
        .sub,
        .in,
        .function_pipe, // TODO
        => {
            const infix = datas[node];
            try self.renderExpression(infix.lhs, .space);
            const op_token = main_tokens[node];
            try ais.pushIndent(.binop);
            if (token_tags[op_token + 1] == .newline) {
                try self.renderToken(op_token, .newline);
            } else {
                try self.renderToken(op_token, .space);
            }
            try self.renderExpression(infix.rhs, space);
            ais.popIndent();
        },

        // TODO: assign_destructure

        .bit_not,
        .bool_not,
        .negation,
        .ref_of,
        => {
            try self.renderToken(main_tokens[node], .none);
            return self.renderExpression(datas[node].lhs, space);
        },

        .array_type => {
            try self.renderToken(main_tokens[node], .none); // [
            try self.renderToken(main_tokens[node] + 1, .none); // ]
            return self.renderExpression(datas[node].lhs, space);
        },

        .ref_type => {
            try self.renderToken(main_tokens[node], .none); // *
            if (datas[node].lhs != 0) {
                try self.renderToken(datas[node].lhs, .space); // mut
            }
            return self.renderExpression(datas[node].rhs, space);
        },

        .array_init_two,
        .array_init_two_comma,
        .array_init,
        .array_init_comma,
        => {
            @panic("TODO");
        },

        // TODO: struct_init

        .call_one,
        .call_one_comma,
        .call,
        .call_comma,
        => {
            @panic("TODO");
        },

        .array_access,
        .slice,
        => {
            const suffix = datas[node];
            const lbracket = main_tokens[node];
            const rbracket = ast.lastToken(suffix.rhs) + 1;
            try self.renderExpression(suffix.lhs, .none);
            // try ais.pushIndent(.normal);
            try self.renderToken(lbracket, .none); // [
            try self.renderExpression(suffix.rhs, .none);
            // ais.popIndent();
            return self.renderToken(rbracket, space); // ]
        },

        .deref,
        .unwrap,
        => {
            try self.renderExpression(datas[node].lhs, .none);
            try self.renderToken(main_tokens[node], .none);
            return self.renderToken(datas[node].rhs, space);
        },

        .@"break" => {
            const main_token = main_tokens[node];
            const label_token = datas[node].lhs;
            const expr_token = datas[node].rhs;
            if (label_token == 0 and expr_token == 0) {
                try self.renderToken(main_token, space); // break
            } else if (label_token == 0 and expr_token != 0) {
                try self.renderToken(main_token, .space); // break
                try self.renderExpression(expr_token, space);
            } else if (label_token != 0 and expr_token == 0) {
                try self.renderToken(main_token, .space); // break
                try self.renderToken(label_token - 1, .none); // :
                try self.renderIdentifier(label_token, space, .eagerly_unquote);
            } else if (label_token != 0 and expr_token != 0) {
                try self.renderToken(main_token, .space); // break
                try self.renderToken(label_token - 1, .none); // :
                try self.renderIdentifier(label_token, .space, .eagerly_unquote);
                try self.renderExpression(expr_token, space);
            }
        },

        .@"continue" => {
            const main_token = main_tokens[node];
            const label_token = datas[node].lhs;
            if (label_token == 0) {
                try self.renderToken(main_token, space); // continue
            } else {
                try self.renderToken(main_token, .space); // continue
                try self.renderToken(label_token - 1, .none); // :
                try self.renderIdentifier(label_token, space, .eagerly_unquote);
            }
        },

        .@"return" => {
            if (datas[node].rhs != 0) {
                try self.renderToken(main_tokens[node], .space);
                try self.renderExpression(datas[node].lhs, space);
            } else {
                try self.renderToken(main_tokens[node], space);
            }
        },

        // TODO: grouped_expression (if it goes back into the language)

        .container_decl,
        .container_decl_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .container_decl_arg,
        .container_decl_arg_trailing,
        => {
            @panic("TODO");
        },

        .fn_proto_one,
        .fn_proto,
        => {
            @panic("TODO");
        },

        .match => {
            @panic("TODO");
        },

        .match_case_one,
        .match_case,
        => {
            @panic("TODO");
        },

        // TODO: loop

        .@"if",
        .if_else,
        => {
            @panic("TODO");
        },

        .bind => {
            @panic("TODO: idk");
        },

        .attr_one,
        .attr,
        .attr_decl_one,
        .attr_decl,
        .decl,
        .container_field,
        .root,
        => unreachable,
    }
}

// TODO: renderExpressionFixup
// TODO: renderSlice

fn renderDecl(
    self: *Render,
    decl: Ast.full.Decl,
    /// `comma_space` and `space` are used for destructure LHS decls.
    space: Space,
) Error!void {
    try renderDeclWithoutFixups(self, decl, space);
    const name_token = decl.ast.let_token + @intFromBool(decl.ast.is_mutable) + 1;
    if (self.fixups.unused_var_decls.contains(name_token)) {
        // Discard the variable like this: `_ = foo;`
        const w = self.ais.writer();
        try w.writeAll("_ = ");
        try w.writeAll(tokenSliceForRender(self.ast, name_token));
        try w.writeAll("\n");
    }
}

fn renderDeclWithoutFixups(
    self: *Render,
    decl: Ast.full.Decl,
    /// `comma_space` and `space` are used for destructure LHS decls.
    space: Space,
) Error!void {
    const ast = self.ast;
    const ais = self.ais;

    // TODO
    // for (decl.attributes) |attribute| {
    //     try renderAttribute(r, attribute, .newline);
    // }

    try self.renderToken(decl.ast.let_token, .space); // let
    const name_token = if (decl.ast.is_mutable) name: {
        try self.renderToken(decl.ast.let_token + 1, .space); // mut
        break :name decl.ast.let_token + 2;
    } else decl.ast.let_token + 1;

    try self.renderIdentifier(name_token, .space, .preserve_when_shadowing); // name

    if (decl.ast.type_node != 0) {
        // TODO: type_node - 1 for the colon?
        try self.renderToken(name_token + 1, .space); // :
        try self.renderExpression(decl.ast.type_node, .space); // type
    }

    if (decl.ast.init_node != 0) {
        const token_tags = ast.tokens.items(.tag);
        const prev_token = ast.firstToken(decl.ast.init_node) - 1;
        if (token_tags[prev_token] == .equal) {
            try self.renderToken(prev_token, .space); // =
        } else {
            assert(token_tags[prev_token - 1] == .equal);
            assert(token_tags[prev_token] == .newline);
            try self.renderToken(prev_token - 1, .newline); // =
        }
        try ais.pushIndent(.after_equals);
        // TODO: doesn't render indent correctly
        try self.renderExpression(decl.ast.init_node, space);
        ais.popIndent();
    }
}

// TODO: renderIf
// TODO: renderLoop
// TODO: renderContainerField
// TODO: renderBuiltin
// TODO: renderFnProto
// TODO: renderMatchCase
// TODO: renderBlock
// TODO: finishRenderBlock
// TODO: renderStructInit
// TODO: renderArrayInit
// TODO: renderContainerDecl
// TODO: renderCall
// TODO: renderParamList
// TODO: renderExpressionComma
// TODO: renderTokenComma
// TODO: renderIdentifierComma

const Space = enum {
    /// Output the token lexeme only.
    none,
    /// Output the token lexeme followed by a single space.
    space,
    /// Output the token lexeme followed by a newline.
    newline,
    /// If the next token is a comma, render it as well. If not, insert one.
    /// In either case, a newline will be inserted afterwards.
    comma,
    /// Additionally consume the next token if it is a comma.
    /// In either case, a space will be inserted afterwards.
    comma_space,
    /// Additionally consume the next token if it is a newline.
    /// In either case, a newline will be inserted afterwards.
    endline,
    /// Skip rendering whitespace and comments. If this is used, the caller
    /// *must* handle whitespace and comments manually.
    skip,
};

fn renderToken(self: *Render, token_index: Ast.TokenIndex, space: Space) Error!void {
    const lexeme = tokenSliceForRender(self.ast, token_index);
    try self.ais.writer().writeAll(lexeme);
    try renderSpace(self, token_index, lexeme.len, space);
}

fn renderTokenOverrideSpaceMode(
    self: *Render,
    token_index: Ast.TokenIndex,
    space: Space,
    override_space: Space,
) Error!void {
    const ais = self.ais;
    const lexeme = tokenSliceForRender(self.ast, token_index);
    try ais.writer().writeAll(lexeme);
    ais.enableSpaceMode(override_space);
    defer ais.disableSpaceMode();
    try renderSpace(self, token_index, lexeme.len, space);
}

// TODO
fn renderSpace(
    self: *Render,
    token_index: Ast.TokenIndex,
    lexeme_len: usize,
    space: Space,
) Error!void {
    const ast = self.ast;
    const ais = self.ais;
    const token_tags = ast.tokens.items(.tag);
    const token_starts = ast.tokens.items(.start);

    const token_start = token_starts[token_index];

    if (space == .skip) return;

    if (space == .comma and token_tags[token_index + 1] != .comma) {
        try ais.writer().writeByte(',');
    }
    if (space == .endline or space == .comma) ais.enableSpaceMode(space);
    defer ais.disableSpaceMode();
    const comment = try renderComments(self, token_start + lexeme_len, token_starts[token_index + 1]);
    switch (space) {
        .none => {},
        .space => if (!comment) try ais.writer().writeByte(' '),
        .newline => if (!comment) try ais.insertNewline(),

        .comma => if (token_tags[token_index + 1] == .comma) {
            try renderToken(self, token_index + 1, .newline);
        } else if (!comment) {
            try ais.insertNewline();
        },

        .comma_space => if (token_tags[token_index + 1] == .comma) {
            try renderToken(self, token_index + 1, .space);
        } else if (!comment) {
            try ais.writer().writeByte(' ');
        },

        // TODO: wrong
        .endline => if (!comment) try ais.insertNewline(),
        // .endline => if (token_tags[token_index + 1] == .newline) {
        //     try renderToken(self, token_index + 1, .newline);
        // } else if (!comment) {
        //     try ais.insertNewline();
        // },

        .skip => unreachable,
    }
}

fn renderOnlySpace(self: *Render, space: Space) Error!void {
    const ais = self.ais;
    switch (space) {
        .none => {},
        .space => try ais.writer().writeByte(' '),
        .newline, .endline => try ais.insertNewline(),
        .comma => try ais.writer().writeAll(",\n"),
        .comma_space => try ais.writer().writeAll(", "),
        // .endline => try ais.writer().writeAll("\n"),
        .skip => unreachable,
    }
}

const QuoteBehavior = enum {
    preserve_when_shadowing,
    eagerly_unquote,
    eagerly_unquote_except_underscore,
};

fn renderIdentifier(
    self: *Render,
    token_index: Ast.TokenIndex,
    space: Space,
    quote: QuoteBehavior,
) Error!void {
    const ast = self.ast;
    const token_tags = ast.tokens.items(.tag);
    assert(token_tags[token_index] == .identifier);
    const lexeme = tokenSliceForRender(ast, token_index);

    if (self.fixups.rename_identifiers.get(lexeme)) |mangled| {
        try self.ais.writer().writeAll(mangled);
        try renderSpace(self, token_index, lexeme.len, space);
        return;
    }

    if (lexeme[0] != '@') {
        return renderToken(self, token_index, space);
    }

    assert(lexeme.len >= 3);
    assert(lexeme[0] == '@');
    assert(lexeme[1] == '\"');
    assert(lexeme[lexeme.len - 1] == '\"');
    const contents = lexeme[2 .. lexeme.len - 1]; // inside the @"" quotation

    // Empty name can't be unquoted.
    if (contents.len == 0) {
        return renderQuotedIdentifier(self, token_index, space, false);
    }

    // Special case for _.
    if (isUnderscore(contents)) switch (quote) {
        .eagerly_unquote => return renderQuotedIdentifier(self, token_index, space, true),
        .eagerly_unquote_except_underscore,
        .preserve_when_shadowing,
        => return renderQuotedIdentifier(self, token_index, space, false),
    };

    // Scan the entire name for characters that would (after un-escaping) be illegal in a symbol,
    // i.e. contents don't match: [A-Za-z_][A-Za-z0-9_]*
    var contents_i: usize = 0;
    while (contents_i < contents.len) {
        switch (contents[contents_i]) {
            '0'...'9' => if (contents_i == 0) return renderQuotedIdentifier(self, token_index, space, false),
            'A'...'Z', 'a'...'z', '_' => {},
            '\\' => {
                var esc_offset = contents_i;
                const res = string_literal.parseEscapeSequence(contents, &esc_offset);
                switch (res) {
                    .success => |char| switch (char) {
                        '0'...'9' => if (contents_i == 0) return renderQuotedIdentifier(self, token_index, space, false),
                        'A'...'Z', 'a'...'z', '_' => {},
                        else => return renderQuotedIdentifier(self, token_index, space, false),
                    },
                    .failure => return renderQuotedIdentifier(self, token_index, space, false),
                }
                contents_i += esc_offset;
                continue;
            },
            else => return renderQuotedIdentifier(self, token_index, space, false),
        }
        contents_i += 1;
    }

    // Read enough of the name (while un-escaping) to determine if it's a keyword or primitive.
    // If it's too long to fit in this buffer, we know it's neither and quoting is unnecessary.
    // If we read the whole thing, we have to do further checks.
    const longest_keyword_or_primitive_len = comptime blk: {
        var longest = 0;
        for (primitives.names.keys()) |key| {
            if (key.len > longest) longest = key.len;
        }
        for (Token.keywords.keys()) |key| {
            if (key.len > longest) longest = key.len;
        }
        break :blk longest;
    };
    var buf: [longest_keyword_or_primitive_len]u8 = undefined;

    contents_i = 0;
    var buf_i: usize = 0;
    while (contents_i < contents.len and buf_i < longest_keyword_or_primitive_len) {
        if (contents[contents_i] == '\\') {
            const res = string_literal.parseEscapeSequence(contents, &contents_i).success;
            buf[buf_i] = @as(u8, @intCast(res));
            buf_i += 1;
        } else {
            buf[buf_i] = contents[contents_i];
            contents_i += 1;
            buf_i += 1;
        }
    }

    // We read the whole thing, so it could be a keyword or primitive.
    if (contents_i == contents.len) {
        if (!isValidId(buf[0..buf_i])) {
            return renderQuotedIdentifier(self, token_index, space, false);
        }
        if (primitives.isPrimitive(buf[0..buf_i])) switch (quote) {
            .eagerly_unquote,
            .eagerly_unquote_except_underscore,
            => return renderQuotedIdentifier(self, token_index, space, true),
            .preserve_when_shadowing => return renderQuotedIdentifier(self, token_index, space, false),
        };
    }

    try renderQuotedIdentifier(self, token_index, space, true);
}

/// Renders a @"" quoted identifier, normalizing escapes.
/// Unnecessary escapes are un-escaped, and \u escapes are normalized to \x when they fit.
/// If unquote is true, the @"" is removed and the result is a bare symbol whose validity is asserted.
fn renderQuotedIdentifier(self: *Render, token_index: Ast.TokenIndex, space: Space, comptime unquote: bool) !void {
    const ast = self.ast;
    const ais = self.ais;
    const token_tags = ast.tokens.items(.tag);
    assert(token_tags[token_index] == .identifier);
    const lexeme = tokenSliceForRender(ast, token_index);
    assert(lexeme.len >= 3 and lexeme[0] == '@');

    if (!unquote) try ais.writer().writeAll("@\"");
    const contents = lexeme[2 .. lexeme.len - 1];
    try renderIdentifierContents(ais.writer(), contents);
    if (!unquote) try ais.writer().writeByte('\"');

    try renderSpace(self, token_index, lexeme.len, space);
}

fn renderIdentifierContents(writer: anytype, bytes: []const u8) !void {
    var pos: usize = 0;
    while (pos < bytes.len) {
        const byte = bytes[pos];
        switch (byte) {
            '\\' => {
                const old_pos = pos;
                const res = string_literal.parseEscapeSequence(bytes, &pos);
                const escape_sequence = bytes[old_pos..pos];
                switch (res) {
                    .success => |codepoint| {
                        if (codepoint <= 0x7f) {
                            const buf = [1]u8{@as(u8, @intCast(codepoint))};
                            try std.fmt.format(writer, "{}", .{fmtEscapes(&buf)});
                        } else {
                            try writer.writeAll(escape_sequence);
                        }
                    },
                    .failure => {
                        try writer.writeAll(escape_sequence);
                    },
                }
            },
            0x00...('\\' - 1), ('\\' + 1)...0x7f => {
                const buf = [1]u8{byte};
                try std.fmt.format(writer, "{}", .{fmtEscapes(&buf)});
                pos += 1;
            },
            0x80...0xff => {
                try writer.writeByte(byte);
                pos += 1;
            },
        }
    }
}

// TODO: renderIdentifierContents

/// Returns true if there exists a line comment between any of the tokens from
/// `start_token` to `end_token`. This is used to determine if e.g. a
/// fn_proto should be wrapped and have a trailing comma inserted even if
/// there is none in the source.
fn hasComment(tree: Ast, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    const token_starts = tree.tokens.items(.start);
    var i = start_token;
    while (i < end_token) : (i += 1) {
        const start = token_starts[i] + tree.tokenSlice(i).len;
        const end = token_starts[i + 1];
        if (std.mem.indexOfScalar(u8, tree.source[start..end], ';') != null) {
            return true;
        }
    }
    return false;
}

/// Assumes that start is the first byte past the previous token and
/// that end is the last byte before the next token.
fn renderComments(self: *Render, start: usize, end: usize) Error!bool {
    const ast = self.ast;
    const ais = self.ais;

    var index: usize = start;
    while (std.mem.indexOfScalar(u8, ast.source[index..end], ';')) |offset| {
        const comment_start = index + offset;

        // If there is no newline, the comment ends with EOF
        const newline_index = std.mem.indexOfScalar(u8, ast.source[comment_start..end], '\n');
        const newline = if (newline_index) |i| comment_start + i else null;

        const untrimmed_comment = ast.source[comment_start .. newline orelse ast.source.len];
        const trimmed_comment = std.mem.trimRight(u8, untrimmed_comment, &std.ascii.whitespace);

        // Don't leave any whitespace at the start of the file
        if (index != 0) {
            if (index == start and std.mem.containsAtLeast(u8, ast.source[index..comment_start], 2, "\n")) {
                // Leave up to one empty line before the first comment
                try ais.insertNewline();
                try ais.insertNewline();
            } else if (std.mem.indexOfScalar(u8, ast.source[index..comment_start], '\n') != null) {
                // Respect the newline directly before the comment.
                // Note: This allows an empty line between comments
                try ais.insertNewline();
            } else if (index == start) {
                // Otherwise if the first comment is on the same line as
                // the token before it, prefix it with a single space.
                try ais.writer().writeByte(' ');
            }
        }

        index = 1 + (newline orelse end - 1);

        const comment_content = std.mem.trimLeft(u8, trimmed_comment[";".len..], &std.ascii.whitespace);
        if (ais.disabled_offset != null and std.mem.eql(u8, comment_content, "nov fmt: on")) {
            // Write the source for which formatting was disabled directly
            // to the underlying writer, fixing up invalid whitespace.
            const disabled_source = ast.source[ais.disabled_offset.?..comment_start];
            try writeFixingWhitespace(ais.underlying_writer, disabled_source);
            // Write with the canonical single space.
            try ais.underlying_writer.writeAll("; nov fmt: on\n");
            ais.disabled_offset = null;
        } else if (ais.disabled_offset == null and std.mem.eql(u8, comment_content, "nov fmt: off")) {
            // Write with the canonical single space.
            try ais.writer().writeAll("; nov fmt: off\n");
            ais.disabled_offset = index;
        } else {
            // Write the comment minus trailing whitespace.
            try ais.writer().print("{s}\n", .{trimmed_comment});
        }
    }

    if (index != start and std.mem.containsAtLeast(u8, ast.source[index - 1 .. end], 2, "\n")) {
        // Don't leave any whitespace at the end of the file
        if (end != ast.source.len) {
            try ais.insertNewline();
        }
    }

    return index != start;
}

fn renderExtraNewline(self: *Render, node: Ast.Node.Index) Error!void {
    return renderExtraNewlineToken(self, self.ast.firstToken(node));
}

/// Check if there is an empty line immediately before the given token. If so, render it.
fn renderExtraNewlineToken(self: *Render, token_index: Ast.TokenIndex) Error!void {
    const ast = self.ast;
    const ais = self.ais;
    const token_starts = ast.tokens.items(.start);
    const token_start = token_starts[token_index];
    if (token_start == 0) return;
    const prev_token_end = if (token_index == 0)
        0
    else
        token_starts[token_index - 1] + tokenSliceForRender(ast, token_index - 1).len;

    // If there is a immediately preceding comment or doc_comment,
    // skip it because required extra newline has already been rendered.
    if (std.mem.indexOfScalar(u8, ast.source[prev_token_end..token_start], ';') != null) return;
    if (token_index > 0 and ast.tokens.items(.tag)[token_index - 1] == .doc_comment) return;

    // Iterate backwards to the end of the previous token, stopping if a
    // non-whitespace character is encountered or two newlines have been found.
    var i = token_start - 1;
    var newlines: u2 = 0;
    while (std.ascii.isWhitespace(ast.source[i])) : (i -= 1) {
        if (ast.source[i] == '\n') newlines += 1;
        if (newlines == 2) return ais.insertNewline();
        if (i == prev_token_end) break;
    }
}

/// end_token is the token one past the last doc comment token. This function
/// searches backwards from there.
fn renderDocComments(self: *Render, end_token: Ast.TokenIndex) Error!void {
    const ast = self.ast;
    // Search backwards for the first doc comment.
    const token_tags = ast.tokens.items(.tag);
    if (end_token == 0) return;
    var tok = end_token - 1;
    while (token_tags[tok] == .doc_comment) {
        if (tok == 0) break;
        tok -= 1;
    } else {
        tok += 1;
    }
    const first_tok = tok;
    if (first_tok == end_token) return;

    if (first_tok != 0) {
        const prev_token_tag = token_tags[first_tok - 1];

        // Prevent accidental use of `renderDocComments` for a function argument doc comment
        assert(prev_token_tag != .l_paren);

        if (prev_token_tag != .l_brace) {
            try renderExtraNewlineToken(self, first_tok);
        }
    }

    while (token_tags[tok] == .doc_comment) : (tok += 1) {
        try renderToken(self, tok, .newline);
    }
}

// TODO: discardAllParams

fn tokenSliceForRender(ast: Ast, token_index: Ast.TokenIndex) []const u8 {
    const token_slice = ast.tokenSlice(token_index);
    if (ast.tokens.items(.tag)[token_index] == .doc_comment) {
        return std.mem.trimRight(u8, token_slice, &std.ascii.whitespace);
    } else {
        return token_slice;
    }
}

// TODO: hasSameLineComment
// TODO: anythingBetween

fn writeFixingWhitespace(writer: std.ArrayList(u8).Writer, slice: []const u8) Error!void {
    for (slice) |byte| switch (byte) {
        '\t' => try writer.writeAll(" " ** indent_delta),
        '\r' => {},
        else => try writer.writeByte(byte),
    };
}

fn nodeIsBlock(tag: Ast.Node.Tag) bool {
    return switch (tag) {
        .block,
        .block_newline,
        .block_two,
        .block_two_newline,
        => true,
        else => false,
    };
}

fn nodeIsIfLoopMatch(tag: Ast.Node.Tag) bool {
    return switch (tag) {
        .@"if",
        .if_else,
        // TODO: loop
        .match,
        => true,
        else => false,
    };
}

// TODO: nodeCausesSliceOpSpace
// TODO: rowSize

fn isUnderscore(bytes: []const u8) bool {
    return bytes.len == 1 and bytes[0] == '_';
}

fn isValidId(bytes: []const u8) bool {
    if (bytes.len == 0) return false;
    for (bytes, 0..) |c, i| {
        switch (c) {
            '_', 'a'...'z', 'A'...'Z' => {},
            '0'...'9' => if (i == 0) return false,
            else => return false,
        }
    }
    return Token.getKeyword(bytes) == null;
}

/// Return a Formatter for Nov Escapes of a double quoted string.
/// The format specifier must be one of:
///  * `{}` treats contents as a double-quoted string.
///  * `{'}` treats contents as a single-quoted string.
fn fmtEscapes(bytes: []const u8) std.fmt.Formatter(stringEscape) {
    return .{ .data = bytes };
}

/// Print the string as escaped contents of a double quoted or single-quoted string.
/// Format `{}` treats contents as a double-quoted string.
/// Format `{'}` treats contents as a single-quoted string.
fn stringEscape(
    bytes: []const u8,
    comptime f: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;
    for (bytes) |byte| switch (byte) {
        '\n' => try writer.writeAll("\\n"),
        '\r' => try writer.writeAll("\\r"),
        '\t' => try writer.writeAll("\\t"),
        '\\' => try writer.writeAll("\\\\"),
        '"' => {
            if (f.len == 1 and f[0] == '\'') {
                try writer.writeByte('"');
            } else if (f.len == 0) {
                try writer.writeAll("\\\"");
            } else {
                @compileError("expected {} or {'}, found {" ++ f ++ "}");
            }
        },
        '\'' => {
            if (f.len == 1 and f[0] == '\'') {
                try writer.writeAll("\\'");
            } else if (f.len == 0) {
                try writer.writeByte('\'');
            } else {
                @compileError("expected {} or {'}, found {" ++ f ++ "}");
            }
        },
        ' ', '!', '#'...'&', '('...'[', ']'...'~' => try writer.writeByte(byte),
        0 => try writer.writeAll("\\0"),
        // Use hex escapes for rest any unprintable characters.
        else => {
            try writer.writeAll("\\x");
            try std.fmt.formatInt(byte, 16, .lower, .{ .width = 2, .fill = '0' }, writer);
        },
    };
}

/// Automatically inserts indentation of written data by keeping
/// track of the current indentation level
///
/// We introduce a new indentation scope with pushIndent/popIndent whenever
/// we potentially want to introduce an indent after the next newline.
///
/// Indentation should only ever increment by one from one line to the next,
/// no matter how many new indentation scopes are introduced. This is done by
/// only realizing the indentation from the most recent scope. As an example:
///
///         while (foo) if (bar)
///             f(x);
///
/// The body of `while` introduces a new indentation scope and the body of
/// `if` also introduces a new indentation scope. When the newline is seen,
/// only the indentation scope of the `if` is realized, and the `while` is
/// not.
///
/// As comments are rendered during space rendering, we need to keep track
/// of the appropriate indentation level for them with pushSpace/popSpace.
/// This should be done whenever a scope that ends in a .newline or a
/// .comma is introduced.
fn AutoIndentingStream(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        pub const WriteError = UnderlyingWriter.Error;
        pub const Writer = std.io.Writer(*Self, WriteError, write);

        pub const IndentType = enum {
            normal,
            after_equals,
            binop,
            field_access,
        };
        const StackElem = struct {
            indent_type: IndentType,
            realized: bool,
        };
        const SpaceElem = struct {
            space: Space,
            indent_count: usize,
        };

        underlying_writer: UnderlyingWriter,

        /// Offset into the source at which formatting has been disabled with
        /// a `zig fmt: off` comment.
        ///
        /// If non-null, the AutoIndentingStream will not write any bytes
        /// to the underlying writer. It will however continue to track the
        /// indentation level.
        disabled_offset: ?usize = null,

        indent_count: usize = 0,
        indent_delta: usize,
        indent_stack: std.ArrayList(StackElem),
        space_stack: std.ArrayList(SpaceElem),
        space_mode: ?usize = null,
        disable_indent_committing: usize = 0,
        current_line_empty: bool = true,
        /// the most recently applied indent
        applied_indent: usize = 0,

        pub fn init(buffer: *std.ArrayList(u8), indent_delta_: usize) Self {
            return .{
                .underlying_writer = buffer.writer(),
                .indent_delta = indent_delta_,
                .indent_stack = std.ArrayList(StackElem).init(buffer.allocator),
                .space_stack = std.ArrayList(SpaceElem).init(buffer.allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.indent_stack.deinit();
            self.space_stack.deinit();
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0) {
                return @as(usize, 0);
            }

            try self.applyIndent();
            return self.writeNoIndent(bytes);
        }

        // Change the indent delta without changing the final indentation level
        pub fn setIndentDelta(self: *Self, new_indent_delta: usize) void {
            if (self.indent_delta == new_indent_delta) {
                return;
            } else if (self.indent_delta > new_indent_delta) {
                assert(self.indent_delta % new_indent_delta == 0);
                self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
            } else {
                // assert that the current indentation (in spaces) in a multiple of the new delta
                assert((self.indent_count * self.indent_delta) % new_indent_delta == 0);
                self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
            }
            self.indent_delta = new_indent_delta;
        }

        fn writeNoIndent(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0) {
                return @as(usize, 0);
            }

            if (self.disabled_offset == null) {
                try self.underlying_writer.writeAll(bytes);
            }
            if (bytes[bytes.len - 1] == '\n') {
                self.resetLine();
            }
            return bytes.len;
        }

        pub fn insertNewline(self: *Self) WriteError!void {
            _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;

            if (self.disable_indent_committing > 0) {
                return;
            }

            if (self.indent_stack.items.len > 0) {
                // By default, we realize the most recent indentation scope.
                var to_realize = self.indent_stack.items.len - 1;

                if (self.indent_stack.items.len >= 2 and
                    self.indent_stack.items[to_realize - 1].indent_type == .after_equals and
                    self.indent_stack.items[to_realize - 1].realized and
                    self.indent_stack.items[to_realize].indent_type == .binop)
                {
                    // If we are in a .binop scope and our direct parent is .after_equals, don't indent.
                    // This ensures correct indentation in the below example:
                    //
                    //        const foo =
                    //            (x >= 'a' and x <= 'z') or         //<-- we are here
                    //            (x >= 'A' and x <= 'Z');
                    //
                    return;
                }

                if (self.indent_stack.items[to_realize].indent_type == .field_access) {
                    // Only realize the top-most field_access in a chain.
                    while (to_realize > 0 and self.indent_stack.items[to_realize - 1].indent_type == .field_access)
                        to_realize -= 1;
                }

                if (self.indent_stack.items[to_realize].realized) {
                    return;
                }
                self.indent_stack.items[to_realize].realized = true;
                self.indent_count += 1;
            }
        }

        /// Disables indentation level changes during the next newlines until re-enabled.
        pub fn disableIndentCommitting(self: *Self) void {
            self.disable_indent_committing += 1;
        }

        pub fn enableIndentCommitting(self: *Self) void {
            assert(self.disable_indent_committing > 0);
            self.disable_indent_committing -= 1;
        }

        pub fn pushSpace(self: *Self, space: Space) !void {
            try self.space_stack.append(.{ .space = space, .indent_count = self.indent_count });
        }

        pub fn popSpace(self: *Self) void {
            _ = self.space_stack.pop();
        }

        /// Sets current indentation level to be the same as that of the last pushSpace.
        pub fn enableSpaceMode(self: *Self, space: Space) void {
            if (self.space_stack.items.len == 0) {
                return;
            }
            const curr = self.space_stack.getLast();
            if (curr.space != space) {
                return;
            }
            self.space_mode = curr.indent_count;
        }

        pub fn disableSpaceMode(self: *Self) void {
            self.space_mode = null;
        }

        pub fn lastSpaceModeIndent(self: *Self) usize {
            if (self.space_stack.items.len == 0) {
                return 0;
            }
            return self.space_stack.getLast().indent_count * self.indent_delta;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) WriteError!void {
            if (!self.current_line_empty) {
                try self.insertNewline();
            }
        }

        /// Push default indentation
        /// Doesn't actually write any indentation.
        /// Just primes the stream to be able to write the correct indentation if it needs to.
        pub fn pushIndent(self: *Self, indent_type: IndentType) !void {
            try self.indent_stack.append(.{ .indent_type = indent_type, .realized = false });
        }

        /// Forces an indentation level to be realized.
        pub fn forcePushIndent(self: *Self, indent_type: IndentType) !void {
            try self.indent_stack.append(.{ .indent_type = indent_type, .realized = true });
            self.indent_count += 1;
        }

        pub fn popIndent(self: *Self) void {
            if (self.indent_stack.pop().realized) {
                assert(self.indent_count > 0);
                self.indent_count -= 1;
            }
        }

        pub fn indentStackEmpty(self: *Self) bool {
            return self.indent_stack.items.len == 0;
        }

        /// Writes ' ' bytes if the current line is empty
        fn applyIndent(self: *Self) WriteError!void {
            const current_indent = self.currentIndent();
            if (self.current_line_empty and current_indent > 0) {
                if (self.disabled_offset == null) {
                    try self.underlying_writer.writeByteNTimes(' ', current_indent);
                }
                self.applied_indent = current_indent;
            }
            self.current_line_empty = false;
        }

        /// Checks to see if the most recent indentation exceeds the currently pushed indents
        pub fn isLineOverIndented(self: *Self) bool {
            if (self.current_line_empty) {
                return false;
            }
            return self.applied_indent > self.currentIndent();
        }

        fn currentIndent(self: *Self) usize {
            const indent_count = self.space_mode orelse self.indent_count;
            return indent_count * self.indent_delta;
        }
    };
}
