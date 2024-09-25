const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const meta = std.meta;
const Ast = @import("Ast.zig");
const Token = @import("Tokenizer.zig").Token;
const primitives = @import("primitives.zig");

const indent_delta = 4;
const asm_indent_delta = 2;

pub const Error = Allocator.Error;

const Ais = AutoIndentingStream(std.ArrayList(u8).Writer);

pub const Fixups = struct {
    /// The key is the mut token (`var`/`const`) of the variable declaration
    /// that should have a `_ = foo;` inserted afterwards.
    unused_var_decls: std.AutoHashMapUnmanaged(Ast.TokenIndex, void) = .{},
    /// The functions in this unordered set of AST fn decl nodes will render
    /// with a function body of `@trap()` instead, with all parameters
    /// discarded.
    gut_functions: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .{},
    /// These global declarations will be omitted.
    omit_nodes: std.AutoHashMapUnmanaged(Ast.Node.Index, void) = .{},
    /// These expressions will be replaced with the string value.
    replace_nodes_with_string: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .{},
    /// The string value will be inserted directly after the node.
    append_string_after_node: std.AutoHashMapUnmanaged(Ast.Node.Index, []const u8) = .{},
    /// These nodes will be replaced with a different node.
    replace_nodes_with_node: std.AutoHashMapUnmanaged(Ast.Node.Index, Ast.Node.Index) = .{},
    /// Change all identifier names matching the key to be value instead.
    rename_identifiers: std.StringArrayHashMapUnmanaged([]const u8) = .{},

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

    pub fn deinit(f: *Fixups, gpa: Allocator) void {
        f.unused_var_decls.deinit(gpa);
        f.gut_functions.deinit(gpa);
        f.omit_nodes.deinit(gpa);
        f.replace_nodes_with_string.deinit(gpa);
        f.append_string_after_node.deinit(gpa);
        f.replace_nodes_with_node.deinit(gpa);
        f.rename_identifiers.deinit(gpa);
        f.* = undefined;
    }
};

const Render = struct {
    gpa: Allocator,
    ais: *Ais,
    tree: Ast,
    fixups: Fixups,
};

pub fn renderTree(buffer: *std.ArrayList(u8), tree: Ast, fixups: Fixups) Error!void {
    assert(tree.errors.len == 0); // Cannot render an invalid tree.
    var auto_indenting_stream = Ais{
        .indent_delta = indent_delta,
        .underlying_writer = buffer.writer(),
    };
    var r: Render = .{
        .gpa = buffer.allocator,
        .ais = &auto_indenting_stream,
        .tree = tree,
        .fixups = fixups,
    };

    // Render all the line comments at the beginning of the file.
    const comment_end_loc = tree.tokens.items(.start)[0];
    _ = try renderComments(&r, 0, comment_end_loc);

    try renderMembers(&r, tree.rootDecls());

    if (auto_indenting_stream.disabled_offset) |disabled_offset| {
        try writeFixingWhitespace(auto_indenting_stream.underlying_writer, tree.source[disabled_offset..]);
    }
}

/// Render all members in the given slice, keeping empty lines where appropriate
fn renderMembers(r: *Render, members: []const Ast.Node.Index) Error!void {
    // const tree = r.tree;
    if (members.len == 0) return;
    try renderMember(r, members[0], .newline);
    for (members[1..]) |member| {
        try renderExtraNewline(r, member);
        try renderMember(r, member, .newline);
    }
}

fn renderMember(
    r: *Render,
    decl: Ast.Node.Index,
    space: Space,
) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    const datas = tree.nodes.items(.data);
    if (r.fixups.omit_nodes.contains(decl)) return;
    switch (tree.nodes.items(.tag)[decl]) {
        .fn_decl => {
            // Some examples:
            // pub extern "foo" fn ...
            // export fn ...
            const fn_proto = datas[decl].lhs;
            const fn_token = main_tokens[fn_proto];
            // Go back to the first token we should render here.
            var i = fn_token;
            while (i > 0) {
                i -= 1;
                switch (token_tags[i]) {
                    // .keyword_pub,
                    .string_literal,
                    => continue,

                    else => {
                        i += 1;
                        break;
                    },
                }
            }
            while (i < fn_token) : (i += 1) {
                try renderToken(r, i, .space);
            }
            assert(datas[decl].rhs != 0);
            try renderExpression(r, fn_proto, .space);
            const body_node = datas[decl].rhs;
            if (r.fixups.gut_functions.contains(decl)) {
                ais.pushIndent();
                const lbrace = tree.nodes.items(.main_token)[body_node];
                try renderToken(r, lbrace, .newline);
                try discardAllParams(r, fn_proto);
                try ais.writer().writeAll("@trap();");
                ais.popIndent();
                try ais.insertNewline();
                try renderToken(r, tree.lastToken(body_node), space); // rbrace
            } else if (r.fixups.unused_var_decls.count() != 0) {
                ais.pushIndentNextLine();
                const lbrace = tree.nodes.items(.main_token)[body_node];
                try renderToken(r, lbrace, .newline);

                // var fn_proto_buf: [1]Ast.Node.Index = undefined;
                // const full_fn_proto = tree.fullFnProto(&fn_proto_buf, fn_proto).?;
                // var it = full_fn_proto.iterate(&tree);
                // while (it.next()) |param| {
                //     const name_ident = param.name_token.?;
                //     assert(token_tags[name_ident] == .identifier);
                //     if (r.fixups.unused_var_decls.contains(name_ident)) {
                //         const w = ais.writer();
                //         try w.writeAll("_ = ");
                //         try w.writeAll(tokenSliceForRender(r.tree, name_ident));
                //         try w.writeAll(";\n");
                //     }
                // }
                var statements_buf: [2]Ast.Node.Index = undefined;
                const statements = switch (node_tags[body_node]) {
                    .block_two,
                    => b: {
                        statements_buf = .{ datas[body_node].lhs, datas[body_node].rhs };
                        if (datas[body_node].lhs == 0) {
                            break :b statements_buf[0..0];
                        } else if (datas[body_node].rhs == 0) {
                            break :b statements_buf[0..1];
                        } else {
                            break :b statements_buf[0..2];
                        }
                    },
                    .block,
                    => tree.extra_data[datas[body_node].lhs..datas[body_node].rhs],

                    else => {
                        // TODO
                        std.log.debug("Unexpected node tag: {}\n", .{node_tags[body_node]});
                        // unreachable,
                        return;
                    },
                };
                return finishRenderBlock(r, body_node, statements, space);
            } else {
                return renderExpression(r, body_node, space);
            }
        },
        .fn_proto_one,
        .fn_proto,
        => {
            // Extern function prototypes are parsed as these tags.
            // Go back to the first token we should render here.
            const fn_token = main_tokens[decl];
            var i = fn_token;
            while (i > 0) {
                i -= 1;
                switch (token_tags[i]) {
                    // .keyword_pub,
                    .string_literal,
                    => continue,

                    else => {
                        i += 1;
                        break;
                    },
                }
            }
            while (i < fn_token) : (i += 1) {
                try renderToken(r, i, .space);
            }
            try renderExpression(r, decl, .none);
            return renderToken(r, tree.lastToken(decl) + 1, space); // semicolon
        },

        .var_decl,
        .mut_var_decl,
        => {
            return renderVarDecl(r, tree.fullVarDecl(decl).?, .semicolon);
        },

        // .container_field_init,
        // .container_field_align,
        // .container_field,
        // => return renderContainerField(r, container, tree.fullContainerField(decl).?, space),

        .root => unreachable,
        else => {
            // TODO
            try renderExpression(r, decl, space);
            // unreachable,
            return;
        },
    }
}

/// Render all expressions in the slice, keeping empty lines where appropriate
fn renderExpressions(r: *Render, expressions: []const Ast.Node.Index, space: Space) Error!void {
    if (expressions.len == 0) return;
    try renderExpression(r, expressions[0], space);
    for (expressions[1..]) |expression| {
        try renderExtraNewline(r, expression);
        try renderExpression(r, expression, space);
    }
}

fn renderExpression(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    const node_tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);
    if (r.fixups.replace_nodes_with_string.get(node)) |replacement| {
        try ais.writer().writeAll(replacement);
        try renderOnlySpace(r, space);
        return;
    } else if (r.fixups.replace_nodes_with_node.get(node)) |replacement| {
        return renderExpression(r, replacement, space);
    }
    switch (node_tags[node]) {
        .identifier => {
            const token_index = main_tokens[node];
            return renderIdentifier(r, token_index, space, .preserve_when_shadowing);
        },

        .int_literal,
        .float_literal,
        // .unreachable_literal,
        .string_literal,
        => return renderToken(r, main_tokens[node], space),

        .multiline_string_literal => {
            var locked_indents = ais.lockOneShotIndent();
            try ais.maybeInsertNewline();

            var i = datas[node].lhs;
            while (i <= datas[node].rhs) : (i += 1) try renderToken(r, i, .newline);

            while (locked_indents > 0) : (locked_indents -= 1) ais.popIndent();

            switch (space) {
                .none, .space, .newline, .skip => {},
                .semicolon => {},
                .comma => if (token_tags[i] == .comma) try renderToken(r, i, .newline),
                .comma_space => if (token_tags[i] == .comma) try renderToken(r, i, .space),
            }
        },

        // .error_value => {
        //     try renderToken(r, main_tokens[node], .none);
        //     try renderToken(r, main_tokens[node] + 1, .none);
        //     return renderIdentifier(r, main_tokens[node] + 2, space, .eagerly_unquote);
        // },

        .block_two,
        => {
            const statements = [2]Ast.Node.Index{ datas[node].lhs, datas[node].rhs };
            if (datas[node].lhs == 0) {
                return renderBlock(r, node, statements[0..0], space);
            } else if (datas[node].rhs == 0) {
                return renderBlock(r, node, statements[0..1], space);
            } else {
                return renderBlock(r, node, statements[0..2], space);
            }
        },
        .block,
        => {
            const statements = tree.extra_data[datas[node].lhs..datas[node].rhs];
            return renderBlock(r, node, statements, space);
        },

        .field_access => {
            const main_token = main_tokens[node];
            const field_access = datas[node];

            try renderExpression(r, field_access.lhs, .none);

            // Allow a line break between the lhs and the dot if the lhs and rhs
            // are on different lines.
            const lhs_last_token = tree.lastToken(field_access.lhs);
            const same_line = tree.tokensOnSameLine(lhs_last_token, main_token + 1);
            if (!same_line) {
                if (!hasComment(tree, lhs_last_token, main_token)) try ais.insertNewline();
                ais.pushIndentOneShot();
            }

            try renderToken(r, main_token, .none); // .

            // This check ensures that zag() is indented in the following example:
            // const x = foo
            //     .bar()
            //     . // comment
            //     zag();
            if (!same_line and hasComment(tree, main_token, main_token + 1)) {
                ais.pushIndentOneShot();
            }

            return renderIdentifier(r, field_access.rhs, space, .eagerly_unquote); // field
        },

        .match_range,
        => {
            const infix = datas[node];
            try renderExpression(r, infix.lhs, .none);
            try renderToken(r, main_tokens[node], .none);
            return renderExpression(r, infix.rhs, space);
        },
        // .for_range => {
        //     const infix = datas[node];
        //     try renderExpression(r, infix.lhs, .none);
        //     if (infix.rhs != 0) {
        //         try renderToken(r, main_tokens[node], .none);
        //         return renderExpression(r, infix.rhs, space);
        //     } else {
        //         return renderToken(r, main_tokens[node], space);
        //     }
        // },

        .add,
        .assign,
        .assign_bit_and,
        .assign_bit_or,
        .assign_shl,
        .assign_shr,
        .assign_bit_xor,
        .assign_div,
        .assign_sub,
        .assign_mod,
        .assign_add,
        .assign_mul,
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
        .optional_fallback,
        => {
            const infix = datas[node];
            try renderExpression(r, infix.lhs, .space);
            const op_token = main_tokens[node];
            if (tree.tokensOnSameLine(op_token, op_token + 1)) {
                try renderToken(r, op_token, .space);
            } else {
                ais.pushIndent();
                try renderToken(r, op_token, .newline);
                ais.popIndent();
            }
            ais.pushIndentOneShot();
            return renderExpression(r, infix.rhs, space);
        },

        // .assign_destructure => {
        //     const full = tree.assignDestructure(node);
        //     if (full.comptime_token) |comptime_token| {
        //         try renderToken(r, comptime_token, .space);
        //     }

        //     for (full.ast.variables, 0..) |variable_node, i| {
        //         const variable_space: Space = if (i == full.ast.variables.len - 1) .space else .comma_space;
        //         switch (node_tags[variable_node]) {
        //             .var_decl,
        //             .mut_var_decl,
        //             => {
        //                 try renderVarDecl(r, tree.fullVarDecl(variable_node).?, true, variable_space);
        //             },
        //             else => try renderExpression(r, variable_node, variable_space),
        //         }
        //     }
        //     if (tree.tokensOnSameLine(full.ast.equal_token, full.ast.equal_token + 1)) {
        //         try renderToken(r, full.ast.equal_token, .space);
        //     } else {
        //         ais.pushIndent();
        //         try renderToken(r, full.ast.equal_token, .newline);
        //         ais.popIndent();
        //     }
        //     ais.pushIndentOneShot();
        //     return renderExpression(r, full.ast.value_expr, space);
        // },

        .bit_not,
        .bool_not,
        .negation,
        .optional_type,
        => {
            try renderToken(r, main_tokens[node], .none);
            return renderExpression(r, datas[node].lhs, space);
        },

        .slice_type,
        => {
            // TODO
            // return renderArrayType(r, tree.fullArrayType(node).?, space);
        },

        .slice_init_one,
        .slice_init_dot_two,
        .slice_init_dot,
        .slice_init,
        => {
            // TODO
            // var elements: [2]Ast.Node.Index = undefined;
            // return renderArrayInit(r, tree.fullArrayInit(&elements, node).?, space);
        },

        // .struct_init_one,
        // .struct_init_one_comma,
        // .struct_init_dot_two,
        // .struct_init_dot_two_comma,
        // .struct_init_dot,
        // .struct_init_dot_comma,
        // .struct_init,
        // .struct_init_comma,
        // => {
        //     var buf: [2]Ast.Node.Index = undefined;
        //     return renderStructInit(r, node, tree.fullStructInit(&buf, node).?, space);
        // },

        .call_one,
        .call,
        => {
            return renderCall(r, tree.fullCall(node).?, space);
        },

        .slice_access => {
            const suffix = datas[node];
            const lbracket = tree.firstToken(suffix.rhs) - 1;
            const rbracket = tree.lastToken(suffix.rhs) + 1;
            const one_line = tree.tokensOnSameLine(lbracket, rbracket);
            const inner_space = if (one_line) Space.none else Space.newline;
            try renderExpression(r, suffix.lhs, .none);
            ais.pushIndentNextLine();
            try renderToken(r, lbracket, inner_space); // [
            try renderExpression(r, suffix.rhs, inner_space);
            ais.popIndent();
            return renderToken(r, rbracket, space); // ]
        },

        .slice_open,
        .slice,
        => return renderSlice(r, node, tree.fullSlice(node).?, space),

        .unwrap_optional => {
            try renderExpression(r, datas[node].lhs, .none);
            try renderToken(r, main_tokens[node], .none);
            return renderToken(r, datas[node].rhs, space);
        },

        .@"break", .@"continue" => {
            const main_token = main_tokens[node];
            const label_token = datas[node].lhs;
            const target = datas[node].rhs;
            if (label_token == 0 and target == 0) {
                try renderToken(r, main_token, space); // break/continue
            } else if (label_token == 0 and target != 0) {
                try renderToken(r, main_token, .space); // break/continue
                try renderExpression(r, target, space);
            } else if (label_token != 0 and target == 0) {
                try renderToken(r, main_token, .space); // break/continue
                try renderToken(r, label_token - 1, .none); // :
                try renderIdentifier(r, label_token, space, .eagerly_unquote); // identifier
            } else if (label_token != 0 and target != 0) {
                try renderToken(r, main_token, .space); // break/continue
                try renderToken(r, label_token - 1, .none); // :
                try renderIdentifier(r, label_token, .space, .eagerly_unquote); // identifier
                try renderExpression(r, target, space);
            }
        },

        .@"return" => {
            if (datas[node].lhs != 0) {
                try renderToken(r, main_tokens[node], .space);
                try renderExpression(r, datas[node].lhs, space);
            } else {
                try renderToken(r, main_tokens[node], space);
            }
        },

        .grouped_expression => {
            try renderToken(r, main_tokens[node], .none); // lparen
            ais.pushIndentOneShot();
            try renderExpression(r, datas[node].lhs, .none);
            return renderToken(r, datas[node].rhs, space); // rparen
        },

        .fn_proto_one,
        .fn_proto,
        => {
            // TODO
            // return renderFnProto(r, tree.fullFnProto(node).?, space);
        },

        .match => {
            // TODO
            // const full = tree.matchFull(node);

            // if (full.label_token) |label_token| {
            //     try renderIdentifier(r, label_token, .none, .eagerly_unquote); // label
            //     try renderToken(r, label_token + 1, .space); // :
            // }

            // const rparen = tree.lastToken(full.ast.condition) + 1;

            // try renderToken(r, full.ast.switch_token, .space); // switch
            // try renderToken(r, full.ast.switch_token + 1, .none); // (
            // try renderExpression(r, full.ast.condition, .none); // condition expression
            // try renderToken(r, rparen, .space); // )

            // ais.pushIndentNextLine();
            // if (full.ast.cases.len == 0) {
            //     try renderToken(r, rparen + 1, .none); // {
            // } else {
            //     try renderToken(r, rparen + 1, .newline); // {
            //     try renderExpressions(r, full.ast.cases, .comma);
            // }
            // ais.popIndent();
            // return renderToken(r, tree.lastToken(node), space); // }
        },

        .match_case_one,
        .match_case,
        => {
            return renderMatchCase(r, tree.fullMatchCase(node).?, space);
        },

        // .while_simple,
        // .while_cont,
        // .@"while",
        // => return renderWhile(r, tree.fullWhile(node).?, space),

        // .for_simple,
        // .@"for",
        // => return renderFor(r, tree.fullFor(node).?, space),

        .@"if",
        .if_else,
        => {
            return renderIf(r, tree.fullIf(node).?, space);
        },

        // .enum_literal => {
        //     try renderToken(r, main_tokens[node] - 1, .none); // .
        //     return renderIdentifier(r, main_tokens[node], space, .eagerly_unquote); // name
        // },

        .fn_decl => unreachable,
        // .container_field => unreachable,
        // .container_field_init => unreachable,
        // .container_field_align => unreachable,
        .root => unreachable,
        .var_decl => unreachable,
        .mut_var_decl => unreachable,

        .no_op => {},
        .loop => {},
        .pipe => {},
        .lambda => {},
    }
}

/// Same as `renderExpression`, but afterwards looks for any
/// append_string_after_node fixups to apply
fn renderExpressionFixup(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const ais = r.ais;
    try renderExpression(r, node, space);
    if (r.fixups.append_string_after_node.get(node)) |bytes| {
        try ais.writer().writeAll(bytes);
    }
}

fn renderArrayType(
    r: *Render,
    array_type: Ast.full.ArrayType,
    space: Space,
) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const rbracket = tree.firstToken(array_type.ast.elem_type) - 1;
    const one_line = tree.tokensOnSameLine(array_type.ast.lbracket, rbracket);
    const inner_space = if (one_line) Space.none else Space.newline;
    ais.pushIndentNextLine();
    try renderToken(r, array_type.ast.lbracket, inner_space); // lbracket
    try renderExpression(r, array_type.ast.elem_count, inner_space);
    if (array_type.ast.sentinel != 0) {
        try renderToken(r, tree.firstToken(array_type.ast.sentinel) - 1, inner_space); // colon
        try renderExpression(r, array_type.ast.sentinel, inner_space);
    }
    ais.popIndent();
    try renderToken(r, rbracket, .none); // rbracket
    return renderExpression(r, array_type.ast.elem_type, space);
}

fn renderPtrType(r: *Render, ptr_type: Ast.full.PtrType, space: Space) Error!void {
    const tree = r.tree;
    switch (ptr_type.size) {
        .One => {
            // Since ** tokens exist and the same token is shared by two
            // nested pointer types, we check to see if we are the parent
            // in such a relationship. If so, skip rendering anything for
            // this pointer type and rely on the child to render our asterisk
            // as well when it renders the ** token.
            if (tree.tokens.items(.tag)[ptr_type.ast.main_token] == .asterisk_asterisk and
                ptr_type.ast.main_token == tree.nodes.items(.main_token)[ptr_type.ast.child_type])
            {
                return renderExpression(r, ptr_type.ast.child_type, space);
            }
            try renderToken(r, ptr_type.ast.main_token, .none); // asterisk
        },
        .Many => {
            if (ptr_type.ast.sentinel == 0) {
                try renderToken(r, ptr_type.ast.main_token, .none); // lbracket
                try renderToken(r, ptr_type.ast.main_token + 1, .none); // asterisk
                try renderToken(r, ptr_type.ast.main_token + 2, .none); // rbracket
            } else {
                try renderToken(r, ptr_type.ast.main_token, .none); // lbracket
                try renderToken(r, ptr_type.ast.main_token + 1, .none); // asterisk
                try renderToken(r, ptr_type.ast.main_token + 2, .none); // colon
                try renderExpression(r, ptr_type.ast.sentinel, .none);
                try renderToken(r, tree.lastToken(ptr_type.ast.sentinel) + 1, .none); // rbracket
            }
        },
        .C => {
            try renderToken(r, ptr_type.ast.main_token, .none); // lbracket
            try renderToken(r, ptr_type.ast.main_token + 1, .none); // asterisk
            try renderToken(r, ptr_type.ast.main_token + 2, .none); // c
            try renderToken(r, ptr_type.ast.main_token + 3, .none); // rbracket
        },
        .Slice => {
            if (ptr_type.ast.sentinel == 0) {
                try renderToken(r, ptr_type.ast.main_token, .none); // lbracket
                try renderToken(r, ptr_type.ast.main_token + 1, .none); // rbracket
            } else {
                try renderToken(r, ptr_type.ast.main_token, .none); // lbracket
                try renderToken(r, ptr_type.ast.main_token + 1, .none); // colon
                try renderExpression(r, ptr_type.ast.sentinel, .none);
                try renderToken(r, tree.lastToken(ptr_type.ast.sentinel) + 1, .none); // rbracket
            }
        },
    }

    if (ptr_type.allowzero_token) |allowzero_token| {
        try renderToken(r, allowzero_token, .space);
    }

    if (ptr_type.ast.align_node != 0) {
        const align_first = tree.firstToken(ptr_type.ast.align_node);
        try renderToken(r, align_first - 2, .none); // align
        try renderToken(r, align_first - 1, .none); // lparen
        try renderExpression(r, ptr_type.ast.align_node, .none);
        if (ptr_type.ast.bit_range_start != 0) {
            assert(ptr_type.ast.bit_range_end != 0);
            try renderToken(r, tree.firstToken(ptr_type.ast.bit_range_start) - 1, .none); // colon
            try renderExpression(r, ptr_type.ast.bit_range_start, .none);
            try renderToken(r, tree.firstToken(ptr_type.ast.bit_range_end) - 1, .none); // colon
            try renderExpression(r, ptr_type.ast.bit_range_end, .none);
            try renderToken(r, tree.lastToken(ptr_type.ast.bit_range_end) + 1, .space); // rparen
        } else {
            try renderToken(r, tree.lastToken(ptr_type.ast.align_node) + 1, .space); // rparen
        }
    }

    if (ptr_type.ast.addrspace_node != 0) {
        const addrspace_first = tree.firstToken(ptr_type.ast.addrspace_node);
        try renderToken(r, addrspace_first - 2, .none); // addrspace
        try renderToken(r, addrspace_first - 1, .none); // lparen
        try renderExpression(r, ptr_type.ast.addrspace_node, .none);
        try renderToken(r, tree.lastToken(ptr_type.ast.addrspace_node) + 1, .space); // rparen
    }

    if (ptr_type.const_token) |const_token| {
        try renderToken(r, const_token, .space);
    }

    if (ptr_type.volatile_token) |volatile_token| {
        try renderToken(r, volatile_token, .space);
    }

    try renderExpression(r, ptr_type.ast.child_type, space);
}

fn renderSlice(
    r: *Render,
    slice_node: Ast.Node.Index,
    slice: Ast.full.Slice,
    space: Space,
) Error!void {
    _ = r;
    _ = slice_node;
    _ = slice;
    _ = space;
    // TODO
    // const tree = r.tree;
    // const node_tags = tree.nodes.items(.tag);
    // const after_start_space_bool = nodeCausesSliceOpSpace(node_tags[slice.ast.start]) or
    //     if (slice.ast.end != 0) nodeCausesSliceOpSpace(node_tags[slice.ast.end]) else false;
    // const after_start_space = if (after_start_space_bool) Space.space else Space.none;
    // const after_dots_space = if (slice.ast.end != 0)
    //     after_start_space
    // else if (slice.ast.sentinel != 0) Space.space else Space.none;

    // try renderExpression(r, slice.ast.sliced, .none);
    // try renderToken(r, slice.ast.lbracket, .none); // lbracket

    // const start_last = tree.lastToken(slice.ast.start);
    // try renderExpression(r, slice.ast.start, after_start_space);
    // try renderToken(r, start_last + 1, after_dots_space); // ellipsis2 ("..")

    // if (slice.ast.end != 0) {
    //     const after_end_space = if (slice.ast.sentinel != 0) Space.space else Space.none;
    //     try renderExpression(r, slice.ast.end, after_end_space);
    // }

    // if (slice.ast.sentinel != 0) {
    //     try renderToken(r, tree.firstToken(slice.ast.sentinel) - 1, .none); // colon
    //     try renderExpression(r, slice.ast.sentinel, .none);
    // }

    // try renderToken(r, tree.lastToken(slice_node), space); // rbracket
}

fn renderVarDecl(
    r: *Render,
    var_decl: Ast.full.VarDecl,
    /// `comma_space` and `space` are used for destructure LHS decls.
    space: Space,
) Error!void {
    try renderVarDeclWithoutFixups(r, var_decl, space);
    // TODO
    // if (r.fixups.unused_var_decls.contains(var_decl.ast.mut_token + 1)) {
    //     // Discard the variable like this: `_ = foo;`
    //     const w = r.ais.writer();
    //     try w.writeAll("_ = ");
    //     try w.writeAll(tokenSliceForRender(r.tree, var_decl.ast.mut_token + 1));
    //     try w.writeAll(";\n");
    // }
}

fn renderVarDeclWithoutFixups(
    r: *Render,
    var_decl: Ast.full.VarDecl,
    /// `comma_space` and `space` are used for destructure LHS decls.
    space: Space,
) Error!void {
    const tree = r.tree;
    const ais = r.ais;

    try renderToken(r, var_decl.ast.let_token, .space);
    if (var_decl.ast.mut_token) |mut_token| {
        try renderToken(r, mut_token, .space);
    }
    const first_token = var_decl.ast.mut_token orelse var_decl.ast.let_token;

    if (var_decl.ast.type_node != 0 or var_decl.ast.init_node != 0) {
        const name_space = if (var_decl.ast.type_node == 0 and var_decl.ast.init_node != 0)
            Space.space
        else
            Space.none;

        try renderIdentifier(r, first_token + 1, name_space, .preserve_when_shadowing); // name
    } else {
        return renderIdentifier(r, first_token + 1, space, .preserve_when_shadowing); // name
    }

    if (var_decl.ast.type_node != 0) {
        try renderToken(r, first_token + 2, Space.space); // :
        if (var_decl.ast.init_node != 0) {
            try renderExpression(r, var_decl.ast.type_node, .space);
        } else {
            return renderExpression(r, var_decl.ast.type_node, space);
        }
    }

    assert(var_decl.ast.init_node != 0);

    const eq_token = tree.firstToken(var_decl.ast.init_node) - 1;
    const eq_space: Space = if (tree.tokensOnSameLine(eq_token, eq_token + 1)) .space else .newline;
    {
        ais.pushIndent();
        try renderToken(r, eq_token, eq_space); // =
        ais.popIndent();
    }
    ais.pushIndentOneShot();
    return renderExpression(r, var_decl.ast.init_node, space); // ;
}

fn renderIf(r: *Render, if_node: Ast.full.If, space: Space) Error!void {
    return renderWhile(r, .{
        .ast = .{
            .while_token = if_node.ast.if_token,
            .cond_expr = if_node.ast.cond_expr,
            .cont_expr = 0,
            .then_expr = if_node.ast.then_expr,
            .else_expr = if_node.ast.else_expr,
        },
        // .label_token = null,
        // .payload_token = if_node.payload_token,
        .else_token = if_node.else_token,
        // .error_token = if_node.error_token,
    }, space);
}

/// Note that this function is additionally used to render if expressions, with
/// respective values set to null.
fn renderWhile(r: *Render, while_node: Ast.full.While, space: Space) Error!void {
    const tree = r.tree;
    // const token_tags = tree.tokens.items(.tag);

    // if (while_node.label_token) |label| {
    //     try renderIdentifier(r, label, .none, .eagerly_unquote); // label
    //     try renderToken(r, label + 1, .space); // :
    // }

    try renderToken(r, while_node.ast.while_token, .space); // if/for/while
    try renderToken(r, while_node.ast.while_token + 1, .none); // lparen
    try renderExpression(r, while_node.ast.cond_expr, .none); // condition

    var last_prefix_token = tree.lastToken(while_node.ast.cond_expr) + 1; // rparen

    // if (while_node.payload_token) |payload_token| {
    //     try renderToken(r, last_prefix_token, .space);
    //     try renderToken(r, payload_token - 1, .none); // |
    //     const ident = blk: {
    //         if (token_tags[payload_token] == .asterisk) {
    //             try renderToken(r, payload_token, .none); // *
    //             break :blk payload_token + 1;
    //         } else {
    //             break :blk payload_token;
    //         }
    //     };
    //     try renderIdentifier(r, ident, .none, .preserve_when_shadowing); // identifier
    //     const pipe = blk: {
    //         if (token_tags[ident + 1] == .comma) {
    //             try renderToken(r, ident + 1, .space); // ,
    //             try renderIdentifier(r, ident + 2, .none, .preserve_when_shadowing); // index
    //             break :blk ident + 3;
    //         } else {
    //             break :blk ident + 1;
    //         }
    //     };
    //     last_prefix_token = pipe;
    // }

    if (while_node.ast.cont_expr != 0) {
        try renderToken(r, last_prefix_token, .space);
        const lparen = tree.firstToken(while_node.ast.cont_expr) - 1;
        try renderToken(r, lparen - 1, .space); // :
        try renderToken(r, lparen, .none); // lparen
        try renderExpression(r, while_node.ast.cont_expr, .none);
        last_prefix_token = tree.lastToken(while_node.ast.cont_expr) + 1; // rparen
    }

    try renderThenElse(
        r,
        last_prefix_token,
        while_node.ast.then_expr,
        while_node.else_token,
        null, //while_node.error_token,
        while_node.ast.else_expr,
        space,
    );
}

fn renderThenElse(
    r: *Render,
    last_prefix_token: Ast.TokenIndex,
    then_expr: Ast.Node.Index,
    else_token: Ast.TokenIndex,
    maybe_error_token: ?Ast.TokenIndex,
    else_expr: Ast.Node.Index,
    space: Space,
) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const node_tags = tree.nodes.items(.tag);
    const then_expr_is_block = nodeIsBlock(node_tags[then_expr]);
    const indent_then_expr = !then_expr_is_block and
        !tree.tokensOnSameLine(last_prefix_token, tree.firstToken(then_expr));
    if (indent_then_expr or (then_expr_is_block and ais.isLineOverIndented())) {
        ais.pushIndentNextLine();
        try renderToken(r, last_prefix_token, .newline);
        ais.popIndent();
    } else {
        try renderToken(r, last_prefix_token, .space);
    }

    if (else_expr != 0) {
        if (indent_then_expr) {
            ais.pushIndent();
            try renderExpression(r, then_expr, .newline);
            ais.popIndent();
        } else {
            try renderExpression(r, then_expr, .space);
        }

        var last_else_token = else_token;

        if (maybe_error_token) |error_token| {
            try renderToken(r, else_token, .space); // else
            try renderToken(r, error_token - 1, .none); // |
            try renderIdentifier(r, error_token, .none, .preserve_when_shadowing); // identifier
            last_else_token = error_token + 1; // |
        }

        const indent_else_expr = indent_then_expr and
            !nodeIsBlock(node_tags[else_expr]) and
            !nodeIsIfForWhileMatch(node_tags[else_expr]);
        if (indent_else_expr) {
            ais.pushIndentNextLine();
            try renderToken(r, last_else_token, .newline);
            ais.popIndent();
            try renderExpressionIndented(r, else_expr, space);
        } else {
            try renderToken(r, last_else_token, .space);
            try renderExpression(r, else_expr, space);
        }
    } else {
        if (indent_then_expr) {
            try renderExpressionIndented(r, then_expr, space);
        } else {
            try renderExpression(r, then_expr, space);
        }
    }
}

fn renderFor(r: *Render, for_node: Ast.full.For, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);

    if (for_node.label_token) |label| {
        try renderIdentifier(r, label, .none, .eagerly_unquote); // label
        try renderToken(r, label + 1, .space); // :
    }

    if (for_node.inline_token) |inline_token| {
        try renderToken(r, inline_token, .space); // inline
    }

    try renderToken(r, for_node.ast.for_token, .space); // if/for/while

    const lparen = for_node.ast.for_token + 1;
    try renderParamList(r, lparen, for_node.ast.inputs, .space);

    var cur = for_node.payload_token;
    const pipe = std.mem.indexOfScalarPos(std.zig.Token.Tag, token_tags, cur, .pipe).?;
    if (token_tags[pipe - 1] == .comma) {
        ais.pushIndentNextLine();
        try renderToken(r, cur - 1, .newline); // |
        while (true) {
            if (token_tags[cur] == .asterisk) {
                try renderToken(r, cur, .none); // *
                cur += 1;
            }
            try renderIdentifier(r, cur, .none, .preserve_when_shadowing); // identifier
            cur += 1;
            if (token_tags[cur] == .comma) {
                try renderToken(r, cur, .newline); // ,
                cur += 1;
            }
            if (token_tags[cur] == .pipe) {
                break;
            }
        }
        ais.popIndent();
    } else {
        try renderToken(r, cur - 1, .none); // |
        while (true) {
            if (token_tags[cur] == .asterisk) {
                try renderToken(r, cur, .none); // *
                cur += 1;
            }
            try renderIdentifier(r, cur, .none, .preserve_when_shadowing); // identifier
            cur += 1;
            if (token_tags[cur] == .comma) {
                try renderToken(r, cur, .space); // ,
                cur += 1;
            }
            if (token_tags[cur] == .pipe) {
                break;
            }
        }
    }

    try renderThenElse(
        r,
        cur,
        for_node.ast.then_expr,
        for_node.else_token,
        null,
        for_node.ast.else_expr,
        space,
    );
}

fn renderFnProto(r: *Render, fn_proto: Ast.full.FnProto, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    const token_starts = tree.tokens.items(.start);

    const after_fn_token = fn_proto.ast.fn_token + 1;
    const lparen = if (token_tags[after_fn_token] == .identifier) blk: {
        try renderToken(r, fn_proto.ast.fn_token, .space); // fn
        try renderIdentifier(r, after_fn_token, .none, .preserve_when_shadowing); // name
        break :blk after_fn_token + 1;
    } else blk: {
        try renderToken(r, fn_proto.ast.fn_token, .space); // fn
        break :blk fn_proto.ast.fn_token + 1;
    };
    assert(token_tags[lparen] == .l_paren);

    const maybe_bang = tree.firstToken(fn_proto.ast.return_type) - 1;
    const rparen = blk: {
        // These may appear in any order, so we have to check the token_starts array
        // to find out which is first.
        var rparen = if (token_tags[maybe_bang] == .bang) maybe_bang - 1 else maybe_bang;
        var smallest_start = token_starts[maybe_bang];
        if (fn_proto.ast.align_expr != 0) {
            const tok = tree.firstToken(fn_proto.ast.align_expr) - 3;
            const start = token_starts[tok];
            if (start < smallest_start) {
                rparen = tok;
                smallest_start = start;
            }
        }
        if (fn_proto.ast.addrspace_expr != 0) {
            const tok = tree.firstToken(fn_proto.ast.addrspace_expr) - 3;
            const start = token_starts[tok];
            if (start < smallest_start) {
                rparen = tok;
                smallest_start = start;
            }
        }
        if (fn_proto.ast.section_expr != 0) {
            const tok = tree.firstToken(fn_proto.ast.section_expr) - 3;
            const start = token_starts[tok];
            if (start < smallest_start) {
                rparen = tok;
                smallest_start = start;
            }
        }
        if (fn_proto.ast.callconv_expr != 0) {
            const tok = tree.firstToken(fn_proto.ast.callconv_expr) - 3;
            const start = token_starts[tok];
            if (start < smallest_start) {
                rparen = tok;
                smallest_start = start;
            }
        }
        break :blk rparen;
    };
    assert(token_tags[rparen] == .r_paren);

    // The params list is a sparse set that does *not* include anytype or ... parameters.

    const trailing_comma = token_tags[rparen - 1] == .comma;
    if (!trailing_comma and !hasComment(tree, lparen, rparen)) {
        // Render all on one line, no trailing comma.
        try renderToken(r, lparen, .none); // (

        var param_i: usize = 0;
        var last_param_token = lparen;
        while (true) {
            last_param_token += 1;
            switch (token_tags[last_param_token]) {
                // .ellipsis3 => {
                //     try renderToken(r, last_param_token, .none); // ...
                //     break;
                // },
                .identifier => {},
                // .keyword_anytype => {
                //     try renderToken(r, last_param_token, .none); // anytype
                //     continue;
                // },
                .r_paren => break,
                .comma => {
                    try renderToken(r, last_param_token, .space); // ,
                    continue;
                },
                else => {}, // Parameter type without a name.
            }
            if (token_tags[last_param_token] == .identifier and
                token_tags[last_param_token + 1] == .colon)
            {
                try renderIdentifier(r, last_param_token, .none, .preserve_when_shadowing); // name
                last_param_token += 1;
                try renderToken(r, last_param_token, .space); // :
                last_param_token += 1;
            }
            if (token_tags[last_param_token] == .keyword_anytype) {
                try renderToken(r, last_param_token, .none); // anytype
                continue;
            }
            const param = fn_proto.ast.params[param_i];
            param_i += 1;
            try renderExpression(r, param, .none);
            last_param_token = tree.lastToken(param);
        }
    } else {
        // One param per line.
        ais.pushIndent();
        try renderToken(r, lparen, .newline); // (

        var param_i: usize = 0;
        var last_param_token = lparen;
        while (true) {
            last_param_token += 1;
            switch (token_tags[last_param_token]) {
                .identifier => {},
                // .keyword_anytype => {
                //     try renderToken(r, last_param_token, .comma); // anytype
                //     if (token_tags[last_param_token + 1] == .comma)
                //         last_param_token += 1;
                //     continue;
                // },
                .r_paren => break,
                else => {}, // Parameter type without a name.
            }
            if (token_tags[last_param_token] == .identifier and
                token_tags[last_param_token + 1] == .colon)
            {
                try renderIdentifier(r, last_param_token, .none, .preserve_when_shadowing); // name
                last_param_token += 1;
                try renderToken(r, last_param_token, .space); // :
                last_param_token += 1;
            }
            if (token_tags[last_param_token] == .keyword_anytype) {
                try renderToken(r, last_param_token, .comma); // anytype
                if (token_tags[last_param_token + 1] == .comma)
                    last_param_token += 1;
                continue;
            }
            const param = fn_proto.ast.params[param_i];
            param_i += 1;
            try renderExpression(r, param, .comma);
            last_param_token = tree.lastToken(param);
            if (token_tags[last_param_token + 1] == .comma) last_param_token += 1;
        }
        ais.popIndent();
    }

    try renderToken(r, rparen, .space); // )

    if (fn_proto.ast.align_expr != 0) {
        const align_lparen = tree.firstToken(fn_proto.ast.align_expr) - 1;
        const align_rparen = tree.lastToken(fn_proto.ast.align_expr) + 1;

        try renderToken(r, align_lparen - 1, .none); // align
        try renderToken(r, align_lparen, .none); // (
        try renderExpression(r, fn_proto.ast.align_expr, .none);
        try renderToken(r, align_rparen, .space); // )
    }

    if (fn_proto.ast.addrspace_expr != 0) {
        const align_lparen = tree.firstToken(fn_proto.ast.addrspace_expr) - 1;
        const align_rparen = tree.lastToken(fn_proto.ast.addrspace_expr) + 1;

        try renderToken(r, align_lparen - 1, .none); // addrspace
        try renderToken(r, align_lparen, .none); // (
        try renderExpression(r, fn_proto.ast.addrspace_expr, .none);
        try renderToken(r, align_rparen, .space); // )
    }

    if (fn_proto.ast.section_expr != 0) {
        const section_lparen = tree.firstToken(fn_proto.ast.section_expr) - 1;
        const section_rparen = tree.lastToken(fn_proto.ast.section_expr) + 1;

        try renderToken(r, section_lparen - 1, .none); // section
        try renderToken(r, section_lparen, .none); // (
        try renderExpression(r, fn_proto.ast.section_expr, .none);
        try renderToken(r, section_rparen, .space); // )
    }

    const is_callconv_inline = mem.eql(u8, "Inline", tree.tokenSlice(tree.nodes.items(.main_token)[fn_proto.ast.callconv_expr]));
    const is_declaration = fn_proto.name_token != null;
    if (fn_proto.ast.callconv_expr != 0 and !(is_declaration and is_callconv_inline)) {
        const callconv_lparen = tree.firstToken(fn_proto.ast.callconv_expr) - 1;
        const callconv_rparen = tree.lastToken(fn_proto.ast.callconv_expr) + 1;

        try renderToken(r, callconv_lparen - 1, .none); // callconv
        try renderToken(r, callconv_lparen, .none); // (
        try renderExpression(r, fn_proto.ast.callconv_expr, .none);
        try renderToken(r, callconv_rparen, .space); // )
    }

    if (token_tags[maybe_bang] == .bang) {
        try renderToken(r, maybe_bang, .none); // !
    }
    return renderExpression(r, fn_proto.ast.return_type, space);
}

fn renderMatchCase(
    r: *Render,
    switch_case: Ast.full.MatchCase,
    space: Space,
) Error!void {
    const tree = r.tree;
    const token_tags = tree.tokens.items(.tag);
    const trailing_comma = token_tags[switch_case.ast.arrow_token - 1] == .comma;
    const has_comment_before_arrow = blk: {
        if (switch_case.ast.values.len == 0) break :blk false;
        break :blk hasComment(tree, tree.firstToken(switch_case.ast.values[0]), switch_case.ast.arrow_token);
    };

    // Render everything before the arrow
    if (switch_case.ast.values.len == 0) {
        try renderToken(r, switch_case.ast.arrow_token - 1, .space); // else keyword
    } else if (trailing_comma or has_comment_before_arrow) {
        // Render each value on a new line
        try renderExpressions(r, switch_case.ast.values, .comma);
    } else {
        // Render on one line
        for (switch_case.ast.values) |value_expr| {
            try renderExpression(r, value_expr, .comma_space);
        }
    }

    // Render the arrow and everything after it
    try renderToken(r, switch_case.ast.arrow_token, .space);

    try renderExpression(r, switch_case.ast.target_expr, space);
}

fn renderBlock(
    r: *Render,
    block_node: Ast.Node.Index,
    statements: []const Ast.Node.Index,
    space: Space,
) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    const lbrace = tree.nodes.items(.main_token)[block_node];

    if (token_tags[lbrace - 1] == .colon and
        token_tags[lbrace - 2] == .identifier)
    {
        try renderIdentifier(r, lbrace - 2, .none, .eagerly_unquote); // identifier
        try renderToken(r, lbrace - 1, .space); // :
    }
    ais.pushIndentNextLine();
    if (statements.len == 0) {
        try renderToken(r, lbrace, .none);
        ais.popIndent();
        try renderToken(r, tree.lastToken(block_node), space); // rbrace
        return;
    }
    try renderToken(r, lbrace, .newline);
    return finishRenderBlock(r, block_node, statements, space);
}

fn finishRenderBlock(
    r: *Render,
    block_node: Ast.Node.Index,
    statements: []const Ast.Node.Index,
    space: Space,
) Error!void {
    const tree = r.tree;
    const node_tags = tree.nodes.items(.tag);
    const ais = r.ais;
    for (statements, 0..) |stmt, i| {
        if (i != 0) try renderExtraNewline(r, stmt);
        if (r.fixups.omit_nodes.contains(stmt)) continue;
        switch (node_tags[stmt]) {
            .var_decl,
            .mut_var_decl,
            => try renderVarDecl(r, tree.fullVarDecl(stmt).?, .semicolon),

            else => try renderExpression(r, stmt, .semicolon),
        }
    }
    ais.popIndent();

    try renderToken(r, tree.lastToken(block_node), space); // rbrace
}

fn renderStructInit(
    r: *Render,
    struct_node: Ast.Node.Index,
    struct_init: Ast.full.StructInit,
    space: Space,
) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    if (struct_init.ast.type_expr == 0) {
        try renderToken(r, struct_init.ast.lbrace - 1, .none); // .
    } else {
        try renderExpression(r, struct_init.ast.type_expr, .none); // T
    }
    if (struct_init.ast.fields.len == 0) {
        ais.pushIndentNextLine();
        try renderToken(r, struct_init.ast.lbrace, .none); // lbrace
        ais.popIndent();
        return renderToken(r, struct_init.ast.lbrace + 1, space); // rbrace
    }

    const rbrace = tree.lastToken(struct_node);
    const trailing_comma = token_tags[rbrace - 1] == .comma;
    if (trailing_comma or hasComment(tree, struct_init.ast.lbrace, rbrace)) {
        // Render one field init per line.
        ais.pushIndentNextLine();
        try renderToken(r, struct_init.ast.lbrace, .newline);

        try renderToken(r, struct_init.ast.lbrace + 1, .none); // .
        try renderIdentifier(r, struct_init.ast.lbrace + 2, .space, .eagerly_unquote); // name
        // Don't output a space after the = if expression is a multiline string,
        // since then it will start on the next line.
        const nodes = tree.nodes.items(.tag);
        const field_node = struct_init.ast.fields[0];
        const expr = nodes[field_node];
        var space_after_equal: Space = if (expr == .multiline_string_literal) .none else .space;
        try renderToken(r, struct_init.ast.lbrace + 3, space_after_equal); // =
        try renderExpressionFixup(r, field_node, .comma);

        for (struct_init.ast.fields[1..]) |field_init| {
            const init_token = tree.firstToken(field_init);
            try renderExtraNewlineToken(r, init_token - 3);
            try renderToken(r, init_token - 3, .none); // .
            try renderIdentifier(r, init_token - 2, .space, .eagerly_unquote); // name
            space_after_equal = if (nodes[field_init] == .multiline_string_literal) .none else .space;
            try renderToken(r, init_token - 1, space_after_equal); // =
            try renderExpressionFixup(r, field_init, .comma);
        }

        ais.popIndent();
    } else {
        // Render all on one line, no trailing comma.
        try renderToken(r, struct_init.ast.lbrace, .space);

        for (struct_init.ast.fields) |field_init| {
            const init_token = tree.firstToken(field_init);
            try renderToken(r, init_token - 3, .none); // .
            try renderIdentifier(r, init_token - 2, .space, .eagerly_unquote); // name
            try renderToken(r, init_token - 1, .space); // =
            try renderExpressionFixup(r, field_init, .comma_space);
        }
    }

    return renderToken(r, rbrace, space);
}

fn renderArrayInit(
    r: *Render,
    array_init: Ast.full.ArrayInit,
    space: Space,
) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const gpa = r.gpa;
    const token_tags = tree.tokens.items(.tag);

    if (array_init.ast.type_expr == 0) {
        try renderToken(r, array_init.ast.lbrace - 1, .none); // .
    } else {
        try renderExpression(r, array_init.ast.type_expr, .none); // T
    }

    if (array_init.ast.elements.len == 0) {
        ais.pushIndentNextLine();
        try renderToken(r, array_init.ast.lbrace, .none); // lbrace
        ais.popIndent();
        return renderToken(r, array_init.ast.lbrace + 1, space); // rbrace
    }

    const last_elem = array_init.ast.elements[array_init.ast.elements.len - 1];
    const last_elem_token = tree.lastToken(last_elem);
    const trailing_comma = token_tags[last_elem_token + 1] == .comma;
    const rbrace = if (trailing_comma) last_elem_token + 2 else last_elem_token + 1;
    assert(token_tags[rbrace] == .r_brace);

    if (array_init.ast.elements.len == 1) {
        const only_elem = array_init.ast.elements[0];
        const first_token = tree.firstToken(only_elem);
        if (token_tags[first_token] != .multiline_string_literal_line and
            !anythingBetween(tree, last_elem_token, rbrace))
        {
            try renderToken(r, array_init.ast.lbrace, .none);
            try renderExpression(r, only_elem, .none);
            return renderToken(r, rbrace, space);
        }
    }

    const contains_comment = hasComment(tree, array_init.ast.lbrace, rbrace);
    const contains_multiline_string = hasMultilineString(tree, array_init.ast.lbrace, rbrace);

    if (!trailing_comma and !contains_comment and !contains_multiline_string) {
        // Render all on one line, no trailing comma.
        if (array_init.ast.elements.len == 1) {
            // If there is only one element, we don't use spaces
            try renderToken(r, array_init.ast.lbrace, .none);
            try renderExpression(r, array_init.ast.elements[0], .none);
        } else {
            try renderToken(r, array_init.ast.lbrace, .space);
            for (array_init.ast.elements) |elem| {
                try renderExpression(r, elem, .comma_space);
            }
        }
        return renderToken(r, last_elem_token + 1, space); // rbrace
    }

    ais.pushIndentNextLine();
    try renderToken(r, array_init.ast.lbrace, .newline);

    var expr_index: usize = 0;
    while (true) {
        const row_size = rowSize(tree, array_init.ast.elements[expr_index..], rbrace);
        const row_exprs = array_init.ast.elements[expr_index..];
        // A place to store the width of each expression and its column's maximum
        const widths = try gpa.alloc(usize, row_exprs.len + row_size);
        defer gpa.free(widths);
        @memset(widths, 0);

        const expr_newlines = try gpa.alloc(bool, row_exprs.len);
        defer gpa.free(expr_newlines);
        @memset(expr_newlines, false);

        const expr_widths = widths[0..row_exprs.len];
        const column_widths = widths[row_exprs.len..];

        // Find next row with trailing comment (if any) to end the current section.
        const section_end = sec_end: {
            var this_line_first_expr: usize = 0;
            var this_line_size = rowSize(tree, row_exprs, rbrace);
            for (row_exprs, 0..) |expr, i| {
                // Ignore comment on first line of this section.
                if (i == 0) continue;
                const expr_last_token = tree.lastToken(expr);
                if (tree.tokensOnSameLine(tree.firstToken(row_exprs[0]), expr_last_token))
                    continue;
                // Track start of line containing comment.
                if (!tree.tokensOnSameLine(tree.firstToken(row_exprs[this_line_first_expr]), expr_last_token)) {
                    this_line_first_expr = i;
                    this_line_size = rowSize(tree, row_exprs[this_line_first_expr..], rbrace);
                }

                const maybe_comma = expr_last_token + 1;
                if (token_tags[maybe_comma] == .comma) {
                    if (hasSameLineComment(tree, maybe_comma))
                        break :sec_end i - this_line_size + 1;
                }
            }
            break :sec_end row_exprs.len;
        };
        expr_index += section_end;

        const section_exprs = row_exprs[0..section_end];

        var sub_expr_buffer = std.ArrayList(u8).init(gpa);
        defer sub_expr_buffer.deinit();

        const sub_expr_buffer_starts = try gpa.alloc(usize, section_exprs.len + 1);
        defer gpa.free(sub_expr_buffer_starts);

        var auto_indenting_stream = Ais{
            .indent_delta = indent_delta,
            .underlying_writer = sub_expr_buffer.writer(),
        };
        var sub_render: Render = .{
            .gpa = r.gpa,
            .ais = &auto_indenting_stream,
            .tree = r.tree,
            .fixups = r.fixups,
        };

        // Calculate size of columns in current section
        var column_counter: usize = 0;
        var single_line = true;
        var contains_newline = false;
        for (section_exprs, 0..) |expr, i| {
            const start = sub_expr_buffer.items.len;
            sub_expr_buffer_starts[i] = start;

            if (i + 1 < section_exprs.len) {
                try renderExpression(&sub_render, expr, .none);
                const width = sub_expr_buffer.items.len - start;
                const this_contains_newline = mem.indexOfScalar(u8, sub_expr_buffer.items[start..], '\n') != null;
                contains_newline = contains_newline or this_contains_newline;
                expr_widths[i] = width;
                expr_newlines[i] = this_contains_newline;

                if (!this_contains_newline) {
                    const column = column_counter % row_size;
                    column_widths[column] = @max(column_widths[column], width);

                    const expr_last_token = tree.lastToken(expr) + 1;
                    const next_expr = section_exprs[i + 1];
                    column_counter += 1;
                    if (!tree.tokensOnSameLine(expr_last_token, tree.firstToken(next_expr))) single_line = false;
                } else {
                    single_line = false;
                    column_counter = 0;
                }
            } else {
                try renderExpression(&sub_render, expr, .comma);
                const width = sub_expr_buffer.items.len - start - 2;
                const this_contains_newline = mem.indexOfScalar(u8, sub_expr_buffer.items[start .. sub_expr_buffer.items.len - 1], '\n') != null;
                contains_newline = contains_newline or this_contains_newline;
                expr_widths[i] = width;
                expr_newlines[i] = contains_newline;

                if (!contains_newline) {
                    const column = column_counter % row_size;
                    column_widths[column] = @max(column_widths[column], width);
                }
            }
        }
        sub_expr_buffer_starts[section_exprs.len] = sub_expr_buffer.items.len;

        // Render exprs in current section.
        column_counter = 0;
        for (section_exprs, 0..) |expr, i| {
            const start = sub_expr_buffer_starts[i];
            const end = sub_expr_buffer_starts[i + 1];
            const expr_text = sub_expr_buffer.items[start..end];
            if (!expr_newlines[i]) {
                try ais.writer().writeAll(expr_text);
            } else {
                var by_line = std.mem.splitScalar(u8, expr_text, '\n');
                var last_line_was_empty = false;
                try ais.writer().writeAll(by_line.first());
                while (by_line.next()) |line| {
                    if (std.mem.startsWith(u8, line, "//") and last_line_was_empty) {
                        try ais.insertNewline();
                    } else {
                        try ais.maybeInsertNewline();
                    }
                    last_line_was_empty = (line.len == 0);
                    try ais.writer().writeAll(line);
                }
            }

            if (i + 1 < section_exprs.len) {
                const next_expr = section_exprs[i + 1];
                const comma = tree.lastToken(expr) + 1;

                if (column_counter != row_size - 1) {
                    if (!expr_newlines[i] and !expr_newlines[i + 1]) {
                        // Neither the current or next expression is multiline
                        try renderToken(r, comma, .space); // ,
                        assert(column_widths[column_counter % row_size] >= expr_widths[i]);
                        const padding = column_widths[column_counter % row_size] - expr_widths[i];
                        try ais.writer().writeByteNTimes(' ', padding);

                        column_counter += 1;
                        continue;
                    }
                }

                if (single_line and row_size != 1) {
                    try renderToken(r, comma, .space); // ,
                    continue;
                }

                column_counter = 0;
                try renderToken(r, comma, .newline); // ,
                try renderExtraNewline(r, next_expr);
            }
        }

        if (expr_index == array_init.ast.elements.len)
            break;
    }

    ais.popIndent();
    return renderToken(r, rbrace, space); // rbrace
}

fn renderCall(
    r: *Render,
    call: Ast.full.Call,
    space: Space,
) Error!void {
    try renderExpression(r, call.ast.fn_expr, .none);
    try renderParamList(r, call.ast.lparen, call.ast.args, space);
}

fn renderParamList(
    r: *Render,
    lparen: Ast.TokenIndex,
    params: []const Ast.Node.Index,
    space: Space,
) Error!void {
    _ = r;
    _ = lparen;
    _ = params;
    _ = space;
    // TODO
    // const tree = r.tree;
    // const ais = r.ais;
    // const token_tags = tree.tokens.items(.tag);

    // if (params.len == 0) {
    //     ais.pushIndentNextLine();
    //     try renderToken(r, lparen, .none);
    //     ais.popIndent();
    //     return renderToken(r, lparen + 1, space); // )
    // }

    // const last_param = params[params.len - 1];
    // const after_last_param_tok = tree.lastToken(last_param); // + 1
    // if (token_tags[after_last_param_tok] == .comma) {
    //     ais.pushIndentNextLine();
    //     try renderToken(r, lparen, .newline); // (
    //     for (params, 0..) |param_node, i| {
    //         if (i + 1 < params.len) {
    //             try renderExpression(r, param_node, .none);

    //             // Unindent the comma for multiline string literals.
    //             const is_multiline_string =
    //                 token_tags[tree.firstToken(param_node)] == .multiline_string_literal_line;
    //             if (is_multiline_string) ais.popIndent();

    //             const comma = tree.lastToken(param_node) + 1;
    //             try renderToken(r, comma, .newline); // ,

    //             if (is_multiline_string) ais.pushIndent();

    //             try renderExtraNewline(r, params[i + 1]);
    //         } else {
    //             try renderExpression(r, param_node, .comma);
    //         }
    //     }
    //     ais.popIndent();
    //     return renderToken(r, after_last_param_tok + 1, space); // )
    // }

    // try renderToken(r, lparen, .none); // (

    // for (params, 0..) |param_node, i| {
    //     const first_param_token = tree.firstToken(param_node);
    //     if (token_tags[first_param_token] == .multiline_string_literal_line or
    //         hasSameLineComment(tree, first_param_token - 1))
    //     {
    //         ais.pushIndentOneShot();
    //     }
    //     try renderExpression(r, param_node, .none);

    //     if (i + 1 < params.len) {
    //         const comma = tree.lastToken(param_node) + 1;
    //         const next_multiline_string =
    //             token_tags[tree.firstToken(params[i + 1])] == .multiline_string_literal_line;
    //         const comma_space: Space = if (next_multiline_string) .none else .space;
    //         try renderToken(r, comma, comma_space);
    //     }
    // }

    // return renderToken(r, after_last_param_tok, space); // )
}

/// Renders the given expression indented, popping the indent before rendering
/// any following line comments
fn renderExpressionIndented(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_starts = tree.tokens.items(.start);
    const token_tags = tree.tokens.items(.tag);

    ais.pushIndent();

    var last_token = tree.lastToken(node);
    const punctuation = switch (space) {
        .none, .space, .newline, .skip => false,
        .comma => true,
        .comma_space => token_tags[last_token + 1] == .comma,
        .semicolon => false,
    };

    try renderExpression(r, node, if (punctuation) .none else .skip);

    switch (space) {
        .none, .space, .newline, .skip => {},
        .comma => {
            if (token_tags[last_token + 1] == .comma) {
                try renderToken(r, last_token + 1, .skip);
                last_token += 1;
            } else {
                try ais.writer().writeByte(',');
            }
        },
        .comma_space => if (token_tags[last_token + 1] == .comma) {
            try renderToken(r, last_token + 1, .skip);
            last_token += 1;
        },
        .semicolon => {},
        // .semicolon => if (token_tags[last_token + 1] == .semicolon) {
        //     try renderToken(r, last_token + 1, .skip);
        //     last_token += 1;
        // },
    }

    ais.popIndent();

    if (space == .skip) return;

    const comment_start = token_starts[last_token] + tokenSliceForRender(tree, last_token).len;
    const comment = try renderComments(r, comment_start, token_starts[last_token + 1]);

    if (!comment) switch (space) {
        .none => {},
        .space,
        .comma_space,
        => try ais.writer().writeByte(' '),
        .newline,
        .comma,
        .semicolon,
        => try ais.insertNewline(),
        .skip => unreachable,
    };
}

/// Render an expression, and the comma that follows it, if it is present in the source.
/// If a comma is present, and `space` is `Space.comma`, render only a single comma.
fn renderExpressionComma(r: *Render, node: Ast.Node.Index, space: Space) Error!void {
    const tree = r.tree;
    const token_tags = tree.tokens.items(.tag);
    const maybe_comma = tree.lastToken(node) + 1;
    if (token_tags[maybe_comma] == .comma and space != .comma) {
        try renderExpression(r, node, .none);
        return renderToken(r, maybe_comma, space);
    } else {
        return renderExpression(r, node, space);
    }
}

/// Render a token, and the comma that follows it, if it is present in the source.
/// If a comma is present, and `space` is `Space.comma`, render only a single comma.
fn renderTokenComma(r: *Render, token: Ast.TokenIndex, space: Space) Error!void {
    const tree = r.tree;
    const token_tags = tree.tokens.items(.tag);
    const maybe_comma = token + 1;
    if (token_tags[maybe_comma] == .comma and space != .comma) {
        try renderToken(r, token, .none);
        return renderToken(r, maybe_comma, space);
    } else {
        return renderToken(r, token, space);
    }
}

/// Render an identifier, and the comma that follows it, if it is present in the source.
/// If a comma is present, and `space` is `Space.comma`, render only a single comma.
fn renderIdentifierComma(r: *Render, token: Ast.TokenIndex, space: Space, quote: QuoteBehavior) Error!void {
    const tree = r.tree;
    const token_tags = tree.tokens.items(.tag);
    const maybe_comma = token + 1;
    if (token_tags[maybe_comma] == .comma and space != .comma) {
        try renderIdentifier(r, token, .none, quote);
        return renderToken(r, maybe_comma, space);
    } else {
        return renderIdentifier(r, token, space, quote);
    }
}

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
    /// Additionally consume the next token if it is a semicolon.
    /// In either case, a newline will be inserted afterwards.
    semicolon,
    /// Skip rendering whitespace and comments. If this is used, the caller
    /// *must* handle whitespace and comments manually.
    skip,
};

fn renderToken(r: *Render, token_index: Ast.TokenIndex, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const lexeme = tokenSliceForRender(tree, token_index);
    try ais.writer().writeAll(lexeme);
    try renderSpace(r, token_index, lexeme.len, space);
}

fn renderSpace(r: *Render, token_index: Ast.TokenIndex, lexeme_len: usize, space: Space) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    const token_starts = tree.tokens.items(.start);

    const token_start = token_starts[token_index];

    if (space == .skip) return;

    if (space == .comma and token_tags[token_index + 1] != .comma) {
        try ais.writer().writeByte(',');
    }

    const comment = try renderComments(r, token_start + lexeme_len, token_starts[token_index + 1]);
    switch (space) {
        .none => {},
        .space => if (!comment) try ais.writer().writeByte(' '),
        .newline => if (!comment) try ais.insertNewline(),

        .comma => if (token_tags[token_index + 1] == .comma) {
            try renderToken(r, token_index + 1, .newline);
        } else if (!comment) {
            try ais.insertNewline();
        },

        .comma_space => if (token_tags[token_index + 1] == .comma) {
            try renderToken(r, token_index + 1, .space);
        } else if (!comment) {
            try ais.writer().writeByte(' ');
        },

        .semicolon => if (!comment) {
            try ais.insertNewline();
        },

        .skip => unreachable,
    }
}

fn renderOnlySpace(r: *Render, space: Space) Error!void {
    const ais = r.ais;
    switch (space) {
        .none => {},
        .space => try ais.writer().writeByte(' '),
        .newline => try ais.insertNewline(),
        .comma => try ais.writer().writeAll(",\n"),
        .comma_space => try ais.writer().writeAll(", "),
        .semicolon => try ais.writer().writeAll(";\n"),
        .skip => unreachable,
    }
}

const QuoteBehavior = enum {
    preserve_when_shadowing,
    eagerly_unquote,
    eagerly_unquote_except_underscore,
};

fn renderIdentifier(r: *Render, token_index: Ast.TokenIndex, space: Space, quote: QuoteBehavior) Error!void {
    const tree = r.tree;
    const token_tags = tree.tokens.items(.tag);
    assert(token_tags[token_index] == .identifier);
    const lexeme = tokenSliceForRender(tree, token_index);

    if (r.fixups.rename_identifiers.get(lexeme)) |mangled| {
        try r.ais.writer().writeAll(mangled);
        try renderSpace(r, token_index, lexeme.len, space);
        return;
    }

    if (lexeme[0] != '@') {
        return renderToken(r, token_index, space);
    }

    assert(lexeme.len >= 3);
    assert(lexeme[0] == '@');
    assert(lexeme[1] == '\"');
    assert(lexeme[lexeme.len - 1] == '\"');
    const contents = lexeme[2 .. lexeme.len - 1]; // inside the @"" quotation

    // Empty name can't be unquoted.
    if (contents.len == 0) {
        return renderQuotedIdentifier(r, token_index, space, false);
    }

    // Special case for _.
    if (std.zig.isUnderscore(contents)) switch (quote) {
        .eagerly_unquote => return renderQuotedIdentifier(r, token_index, space, true),
        .eagerly_unquote_except_underscore,
        .preserve_when_shadowing,
        => return renderQuotedIdentifier(r, token_index, space, false),
    };

    // Scan the entire name for characters that would (after un-escaping) be illegal in a symbol,
    // i.e. contents don't match: [A-Za-z_][A-Za-z0-9_]*
    var contents_i: usize = 0;
    while (contents_i < contents.len) {
        switch (contents[contents_i]) {
            '0'...'9' => if (contents_i == 0) return renderQuotedIdentifier(r, token_index, space, false),
            'A'...'Z', 'a'...'z', '_' => {},
            '\\' => {
                var esc_offset = contents_i;
                const res = std.zig.string_literal.parseEscapeSequence(contents, &esc_offset);
                switch (res) {
                    .success => |char| switch (char) {
                        '0'...'9' => if (contents_i == 0) return renderQuotedIdentifier(r, token_index, space, false),
                        'A'...'Z', 'a'...'z', '_' => {},
                        else => return renderQuotedIdentifier(r, token_index, space, false),
                    },
                    .failure => return renderQuotedIdentifier(r, token_index, space, false),
                }
                contents_i += esc_offset;
                continue;
            },
            else => return renderQuotedIdentifier(r, token_index, space, false),
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
        for (std.zig.Token.keywords.keys()) |key| {
            if (key.len > longest) longest = key.len;
        }
        break :blk longest;
    };
    var buf: [longest_keyword_or_primitive_len]u8 = undefined;

    contents_i = 0;
    var buf_i: usize = 0;
    while (contents_i < contents.len and buf_i < longest_keyword_or_primitive_len) {
        if (contents[contents_i] == '\\') {
            const res = std.zig.string_literal.parseEscapeSequence(contents, &contents_i).success;
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
        if (!std.zig.isValidId(buf[0..buf_i])) {
            return renderQuotedIdentifier(r, token_index, space, false);
        }
        if (primitives.isPrimitive(buf[0..buf_i])) switch (quote) {
            .eagerly_unquote,
            .eagerly_unquote_except_underscore,
            => return renderQuotedIdentifier(r, token_index, space, true),
            .preserve_when_shadowing => return renderQuotedIdentifier(r, token_index, space, false),
        };
    }

    try renderQuotedIdentifier(r, token_index, space, true);
}

// Renders a @"" quoted identifier, normalizing escapes.
// Unnecessary escapes are un-escaped, and \u escapes are normalized to \x when they fit.
// If unquote is true, the @"" is removed and the result is a bare symbol whose validity is asserted.
fn renderQuotedIdentifier(r: *Render, token_index: Ast.TokenIndex, space: Space, comptime unquote: bool) !void {
    const tree = r.tree;
    const ais = r.ais;
    const token_tags = tree.tokens.items(.tag);
    assert(token_tags[token_index] == .identifier);
    const lexeme = tokenSliceForRender(tree, token_index);
    assert(lexeme.len >= 3 and lexeme[0] == '@');

    if (!unquote) try ais.writer().writeAll("@\"");
    const contents = lexeme[2 .. lexeme.len - 1];
    try renderIdentifierContents(ais.writer(), contents);
    if (!unquote) try ais.writer().writeByte('\"');

    try renderSpace(r, token_index, lexeme.len, space);
}

fn renderIdentifierContents(writer: anytype, bytes: []const u8) !void {
    var pos: usize = 0;
    while (pos < bytes.len) {
        const byte = bytes[pos];
        switch (byte) {
            '\\' => {
                const old_pos = pos;
                const res = std.zig.string_literal.parseEscapeSequence(bytes, &pos);
                const escape_sequence = bytes[old_pos..pos];
                switch (res) {
                    .success => |codepoint| {
                        if (codepoint <= 0x7f) {
                            const buf = [1]u8{@as(u8, @intCast(codepoint))};
                            try std.fmt.format(writer, "{}", .{std.zig.fmtEscapes(&buf)});
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
                try std.fmt.format(writer, "{}", .{std.zig.fmtEscapes(&buf)});
                pos += 1;
            },
            0x80...0xff => {
                try writer.writeByte(byte);
                pos += 1;
            },
        }
    }
}

/// Returns true if there exists a line comment between any of the tokens from
/// `start_token` to `end_token`. This is used to determine if e.g. a
/// fn_proto should be wrapped and have a trailing comma inserted even if
/// there is none in the source.
fn hasComment(tree: Ast, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    _ = tree;
    _ = start_token;
    _ = end_token;
    return false;
    // TODO
    // const token_starts = tree.tokens.items(.start);

    // var i = start_token;
    // while (i < end_token) : (i += 1) {
    //     const start = token_starts[i] + tree.tokenSlice(i).len;
    //     const end = token_starts[i + 1];
    //     if (mem.indexOf(u8, tree.source[start..end], "//") != null) return true;
    // }

    // return false;
}

/// Returns true if there exists a multiline string literal between the start
/// of token `start_token` and the start of token `end_token`.
fn hasMultilineString(tree: Ast, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    const token_tags = tree.tokens.items(.tag);

    for (token_tags[start_token..end_token]) |tag| {
        switch (tag) {
            .multiline_string_literal_line => return true,
            else => continue,
        }
    }

    return false;
}

/// Assumes that start is the first byte past the previous token and
/// that end is the last byte before the next token.
fn renderComments(r: *Render, start: usize, end: usize) Error!bool {
    _ = r;
    _ = start;
    _ = end;
    return false;
    // TODO
    //     const tree = r.tree;
    //     const ais = r.ais;

    //     var index: usize = start;
    //     while (mem.indexOf(u8, tree.source[index..end], "//")) |offset| {
    //         const comment_start = index + offset;

    //         // If there is no newline, the comment ends with EOF
    //         const newline_index = mem.indexOfScalar(u8, tree.source[comment_start..end], '\n');
    //         const newline = if (newline_index) |i| comment_start + i else null;

    //         const untrimmed_comment = tree.source[comment_start .. newline orelse tree.source.len];
    //         const trimmed_comment = mem.trimRight(u8, untrimmed_comment, &std.ascii.whitespace);

    //         // Don't leave any whitespace at the start of the file
    //         if (index != 0) {
    //             if (index == start and mem.containsAtLeast(u8, tree.source[index..comment_start], 2, "\n")) {
    //                 // Leave up to one empty line before the first comment
    //                 try ais.insertNewline();
    //                 try ais.insertNewline();
    //             } else if (mem.indexOfScalar(u8, tree.source[index..comment_start], '\n') != null) {
    //                 // Respect the newline directly before the comment.
    //                 // Note: This allows an empty line between comments
    //                 try ais.insertNewline();
    //             } else if (index == start) {
    //                 // Otherwise if the first comment is on the same line as
    //                 // the token before it, prefix it with a single space.
    //                 try ais.writer().writeByte(' ');
    //             }
    //         }

    //         index = 1 + (newline orelse end - 1);

    //         const comment_content = mem.trimLeft(u8, trimmed_comment["//".len..], &std.ascii.whitespace);
    //         if (ais.disabled_offset != null and mem.eql(u8, comment_content, "zig fmt: on")) {
    //             // Write the source for which formatting was disabled directly
    //             // to the underlying writer, fixing up invalid whitespace.
    //             const disabled_source = tree.source[ais.disabled_offset.?..comment_start];
    //             try writeFixingWhitespace(ais.underlying_writer, disabled_source);
    //             // Write with the canonical single space.
    //             try ais.underlying_writer.writeAll("// zig fmt: on\n");
    //             ais.disabled_offset = null;
    //         } else if (ais.disabled_offset == null and mem.eql(u8, comment_content, "zig fmt: off")) {
    //             // Write with the canonical single space.
    //             try ais.writer().writeAll("// zig fmt: off\n");
    //             ais.disabled_offset = index;
    //         } else {
    //             // Write the comment minus trailing whitespace.
    //             try ais.writer().print("{s}\n", .{trimmed_comment});
    //         }
    //     }

    //     if (index != start and mem.containsAtLeast(u8, tree.source[index - 1 .. end], 2, "\n")) {
    //         // Don't leave any whitespace at the end of the file
    //         if (end != tree.source.len) {
    //             try ais.insertNewline();
    //         }
    //     }

    //     return index != start;
}

fn renderExtraNewline(r: *Render, node: Ast.Node.Index) Error!void {
    return renderExtraNewlineToken(r, r.tree.firstToken(node));
}

/// Check if there is an empty line immediately before the given token. If so, render it.
fn renderExtraNewlineToken(r: *Render, token_index: Ast.TokenIndex) Error!void {
    const tree = r.tree;
    const ais = r.ais;
    const token_starts = tree.tokens.items(.start);
    const token_start = token_starts[token_index];
    if (token_start == 0) return;
    const prev_token_end = if (token_index == 0)
        0
    else
        token_starts[token_index - 1] + tokenSliceForRender(tree, token_index - 1).len;

    // Iterate backwards to the end of the previous token, stopping if a
    // non-whitespace character is encountered or two newlines have been found.
    var i = token_start - 1;
    var newlines: u2 = 0;
    while (std.ascii.isWhitespace(tree.source[i])) : (i -= 1) {
        if (tree.source[i] == '\n') newlines += 1;
        if (newlines == 2) return ais.insertNewline();
        if (i == prev_token_end) break;
    }
}

fn discardAllParams(r: *Render, fn_proto_node: Ast.Node.Index) Error!void {
    _ = r;
    _ = fn_proto_node;
    // const tree = &r.tree;
    // const ais = r.ais;
    // var buf: [1]Ast.Node.Index = undefined;
    // const fn_proto = tree.fullFnProto(&buf, fn_proto_node).?;
    // const token_tags = tree.tokens.items(.tag);
    // var it = fn_proto.iterate(tree);
    // while (it.next()) |param| {
    //     const name_ident = param.name_token.?;
    //     assert(token_tags[name_ident] == .identifier);
    //     const w = ais.writer();
    //     try w.writeAll("_ = ");
    //     try w.writeAll(tokenSliceForRender(r.tree, name_ident));
    //     try w.writeAll(";\n");
    // }
}

fn tokenSliceForRender(tree: Ast, token_index: Ast.TokenIndex) []const u8 {
    return tree.tokenSlice(token_index);
}

fn hasSameLineComment(tree: Ast, token_index: Ast.TokenIndex) bool {
    const token_starts = tree.tokens.items(.start);
    const between_source = tree.source[token_starts[token_index]..token_starts[token_index + 1]];
    for (between_source) |byte| switch (byte) {
        '\n' => return false,
        '/' => return true,
        else => continue,
    };
    return false;
}

/// Returns `true` if and only if there are any tokens or line comments between
/// start_token and end_token.
fn anythingBetween(tree: Ast, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    if (start_token + 1 != end_token) return true;
    const token_starts = tree.tokens.items(.start);
    const between_source = tree.source[token_starts[start_token]..token_starts[start_token + 1]];
    for (between_source) |byte| switch (byte) {
        '/' => return true,
        else => continue,
    };
    return false;
}

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
        .block_two,
        => true,
        else => false,
    };
}

fn nodeIsIfForWhileMatch(tag: Ast.Node.Tag) bool {
    return switch (tag) {
        .@"if",
        .if_else,
        // .@"for",
        // .for_simple,
        // .@"while",
        // .while_simple,
        // .while_cont,
        .match,
        => true,
        else => false,
    };
}

fn nodeCausesSliceOpSpace(tag: Ast.Node.Tag) bool {
    return switch (tag) {
        .add,
        .assign,
        .assign_bit_and,
        .assign_bit_or,
        .assign_shl,
        .assign_shr,
        .assign_bit_xor,
        .assign_div,
        .assign_sub,
        .assign_mod,
        .assign_add,
        .assign_mul,
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
        .error_union,
        .greater_or_equal,
        .greater_than,
        .less_or_equal,
        .less_than,
        .mod,
        .mul,
        .mul_wrap,
        .sub,
        .sub_wrap,
        .optional_fallback,
        => true,

        else => false,
    };
}

// Returns the number of nodes in `exprs` that are on the same line as `rtoken`.
fn rowSize(tree: Ast, exprs: []const Ast.Node.Index, rtoken: Ast.TokenIndex) usize {
    const token_tags = tree.tokens.items(.tag);

    const first_token = tree.firstToken(exprs[0]);
    if (tree.tokensOnSameLine(first_token, rtoken)) {
        const maybe_comma = rtoken - 1;
        if (token_tags[maybe_comma] == .comma)
            return 1;
        return exprs.len; // no newlines
    }

    var count: usize = 1;
    for (exprs, 0..) |expr, i| {
        if (i + 1 < exprs.len) {
            const expr_last_token = tree.lastToken(expr) + 1;
            if (!tree.tokensOnSameLine(expr_last_token, tree.firstToken(exprs[i + 1]))) return count;
            count += 1;
        } else {
            return count;
        }
    }
    unreachable;
}

/// Automatically inserts indentation of written data by keeping
/// track of the current indentation level
fn AutoIndentingStream(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        pub const WriteError = UnderlyingWriter.Error;
        pub const Writer = std.io.Writer(*Self, WriteError, write);

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
        current_line_empty: bool = true,
        /// automatically popped when applied
        indent_one_shot_count: usize = 0,
        /// the most recently applied indent
        applied_indent: usize = 0,
        /// not used until the next line
        indent_next_line: usize = 0,

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

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
            if (bytes.len == 0)
                return @as(usize, 0);

            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n')
                self.resetLine();
            return bytes.len;
        }

        pub fn insertNewline(self: *Self) WriteError!void {
            _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;
            self.indent_next_line = 0;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) WriteError!void {
            if (!self.current_line_empty)
                try self.insertNewline();
        }

        /// Push default indentation
        /// Doesn't actually write any indentation.
        /// Just primes the stream to be able to write the correct indentation if it needs to.
        pub fn pushIndent(self: *Self) void {
            self.indent_count += 1;
        }

        /// Push an indent that is automatically popped after being applied
        pub fn pushIndentOneShot(self: *Self) void {
            self.indent_one_shot_count += 1;
            self.pushIndent();
        }

        /// Turns all one-shot indents into regular indents
        /// Returns number of indents that must now be manually popped
        pub fn lockOneShotIndent(self: *Self) usize {
            const locked_count = self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            return locked_count;
        }

        /// Push an indent that should not take effect until the next line
        pub fn pushIndentNextLine(self: *Self) void {
            self.indent_next_line += 1;
            self.pushIndent();
        }

        pub fn popIndent(self: *Self) void {
            assert(self.indent_count != 0);
            self.indent_count -= 1;

            if (self.indent_next_line > 0)
                self.indent_next_line -= 1;
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

            self.indent_count -= self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            self.current_line_empty = false;
        }

        /// Checks to see if the most recent indentation exceeds the currently pushed indents
        pub fn isLineOverIndented(self: *Self) bool {
            if (self.current_line_empty) return false;
            return self.applied_indent > self.currentIndent();
        }

        fn currentIndent(self: *Self) usize {
            var indent_current: usize = 0;
            if (self.indent_count > 0) {
                const indent_count = self.indent_count - self.indent_next_line;
                indent_current = indent_count * self.indent_delta;
            }
            return indent_current;
        }
    };
}
