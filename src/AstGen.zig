// Originally based on https://github.com/ziglang/zig/blob/master/lib/std/zig/AstGen.zig
// See https://github.com/ziglang/zig/blob/master/LICENSE for additional LICENSE details

const std = @import("std");
const builtin = @import("builtin");
const string_literal = @import("string_literal.zig");
const Ast = @import("Ast.zig");
const Nir = @import("Nir.zig");
const isPrimitive = @import("primitives.zig").isPrimitive;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;
const StringIndexContext = std.hash_map.StringIndexContext;

const AstGen = @This();
const Error = error{AnalysisFail} || Allocator.Error;

ast: *const Ast,
// TODO nodes_need_rl
allocator: Allocator,
/// Used for temporary allocations; freed after AstGen is complete.
/// The resulting NIR code has no references to anything in this arena.
arena: Allocator,
instructions: std.MultiArrayList(Nir.Inst),
string_bytes: std.ArrayListUnmanaged(u8),
extra: std.ArrayListUnmanaged(u32),
/// Tracks the current byte offset within the source file.
/// Used to populate line deltas in the NIR. AstGen maintains
/// this "cursor" throughout the entire AST lowering process in order
/// to avoid starting over the line/column scan for every declaration, which
/// would be O(N^2).
source_offset: u32,
/// Tracks the corresponding line of `source_offset`.
/// This value is absolute.
source_line: u32,
/// Tracks the corresponding column of `source_offset`.
/// This value is absolute.
source_column: u32,
string_table: std.HashMapUnmanaged(u32, void, StringIndexContext, std.hash_map.default_max_load_percentage),
compile_errors: std.ArrayListUnmanaged(Nir.Inst.CompileErrors.Item),
/// The topmost block of the current function.
fn_block: ?*NirGen,
fn_var_args: bool,
/// Whether we are somewhere within a function. If `true`, any container decls may be
/// generic and thus must be tunneled through closure.
within_fn: bool,
/// The return type of the current function. This may be a trivial `Ref`, or
/// otherwise it refers to a `ret_type` instruction.
fn_ret_ty: Nir.Inst.Ref,
/// Maps string table indexes to the first `@import` NIR instruction
/// that uses this string as the operand.
imports: std.AutoArrayHashMapUnmanaged(Nir.NullTerminatedString, Ast.TokenIndex),
/// Used for temporary storage when building payloads.
scratch: std.ArrayListUnmanaged(u32),
/// Whenever a `ref` instruction is needed, it is created and saved in this
/// table instead of being immediately appended to the current block body.
/// Then, when the instruction is being added to the parent block (typically from
/// setBlockBody), if it has a ref_table entry, then the ref instruction is added
/// there. This makes sure two properties are upheld:
/// 1. All pointers to the same locals return the same address. This is required
///    to be compliant with the language specification.
/// 2. `ref` instructions will dominate their uses. This is a required property
///    of NIR.
/// The key is the ref operand; the value is the ref instruction.
ref_table: std.AutoHashMapUnmanaged(Nir.Inst.Index, Nir.Inst.Index),
/// Any information which should trigger invalidation of incremental compilation
/// data should be used to update this hasher. The result is the final source
/// hash of the enclosing declaration/etc.
src_hasher: std.zig.SrcHasher,

pub fn generate(allocator: Allocator, ast: *const Ast) Allocator.Error!Nir {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var astgen: AstGen = .{
        .ast = ast,
        .allocator = allocator,
        .arena = arena.allocator(),
        .instructions = .{},
        .string_bytes = .empty,
        .extra = .empty,
        .source_offset = 0,
        .source_line = 0,
        .source_column = 0,
        .string_table = .empty,
        .compile_errors = .empty,
        .fn_block = null,
        .fn_var_args = false,
        .within_fn = false,
        .fn_ret_ty = .none,
        .imports = .empty,
        .scratch = .empty,
        .ref_table = .empty,
        .src_hasher = undefined,
    };
    defer astgen.deinit();

    // String table index 0 is reserved for `NullTerminatedString.empty`.
    try astgen.string_bytes.append(allocator, 0);

    // We expect at least as many NIR instructions and extra data items as AST nodes.
    try astgen.instructions.ensureTotalCapacity(allocator, ast.nodes.len);

    // First few indexes of extra are reserved and set at the end.
    const reserved_count = @typeInfo(Nir.ExtraIndex).@"enum".fields.len;
    try astgen.extra.ensureTotalCapacity(allocator, ast.nodes.len + reserved_count);
    astgen.extra.items.len += reserved_count;

    var top_scope: Scope.Top = .{};

    var ng_instructions: std.ArrayListUnmanaged(Nir.Inst.Index) = .empty;
    defer ng_instructions.deinit(allocator);
    var gen_scope: NirGen = .{
        .is_comptime = true,
        .parent = &top_scope.base,
        .anon_name_strategy = .parent,
        .decl_node_index = 0,
        .decl_line = 0,
        .astgen = &astgen,
        .instructions = &ng_instructions,
        .instructions_top = 0,
    };

    // The AST -> NIR lowering process assumes an AST that does not have any parse errors.
    if (ast.errors.len != 0) {
        try lowerAstErrors(&astgen);
    } else if (topLevel(&gen_scope)) |struct_decl_ref| {
        assert(struct_decl_ref.toIndex().? == .root);
    } else |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.AnalysisFail => {}, // Handled via compile_errors below.
    }

    const err_index = @intFromEnum(Nir.ExtraIndex.compile_errors);
    if (astgen.compile_errors.items.len == 0) {
        astgen.extra.items[err_index] = 0;
    } else {
        try astgen.extra.ensureUnusedCapacity(allocator, 1 + astgen.compile_errors.items.len *
            @typeInfo(Nir.Inst.CompileErrors.Item).@"struct".fields.len);

        astgen.extra.items[err_index] = astgen.addExtraAssumeCapacity(Nir.Inst.CompileErrors{
            .items_len = @intCast(astgen.compile_errors.items.len),
        });

        for (astgen.compile_errors.items) |item| {
            _ = astgen.addExtraAssumeCapacity(item);
        }
    }

    const imports_index = @intFromEnum(Nir.ExtraIndex.imports);
    if (astgen.imports.count() == 0) {
        astgen.extra.items[imports_index] = 0;
    } else {
        try astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.Imports).@"struct".fields.len +
            astgen.imports.count() * @typeInfo(Nir.Inst.Imports.Item).@"struct".fields.len);

        astgen.extra.items[imports_index] = astgen.addExtraAssumeCapacity(Nir.Inst.Imports{
            .imports_len = @intCast(astgen.imports.count()),
        });

        var it = astgen.imports.iterator();
        while (it.next()) |entry| {
            _ = astgen.addExtraAssumeCapacity(Nir.Inst.Imports.Item{
                .name = entry.key_ptr.*,
                .token = entry.value_ptr.*,
            });
        }
    }

    return .{
        .instructions = astgen.instructions.toOwnedSlice(),
        .string_bytes = try astgen.string_bytes.toOwnedSlice(allocator),
        .extra = try astgen.extra.toOwnedSlice(allocator),
    };
}

fn deinit(self: *AstGen) void {
    self.instructions.deinit(self.allocator);
    self.extra.deinit(self.allocator);
    self.string_bytes.deinit(self.allocator);
    self.string_table.deinit(self.allocator);
    self.compile_errors.deinit(self.allocator);
    self.imports.deinit(self.allocator);
    self.scratch.deinit(self.allocator);
    self.ref_table.deinit(self.allocator);
}

// TODOa: replace setStruct with setRoot
fn topLevel(ng: *NirGen) Error!Nir.Inst.Ref {
    const decls = ng.astgen.ast.rootDecls();
    const decl_inst = try ng.reserveInstructionIndex();

    const scope = &ng.base;
    const node = 0;
    const layout = .auto;
    const backing_int_node = 0;

    if (decls.len == 0) {
        try ng.setStruct(decl_inst, .{
            .src_node = node,
            .layout = layout,
            .captures_len = 0,
            .fields_len = 0,
            .decls_len = 0,
            .has_backing_int = false,
            .known_non_opv = false,
            .known_comptime_only = false,
            .is_tuple = false,
            .any_comptime_fields = false,
            .any_default_inits = false,
            .any_aligned_fields = false,
            .fields_hash = std.zig.hashSrc(@tagName(layout)),
        });
        return decl_inst.toRef();
    }

    const astgen = ng.astgen;
    const allocator = astgen.allocator;
    const ast = astgen.ast;

    var namespace: Scope.Namespace = .{
        .parent = scope,
        .node = node,
        .inst = decl_inst,
        .declaring_ng = ng,
        .maybe_generic = astgen.within_fn,
    };
    defer namespace.deinit(allocator);

    // The struct_decl instruction introduces a scope in which the decls of the struct
    // are in scope, so that field types, alignments, and default value expressions
    // can refer to decls within the struct itself.
    astgen.advanceSourceCursorToNode(node);
    var block_scope: NirGen = .{
        .parent = &namespace.base,
        .decl_node_index = node,
        .decl_line = ng.decl_line,
        .astgen = astgen,
        .is_comptime = true,
        .instructions = ng.instructions,
        .instructions_top = ng.instructions.items.len,
    };
    defer block_scope.unstack();

    const scratch_top = astgen.scratch.items.len;
    defer astgen.scratch.items.len = scratch_top;

    const decl_count = try astgen.scanContainer(&namespace, decls, .@"struct");

    const bits_per_field = 4;
    const max_field_size = 5;
    var wip_members = try WipMembers.init(allocator, &astgen.scratch, decl_count, 0, bits_per_field, max_field_size);
    defer wip_members.deinit();

    // We will use the scratch buffer, starting here, for the bodies:
    //    bodies: { // for every fields_len
    //        field_type_body_inst: Inst, // for each field_type_body_len
    //        align_body_inst: Inst, // for each align_body_len
    //        init_body_inst: Inst, // for each init_body_len
    //    }
    // Note that the scratch buffer is simultaneously being used by WipMembers, however
    // it will not access any elements beyond this point in the ArrayList. It also
    // accesses via the ArrayList items field so it can handle the scratch buffer being
    // reallocated.
    // No defer needed here because it is handled by `wip_members.deinit()` above.
    const bodies_start = astgen.scratch.items.len;

    const old_hasher = astgen.src_hasher;
    defer astgen.src_hasher = old_hasher;
    astgen.src_hasher = std.zig.SrcHasher.init(.{});
    astgen.src_hasher.update(@tagName(layout));
    if (backing_int_node != 0) {
        astgen.src_hasher.update(ast.getNodeSource(backing_int_node));
    }

    for (decls) |top_level_decl| {
        assert(try containerMember(&block_scope, &namespace.base, &wip_members, top_level_decl) == .decl);
    }

    var fields_hash: std.zig.SrcHash = undefined;
    astgen.src_hasher.final(&fields_hash);

    try ng.setStruct(decl_inst, .{
        .src_node = node,
        .layout = layout,
        .captures_len = @intCast(namespace.captures.count()),
        .fields_len = 0,
        .decls_len = decl_count,
        .has_backing_int = false,
        .known_non_opv = false,
        .known_comptime_only = false,
        .is_tuple = false,
        .any_comptime_fields = false,
        .any_default_inits = false,
        .any_aligned_fields = false,
        .fields_hash = fields_hash,
    });

    wip_members.finishBits(bits_per_field);
    const decls_slice = wip_members.declsSlice();
    const fields_slice = wip_members.fieldsSlice();
    const bodies_slice = astgen.scratch.items[bodies_start..];
    try astgen.extra.ensureUnusedCapacity(allocator, 0 + 2 +
        decls_slice.len + namespace.captures.count() + fields_slice.len + bodies_slice.len);
    astgen.extra.appendSliceAssumeCapacity(@ptrCast(namespace.captures.keys()));
    astgen.extra.appendSliceAssumeCapacity(decls_slice);
    astgen.extra.appendSliceAssumeCapacity(fields_slice);
    astgen.extra.appendSliceAssumeCapacity(bodies_slice);

    block_scope.unstack();
    return decl_inst.toRef();
}

const ResultInfo = struct {
    /// The semantics requested for the result location
    rl: Loc,

    /// The "operator" consuming the result location
    ctx: Context = .none,

    /// Turns a `coerced_ty` back into a `ty`. Should be called at branch points
    /// such as if and switch expressions.
    fn br(ri: ResultInfo) ResultInfo {
        return switch (ri.rl) {
            .coerced_ty => |ty| .{
                .rl = .{ .ty = ty },
                .ctx = ri.ctx,
            },
            else => ri,
        };
    }

    fn nirTag(ri: ResultInfo) Nir.Inst.Tag {
        switch (ri.rl) {
            .ty => return switch (ri.ctx) {
                .shift_op => .as_shift_operand,
                else => .as_node,
            },
            else => unreachable,
        }
    }

    const Loc = union(enum) {
        /// The expression is the right-hand side of assignment to `_`. Only the side-effects of the
        /// expression should be generated. The result instruction from the expression must
        /// be ignored.
        discard,
        /// The expression has an inferred type, and it will be evaluated as an rvalue.
        none,
        /// The expression will be coerced into this type, but it will be evaluated as an rvalue.
        ty: Nir.Inst.Ref,
        /// Same as `ty` but it is guaranteed that Sema will additionally perform the coercion,
        /// so no `as` instruction needs to be emitted.
        coerced_ty: Nir.Inst.Ref,
        /// The expression must generate a pointer rather than a value. For example, the left hand side
        /// of an assignment uses this kind of result location.
        ref,
        /// The expression must generate a pointer rather than a value, and the pointer will be coerced
        /// by other code to this type, which is guaranteed by earlier instructions to be a pointer type.
        ref_coerced_ty: Nir.Inst.Ref,
        /// The expression must store its result into this typed pointer. The result instruction
        /// from the expression must be ignored.
        ptr: PtrResultLoc,
        /// The expression must store its result into this allocation, which has an inferred type.
        /// The result instruction from the expression must be ignored.
        /// Always an instruction with tag `alloc_inferred`.
        inferred_ptr: Nir.Inst.Ref,
        /// The expression has a sequence of pointers to store its results into due to a destructure
        /// operation. Each of these pointers may or may not have an inferred type.
        destructure: struct {
            /// The AST node of the destructure operation itself.
            src_node: Ast.Node.Index,
            /// The pointers to store results into.
            components: []const DestructureComponent,
        },

        const DestructureComponent = union(enum) {
            typed_ptr: PtrResultLoc,
            inferred_ptr: Nir.Inst.Ref,
            discard,
        };

        const PtrResultLoc = struct {
            inst: Nir.Inst.Ref,
            src_node: ?Ast.Node.Index = null,
        };

        /// Find the result type for a cast builtin given the result location.
        /// If the location does not have a known result type, returns `null`.
        fn resultType(rl: Loc, ng: *NirGen, node: Ast.Node.Index) !?Nir.Inst.Ref {
            return switch (rl) {
                .discard, .none, .ref, .inferred_ptr, .destructure => null,
                .ty, .coerced_ty => |ty_ref| ty_ref,
                .ref_coerced_ty => |ptr_ty| try ng.addUnNode(.elem_type, ptr_ty, node),
                .ptr => |ptr| {
                    const ptr_ty = try ng.addUnNode(.typeof, ptr.inst, node);
                    return try ng.addUnNode(.elem_type, ptr_ty, node);
                },
            };
        }

        /// Find the result type for a cast builtin given the result location.
        /// If the location does not have a known result type, emits an error on
        /// the given node.
        fn resultTypeForCast(rl: Loc, ng: *NirGen, node: Ast.Node.Index, builtin_name: []const u8) !Nir.Inst.Ref {
            const astgen = ng.astgen;
            if (try rl.resultType(ng, node)) |ty| return ty;
            switch (rl) {
                .destructure => |destructure| return astgen.failNodeNotes(node, "{s} must have a known result type", .{builtin_name}, &.{
                    try astgen.errNoteNode(destructure.src_node, "destructure expressions do not provide a single result type", .{}),
                    try astgen.errNoteNode(node, "use @as to provide explicit result type", .{}),
                }),
                else => return astgen.failNodeNotes(node, "{s} must have a known result type", .{builtin_name}, &.{
                    try astgen.errNoteNode(node, "use @as to provide explicit result type", .{}),
                }),
            }
        }
    };

    const Context = enum {
        /// The expression is the operand to a return expression.
        @"return",
        /// The expression is the input to an error-handling operator (if-else, try, or catch).
        error_handling_expr,
        /// The expression is the right-hand side of a shift operation.
        shift_op,
        /// The expression is an argument in a function call.
        fn_arg,
        /// The expression is the right-hand side of an initializer for a `const` variable
        const_init,
        /// The expression is the right-hand side of an assignment expression.
        assignment,
        /// No specific operator in particular.
        none,
    };
};

const coerced_type_ri: ResultInfo = .{ .rl = .{ .coerced_ty = .type_type } };
const coerced_bool_ri: ResultInfo = .{ .rl = .{ .coerced_ty = .bool_type } };

fn typeExpr(ng: *NirGen, scope: *Scope, type_node: Ast.Node.Index) Error!Nir.Inst.Ref {
    return comptimeExpr(ng, scope, coerced_type_ri, type_node);
}

fn reachableTypeExpr(
    ng: *NirGen,
    scope: *Scope,
    type_node: Ast.Node.Index,
    reachable_node: Ast.Node.Index,
) Error!Nir.Inst.Ref {
    return reachableExprComptime(ng, scope, coerced_type_ri, type_node, reachable_node, true);
}

/// Same as `expr` but fails with a compile error if the result type is `noreturn`.
fn reachableExpr(
    ng: *NirGen,
    scope: *Scope,
    ri: ResultInfo,
    node: Ast.Node.Index,
    reachable_node: Ast.Node.Index,
) Error!Nir.Inst.Ref {
    return reachableExprComptime(ng, scope, ri, node, reachable_node, false);
}

fn reachableExprComptime(
    ng: *NirGen,
    scope: *Scope,
    ri: ResultInfo,
    node: Ast.Node.Index,
    reachable_node: Ast.Node.Index,
    force_comptime: bool,
) Error!Nir.Inst.Ref {
    const result_inst = if (force_comptime)
        try comptimeExpr(ng, scope, ri, node)
    else
        try expr(ng, scope, ri, node);

    if (ng.refIsNoReturn(result_inst)) {
        try ng.astgen.appendErrorNodeNotes(reachable_node, "unreachable code", .{}, &[_]u32{
            try ng.astgen.errNoteNode(node, "control flow is diverted here", .{}),
        });
    }
    return result_inst;
}

// TODO: lvalExpr

/// Turn Nov AST into untyped NIR instructions.
/// When `rl` is discard, ptr, inferred_ptr, or inferred_ptr, the
/// result instruction can be used to inspect whether it is isNoReturn() but that is it,
/// it must otherwise not be used.
fn expr(ng: *NirGen, scope: *Scope, ri: ResultInfo, node: Ast.Node.Index) Error!Nir.Inst.Ref {
    const astgen = ng.astgen;
    const ast = astgen.ast;
    // const main_tokens = ast.nodes.items(.main_token);
    // const token_tags = ast.tokens.items(.tag);
    // const node_datas = ast.nodes.items(.data);
    const node_tags: []const Ast.Node.Tag = ast.nodes.items(.tag);

    const prev_anon_name_strategy = ng.anon_name_strategy;
    defer ng.anon_name_strategy = prev_anon_name_strategy;
    if (!nodeUsesAnonNameStrategy(ast, node)) {
        ng.anon_name_strategy = .anon;
    }

    switch (node_tags[node]) {
        .root => unreachable, // Top-level declaration.
        else => {
            std.log.err("AstGen.expr({})", .{node_tags[node]});
            unreachable;
        },

        .add => return simpleBinOp(ng, scope, ri, node, .add),
        .sub => return simpleBinOp(ng, scope, ri, node, .sub),
        .mul => return simpleBinOp(ng, scope, ri, node, .mul),

        .negation => return negation(ng, scope, ri, node),

        .char_literal => return charLiteral(ng, ri, node),
        .number_literal => return numberLiteral(ng, ri, node, node, .positive),
    }
}

// TODO: nosuspendExpr
// TODO: suspendExpr
// TODO: awaitExpr
// TODO: resumeExpr
// TODO: fnProtoExpr
// TODO: arrayInitExpr
// TODO: arrayInitExprAnon
// TODO: arrayInitExprTyped
// TODO: arrayInitExprPtr
// TODO: structInitExpr
// TODO: structInitExprAnon
// TODO: structInitExprTyped
// TODO: structInitExprPtr

fn comptimeExpr(ng: *NirGen, scope: *Scope, ri: ResultInfo, node: Ast.Node.Index) Error!Nir.Inst.Ref {
    return expr(ng, scope, ri, node);
}

// TODO: comptimeExprAst
// TODO: restoreErrRetIndex
// TODO: breakExpr
// TODO: continueExpr
// TODO: fullBodyExpr
// TODO: blockExpr
// TODO: labeledBlockExpr
// TODO: blockExprStmts
// TODO: unusedResultExpr
// TODO: addEnsureResult
// TODO: countDefers
// TODO: genDefers
// TODO: checkUsed
// TODO: deferStmt

// TODO
fn varDecl(
    ng: *NirGen,
    scope: *Scope,
    node: Ast.Node.Index,
    block_arena: Allocator,
    full_decl: Ast.full.Decl,
) Error!*Scope {
    try emitDbgNode(ng, node);
    const astgen = ng.astgen;
    const ast = astgen.ast;
    // const token_tags = ast.tokens.items(.tag);
    // const main_tokens = ast.nodes.items(.main_token);

    const name_token = full_decl.ast.let_token + 1 + @intFromBool(full_decl.ast.is_mutable);
    const ident_name_raw = ast.tokenSlice(name_token);
    // TODO: remove `_` from tokens?
    if (std.mem.eql(u8, ident_name_raw, "_")) {
        return astgen.failTok(name_token, "'_' used as an identifer without @\"_\" syntax", .{});
    }
    const ident_name = try astgen.identAsString(name_token);

    try astgen.detectLocalShadowing(
        scope,
        ident_name,
        name_token,
        ident_name_raw,
        if (full_decl.ast.is_mutable) .@"local variable" else .@"local constant",
    );

    if (full_decl.ast.init_node == 0) {
        // TODO: no
        return astgen.failNode(node, "variables must be initialized", .{});
    }

    if (full_decl.ast.is_mutable) {
        // TODO mut
        unreachable;
    } else {
        // Depending on the type of AST the initialization expression is, we may need an lvalue
        // or an rvalue as a result location. If it is an rvalue, we can use the instruction as
        // the variable, no memory location needed.
        const type_node = full_decl.ast.type_node;
        // if (!astgen.nodes_need_rl.contains(node)) {
        //     const result_info: ResultInfo = if (type_node != 0) .{
        //         .rl = .{ .ty = try typeExpr(gz, scope, type_node) },
        //         .ctx = .const_init,
        //     } else .{ .rl = .none, .ctx = .const_init };
        //     const prev_anon_name_strategy = gz.anon_name_strategy;
        //     gz.anon_name_strategy = .dbg_var;
        //     const init_inst = try reachableExpr(gz, scope, result_info, var_decl.ast.init_node, node);
        //     gz.anon_name_strategy = prev_anon_name_strategy;

        //     try gz.addDbgVar(.dbg_var_val, ident_name, init_inst);

        //     // The const init expression may have modified the error return trace, so signal
        //     // to Sema that it should save the new index for restoring later.
        //     if (nodeMayAppendToErrorTrace(tree, var_decl.ast.init_node))
        //         _ = try gz.addSaveErrRetIndex(.{ .if_of_error_type = init_inst });

        //     const sub_scope = try block_arena.create(Scope.LocalVal);
        //     sub_scope.* = .{
        //         .parent = scope,
        //         .gen_zir = gz,
        //         .name = ident_name,
        //         .inst = init_inst,
        //         .token_src = name_token,
        //         .id_cat = .@"local constant",
        //     };
        //     return &sub_scope.base;
        // }

        const is_comptime = ng.is_comptime;

        var resolve_inferred_alloc: Nir.Inst.Ref = .none;
        var opt_type_inst: Nir.Inst.Ref = .none;
        const init_rl: ResultInfo.Loc = if (type_node != 0) init_rl: {
            const type_inst = try typeExpr(ng, scope, type_node);
            opt_type_inst = type_inst;
            break :init_rl .{ .ptr = .{ .inst = try ng.addUnNode(.alloc, type_inst, node) } };
        } else init_rl: {
            const alloc_inst = ptr: {
                const tag: Nir.Inst.Tag = if (is_comptime)
                    .alloc_inferred_comptime
                else
                    .alloc_inferred;
                break :ptr try ng.addNode(tag, node);
            };
            resolve_inferred_alloc = alloc_inst;
            break :init_rl .{ .inferred_ptr = alloc_inst };
        };
        const var_ptr = switch (init_rl) {
            .ptr => |ptr| ptr.inst,
            .inferred_ptr => |inst| inst,
            else => unreachable,
        };
        const init_result_info: ResultInfo = .{ .rl = init_rl, .ctx = .const_init };

        const prev_anon_name_strategy = ng.anon_name_strategy;
        ng.anon_name_strategy = .dbg_var;
        defer ng.anon_name_strategy = prev_anon_name_strategy;
        const init_inst = try reachableExpr(ng, scope, init_result_info, full_decl.ast.init_node, node);

        // The const init expression may have modified the error return trace, so signal
        // to Sema that it should save the new index for restoring later.
        _ = init_inst;
        // if (nodeMayAppendToErrorTrace(tree, full_decl.ast.init_node)) {
        //     _ = try ng.addSaveErrRetIndex(.{ .if_of_error_type = init_inst });
        // }

        const const_ptr = if (resolve_inferred_alloc != .none) p: {
            _ = try ng.addUnNode(.resolve_inferred_alloc, resolve_inferred_alloc, node);
            break :p var_ptr;
        } else try ng.addUnNode(.make_ptr_const, var_ptr, node);

        try ng.addDbgVar(.dbg_var_ptr, ident_name, const_ptr);

        const sub_scope = try block_arena.create(Scope.LocalPtr);
        sub_scope.* = .{
            .parent = scope,
            .gen_zir = ng,
            .name = ident_name,
            .ptr = const_ptr,
            .token_src = name_token,
            .maybe_comptime = true,
            .id_cat = .@"local constant",
        };
        return &sub_scope.base;
    }
}

fn emitDbgNode(ng: *NirGen, node: Ast.Node.Index) !void {
    // The instruction emitted here is for debugging runtime code.
    // If the current block will be evaluated only during semantic analysis
    // then no dbg_stmt NIR instruction is needed.
    if (ng.is_comptime) return;
    const astgen = ng.astgen;
    astgen.advanceSourceCursorToNode(node);
    const line = astgen.source_line - ng.decl_line;
    const column = astgen.source_column;
    try emitDbgStmt(ng, .{ line, column });
}

// TODO: assign
// TODO: assignDestructure
// TODO: assignDestructureMaybeDecls
// TODO: assignOp
// TODO: assignShift
// TODO: ptrType
// TODO: arrayType
// TODO: arrayTypeSentinel

// TODO
const WipMembers = struct {
    payload: *std.ArrayListUnmanaged(u32),
    payload_top: usize,
    field_bits_start: u32,
    fields_start: u32,
    fields_end: u32,
    decl_index: u32 = 0,
    field_index: u32 = 0,

    fn init(
        allocator: Allocator,
        payload: *std.ArrayListUnmanaged(u32),
        decl_count: u32,
        field_count: u32,
        comptime bits_per_field: u32,
        comptime max_field_size: u32,
    ) Allocator.Error!WipMembers {
        const payload_top: u32 = @intCast(payload.items.len);
        const field_bits_start = payload_top + decl_count;
        const fields_start = field_bits_start + if (bits_per_field > 0) blk: {
            const fields_per_u32 = 32 / bits_per_field;
            break :blk (field_count + fields_per_u32 - 1) / fields_per_u32;
        } else 0;
        const payload_end = fields_start + field_count * max_field_size;
        try payload.resize(allocator, payload_end);
        return .{
            .payload = payload,
            .payload_top = payload_top,
            .field_bits_start = field_bits_start,
            .fields_start = fields_start,
            .fields_end = fields_start,
        };
    }

    fn nextDecl(self: *WipMembers, decl_inst: Nir.Inst.Index) void {
        self.payload.items[self.payload_top + self.decl_index] = @intFromEnum(decl_inst);
        self.decl_index += 1;
    }

    fn nextField(self: *WipMembers, comptime bits_per_field: u32, bits: [bits_per_field]bool) void {
        const fields_per_u32 = 32 / bits_per_field;
        const index = self.field_bits_start + self.field_index / fields_per_u32;
        assert(index < self.fields_start);
        var bit_bag: u32 = if (self.field_index % fields_per_u32 == 0) 0 else self.payload.items[index];
        bit_bag >>= bits_per_field;
        comptime var i = 0;
        inline while (i < bits_per_field) : (i += 1) {
            bit_bag |= @as(u32, @intFromBool(bits[i])) << (32 - bits_per_field + i);
        }
        self.payload.items[index] = bit_bag;
        self.field_index += 1;
    }

    fn appendToField(self: *WipMembers, data: u32) void {
        assert(self.fields_end < self.payload.items.len);
        self.payload.items[self.fields_end] = data;
        self.fields_end += 1;
    }

    fn finishBits(self: *WipMembers, comptime bits_per_field: u32) void {
        if (bits_per_field > 0) {
            const fields_per_u32 = 32 / bits_per_field;
            const empty_field_slots = fields_per_u32 - (self.field_index % fields_per_u32);
            if (self.field_index > 0 and empty_field_slots < fields_per_u32) {
                const index = self.field_bits_start + self.field_index / fields_per_u32;
                self.payload.items[index] >>= @intCast(empty_field_slots * bits_per_field);
            }
        }
    }

    fn declsSlice(self: *WipMembers) []u32 {
        return self.payload.items[self.payload_top..][0..self.decl_index];
    }

    fn fieldsSlice(self: *WipMembers) []u32 {
        return self.payload.items[self.field_bits_start..self.fields_end];
    }

    fn deinit(self: *WipMembers) void {
        self.payload.items.len = self.payload_top;
    }
};

// TODO: fnDecl

// TODO
fn globalVarDecl(
    self: *AstGen,
    ng: *NirGen,
    scope: *Scope,
    wip_members: *WipMembers,
    node: Ast.Node.Index,
    full_decl: Ast.full.Decl,
) Error!void {
    const ast = self.ast;
    // const token_tags = ast.tokens.items(.tag);

    const old_hasher = self.src_hasher;
    defer self.src_hasher = old_hasher;
    self.src_hasher = std.zig.SrcHasher.init(.{});
    self.src_hasher.update(ast.getNodeSource(node));
    self.src_hasher.update(std.mem.asBytes(&self.source_column));

    // We do this at the beginning so that the instruction index marks the range start
    // of the top level declaration.
    const decl_inst = try ng.makeDeclaration(node);

    const name_token = full_decl.ast.let_token + 1 + @intFromBool(full_decl.ast.is_mutable);
    self.advanceSourceCursorToNode(node);

    var block_scope: NirGen = .{
        .parent = scope,
        .decl_node_index = node,
        .decl_line = self.source_line,
        .astgen = self,
        .is_comptime = true,
        .instructions = ng.instructions,
        .instructions_top = ng.instructions.items.len,
    };
    defer block_scope.unstack();

    const is_pub = false;
    const is_export = false;
    const is_extern = false;
    wip_members.nextDecl(decl_inst);
    const is_threadlocal = false;
    const lib_name = .empty;
    const doc_comment_index = try self.docCommentAsString(ast.firstToken(node));

    const var_inst: Nir.Inst.Ref = if (full_decl.ast.init_node != 0) vi: {
        if (is_extern) {
            return self.failNode(
                full_decl.ast.init_node,
                "extern variables have no initializers",
                .{},
            );
        }

        const type_inst: Nir.Inst.Ref = if (full_decl.ast.type_node != 0)
            try expr(
                &block_scope,
                &block_scope.base,
                coerced_type_ri,
                full_decl.ast.type_node,
            )
        else
            .none;

        block_scope.anon_name_strategy = .parent;

        const init_inst = try expr(
            &block_scope,
            &block_scope.base,
            if (type_inst != .none) .{ .rl = .{ .ty = type_inst } } else .{ .rl = .none },
            full_decl.ast.init_node,
        );

        if (full_decl.ast.is_mutable) {
            const var_inst = try block_scope.addVar(.{
                .var_type = type_inst,
                .lib_name = .empty,
                .align_inst = .none, // passed via the decls data
                .init = init_inst,
                .is_extern = false,
                .is_const = !full_decl.ast.is_mutable,
                .is_threadlocal = is_threadlocal,
            });
            break :vi var_inst;
        } else {
            break :vi init_inst;
        }
    } else if (!is_extern) {
        return self.failNode(node, "variables must be initialized", .{});
    } else if (full_decl.ast.type_node != 0) vi: {
        // Extern variable which has an explicit type.
        const type_inst = try typeExpr(&block_scope, &block_scope.base, full_decl.ast.type_node);

        block_scope.anon_name_strategy = .parent;

        const var_inst = try block_scope.addVar(.{
            .var_type = type_inst,
            .lib_name = lib_name,
            .align_inst = .none, // passed via the decls data
            .init = .none,
            .is_extern = true,
            .is_const = !full_decl.ast.is_mutable,
            .is_threadlocal = is_threadlocal,
        });
        break :vi var_inst;
    } else {
        return self.failNode(node, "unable to infer variable type", .{});
    };

    // We do this at the end so that the instruction index marks the end
    // range of a top level declaration.
    _ = try block_scope.addBreakWithSrcNode(.break_inline, decl_inst, var_inst, node);
    // TODO: align linksection addrspace

    var hash: std.zig.SrcHash = undefined;
    self.src_hasher.final(&hash);
    try setDeclaration(
        decl_inst,
        hash,
        .{ .named = name_token },
        block_scope.decl_line,
        is_pub,
        is_export,
        doc_comment_index,
        &block_scope,
    );
}

// TODO: comptimeDecl
// TODO: usignnamespaceDecl
// TODO: testDecl
// TODO: structDeclInner
// TODO: unionDeclInner
// TODO: containerDecl

// const ContainerMemberResult = union(enum) { decl, field: Ast.full.ContainerField };
const ContainerMemberResult = union(enum) { decl };

// TODO
fn containerMember(
    ng: *NirGen,
    scope: *Scope,
    wip_members: *WipMembers,
    member_node: Ast.Node.Index,
) Error!ContainerMemberResult {
    const astgen = ng.astgen;
    const ast = astgen.ast;
    const node_tags = ast.nodes.items(.tag);
    // const node_datas = ast.nodes.items(.data);
    switch (node_tags[member_node]) {
        // .container_field_init,
        // .container_field_align,
        // .container_field,
        // => return ContainerMemberResult{ .field = tree.fullContainerField(member_node).? },

        // .fn_proto,
        // .fn_proto_multi,
        // .fn_proto_one,
        // .fn_proto_simple,
        // .fn_decl,
        // => {
        //     var buf: [1]Ast.Node.Index = undefined;
        //     const full = tree.fullFnProto(&buf, member_node).?;
        //     const body = if (node_tags[member_node] == .fn_decl) node_datas[member_node].rhs else 0;

        //     astgen.fnDecl(ng, scope, wip_members, member_node, body, full) catch |err| switch (err) {
        //         error.OutOfMemory => return error.OutOfMemory,
        //         error.AnalysisFail => {},
        //     };
        // },

        // .global_var_decl,
        // .local_var_decl,
        // .simple_var_decl,
        // .aligned_var_decl,
        .decl,
        => {
            astgen.globalVarDecl(ng, scope, wip_members, member_node, ast.fullDecl(member_node).?) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AnalysisFail => {},
            };
        },

        // .test_decl => {
        //     astgen.testDecl(ng, scope, wip_members, member_node) catch |err| switch (err) {
        //         error.OutOfMemory => return error.OutOfMemory,
        //         error.AnalysisFail => {},
        //     };
        // },
        else => unreachable,
    }
    return .decl;
}

// TODO: errorSetDecl
// TODO: tryExpr
// TODO: orelseCatchExpr
// TODO: fieldAccess
// TODO: addFieldAccess
// TODO: arrayAccess

fn simpleBinOp(
    ng: *NirGen,
    scope: *Scope,
    ri: ResultInfo,
    node: Ast.Node.Index,
    op_inst_tag: Nir.Inst.Tag,
) Error!Nir.Inst.Ref {
    const astgen = ng.astgen;
    const ast = astgen.ast;
    const node_data = ast.nodes.items(.data)[node];

    const lhs = try reachableExpr(ng, scope, .{ .rl = .none }, node_data.lhs, node);
    const cursor = switch (op_inst_tag) {
        .add, .sub, .mul, .div, .mod_rem => maybeAdvanceSourceCursorToMainToken(ng, node),
        else => undefined,
    };
    const rhs = try reachableExpr(ng, scope, .{ .rl = .none }, node_data.rhs, node);

    switch (op_inst_tag) {
        .add, .sub, .mul, .div, .mod_rem => try emitDbgStmt(ng, cursor),
        else => {},
    }

    const result = try ng.addPlNode(op_inst_tag, node, Nir.Inst.Bin{ .lhs = lhs, .rhs = rhs });
    return rvalue(ng, ri, result, node);
}

// TODO: simpleStrTok
// TODO: boolBinOp
// TODO: ifExpr
// TODO: setCondBrPayload
// TODO: whileExpr
// TODO: forExpr
// TODO: switchExprErrUnion
// TODO: switchExpr
// TODO: ret
// TODO: parseBitCount
// TODO: identifier
// TODO: localVarRef
// TODO: tunnelThroughClosure
// TODO: stringLiteral

fn charLiteral(gz: *NirGen, ri: ResultInfo, node: Ast.Node.Index) Error!Nir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.ast;
    const main_tokens = tree.nodes.items(.main_token);
    const main_token = main_tokens[node];
    const slice = tree.tokenSlice(main_token);

    switch (string_literal.parseCharLiteral(slice)) {
        .success => |codepoint| {
            const result = try gz.addInt(codepoint);
            return rvalue(gz, ri, result, node);
        },
        .failure => |err| return astgen.failWithStrLitError(err, main_token, slice, 0),
    }
}

const Sign = enum { negative, positive };

fn numberLiteral(
    ng: *NirGen,
    ri: ResultInfo,
    node: Ast.Node.Index,
    source_node: Ast.Node.Index,
    sign: Sign,
) Error!Nir.Inst.Ref {
    const astgen = ng.astgen;
    const ast = astgen.ast;
    const token = ast.nodes.items(.main_token)[node];
    const bytes = ast.tokenSlice(token);

    const result = switch (std.zig.parseNumberLiteral(bytes)) {
        .int => |num| switch (num) {
            0 => if (sign == .positive) .zero else return astgen.failTokNotes(
                token,
                "integer literal '-0' is ambiguous",
                .{},
                &.{
                    try astgen.errNoteTok(token, "use '0' for an integer zero", .{}),
                    try astgen.errNoteTok(token, "use '-0.' for a floating-point signed zero", .{}),
                },
            ),
            1 => {
                const result: Nir.Inst.Ref = switch (sign) {
                    .positive => .one,
                    .negative => .negative_one,
                };
                return rvalue(ng, ri, result, source_node);
            },
            else => try ng.add(.{
                .tag = .int,
                .data = .{ .int = num },
            }),
        },
        .big_int => |base| big: {
            var big_int = try std.math.big.int.Managed.init(astgen.allocator);
            defer big_int.deinit();
            const prefix_offset: usize = if (base == .decimal) 0 else 2;
            big_int.setString(@intFromEnum(base), bytes[prefix_offset..]) catch |err| switch (err) {
                error.InvalidCharacter => unreachable, // caught in parseNumberLiteral
                error.InvalidBase => unreachable, // we only pass 16, 8, 2, see above
                error.OutOfMemory => return error.OutOfMemory,
            };

            const limbs = big_int.limbs[0..big_int.len()];
            assert(big_int.isPositive());
            break :big try ng.addIntBig(limbs);
        },
        .float => {
            const unsigned_float_number = std.fmt.parseFloat(f128, bytes) catch |err| switch (err) {
                error.InvalidCharacter => unreachable, // validated by tokenizer
            };
            const float_number = switch (sign) {
                .negative => -unsigned_float_number,
                .positive => unsigned_float_number,
            };
            // If the value fits into a f64 without losing any precision, store it that way.
            @setFloatMode(.strict);
            const smaller_float: f64 = @floatCast(float_number);
            const bigger_again: f128 = smaller_float;
            if (bigger_again == float_number) {
                const result = try ng.addFloat(smaller_float);
                return rvalue(ng, ri, result, source_node);
            }
            // We need to use 128 bits. Break the float into 4 u32 values so we can
            // put it into the `extra` array.
            const int_bits: u128 = @bitCast(float_number);
            const result = try ng.addPlNode(.float128, node, Nir.Inst.Float128{
                .piece0 = @truncate(int_bits),
                .piece1 = @truncate(int_bits >> 32),
                .piece2 = @truncate(int_bits >> 64),
                .piece3 = @truncate(int_bits >> 96),
            });
            return rvalue(ng, ri, result, source_node);
        },
        .failure => |err| return astgen.failWithNumberError(err, token, bytes),
    };

    if (sign == .positive) {
        return rvalue(ng, ri, result, source_node);
    } else {
        const negated = try ng.addUnNode(.negate, result, source_node);
        return rvalue(ng, ri, negated, source_node);
    }
}

fn failWithNumberError(astgen: *AstGen, err: std.zig.number_literal.Error, token: Ast.TokenIndex, bytes: []const u8) Error {
    const is_float = std.mem.indexOfScalar(u8, bytes, '.') != null;
    switch (err) {
        .leading_zero => if (is_float) {
            return astgen.failTok(token, "number '{s}' has leading zero", .{bytes});
        } else {
            return astgen.failTokNotes(token, "number '{s}' has leading zero", .{bytes}, &.{
                try astgen.errNoteTok(token, "use '0o' prefix for octal literals", .{}),
            });
        },
        .digit_after_base => return astgen.failTok(token, "expected a digit after base prefix", .{}),
        .upper_case_base => |i| return astgen.failOff(token, @intCast(i), "base prefix must be lowercase", .{}),
        .invalid_float_base => |i| return astgen.failOff(token, @intCast(i), "invalid base for float literal", .{}),
        .repeated_underscore => |i| return astgen.failOff(token, @intCast(i), "repeated digit separator", .{}),
        .invalid_underscore_after_special => |i| return astgen.failOff(token, @intCast(i), "expected digit before digit separator", .{}),
        .invalid_digit => |info| return astgen.failOff(token, @intCast(info.i), "invalid digit '{c}' for {s} base", .{ bytes[info.i], @tagName(info.base) }),
        .invalid_digit_exponent => |i| return astgen.failOff(token, @intCast(i), "invalid digit '{c}' in exponent", .{bytes[i]}),
        .duplicate_exponent => |i| return astgen.failOff(token, @intCast(i), "duplicate exponent", .{}),
        .exponent_after_underscore => |i| return astgen.failOff(token, @intCast(i), "expected digit before exponent", .{}),
        .special_after_underscore => |i| return astgen.failOff(token, @intCast(i), "expected digit before '{c}'", .{bytes[i]}),
        .trailing_special => |i| return astgen.failOff(token, @intCast(i), "expected digit after '{c}'", .{bytes[i - 1]}),
        .trailing_underscore => |i| return astgen.failOff(token, @intCast(i), "trailing digit separator", .{}),
        .duplicate_period => unreachable, // Validated by tokenizer
        .invalid_character => unreachable, // Validated by tokenizer
        .invalid_exponent_sign => |i| {
            assert(bytes.len >= 2 and bytes[0] == '0' and bytes[1] == 'x'); // Validated by tokenizer
            return astgen.failOff(token, @intCast(i), "sign '{c}' cannot follow digit '{c}' in hex base", .{ bytes[i], bytes[i - 1] });
        },
        .period_after_exponent => |i| return astgen.failOff(token, @intCast(i), "unexpected period after exponent", .{}),
    }
}

// TODO: as
// TODO: unionInit
// TODO: bitCast
// TODO: ptrCast
// TODO: typeOf
// TODO: minMax
// TODO: builtinCall
// TODO: hasDeclOrField
// TODO: typeCast

fn simpleUnOpType(
    ng: *NirGen,
    scope: *Scope,
    ri: ResultInfo,
    node: Ast.Node.Index,
    operand_node: Ast.Node.Index,
    tag: Nir.Inst.Tag,
) Error!Nir.Inst.Ref {
    const operand = try typeExpr(ng, scope, operand_node);
    const result = try ng.addUnNode(tag, operand, node);
    return rvalue(ng, ri, result, node);
}

// TODO
fn simpleUnOp(
    ng: *NirGen,
    scope: *Scope,
    ri: ResultInfo,
    node: Ast.Node.Index,
    operand_ri: ResultInfo,
    operand_node: Ast.Node.Index,
    tag: Nir.Inst.Tag,
) Error!Nir.Inst.Ref {
    const cursor = maybeAdvanceSourceCursorToMainToken(ng, node);
    const operand = if (tag == .compile_error)
        try comptimeExpr(ng, scope, operand_ri, operand_node)
    else
        try expr(ng, scope, operand_ri, operand_node);
    switch (tag) {
        .tag_name, .error_name, .int_from_ptr => try emitDbgStmt(ng, cursor),
        else => {},
    }
    const result = try ng.addUnNode(tag, operand, node);
    return rvalue(ng, ri, result, node);
}

fn negation(
    ng: *NirGen,
    scope: *Scope,
    ri: ResultInfo,
    node: Ast.Node.Index,
) Error!Nir.Inst.Ref {
    const astgen = ng.astgen;
    const ast = astgen.ast;
    const node_tags = ast.nodes.items(.tag);
    const node_datas = ast.nodes.items(.data);

    // Check for float literal as the sub-expression because we want to preserve
    // its negativity rather than having it go through comptime subtraction.
    const operand_node = node_datas[node].rhs;
    if (node_tags[operand_node] == .number_literal) {
        return numberLiteral(ng, ri, operand_node, node, .negative);
    }

    const operand = try expr(ng, scope, .{ .rl = .none }, operand_node);
    const result = try ng.addUnNode(.negate, operand, node);
    return rvalue(ng, ri, result, node);
}

// TODO: cmpxchg
// TODO: bitBuiltin
// TODO: divBuiltin
// TODO: simpleCBuiltin
// TODO: offsetOf
// TODO: shiftOp
// TODO: cImport
// TODO: overflowArithmetic
// TODO: callExpr
// TODO: calleeExpr

// TODO: primitive_instrs

// TODO: nodeIsTriviallyZero
// TODO: nodeMayAppendToErrorTrace
// TODO: nodeMayEvalToError
// TODO: nodeImpliesMoreThanOnePossibleValue
// TODO: nodeImpliesComptimeOnly

/// Returns `true` if the node uses `ng.anon_name_strategy`.
// TODO
fn nodeUsesAnonNameStrategy(ast: *const Ast, node: Ast.Node.Index) bool {
    const node_tags = ast.nodes.items(.tag);
    switch (node_tags[node]) {
        // .container_decl,
        // .container_decl_trailing,
        // .container_decl_two,
        // .container_decl_two_trailing,
        // .container_decl_arg,
        // .container_decl_arg_trailing,
        // .tagged_union,
        // .tagged_union_trailing,
        // .tagged_union_two,
        // .tagged_union_two_trailing,
        // .tagged_union_enum_tag,
        // .tagged_union_enum_tag_trailing,
        // => return true,
        // .builtin_call_two, .builtin_call_two_comma, .builtin_call, .builtin_call_comma => {
        //     const builtin_token = ast.nodes.items(.main_token)[node];
        //     const builtin_name = ast.tokenSlice(builtin_token);
        //     return std.mem.eql(u8, builtin_name, "@Type");
        // },
        else => return false,
    }
}

/// Applies `rl` semantics to `result`. Expressions which do not do their own handling of
/// result locations must call this function on their result.
/// As an example, if `ri.rl` is `.ptr`, it will write the result to the pointer.
/// If `ri.rl` is `.ty`, it will coerce the result to the type.
/// Assumes nothing stacked on `gz`.
fn rvalue(
    ng: *NirGen,
    ri: ResultInfo,
    raw_result: Nir.Inst.Ref,
    src_node: Ast.Node.Index,
) Error!Nir.Inst.Ref {
    if (true) return raw_result; // TODO
    return rvalueInner(ng, ri, raw_result, src_node, true);
}

/// Like `rvalue`, but refuses to perform coercions before taking references for
/// the `ref_coerced_ty` result type. This is used for local variables which do
/// not have `alloc`s, because we want variables to have consistent addresses,
/// i.e. we want them to act like lvalues.
fn rvalueNoCoercePreRef(
    ng: *NirGen,
    ri: ResultInfo,
    raw_result: Nir.Inst.Ref,
    src_node: Ast.Node.Index,
) Error!Nir.Inst.Ref {
    return rvalueInner(ng, ri, raw_result, src_node, false);
}

// TODO
fn rvalueInner(
    ng: *NirGen,
    ri: ResultInfo,
    raw_result: Nir.Inst.Ref,
    src_node: Ast.Node.Index,
    allow_coerce_pre_ref: bool,
) Error!Nir.Inst.Ref {
    const result = r: {
        if (raw_result.toIndex()) |result_index| {
            const nir_tags = ng.astgen.instructions.items(.tag);
            const data = ng.astgen.instructions.items(.data)[@intFromEnum(result_index)];
            if (nir_tags[@intFromEnum(result_index)].isAlwaysVoid(data)) {
                break :r Nir.Inst.Ref.void_value;
            }
        }
        break :r raw_result;
    };
    if (ng.endsWithNoReturn()) return result;
    switch (ri.rl) {
        .none, .coerced_ty => return result,
        .discard => {
            // Emit a compile error for discarding error values.
            _ = try ng.addUnNode(.ensure_result_non_error, result, src_node);
            return .void_value;
        },
        .ref, .ref_coerced_ty => {
            const coerced_result = if (allow_coerce_pre_ref and ri.rl == .ref_coerced_ty) res: {
                const ptr_ty = ri.rl.ref_coerced_ty;
                break :res try ng.addPlNode(.coerce_ptr_elem_ty, src_node, Nir.Inst.Bin{
                    .lhs = ptr_ty,
                    .rhs = result,
                });
            } else result;
            // We need a pointer but we have a value.
            // Unfortunately it's not quite as simple as directly emitting a ref
            // instruction here because we need subsequent address-of operator on
            // const locals to return the same address.
            const astgen = ng.astgen;
            const ast = astgen.ast;
            const src_token = ast.firstToken(src_node);
            const result_index = coerced_result.toIndex() orelse
                return ng.addUnTok(.ref, coerced_result, src_token);
            const nir_tags = ng.astgen.instructions.items(.tag);
            if (nir_tags[@intFromEnum(result_index)].isParam() or astgen.isInferred(coerced_result))
                return ng.addUnTok(.ref, coerced_result, src_token);
            const gop = try astgen.ref_table.getOrPut(astgen.allocator, result_index);
            if (!gop.found_existing) {
                gop.value_ptr.* = try ng.makeUnTok(.ref, coerced_result, src_token);
            }
            return gop.value_ptr.*.toRef();
        },
        .ty => |ty_inst| {
            // Quickly eliminate some common, unnecessary type coercion.
            const as_ty = @as(u64, @intFromEnum(Nir.Inst.Ref.type_type)) << 32;
            const as_bool = @as(u64, @intFromEnum(Nir.Inst.Ref.bool_type)) << 32;
            const as_void = @as(u64, @intFromEnum(Nir.Inst.Ref.void_type)) << 32;
            const as_comptime_int = @as(u64, @intFromEnum(Nir.Inst.Ref.comptime_int_type)) << 32;
            // const as_usize = @as(u64, @intFromEnum(Nir.Inst.Ref.usize_type)) << 32;
            // const as_u8 = @as(u64, @intFromEnum(Nir.Inst.Ref.u8_type)) << 32;
            switch ((@as(u64, @intFromEnum(ty_inst)) << 32) | @as(u64, @intFromEnum(result))) {
                as_ty | @intFromEnum(Nir.Inst.Ref.u8_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.i8_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.u16_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.i16_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.u32_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.i32_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.u64_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.i64_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.u128_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.i128_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.uint_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.int_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_char_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_short_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_ushort_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_int_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_uint_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_long_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_ulong_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_longlong_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_ulonglong_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.c_longdouble_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.f16_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.f32_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.f64_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.f80_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.f128_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.anyptr_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.bool_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.void_type),
                as_ty | @intFromEnum(Nir.Inst.Ref.type_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.anyerror_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.comptime_int_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.comptime_float_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.noreturn_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.anyframe_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.null_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.undefined_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.enum_literal_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.manyptr_u8_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.manyptr_const_u8_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.manyptr_const_u8_sentinel_0_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.single_const_pointer_to_comptime_int_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.slice_const_u8_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.slice_const_u8_sentinel_0_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.anyerror_void_error_union_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.generic_poison_type),
                // as_ty | @intFromEnum(Nir.Inst.Ref.empty_struct_type),
                as_comptime_int | @intFromEnum(Nir.Inst.Ref.zero),
                as_comptime_int | @intFromEnum(Nir.Inst.Ref.one),
                as_comptime_int | @intFromEnum(Nir.Inst.Ref.negative_one),
                // as_usize | @intFromEnum(Nir.Inst.Ref.zero_usize),
                // as_usize | @intFromEnum(Nir.Inst.Ref.one_usize),
                // as_u8 | @intFromEnum(Nir.Inst.Ref.zero_u8),
                // as_u8 | @intFromEnum(Nir.Inst.Ref.one_u8),
                // as_u8 | @intFromEnum(Nir.Inst.Ref.four_u8),
                as_bool | @intFromEnum(Nir.Inst.Ref.bool_true),
                as_bool | @intFromEnum(Nir.Inst.Ref.bool_false),
                as_void | @intFromEnum(Nir.Inst.Ref.void_value),
                => return result, // type of result is already correct

                // as_usize | @intFromEnum(Nir.Inst.Ref.zero) => return .zero_usize,
                // as_u8 | @intFromEnum(Nir.Inst.Ref.zero) => return .zero_u8,
                // as_usize | @intFromEnum(Nir.Inst.Ref.one) => return .one_usize,
                // as_u8 | @intFromEnum(Nir.Inst.Ref.one) => return .one_u8,
                // as_comptime_int | @intFromEnum(Nir.Inst.Ref.zero_usize) => return .zero,
                // as_u8 | @intFromEnum(Nir.Inst.Ref.zero_usize) => return .zero_u8,
                // as_comptime_int | @intFromEnum(Nir.Inst.Ref.one_usize) => return .one,
                // as_u8 | @intFromEnum(Nir.Inst.Ref.one_usize) => return .one_u8,
                // as_comptime_int | @intFromEnum(Nir.Inst.Ref.zero_u8) => return .zero,
                // as_usize | @intFromEnum(Nir.Inst.Ref.zero_u8) => return .zero_usize,
                // as_comptime_int | @intFromEnum(Nir.Inst.Ref.one_u8) => return .one,
                // as_usize | @intFromEnum(Nir.Inst.Ref.one_u8) => return .one_usize,

                // Need an explicit type coercion instruction.
                else => return ng.addPlNode(ri.nirTag(), src_node, Nir.Inst.As{
                    .dest_type = ty_inst,
                    .operand = result,
                }),
            }
        },
        .ptr => |ptr_res| {
            _ = try ng.addPlNode(.store_node, ptr_res.src_node orelse src_node, Nir.Inst.Bin{
                .lhs = ptr_res.inst,
                .rhs = result,
            });
            return .void_value;
        },
        .inferred_ptr => |alloc| {
            _ = try ng.addPlNode(.store_to_inferred_ptr, src_node, Nir.Inst.Bin{
                .lhs = alloc,
                .rhs = result,
            });
            return .void_value;
        },
        .destructure => |destructure| {
            const components = destructure.components;
            _ = try ng.addPlNode(.validate_destructure, src_node, Nir.Inst.ValidateDestructure{
                .operand = result,
                .destructure_node = ng.nodeIndexToRelative(destructure.src_node),
                .expect_len = @intCast(components.len),
            });
            for (components, 0..) |component, i| {
                if (component == .discard) continue;
                const elem_val = try ng.add(.{
                    .tag = .elem_val_imm,
                    .data = .{ .elem_val_imm = .{
                        .operand = result,
                        .idx = @intCast(i),
                    } },
                });
                switch (component) {
                    .typed_ptr => |ptr_res| {
                        _ = try ng.addPlNode(.store_node, ptr_res.src_node orelse src_node, Nir.Inst.Bin{
                            .lhs = ptr_res.inst,
                            .rhs = elem_val,
                        });
                    },
                    .inferred_ptr => |ptr_inst| {
                        _ = try ng.addPlNode(.store_to_inferred_ptr, src_node, Nir.Inst.Bin{
                            .lhs = ptr_inst,
                            .rhs = elem_val,
                        });
                    },
                    .discard => unreachable,
                }
            }
            return .void_value;
        },
    }
}

/// Given an identifier token, obtain the string for it.
/// If the token uses @"" syntax, parses as a string, reports errors if applicable,
/// and allocates the result within `astgen.arena`.
/// Otherwise, returns a reference to the source code bytes directly.
/// See also `appendIdentStr` and `parseStrLit`.
fn identifierTokenString(self: *AstGen, token: Ast.TokenIndex) Error![]const u8 {
    const ast = self.ast;
    const token_tags = ast.tokens.items(.tag);
    assert(token_tags[token] == .identifier);
    const ident_name = ast.tokenSlice(token);
    if (!std.mem.startsWith(u8, ident_name, "@")) {
        return ident_name;
    }
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(self.allocator);
    try self.parseStrLit(token, &buf, ident_name, 1);
    if (std.mem.indexOfScalar(u8, buf.items, 0) != null) {
        return self.failTok(token, "identifier cannot contain null bytes", .{});
    } else if (buf.items.len == 0) {
        return self.failTok(token, "identifier cannot be empty", .{});
    }
    const duped = try self.arena.dupe(u8, buf.items);
    return duped;
}

/// Given an identifier token, obtain the string for it (possibly parsing as a string
/// literal if it is @"" syntax), and append the string to `buf`.
/// See also `identifierTokenString` and `parseStrLit`.
fn appendIdentStr(self: *AstGen, token: Ast.TokenIndex, buf: *std.ArrayListUnmanaged(u8)) Error!void {
    const ast = self.ast;
    const token_tags = ast.tokens.items(.tag);
    assert(token_tags[token] == .identifier);
    const ident_name = ast.tokenSlice(token);
    if (!std.mem.startsWith(u8, ident_name, "@")) {
        return buf.appendSlice(self.allocator, ident_name);
    } else {
        const start = buf.items.len;
        try self.parseStrLit(token, buf, ident_name, 1);
        const slice = buf.items[start..];
        if (std.mem.indexOfScalar(u8, slice, 0) != null) {
            return self.failTok(token, "identifier cannot contain null bytes", .{});
        } else if (slice.len == 0) {
            return self.failTok(token, "identifier cannot be empty", .{});
        }
    }
}

// TODO: handle string interpolation here?
// in string_literal.parseWrite

/// Appends the result to `buf`.
fn parseStrLit(
    self: *AstGen,
    token: Ast.TokenIndex,
    buf: *std.ArrayListUnmanaged(u8),
    bytes: []const u8,
    offset: u32,
) Error!void {
    const raw_string = bytes[offset..];
    var buf_managed = buf.toManaged(self.allocator);
    const result = string_literal.parseWrite(buf_managed.writer(), raw_string);
    buf.* = buf_managed.moveToUnmanaged();
    switch (try result) {
        .success => return,
        .failure => |err| return self.failWithStrLitError(err, token, bytes, offset),
    }
}

// TODO
fn failWithStrLitError(
    self: *AstGen,
    err: string_literal.Error,
    token: Ast.TokenIndex,
    bytes: []const u8,
    offset: u32,
) Error {
    const raw_string = bytes[offset..];
    switch (err) {
        .invalid_escape_character => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "invalid escape character: '{c}'",
                .{raw_string[bad_index]},
            );
        },
        .expected_hex_digit => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "expected hex digit, found '{c}'",
                .{raw_string[bad_index]},
            );
        },
        .empty_unicode_escape_sequence => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "empty unicode escape sequence",
                .{},
            );
        },
        .expected_hex_digit_or_rbrace => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "expected hex digit or '}}', found '{c}'",
                .{raw_string[bad_index]},
            );
        },
        .invalid_unicode_codepoint => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "unicode escape does not correspond to a valid codepoint",
                .{},
            );
        },
        .expected_lbrace => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "expected '{{', found '{c}",
                .{raw_string[bad_index]},
            );
        },
        .expected_rbrace => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "expected '}}', found '{c}",
                .{raw_string[bad_index]},
            );
        },
        .expected_single_quote => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "expected single quote ('), found '{c}",
                .{raw_string[bad_index]},
            );
        },
        .invalid_character => |bad_index| {
            return self.failOff(
                token,
                offset + @as(u32, @intCast(bad_index)),
                "invalid byte in string or character literal: '{c}'",
                .{raw_string[bad_index]},
            );
        },
        .empty_char_literal => {
            return self.failOff(token, offset, "empty character literal", .{});
        },
    }
}

fn failNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) Error {
    return astgen.failNodeNotes(node, format, args, &[0]u32{});
}

fn appendErrorNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!void {
    try astgen.appendErrorNodeNotes(node, format, args, &[0]u32{});
}

fn appendErrorNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) Allocator.Error!void {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Nir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.allocator).print(format ++ "\x00", args);
    const notes_index: u32 = if (notes.len != 0) blk: {
        const notes_start = astgen.extra.items.len;
        try astgen.extra.ensureTotalCapacity(astgen.allocator, notes_start + 1 + notes.len);
        astgen.extra.appendAssumeCapacity(@intCast(notes.len));
        astgen.extra.appendSliceAssumeCapacity(notes);
        break :blk @intCast(notes_start);
    } else 0;
    try astgen.compile_errors.append(astgen.allocator, .{
        .msg = msg,
        .node = node,
        .token = 0,
        .byte_offset = 0,
        .notes = notes_index,
    });
}

fn failNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) Error {
    try appendErrorNodeNotes(astgen, node, format, args, notes);
    return error.AnalysisFail;
}

fn failTok(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
) Error {
    return astgen.failTokNotes(token, format, args, &[0]u32{});
}

fn appendErrorTok(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
) !void {
    try astgen.appendErrorTokNotesOff(token, 0, format, args, &[0]u32{});
}

fn failTokNotes(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) Error {
    try appendErrorTokNotesOff(astgen, token, 0, format, args, notes);
    return error.AnalysisFail;
}

fn appendErrorTokNotes(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) !void {
    return appendErrorTokNotesOff(astgen, token, 0, format, args, notes);
}

/// Same as `fail`, except given a token plus an offset from its starting byte
/// offset.
fn failOff(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
) Error {
    try appendErrorTokNotesOff(astgen, token, byte_offset, format, args, &.{});
    return error.AnalysisFail;
}

fn appendErrorTokNotesOff(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) !void {
    @branchHint(.cold);
    const allocator = astgen.allocator;
    const string_bytes = &astgen.string_bytes;
    const msg: Nir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(allocator).print(format ++ "\x00", args);
    const notes_index: u32 = if (notes.len != 0) blk: {
        const notes_start = astgen.extra.items.len;
        try astgen.extra.ensureTotalCapacity(allocator, notes_start + 1 + notes.len);
        astgen.extra.appendAssumeCapacity(@intCast(notes.len));
        astgen.extra.appendSliceAssumeCapacity(notes);
        break :blk @intCast(notes_start);
    } else 0;
    try astgen.compile_errors.append(allocator, .{
        .msg = msg,
        .node = 0,
        .token = token,
        .byte_offset = byte_offset,
        .notes = notes_index,
    });
}

fn errNoteTok(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    return errNoteTokOff(astgen, token, 0, format, args);
}

fn errNoteTokOff(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Nir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.allocator).print(format ++ "\x00", args);
    return astgen.addExtra(Nir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = 0,
        .token = token,
        .byte_offset = byte_offset,
        .notes = 0,
    });
}

fn errNoteNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Nir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.allocator).print(format ++ "\x00", args);
    return astgen.addExtra(Nir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = node,
        .token = 0,
        .byte_offset = 0,
        .notes = 0,
    });
}

fn identAsString(astgen: *AstGen, ident_token: Ast.TokenIndex) Error!Nir.NullTerminatedString {
    const allocator = astgen.allocator;
    const string_bytes = &astgen.string_bytes;
    const str_index: u32 = @intCast(string_bytes.items.len);
    try astgen.appendIdentStr(ident_token, string_bytes);
    const key: []const u8 = string_bytes.items[str_index..];
    const gop = try astgen.string_table.getOrPutContextAdapted(
        allocator,
        key,
        StringIndexAdapter{ .bytes = string_bytes },
        StringIndexContext{ .bytes = string_bytes },
    );
    if (gop.found_existing) {
        string_bytes.shrinkRetainingCapacity(str_index);
        return @enumFromInt(gop.key_ptr.*);
    } else {
        gop.key_ptr.* = str_index;
        try string_bytes.append(allocator, 0);
        return @enumFromInt(str_index);
    }
}

// TODO: idk about all below

/// Adds a doc comment block to `string_bytes` by walking backwards from `end_token`.
/// `end_token` must point at the first token after the last doc coment line.
/// Returns 0 if no doc comment is present.
fn docCommentAsString(astgen: *AstGen, end_token: Ast.TokenIndex) !Nir.NullTerminatedString {
    if (end_token == 0) return .empty;

    const token_tags = astgen.ast.tokens.items(.tag);

    var tok = end_token - 1;
    while (token_tags[tok] == .doc_comment) {
        if (tok == 0) break;
        tok -= 1;
    } else {
        tok += 1;
    }

    return docCommentAsStringFromFirst(astgen, end_token, tok);
}

/// end_token must be > the index of the last doc comment.
fn docCommentAsStringFromFirst(
    astgen: *AstGen,
    end_token: Ast.TokenIndex,
    start_token: Ast.TokenIndex,
) !Nir.NullTerminatedString {
    if (start_token == end_token) return .empty;

    const allocator = astgen.allocator;
    const string_bytes = &astgen.string_bytes;
    const str_index: u32 = @intCast(string_bytes.items.len);
    const token_starts = astgen.ast.tokens.items(.start);
    const token_tags = astgen.ast.tokens.items(.tag);

    const total_bytes = token_starts[end_token] - token_starts[start_token];
    try string_bytes.ensureUnusedCapacity(allocator, total_bytes);

    var current_token = start_token;
    while (current_token < end_token) : (current_token += 1) {
        switch (token_tags[current_token]) {
            .doc_comment => {
                const tok_bytes = astgen.ast.tokenSlice(current_token)[3..];
                string_bytes.appendSliceAssumeCapacity(tok_bytes);
                if (current_token != end_token - 1) {
                    string_bytes.appendAssumeCapacity('\n');
                }
            },
            else => break,
        }
    }

    const key: []const u8 = string_bytes.items[str_index..];
    const gop = try astgen.string_table.getOrPutContextAdapted(allocator, key, StringIndexAdapter{
        .bytes = string_bytes,
    }, StringIndexContext{
        .bytes = string_bytes,
    });

    if (gop.found_existing) {
        string_bytes.shrinkRetainingCapacity(str_index);
        return @enumFromInt(gop.key_ptr.*);
    } else {
        gop.key_ptr.* = str_index;
        try string_bytes.append(allocator, 0);
        return @enumFromInt(str_index);
    }
}

const IndexSlice = struct { index: Nir.NullTerminatedString, len: u32 };

fn strLitAsString(astgen: *AstGen, str_lit_token: Ast.TokenIndex) !IndexSlice {
    const allocator = astgen.allocator;
    const string_bytes = &astgen.string_bytes;
    const str_index: u32 = @intCast(string_bytes.items.len);
    const token_bytes = astgen.ast.tokenSlice(str_lit_token);
    try astgen.parseStrLit(str_lit_token, string_bytes, token_bytes, 0);
    const key: []const u8 = string_bytes.items[str_index..];
    if (std.mem.indexOfScalar(u8, key, 0)) |_| return .{
        .index = @enumFromInt(str_index),
        .len = @intCast(key.len),
    };
    const gop = try astgen.string_table.getOrPutContextAdapted(allocator, key, StringIndexAdapter{
        .bytes = string_bytes,
    }, StringIndexContext{
        .bytes = string_bytes,
    });
    if (gop.found_existing) {
        string_bytes.shrinkRetainingCapacity(str_index);
        return .{
            .index = @enumFromInt(gop.key_ptr.*),
            .len = @intCast(key.len),
        };
    } else {
        gop.key_ptr.* = str_index;
        // Still need a null byte because we are using the same table
        // to lookup null terminated strings, so if we get a match, it has to
        // be null terminated for that to work.
        try string_bytes.append(allocator, 0);
        return .{
            .index = @enumFromInt(str_index),
            .len = @intCast(key.len),
        };
    }
}

fn strLitNodeAsString(astgen: *AstGen, node: Ast.Node.Index) !IndexSlice {
    const ast = astgen.ast;
    const node_datas = ast.nodes.items(.data);

    const start = node_datas[node].lhs;
    const end = node_datas[node].rhs;

    const allocator = astgen.allocator;
    const string_bytes = &astgen.string_bytes;
    const str_index = string_bytes.items.len;

    // First line: do not append a newline.
    var tok_i = start;
    {
        const slice = ast.tokenSlice(tok_i);
        const carriage_return_ending: usize = if (slice[slice.len - 2] == '\r') 2 else 1;
        const line_bytes = slice[2 .. slice.len - carriage_return_ending];
        try string_bytes.appendSlice(allocator, line_bytes);
        tok_i += 1;
    }
    // Following lines: each line prepends a newline.
    while (tok_i <= end) : (tok_i += 1) {
        const slice = ast.tokenSlice(tok_i);
        const carriage_return_ending: usize = if (slice[slice.len - 2] == '\r') 2 else 1;
        const line_bytes = slice[2 .. slice.len - carriage_return_ending];
        try string_bytes.ensureUnusedCapacity(allocator, line_bytes.len + 1);
        string_bytes.appendAssumeCapacity('\n');
        string_bytes.appendSliceAssumeCapacity(line_bytes);
    }
    const len = string_bytes.items.len - str_index;
    try string_bytes.append(allocator, 0);
    return IndexSlice{
        .index = @enumFromInt(str_index),
        .len = @intCast(len),
    };
}

fn testNameString(astgen: *AstGen, str_lit_token: Ast.TokenIndex) !Nir.NullTerminatedString {
    const allocator = astgen.allocator;
    const string_bytes = &astgen.string_bytes;
    const str_index: u32 = @intCast(string_bytes.items.len);
    const token_bytes = astgen.ast.tokenSlice(str_lit_token);
    try string_bytes.append(allocator, 0); // Indicates this is a test.
    try astgen.parseStrLit(str_lit_token, string_bytes, token_bytes, 0);
    const slice = string_bytes.items[str_index + 1 ..];
    if (std.mem.indexOfScalar(u8, slice, 0) != null) {
        return astgen.failTok(str_lit_token, "test name cannot contain null bytes", .{});
    } else if (slice.len == 0) {
        return astgen.failTok(str_lit_token, "empty test name must be omitted", .{});
    }
    try string_bytes.append(allocator, 0);
    return @enumFromInt(str_index);
}

const Scope = struct {
    tag: Tag,

    fn cast(base: *Scope, comptime T: type) ?*T {
        if (T == Defer) {
            switch (base.tag) {
                .defer_normal, .defer_error => return @alignCast(@fieldParentPtr("base", base)),
                else => return null,
            }
        }
        if (T == Namespace) {
            switch (base.tag) {
                .namespace => return @alignCast(@fieldParentPtr("base", base)),
                else => return null,
            }
        }
        if (base.tag != T.base_tag)
            return null;

        return @alignCast(@fieldParentPtr("base", base));
    }

    fn parent(base: *Scope) ?*Scope {
        return switch (base.tag) {
            .nir_gen => base.cast(NirGen).?.parent,
            .local_val => base.cast(LocalVal).?.parent,
            .local_ptr => base.cast(LocalPtr).?.parent,
            .defer_normal, .defer_error => base.cast(Defer).?.parent,
            .namespace => base.cast(Namespace).?.parent,
            .top => null,
        };
    }

    const Tag = enum {
        nir_gen,
        local_val,
        local_ptr,
        defer_normal,
        defer_error,
        namespace,
        top,
    };

    /// The category of identifier. These tag names are user-visible in compile errors.
    const IdCat = enum {
        @"function parameter",
        @"local constant",
        @"local variable",
        @"switch tag capture",
        capture,
    };

    /// This is always a `const` local and importantly the `inst` is a value type, not a pointer.
    /// This structure lives as long as the AST generation of the Block
    /// node that contains the variable.
    const LocalVal = struct {
        const base_tag: Tag = .local_val;
        base: Scope = Scope{ .tag = base_tag },
        /// Parents can be: `LocalVal`, `LocalPtr`, `NirGen`, `Defer`, `Namespace`.
        parent: *Scope,
        nir_gen: *NirGen,
        inst: Nir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.TokenIndex,
        /// Track the first identifer where it is referenced.
        /// 0 means never referenced.
        used: Ast.TokenIndex = 0,
        /// Track the identifier where it is discarded, like this `_ = foo;`.
        /// 0 means never discarded.
        discarded: Ast.TokenIndex = 0,
        /// String table index.
        name: Nir.NullTerminatedString,
        id_cat: IdCat,
    };

    /// This could be a `const` or `var` local. It has a pointer instead of a value.
    /// This structure lives as long as the AST generation of the Block
    /// node that contains the variable.
    const LocalPtr = struct {
        const base_tag: Tag = .local_ptr;
        base: Scope = Scope{ .tag = base_tag },
        /// Parents can be: `LocalVal`, `LocalPtr`, `NirGen`, `Defer`, `Namespace`.
        parent: *Scope,
        nir_gen: *NirGen,
        ptr: Nir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.TokenIndex,
        /// Track the first identifer where it is referenced.
        /// 0 means never referenced.
        used: Ast.TokenIndex = 0,
        /// Track the identifier where it is discarded, like this `_ = foo;`.
        /// 0 means never discarded.
        discarded: Ast.TokenIndex = 0,
        /// Whether this value is used as an lvalue after inititialization.
        /// If not, we know it can be `const`, so will emit a compile error if it is `var`.
        used_as_lvalue: bool = false,
        /// String table index.
        name: Nir.NullTerminatedString,
        id_cat: IdCat,
        /// true means we find out during Sema whether the value is comptime.
        /// false means it is already known at AstGen the value is runtime-known.
        maybe_comptime: bool,
    };

    const Defer = struct {
        base: Scope,
        /// Parents can be: `LocalVal`, `LocalPtr`, `NirGen`, `Defer`, `Namespace`.
        parent: *Scope,
        index: u32,
        len: u32,
        remapped_err_code: Nir.Inst.OptionalIndex = .none,
    };

    /// Represents a global scope that has any number of declarations in it.
    /// Each declaration has this as the parent scope.
    const Namespace = struct {
        const base_tag: Tag = .namespace;
        base: Scope = Scope{ .tag = base_tag },

        /// Parents can be: `LocalVal`, `LocalPtr`, `NirGen`, `Defer`, `Namespace`.
        parent: *Scope,
        /// Maps string table index to the source location of declaration,
        /// for the purposes of reporting name shadowing compile errors.
        decls: std.AutoHashMapUnmanaged(Nir.NullTerminatedString, Ast.Node.Index) = .{},
        node: Ast.Node.Index,
        inst: Nir.Inst.Index,
        maybe_generic: bool,

        /// The astgen scope containing this namespace.
        /// Only valid during astgen.
        declaring_ng: ?*NirGen,

        /// Set of captures used by this namespace.
        captures: std.AutoArrayHashMapUnmanaged(Nir.Inst.Capture, void) = .{},

        fn deinit(self: *Namespace, allocator: Allocator) void {
            self.decls.deinit(allocator);
            self.captures.deinit(allocator);
            self.* = undefined;
        }
    };

    const Top = struct {
        const base_tag: Scope.Tag = .top;
        base: Scope = Scope{ .tag = base_tag },
    };
};

/// This is a temporary structure; references to it are valid only
/// while constructing a `Nir`.
const NirGen = struct {
    const base_tag: Scope.Tag = .nir_gen;
    base: Scope = Scope{ .tag = base_tag },
    /// Whether we're already in a scope known to be comptime. This is set
    /// whenever we know Sema will analyze the current block with `is_comptime`,
    /// for instance when we're within a `struct_decl` or a `block_comptime`.
    is_comptime: bool,
    /// Whether we're in an expression within a `@TypeOf` operand. In this case, closure of runtime
    /// variables is permitted where it is usually not.
    is_typeof: bool = false,
    /// This is set to true for a `NirGen` of a `block_inline`, indicating that
    /// exits from this block should use `break_inline` rather than `break`.
    is_inline: bool = false,
    c_import: bool = false,
    /// How decls created in this scope should be named.
    anon_name_strategy: Nir.Inst.NameStrategy = .anon,
    /// The containing decl AST node.
    decl_node_index: Ast.Node.Index,
    /// The containing decl line index, absolute.
    decl_line: u32,
    /// Parents can be: `LocalVal`, `LocalPtr`, `NirGen`, `Defer`, `Namespace`.
    parent: *Scope,
    /// All `NirGen` scopes for the same NIR share this.
    astgen: *AstGen,
    /// Keeps track of the list of instructions in this scope. Possibly shared.
    /// Indexes to instructions in `astgen`.
    instructions: *std.ArrayListUnmanaged(Nir.Inst.Index),
    /// A sub-block may share its instructions ArrayList with containing NirGen,
    /// if use is strictly nested. This saves prior size of list for unstacking.
    instructions_top: usize,
    label: ?Label = null,
    break_block: Nir.Inst.OptionalIndex = .none,
    continue_block: Nir.Inst.OptionalIndex = .none,
    /// Only valid when setBreakResultInfo is called.
    break_result_info: AstGen.ResultInfo = undefined,

    suspend_node: Ast.Node.Index = 0,
    nosuspend_node: Ast.Node.Index = 0,
    /// Set if this NirGen is a defer.
    cur_defer_node: Ast.Node.Index = 0,
    // Set if this NirGen is a defer or it is inside a defer.
    any_defer_node: Ast.Node.Index = 0,

    const unstacked_top = std.math.maxInt(usize);
    /// Call unstack before adding any new instructions to containing NirGen.
    fn unstack(self: *NirGen) void {
        if (self.instructions_top != unstacked_top) {
            self.instructions.items.len = self.instructions_top;
            self.instructions_top = unstacked_top;
        }
    }

    fn isEmpty(self: *const NirGen) bool {
        return (self.instructions_top == unstacked_top) or
            (self.instructions.items.len == self.instructions_top);
    }

    fn instructionsSlice(self: *const NirGen) []Nir.Inst.Index {
        return if (self.instructions_top == unstacked_top)
            &[0]Nir.Inst.Index{}
        else
            self.instructions.items[self.instructions_top..];
    }

    fn instructionsSliceUpto(self: *const NirGen, stacked_ng: *NirGen) []Nir.Inst.Index {
        return if (self.instructions_top == unstacked_top)
            &[0]Nir.Inst.Index{}
        else if (self.instructions == stacked_ng.instructions and stacked_ng.instructions_top != unstacked_top)
            self.instructions.items[self.instructions_top..stacked_ng.instructions_top]
        else
            self.instructions.items[self.instructions_top..];
    }

    fn makeSubBlock(self: *NirGen, scope: *Scope) NirGen {
        return .{
            .is_comptime = self.is_comptime,
            .is_typeof = self.is_typeof,
            .c_import = self.c_import,
            .decl_node_index = self.decl_node_index,
            .decl_line = self.decl_line,
            .parent = scope,
            .astgen = self.astgen,
            .suspend_node = self.suspend_node,
            .nosuspend_node = self.nosuspend_node,
            .any_defer_node = self.any_defer_node,
            .instructions = self.instructions,
            .instructions_top = self.instructions.items.len,
        };
    }

    const Label = struct {
        token: Ast.TokenIndex,
        block_inst: Nir.Inst.Index,
        used: bool = false,
    };

    /// Assumes nothing stacked on `self`.
    fn endsWithNoReturn(self: NirGen) bool {
        if (self.isEmpty()) return false;
        const tags = self.astgen.instructions.items(.tag);
        const last_inst = self.instructions.items[self.instructions.items.len - 1];
        return tags[@intFromEnum(last_inst)].isNoReturn();
    }

    /// TODO all uses of this should be replaced with uses of `endsWithNoReturn`.
    fn refIsNoReturn(self: NirGen, inst_ref: Nir.Inst.Ref) bool {
        if (inst_ref.toIndex()) |inst_index| {
            return self.astgen.instructions.items(.tag)[@intFromEnum(inst_index)].isNoReturn();
        }
        return false;
    }

    fn nodeIndexToRelative(self: NirGen, node_index: Ast.Node.Index) i32 {
        return @as(i32, @bitCast(node_index)) - @as(i32, @bitCast(self.decl_node_index));
    }

    fn tokenIndexToRelative(self: NirGen, token: Ast.TokenIndex) u32 {
        return token - self.srcToken();
    }

    fn srcToken(self: NirGen) Ast.TokenIndex {
        return self.astgen.ast.firstToken(self.decl_node_index);
    }

    fn setBreakResultInfo(self: *NirGen, parent_ri: AstGen.ResultInfo) void {
        // Depending on whether the result location is a pointer or value, different
        // NIR needs to be generated. In the former case we rely on storing to the
        // pointer to communicate the result, and use breakvoid; in the latter case
        // the block break instructions will have the result values.
        switch (parent_ri.rl) {
            .coerced_ty => |ty_inst| {
                // Type coercion needs to happen before breaks.
                self.break_result_info = .{ .rl = .{ .ty = ty_inst }, .ctx = parent_ri.ctx };
            },
            .discard => {
                // We don't forward the result context here. This prevents
                // "unnecessary discard" errors from being caused by expressions
                // far from the actual discard, such as a `break` from a
                // discarded block.
                self.break_result_info = .{ .rl = .discard };
            },
            else => {
                self.break_result_info = parent_ri;
            },
        }
    }

    /// Assumes nothing stacked on `ng`. Unstacks `ng`.
    fn setBoolBrBody(ng: *NirGen, bool_br: Nir.Inst.Index, bool_br_lhs: Nir.Inst.Ref) !void {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;
        const body = ng.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        try astgen.extra.ensureUnusedCapacity(
            allocator,
            @typeInfo(Nir.Inst.BoolBr).Struct.fields.len + body_len,
        );
        const nir_datas = astgen.instructions.items(.data);
        nir_datas[@intFromEnum(bool_br)].pl_node.payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.BoolBr{
            .lhs = bool_br_lhs,
            .body_len = body_len,
        });
        astgen.appendBodyWithFixups(body);
        ng.unstack();
    }

    /// Assumes nothing stacked on `ng`. Unstacks `ng`.
    fn setBlockBody(ng: *NirGen, inst: Nir.Inst.Index) !void {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;
        const body = ng.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        try astgen.extra.ensureUnusedCapacity(
            allocator,
            @typeInfo(Nir.Inst.Block).Struct.fields.len + body_len,
        );
        const nir_datas = astgen.instructions.items(.data);
        nir_datas[@intFromEnum(inst)].pl_node.payload_index = astgen.addExtraAssumeCapacity(
            Nir.Inst.Block{ .body_len = body_len },
        );
        astgen.appendBodyWithFixups(body);
        ng.unstack();
    }

    /// Assumes nothing stacked on `ng`. Unstacks `ng`.
    fn setTryBody(ng: *NirGen, inst: Nir.Inst.Index, operand: Nir.Inst.Ref) !void {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;
        const body = ng.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        try astgen.extra.ensureUnusedCapacity(
            allocator,
            @typeInfo(Nir.Inst.Try).Struct.fields.len + body_len,
        );
        const nir_datas = astgen.instructions.items(.data);
        nir_datas[@intFromEnum(inst)].pl_node.payload_index = astgen.addExtraAssumeCapacity(
            Nir.Inst.Try{
                .operand = operand,
                .body_len = body_len,
            },
        );
        astgen.appendBodyWithFixups(body);
        ng.unstack();
    }

    /// Must be called with the following stack set up:
    ///  * ng (bottom)
    ///  * align_ng
    ///  * addrspace_ng
    ///  * section_ng
    ///  * cc_ng
    ///  * ret_ng
    ///  * body_ng (top)
    /// Unstacks all of those except for `ng`.
    fn addFunc(ng: *NirGen, args: struct {
        src_node: Ast.Node.Index,
        lbrace_line: u32 = 0,
        lbrace_column: u32 = 0,
        param_block: Nir.Inst.Index,

        align_ng: ?*NirGen,
        addrspace_ng: ?*NirGen,
        section_ng: ?*NirGen,
        cc_ng: ?*NirGen,
        ret_ng: ?*NirGen,
        body_ng: ?*NirGen,

        align_ref: Nir.Inst.Ref,
        addrspace_ref: Nir.Inst.Ref,
        section_ref: Nir.Inst.Ref,
        cc_ref: Nir.Inst.Ref,
        ret_ref: Nir.Inst.Ref,

        lib_name: Nir.NullTerminatedString,
        noalias_bits: u32,
        is_var_args: bool,
        is_inferred_error: bool,
        is_test: bool,
        is_extern: bool,
        is_noinline: bool,
    }) !Nir.Inst.Ref {
        assert(args.src_node != 0);
        const astgen = ng.astgen;
        const allocator = astgen.allocator;
        const ret_ref = if (args.ret_ref == .void_type) .none else args.ret_ref;
        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);

        try astgen.instructions.ensureUnusedCapacity(allocator, 1);

        var body: []Nir.Inst.Index = &[0]Nir.Inst.Index{};
        var ret_body: []Nir.Inst.Index = &[0]Nir.Inst.Index{};
        var src_locs_and_hash_buffer: [7]u32 = undefined;
        var src_locs_and_hash: []u32 = src_locs_and_hash_buffer[0..0];
        if (args.body_ng) |body_ng| {
            const ast = astgen.ast;
            const node_tags = ast.nodes.items(.tag);
            const node_datas = ast.nodes.items(.data);
            const token_starts = ast.tokens.items(.start);
            const fn_decl = args.src_node;
            assert(node_tags[fn_decl] == .fn_decl or node_tags[fn_decl] == .test_decl);
            const block = node_datas[fn_decl].rhs;
            const rbrace_start = token_starts[ast.lastToken(block)];
            astgen.advanceSourceCursor(rbrace_start);
            const rbrace_line: u32 = @intCast(astgen.source_line - ng.decl_line);
            const rbrace_column: u32 = @intCast(astgen.source_column);

            const columns = args.lbrace_column | (rbrace_column << 16);

            const proto_hash: std.zig.SrcHash = switch (node_tags[fn_decl]) {
                .fn_decl => sig_hash: {
                    const proto_node = node_datas[fn_decl].lhs;
                    break :sig_hash std.zig.hashSrc(ast.getNodeSource(proto_node));
                },
                .test_decl => std.zig.hashSrc(""), // tests don't have a prototype
                else => unreachable,
            };
            const proto_hash_arr: [4]u32 = @bitCast(proto_hash);

            src_locs_and_hash_buffer = .{
                args.lbrace_line,
                rbrace_line,
                columns,
                proto_hash_arr[0],
                proto_hash_arr[1],
                proto_hash_arr[2],
                proto_hash_arr[3],
            };
            src_locs_and_hash = &src_locs_and_hash_buffer;

            body = body_ng.instructionsSlice();
            if (args.ret_ng) |ret_ng|
                ret_body = ret_ng.instructionsSliceUpto(body_ng);
        } else {
            if (args.ret_ng) |ret_ng|
                ret_body = ret_ng.instructionsSlice();
        }
        const body_len = astgen.countBodyLenAfterFixups(body);

        if (args.cc_ref != .none or args.lib_name != .empty or args.is_var_args or args.is_test or
            args.is_extern or args.align_ref != .none or args.section_ref != .none or
            args.addrspace_ref != .none or args.noalias_bits != 0 or args.is_noinline)
        {
            var align_body: []Nir.Inst.Index = &.{};
            var addrspace_body: []Nir.Inst.Index = &.{};
            var section_body: []Nir.Inst.Index = &.{};
            var cc_body: []Nir.Inst.Index = &.{};
            if (args.ret_ng != null) {
                align_body = args.align_ng.?.instructionsSliceUpto(args.addrspace_ng.?);
                addrspace_body = args.addrspace_ng.?.instructionsSliceUpto(args.section_ng.?);
                section_body = args.section_ng.?.instructionsSliceUpto(args.cc_ng.?);
                cc_body = args.cc_ng.?.instructionsSliceUpto(args.ret_ng.?);
            }

            try astgen.extra.ensureUnusedCapacity(
                allocator,
                @typeInfo(Nir.Inst.FuncFancy).Struct.fields.len +
                    fancyFnExprExtraLen(astgen, align_body, args.align_ref) +
                    fancyFnExprExtraLen(astgen, addrspace_body, args.addrspace_ref) +
                    fancyFnExprExtraLen(astgen, section_body, args.section_ref) +
                    fancyFnExprExtraLen(astgen, cc_body, args.cc_ref) +
                    fancyFnExprExtraLen(astgen, ret_body, ret_ref) +
                    body_len + src_locs_and_hash.len +
                    @intFromBool(args.lib_name != .empty) +
                    @intFromBool(args.noalias_bits != 0),
            );
            const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.FuncFancy{
                .param_block = args.param_block,
                .body_len = body_len,
                .bits = .{
                    .is_var_args = args.is_var_args,
                    .is_inferred_error = args.is_inferred_error,
                    .is_test = args.is_test,
                    .is_extern = args.is_extern,
                    .is_noinline = args.is_noinline,
                    .has_lib_name = args.lib_name != .empty,
                    .has_any_noalias = args.noalias_bits != 0,

                    .has_align_ref = args.align_ref != .none,
                    .has_addrspace_ref = args.addrspace_ref != .none,
                    .has_section_ref = args.section_ref != .none,
                    .has_cc_ref = args.cc_ref != .none,
                    .has_ret_ty_ref = ret_ref != .none,

                    .has_align_body = align_body.len != 0,
                    .has_addrspace_body = addrspace_body.len != 0,
                    .has_section_body = section_body.len != 0,
                    .has_cc_body = cc_body.len != 0,
                    .has_ret_ty_body = ret_body.len != 0,
                },
            });
            if (args.lib_name != .empty) {
                astgen.extra.appendAssumeCapacity(@intFromEnum(args.lib_name));
            }

            const nir_datas = astgen.instructions.items(.data);
            if (align_body.len != 0) {
                astgen.extra.appendAssumeCapacity(countBodyLenAfterFixups(astgen, align_body));
                astgen.appendBodyWithFixups(align_body);
                const break_extra = nir_datas[@intFromEnum(align_body[align_body.len - 1])].@"break".payload_index;
                astgen.extra.items[break_extra + std.meta.fieldIndex(Nir.Inst.Break, "block_inst").?] =
                    @intFromEnum(new_index);
            } else if (args.align_ref != .none) {
                astgen.extra.appendAssumeCapacity(@intFromEnum(args.align_ref));
            }
            if (addrspace_body.len != 0) {
                astgen.extra.appendAssumeCapacity(countBodyLenAfterFixups(astgen, addrspace_body));
                astgen.appendBodyWithFixups(addrspace_body);
                const break_extra =
                    nir_datas[@intFromEnum(addrspace_body[addrspace_body.len - 1])].@"break".payload_index;
                astgen.extra.items[break_extra + std.meta.fieldIndex(Nir.Inst.Break, "block_inst").?] =
                    @intFromEnum(new_index);
            } else if (args.addrspace_ref != .none) {
                astgen.extra.appendAssumeCapacity(@intFromEnum(args.addrspace_ref));
            }
            if (section_body.len != 0) {
                astgen.extra.appendAssumeCapacity(countBodyLenAfterFixups(astgen, section_body));
                astgen.appendBodyWithFixups(section_body);
                const break_extra =
                    nir_datas[@intFromEnum(section_body[section_body.len - 1])].@"break".payload_index;
                astgen.extra.items[break_extra + std.meta.fieldIndex(Nir.Inst.Break, "block_inst").?] =
                    @intFromEnum(new_index);
            } else if (args.section_ref != .none) {
                astgen.extra.appendAssumeCapacity(@intFromEnum(args.section_ref));
            }
            if (cc_body.len != 0) {
                astgen.extra.appendAssumeCapacity(countBodyLenAfterFixups(astgen, cc_body));
                astgen.appendBodyWithFixups(cc_body);
                const break_extra = nir_datas[@intFromEnum(cc_body[cc_body.len - 1])].@"break".payload_index;
                astgen.extra.items[break_extra + std.meta.fieldIndex(Nir.Inst.Break, "block_inst").?] =
                    @intFromEnum(new_index);
            } else if (args.cc_ref != .none) {
                astgen.extra.appendAssumeCapacity(@intFromEnum(args.cc_ref));
            }
            if (ret_body.len != 0) {
                astgen.extra.appendAssumeCapacity(countBodyLenAfterFixups(astgen, ret_body));
                astgen.appendBodyWithFixups(ret_body);
                const break_extra = nir_datas[@intFromEnum(ret_body[ret_body.len - 1])].@"break".payload_index;
                astgen.extra.items[break_extra + std.meta.fieldIndex(Nir.Inst.Break, "block_inst").?] =
                    @intFromEnum(new_index);
            } else if (ret_ref != .none) {
                astgen.extra.appendAssumeCapacity(@intFromEnum(ret_ref));
            }

            if (args.noalias_bits != 0) {
                astgen.extra.appendAssumeCapacity(args.noalias_bits);
            }

            astgen.appendBodyWithFixups(body);
            astgen.extra.appendSliceAssumeCapacity(src_locs_and_hash);

            // Order is important when unstacking.
            if (args.body_ng) |body_ng| body_ng.unstack();
            if (args.ret_ng != null) {
                args.ret_ng.?.unstack();
                args.cc_ng.?.unstack();
                args.section_ng.?.unstack();
                args.addrspace_ng.?.unstack();
                args.align_ng.?.unstack();
            }

            try ng.instructions.ensureUnusedCapacity(allocator, 1);

            astgen.instructions.appendAssumeCapacity(.{
                .tag = .func_fancy,
                .data = .{ .pl_node = .{
                    .src_node = ng.nodeIndexToRelative(args.src_node),
                    .payload_index = payload_index,
                } },
            });
            ng.instructions.appendAssumeCapacity(new_index);
            return new_index.toRef();
        } else {
            try astgen.extra.ensureUnusedCapacity(
                allocator,
                @typeInfo(Nir.Inst.Func).Struct.fields.len + 1 +
                    fancyFnExprExtraLen(astgen, ret_body, ret_ref) +
                    body_len + src_locs_and_hash.len,
            );

            const ret_body_len = if (ret_body.len != 0)
                countBodyLenAfterFixups(astgen, ret_body)
            else
                @intFromBool(ret_ref != .none);

            const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.Func{
                .param_block = args.param_block,
                .ret_body_len = ret_body_len,
                .body_len = body_len,
            });
            const nir_datas = astgen.instructions.items(.data);
            if (ret_body.len != 0) {
                astgen.appendBodyWithFixups(ret_body);

                const break_extra = nir_datas[@intFromEnum(ret_body[ret_body.len - 1])].@"break".payload_index;
                astgen.extra.items[break_extra + std.meta.fieldIndex(Nir.Inst.Break, "block_inst").?] =
                    @intFromEnum(new_index);
            } else if (ret_ref != .none) {
                astgen.extra.appendAssumeCapacity(@intFromEnum(ret_ref));
            }
            astgen.appendBodyWithFixups(body);
            astgen.extra.appendSliceAssumeCapacity(src_locs_and_hash);

            // Order is important when unstacking.
            if (args.body_ng) |body_ng| body_ng.unstack();
            if (args.ret_ng) |ret_ng| ret_ng.unstack();
            if (args.cc_ng) |cc_ng| cc_ng.unstack();
            if (args.section_ng) |section_ng| section_ng.unstack();
            if (args.addrspace_ng) |addrspace_ng| addrspace_ng.unstack();
            if (args.align_ng) |align_ng| align_ng.unstack();

            try ng.instructions.ensureUnusedCapacity(allocator, 1);

            const tag: Nir.Inst.Tag = if (args.is_inferred_error) .func_inferred else .func;
            astgen.instructions.appendAssumeCapacity(.{
                .tag = tag,
                .data = .{ .pl_node = .{
                    .src_node = ng.nodeIndexToRelative(args.src_node),
                    .payload_index = payload_index,
                } },
            });
            ng.instructions.appendAssumeCapacity(new_index);
            return new_index.toRef();
        }
    }

    fn fancyFnExprExtraLen(astgen: *AstGen, body: []Nir.Inst.Index, ref: Nir.Inst.Ref) u32 {
        // In the case of non-empty body, there is one for the body length,
        // and then one for each instruction.
        return countBodyLenAfterFixups(astgen, body) + @intFromBool(ref != .none);
    }

    fn addVar(ng: *NirGen, args: struct {
        align_inst: Nir.Inst.Ref,
        lib_name: Nir.NullTerminatedString,
        var_type: Nir.Inst.Ref,
        init: Nir.Inst.Ref,
        is_extern: bool,
        is_const: bool,
        is_threadlocal: bool,
    }) !Nir.Inst.Ref {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.instructions.ensureUnusedCapacity(allocator, 1);

        try astgen.extra.ensureUnusedCapacity(
            allocator,
            @typeInfo(Nir.Inst.ExtendedVar).@"struct".fields.len +
                @intFromBool(args.lib_name != .empty) +
                @intFromBool(args.align_inst != .none) +
                @intFromBool(args.init != .none),
        );
        const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.ExtendedVar{
            .var_type = args.var_type,
        });
        if (args.lib_name != .empty) {
            astgen.extra.appendAssumeCapacity(@intFromEnum(args.lib_name));
        }
        if (args.align_inst != .none) {
            astgen.extra.appendAssumeCapacity(@intFromEnum(args.align_inst));
        }
        if (args.init != .none) {
            astgen.extra.appendAssumeCapacity(@intFromEnum(args.init));
        }

        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = .variable,
                .small = @bitCast(Nir.Inst.ExtendedVar.Small{
                    .has_lib_name = args.lib_name != .empty,
                    .has_align = args.align_inst != .none,
                    .has_init = args.init != .none,
                    .is_extern = args.is_extern,
                    .is_const = args.is_const,
                    .is_threadlocal = args.is_threadlocal,
                }),
                .operand = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addInt(ng: *NirGen, integer: u64) !Nir.Inst.Ref {
        return ng.add(.{
            .tag = .int,
            .data = .{ .int = integer },
        });
    }

    fn addIntBig(ng: *NirGen, limbs: []const std.math.big.Limb) !Nir.Inst.Ref {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;
        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.string_bytes.ensureUnusedCapacity(allocator, @sizeOf(std.math.big.Limb) * limbs.len);

        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(.{
            .tag = .int_big,
            .data = .{ .str = .{
                .start = @enumFromInt(astgen.string_bytes.items.len),
                .len = @intCast(limbs.len),
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        astgen.string_bytes.appendSliceAssumeCapacity(std.mem.sliceAsBytes(limbs));
        return new_index.toRef();
    }

    fn addFloat(ng: *NirGen, number: f64) !Nir.Inst.Ref {
        return ng.add(.{
            .tag = .float,
            .data = .{ .float = number },
        });
    }

    fn addUnNode(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        operand: Nir.Inst.Ref,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Nir.Inst.Ref {
        assert(operand != .none);
        return ng.add(.{
            .tag = tag,
            .data = .{ .un_node = .{
                .operand = operand,
                .src_node = ng.nodeIndexToRelative(src_node),
            } },
        });
    }

    fn makeUnNode(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        operand: Nir.Inst.Ref,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Nir.Inst.Index {
        assert(operand != .none);
        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        try ng.astgen.instructions.append(ng.astgen.allocator, .{
            .tag = tag,
            .data = .{ .un_node = .{
                .operand = operand,
                .src_node = ng.nodeIndexToRelative(src_node),
            } },
        });
        return new_index;
    }

    fn addPlNode(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
        extra: anytype,
    ) !Nir.Inst.Ref {
        const allocator = ng.astgen.allocator;
        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try ng.astgen.instructions.ensureUnusedCapacity(allocator, 1);

        const payload_index = try ng.astgen.addExtra(extra);
        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        ng.astgen.instructions.appendAssumeCapacity(.{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = ng.nodeIndexToRelative(src_node),
                .payload_index = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addPlNodePayloadIndex(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
        payload_index: u32,
    ) !Nir.Inst.Ref {
        return try ng.add(.{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = ng.nodeIndexToRelative(src_node),
                .payload_index = payload_index,
            } },
        });
    }

    /// Supports `param_ng` stacked on `ng`. Assumes nothing stacked on `param_ng`. Unstacks `param_ng`.
    fn addParam(
        ng: *NirGen,
        param_ng: *NirGen,
        tag: Nir.Inst.Tag,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.TokenIndex,
        name: Nir.NullTerminatedString,
        first_doc_comment: ?Ast.TokenIndex,
    ) !Nir.Inst.Index {
        const allocator = ng.astgen.allocator;
        const param_body = param_ng.instructionsSlice();
        const body_len = ng.astgen.countBodyLenAfterFixups(param_body);
        try ng.astgen.instructions.ensureUnusedCapacity(allocator, 1);
        try ng.astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.Param).Struct.fields.len + body_len);

        const doc_comment_index = if (first_doc_comment) |first|
            try ng.astgen.docCommentAsStringFromFirst(abs_tok_index, first)
        else
            .empty;

        const payload_index = ng.astgen.addExtraAssumeCapacity(Nir.Inst.Param{
            .name = name,
            .doc_comment = doc_comment_index,
            .body_len = @intCast(body_len),
        });
        ng.astgen.appendBodyWithFixups(param_body);
        param_ng.unstack();

        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        ng.astgen.instructions.appendAssumeCapacity(.{
            .tag = tag,
            .data = .{ .pl_tok = .{
                .src_tok = ng.tokenIndexToRelative(abs_tok_index),
                .payload_index = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn addExtendedPayload(ng: *NirGen, opcode: Nir.Inst.Extended, extra: anytype) !Nir.Inst.Ref {
        return addExtendedPayloadSmall(ng, opcode, undefined, extra);
    }

    fn addExtendedPayloadSmall(
        ng: *NirGen,
        opcode: Nir.Inst.Extended,
        small: u16,
        extra: anytype,
    ) !Nir.Inst.Ref {
        const allocator = ng.astgen.allocator;

        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try ng.astgen.instructions.ensureUnusedCapacity(allocator, 1);

        const payload_index = try ng.astgen.addExtra(extra);
        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        ng.astgen.instructions.appendAssumeCapacity(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = opcode,
                .small = small,
                .operand = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addExtendedMultiOp(
        ng: *NirGen,
        opcode: Nir.Inst.Extended,
        node: Ast.Node.Index,
        operands: []const Nir.Inst.Ref,
    ) !Nir.Inst.Ref {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.extra.ensureUnusedCapacity(
            allocator,
            @typeInfo(Nir.Inst.NodeMultiOp).Struct.fields.len + operands.len,
        );

        const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.NodeMultiOp{
            .src_node = ng.nodeIndexToRelative(node),
        });
        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = opcode,
                .small = @intCast(operands.len),
                .operand = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        astgen.appendRefsAssumeCapacity(operands);
        return new_index.toRef();
    }

    fn addExtendedMultiOpPayloadIndex(
        ng: *NirGen,
        opcode: Nir.Inst.Extended,
        payload_index: u32,
        trailing_len: usize,
    ) !Nir.Inst.Ref {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.instructions.ensureUnusedCapacity(allocator, 1);
        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = opcode,
                .small = @intCast(trailing_len),
                .operand = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addExtendedNodeSmall(
        ng: *NirGen,
        opcode: Nir.Inst.Extended,
        src_node: Ast.Node.Index,
        small: u16,
    ) !Nir.Inst.Ref {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.instructions.ensureUnusedCapacity(allocator, 1);
        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = opcode,
                .small = small,
                .operand = @bitCast(ng.nodeIndexToRelative(src_node)),
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addUnTok(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        operand: Nir.Inst.Ref,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.TokenIndex,
    ) !Nir.Inst.Ref {
        assert(operand != .none);
        return ng.add(.{
            .tag = tag,
            .data = .{ .un_tok = .{
                .operand = operand,
                .src_tok = ng.tokenIndexToRelative(abs_tok_index),
            } },
        });
    }

    fn makeUnTok(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        operand: Nir.Inst.Ref,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.TokenIndex,
    ) !Nir.Inst.Index {
        const astgen = ng.astgen;
        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        assert(operand != .none);
        try astgen.instructions.append(astgen.allocator, .{
            .tag = tag,
            .data = .{ .un_tok = .{
                .operand = operand,
                .src_tok = ng.tokenIndexToRelative(abs_tok_index),
            } },
        });
        return new_index;
    }

    fn addStrTok(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        str_index: Nir.NullTerminatedString,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.TokenIndex,
    ) !Nir.Inst.Ref {
        return ng.add(.{
            .tag = tag,
            .data = .{ .str_tok = .{
                .start = str_index,
                .src_tok = ng.tokenIndexToRelative(abs_tok_index),
            } },
        });
    }

    fn addSaveErrRetIndex(
        ng: *NirGen,
        cond: union(enum) {
            always: void,
            if_of_error_type: Nir.Inst.Ref,
        },
    ) !Nir.Inst.Index {
        return ng.addAsIndex(.{
            .tag = .save_err_ret_index,
            .data = .{ .save_err_ret_index = .{
                .operand = switch (cond) {
                    .if_of_error_type => |x| x,
                    else => .none,
                },
            } },
        });
    }

    const BranchTarget = union(enum) {
        ret,
        block: Nir.Inst.Index,
    };

    fn addRestoreErrRetIndex(
        ng: *NirGen,
        bt: BranchTarget,
        cond: union(enum) {
            always: void,
            if_non_error: Nir.Inst.Ref,
        },
        src_node: Ast.Node.Index,
    ) !Nir.Inst.Index {
        switch (cond) {
            .always => return ng.addAsIndex(.{
                .tag = .restore_err_ret_index_unconditional,
                .data = .{ .un_node = .{
                    .operand = switch (bt) {
                        .ret => .none,
                        .block => |b| b.toRef(),
                    },
                    .src_node = ng.nodeIndexToRelative(src_node),
                } },
            }),
            .if_non_error => |operand| switch (bt) {
                .ret => return ng.addAsIndex(.{
                    .tag = .restore_err_ret_index_fn_entry,
                    .data = .{ .un_node = .{
                        .operand = operand,
                        .src_node = ng.nodeIndexToRelative(src_node),
                    } },
                }),
                .block => |block| return (try ng.addExtendedPayload(
                    .restore_err_ret_index,
                    Nir.Inst.RestoreErrRetIndex{
                        .src_node = ng.nodeIndexToRelative(src_node),
                        .block = block.toRef(),
                        .operand = operand,
                    },
                )).toIndex().?,
            },
        }
    }

    fn addBreak(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        block_inst: Nir.Inst.Index,
        operand: Nir.Inst.Ref,
    ) !Nir.Inst.Index {
        const allocator = ng.astgen.allocator;
        try ng.instructions.ensureUnusedCapacity(allocator, 1);

        const new_index = try ng.makeBreak(tag, block_inst, operand);
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn makeBreak(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        block_inst: Nir.Inst.Index,
        operand: Nir.Inst.Ref,
    ) !Nir.Inst.Index {
        return ng.makeBreakCommon(tag, block_inst, operand, null);
    }

    fn addBreakWithSrcNode(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        block_inst: Nir.Inst.Index,
        operand: Nir.Inst.Ref,
        operand_src_node: Ast.Node.Index,
    ) !Nir.Inst.Index {
        const allocator = ng.astgen.allocator;
        try ng.instructions.ensureUnusedCapacity(allocator, 1);

        const new_index = try ng.makeBreakWithSrcNode(tag, block_inst, operand, operand_src_node);
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn makeBreakWithSrcNode(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        block_inst: Nir.Inst.Index,
        operand: Nir.Inst.Ref,
        operand_src_node: Ast.Node.Index,
    ) !Nir.Inst.Index {
        return ng.makeBreakCommon(tag, block_inst, operand, operand_src_node);
    }

    fn makeBreakCommon(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        block_inst: Nir.Inst.Index,
        operand: Nir.Inst.Ref,
        operand_src_node: ?Ast.Node.Index,
    ) !Nir.Inst.Index {
        const allocator = ng.astgen.allocator;
        try ng.astgen.instructions.ensureUnusedCapacity(allocator, 1);
        try ng.astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.Break).@"struct".fields.len);

        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        ng.astgen.instructions.appendAssumeCapacity(.{
            .tag = tag,
            .data = .{ .@"break" = .{
                .operand = operand,
                .payload_index = ng.astgen.addExtraAssumeCapacity(Nir.Inst.Break{
                    .operand_src_node = if (operand_src_node) |src_node|
                        ng.nodeIndexToRelative(src_node)
                    else
                        Nir.Inst.Break.no_src_node,
                    .block_inst = block_inst,
                }),
            } },
        });
        return new_index;
    }

    fn addBin(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        lhs: Nir.Inst.Ref,
        rhs: Nir.Inst.Ref,
    ) !Nir.Inst.Ref {
        assert(lhs != .none);
        assert(rhs != .none);
        return ng.add(.{
            .tag = tag,
            .data = .{ .bin = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
        });
    }

    fn addDefer(ng: *NirGen, index: u32, len: u32) !void {
        _ = try ng.add(.{
            .tag = .@"defer",
            .data = .{ .@"defer" = .{
                .index = index,
                .len = len,
            } },
        });
    }

    fn addDecl(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        decl_index: u32,
        src_node: Ast.Node.Index,
    ) !Nir.Inst.Ref {
        return ng.add(.{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = ng.nodeIndexToRelative(src_node),
                .payload_index = decl_index,
            } },
        });
    }

    fn addNode(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Nir.Inst.Ref {
        return ng.add(.{
            .tag = tag,
            .data = .{ .node = ng.nodeIndexToRelative(src_node) },
        });
    }

    fn addInstNode(
        ng: *NirGen,
        tag: Nir.Inst.Tag,
        inst: Nir.Inst.Index,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Nir.Inst.Ref {
        return ng.add(.{
            .tag = tag,
            .data = .{ .inst_node = .{
                .inst = inst,
                .src_node = ng.nodeIndexToRelative(src_node),
            } },
        });
    }

    fn addNodeExtended(
        ng: *NirGen,
        opcode: Nir.Inst.Extended,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Nir.Inst.Ref {
        return ng.add(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = opcode,
                .small = undefined,
                .operand = @bitCast(ng.nodeIndexToRelative(src_node)),
            } },
        });
    }

    fn addAllocExtended(
        ng: *NirGen,
        args: struct {
            /// Absolute node index. This function does the conversion to offset from Decl.
            node: Ast.Node.Index,
            type_inst: Nir.Inst.Ref,
            align_inst: Nir.Inst.Ref,
            is_const: bool,
            is_comptime: bool,
        },
    ) !Nir.Inst.Ref {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.extra.ensureUnusedCapacity(
            allocator,
            @typeInfo(Nir.Inst.AllocExtended).Struct.fields.len +
                @intFromBool(args.type_inst != .none) +
                @intFromBool(args.align_inst != .none),
        );
        const payload_index = ng.astgen.addExtraAssumeCapacity(Nir.Inst.AllocExtended{
            .src_node = ng.nodeIndexToRelative(args.node),
        });
        if (args.type_inst != .none) {
            astgen.extra.appendAssumeCapacity(@intFromEnum(args.type_inst));
        }
        if (args.align_inst != .none) {
            astgen.extra.appendAssumeCapacity(@intFromEnum(args.align_inst));
        }

        const has_type: u4 = @intFromBool(args.type_inst != .none);
        const has_align: u4 = @intFromBool(args.align_inst != .none);
        const is_const: u4 = @intFromBool(args.is_const);
        const is_comptime: u4 = @intFromBool(args.is_comptime);
        const small: u16 = has_type | (has_align << 1) | (is_const << 2) | (is_comptime << 3);

        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = .alloc,
                .small = small,
                .operand = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addAsm(
        ng: *NirGen,
        args: struct {
            tag: Nir.Inst.Extended,
            /// Absolute node index. This function does the conversion to offset from Decl.
            node: Ast.Node.Index,
            asm_source: Nir.NullTerminatedString,
            output_type_bits: u32,
            is_volatile: bool,
            outputs: []const Nir.Inst.Asm.Output,
            inputs: []const Nir.Inst.Asm.Input,
            clobbers: []const u32,
        },
    ) !Nir.Inst.Ref {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.instructions.ensureUnusedCapacity(allocator, 1);
        try astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.Asm).Struct.fields.len +
            args.outputs.len * @typeInfo(Nir.Inst.Asm.Output).Struct.fields.len +
            args.inputs.len * @typeInfo(Nir.Inst.Asm.Input).Struct.fields.len +
            args.clobbers.len);

        const payload_index = ng.astgen.addExtraAssumeCapacity(Nir.Inst.Asm{
            .src_node = ng.nodeIndexToRelative(args.node),
            .asm_source = args.asm_source,
            .output_type_bits = args.output_type_bits,
        });
        for (args.outputs) |output| {
            _ = ng.astgen.addExtraAssumeCapacity(output);
        }
        for (args.inputs) |input| {
            _ = ng.astgen.addExtraAssumeCapacity(input);
        }
        ng.astgen.extra.appendSliceAssumeCapacity(args.clobbers);

        //  * 0b00000000_000XXXXX - `outputs_len`.
        //  * 0b000000XX_XXX00000 - `inputs_len`.
        //  * 0b0XXXXX00_00000000 - `clobbers_len`.
        //  * 0bX0000000_00000000 - is volatile
        const small: u16 = @as(u16, @intCast(args.outputs.len)) |
            @as(u16, @intCast(args.inputs.len << 5)) |
            @as(u16, @intCast(args.clobbers.len << 10)) |
            (@as(u16, @intFromBool(args.is_volatile)) << 15);

        const new_index: Nir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(.{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = args.tag,
                .small = small,
                .operand = payload_index,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    /// Note that this returns a `Nir.Inst.Index` not a ref.
    /// Does *not* append the block instruction to the scope.
    /// Leaves the `payload_index` field undefined.
    fn makeBlockInst(ng: *NirGen, tag: Nir.Inst.Tag, node: Ast.Node.Index) !Nir.Inst.Index {
        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        const allocator = ng.astgen.allocator;
        try ng.astgen.instructions.append(allocator, .{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = ng.nodeIndexToRelative(node),
                .payload_index = undefined,
            } },
        });
        return new_index;
    }

    /// Note that this returns a `Zir.Inst.Index` not a ref.
    /// Does *not* append the block instruction to the scope.
    /// Leaves the `payload_index` field undefined. Use `setDeclaration` to finalize.
    fn makeDeclaration(ng: *NirGen, node: Ast.Node.Index) !Nir.Inst.Index {
        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        try ng.astgen.instructions.append(ng.astgen.allocator, .{
            .tag = .declaration,
            .data = .{ .declaration = .{
                .src_node = node,
                .payload_index = undefined,
            } },
        });
        return new_index;
    }

    /// Note that this returns a `Nir.Inst.Index` not a ref.
    /// Leaves the `payload_index` field undefined.
    fn addCondBr(ng: *NirGen, tag: Nir.Inst.Tag, node: Ast.Node.Index) !Nir.Inst.Index {
        const allocator = ng.astgen.allocator;
        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        try ng.astgen.instructions.append(allocator, .{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = ng.nodeIndexToRelative(node),
                .payload_index = undefined,
            } },
        });
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn setStruct(ng: *NirGen, inst: Nir.Inst.Index, args: struct {
        src_node: Ast.Node.Index,
        captures_len: u32,
        fields_len: u32,
        decls_len: u32,
        has_backing_int: bool,
        layout: std.builtin.Type.ContainerLayout,
        known_non_opv: bool,
        known_comptime_only: bool,
        is_tuple: bool,
        any_comptime_fields: bool,
        any_default_inits: bool,
        any_aligned_fields: bool,
        fields_hash: std.zig.SrcHash,
    }) !void {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        // Node 0 is valid for the root `struct_decl` of a file!
        assert(args.src_node != 0 or ng.parent.tag == .top);

        const fields_hash_arr: [4]u32 = @bitCast(args.fields_hash);

        try astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.StructDecl).@"struct".fields.len + 3);
        const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.StructDecl{
            .fields_hash_0 = fields_hash_arr[0],
            .fields_hash_1 = fields_hash_arr[1],
            .fields_hash_2 = fields_hash_arr[2],
            .fields_hash_3 = fields_hash_arr[3],
            .src_line = astgen.source_line,
            .src_node = args.src_node,
        });

        if (args.captures_len != 0) {
            astgen.extra.appendAssumeCapacity(args.captures_len);
        }
        if (args.fields_len != 0) {
            astgen.extra.appendAssumeCapacity(args.fields_len);
        }
        if (args.decls_len != 0) {
            astgen.extra.appendAssumeCapacity(args.decls_len);
        }
        astgen.instructions.set(@intFromEnum(inst), .{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = .struct_decl,
                .small = @bitCast(Nir.Inst.StructDecl.Small{
                    .has_captures_len = args.captures_len != 0,
                    .has_fields_len = args.fields_len != 0,
                    .has_decls_len = args.decls_len != 0,
                    .has_backing_int = args.has_backing_int,
                    .known_non_opv = args.known_non_opv,
                    .known_comptime_only = args.known_comptime_only,
                    .is_tuple = args.is_tuple,
                    .name_strategy = ng.anon_name_strategy,
                    .layout = args.layout,
                    .any_comptime_fields = args.any_comptime_fields,
                    .any_default_inits = args.any_default_inits,
                    .any_aligned_fields = args.any_aligned_fields,
                }),
                .operand = payload_index,
            } },
        });
    }

    fn setUnion(ng: *NirGen, inst: Nir.Inst.Index, args: struct {
        src_node: Ast.Node.Index,
        tag_type: Nir.Inst.Ref,
        captures_len: u32,
        body_len: u32,
        fields_len: u32,
        decls_len: u32,
        layout: std.builtin.Type.ContainerLayout,
        auto_enum_tag: bool,
        any_aligned_fields: bool,
        fields_hash: std.zig.SrcHash,
    }) !void {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        assert(args.src_node != 0);

        const fields_hash_arr: [4]u32 = @bitCast(args.fields_hash);

        try astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.UnionDecl).Struct.fields.len + 5);
        const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.UnionDecl{
            .fields_hash_0 = fields_hash_arr[0],
            .fields_hash_1 = fields_hash_arr[1],
            .fields_hash_2 = fields_hash_arr[2],
            .fields_hash_3 = fields_hash_arr[3],
            .src_node = ng.nodeIndexToRelative(args.src_node),
        });

        if (args.tag_type != .none) {
            astgen.extra.appendAssumeCapacity(@intFromEnum(args.tag_type));
        }
        if (args.captures_len != 0) {
            astgen.extra.appendAssumeCapacity(args.captures_len);
        }
        if (args.body_len != 0) {
            astgen.extra.appendAssumeCapacity(args.body_len);
        }
        if (args.fields_len != 0) {
            astgen.extra.appendAssumeCapacity(args.fields_len);
        }
        if (args.decls_len != 0) {
            astgen.extra.appendAssumeCapacity(args.decls_len);
        }
        astgen.instructions.set(@intFromEnum(inst), .{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = .union_decl,
                .small = @bitCast(Nir.Inst.UnionDecl.Small{
                    .has_tag_type = args.tag_type != .none,
                    .has_captures_len = args.captures_len != 0,
                    .has_body_len = args.body_len != 0,
                    .has_fields_len = args.fields_len != 0,
                    .has_decls_len = args.decls_len != 0,
                    .name_strategy = ng.anon_name_strategy,
                    .layout = args.layout,
                    .auto_enum_tag = args.auto_enum_tag,
                    .any_aligned_fields = args.any_aligned_fields,
                }),
                .operand = payload_index,
            } },
        });
    }

    fn setEnum(ng: *NirGen, inst: Nir.Inst.Index, args: struct {
        src_node: Ast.Node.Index,
        tag_type: Nir.Inst.Ref,
        captures_len: u32,
        body_len: u32,
        fields_len: u32,
        decls_len: u32,
        nonexhaustive: bool,
        fields_hash: std.zig.SrcHash,
    }) !void {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        assert(args.src_node != 0);

        const fields_hash_arr: [4]u32 = @bitCast(args.fields_hash);

        try astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.EnumDecl).Struct.fields.len + 5);
        const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.EnumDecl{
            .fields_hash_0 = fields_hash_arr[0],
            .fields_hash_1 = fields_hash_arr[1],
            .fields_hash_2 = fields_hash_arr[2],
            .fields_hash_3 = fields_hash_arr[3],
            .src_node = ng.nodeIndexToRelative(args.src_node),
        });

        if (args.tag_type != .none) {
            astgen.extra.appendAssumeCapacity(@intFromEnum(args.tag_type));
        }
        if (args.captures_len != 0) {
            astgen.extra.appendAssumeCapacity(args.captures_len);
        }
        if (args.body_len != 0) {
            astgen.extra.appendAssumeCapacity(args.body_len);
        }
        if (args.fields_len != 0) {
            astgen.extra.appendAssumeCapacity(args.fields_len);
        }
        if (args.decls_len != 0) {
            astgen.extra.appendAssumeCapacity(args.decls_len);
        }
        astgen.instructions.set(@intFromEnum(inst), .{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = .enum_decl,
                .small = @bitCast(Nir.Inst.EnumDecl.Small{
                    .has_tag_type = args.tag_type != .none,
                    .has_captures_len = args.captures_len != 0,
                    .has_body_len = args.body_len != 0,
                    .has_fields_len = args.fields_len != 0,
                    .has_decls_len = args.decls_len != 0,
                    .name_strategy = ng.anon_name_strategy,
                    .nonexhaustive = args.nonexhaustive,
                }),
                .operand = payload_index,
            } },
        });
    }

    fn setOpaque(ng: *NirGen, inst: Nir.Inst.Index, args: struct {
        src_node: Ast.Node.Index,
        captures_len: u32,
        decls_len: u32,
    }) !void {
        const astgen = ng.astgen;
        const allocator = astgen.allocator;

        assert(args.src_node != 0);

        try astgen.extra.ensureUnusedCapacity(allocator, @typeInfo(Nir.Inst.OpaqueDecl).Struct.fields.len + 2);
        const payload_index = astgen.addExtraAssumeCapacity(Nir.Inst.OpaqueDecl{
            .src_node = ng.nodeIndexToRelative(args.src_node),
        });

        if (args.captures_len != 0) {
            astgen.extra.appendAssumeCapacity(args.captures_len);
        }
        if (args.decls_len != 0) {
            astgen.extra.appendAssumeCapacity(args.decls_len);
        }
        astgen.instructions.set(@intFromEnum(inst), .{
            .tag = .extended,
            .data = .{ .extended = .{
                .opcode = .opaque_decl,
                .small = @bitCast(Nir.Inst.OpaqueDecl.Small{
                    .has_captures_len = args.captures_len != 0,
                    .has_decls_len = args.decls_len != 0,
                    .name_strategy = ng.anon_name_strategy,
                }),
                .operand = payload_index,
            } },
        });
    }

    fn add(ng: *NirGen, inst: Nir.Inst) !Nir.Inst.Ref {
        return (try ng.addAsIndex(inst)).toRef();
    }

    fn addAsIndex(ng: *NirGen, inst: Nir.Inst) !Nir.Inst.Index {
        const allocator = ng.astgen.allocator;
        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try ng.astgen.instructions.ensureUnusedCapacity(allocator, 1);

        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        ng.astgen.instructions.appendAssumeCapacity(inst);
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn reserveInstructionIndex(ng: *NirGen) !Nir.Inst.Index {
        const allocator = ng.astgen.allocator;
        try ng.instructions.ensureUnusedCapacity(allocator, 1);
        try ng.astgen.instructions.ensureUnusedCapacity(allocator, 1);

        const new_index: Nir.Inst.Index = @enumFromInt(ng.astgen.instructions.len);
        ng.astgen.instructions.len += 1;
        ng.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }

    fn addRet(ng: *NirGen, ri: ResultInfo, operand: Nir.Inst.Ref, node: Ast.Node.Index) !void {
        switch (ri.rl) {
            .ptr => |ptr_res| _ = try ng.addUnNode(.ret_load, ptr_res.inst, node),
            .coerced_ty => _ = try ng.addUnNode(.ret_node, operand, node),
            else => unreachable,
        }
    }

    fn addDbgVar(ng: *NirGen, tag: Nir.Inst.Tag, name: Nir.NullTerminatedString, inst: Nir.Inst.Ref) !void {
        if (ng.is_comptime) return;

        _ = try ng.add(.{ .tag = tag, .data = .{
            .str_op = .{
                .str = name,
                .operand = inst,
            },
        } });
    }
};

/// This can only be for short-lived references; the memory becomes invalidated
/// when another string is added.
fn nullTerminatedString(astgen: AstGen, index: Nir.NullTerminatedString) [*:0]const u8 {
    return @ptrCast(astgen.string_bytes.items[@intFromEnum(index)..]);
}

/// Local variables shadowing detection, including function parameters.
fn detectLocalShadowing(
    astgen: *AstGen,
    scope: *Scope,
    ident_name: Nir.NullTerminatedString,
    name_token: Ast.TokenIndex,
    token_bytes: []const u8,
    id_cat: Scope.IdCat,
) !void {
    const allocator = astgen.allocator;
    if (token_bytes[0] != '@' and isPrimitive(token_bytes)) {
        return astgen.failTokNotes(name_token, "name shadows primitive '{s}'", .{
            token_bytes,
        }, &[_]u32{
            try astgen.errNoteTok(name_token, "consider using @\"{s}\" to disambiguate", .{
                token_bytes,
            }),
        });
    }

    var s = scope;
    var outer_scope = false;
    while (true) switch (s.tag) {
        .local_val => {
            const local_val = s.cast(Scope.LocalVal).?;
            if (local_val.name == ident_name) {
                const name_slice = std.mem.span(astgen.nullTerminatedString(ident_name));
                const name = try allocator.dupe(u8, name_slice);
                defer allocator.free(name);
                if (outer_scope) {
                    return astgen.failTokNotes(name_token, "{s} '{s}' shadows {s} from outer scope", .{
                        @tagName(id_cat), name, @tagName(local_val.id_cat),
                    }, &[_]u32{
                        try astgen.errNoteTok(
                            local_val.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                }
                return astgen.failTokNotes(name_token, "redeclaration of {s} '{s}'", .{
                    @tagName(local_val.id_cat), name,
                }, &[_]u32{
                    try astgen.errNoteTok(
                        local_val.token_src,
                        "previous declaration here",
                        .{},
                    ),
                });
            }
            s = local_val.parent;
        },
        .local_ptr => {
            const local_ptr = s.cast(Scope.LocalPtr).?;
            if (local_ptr.name == ident_name) {
                const name_slice = std.mem.span(astgen.nullTerminatedString(ident_name));
                const name = try allocator.dupe(u8, name_slice);
                defer allocator.free(name);
                if (outer_scope) {
                    return astgen.failTokNotes(name_token, "{s} '{s}' shadows {s} from outer scope", .{
                        @tagName(id_cat), name, @tagName(local_ptr.id_cat),
                    }, &[_]u32{
                        try astgen.errNoteTok(
                            local_ptr.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                }
                return astgen.failTokNotes(name_token, "redeclaration of {s} '{s}'", .{
                    @tagName(local_ptr.id_cat), name,
                }, &[_]u32{
                    try astgen.errNoteTok(
                        local_ptr.token_src,
                        "previous declaration here",
                        .{},
                    ),
                });
            }
            s = local_ptr.parent;
        },
        .namespace => {
            outer_scope = true;
            const ns = s.cast(Scope.Namespace).?;
            const decl_node = ns.decls.get(ident_name) orelse {
                s = ns.parent;
                continue;
            };
            const name_slice = std.mem.span(astgen.nullTerminatedString(ident_name));
            const name = try allocator.dupe(u8, name_slice);
            defer allocator.free(name);
            return astgen.failTokNotes(name_token, "{s} shadows declaration of '{s}'", .{
                @tagName(id_cat), name,
            }, &[_]u32{
                try astgen.errNoteNode(decl_node, "declared here", .{}),
            });
        },
        .nir_gen => {
            s = s.cast(NirGen).?.parent;
            outer_scope = true;
        },
        .defer_normal, .defer_error => s = s.cast(Scope.Defer).?.parent,
        .top => break,
    };
}

const LineColumn = struct { u32, u32 };

/// Advances the source cursor to the main token of `node` if not in comptime scope.
/// Usually paired with `emitDbgStmt`.
fn maybeAdvanceSourceCursorToMainToken(ng: *NirGen, node: Ast.Node.Index) LineColumn {
    if (ng.is_comptime) return .{ ng.astgen.source_line - ng.decl_line, ng.astgen.source_column };

    const ast = ng.astgen.ast;
    const token_starts = ast.tokens.items(.start);
    const main_tokens = ast.nodes.items(.main_token);
    const node_start = token_starts[main_tokens[node]];
    ng.astgen.advanceSourceCursor(node_start);

    return .{ ng.astgen.source_line - ng.decl_line, ng.astgen.source_column };
}

/// Advances the source cursor to the beginning of `node`.
fn advanceSourceCursorToNode(astgen: *AstGen, node: Ast.Node.Index) void {
    const ast = astgen.ast;
    const token_starts = ast.tokens.items(.start);
    const node_start = token_starts[ast.firstToken(node)];
    astgen.advanceSourceCursor(node_start);
}

/// Advances the source cursor to an absolute byte offset `end` in the file.
fn advanceSourceCursor(astgen: *AstGen, end: usize) void {
    const source = astgen.ast.source;
    var i = astgen.source_offset;
    var line = astgen.source_line;
    var column = astgen.source_column;
    assert(i <= end);
    while (i < end) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }
    astgen.source_offset = i;
    astgen.source_line = line;
    astgen.source_column = column;
}

// TODO
/// Detects name conflicts for decls and fields, and populates `namespace.decls` with all named declarations.
/// Returns the number of declarations in the namespace, including unnamed declarations (e.g. `comptime` decls).
fn scanContainer(
    self: *AstGen,
    namespace: *Scope.Namespace,
    members: []const Ast.Node.Index,
    container_kind: enum { @"struct", @"union", @"enum", @"opaque" },
) !u32 {
    const allocator = self.allocator;
    const ast = self.ast;
    const node_tags = ast.nodes.items(.tag);
    const main_tokens = ast.nodes.items(.main_token);
    // const token_tags = ast.tokens.items(.tag);

    // This type forms a linked list of source tokens declaring the same name.
    const NameEntry = struct {
        tok: Ast.TokenIndex,
        /// Using a linked list here simplifies memory management, and is acceptable since
        ///ewntries are only allocated in error situations. The entries are allocated into the
        /// AstGen arena.
        next: ?*@This(),
    };

    // The maps below are allocated into this SFBA to avoid using the GPA for small namespaces.
    var sfba_state = std.heap.stackFallback(512, allocator);
    const sfba = sfba_state.get();

    var names: std.AutoArrayHashMapUnmanaged(Nir.NullTerminatedString, NameEntry) = .empty;
    // var test_names: std.AutoArrayHashMapUnmanaged(Nir.NullTerminatedString, NameEntry) = .empty;
    // var decltest_names: std.AutoArrayHashMapUnmanaged(Nir.NullTerminatedString, NameEntry) = .empty;
    defer {
        names.deinit(sfba);
        // test_names.deinit(sfba);
        // decltest_names.deinit(sfba);
    }

    var any_duplicates = false;
    var decl_count: u32 = 0;
    for (members) |member_node| {
        const Kind = enum { decl, field };
        const kind: Kind, const name_token = switch (node_tags[member_node]) {
            // .container_field_init,
            // .container_field_align,
            // .container_field,
            // => blk: {
            //     var full = ast.fullContainerField(member_node).?;
            //     switch (container_kind) {
            //         .@"struct", .@"opaque" => {},
            //         .@"union", .@"enum" => full.convertToNonTupleLike(self.tree.nodes),
            //     }
            //     if (full.ast.tuple_like) continue;
            //     break :blk .{ .field, full.ast.main_token };
            // },

            .decl,
            => blk: {
                decl_count += 1;
                break :blk .{ .decl, main_tokens[member_node] + 1 };
            },

            // .fn_proto_simple,
            // .fn_proto_multi,
            // .fn_proto_one,
            // .fn_proto,
            // .fn_decl,
            // => blk: {
            //     decl_count += 1;
            //     const ident = main_tokens[member_node] + 1;
            //     if (token_tags[ident] != .identifier) {
            //         try self.appendErrorNode(member_node, "missing function name", .{});
            //         continue;
            //     }
            //     break :blk .{ .decl, ident };
            // },

            // .test_decl => {
            //     decl_count += 1;
            //     // We don't want shadowing detection here, and test names work a bit differently, so
            //     // we must do the redeclaration detection ourselves.
            //     const test_name_token = main_tokens[member_node] + 1;
            //     const new_ent: NameEntry = .{
            //         .tok = test_name_token,
            //         .next = null,
            //     };
            //     switch (token_tags[test_name_token]) {
            //         else => {}, // unnamed test
            //         .string_literal => {
            //             const name = try self.strLitAsString(test_name_token);
            //             const gop = try test_names.getOrPut(sfba, name.index);
            //             if (gop.found_existing) {
            //                 var e = gop.value_ptr;
            //                 while (e.next) |n| e = n;
            //                 e.next = try self.arena.create(NameEntry);
            //                 e.next.?.* = new_ent;
            //                 any_duplicates = true;
            //             } else {
            //                 gop.value_ptr.* = new_ent;
            //             }
            //         },
            //         .identifier => {
            //             const name = try self.identAsString(test_name_token);
            //             const gop = try decltest_names.getOrPut(sfba, name);
            //             if (gop.found_existing) {
            //                 var e = gop.value_ptr;
            //                 while (e.next) |n| e = n;
            //                 e.next = try self.arena.create(NameEntry);
            //                 e.next.?.* = new_ent;
            //                 any_duplicates = true;
            //             } else {
            //                 gop.value_ptr.* = new_ent;
            //             }
            //         },
            //     }
            //     continue;
            // },

            else => unreachable,
        };

        const name_str_index = try self.identAsString(name_token);

        if (kind == .decl) {
            // Put the name straight into `decls`, even if there are compile errors.
            // This avoids incorrect "undeclared identifier" errors later on.
            try namespace.decls.put(allocator, name_str_index, member_node);
        }

        {
            const gop = try names.getOrPut(sfba, name_str_index);
            const new_ent: NameEntry = .{
                .tok = name_token,
                .next = null,
            };
            if (gop.found_existing) {
                var e = gop.value_ptr;
                while (e.next) |n| e = n;
                e.next = try self.arena.create(NameEntry);
                e.next.?.* = new_ent;
                any_duplicates = true;
                continue;
            } else {
                gop.value_ptr.* = new_ent;
            }
        }

        // For fields, we only needed the duplicate check! Decls have some more checks to do, though.
        switch (kind) {
            .decl => {},
            .field => continue,
        }

        const token_bytes = self.ast.tokenSlice(name_token);
        if (token_bytes[0] != '@' and isPrimitive(token_bytes)) {
            try self.appendErrorTokNotes(name_token, "name shadows primitive '{s}'", .{
                token_bytes,
            }, &.{
                try self.errNoteTok(name_token, "consider using @\"{s}\" to disambiguate", .{
                    token_bytes,
                }),
            });
            continue;
        }

        var s = namespace.parent;
        while (true) switch (s.tag) {
            .local_val => {
                const local_val = s.cast(Scope.LocalVal).?;
                if (local_val.name == name_str_index) {
                    try self.appendErrorTokNotes(name_token, "declaration '{s}' shadows {s} from outer scope", .{
                        token_bytes, @tagName(local_val.id_cat),
                    }, &.{
                        try self.errNoteTok(
                            local_val.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                    break;
                }
                s = local_val.parent;
            },
            .local_ptr => {
                const local_ptr = s.cast(Scope.LocalPtr).?;
                if (local_ptr.name == name_str_index) {
                    try self.appendErrorTokNotes(name_token, "declaration '{s}' shadows {s} from outer scope", .{
                        token_bytes, @tagName(local_ptr.id_cat),
                    }, &.{
                        try self.errNoteTok(
                            local_ptr.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                    break;
                }
                s = local_ptr.parent;
            },
            .namespace => s = s.cast(Scope.Namespace).?.parent,
            .nir_gen => s = s.cast(NirGen).?.parent,
            .defer_normal, .defer_error => s = s.cast(Scope.Defer).?.parent,
            .top => break,
        };
    }

    if (!any_duplicates) return decl_count;

    for (names.keys(), names.values()) |name, first| {
        if (first.next == null) continue;
        var notes: std.ArrayListUnmanaged(u32) = .empty;
        var prev: NameEntry = first;
        while (prev.next) |cur| : (prev = cur.*) {
            try notes.append(self.arena, try self.errNoteTok(cur.tok, "duplicate name here", .{}));
        }
        try notes.append(self.arena, try self.errNoteNode(namespace.node, "{s} declared here", .{@tagName(container_kind)}));
        const name_duped = try self.arena.dupe(u8, std.mem.span(self.nullTerminatedString(name)));
        try self.appendErrorTokNotes(first.tok, "duplicate {s} member name '{s}'", .{ @tagName(container_kind), name_duped }, notes.items);
    }

    // for (test_names.keys(), test_names.values()) |name, first| {
    //     if (first.next == null) continue;
    //     var notes: std.ArrayListUnmanaged(u32) = .empty;
    //     var prev: NameEntry = first;
    //     while (prev.next) |cur| : (prev = cur.*) {
    //         try notes.append(self.arena, try self.errNoteTok(cur.tok, "duplicate test here", .{}));
    //     }
    //     try notes.append(self.arena, try self.errNoteNode(namespace.node, "{s} declared here", .{@tagName(container_kind)}));
    //     const name_duped = try self.arena.dupe(u8, mem.span(self.nullTerminatedString(name)));
    //     try self.appendErrorTokNotes(first.tok, "duplicate test name '{s}'", .{name_duped}, notes.items);
    // }

    // for (decltest_names.keys(), decltest_names.values()) |name, first| {
    //     if (first.next == null) continue;
    //     var notes: std.ArrayListUnmanaged(u32) = .empty;
    //     var prev: NameEntry = first;
    //     while (prev.next) |cur| : (prev = cur.*) {
    //         try notes.append(self.arena, try self.errNoteTok(cur.tok, "duplicate decltest here", .{}));
    //     }
    //     try notes.append(self.arena, try self.errNoteNode(namespace.node, "{s} declared here", .{@tagName(container_kind)}));
    //     const name_duped = try self.arena.dupe(u8, mem.span(self.nullTerminatedString(name)));
    //     try self.appendErrorTokNotes(first.tok, "duplicate decltest '{s}'", .{name_duped}, notes.items);
    // }

    return decl_count;
}

fn isInferred(astgen: *AstGen, ref: Nir.Inst.Ref) bool {
    const inst = ref.toIndex() orelse return false;
    const nir_tags = astgen.instructions.items(.tag);
    return switch (nir_tags[@intFromEnum(inst)]) {
        .alloc_inferred,
        .alloc_inferred_mut,
        .alloc_inferred_comptime,
        .alloc_inferred_comptime_mut,
        => true,

        .extended => {
            const nir_data = astgen.instructions.items(.data);
            if (nir_data[@intFromEnum(inst)].extended.opcode != .alloc) return false;
            const small: Nir.Inst.AllocExtended.Small = @bitCast(nir_data[@intFromEnum(inst)].extended.small);
            return !small.has_type;
        },

        else => false,
    };
}

/// Assumes capacity for body has already been added. Needed capacity taking into
/// account fixups can be found with `countBodyLenAfterFixups`.
fn appendBodyWithFixups(astgen: *AstGen, body: []const Nir.Inst.Index) void {
    return appendBodyWithFixupsArrayList(astgen, &astgen.extra, body);
}

fn appendBodyWithFixupsArrayList(
    astgen: *AstGen,
    list: *std.ArrayListUnmanaged(u32),
    body: []const Nir.Inst.Index,
) void {
    for (body) |body_inst| {
        appendPossiblyRefdBodyInst(astgen, list, body_inst);
    }
}

fn appendPossiblyRefdBodyInst(
    astgen: *AstGen,
    list: *std.ArrayListUnmanaged(u32),
    body_inst: Nir.Inst.Index,
) void {
    list.appendAssumeCapacity(@intFromEnum(body_inst));
    const kv = astgen.ref_table.fetchRemove(body_inst) orelse return;
    const ref_inst = kv.value;
    return appendPossiblyRefdBodyInst(astgen, list, ref_inst);
}

fn countBodyLenAfterFixups(astgen: *AstGen, body: []const Nir.Inst.Index) u32 {
    var count = body.len;
    for (body) |body_inst| {
        var check_inst = body_inst;
        while (astgen.ref_table.get(check_inst)) |ref_inst| {
            count += 1;
            check_inst = ref_inst;
        }
    }
    return @intCast(count);
}

fn emitDbgStmt(ng: *NirGen, lc: LineColumn) !void {
    if (ng.is_comptime) return;
    if (ng.instructions.items.len > ng.instructions_top) {
        const astgen = ng.astgen;
        const last = ng.instructions.items[ng.instructions.items.len - 1];
        if (astgen.instructions.items(.tag)[@intFromEnum(last)] == .dbg_stmt) {
            astgen.instructions.items(.data)[@intFromEnum(last)].dbg_stmt = .{
                .line = lc[0],
                .column = lc[1],
            };
            return;
        }
    }

    _ = try ng.add(.{ .tag = .dbg_stmt, .data = .{
        .dbg_stmt = .{
            .line = lc[0],
            .column = lc[1],
        },
    } });
}

/// In some cases, Sema expects us to generate a `dbg_stmt` at the instruction
/// *index* directly preceding the next instruction (e.g. if a call is %10, it
/// expects a dbg_stmt at %9). TODO: this logic may allow redundant dbg_stmt
/// instructions; fix up Sema so we don't need it!
fn emitDbgStmtForceCurrentIndex(ng: *NirGen, lc: LineColumn) !void {
    const astgen = ng.astgen;
    if (ng.instructions.items.len > ng.instructions_top and
        @intFromEnum(ng.instructions.items[ng.instructions.items.len - 1]) == astgen.instructions.len - 1)
    {
        const last = astgen.instructions.len - 1;
        if (astgen.instructions.items(.tag)[last] == .dbg_stmt) {
            astgen.instructions.items(.data)[last].dbg_stmt = .{
                .line = lc[0],
                .column = lc[1],
            };
            return;
        }
    }

    _ = try ng.add(.{ .tag = .dbg_stmt, .data = .{
        .dbg_stmt = .{
            .line = lc[0],
            .column = lc[1],
        },
    } });
}

fn lowerAstErrors(astgen: *AstGen) !void {
    const ast = astgen.ast;
    assert(ast.errors.len > 0);

    const allocator = astgen.allocator;
    const parse_err = ast.errors[0];

    var msg: std.ArrayListUnmanaged(u8) = .{};
    defer msg.deinit(allocator);

    const token_starts = ast.tokens.items(.start);
    const token_tags = ast.tokens.items(.tag);

    var notes: std.ArrayListUnmanaged(u32) = .{};
    defer notes.deinit(allocator);

    if (token_tags[parse_err.token + @intFromBool(parse_err.token_is_prev)] == .invalid) {
        const tok = parse_err.token + @intFromBool(parse_err.token_is_prev);
        const bad_off: u32 = @intCast(ast.tokenSlice(parse_err.token + @intFromBool(parse_err.token_is_prev)).len);
        const byte_abs = token_starts[parse_err.token + @intFromBool(parse_err.token_is_prev)] + bad_off;
        try notes.append(allocator, try astgen.errNoteTokOff(tok, bad_off, "invalid byte: '{'}'", .{
            std.zig.fmtEscapes(ast.source[byte_abs..][0..1]),
        }));
    }

    for (ast.errors[1..]) |note| {
        if (!note.is_note) break;

        msg.clearRetainingCapacity();
        try ast.renderError(note, msg.writer(allocator));
        try notes.append(allocator, try astgen.errNoteTok(note.token, "{s}", .{msg.items}));
    }

    const extra_offset = ast.errorOffset(parse_err);
    msg.clearRetainingCapacity();
    try ast.renderError(parse_err, msg.writer(allocator));
    try astgen.appendErrorTokNotesOff(parse_err.token, extra_offset, "{s}", .{msg.items}, notes.items);
}

const DeclarationName = union(enum) {
    named: Ast.TokenIndex,
    named_test: Ast.TokenIndex,
    unnamed_test,
    decltest: Nir.NullTerminatedString,
    @"comptime",
    @"usingnamespace",
};

// TODO
/// Sets all extra data for a `declaration` instruction.
/// Unstacks `value_ng`, `align_ng`, `linksection_ng`, and `addrspace_ng`.
fn setDeclaration(
    decl_inst: Nir.Inst.Index,
    src_hash: std.zig.SrcHash,
    name: DeclarationName,
    src_line: u32,
    is_pub: bool,
    is_export: bool,
    doc_comment: Nir.NullTerminatedString,
    value_ng: *NirGen,
) !void {
    const astgen = value_ng.astgen;
    const allocator = astgen.allocator;

    const value_body = value_ng.instructionsSlice();
    const value_len = astgen.countBodyLenAfterFixups(value_body);

    const true_doc_comment: Nir.NullTerminatedString = switch (name) {
        .decltest => |test_name| test_name,
        else => doc_comment,
    };

    const src_hash_arr: [4]u32 = @bitCast(src_hash);

    const extra: Nir.Inst.Declaration = .{
        .src_hash_0 = src_hash_arr[0],
        .src_hash_1 = src_hash_arr[1],
        .src_hash_2 = src_hash_arr[2],
        .src_hash_3 = src_hash_arr[3],
        .name = switch (name) {
            .named => |tok| @enumFromInt(@intFromEnum(try astgen.identAsString(tok))),
            .named_test => |tok| @enumFromInt(@intFromEnum(try astgen.testNameString(tok))),
            .unnamed_test => .unnamed_test,
            .decltest => .decltest,
            .@"comptime" => .@"comptime",
            .@"usingnamespace" => .@"usingnamespace",
        },
        .src_line = src_line,
        .flags = .{
            .value_body_len = @intCast(value_len),
            .is_pub = is_pub,
            .is_export = is_export,
            .has_doc_comment = true_doc_comment != .empty,
            .has_align_linksection_addrspace = false,
        },
    };
    astgen.instructions.items(.data)[@intFromEnum(decl_inst)].declaration.payload_index = try astgen.addExtra(extra);
    if (extra.flags.has_doc_comment) {
        try astgen.extra.append(allocator, @intFromEnum(true_doc_comment));
    }
    try astgen.extra.ensureUnusedCapacity(allocator, value_len);
    astgen.appendBodyWithFixups(value_body);
    value_ng.unstack();
}

fn addExtra(self: *AstGen, extra: anytype) Allocator.Error!u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    try self.extra.ensureUnusedCapacity(self.allocator, fields.len);
    return self.addExtraAssumeCapacity(extra);
}

fn addExtraAssumeCapacity(self: *AstGen, extra: anytype) u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    const extra_index = self.extra.items.len;
    self.extra.items.len += fields.len;
    self.setExtra(extra_index, extra);
    return @intCast(extra_index);
}

fn setExtra(self: *AstGen, index: usize, extra: anytype) void {
    const fields = std.meta.fields(@TypeOf(extra));
    var i = index;
    inline for (fields) |field| {
        self.extra.items[i] = switch (field.type) {
            u32 => @field(extra, field.name),

            Nir.Inst.Ref,
            Nir.Inst.Index,
            Nir.Inst.Declaration.Name,
            Nir.NullTerminatedString,
            => @intFromEnum(@field(extra, field.name)),

            i32,
            // Nir.Inst.Call.Flags,
            // Nir.Inst.BuiltinCall.Flags,
            // Nir.Inst.SwitchBlock.Bits,
            // Nir.Inst.SwitchBlockErrUnion.Bits,
            // Nir.Inst.FuncFancy.Bits,
            Nir.Inst.Declaration.Flags,
            => @bitCast(@field(extra, field.name)),

            else => @compileError("setExtra: bad field type: " ++ @typeName(field.type)),
        };
        i += 1;
    }
}

fn reserveExtra(self: *AstGen, size: usize) Allocator.Error!u32 {
    const extra_index: u32 = @intCast(self.extra.items.len);
    try self.extra.resize(self.allocator, extra_index + size);
    return extra_index;
}

fn appendRefs(self: *AstGen, refs: []const Nir.Inst.Ref) Allocator.Error!void {
    return self.extra.appendSlice(self.allocator, refs);
}

fn appendRefsAssumeCapacity(self: *AstGen, refs: []const Nir.Inst.Ref) !void {
    return self.extra.appendSliceAssumeCapacity(refs);
}
