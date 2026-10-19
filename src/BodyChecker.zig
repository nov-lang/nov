const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const Literal = @import("Literal.zig");
const TypeResolver = @import("TypeResolver.zig");
const isPrimitive = @import("primitives.zig").isPrimitive;

pub const Error = Ir.BuildError;
const Known = struct { ty: Ir.TypeId, annotation: ?Module.Span = null };
const Binding = struct {
    known: Known,
    index: u32 = 0,
    initializer: ?Ast.Node.Index = null,
    name_span: Module.Span,
};
// Bounds recursive checking/probing, not the iterative literal-grouping walk.
const max_recursive_depth = 128;

/// Checks one function against the complete annotation table. Does not evaluate
/// operations, choose overflow behavior, or claim other declarations are checked.
/// Requires the complete, valid annotation table produced by Sema/TypeResolver.
/// The caller owns rollback of diagnostics and IR allocations on failure.
pub fn check(module: *Module, ir: *Ir, types: []const ?Ir.TypeId, id: Module.DeclId) Error!bool {
    return checkInner(module, ir, types, id, null);
}

/// Reuses the checked walk to emit values in callee/argument and operand order.
/// Source length and all function identities must already be registered in IR.
pub fn lower(module: *Module, ir: *Ir, types: []const ?Ir.TypeId, constants: []const ?Ir.ConstId, functions: []const ?Ir.FunctionId, id: Module.DeclId) Error!bool {
    if (constants.len != types.len or functions.len != types.len) return error.InvalidFunctionId;
    var arena = std.heap.ArenaAllocator.init(module.allocator);
    defer arena.deinit();
    var emitter: Emitter = .{ .allocator = arena.allocator(), .constants = constants, .functions = functions };
    if (!try checkInner(module, ir, types, id, &emitter)) return false;
    const body_node = module.ast.decl(module.getDeclaration(id).?.node).ast.init_node;
    const result = emitter.values.get(body_node) orelse return error.InvalidValueId;
    const function = functions[@intFromEnum(id)] orelse return error.InvalidFunctionId;
    try ir.setFunctionBody(function, emitter.instructions.items, result);
    return true;
}

const Emitter = struct {
    allocator: std.mem.Allocator,
    constants: []const ?Ir.ConstId,
    functions: []const ?Ir.FunctionId,
    instructions: std.ArrayList(Ir.Instruction) = .empty,
    values: std.AutoHashMapUnmanaged(Ast.Node.Index, Ir.ValueId) = .empty,

    fn record(self: *Emitter, node: Ast.Node.Index, ty: Ir.TypeId, span: Ir.Span, op: Ir.Operation) Error!void {
        if (self.values.contains(node)) return;
        if (self.instructions.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
        const id: Ir.ValueId = @enumFromInt(self.instructions.items.len);
        try self.instructions.append(self.allocator, .{ .ty = ty, .span = span, .op = op });
        try self.values.put(self.allocator, node, id);
    }

    fn value(self: *Emitter, node: Ast.Node.Index) Error!Ir.ValueId {
        return self.values.get(node) orelse error.InvalidValueId;
    }
};

fn checkInner(module: *Module, ir: *Ir, types: []const ?Ir.TypeId, id: Module.DeclId, emitter: ?*Emitter) Error!bool {
    if (types.len != module.declarations.items.len) return error.InvalidTypeId;
    const declaration = module.getDeclaration(id) orelse return error.InvalidTypeId;
    const type_id = types[@intFromEnum(id)] orelse return false;
    const ty = ir.getType(type_id) orelse return error.InvalidTypeId;
    if (ty != .function) return error.InvalidTypeId;
    const full = module.ast.decl(declaration.node);
    const proto = module.ast.fullFnProto(full.ast.type_node) orelse return error.InvalidTypeId;
    if (proto.ast.params.len != ty.function.params.len) return error.InvalidTypeId;
    var context: Context = .{ .module = module, .ir = ir, .types = types, .emitter = emitter };
    defer context.bindings.deinit(module.allocator);
    defer context.locals.deinit(module.allocator);
    var iterator = proto.iterate(&module.ast);
    var i: usize = 0;
    var valid = true;
    while (iterator.next()) |param| : (i += 1) {
        if (param.name_token) |token| {
            const name = module.ast.tokenSlice(token);
            const span = module.tokenSpan(token);
            if (module.lookup(name) != null or isPrimitive(name)) {
                const related = if (module.lookup(name)) |other| module.getDeclaration(other).?.name_span else null;
                try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_shadowing, .span = span, .related = related });
                valid = false;
                continue;
            }
            try context.bindings.put(module.allocator, name, .{ .index = @intCast(i), .name_span = span, .known = .{
                .ty = ty.function.params[i],
                .annotation = module.tokenSpan(module.ast.firstToken(param.type_expr)),
            } });
        }
    }
    if (!valid) return false;
    if (full.ast.init_node == 0) {
        try module.diagnostics.append(module.allocator, .{ .tag = .missing_initializer, .span = declaration.name_span });
        return false;
    }
    const annotation = module.tokenSpan(if (proto.ast.return_type) |node| module.ast.firstToken(node) else proto.lparen);
    context.return_type = .{ .ty = ty.function.result, .annotation = annotation };
    return (try context.expression(full.ast.init_node, .{ .ty = ty.function.result, .annotation = annotation }, 0)) != null;
}

const Context = struct {
    module: *Module,
    ir: *Ir,
    types: []const ?Ir.TypeId,
    emitter: ?*Emitter = null,
    return_type: Known = undefined,
    // Termination is independent of the contextual value type. A returning
    // expression has no merge value, and enclosing expressions alias its exit.
    terminated: bool = false,
    terminal: ?Ir.ValueId = null,
    bindings: std.StringHashMapUnmanaged(Binding) = .empty,
    locals: std.ArrayList([]const u8) = .empty,

    fn restoreScope(self: *Context, start: usize) void {
        for (self.locals.items[start..]) |name| _ = self.bindings.remove(name);
        self.locals.shrinkRetainingCapacity(start);
    }

    fn bindLocal(self: *Context, name: []const u8, binding: Binding) Error!void {
        try self.locals.append(self.module.allocator, name);
        try self.bindings.put(self.module.allocator, name, binding);
    }

    fn conflict(self: *Context, name: []const u8) ?Module.Span {
        if (self.bindings.get(name)) |binding| return binding.name_span;
        if (self.module.lookup(name)) |id| return self.module.getDeclaration(id).?.name_span;
        return null;
    }

    fn local(self: *Context, node: Ast.Node.Index, depth: usize) Error!bool {
        const ast = &self.module.ast;
        const decl = ast.decl(node).ast;
        const token = decl.let_token + 1 + @as(u32, @intFromBool(decl.is_mutable));
        const name = ast.tokenSlice(token);
        const name_span = self.module.tokenSpan(token);
        if (std.mem.startsWith(u8, name, "@\"")) {
            _ = try self.fail(.unsupported_name, node, null);
            return false;
        }
        const related = self.conflict(name);
        if (related != null or isPrimitive(name)) {
            try self.module.diagnostics.append(self.module.allocator, .{ .tag = .unsupported_shadowing, .span = name_span, .related = related });
            return false;
        }
        if (decl.is_mutable or decl.type_node == 0 or std.mem.eql(u8, name, "_")) {
            _ = try self.fail(.unsupported_local_declaration, node, null);
            return false;
        }
        // Named parameters introduce a local function definition, not a value
        // annotation. Closures/local function bodies are outside this slice.
        if (ast.fullFnProto(decl.type_node)) |proto| {
            var iterator = proto.iterate(ast);
            while (iterator.next()) |param| {
                if (param.name_token != null) {
                    _ = try self.fail(.unsupported_local_declaration, node, null);
                    return false;
                }
            }
        }
        if (decl.init_node == 0) {
            _ = try self.fail(.missing_initializer, node, null);
            return false;
        }
        const ty = (try TypeResolver.resolve(self.module, self.ir, decl.type_node)) orelse return false;
        const known: Known = .{ .ty = ty, .annotation = self.module.tokenSpan(ast.firstToken(decl.type_node)) };
        // Publish only after checking/evaluating the initializer: no self or
        // forward local references, and initialization happens exactly once.
        if (try self.expression(decl.init_node, known, depth + 1) == null) return false;
        if (self.terminated) return true;
        try self.bindLocal(name, .{ .known = known, .initializer = decl.init_node, .name_span = name_span });
        return true;
    }

    fn blockExpression(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.ast;
        var buffer: [2]Ast.Node.Index = undefined;
        const block = ast.fullBlock(&buffer, node).?;
        if (ast.firstToken(node) != block.ast.lbrace) return self.fail(.unsupported_body_expression, node, null);
        const start = self.locals.items.len;
        defer self.restoreScope(start);
        for (block.ast.statements, 0..) |statement, i| {
            if (self.terminated) return self.fail(.unreachable_code, statement, null);
            if (ast.nodes.items(.tag)[statement] == .decl) {
                if (!try self.local(statement, depth)) return null;
            } else if (i + 1 == block.ast.statements.len) {
                const ty = (try self.expression(statement, expected, depth + 1)) orelse return null;
                if (self.emitter) |emitter| try emitter.values.put(emitter.allocator, node, try emitter.value(statement));
                return ty;
            } else {
                const ty = (try self.expression(statement, null, depth + 1)) orelse return null;
                if (!self.terminated and self.ir.getType(ty).? != .void) return self.fail(.unused_block_value, statement, null);
            }
        }
        if (self.terminated) return if (expected) |want| want.ty else try self.ir.internType(.void);
        return self.fit(try self.ir.internType(.void), expected, node);
    }

    fn probeBlock(self: *Context, node: Ast.Node.Index, depth: usize) Error!?Known {
        const ast = &self.module.ast;
        var buffer: [2]Ast.Node.Index = undefined;
        const block = ast.fullBlock(&buffer, node).?;
        if (ast.firstToken(node) != block.ast.lbrace) return null;
        const start = self.locals.items.len;
        defer self.restoreScope(start);
        // Probing only supplies context, never emits/evaluates initializers or
        // keeps diagnostics. The actual left-to-right walk validates everything.
        const diagnostics_start = self.module.diagnostics.items.len;
        defer self.module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
        for (block.ast.statements, 0..) |statement, i| {
            if (ast.nodes.items(.tag)[statement] == .decl) {
                const decl = ast.decl(statement).ast;
                if (decl.is_mutable or decl.type_node == 0) return null;
                const token = decl.let_token + 1;
                const name = ast.tokenSlice(token);
                if (self.conflict(name) != null or isPrimitive(name)) return null;
                const ty = (try TypeResolver.resolve(self.module, self.ir, decl.type_node)) orelse return null;
                try self.bindLocal(name, .{ .known = .{ .ty = ty }, .initializer = decl.init_node, .name_span = self.module.tokenSpan(token) });
            } else if (i + 1 == block.ast.statements.len) {
                return self.probe(statement, depth + 1);
            }
        }
        return .{ .ty = try self.ir.internType(.void) };
    }

    fn span(self: *Context, node: Ast.Node.Index) Module.Span {
        return self.module.tokenSpan(self.module.ast.nodes.items(.main_token)[node]);
    }

    fn fail(self: *Context, tag: Module.Diagnostic.Tag, node: Ast.Node.Index, related: ?Module.Span) Error!?Ir.TypeId {
        const location = if (tag == .unreachable_code) self.module.tokenSpan(self.module.ast.firstToken(node)) else self.span(node);
        try self.module.diagnostics.append(self.module.allocator, .{ .tag = tag, .span = location, .related = related });
        return null;
    }

    fn fit(self: *Context, ty: Ir.TypeId, expected: ?Known, node: Ast.Node.Index) Error!?Ir.TypeId {
        if (self.terminated) return if (expected) |want| want.ty else ty;
        if (expected) |want| {
            if (ty != want.ty) return self.fail(.expression_type_mismatch, node, want.annotation);
        }
        return ty;
    }

    fn globalType(self: *Context, id: Module.DeclId) ?Known {
        const ty = self.types[@intFromEnum(id)] orelse return null;
        const full = self.module.ast.decl(self.module.getDeclaration(id).?.node);
        return .{ .ty = ty, .annotation = if (full.ast.type_node != 0) self.module.tokenSpan(self.module.ast.firstToken(full.ast.type_node)) else null };
    }

    fn printing(self: *Context, node: Ast.Node.Index) ?bool {
        const ast = &self.module.ast;
        if (ast.nodes.items(.tag)[node] != .builtin_literal) return null;
        const name = self.module.sourceSlice(self.span(node)).?;
        if (std.mem.eql(u8, name, "@print")) return false;
        if (std.mem.eql(u8, name, "@println")) return true;
        return null;
    }

    // Finds an already determined operand type without defaulting integer literals
    // or diagnosing unbound names. The actual walk still checks both operands.
    fn probe(self: *Context, node: Ast.Node.Index, depth: usize) Error!?Known {
        if (depth >= max_recursive_depth) return null;
        const ast = &self.module.ast;
        const data = ast.nodes.items(.data)[node];
        return switch (ast.nodes.items(.tag)[node]) {
            .identifier => blk: {
                const name = self.module.sourceSlice(self.span(node)).?;
                if (self.bindings.get(name)) |binding| break :blk binding.known;
                if (self.module.lookup(name)) |id| break :blk self.globalType(id);
                if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) break :blk .{ .ty = try self.ir.internType(.boolean) };
                break :blk null;
            },
            .block, .block_two, .block_newline, .block_two_newline => try self.probeBlock(node, depth),
            .@"if" => .{ .ty = try self.ir.internType(.void) },
            .if_else => blk: {
                const branch = ast.fullIf(node).?.ast;
                break :blk (try self.probe(branch.then_expr, depth + 1)) orelse try self.probe(branch.else_expr, depth + 1);
            },
            .negation, .bit_not => try self.probe(data.rhs, depth + 1),
            .add, .sub, .mul, .div, .mod => (try self.probe(data.lhs, depth + 1)) orelse try self.probe(data.rhs, depth + 1),
            .bool_not, .bool_and, .bool_or, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => .{ .ty = try self.ir.internType(.boolean) },
            .call, .call_one => blk: {
                const call = ast.fullCall(node).?;
                if (self.printing(call.ast.fn_expr) != null) break :blk .{ .ty = try self.ir.internType(.void) };
                const callee = (try self.probe(call.ast.fn_expr, depth + 1)) orelse break :blk null;
                const ty = self.ir.getType(callee.ty).?;
                if (ty != .function) break :blk null;
                break :blk .{ .ty = ty.function.result, .annotation = callee.annotation };
            },
            else => null,
        };
    }

    // With no numeric context, validate names and supported shape before asking
    // for a type. Otherwise missing names can masquerade as defaulting problems.
    fn validateUnconstrained(self: *Context, node: Ast.Node.Index, depth: usize) Error!bool {
        if (depth >= max_recursive_depth) {
            _ = try self.fail(.body_nesting_limit, node, null);
            return false;
        }
        const ast = &self.module.ast;
        const data = ast.nodes.items(.data)[node];
        switch (ast.nodes.items(.tag)[node]) {
            .number_literal => switch (std.zig.parseNumberLiteral(self.module.sourceSlice(self.span(node)).?)) {
                .int, .big_int => return true,
                .float => {
                    _ = try self.fail(.unsupported_literal, node, null);
                    return false;
                },
                .failure => {
                    _ = try self.fail(.invalid_literal, node, null);
                    return false;
                },
            },
            .identifier, .call, .call_one => return (try self.expression(node, null, depth + 1)) != null,
            .negation, .bit_not, .bool_not => return self.validateUnconstrained(data.rhs, depth + 1),
            .add, .sub, .mul, .div, .mod, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .bool_and, .bool_or => {
                const lhs = try self.validateUnconstrained(data.lhs, depth + 1);
                const rhs = try self.validateUnconstrained(data.rhs, depth + 1);
                return lhs and rhs;
            },
            .@"if", .if_else, .block, .block_two, .block_newline, .block_two_newline => return (try self.expression(node, null, depth + 1)) != null,
            else => {},
        }
        _ = try self.fail(.unsupported_body_expression, node, null);
        return false;
    }

    fn missingOperandType(self: *Context, node: Ast.Node.Index, lhs: Ast.Node.Index, rhs: ?Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const left_valid = try self.validateUnconstrained(lhs, depth + 1);
        const right_valid = if (rhs) |right| try self.validateUnconstrained(right, depth + 1) else true;
        if (!left_valid or !right_valid) return null;
        if (expected) |want| {
            if (self.ir.getType(want.ty).? != .integer) return self.fail(.operator_not_supported, node, want.annotation);
        }
        return self.fail(.literal_needs_type, lhs, null);
    }

    fn irSpan(self: *Context, node: Ast.Node.Index) Ir.Span {
        const ast = &self.module.ast;
        return .{
            .start = self.module.tokenSpan(ast.firstToken(node)).start,
            .end = self.module.tokenSpan(ast.lastToken(node)).end,
        };
    }

    fn emitNode(self: *Context, node: Ast.Node.Index, ty: Ir.TypeId) Error!bool {
        const emitter = self.emitter.?;
        if (emitter.values.contains(node)) return true;
        const ast = &self.module.ast;
        const data = ast.nodes.items(.data)[node];
        const tag = ast.nodes.items(.tag)[node];
        const op: Ir.Operation = switch (tag) {
            .identifier => blk: {
                const name = self.module.sourceSlice(self.span(node)).?;
                if (self.bindings.get(name)) |binding| {
                    if (binding.initializer) |initializer| {
                        try emitter.values.put(emitter.allocator, node, try emitter.value(initializer));
                        return true;
                    }
                    break :blk .{ .parameter = binding.index };
                }
                const id = self.module.lookup(name) orelse return error.InvalidFunctionId;
                if (emitter.functions[@intFromEnum(id)]) |function| break :blk .{ .function_ref = function };
                if (emitter.constants[@intFromEnum(id)]) |constant| break :blk .{ .constant = constant };
                _ = try self.fail(.unsupported_lowering, node, null);
                return false;
            },
            .block, .block_two, .block_newline, .block_two_newline => blk: {
                // Value-result blocks were already aliased before leaving scope.
                // Empty/declaration-ended blocks produce unit after their effects.
                break :blk .{ .constant = try self.ir.internConstant(.{ .ty = ty, .value = .void }) };
            },
            .negation, .bit_not, .bool_not => .{ .unary = .{
                .op = switch (tag) {
                    .negation => .negate,
                    .bit_not => .bit_not,
                    .bool_not => .bool_not,
                    else => unreachable,
                },
                .operand = try emitter.value(data.rhs),
            } },
            .add, .sub, .mul, .div, .mod, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => .{ .binary = .{
                .op = switch (tag) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .mul,
                    .div => .div,
                    .mod => .mod,
                    .equal_equal => .eq,
                    .bang_equal => .ne,
                    .less_than => .lt,
                    .less_or_equal => .le,
                    .greater_than => .gt,
                    .greater_or_equal => .ge,
                    else => unreachable,
                },
                .lhs = try emitter.value(data.lhs),
                .rhs = try emitter.value(data.rhs),
            } },
            .call, .call_one => blk: {
                const call = ast.fullCall(node).?;
                const args = try emitter.allocator.alloc(Ir.ValueId, call.ast.args.len);
                for (call.ast.args, args) |arg, *value| value.* = try emitter.value(arg);
                if (self.printing(call.ast.fn_expr)) |newline| break :blk .{ .print = .{ .args = args, .newline = newline } };
                break :blk .{ .call = .{ .callee = try emitter.value(call.ast.fn_expr), .args = args } };
            },
            else => {
                _ = try self.fail(.unsupported_lowering, node, null);
                return false;
            },
        };
        try emitter.record(node, ty, self.irSpan(node), op);
        return true;
    }

    fn expression(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ty = if (self.terminated)
            (if (expected) |want| want.ty else try self.ir.internType(.void))
        else
            (try self.expressionInner(node, expected, depth)) orelse return null;
        if (self.emitter) |emitter| {
            if (self.terminated) {
                try emitter.values.put(emitter.allocator, node, self.terminal.?);
            } else if (!try self.emitNode(node, ty)) return null;
        }
        return ty;
    }

    fn expressionInner(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        if (depth >= max_recursive_depth) return self.fail(.body_nesting_limit, node, null);
        const hint = expected orelse try self.probe(node, depth);
        if (hint) |known| {
            switch (try Literal.check(self.module, self.ir, node, known.ty, known.annotation)) {
                .constant => |constant| {
                    if (self.emitter) |emitter| try emitter.record(node, known.ty, self.irSpan(node), .{ .constant = constant });
                    return known.ty;
                },
                .failed => return null,
                .not_literal => {},
            }
        }
        const ast = &self.module.ast;
        const data = ast.nodes.items(.data)[node];
        switch (ast.nodes.items(.tag)[node]) {
            .identifier => {
                const name = self.module.sourceSlice(self.span(node)).?;
                if (self.bindings.get(name)) |binding| return self.fit(binding.known.ty, expected, node);
                if (self.module.lookup(name)) |id| {
                    const declaration = self.module.getDeclaration(id).?;
                    if (isPrimitive(name)) return self.fail(.unsupported_shadowing, node, declaration.name_span);
                    const known = self.globalType(id) orelse return self.fail(.unresolved_value_type, node, declaration.name_span);
                    return self.fit(known.ty, expected, node);
                }
                if (std.mem.startsWith(u8, name, "@\"")) return self.fail(.unsupported_name, node, null);
                return self.fail(if (isPrimitive(name)) .unsupported_body_expression else .unknown_name, node, null);
            },
            .@"return" => {
                if (data.rhs != 0) {
                    if (try self.expression(data.rhs, self.return_type, depth + 1) == null) return null;
                    if (self.terminated) return self.return_type.ty;
                } else if (self.ir.getType(self.return_type.ty).? != .void) {
                    return self.fail(.expression_type_mismatch, node, self.return_type.annotation);
                }
                const unit = try self.ir.internType(.void);
                if (self.emitter) |emitter| {
                    try emitter.record(node, unit, self.irSpan(node), .{ .return_value = if (data.rhs != 0) try emitter.value(data.rhs) else null });
                    self.terminal = try emitter.value(node);
                }
                self.terminated = true;
                return if (expected) |want| want.ty else unit;
            },
            .number_literal => return self.fail(.literal_needs_type, node, null),
            .char_literal, .string_literal => return self.fail(.unsupported_literal, node, null),
            .block, .block_two, .block_newline, .block_two_newline => return self.blockExpression(node, expected, depth),
            .@"if", .if_else => {
                const branch = ast.fullIf(node).?.ast;
                const boolean = try self.ir.internType(.boolean);
                const condition = try self.expression(branch.cond_expr, .{ .ty = boolean }, depth + 1);
                if (self.terminated) return if (expected) |want| want.ty else boolean;
                const result: ?Known = if (branch.else_expr == 0)
                    .{ .ty = try self.ir.internType(.void), .annotation = self.span(node) }
                else
                    hint;
                const then_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
                const then_ty = try self.expression(branch.then_expr, result, depth + 1);
                const then_returns = self.terminated;
                self.terminated = false;
                const else_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
                const else_ty = if (branch.else_expr != 0)
                    try self.expression(branch.else_expr, result, depth + 1)
                else
                    try self.ir.internType(.void);
                if (condition == null or then_ty == null or else_ty == null) return null;
                const else_returns = self.terminated;
                self.terminated = then_returns and else_returns;
                const merged = if (self.terminated) try self.ir.internType(.void) else if (then_returns) else_ty.? else then_ty.?;
                if (!then_returns and !else_returns and then_ty.? != else_ty.?) return self.fail(.expression_type_mismatch, branch.else_expr, self.span(branch.then_expr));
                if (try self.fit(merged, expected, node) == null) return null;
                if (self.emitter) |emitter| {
                    try emitter.record(node, merged, self.irSpan(node), .{ .branch = .{
                        .returns = self.terminated,
                        .condition = try emitter.value(branch.cond_expr),
                        .then_start = then_start,
                        .else_start = else_start,
                        .then_value = try emitter.value(branch.then_expr),
                        .else_value = if (branch.else_expr != 0) try emitter.value(branch.else_expr) else null,
                    } });
                    if (self.terminated) self.terminal = try emitter.value(node);
                }
                return if (self.terminated) (if (expected) |want| want.ty else merged) else merged;
            },
            .add, .sub, .mul, .div, .mod => {
                const integer_context = if (expected) |want| (if (self.ir.getType(want.ty).? == .integer) expected else null) else null;
                const operand = (try self.probe(data.lhs, depth + 1)) orelse (try self.probe(data.rhs, depth + 1)) orelse integer_context orelse return self.missingOperandType(node, data.lhs, data.rhs, expected, depth);
                if (self.ir.getType(operand.ty).? != .integer) return self.fail(.operator_not_supported, node, operand.annotation);
                const lhs = try self.expression(data.lhs, operand, depth + 1);
                const rhs = try self.expression(data.rhs, operand, depth + 1);
                if (lhs == null or rhs == null) return null;
                return self.fit(operand.ty, expected, node);
            },
            .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => {
                const operand = (try self.probe(data.lhs, depth + 1)) orelse (try self.probe(data.rhs, depth + 1)) orelse return self.missingOperandType(node, data.lhs, data.rhs, null, depth);
                const ty = self.ir.getType(operand.ty).?;
                const equality = ast.nodes.items(.tag)[node] == .equal_equal or ast.nodes.items(.tag)[node] == .bang_equal;
                if (ty != .integer and !(ty == .boolean and equality)) return self.fail(.operator_not_supported, node, operand.annotation);
                const lhs = try self.expression(data.lhs, operand, depth + 1);
                const rhs = try self.expression(data.rhs, operand, depth + 1);
                if (lhs == null or rhs == null) return null;
                return self.fit(try self.ir.internType(.boolean), expected, node);
            },
            .bool_and, .bool_or, .bool_not => {
                const boolean = try self.ir.internType(.boolean);
                const operand: Known = .{ .ty = boolean, .annotation = if (expected) |want| want.annotation else null };
                const lhs = if (ast.nodes.items(.tag)[node] != .bool_not) try self.expression(data.lhs, operand, depth + 1) else boolean;
                if (self.terminated) return boolean;
                const rhs_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
                const rhs = try self.expression(data.rhs, operand, depth + 1);
                if (lhs == null or rhs == null) return null;
                if (self.emitter) |emitter| {
                    const tag = ast.nodes.items(.tag)[node];
                    if (tag != .bool_not) try emitter.record(node, boolean, self.irSpan(node), .{ .short_circuit = .{
                        .kind = if (tag == .bool_and) .@"and" else .@"or",
                        .lhs = try emitter.value(data.lhs),
                        .rhs_start = rhs_start,
                        .rhs = try emitter.value(data.rhs),
                    } });
                }
                if (ast.nodes.items(.tag)[node] != .bool_not) self.terminated = false;
                return self.fit(boolean, expected, node);
            },
            .negation, .bit_not => {
                const integer_context = if (expected) |want| (if (self.ir.getType(want.ty).? == .integer) expected else null) else null;
                const operand = (try self.probe(data.rhs, depth + 1)) orelse integer_context orelse return self.missingOperandType(node, data.rhs, null, expected, depth);
                const ty = self.ir.getType(operand.ty).?;
                if (ty != .integer) return self.fail(.operator_not_supported, node, operand.annotation);
                if (ast.nodes.items(.tag)[node] == .negation and ty.integer.signedness == .unsigned) return self.fail(.unsupported_body_expression, node, null);
                if (try self.expression(data.rhs, operand, depth + 1) == null) return null;
                return self.fit(operand.ty, expected, node);
            },
            .call, .call_one => {
                const call = ast.fullCall(node).?;
                if (self.printing(call.ast.fn_expr) != null) {
                    var valid = true;
                    for (call.ast.args) |arg| {
                        const arg_type = (try self.expression(arg, null, depth + 1)) orelse {
                            valid = false;
                            continue;
                        };
                        const arg_ty = self.ir.getType(arg_type).?;
                        if (!self.terminated and arg_ty != .boolean and arg_ty != .integer) {
                            _ = try self.fail(.unsupported_body_expression, arg, null);
                            valid = false;
                        }
                    }
                    if (!valid) return null;
                    return self.fit(try self.ir.internType(.void), expected, node);
                }
                const callee = (try self.expression(call.ast.fn_expr, null, depth + 1)) orelse return null;
                if (self.terminated) return if (expected) |want| want.ty else try self.ir.internType(.void);
                const origin = try self.probe(call.ast.fn_expr, depth + 1);
                const annotation = if (origin) |known| known.annotation else null;
                const ty = self.ir.getType(callee).?;
                if (ty != .function) return self.fail(.not_callable, node, annotation);
                if (call.ast.args.len != ty.function.params.len) return self.fail(.argument_count_mismatch, node, annotation);
                var valid = true;
                for (call.ast.args, ty.function.params) |arg, param| {
                    if (try self.expression(arg, .{ .ty = param, .annotation = annotation }, depth + 1) == null) valid = false;
                }
                if (!valid) return null;
                return self.fit(ty.function.result, expected, node);
            },
            else => return self.fail(.unsupported_body_expression, node, null),
        }
    }
};

test "returns use enclosing signatures and merge only continuing paths" {
    var module = try Module.init(std.testing.allocator, "return.nov",
        \\let direct: () -> int = return 42
        \\let unit: () -> void = {return}
        \\let effect: () -> void = return @println(true)
        \\let fallback: (c: bool) -> int = {if c {return 1}
        \\    2}
        \\let nested: (c: bool) -> int = {if c {{return 3}} else {return 4}}
        \\let mixed: (c: bool) -> int = if c {return 5} else {6}
        \\let reverse: (c: bool) -> int = if c {7} else {return 8}
        \\let typed: (c: bool, n: int) -> void = {
        \\    let x: bool = if c {return} else {true}
        \\    @println(if x {n} else {return})
        \\}
        \\let short: (c: bool) -> int = {let b: bool = c and {return 9}
        \\    if b {return 10}
        \\    11}
        \\let init: () -> int = {let x: bool = {return 12}
        \\}
        \\let nestedreturn: () -> int = return {return 13}
        \\let condition: () -> int = if {return 14} {} else {}
        \\let negate: () -> int = !{return 15}
    );
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
    try std.testing.expect(!module.hasErrors());
}

test "return diagnostics retain value locations signature notes and reachability" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag, token: []const u8 };
    for ([_]Case{
        .{ .source = "let f: () -> int = return", .tag = .expression_type_mismatch, .token = "return" },
        .{ .source = "let f: () -> int = return false", .tag = .literal_type_mismatch, .token = "false" },
        .{ .source = "let f: () -> void = return 1", .tag = .literal_type_mismatch, .token = "1" },
        .{ .source = "let f: () -> int = return missing", .tag = .unknown_name, .token = "missing" },
        .{ .source = "let f: () -> int = {let b: bool = {return false}\n1}", .tag = .literal_type_mismatch, .token = "false" },
        .{ .source = "let f: () -> int = {return 1\n2}", .tag = .unreachable_code, .token = "2" },
        .{ .source = "let f: () -> int = {{return 1}\nlet n: int = 2\n}", .tag = .unreachable_code, .token = "let" },
        .{ .source = "let f: (c: bool) -> int = {if c {return 1} else {return 2}\n3}", .tag = .unreachable_code, .token = "3" },
        .{ .source = "let f: (c: bool) -> int = {if c {return 1}}", .tag = .expression_type_mismatch, .token = "if" },
        .{ .source = "let f: () -> int = if true {return 1} else {return false}", .tag = .literal_type_mismatch, .token = "false" },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "return.nov", case.source);
        defer module.deinit();

        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        try std.testing.expect(module.hasErrors());
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(diagnostic.span).?);
        if (case.tag == .literal_type_mismatch or case.tag == .expression_type_mismatch) try std.testing.expect(diagnostic.related != null);
    }
}

test "if branches infer bidirectionally and keep separate lexical scopes" {
    var module = try Module.init(std.testing.allocator, "if.nov",
        \\let id: (x: int) -> int = x
        \\let f: (c: bool, n: int) -> int = if c {1} else if !c {n} else {2}
        \\let out: (c: bool, n: int) -> void = {
        \\    @println(if c {1} else {n}, if c {n} else {2})
        \\    @println({if c {1} else {2}} == n)
        \\    let callback: (int) -> int = if c {id} else {id}
        \\    if c {let reused: int = callback(1)
        \\        @println(reused)} else {let reused: int = n
        \\        @println(reused)}
        \\    if c {@println(true)}
        \\}
    );
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
    try std.testing.expect(!module.hasErrors());
}

test "if diagnostics check conditions both branches results and lexical visibility" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag, token: []const u8 };
    for ([_]Case{
        .{ .source = "let f: (n: int) -> int = if n {1} else {2}", .tag = .expression_type_mismatch, .token = "n" },
        .{ .source = "let f: () -> int = if true {1} else {false}", .tag = .literal_type_mismatch, .token = "false" },
        .{ .source = "let f: () -> void = if false {missing} else {}", .tag = .unknown_name, .token = "missing" },
        .{ .source = "let f: () -> void = if true {} else {missing}", .tag = .unknown_name, .token = "missing" },
        .{ .source = "let f: (n: int) -> void = if true {n}", .tag = .expression_type_mismatch, .token = "n" },
        .{ .source = "let f: () -> int = if true {}", .tag = .expression_type_mismatch, .token = "if" },
        .{ .source = "let f: () -> void = @println(if true {1} else {2})", .tag = .literal_needs_type, .token = "1" },
        .{ .source = "let f: () -> void = if true {let hidden: bool = true\n} else {@println(hidden)}", .tag = .unknown_name, .token = "hidden" },
        .{ .source = "let f: () -> bool = {if true {let hidden: bool = true\n}\nhidden}", .tag = .unknown_name, .token = "hidden" },
        .{ .source = "let f: (n: int, u: uint) -> void = @println(if true {n} else {u})", .tag = .expression_type_mismatch, .token = "u" },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "if.nov", case.source);
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        try std.testing.expect(module.hasErrors());
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(diagnostic.span).?);
    }
}

test "typed locals and final expressions work across lexical blocks" {
    var module = try Module.init(std.testing.allocator, "locals.nov",
        \\let id: (x: i32) -> i32 = x
        \\let f: (p: i32) -> i32 = {
        \\    let x: i32 = p + 1
        \\    @println(x)
        \\    let y: i32 = {
        \\        let inner: i32 = id(x)
        \\        inner + 1
        \\    }
        \\    y
        \\}
        \\let out: () -> void = {
        \\    {let reused: bool = true
        \\    }
        \\    {let reused: bool = false
        \\    }
        \\    @println({let n: i32 = 2
        \\        n} == 2)
        \\}
    );
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
}

test "local scope declaration and block result diagnostics" {
    const Case = struct { body: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .body = "{let n: i32 = n\nn}", .tag = .unknown_name },
        .{ .body = "{let n: i32 = later\nlet later: i32 = 1\nn}", .tag = .unknown_name },
        .{ .body = "{{let inner: i32 = 1\n}\ninner}", .tag = .unknown_name },
        .{ .body = "{let n: i32 = 1\nlet n: i32 = 2\nn}", .tag = .unsupported_shadowing },
        .{ .body = "{let p: i32 = 1\np}", .tag = .unsupported_shadowing },
        .{ .body = "{let i32: i32 = 1\ni32}", .tag = .unsupported_shadowing },
        .{ .body = "{let mut n: i32 = 1\nn}", .tag = .unsupported_local_declaration },
        .{ .body = "{let n = 1\nn}", .tag = .unsupported_local_declaration },
        .{ .body = "{let n: i32\nn}", .tag = .missing_initializer },
        .{ .body = "{let n: i8 = 128\nn}", .tag = .literal_out_of_range },
        .{ .body = "{let n: missing = 1\nn}", .tag = .unknown_type },
        .{ .body = "{let f: i32 = 1\nf}", .tag = .unsupported_shadowing },
        .{ .body = "{let n: i32 = true\nn}", .tag = .literal_type_mismatch },
        .{ .body = "{let n: int = 1\nlet u: uint = n\np}", .tag = .expression_type_mismatch },
        .{ .body = "{let n: i32 = 1\nn = 2\nn}", .tag = .unsupported_body_expression },
        .{ .body = "{let n: i32 = 1\n{let hidden: i32 = n\n}\nhidden} + 1", .tag = .unknown_name },
        .{ .body = "{let n: i32 = 1\n}", .tag = .expression_type_mismatch },
        .{ .body = "{p\np}", .tag = .unused_block_value },
        .{ .body = "{let n: i32 = 1\n{let n: i32 = 2\nn}}", .tag = .unsupported_shadowing },
        .{ .body = "{let callback: (x: i32) -> i32 = x\n1}", .tag = .unsupported_local_declaration },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: (p: i32) -> i32 = {s}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "locals.nov", source);
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
    }
}

test "local shadowing reports both declarations and locals do not leak to other functions" {
    var module = try Module.init(std.testing.allocator, "scope.nov", "let first: () -> void = {let secret: i32 = 1\n{let secret: i32 = 2\n}\n}\nlet second: () -> i32 = secret");
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 2), module.diagnostics.items.len);
    const diagnostic = module.diagnostics.items[0];
    try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_shadowing, diagnostic.tag);
    try std.testing.expectEqualStrings("secret", module.sourceSlice(diagnostic.span).?);
    try std.testing.expectEqualStrings("secret", module.sourceSlice(diagnostic.related.?).?);
    try std.testing.expect(diagnostic.related.?.start < diagnostic.span.start);
    try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_name, module.diagnostics.items[1].tag);
}

test "printing accepts typed scalar arguments and returns void" {
    var module = try Module.init(std.testing.allocator, "print.nov", "let empty: () -> void = @print()\nlet line: () -> void = @println()\nlet out: (a: i8, b: u64, c: bool) -> void = @println(a, b, c, true, false, a + 1)");
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(@import("Sema.zig").Status.body_checked, entry.status);
}

test "printing rejects unresolved literals non-scalars unsupported builtins and nonvoid results" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: () -> void = @println(1)", .tag = .literal_needs_type },
        .{ .source = "let n: int = 1\nlet f: () -> void = @println(2)", .tag = .literal_needs_type },
        .{ .source = "let n: uint = 1\nlet f: () -> int = n", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> void = @println(-1)", .tag = .literal_needs_type },
        .{ .source = "let f: () -> void = @println(\"text\")", .tag = .unsupported_literal },
        .{ .source = "let f: () -> void = @println({})", .tag = .unsupported_body_expression },
        .{ .source = "let f: () -> void = @println(f)", .tag = .unsupported_body_expression },
        .{ .source = "let f: () -> void = @println(@print())", .tag = .unsupported_body_expression },
        .{ .source = "let f: () -> bool = @println(true)", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> void = @println(missing)", .tag = .unknown_name },
        .{ .source = "let f: () -> void = @max(true)", .tag = .unsupported_body_expression },
        .{ .source = "let f: () -> void = @print", .tag = .unsupported_body_expression },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "print.nov", case.source);
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
    }
}

test "function parameters literals arithmetic and forward calls type-check" {
    var module = try Module.init(std.testing.allocator, "functions.nov", "let caller: () -> i32 = add(1, 2)\nlet add: (a: i32, b: i32) -> i32 = {a + b}\nlet minimum: () -> i8 = -{128}\nlet empty: () = {}\nlet overflow_pending: () -> i8 = 127 + 1");
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| {
        try std.testing.expectEqual(@import("Sema.zig").Status.body_checked, entry.status);
        try std.testing.expect(entry.constant == null);
    }
    try sema.ir.verify();
}

test "mutual recursion and function-typed parameters use known signatures" {
    var module = try Module.init(std.testing.allocator, "recursive.nov", "let first: (x: i32) -> i32 = second(x)\nlet second: (x: i32) -> i32 = first(x)\nlet apply: (callback: (i32) -> i32, value: i32) -> i32 = callback(value)\nlet identity: (x: i32) -> i32 = x\nlet factory: () -> (i32) -> i32 = identity\nlet caller: () -> i32 = factory()(42)");
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(@import("Sema.zig").Status.body_checked, entry.status);
    try sema.ir.verify();
}

test "boolean expressions and comparisons infer operand literals from parameters" {
    var module = try Module.init(std.testing.allocator, "booleans.nov", "let less: (value: i32) -> bool = 0 < value\nlet both: (a: bool, b: bool) -> bool = a and !b\nlet equal: () -> bool = true == false\nlet bits: (value: u32) -> u32 = ~value\nlet negative: (value: i32) -> i32 = -value");
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(@import("Sema.zig").Status.body_checked, entry.status);
}

test "body errors identify mismatched returns operands names and calls" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: (value: bool) -> i32 = value", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> i32 = true", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> () -> i32 = true", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> bool = missing + 1 < 2", .tag = .unknown_name },
        .{ .source = "let f: () -> i8 = 128", .tag = .literal_out_of_range },
        .{ .source = "let f: () -> i32 = missing", .tag = .unknown_name },
        .{ .source = "let f: (a: i32, b: i64) -> i32 = a + b", .tag = .expression_type_mismatch },
        .{ .source = "let f: (a: bool) -> bool = a + true", .tag = .operator_not_supported },
        .{ .source = "let f: (value: i32) -> i32 = value()", .tag = .not_callable },
        .{ .source = "let target: (x: i32) -> i32 = x\nlet f: () -> i32 = target()", .tag = .argument_count_mismatch },
        .{ .source = "let target: (x: bool) -> bool = x\nlet f: () -> bool = target(1)", .tag = .literal_type_mismatch },
        .{ .source = "let target: (x: u8) -> u8 = x\nlet f: () -> u8 = target(256)", .tag = .literal_out_of_range },
        .{ .source = "let f: () -> bool = 1 < 2", .tag = .literal_needs_type },
        .{ .source = "let value = 1\nlet f: () -> i32 = value", .tag = .unresolved_value_type },
        .{ .source = "let f: () -> i32 = {}", .tag = .expression_type_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "invalid.nov", case.source);
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expectEqual(@import("Sema.zig").Status.failed, sema.entries[sema.entries.len - 1].status);
        try sema.ir.verify();
    }
}

test "control flow and parameter shadowing stay explicitly unsupported" {
    for ([_][]const u8{
        "let f: () -> i32 = label: {42}",
        "let f: (value: i32) = {value += 1}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "unsupported.nov", source);
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_body_expression, module.diagnostics.items[0].tag);
    }
    for ([_][]const u8{
        "let name: i32 = 1\nlet f: (name: i32) -> i32 = name",
        "let f: (true: bool) -> bool = true",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "shadow.nov", source);
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_shadowing, module.diagnostics.items[0].tag);
    }
}

test "unconstrained operands still diagnose unknown names and unsupported shapes" {
    var module = try Module.init(std.testing.allocator, "names.nov", "let f: () -> bool = missing == other");
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 2), module.diagnostics.items.len);
    for (module.diagnostics.items) |diagnostic| try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_name, diagnostic.tag);
    var unsupported = try Module.init(std.testing.allocator, "local.nov", "let f: () -> bool = {\nlet local = 1\nlocal\n} == 2");
    defer unsupported.deinit();
    var unsupported_sema = try @import("Sema.zig").analyzeFunctions(&unsupported);
    defer unsupported_sema.deinit();
    try std.testing.expectEqual(@as(usize, 1), unsupported.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_local_declaration, unsupported.diagnostics.items[0].tag);
}

test "parameter bindings do not leak between functions and annotated globals remain readable" {
    var module = try Module.init(std.testing.allocator, "scope.nov", "let global: i32 = 42\nlet read: () -> i32 = global\nlet first: (secret: i32) -> i32 = secret\nlet second: () -> i32 = secret");
    defer module.deinit();
    var sema = try @import("Sema.zig").analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_name, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(@import("Sema.zig").Status.constant_checked, sema.entries[0].status);
    try std.testing.expectEqual(@import("Sema.zig").Status.body_checked, sema.entries[1].status);
    try std.testing.expectEqual(@import("Sema.zig").Status.body_checked, sema.entries[2].status);
    try std.testing.expectEqual(@import("Sema.zig").Status.failed, sema.entries[3].status);
}

test "recursive depth is bounded while literal grouping uses iteration" {
    for ([_][]const u8{ "value", "1" }, 0..) |leaf, i| {
        var source = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer source.deinit();
        try source.writer.writeAll("let f: (value: i32) -> i32 = ");
        try source.writer.splatByteAll('{', 132);
        try source.writer.writeAll(leaf);
        try source.writer.splatByteAll('}', 132);
        var module = try Module.init(std.testing.allocator, "deep.nov", source.written());
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        if (i == 0) {
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(Module.Diagnostic.Tag.body_nesting_limit, module.diagnostics.items[0].tag);
        } else {
            try std.testing.expect(!module.hasErrors());
            try std.testing.expectEqual(@import("Sema.zig").Status.body_checked, sema.entries[0].status);
        }
    }
}

fn testAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom.nov", source);
    defer module.deinit();
    var sema = @import("Sema.zig").analyzeFunctions(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try sema.ir.verify();
}

test "body checking rolls back diagnostics and owned storage on allocation failure" {
    for ([_][]const u8{
        "let add: (a: i32, b: i32) -> i32 = a + b\nlet f: () -> i32 = add(1, 2)",
        "let bad: (x: i32) -> bool = x\nlet good: (p: i32, q: i32) -> i32 = p + q",
        "let f: (x: i32) -> i32 = missing(x)",
        "let f: (true: bool) -> bool = true",
        "let f: () -> i32 = {let a: i32 = 1\nlet b: i32 = {let c: i32 = a + 2\nc}\nb}",
        "let f: () -> i32 = {let x: i32 = 1\n{let x: i32 = 2\nx}}",
        "let f: () -> i32 = {let x: (missing) -> i32 = f\n1}",
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}
