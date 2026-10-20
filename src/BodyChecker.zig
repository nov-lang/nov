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
    mutable: bool = false,
    slot: ?Ir.ValueId = null,
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

    fn append(self: *Emitter, ty: Ir.TypeId, span: Ir.Span, op: Ir.Operation) Error!Ir.ValueId {
        if (self.instructions.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
        const id: Ir.ValueId = @enumFromInt(self.instructions.items.len);
        try self.instructions.append(self.allocator, .{ .ty = ty, .span = span, .op = op });
        return id;
    }

    fn record(self: *Emitter, node: Ast.Node.Index, ty: Ir.TypeId, span: Ir.Span, op: Ir.Operation) Error!void {
        if (self.values.contains(node)) return;
        const id = try self.append(ty, span, op);
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
    const checked = (try context.expression(full.ast.init_node, .{ .ty = ty.function.result, .annotation = annotation }, 0)) != null;
    if (checked and !context.terminated) try context.copyNode(full.ast.init_node, ty.function.result);
    return checked;
}

const Context = struct {
    module: *Module,
    ir: *Ir,
    types: []const ?Ir.TypeId,
    emitter: ?*Emitter = null,
    return_type: Known = undefined,
    // Termination is independent of the contextual value type. A terminal
    // expression has no merge value, and enclosing expressions alias its exit.
    terminated: bool = false,
    terminal: ?Ir.ValueId = null,
    loop_depth: usize = 0,
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
        if (std.mem.eql(u8, name, "_")) {
            _ = try self.fail(.unsupported_local_declaration, node, null);
            return false;
        }
        // Named parameters introduce a local function definition, not a value
        // annotation. Closures/local function bodies are outside this slice.
        if (if (decl.type_node != 0) ast.fullFnProto(decl.type_node) else null) |proto| {
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
        const annotation: ?Known = if (decl.type_node != 0) .{
            .ty = (try TypeResolver.resolve(self.module, self.ir, decl.type_node)) orelse return false,
            .annotation = self.module.tokenSpan(ast.firstToken(decl.type_node)),
        } else null;
        if (decl.is_mutable and annotation != null and self.ir.getType(annotation.?.ty).? == .void) {
            _ = try self.fail(.unsupported_local_declaration, node, null);
            return false;
        }
        // Publish only after checking/evaluating the initializer: no self or
        // forward local references, and initialization happens exactly once.
        const ty = (try self.expression(decl.init_node, annotation, depth + 1)) orelse return false;
        if (self.terminated) return true;
        if (annotation == null and self.ir.getType(ty).? == .void) {
            _ = try self.fail(.unsupported_local_declaration, node, null);
            return false;
        }
        const known = annotation orelse Known{ .ty = ty };
        try self.copyNode(decl.init_node, ty);
        const slot = if (decl.is_mutable and self.emitter != null)
            try self.emitter.?.append(ty, self.irSpan(node), .{ .local_slot = try self.emitter.?.value(decl.init_node) })
        else
            null;
        try self.bindLocal(name, .{ .known = known, .initializer = decl.init_node, .name_span = name_span, .mutable = decl.is_mutable, .slot = slot });
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
                const token = decl.let_token + 1 + @as(u32, @intFromBool(decl.is_mutable));
                const name = ast.tokenSlice(token);
                if (self.conflict(name) != null or isPrimitive(name)) return null;
                const known: Known = if (decl.type_node != 0)
                    .{ .ty = (try TypeResolver.resolve(self.module, self.ir, decl.type_node)) orelse return null }
                else if (decl.init_node != 0)
                    (try self.probe(decl.init_node, depth + 1)) orelse return null
                else
                    return null;
                try self.bindLocal(name, .{ .known = known, .initializer = decl.init_node, .name_span = self.module.tokenSpan(token) });
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
            .string_literal => .{ .ty = try self.ir.internType(.string) },
            .array_init, .array_init_comma, .array_init_two, .array_init_two_comma => try self.probeArray(node, depth),
            .array_access => blk: {
                const array = (try self.probe(data.lhs, depth + 1)) orelse break :blk null;
                const ty = self.ir.getType(array.ty).?;
                break :blk if (ty == .array) .{ .ty = ty.array } else null;
            },
            .field_access => if (std.mem.eql(u8, ast.tokenSlice(data.rhs), "len")) .{ .ty = try self.ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } }) } else null,
            .block, .block_two, .block_newline, .block_two_newline => try self.probeBlock(node, depth),
            .loop_condition, .@"if", .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => .{ .ty = try self.ir.internType(.void) },
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

    fn copyNode(self: *Context, node: Ast.Node.Index, ty: Ir.TypeId) Error!void {
        if (self.ir.getType(ty).? != .array) return;
        if (self.emitter) |emitter| {
            const copy = try emitter.append(ty, self.irSpan(node), .{ .array_clone = try emitter.value(node) });
            try emitter.values.put(emitter.allocator, node, copy);
        }
    }

    fn arrayElements(self: *Context, node: Ast.Node.Index, buffer: *[2]Ast.Node.Index) []const Ast.Node.Index {
        const ast = &self.module.ast;
        const data = ast.nodes.items(.data)[node];
        return switch (ast.nodes.items(.tag)[node]) {
            .array_init, .array_init_comma => ast.extra_data[data.lhs..data.rhs],
            else => blk: {
                buffer.* = .{ data.lhs, data.rhs };
                break :blk buffer[0 .. @as(usize, @intFromBool(data.lhs != 0)) + @as(usize, @intFromBool(data.rhs != 0))];
            },
        };
    }

    fn probeArray(self: *Context, node: Ast.Node.Index, depth: usize) Error!?Known {
        var buffer: [2]Ast.Node.Index = undefined;
        // A later known scalar can constrain earlier literals. Never check or
        // emit elements here: the real walk validates conflicts in source order.
        for (self.arrayElements(node, &buffer)) |element| {
            const known = (try self.probe(element, depth + 1)) orelse continue;
            if (Ir.isArrayElement(self.ir.getType(known.ty).?)) {
                return .{ .ty = try self.ir.internType(.{ .array = known.ty }) };
            }
        }
        return null;
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
            .array_init, .array_init_comma, .array_init_two, .array_init_two_comma => blk: {
                var buffer: [2]Ast.Node.Index = undefined;
                const elements = self.arrayElements(node, &buffer);
                const values = try emitter.allocator.alloc(Ir.ValueId, elements.len);
                for (elements, values) |element, *value| value.* = try emitter.value(element);
                break :blk .{ .array_literal = values };
            },
            .array_access => .{ .array_index = .{ .array = try emitter.value(data.lhs), .index = try emitter.value(data.rhs) } },
            .field_access => .{ .array_len = try emitter.value(data.lhs) },
            .identifier => blk: {
                const name = self.module.sourceSlice(self.span(node)).?;
                if (self.bindings.get(name)) |binding| {
                    if (binding.slot) |slot| break :blk .{ .local_load = slot };
                    if (binding.initializer) |initializer| {
                        try emitter.values.put(emitter.allocator, node, try emitter.value(initializer));
                        if (self.ir.getType(ty).? == .array) try self.copyNode(node, ty);
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
        if (tag == .identifier and self.ir.getType(ty).? == .array) try self.copyNode(node, ty);
        return true;
    }

    fn elementAssignment(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.ast;
        const data = ast.nodes.items(.data)[node];
        const target = ast.nodes.items(.data)[data.lhs];
        const tag = ast.nodes.items(.tag)[node];
        const compound = tag != .assign;
        if (ast.nodes.items(.tag)[target.lhs] != .identifier) return self.fail(.unsupported_assignment_target, data.lhs, null);
        const name = self.module.sourceSlice(self.span(target.lhs)).?;
        const binding = self.bindings.get(name) orelse {
            if (self.module.lookup(name)) |id| return self.fail(.immutable_assignment, target.lhs, self.module.getDeclaration(id).?.name_span);
            return self.fail(.unknown_name, target.lhs, null);
        };
        const array = self.ir.getType(binding.known.ty).?;
        if (array != .array) return self.fail(.unsupported_assignment_target, data.lhs, null);
        if (!binding.mutable) return self.fail(.immutable_assignment, target.lhs, binding.name_span);
        if (compound and self.ir.getType(array.array).? != .integer) return self.fail(.operator_not_supported, node, binding.known.annotation);
        if (try self.expression(target.lhs, binding.known, depth + 1) == null) return null;
        const index = try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } });
        if (try self.expression(target.rhs, .{ .ty = index }, depth + 1) == null) return null;
        // Use the same pre-index snapshot for both the old element and final
        // replacement, even if index/RHS effects replace the local array.
        var old: ?Ir.ValueId = null;
        if (compound and !self.terminated) {
            if (self.emitter) |emitter| old = try emitter.append(array.array, self.irSpan(data.lhs), .{ .array_index = .{
                .array = try emitter.value(target.lhs),
                .index = try emitter.value(target.rhs),
            } });
        }
        if (try self.expression(data.rhs, .{ .ty = array.array }, depth + 1) == null) return null;
        const unit = try self.ir.internType(.void);
        if (!self.terminated) {
            if (self.emitter) |emitter| {
                var value = try emitter.value(data.rhs);
                if (compound) value = try emitter.append(array.array, self.irSpan(node), .{ .binary = .{
                    .op = switch (tag) {
                        .assign_add => .add,
                        .assign_sub => .sub,
                        .assign_mul => .mul,
                        .assign_div => .div,
                        .assign_mod => .mod,
                        else => unreachable,
                    },
                    .lhs = old.?,
                    .rhs = value,
                } });
                const updated = try emitter.append(binding.known.ty, self.irSpan(node), .{ .array_set = .{ .array = try emitter.value(target.lhs), .index = try emitter.value(target.rhs), .value = value } });
                try emitter.record(node, unit, self.irSpan(node), .{ .local_store = .{ .slot = binding.slot.?, .value = updated } });
            }
        }
        return self.fit(unit, expected, node);
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
            .array_init, .array_init_comma, .array_init_two, .array_init_two_comma => {
                const want = hint orelse return self.fail(.array_literal_needs_type, node, null);
                const array = self.ir.getType(want.ty).?;
                if (array != .array) return self.fail(.expression_type_mismatch, node, want.annotation);
                var buffer: [2]Ast.Node.Index = undefined;
                for (self.arrayElements(node, &buffer)) |element| {
                    if (try self.expression(element, .{ .ty = array.array }, depth + 1) == null) return null;
                }
                return want.ty;
            },
            .array_access, .field_access => {
                const base = (try self.expression(data.lhs, null, depth + 1)) orelse return null;
                if (self.terminated) return if (expected) |want| want.ty else base;
                const array = self.ir.getType(base).?;
                if (array != .array and !(array == .string and ast.nodes.items(.tag)[node] == .field_access)) return self.fail(.unsupported_body_expression, node, null);
                if (ast.nodes.items(.tag)[node] == .field_access) {
                    if (!std.mem.eql(u8, ast.tokenSlice(data.rhs), "len")) return self.fail(.unsupported_body_expression, node, null);
                    return self.fit(try self.ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } }), expected, node);
                }
                const index = try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } });
                if (try self.expression(data.rhs, .{ .ty = index }, depth + 1) == null) return null;
                return self.fit(array.array, expected, node);
            },
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
            .@"break", .@"continue" => {
                const is_break = ast.nodes.items(.tag)[node] == .@"break";
                if (data.lhs != 0 or (is_break and data.rhs != 0)) return self.fail(.unsupported_loop_control, node, null);
                if (self.loop_depth == 0) return self.fail(.loop_control_outside_loop, node, null);
                const unit = try self.ir.internType(.void);
                if (self.emitter) |emitter| {
                    try emitter.record(node, unit, self.irSpan(node), if (is_break) .loop_break else .loop_continue);
                    self.terminal = try emitter.value(node);
                }
                self.terminated = true;
                return if (expected) |want| want.ty else unit;
            },
            .loop_condition => {
                const unit = try self.ir.internType(.void);
                const boolean = try self.ir.internType(.boolean);
                self.loop_depth += 1;
                defer self.loop_depth -= 1;
                const condition_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
                const condition = try self.expression(data.lhs, .{ .ty = boolean }, depth + 1);
                self.terminated = false;
                const body_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
                const body = try self.expression(data.rhs, .{ .ty = unit }, depth + 1);
                self.terminated = false;
                if (condition == null or body == null) return null;
                if (self.emitter) |emitter| try emitter.record(node, unit, self.irSpan(node), .{ .loop = .{
                    .condition_start = condition_start,
                    .body_start = body_start,
                    .condition = try emitter.value(data.lhs),
                    .body = try emitter.value(data.rhs),
                } });
                return self.fit(unit, expected, node);
            },
            .@"return" => {
                if (data.rhs != 0) {
                    if (try self.expression(data.rhs, self.return_type, depth + 1) == null) return null;
                    if (self.terminated) return self.return_type.ty;
                    try self.copyNode(data.rhs, self.return_type.ty);
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
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => {
                if (ast.nodes.items(.tag)[data.lhs] == .field_access) {
                    const target = ast.nodes.items(.data)[data.lhs];
                    if (std.mem.eql(u8, ast.tokenSlice(target.rhs), "len")) {
                        if (try self.probe(target.lhs, depth + 1)) |base| {
                            if (self.ir.getType(base.ty).? == .array) return self.fail(.unsupported_array_length_write, data.lhs, null);
                        }
                    }
                }
                if (ast.nodes.items(.tag)[data.lhs] == .array_access) return self.elementAssignment(node, expected, depth);
                if (ast.nodes.items(.tag)[data.lhs] != .identifier) return self.fail(.unsupported_assignment_target, data.lhs, null);
                const name = self.module.sourceSlice(self.span(data.lhs)).?;
                const binding = self.bindings.get(name) orelse {
                    if (self.module.lookup(name)) |id| return self.fail(.immutable_assignment, data.lhs, self.module.getDeclaration(id).?.name_span);
                    return self.fail(.unknown_name, data.lhs, null);
                };
                if (!binding.mutable) return self.fail(.immutable_assignment, data.lhs, binding.name_span);
                const tag = ast.nodes.items(.tag)[node];
                const compound = tag != .assign;
                const array_append = tag == .assign_add and self.ir.getType(binding.known.ty).? == .array;
                if (compound and !array_append and self.ir.getType(binding.known.ty).? != .integer) return self.fail(.operator_not_supported, node, binding.known.annotation);
                // Capture the old value before RHS effects, just as for binary
                // arithmetic. A returning RHS never performs the final store.
                if (compound and try self.expression(data.lhs, binding.known, depth + 1) == null) return null;
                if (try self.expression(data.rhs, binding.known, depth + 1) == null) return null;
                const unit = try self.ir.internType(.void);
                if (!self.terminated) {
                    if (self.emitter) |emitter| {
                        var value = try emitter.value(data.rhs);
                        if (array_append) {
                            value = try emitter.append(binding.known.ty, self.irSpan(node), .{ .array_append = .{ .lhs = try emitter.value(data.lhs), .rhs = value } });
                        } else if (compound) value = try emitter.append(binding.known.ty, self.irSpan(node), .{ .binary = .{
                            .op = switch (tag) {
                                .assign_add => .add,
                                .assign_sub => .sub,
                                .assign_mul => .mul,
                                .assign_div => .div,
                                .assign_mod => .mod,
                                else => unreachable,
                            },
                            .lhs = try emitter.value(data.lhs),
                            .rhs = value,
                        } });
                        if (self.ir.getType(binding.known.ty).? == .array) value = try emitter.append(binding.known.ty, self.irSpan(node), .{ .array_clone = value });
                        try emitter.record(node, unit, self.irSpan(node), .{ .local_store = .{ .slot = binding.slot.?, .value = value } });
                    }
                }
                return self.fit(unit, expected, node);
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
                if (self.ir.getType(operand.ty).? != .integer and !(self.ir.getType(operand.ty).? == .string and ast.nodes.items(.tag)[node] == .add)) return self.fail(.operator_not_supported, node, operand.annotation);
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
                        if (!self.terminated and arg_ty != .boolean and arg_ty != .integer and arg_ty != .string) {
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
                    if (try self.expression(arg, .{ .ty = param, .annotation = annotation }, depth + 1) == null) {
                        valid = false;
                    } else if (!self.terminated) try self.copyNode(arg, param);
                }
                if (!valid) return null;
                return self.fit(ty.function.result, expected, node);
            },
            else => return self.fail(.unsupported_body_expression, node, null),
        }
    }
};

test "string inference accepts late literals calls lengths and branch context" {
    var module = try Module.init(std.testing.allocator, "strings.nov",
        \\let id: (s: string) -> string = s
        \\let f: (s: string, b: bool) -> string = {
        \\    let mut x = {let y = "a"
        \\        y + s}
        \\    x = if b {id(x)} else {"b"}
        \\    let length: uint = x.len
        \\    @println(length, "", x)
        \\    if b {"prefix"} else {s} + x
        \\}
    );
    defer module.deinit();
    var sema = try @import("Lowering.zig").lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
}

test "strings reject byte mutation indexing slicing arrays comparisons and eager globals" {
    for ([_][]const u8{
        "let f: () -> void = {let x: []string = []\n}",
        "let f: () -> void = {let x = [\"a\"]\n}",
        "let f: () -> void = {let mut x = \"a\"\nx[0] = \"b\"}",
        "let f: () -> void = @println(\"a\"[0])",
        "let f: () -> void = @println(\"a\"[0..1])",
        "let f: () -> void = {let mut x = \"a\"\nx.len = 1}",
        "let f: () -> void = {let mut x = \"a\"\nx += \"b\"}",
        "let f: () -> bool = \"a\" == \"b\"",
        "let f: () -> string = \"a\" - \"b\"",
        "let f: () -> string = 1 + \"b\"",
        "let f: () -> string = \"a\" + 1",
        "let x: string = \"a\" + \"b\"",
        "let mut x: string = \"a\"",
        "let x = \"a\"",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "unsupported-string.nov", source);
        defer module.deinit();
        if (module.hasErrors()) continue;
        var sema = try @import("Lowering.zig").lower(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "scalar array deferred and ill typed forms fail closed" {
    for ([_][]const u8{
        "let main: () -> void = @println([])",
        "let main: () -> void = {let a: []int = []\n@println(a)}",
        "let main: () -> void = {let a: []int = [1]\na[0] = 2}",
        "let main: () -> void = {let mut a: []int = [1]\na.len = 2}",
        "let main: () -> void = {let mut a: []int = [1]\na.len += 2}",
        "let main: () -> void = {let a: []int = [1]\nlet index: uint = 0\n@println(a[index])}",
        "let main: () -> void = {let a: []int = [true]\n}",
        "let main: () -> void = {let a: [][]int = [[1]]\n}",
        "let main: () -> void = {let a: []i128 = []\n}",
        "let main: () -> void = {let a: []void = []\n}",
        "let main: () -> void = {let a: []int = [1]\n@println(a.cap)}",
        "let main: () -> void = {let a: []int = [1]\n@println(a == a)}",
        "let main: () -> void = {let a: []int = [1]\n@println(a[0..1])}",
        "let main: () -> void = {let mut a: []int = [1]\na += 2}",
        "let main: () -> void = {let mut a: []int = [1]\na -= [2]}",
        "let main: () -> void = {let mut a: []bool = [true]\na[0] += false}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "arrays.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try @import("Lowering.zig").lower(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
    }
}

test "all indexed compound operators reject bool elements immutable parameters and nonnative indices" {
    for ([_][]const u8{ "+=", "-=", "*=", "/=", "%=" }) |op| {
        inline for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
            .{ .source = "let f: () -> void = {{let mut a: []bool = [true]\na[0] {s} false}}", .tag = .operator_not_supported },
            .{ .source = "let f: (a: []int) -> void = {{a[0] {s} 1}}", .tag = .immutable_assignment },
            .{ .source = "let f: () -> void = {{let mut a: []int = [1]\nlet i: i8 = 0\na[i] {s} 1}}", .tag = .expression_type_mismatch },
        }) |case| {
            const source = try std.fmt.allocPrint(std.testing.allocator, case.source, .{op});
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "compound.nov", source);
            defer module.deinit();
            try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
            var sema = try @import("Lowering.zig").lower(&module);
            defer sema.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        }
    }
}

test "condition loops diagnose controls types scope and unreachable tails" {
    for ([_]struct { body: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .body = "{break}", .tag = .loop_control_outside_loop },
        .{ .body = "{continue}", .tag = .loop_control_outside_loop },
        .{ .body = "loop true {break 1}", .tag = .unsupported_loop_control },
        .{ .body = "loop true {break :outer}", .tag = .unsupported_loop_control },
        .{ .body = "loop true {continue :outer}", .tag = .unsupported_loop_control },
        .{ .body = "loop 1 {}", .tag = .literal_type_mismatch },
        .{ .body = "loop true {true}", .tag = .literal_type_mismatch },
        .{ .body = "{loop false {missing}}", .tag = .unknown_name },
        .{ .body = "{loop false {let hidden: int = 1\n}\n@println(hidden)}", .tag = .unknown_name },
        .{ .body = "{loop {let hidden: bool = true\nhidden} {@println(hidden)}}", .tag = .unknown_name },
        .{ .body = "loop true {{break}\n@println(true)}", .tag = .unreachable_code },
        .{ .body = "loop true {continue\n@println(true)}", .tag = .unreachable_code },
        .{ .body = "loop true {if true {break} else {continue}\n@println(true)}", .tag = .unreachable_code },
        .{ .body = "{let n: int = loop false {}\n}", .tag = .expression_type_mismatch },
        .{ .body = "{loop false {}\nbreak}", .tag = .loop_control_outside_loop },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> void = {s}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "loop.nov", source);
        defer module.deinit();
        errdefer std.debug.print("loop case: {s}\n", .{case.body});
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try @import("Lowering.zig").lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "assignment diagnostics identify targets mutability types and lexical scope" {
    const Case = struct { body: []const u8, tag: Module.Diagnostic.Tag, token: []const u8 };
    for ([_]Case{
        .{ .body = "{let n: int = 1\nn = 2}", .tag = .immutable_assignment, .token = "n" },
        .{ .body = "{p += 1}", .tag = .immutable_assignment, .token = "p" },
        .{ .body = "{g = 2}", .tag = .immutable_assignment, .token = "g" },
        .{ .body = "{missing = 1}", .tag = .unknown_name, .token = "missing" },
        .{ .body = "{missing[0] += 1}", .tag = .unknown_name, .token = "missing" },
        .{ .body = "{g[0] += 1}", .tag = .immutable_assignment, .token = "g" },
        .{ .body = "{let a: []int = [1]\na[0] += 2}", .tag = .immutable_assignment, .token = "a" },
        .{ .body = "{let mut a: []bool = [true]\na[0] *= false}", .tag = .operator_not_supported, .token = "*=" },
        .{ .body = "{let mut a: []int = [1]\na[0] -= false}", .tag = .literal_type_mismatch, .token = "false" },
        .{ .body = "{let mut a: []int = [1]\nlet i: uint = 0\na[i] /= 2}", .tag = .expression_type_mismatch, .token = "i" },
        .{ .body = "{let mut a: []int = [1]\nf(p)[0] %= 2}", .tag = .unsupported_assignment_target, .token = "[" },
        .{ .body = "{let mut a: []int = [1]\na[0][0] += 2}", .tag = .unsupported_assignment_target, .token = "[" },
        .{ .body = "{let mut n: int = 1\nn = true}", .tag = .literal_type_mismatch, .token = "true" },
        .{ .body = "{let mut n: int = 1\nn = p}", .tag = .expression_type_mismatch, .token = "p" },
        .{ .body = "{let mut b: bool = true\nb += false}", .tag = .operator_not_supported, .token = "+=" },
        .{ .body = "{let mut n: int = 1\nn + n = 2}", .tag = .unsupported_assignment_target, .token = "+" },
        .{ .body = "{let mut n: int = 1\nn.x = 2}", .tag = .unsupported_assignment_target, .token = "." },
        .{ .body = "{let mut n: int = 1\nn[0] = 2}", .tag = .unsupported_assignment_target, .token = "[" },
        .{ .body = "{let mut n: int = n\n}", .tag = .unknown_name, .token = "n" },
        .{ .body = "{if true {let mut n: int = 1\n}\nn = 2}", .tag = .unknown_name, .token = "n" },
        .{ .body = "{let mut n: int = 1\n{let mut n: int = 2\n}}", .tag = .unsupported_shadowing, .token = "n" },
        .{ .body = "{let mut n: int = 1\nlet x: int = {n = 2}\n}", .tag = .expression_type_mismatch, .token = "=" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let g: int = 0\nlet f: (p: bool) -> void = {s}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "assign.nov", source);
        defer module.deinit();
        errdefer std.debug.print("assignment case: {s}\n", .{case.body});
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try @import("Lowering.zig").lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(module.diagnostics.items[0].span).?);
        try std.testing.expect(!sema.lowering_complete);
    }
}

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

test "inferred locals reuse known expression types and nested probe scopes" {
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let global: {s} = 7\nlet id: (p: {s}) -> {s} = {{\n" ++
            "let yes = true\nlet mut no = !yes\nno = false\n" ++
            "let callback = id\nlet mut other = callback\nother = id\n" ++
            "let from_global = global\nlet from_param = p\nlet called = other(from_param)\n" ++
            "let sum = 1 + called\nlet mut result = if yes {{1}} else {{sum}}\nresult += 1\n" ++
            "let array: []{s} = [p]\nlet mut copy = array\ncopy[0] = result\n" ++
            "let size = copy.len\nlet element = copy[0]\n@println(size, element, no, from_global)\n" ++
            "let nested = 1 + {{let first = p\nlet second = {{let inner = first\ninner}}\nsecond}}\n" ++
            "@println({{let reused = p\nreused}} == {{let reused = p\nreused}})\n" ++
            "nested\n}}", .{ name, name, name, name });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "inferred.nov", source);
        defer module.deinit();
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
    }
}

test "scalar array literals infer known elements bidirectionally across expression scopes" {
    for ([_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint" }) |name| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let global: {s} = 7\nlet id: (x: {s}) -> {s} = x\nlet f: (p: {s}) -> void = {{\n" ++
            "let flags = [true, !false, p == 0]\nlet a = [-0, 1 + 2, global]\n" ++
            "let b = [0, id(p)]\nlet mut c = [0, p]\nc[0] += 1\nlet copy = c\n" ++
            "let indexed = c[-1]\nlet length = flags.len\n" ++
            "let nested = 1 + {{let a_local = [0, p]\nlet inner = {{let b_local = [0, a_local[0]]\nb_local[0]}}\ninner}}\n" ++
            "let late = [0, {{let reused = [0, p]\nreused[0]}}]\n" ++
            "let branch = if true {{[1, p]}} else {{[2, 3]}}\n" ++
            "@println(flags[0], a[0], b[0], copy[0], indexed, length, nested, late[0], branch[0])\n}}", .{ name, name, name, name });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "array-inference.nov", source);
        defer module.deinit();
        var sema = try @import("Lowering.zig").lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        try std.testing.expect(!module.hasErrors());
        try sema.ir.verify();
    }
}

test "array inference uses exact literal boundaries and expected context" {
    const Case = struct { ty: []const u8, literal: []const u8 };
    for ([_]Case{
        .{ .ty = "i8", .literal = "-{128}" },
        .{ .ty = "i8", .literal = "127" },
        .{ .ty = "u8", .literal = "255" },
        .{ .ty = "u8", .literal = "-{0}" },
        .{ .ty = "i64", .literal = "-9223372036854775808" },
        .{ .ty = "u64", .literal = "18446744073709551615" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: (p: {s}) -> []{s} = {{let a = [{s}, p]\na}}\n" ++
            "let take: (a: []{s}) -> void = {{}}\n" ++
            "let out: (p: {s}) -> void = {{let empty: []{s} = []\n" ++
            "let explicit: []{s} = [{s}]\ntake([{s}, p])\n" ++
            "let branch = if false {{[]}} else {{[p]}}\n@println(empty.len, explicit[0], branch[0])\n}}", .{ case.ty, case.ty, case.literal, case.ty, case.ty, case.ty, case.ty, case.literal, case.literal });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "array-boundaries.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try @import("Lowering.zig").lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try sema.ir.verify();
    }
}

test "array inference rejects conflicts defaults and scope leaks without probe diagnostics" {
    const Case = struct { body: []const u8, tag: Module.Diagnostic.Tag, token: []const u8 };
    for ([_]Case{
        .{ .body = "let a = [p, wide]", .tag = .expression_type_mismatch, .token = "wide" },
        .{ .body = "let a = [wide, p]", .tag = .expression_type_mismatch, .token = "p" },
        .{ .body = "let a = [p, unsigned]", .tag = .expression_type_mismatch, .token = "unsigned" },
        .{ .body = "let a = [p, true]", .tag = .literal_type_mismatch, .token = "true" },
        .{ .body = "let a = [true, p]", .tag = .expression_type_mismatch, .token = "p" },
        .{ .body = "let a = [1, true]", .tag = .literal_type_mismatch, .token = "1" },
        .{ .body = "let a = [128, p]", .tag = .literal_out_of_range, .token = "128" },
        .{ .body = "let a = [-129, p]", .tag = .literal_out_of_range, .token = "-129" },
        .{ .body = "let a = [-1, unsigned]", .tag = .literal_out_of_range, .token = "-1" },
        .{ .body = "let a: []i8 = [128, wide]", .tag = .literal_out_of_range, .token = "128" },
        .{ .body = "let a: []i8 = [0, wide]", .tag = .expression_type_mismatch, .token = "wide" },
        .{ .body = "let a = []", .tag = .array_literal_needs_type, .token = "[" },
        .{ .body = "let a = [1, 2]", .tag = .array_literal_needs_type, .token = "[" },
        .{ .body = "let a = [[true]]", .tag = .array_literal_needs_type, .token = "[" },
        .{ .body = "let a = [f]", .tag = .array_literal_needs_type, .token = "[" },
        .{ .body = "let a = [missing, p]", .tag = .unknown_name, .token = "missing" },
        .{ .body = "let a = [0, {let hidden = p\nhidden}]\n@println(hidden)", .tag = .unknown_name, .token = "hidden" },
        .{ .body = "let a = [0, {let x: missing = p\nx}, p]", .tag = .unknown_type, .token = "missing" },
        .{ .body = "let a = [0, {let x = a\nx}, p]", .tag = .unknown_name, .token = "a" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: (p: i8, wide: i16, unsigned: u8) -> void = {{{s}\n}}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "array-inference-errors.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(module.diagnostics.items[0].span).?);
    }
}

test "inferred local failures preserve typing and initializer scope" {
    const Case = struct { body: []const u8, tag: Module.Diagnostic.Tag, token: []const u8 };
    for ([_]Case{
        .{ .body = "let n = 1", .tag = .literal_needs_type, .token = "1" },
        .{ .body = "let mut n = 1 + 2", .tag = .literal_needs_type, .token = "1" },
        .{ .body = "let n = []", .tag = .array_literal_needs_type, .token = "[" },
        .{ .body = "let n = [1, 2]", .tag = .array_literal_needs_type, .token = "[" },
        .{ .body = "let n = n", .tag = .unknown_name, .token = "n" },
        .{ .body = "let n = later\nlet later = p", .tag = .unknown_name, .token = "later" },
        .{ .body = "let n = missing + p", .tag = .unknown_name, .token = "missing" },
        .{ .body = "let n = p\n{let n = true\n}", .tag = .unsupported_shadowing, .token = "n" },
        .{ .body = "let p = true", .tag = .unsupported_shadowing, .token = "p" },
        .{ .body = "let int = true", .tag = .unsupported_shadowing, .token = "int" },
        .{ .body = "{let hidden = p\n}\nlet n = hidden", .tag = .unknown_name, .token = "hidden" },
        .{ .body = "let n = p\nn = 2", .tag = .immutable_assignment, .token = "n" },
        .{ .body = "let mut n = p\nlet wide: i64 = 1\nn = wide", .tag = .expression_type_mismatch, .token = "wide" },
        .{ .body = "let n = 128 + p", .tag = .literal_out_of_range, .token = "128" },
        .{ .body = "let n = {}", .tag = .unsupported_local_declaration, .token = "let" },
        .{ .body = "let mut n = {}", .tag = .unsupported_local_declaration, .token = "let" },
        .{ .body = "let _ = true", .tag = .unsupported_local_declaration, .token = "let" },
        .{ .body = "@println(1 + {let n = missing\nn})", .tag = .unknown_name, .token = "missing" },
        .{ .body = "@println(1 + {let n = p\nn})\nlet out = n", .tag = .unknown_name, .token = "n" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: (p: i8) -> void = {{{s}\n}}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "inferred-errors.nov", source);
        defer module.deinit();

        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var sema = try @import("Sema.zig").analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(module.diagnostics.items[0].span).?);
    }
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
        .{ .body = "{let mut n: void = {}\nn}", .tag = .unsupported_local_declaration },
        .{ .body = "{let n = 1\nn}", .tag = .literal_needs_type },
        .{ .body = "{let n: i32\nn}", .tag = .missing_initializer },
        .{ .body = "{let n: i8 = 128\nn}", .tag = .literal_out_of_range },
        .{ .body = "{let n: missing = 1\nn}", .tag = .unknown_type },
        .{ .body = "{let f: i32 = 1\nf}", .tag = .unsupported_shadowing },
        .{ .body = "{let n: i32 = true\nn}", .tag = .literal_type_mismatch },
        .{ .body = "{let n: int = 1\nlet u: uint = n\np}", .tag = .expression_type_mismatch },
        .{ .body = "{let n: i32 = 1\nn = 2\nn}", .tag = .immutable_assignment },
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
        .{ .source = "let f: () -> void = @println('a')", .tag = .unsupported_literal },
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
    try std.testing.expectEqual(Module.Diagnostic.Tag.literal_needs_type, unsupported.diagnostics.items[0].tag);
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
        "let f: (p: i32) -> i32 = {let n = 1 + {let first = p\nlet second = {let inner = first\ninner}\nsecond}\nn}",
        "let f: () -> bool = {let n = missing\nn}",
        "let f: (p: i8) -> void = {let a = [0, {let inner = [0, p]\ninner[0]}]\n@println(a[0])}",
        "let f: (p: i8) -> void = {let a = [0, {let inner: missing = p\ninner}, p]\n}",
        "let f: (p: i8) -> void = {let a = [128, p]\n}",
        "let f: () -> void = {let n = []\n}",
        "let f: () -> void = {let n = {}\n}",
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}
