const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const string_literal = @import("string_literal.zig");
const Lowering = @import("Lowering.zig");
const Sema = @import("Sema.zig");
const Ast = @import("Ast.zig");
const Compilation = @import("Compilation.zig");
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
    value: ?Ir.ValueId = null,
    name_span: Module.Span,
    borrowed: bool = false,
    borrow: ?Ir.ValueId = null,
    loan_owner: ?struct { owner: Module.Span, origin: Module.Span } = null,
};
// Bounds recursive checking/probing, not the iterative literal-grouping walk.
const max_recursive_depth = 128;

/// Checks one function against the complete annotation table. Does not evaluate
/// operations, choose overflow behavior, or claim other declarations are checked.
/// Requires the complete, valid annotation table produced by Sema/TypeResolver.
/// The caller owns rollback of diagnostics and IR allocations on failure.
pub fn check(module: *Module, ir: *Ir, types: []const ?Ir.TypeId, id: Module.DeclId) Error!bool {
    return checkInner(module, ir, types, id, null, null);
}

/// Checks against a resizable semantic table, requesting concrete signatures
/// as references are encountered. Requested bodies are handled by Sema's queue.
pub fn checkDemand(module: *Module, sema: *Sema, id: Module.DeclId) Error!bool {
    return checkInner(module, &sema.ir, &.{}, id, null, sema);
}

/// Reuses the checked walk to emit values in callee/argument and operand order.
/// Source length and all requested function identities must already exist in IR.
pub fn lower(module: *Module, ir: *Ir, types: []const ?Ir.TypeId, constants: []const ?Ir.ConstId, functions: []const ?Ir.FunctionId, id: Module.DeclId) Error!bool {
    if (constants.len != types.len or functions.len != types.len) return error.InvalidFunctionId;
    var arena = std.heap.ArenaAllocator.init(module.allocator);
    defer arena.deinit();
    var emitter: Emitter = .{ .allocator = arena.allocator(), .constants = constants, .functions = functions };
    if (!try checkInner(module, ir, types, id, &emitter, null)) return false;
    const body_node = module.source.ast.decl(module.getDeclaration(id).?.node).ast.init_node;
    const result = emitter.values.get(body_node) orelse return error.InvalidValueId;
    const function = functions[tableIndex(module, id)] orelse return error.InvalidFunctionId;
    const body: Ir.Body = .{ .instructions = emitter.instructions.items, .result = result };
    // Structural/type verification precedes the control-flow borrow proof.
    try ir.verifyBodyStructure(function, body);
    if (try ir.borrowConflict(body)) |conflict| {
        try module.diagnostics.append(module.allocator, .{ .tag = .borrow_conflict, .span = conflict.access, .related = conflict.origin });
        return false;
    }
    try ir.setFunctionBody(function, emitter.instructions.items, result);
    return true;
}

fn tableIndex(module: *const Module, id: Module.DeclId) usize {
    if (module.compilation) |comp| return @backingInt(comp.declaration_ids.get(.{ .source = module.source.id, .local = id }).?);
    return @backingInt(id);
}

const Emitter = struct {
    allocator: std.mem.Allocator,
    constants: []const ?Ir.ConstId,
    functions: []const ?Ir.FunctionId,
    instructions: std.ArrayList(Ir.Instruction) = .empty,
    values: std.AutoHashMapUnmanaged(Ast.Node.Index, Ir.ValueId) = .empty,

    fn append(self: *Emitter, ty: Ir.TypeId, span: Ir.Span, op: Ir.Operation) Error!Ir.ValueId {
        if (self.instructions.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
        const id: Ir.ValueId = @fromBackingInt(@intCast(self.instructions.items.len));
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

fn checkInner(module: *Module, ir: *Ir, types: []const ?Ir.TypeId, id: Module.DeclId, emitter: ?*Emitter, sema: ?*Sema) Error!bool {
    if (sema == null and module.compilation == null and types.len != module.declarations.items.len) return error.InvalidTypeId;
    const declaration = module.getDeclaration(id) orelse return error.InvalidTypeId;
    const type_id = (if (sema) |analysis| try analysis.requestType(module, id) else types[tableIndex(module, id)]) orelse return false;
    const ty = ir.getType(type_id) orelse return error.InvalidTypeId;
    if (ty != .function) return error.InvalidTypeId;
    if (declaration.is_test and (ty.function.params.len != 0 or ir.getType(ty.function.result).? != .void or declaration.noreturn)) {
        try module.diagnostics.append(module.allocator, .{ .tag = .invalid_test_signature, .span = declaration.name_span });
        return false;
    }
    const full = module.source.ast.decl(declaration.node);
    const proto = module.source.ast.fullFnProto(full.ast.type_node) orelse return error.InvalidTypeId;
    if (proto.ast.params.len != ty.function.params.len) return error.InvalidTypeId;
    var context: Context = .{ .module = module, .ir = ir, .types = types, .emitter = emitter, .container = declaration.container, .concrete_type = declaration.concrete_type, .sema = sema };
    defer context.bindings.deinit(module.allocator);
    defer context.locals.deinit(module.allocator);
    var iterator = proto.iterate(&module.source.ast);
    var i: usize = 0;
    var valid = true;
    while (iterator.next()) |param| : (i += 1) {
        if (param.name_token) |token| {
            const name = module.source.ast.tokenSlice(token);
            const span = module.tokenSpan(token);
            if (context.conflict(name) != null or isPrimitive(name)) {
                const related = context.conflict(name);
                try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_shadowing, .span = span, .related = related });
                valid = false;
                continue;
            }
            const slot = if (emitter != null and context.borrowedName(name) and ir.getType(ty.function.params[i]).? != .void) blk: {
                const value = try emitter.?.append(ty.function.params[i], span, .{ .parameter = @intCast(i) });
                break :blk try emitter.?.append(ty.function.params[i], span, .{ .local_slot = value });
            } else null;
            try context.bindings.put(module.allocator, name, .{ .slot = slot, .index = @intCast(i), .name_span = span, .known = .{
                .ty = ty.function.params[i],
                .annotation = module.tokenSpan(module.source.ast.firstToken(param.type_expr)),
            } });
        }
    }
    if (!valid) return false;
    if (full.ast.init_node == 0) {
        try module.diagnostics.append(module.allocator, .{ .tag = .missing_initializer, .span = declaration.name_span });
        return false;
    }
    if (declaration.noreturn) {
        const first = module.source.ast.firstToken(full.ast.init_node);
        const last = module.source.ast.lastToken(full.ast.init_node);
        for (module.source.ast.tokens.items(.tag)[first .. last + 1], first..) |tag, token| {
            if (tag == .keyword_return) {
                try module.diagnostics.append(module.allocator, .{ .tag = .noreturn_return, .span = module.tokenSpan(@intCast(token)) });
                return false;
            }
        }
    }
    const annotation = module.tokenSpan(if (proto.ast.return_type) |node| module.source.ast.firstToken(node) else proto.lparen);
    context.return_type = .{ .ty = ty.function.result, .annotation = annotation };
    const checked = (try context.expression(full.ast.init_node, .{ .ty = ty.function.result, .annotation = annotation }, 0)) != null;
    if (checked and declaration.noreturn and !context.terminated) {
        _ = try context.fail(.noreturn_completion, full.ast.init_node, declaration.name_span);
        return false;
    }
    if (checked and !context.terminated) try context.copyNode(full.ast.init_node, ty.function.result);
    return checked;
}

const Context = struct {
    const Reservation = struct { owner: Module.Span, origin: Module.Span, parent: ?*const Reservation, reads_allowed: bool = false };
    const LoopContext = struct { allows_value: bool, result: ?Known, valued_break: bool = false, has_break: bool = false };

    module: *Module,
    ir: *Ir,
    types: []const ?Ir.TypeId,
    sema: ?*Sema = null,
    emitter: ?*Emitter = null,
    container: ?Module.DeclId = null,
    concrete_type: ?u32 = null,
    return_type: Known = undefined,
    // Termination is independent of the contextual value type. A terminal
    // expression has no merge value, and enclosing expressions alias its exit.
    terminated: bool = false,
    terminal: ?Ir.ValueId = null,
    loop_context: ?*LoopContext = null,
    bindings: std.StringHashMapUnmanaged(Binding) = .empty,
    locals: std.ArrayList([]const u8) = .empty,
    borrow_base: ?Ast.Node.Index = null,
    reservation: ?*const Reservation = null,

    fn reservedOrigin(self: *const Context, owner: Module.Span) ?Module.Span {
        var cursor = self.reservation;
        while (cursor) |current| : (cursor = current.parent) {
            if (!current.reads_allowed and std.meta.eql(owner, current.owner)) return current.origin;
        }
        return null;
    }

    fn mutationOrigin(self: *const Context, binding: Binding) ?Module.Span {
        const owner = if (binding.loan_owner) |loan| loan.owner else binding.name_span;
        var cursor = self.reservation;
        while (cursor) |current| : (cursor = current.parent) {
            // The iteration element itself may write through its exclusive loan.
            if (std.meta.eql(owner, current.owner) and (!binding.borrowed or current.reads_allowed)) return current.origin;
        }
        return null;
    }

    fn restoreScope(self: *Context, start: usize) void {
        for (self.locals.items[start..]) |name| _ = self.bindings.remove(name);
        self.locals.shrinkRetainingCapacity(start);
    }

    // Decide storage at declaration time so loans in a branch never create a
    // slot whose definition fails to dominate later identifier reads. Names
    // cannot shadow, and this syntax scan does not change value/type policy.
    fn borrowedName(self: *const Context, name: []const u8) bool {
        const ast = &self.module.source.ast;
        for (ast.nodes.items(.tag), ast.nodes.items(.data)) |tag, data| {
            if (tag == .loop_range) {
                const loop = ast.extraData(data.lhs, Ast.Node.RangeLoop);
                if (ast.nodes.items(.tag)[loop.binding] == .ref_of and ast.nodes.items(.tag)[loop.source] == .identifier and std.mem.eql(u8, name, ast.tokenSlice(ast.nodes.items(.main_token)[loop.source]))) return true;
            }
            if (tag != .ref_of) continue;
            var owner = data.rhs;
            if (ast.nodes.items(.tag)[owner] == .array_access) owner = ast.nodes.items(.data)[owner].lhs;
            if (ast.nodes.items(.tag)[owner] == .identifier and std.mem.eql(u8, name, ast.tokenSlice(ast.nodes.items(.main_token)[owner]))) return true;
        }
        return false;
    }

    fn bindLocal(self: *Context, name: []const u8, binding: Binding) Error!void {
        try self.locals.append(self.module.allocator, name);
        var stored = binding;
        if (self.borrowedName(name) and !stored.borrowed and stored.slot == null and stored.value != null and self.emitter != null and self.ir.getType(stored.known.ty).? != .void) {
            stored.slot = try self.emitter.?.append(stored.known.ty, stored.name_span, .{ .local_slot = stored.value.? });
            stored.value = null;
        }
        try self.bindings.put(self.module.allocator, name, stored);
    }

    // Context declaration IDs address the shared semantic table, whereas AST
    // container IDs remain local to the current defining module.
    fn qualify(self: *Context, module: *const Module, id: Module.DeclId) Module.DeclId {
        _ = self;
        return @fromBackingInt(@intCast(tableIndex(module, id)));
    }

    fn declarationModule(self: *Context, id: Module.DeclId) *Module {
        if (self.module.compilation) |comp| return comp.getModule(comp.declarations.items[@backingInt(id)].source).?;
        return self.module;
    }

    fn localId(self: *Context, id: Module.DeclId) Module.DeclId {
        if (self.module.compilation) |comp| return comp.declarations.items[@backingInt(id)].local;
        return id;
    }

    fn getDeclaration(self: *Context, id: Module.DeclId) Module.Declaration {
        return self.declarationModule(id).getDeclaration(self.localId(id)).?;
    }

    fn valueDeclaration(self: *Context, name: []const u8) ?Module.DeclId {
        if (self.container) |owner| if (self.module.lookupConcreteMethod(owner, self.concrete_type, name)) |id| return self.qualify(self.module, id);
        return self.qualify(self.module, self.module.lookup(name) orelse return null);
    }

    fn nominalMethod(self: *Context, ty: Ir.TypeId, name: []const u8) ?Module.DeclId {
        const nominal = self.ir.getType(ty).?;
        const source = switch (nominal) {
            .record => nominal.record.source,
            .enumeration => nominal.enumeration.source,
            else => return null,
        };
        const identity = if (nominal == .record) nominal.record.identity else nominal.enumeration.identity;
        const owner = if (self.module.compilation) |comp| comp.getModule(source).? else self.module;
        const method = owner.lookupConcreteMethod(@fromBackingInt(@intCast(identity)), if (nominal == .record and nominal.record.type_args.len != 0) @backingInt(ty) else null, name) orelse return null;
        if (owner.source.id != self.module.source.id and !owner.getDeclaration(method).?.public) return null;
        return self.qualify(owner, method);
    }

    fn conflict(self: *Context, name: []const u8) ?Module.Span {
        if (self.bindings.get(name)) |binding| return binding.name_span;
        if (self.valueDeclaration(name)) |id| return self.getDeclaration(id).name_span;
        if (self.concrete_type != null) {
            const owner = self.module.getDeclaration(self.container.?).?;
            const full = self.module.source.ast.decl(owner.node).ast;
            var iterator = self.module.source.ast.fullFnProto(full.type_node).?.iterate(&self.module.source.ast);
            while (iterator.next()) |param| {
                const token = param.name_token.?;
                if (std.mem.eql(u8, name, self.module.source.ast.tokenSlice(token))) return self.module.tokenSpan(token);
            }
        }
        return null;
    }

    fn local(self: *Context, node: Ast.Node.Index, depth: usize) Error!bool {
        const ast = &self.module.source.ast;
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
        if (ast.nodes.items(.tag)[decl.init_node] == .ref_of)
            return self.borrowLocal(node, name, name_span, depth);
        const annotation: ?Known = if (decl.type_node != 0) .{
            .ty = (try TypeResolver.resolveInContainer(self.module, self.ir, decl.type_node, self.container, self.concrete_type)) orelse return false,
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
        const slot = if ((decl.is_mutable or self.ir.getType(ty).? == .array or self.borrowedName(name)) and self.ir.getType(ty).? != .void and self.emitter != null)
            try self.emitter.?.append(ty, self.irSpan(node), .{ .local_slot = try self.emitter.?.value(decl.init_node) })
        else
            null;
        try self.bindLocal(name, .{ .known = known, .initializer = decl.init_node, .name_span = name_span, .mutable = decl.is_mutable, .slot = slot });
        return true;
    }

    fn borrowLocal(self: *Context, node: Ast.Node.Index, name: []const u8, name_span: Module.Span, depth: usize) Error!bool {
        const ast = &self.module.source.ast;
        const decl = ast.decl(node).ast;
        const operand = ast.nodes.items(.data)[decl.init_node].rhs;
        const scalar = ast.nodes.items(.tag)[operand] == .identifier;
        if (decl.is_mutable or decl.type_node != 0 or (!scalar and ast.nodes.items(.tag)[operand] != .array_access)) {
            _ = try self.fail(.unsupported_borrow, decl.init_node, null);
            return false;
        }
        const access = ast.nodes.items(.data)[operand];
        const owner_node = if (scalar) operand else access.lhs;
        if (ast.nodes.items(.tag)[owner_node] != .identifier) {
            _ = try self.fail(.unsupported_borrow, operand, null);
            return false;
        }
        const owner = self.bindings.get(ast.tokenSlice(ast.nodes.items(.main_token)[owner_node])) orelse {
            _ = try self.fail(.unsupported_borrow, owner_node, null);
            return false;
        };
        if (self.mutationOrigin(owner)) |origin| {
            _ = try self.fail(.borrow_conflict, owner_node, origin);
            return false;
        }
        const owner_type = self.ir.getType(owner.known.ty).?;
        const supported = if (scalar) owner_type != .void else owner_type == .array;
        if (owner.borrowed or !supported) {
            _ = try self.fail(.unsupported_borrow, operand, owner.name_span);
            return false;
        }
        const unit = try self.ir.internType(.void);
        const reservation = if (self.emitter) |emitter| try emitter.append(unit, self.irSpan(decl.init_node), .{ .borrow_reserve = owner.slot.? }) else null;
        if (scalar) {
            const borrow = if (self.emitter) |emitter| try emitter.append(unit, self.irSpan(decl.init_node), .{ .borrow_start = .{ .reservation = reservation.?, .index = null } }) else null;
            try self.bindLocal(name, .{ .known = owner.known, .name_span = name_span, .mutable = owner.mutable, .borrowed = true, .borrow = borrow, .loan_owner = .{ .owner = owner.name_span, .origin = self.irSpan(decl.init_node) } });
            return true;
        }
        const index_ty = try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } });
        // Reserve the source throughout index evaluation, including terminal
        // paths that never reach activation. Nested independent reservations
        // retain their parents rather than opening an invalidation window.
        const index_reservation: Reservation = .{ .owner = owner.name_span, .origin = self.span(decl.init_node), .parent = self.reservation };
        const parent_reservation = self.reservation;
        self.reservation = &index_reservation;
        defer self.reservation = parent_reservation;
        if (try self.expression(access.rhs, .{ .ty = index_ty }, depth + 1) == null) return false;
        if (self.terminated) return true;
        const borrow = if (self.emitter) |emitter| try emitter.append(unit, self.irSpan(decl.init_node), .{ .borrow_start = .{ .reservation = reservation.?, .index = try emitter.value(access.rhs) } }) else null;
        try self.bindLocal(name, .{ .known = .{ .ty = owner_type.array }, .name_span = name_span, .mutable = owner.mutable, .borrowed = true, .borrow = borrow, .loan_owner = .{ .owner = owner.name_span, .origin = self.irSpan(decl.init_node) } });
        return true;
    }

    fn blockExpression(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
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
        const ast = &self.module.source.ast;
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
                    .{ .ty = (try TypeResolver.resolveInContainer(self.module, self.ir, decl.type_node, self.container, self.concrete_type)) orelse return null }
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

    // Search only this loop's break scope. Like probeBlock, temporary bindings
    // and type diagnostics are rolled back; no initializers are evaluated.
    fn probeBreaks(self: *Context, node: Ast.Node.Index, depth: usize) Error!?Known {
        if (node == 0 or depth >= max_recursive_depth) return null;
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        switch (ast.nodes.items(.tag)[node]) {
            .loop_range => {
                const range = ast.fullRange(node).?;
                return (try self.probeBreaks(range.source, depth + 1)) orelse
                    try self.probeBreaks(range.else_expr, depth + 1);
            },
            .loop_condition, .loop_cont, .loop_else => return self.probeBreaks(ast.fullLoop(node).?.else_expr, depth + 1),
            // The operand runs before this break. Its own controls may exit
            // this loop before the operand ever produces a value.
            .@"break" => return if (data.rhs != 0)
                (try self.probeBreaks(data.rhs, depth + 1)) orelse try self.probe(data.rhs, depth + 1)
            else
                null,
            .block, .block_two, .block_newline, .block_two_newline => {
                const start = self.locals.items.len;
                defer self.restoreScope(start);
                const diagnostics_start = self.module.diagnostics.items.len;
                defer self.module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
                var buffer: [2]Ast.Node.Index = undefined;
                for (ast.fullBlock(&buffer, node).?.ast.statements) |statement| {
                    const tag = ast.nodes.items(.tag)[statement];
                    if (tag == .decl) {
                        const decl = ast.decl(statement).ast;
                        if (try self.probeBreaks(decl.init_node, depth + 1)) |known| return known;
                        const token = decl.let_token + 1 + @as(u32, @intFromBool(decl.is_mutable));
                        const name = ast.tokenSlice(token);
                        if (self.conflict(name) != null or isPrimitive(name)) return null;
                        const known: ?Known = if (decl.type_node != 0)
                            if (try TypeResolver.resolveInContainer(self.module, self.ir, decl.type_node, self.container, self.concrete_type)) |ty| .{ .ty = ty } else null
                        else if (decl.init_node != 0) try self.probe(decl.init_node, depth + 1) else null;
                        if (known) |value| try self.bindLocal(name, .{ .known = value, .name_span = self.module.tokenSpan(token) });
                    } else {
                        if (try self.probeBreaks(statement, depth + 1)) |known| return known;
                        if (tag == .@"return" or tag == .@"break" or tag == .@"continue") break;
                    }
                }
            },
            .@"if", .if_else => {
                const branch = ast.fullIf(node).?.ast;
                return (try self.probeBreaks(branch.cond_expr, depth + 1)) orelse
                    (try self.probeBreaks(branch.then_expr, depth + 1)) orelse
                    (try self.probeBreaks(branch.else_expr, depth + 1));
            },
            .match => {
                const match = ast.fullMatch(node).?.ast;
                for (match.subjects) |subject| if (try self.probeBreaks(subject, depth + 1)) |known| return known;
                for (match.cases) |case| if (try self.probeBreaks(ast.fullMatchCase(case).?.ast.target_expr, depth + 1)) |known| return known;
            },
            .function_pipe => return (try self.probeBreaks(data.rhs, depth + 1)) orelse try self.probeBreaks(data.lhs, depth + 1),
            .call, .call_one => {
                const call = ast.fullCall(node).?;
                if (try self.probeBreaks(call.ast.fn_expr, depth + 1)) |known| return known;
                for (call.ast.args) |arg| if (try self.probeBreaks(arg, depth + 1)) |known| return known;
            },
            .array_init, .array_init_comma, .array_init_two, .array_init_two_comma => {
                var buffer: [2]Ast.Node.Index = undefined;
                for (self.arrayElements(node, &buffer)) |element| if (try self.probeBreaks(element, depth + 1)) |known| return known;
            },
            .negation, .bit_not, .bool_not, .@"return" => return self.probeBreaks(data.rhs, depth + 1),
            .field_access => return self.probeBreaks(data.lhs, depth + 1),
            .slice, .range, .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .bool_and, .bool_or, .in, .array_access, .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => return (try self.probeBreaks(data.lhs, depth + 1)) orelse (try self.probeBreaks(data.rhs, depth + 1)),
            else => {},
        }
        return null;
    }

    fn probeRange(self: *Context, node: Ast.Node.Index, depth: usize) Error!?Known {
        const ast = &self.module.source.ast;
        const range = ast.fullRange(node).?;
        if (range.else_expr == 0) return .{ .ty = try self.ir.internType(.void) };
        const diagnostics_start = self.module.diagnostics.items.len;
        defer self.module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
        // Probe fallback in the parent scope, then make the index available only
        // while looking for owned break hints. Neither walk evaluates bounds.
        const fallback = try self.probe(range.else_expr, depth + 1);
        const source = try self.probe(range.source, depth + 1);
        const index: ?Known = if (source) |known| switch (self.ir.getType(known.ty).?) {
            .range => |ty| .{ .ty = ty },
            .array => |ty| .{ .ty = ty },
            .string => .{ .ty = try self.ir.internType(.rune) },
            else => null,
        } else null;
        const scope = self.locals.items.len;
        defer self.restoreScope(scope);
        const capture = if (ast.nodes.items(.tag)[range.binding] == .ref_of) ast.nodes.items(.data)[range.binding].rhs else range.binding;
        const token = ast.nodes.items(.main_token)[capture];
        const name = ast.tokenSlice(token);
        if (index) |known| {
            if (!std.mem.eql(u8, name, "_") and self.conflict(name) == null and !isPrimitive(name))
                try self.bindLocal(name, .{ .known = known, .name_span = self.module.tokenSpan(token) });
        }
        return (try self.probeBreaks(range.body, depth + 1)) orelse fallback;
    }

    fn span(self: *Context, node: Ast.Node.Index) Module.Span {
        return self.module.tokenSpan(self.module.source.ast.nodes.items(.main_token)[node]);
    }

    fn fail(self: *Context, tag: Module.Diagnostic.Tag, node: Ast.Node.Index, related: ?Module.Span) Error!?Ir.TypeId {
        const location = if (tag == .unreachable_code) self.module.tokenSpan(self.module.source.ast.firstToken(node)) else if (tag == .unknown_field and self.module.source.ast.nodes.items(.tag)[node] == .field_access) self.module.tokenSpan(self.module.source.ast.nodes.items(.data)[node].rhs) else self.span(node);
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

    fn globalType(self: *Context, id: Module.DeclId) Error!?Known {
        const owner = self.declarationModule(id);
        if (self.getDeclaration(id).is_test) {
            try self.module.diagnostics.append(self.module.allocator, .{ .tag = .test_not_callable, .span = self.getDeclaration(id).name_span });
            return null;
        }
        const ty = (if (self.sema) |analysis| try analysis.requestType(owner, self.localId(id)) else if (@backingInt(id) < self.types.len) self.types[@backingInt(id)] else null) orelse return null;
        const full = owner.source.ast.decl(self.getDeclaration(id).node);
        return .{ .ty = ty, .annotation = if (full.ast.type_node != 0) owner.tokenSpan(owner.source.ast.firstToken(full.ast.type_node)) else null };
    }

    fn isAssertion(self: *Context, node: Ast.Node.Index) bool {
        const ast = &self.module.source.ast;
        return ast.nodes.items(.tag)[node] == .builtin_literal and
            std.mem.eql(u8, ast.tokenSlice(ast.nodes.items(.main_token)[node]), "@assert");
    }

    fn isAscription(self: *Context, node: Ast.Node.Index) bool {
        const ast = &self.module.source.ast;
        return ast.nodes.items(.tag)[node] == .builtin_literal and
            std.mem.eql(u8, ast.tokenSlice(ast.nodes.items(.main_token)[node]), "@as");
    }

    fn ascribedType(self: *Context, call: Ast.full.Call) Error!?Known {
        if (call.ast.args.len != 2) return null;
        const node = call.ast.args[0];
        const ty = (try TypeResolver.resolveInContainer(self.module, self.ir, node, self.container, self.concrete_type)) orelse return null;
        return .{ .ty = ty, .annotation = self.span(node) };
    }

    fn staticMethod(self: *Context, node: Ast.Node.Index) Error!?Module.DeclId {
        const ast = &self.module.source.ast;
        if (ast.nodes.items(.tag)[node] != .field_access) return null;
        const data = ast.nodes.items(.data)[node];
        if (self.module.compilation) |comp| if (comp.reference(self.module, node)) |ref| {
            const owner = comp.getModule(ref.source).?;
            if (self.sema) |analysis| try analysis.syncEntries(owner);
            return self.qualify(owner, ref.local);
        };
        if (ast.nodes.items(.tag)[data.lhs] == .identifier) {
            const name = ast.tokenSlice(ast.firstToken(data.lhs));
            if (self.bindings.contains(name)) return null;
            const id = self.valueDeclaration(name) orelse return null;
            if (!self.getDeclaration(id).is_type_alias and !TypeResolver.isThisAlias(&self.declarationModule(id).source.ast, self.getDeclaration(id).node)) return null;
        } else if (ast.fullCall(data.lhs)) |call| {
            if (TypeResolver.genericTarget(self.module, call.ast.fn_expr) == null and !std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This")) return null;
        } else {
            const comp = self.module.compilation orelse return null;
            const ref = comp.reference(self.module, data.lhs) orelse return null;
            if (!comp.getModule(ref.source).?.getDeclaration(ref.local).?.is_type_alias) return null;
        }
        const ty = (try TypeResolver.resolveInContainer(self.module, self.ir, data.lhs, self.container, self.concrete_type)) orelse return null;
        if (self.sema) |analysis| try analysis.syncEntries(self.module);
        return self.nominalMethod(ty, ast.tokenSlice(data.rhs));
    }

    fn receiverMethod(self: *Context, ty: Ir.TypeId, name: []const u8) ?Module.DeclId {
        if (self.fieldIndex(ty, name) != null) return null;
        return self.nominalMethod(ty, name);
    }

    fn directNoreturn(self: *Context, node: Ast.Node.Index) Error!bool {
        const ast = &self.module.source.ast;
        const id = if (ast.nodes.items(.tag)[node] == .identifier) blk: {
            const name = self.module.sourceSlice(self.span(node)).?;
            if (self.bindings.contains(name)) return false;
            break :blk self.valueDeclaration(name) orelse return false;
        } else (try self.staticMethod(node)) orelse return false;
        return self.getDeclaration(id).noreturn;
    }

    fn finishCall(self: *Context, node: Ast.Node.Index, callee: Ast.Node.Index, result: Ir.TypeId, expected: ?Known) Error!?Ir.TypeId {
        if (!self.terminated and try self.directNoreturn(callee)) {
            if (self.emitter) |emitter| {
                if (!try self.emitNode(node, result)) return null;
                self.terminal = try emitter.value(node);
            }
            self.terminated = true;
        }
        return self.fit(result, expected, node);
    }

    fn fieldIndex(self: *Context, ty: Ir.TypeId, name: []const u8) ?u32 {
        const record = self.ir.getType(ty).?;
        if (record != .record) return null;
        for (record.record.fields, 0..) |field, i| if (std.mem.eql(u8, field.name, name)) return @intCast(i);
        return null;
    }

    fn convertPrintable(self: *Context, node: Ast.Node.Index, ty: Ir.TypeId) Error!bool {
        const record = self.ir.getType(ty).?;
        if (record == .boolean or record == .integer or record == .string or record == .rune) return true;
        if (record != .record and record != .enumeration) {
            _ = try self.fail(.unsupported_body_expression, node, null);
            return false;
        }
        const method = self.nominalMethod(ty, "toString") orelse {
            _ = try self.fail(.invalid_to_string, node, null);
            return false;
        };
        const signature_id = if (try self.globalType(method)) |known| known.ty else {
            _ = try self.fail(.invalid_to_string, node, null);
            return false;
        };
        const signature = self.ir.getType(signature_id).?;
        if (signature != .function or signature.function.params.len != 1 or signature.function.params[0] != ty or self.ir.getType(signature.function.result).? != .string) {
            _ = try self.fail(.invalid_to_string, node, self.getDeclaration(method).name_span);
            return false;
        }
        if (self.emitter) |emitter| {
            const callee = try emitter.append(signature_id, self.irSpan(node), .{ .function_ref = emitter.functions[@backingInt(method)].? });
            const args = try emitter.allocator.alloc(Ir.ValueId, 1);
            args[0] = try emitter.value(node);
            const value = try emitter.append(signature.function.result, self.irSpan(node), .{ .call = .{ .callee = callee, .args = args } });
            try emitter.values.put(emitter.allocator, node, value);
        }
        return true;
    }

    fn tagged(self: *Context, ty: Ir.TypeId) bool {
        const t = self.ir.getType(ty).?;
        return t == .record and t.record.tagged;
    }

    fn qualifiedUnion(self: *Context, base: Ast.Node.Index) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
        if (ast.nodes.items(.tag)[base] != .field_access) return null;
        const qualifier = ast.nodes.items(.data)[base].lhs;
        if (ast.fullCall(qualifier)) |call| {
            if (TypeResolver.genericTarget(self.module, call.ast.fn_expr) == null and !std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This")) return null;
        } else {
            if (self.module.compilation) |comp| if (comp.reference(self.module, qualifier)) |ref| {
                if (!comp.getModule(ref.source).?.getDeclaration(ref.local).?.is_type_alias) return null;
                return try TypeResolver.resolveInContainer(self.module, self.ir, qualifier, self.container, self.concrete_type);
            };
            if (ast.nodes.items(.tag)[qualifier] != .identifier) return null;
            const name = ast.tokenSlice(ast.firstToken(qualifier));
            if (self.bindings.contains(name)) return null;
            const id = self.valueDeclaration(name) orelse return null;
            if (!self.getDeclaration(id).is_type_alias and !TypeResolver.isThisAlias(&self.declarationModule(id).source.ast, self.getDeclaration(id).node)) return null;
        }
        const ty = (try TypeResolver.resolveInContainer(self.module, self.ir, qualifier, self.container, self.concrete_type)) orelse return null;
        return if (self.tagged(ty)) ty else null;
    }

    const PropagationShape = struct { family: enum { result, option }, ok: u32, err: u32 };

    fn propagationShape(self: *Context, ty: Ir.TypeId) ?PropagationShape {
        if (!self.tagged(ty) or self.ir.getType(ty).?.record.fields.len != 2) return null;
        if (self.fieldIndex(ty, "ok")) |ok| {
            return .{ .family = .result, .ok = ok, .err = self.fieldIndex(ty, "err") orelse return null };
        }
        const some = self.fieldIndex(ty, "some") orelse return null;
        const none = self.fieldIndex(ty, "none") orelse return null;
        if (self.ir.getType(self.ir.getType(ty).?.record.fields[none].ty).? != .void) return null;
        return .{ .family = .option, .ok = some, .err = none };
    }

    fn propagate(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const operand = self.module.source.ast.nodes.items(.data)[node].lhs;
        const input = (try self.expression(operand, null, depth + 1)) orelse return null;
        if (self.terminated) return if (expected) |want| want.ty else input;
        const shape = self.propagationShape(input) orelse return self.fail(.invalid_propagation_operand, node, null);
        const output = self.return_type.ty;
        const target = self.propagationShape(output) orelse return self.fail(.invalid_propagation_return, node, self.return_type.annotation);
        if (shape.family != target.family) return self.fail(.invalid_propagation_return, node, self.return_type.annotation);
        const ok = self.ir.getType(input).?.record.fields[shape.ok].ty;
        const err = self.ir.getType(input).?.record.fields[shape.err].ty;
        if (err != self.ir.getType(output).?.record.fields[target.err].ty) return self.fail(.propagation_error_mismatch, node, self.return_type.annotation);
        if (try self.fit(ok, expected, node) == null) return null;
        if (self.emitter) |emitter| {
            const unit = try self.ir.internType(.void);
            const boolean = try self.ir.internType(.boolean);
            const value = try emitter.value(operand);
            const condition = try emitter.append(boolean, self.irSpan(node), .{ .union_tag = .{ .base = value, .index = shape.err } });
            const then_start: u32 = @intCast(emitter.instructions.items.len);
            const payload = try emitter.append(err, self.irSpan(node), .{ .union_payload = .{ .base = value, .index = shape.err } });
            const failure = try emitter.append(output, self.irSpan(node), .{ .union_init = .{ .index = target.err, .payload = if (self.ir.getType(err).? == .void) null else payload } });
            const exit = try emitter.append(unit, self.irSpan(node), .{ .return_value = failure });
            const else_start: u32 = @intCast(emitter.instructions.items.len);
            const success = try emitter.append(ok, self.irSpan(node), .{ .union_payload = .{ .base = value, .index = shape.ok } });
            try emitter.record(node, ok, self.irSpan(node), .{ .branch = .{ .condition = condition, .then_start = then_start, .else_start = else_start, .then_value = exit, .else_value = success } });
        }
        return ok;
    }

    fn unionInit(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        const base = data.lhs;
        const contextual = ast.nodes.items(.tag)[base] == .enum_literal;
        const ty = if (contextual) (if (expected) |want| want.ty else return self.fail(.union_needs_type, node, null)) else (try self.qualifiedUnion(base)).?;
        if (!self.tagged(ty)) return self.fail(.invalid_union_constructor, node, null);
        if (try self.fit(ty, expected, node) == null) return null;
        const name = ast.tokenSlice(if (contextual) ast.nodes.items(.main_token)[base] else ast.nodes.items(.data)[base].rhs);
        const index = self.fieldIndex(ty, name) orelse return self.fail(.unknown_field, base, null);
        const payload_ty = self.ir.getType(ty).?.record.fields[index].ty;
        const payload = if (ast.nodes.items(.tag)[node] == .array_access) data.rhs else blk: {
            if (ast.fullStructInit(node).?.fields.len != 0) return self.fail(.invalid_union_constructor, node, null);
            break :blk @as(Ast.Node.Index, 0);
        };
        const unit = self.ir.getType(payload_ty).? == .void;
        if ((payload == 0) != unit) return self.fail(.invalid_union_constructor, node, null);
        if (payload != 0 and try self.expression(payload, .{ .ty = payload_ty }, depth + 1) == null) return null;
        if (self.terminated) return ty;
        if (self.emitter) |emitter| try emitter.record(node, ty, self.irSpan(node), .{ .union_init = .{ .index = index, .payload = if (payload != 0) try emitter.value(payload) else null } });
        return ty;
    }

    fn recordInit(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
        const init = ast.fullStructInit(node).?;
        const ty = (try TypeResolver.resolveInContainer(self.module, self.ir, init.type_node, self.container, self.concrete_type)) orelse return null;
        const record = self.ir.getType(ty).?;
        if (record != .record) return self.fail(.not_a_type, init.type_node, null);
        if (record.record.tagged) return self.fail(.invalid_union_constructor, node, null);
        if (try self.fit(ty, expected, node) == null) return null;
        const seen = try self.module.allocator.alloc(bool, record.record.fields.len);
        defer self.module.allocator.free(seen);
        @memset(seen, false);
        const values = if (self.emitter) |emitter| try emitter.allocator.alloc(Ir.ValueId, seen.len) else null;
        for (init.fields) |field_node| {
            const name = ast.tokenSlice(ast.nodes.items(.main_token)[field_node]);
            const index = self.fieldIndex(ty, name) orelse return self.fail(.unknown_field, field_node, null);
            if (seen[index]) return self.fail(.duplicate_field, field_node, null);
            seen[index] = true;
            const value_node = ast.nodes.items(.data)[field_node].rhs;
            if (try self.expression(value_node, .{ .ty = record.record.fields[index].ty }, depth + 1) == null) return null;
            if (self.terminated) return ty;
            if (self.emitter) |emitter| values.?[index] = try emitter.value(value_node);
        }
        for (record.record.fields, seen, 0..) |field, supplied, i| {
            if (supplied) continue;
            const default = field.default orelse return self.fail(.missing_field, node, field.span);
            if (self.emitter) |emitter| values.?[i] = try emitter.append(field.ty, self.irSpan(node), .{ .constant = default });
        }
        if (self.emitter) |emitter| try emitter.record(node, ty, self.irSpan(node), .{ .record_init = values.? });
        return ty;
    }

    fn isPanic(self: *Context, node: Ast.Node.Index) bool {
        return self.module.source.ast.nodes.items(.tag)[node] == .builtin_literal and
            std.mem.eql(u8, self.module.sourceSlice(self.span(node)).?, "@panic");
    }

    fn printing(self: *Context, node: Ast.Node.Index) ?bool {
        const ast = &self.module.source.ast;
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
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        return switch (ast.nodes.items(.tag)[node]) {
            .identifier => blk: {
                const name = self.module.sourceSlice(self.span(node)).?;
                if (self.bindings.get(name)) |binding| break :blk binding.known;
                if (self.valueDeclaration(name)) |id| break :blk try self.globalType(id);
                if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) break :blk .{ .ty = try self.ir.internType(.boolean) };
                break :blk null;
            },
            .ref_of => self.probe(data.rhs, depth + 1),
            .unwrap => blk: {
                const operand = (try self.probe(data.lhs, depth + 1)) orelse break :blk null;
                const shape = self.propagationShape(operand.ty) orelse break :blk null;
                break :blk .{ .ty = self.ir.getType(operand.ty).?.record.fields[shape.ok].ty };
            },
            .string_literal, .slice => .{ .ty = try self.ir.internType(.string) },
            .char_literal => .{ .ty = try self.ir.internType(.rune) },
            .array_init, .array_init_comma, .array_init_two, .array_init_two_comma => try self.probeArray(node, depth),
            .array_access => blk: {
                if (try self.qualifiedUnion(data.lhs)) |ty| break :blk .{ .ty = ty };
                const array = (try self.probe(data.lhs, depth + 1)) orelse break :blk null;
                const ty = self.ir.getType(array.ty).?;
                break :blk switch (ty) {
                    .array => .{ .ty = ty.array },
                    .string => .{ .ty = try self.ir.internType(.rune) },
                    else => null,
                };
            },
            .container_decl_arg => blk: {
                if (ast.nodes.items(.tag)[data.lhs] == .enum_literal) break :blk null;
                if (try self.qualifiedUnion(data.lhs)) |ty| break :blk .{ .ty = ty };
                const ty = (try TypeResolver.resolveInContainer(self.module, self.ir, data.lhs, self.container, self.concrete_type)) orelse break :blk null;
                break :blk .{ .ty = ty };
            },
            .field_access => blk: {
                if (try Literal.qualifiedEnum(self.module, self.ir, node)) |ty| break :blk .{ .ty = ty };
                if (try self.staticMethod(node)) |id| break :blk try self.globalType(id);
                const base = (try self.probe(data.lhs, depth + 1)) orelse break :blk null;
                if (self.fieldIndex(base.ty, ast.tokenSlice(data.rhs))) |index| break :blk .{ .ty = self.ir.getType(base.ty).?.record.fields[index].ty };
                break :blk if (std.mem.eql(u8, ast.tokenSlice(data.rhs), "len")) .{ .ty = try self.ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } }) } else null;
            },
            .block, .block_two, .block_newline, .block_two_newline => try self.probeBlock(node, depth),
            .loop_condition, .loop_cont, .loop_else => blk: {
                const loop = ast.fullLoop(node).?;
                if (loop.condition != 0 and loop.else_expr == 0) break :blk .{ .ty = try self.ir.internType(.void) };
                break :blk (try self.probeBreaks(loop.condition, depth + 1)) orelse
                    (try self.probeBreaks(loop.body, depth + 1)) orelse
                    (try self.probeBreaks(loop.continuation, depth + 1)) orelse
                    (if (loop.else_expr != 0) try self.probe(loop.else_expr, depth + 1) else null);
            },
            .range => blk: {
                var element = (try self.probe(data.lhs, depth + 1)) orelse try self.probe(data.rhs, depth + 1);
                if (element == null) infer: {
                    const wide = try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 128 } });
                    const diagnostics_start = self.module.diagnostics.items.len;
                    defer self.module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
                    var negative = false;
                    for ([_]Ast.Node.Index{ data.lhs, data.rhs }) |bound| switch (try Literal.check(self.module, self.ir, bound, wide, null)) {
                        .constant => |id| negative = negative or self.ir.getConstant(id).?.value.signed_integer < 0,
                        else => break :infer,
                    };
                    element = .{ .ty = try self.ir.internType(.{ .integer = .{ .signedness = if (negative) .signed else .unsigned, .bits = Ir.native_pointer_bits } }) };
                }
                const known = element orelse break :blk null;
                if (self.ir.getType(known.ty).? != .integer) break :blk null;
                break :blk .{ .ty = try self.ir.internType(.{ .range = known.ty }) };
            },
            .loop_range => try self.probeRange(node, depth),
            .@"if", .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => .{ .ty = try self.ir.internType(.void) },
            .if_else => blk: {
                const branch = ast.fullIf(node).?.ast;
                break :blk (try self.probe(branch.then_expr, depth + 1)) orelse try self.probe(branch.else_expr, depth + 1);
            },
            .negation, .bit_not => try self.probe(data.rhs, depth + 1),
            .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor => (try self.probe(data.lhs, depth + 1)) orelse try self.probe(data.rhs, depth + 1),
            .in, .bool_not, .bool_and, .bool_or, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => .{ .ty = try self.ir.internType(.boolean) },
            .match => blk: {
                const match = ast.fullMatch(node).?.ast;
                const saved_emitter = self.emitter;
                self.emitter = null;
                defer self.emitter = saved_emitter;
                const diagnostic_start = self.module.diagnostics.items.len;
                defer self.module.diagnostics.shrinkRetainingCapacity(diagnostic_start);
                const subject = if (match.subjects.len == 1) try self.probe(match.subjects[0], depth + 1) else null;
                for (match.cases) |case| {
                    const scope = self.locals.items.len;
                    defer self.restoreScope(scope);
                    const prong = ast.fullMatchCase(case).?.ast;
                    if (prong.capture_token != null) {
                        const known = subject orelse continue;
                        if (!try self.bindMatchCapture(prong, &.{.{ .ty = known.ty, .value = null }})) continue;
                    }
                    if (try self.probe(prong.target_expr, depth + 1)) |known| break :blk known;
                }
                break :blk null;
            },
            .function_pipe => blk: {
                if (try self.directNoreturn(data.rhs)) break :blk null;
                if (self.printing(data.rhs) != null) break :blk .{ .ty = try self.ir.internType(.void) };
                const callee = (try self.probe(data.rhs, depth + 1)) orelse break :blk null;
                const ty = self.ir.getType(callee.ty).?;
                break :blk if (ty == .function) .{ .ty = ty.function.result } else null;
            },
            .call, .call_one => blk: {
                const call = ast.fullCall(node).?;
                if (try self.directNoreturn(call.ast.fn_expr)) break :blk null;
                if (self.isAscription(call.ast.fn_expr)) {
                    const diagnostics_start = self.module.diagnostics.items.len;
                    defer self.module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
                    break :blk try self.ascribedType(call);
                }
                if (self.isAssertion(call.ast.fn_expr) or self.printing(call.ast.fn_expr) != null) break :blk .{ .ty = try self.ir.internType(.void) };
                if (ast.nodes.items(.tag)[call.ast.fn_expr] == .field_access and try self.staticMethod(call.ast.fn_expr) == null) {
                    const field = ast.nodes.items(.data)[call.ast.fn_expr];
                    if (try self.probe(field.lhs, depth + 1)) |receiver| {
                        if (self.receiverMethod(receiver.ty, ast.tokenSlice(field.rhs))) |method| {
                            if (self.getDeclaration(method).noreturn) break :blk null;
                            const signature = (try self.globalType(method)) orelse break :blk null;
                            const function = self.ir.getType(signature.ty).?;
                            if (function == .function) break :blk .{ .ty = function.function.result };
                        }
                    }
                }
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
        const ast = &self.module.source.ast;
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
            .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .bool_and, .bool_or => {
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
        if (self.terminated) return if (expected) |want| want.ty else try self.ir.internType(.void);
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
        const ast = &self.module.source.ast;
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
        const ast = &self.module.source.ast;
        return .{
            .source = self.module.source.id,
            .start = self.module.tokenSpan(ast.firstToken(node)).start,
            .end = self.module.tokenSpan(ast.lastToken(node)).end,
        };
    }

    fn emitNode(self: *Context, node: Ast.Node.Index, ty: Ir.TypeId) Error!bool {
        const emitter = self.emitter.?;
        if (emitter.values.contains(node)) return true;
        const ast = &self.module.source.ast;
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
            .slice => blk: {
                const bounds = ast.slice(node).ast;
                break :blk .{ .string_slice = .{
                    .base = try emitter.value(bounds.sliced),
                    .start = if (bounds.start != 0) try emitter.value(bounds.start) else null,
                    .end = if (bounds.end != 0) try emitter.value(bounds.end) else null,
                } };
            },
            .array_access => .{ .array_index = .{ .array = try emitter.value(data.lhs), .index = try emitter.value(data.rhs) } },
            .field_access => blk: {
                if (try self.staticMethod(node)) |id| {
                    if (emitter.functions[@backingInt(id)]) |function| break :blk .{ .function_ref = function };
                    if (emitter.constants[@backingInt(id)]) |constant| break :blk .{ .constant = constant };
                    return error.InvalidFunctionId;
                }
                const base = try emitter.value(data.lhs);
                if (self.fieldIndex(emitter.instructions.items[@backingInt(base)].ty, ast.tokenSlice(data.rhs))) |index| break :blk .{ .field_read = .{ .base = base, .index = index } };
                break :blk .{ .array_len = base };
            },
            .in => .{ .array_contains = .{ .value = try emitter.value(data.lhs), .array = try emitter.value(data.rhs) } },
            .identifier => blk: {
                const name = self.module.sourceSlice(self.span(node)).?;
                if (self.bindings.get(name)) |binding| {
                    if (binding.borrowed) break :blk .{ .borrow_read = binding.borrow.? };
                    if (binding.value) |value| {
                        try emitter.values.put(emitter.allocator, node, value);
                        return true;
                    }
                    if (binding.slot) |slot| break :blk .{ .local_load = slot };
                    if (binding.initializer) |initializer| {
                        try emitter.values.put(emitter.allocator, node, try emitter.value(initializer));
                        if (self.ir.getType(ty).? == .array) try self.copyNode(node, ty);
                        return true;
                    }
                    break :blk .{ .parameter = binding.index };
                }
                const id = self.valueDeclaration(name) orelse return error.InvalidFunctionId;
                if (emitter.functions[@backingInt(id)]) |function| break :blk .{ .function_ref = function };
                if (emitter.constants[@backingInt(id)]) |constant| break :blk .{ .constant = constant };
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
            .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => .{ .binary = .{
                .op = switch (tag) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .mul,
                    .div => .div,
                    .mod => .mod,
                    .bit_and => .bit_and,
                    .bit_or => .bit_or,
                    .bit_xor => .bit_xor,
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
            .function_pipe => blk: {
                const args = try emitter.allocator.alloc(Ir.ValueId, 1);
                args[0] = try emitter.value(data.lhs);
                if (self.printing(data.rhs)) |newline| break :blk .{ .print = .{ .args = args, .newline = newline } };
                break :blk .{ .call = .{ .callee = try emitter.value(data.rhs), .args = args, .noreturn = try self.directNoreturn(data.rhs) } };
            },
            .call, .call_one => blk: {
                const call = ast.fullCall(node).?;
                if (self.isAscription(call.ast.fn_expr)) {
                    try emitter.values.put(emitter.allocator, node, try emitter.value(call.ast.args[1]));
                    return true;
                }
                const args = try emitter.allocator.alloc(Ir.ValueId, call.ast.args.len);
                for (call.ast.args, args) |arg, *value| value.* = try emitter.value(arg);
                if (self.printing(call.ast.fn_expr)) |newline| break :blk .{ .print = .{ .args = args, .newline = newline } };
                break :blk .{ .call = .{ .callee = try emitter.value(call.ast.fn_expr), .args = args, .noreturn = try self.directNoreturn(call.ast.fn_expr) } };
            },
            else => {
                _ = try self.fail(.unsupported_lowering, node, null);
                return false;
            },
        };
        try emitter.record(node, ty, self.irSpan(node), op);
        // All ordinary array reads, including alias reads, detach before
        // index/RHS effects. Creating a loan itself never reads or clones.
        if (tag == .identifier and self.ir.getType(ty).? == .array) try self.copyNode(node, ty);
        return true;
    }

    fn fieldAssignment(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        const target = ast.nodes.items(.data)[data.lhs];
        const name = ast.tokenSlice(target.rhs);
        if (std.mem.eql(u8, name, "len")) {
            if (try self.probe(target.lhs, depth + 1)) |base| {
                const ty = self.ir.getType(base.ty).?;
                if (ty == .array or ty == .string) return self.fail(.unsupported_array_length_write, data.lhs, null);
            }
        }
        var path_nodes: [Ir.max_record_depth]Ast.Node.Index = undefined;
        var path_len: usize = 0;
        var root = target.lhs;
        while (ast.nodes.items(.tag)[root] == .field_access) {
            if (path_len == path_nodes.len) return self.fail(.record_layout_limit, data.lhs, null);
            path_nodes[path_len] = root;
            path_len += 1;
            root = ast.nodes.items(.data)[root].lhs;
        }
        if (ast.nodes.items(.tag)[root] != .identifier) return self.fail(.unsupported_assignment_target, data.lhs, null);
        const binding_name = self.module.sourceSlice(self.span(root)).?;
        const binding = self.bindings.get(binding_name) orelse {
            if (self.module.lookup(binding_name)) |id| return self.fail(.immutable_assignment, target.lhs, self.module.getDeclaration(id).?.name_span);
            return self.fail(.unknown_name, target.lhs, null);
        };
        if (self.mutationOrigin(binding)) |origin| return self.fail(.borrow_conflict, root, origin);
        var record_id = binding.known.ty;
        var path: [Ir.max_record_depth]u32 = undefined;
        for (0..path_len) |p| {
            const access = path_nodes[path_len - p - 1];
            const part = ast.tokenSlice(ast.nodes.items(.data)[access].rhs);
            const current = self.ir.getType(record_id).?;
            if (current != .record or current.record.tagged) return self.fail(.unsupported_assignment_target, access, null);
            const index = self.fieldIndex(record_id, part) orelse return self.fail(.unknown_field, access, null);
            path[p] = index;
            record_id = current.record.fields[index].ty;
        }
        const record = self.ir.getType(record_id).?;
        if (record != .record or record.record.tagged) return self.fail(.unsupported_assignment_target, data.lhs, null);
        if (!binding.mutable) return self.fail(.immutable_assignment, root, binding.name_span);
        const index = self.fieldIndex(record_id, name) orelse return self.fail(.unknown_field, data.lhs, null);
        const field = record.record.fields[index];
        const reservation: Reservation = .{ .owner = if (binding.loan_owner) |loan| loan.owner else binding.name_span, .origin = if (binding.loan_owner) |loan| loan.origin else self.irSpan(node), .parent = self.reservation };
        const parent = self.reservation;
        if (binding.borrowed) self.reservation = &reservation;
        defer self.reservation = parent;
        const tag = ast.nodes.items(.tag)[node];
        const compound = tag != .assign;
        const ty = self.ir.getType(field.ty).?;
        if (compound and ty != .integer and !(ty == .string and tag == .assign_add)) return self.fail(.operator_not_supported, node, null);
        if (compound and try self.expression(data.lhs, .{ .ty = field.ty }, depth + 1) == null) return null;
        if (try self.expression(data.rhs, .{ .ty = field.ty }, depth + 1) == null) return null;
        const unit = try self.ir.internType(.void);
        if (!self.terminated) {
            if (self.emitter) |emitter| {
                var value = try emitter.value(data.rhs);
                if (compound) value = try emitter.append(field.ty, self.irSpan(node), .{ .binary = .{
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
                if (binding.borrowed) {
                    if (path_len == path.len) return self.fail(.record_layout_limit, data.lhs, null);
                    path[path_len] = index;
                    try emitter.record(node, unit, self.irSpan(node), .{ .borrow_store = .{ .borrow = binding.borrow.?, .value = value, .path = try emitter.allocator.dupe(u32, path[0 .. path_len + 1]) } });
                } else try emitter.record(node, unit, self.irSpan(node), .{ .field_store = .{ .slot = binding.slot.?, .index = index, .value = value, .path = try emitter.allocator.dupe(u32, path[0..path_len]) } });
            }
        }
        return self.fit(unit, expected, node);
    }

    fn elementAssignment(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
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
        const reservation: Reservation = .{ .owner = if (binding.loan_owner) |loan| loan.owner else binding.name_span, .origin = if (binding.loan_owner) |loan| loan.origin else self.irSpan(node), .parent = self.reservation };
        const parent = self.reservation;
        if (binding.borrowed) self.reservation = &reservation;
        defer self.reservation = parent;
        if (self.mutationOrigin(binding)) |origin| return self.fail(.borrow_conflict, target.lhs, origin);
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
                if (binding.borrowed) {
                    try emitter.record(node, unit, self.irSpan(node), .{ .borrow_store = .{ .borrow = binding.borrow.?, .value = updated, .path = &.{} } });
                } else try emitter.record(node, unit, self.irSpan(node), .{ .local_store = .{ .slot = binding.slot.?, .value = updated } });
            }
        }
        return self.fit(unit, expected, node);
    }

    const MatchSubject = struct { ty: Ir.TypeId, value: ?Ir.ValueId };
    const MatchResult = struct { ty: Ir.TypeId, value: ?Ir.ValueId };

    fn wildcard(self: *Context, node: Ast.Node.Index) bool {
        const ast = &self.module.source.ast;
        return ast.nodes.items(.tag)[node] == .identifier and std.mem.eql(u8, ast.tokenSlice(ast.nodes.items(.main_token)[node]), "_");
    }

    fn catchall(self: *Context, patterns: []const Ast.Node.Index) bool {
        for (patterns) |pattern| if (!self.wildcard(pattern)) return false;
        return true;
    }

    fn patternShape(self: *Context, node: Ast.Node.Index, depth: usize) Error!bool {
        if (depth >= max_recursive_depth) {
            _ = try self.fail(.body_nesting_limit, node, null);
            return false;
        }
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        switch (ast.nodes.items(.tag)[node]) {
            .bit_or => return try self.patternShape(data.lhs, depth + 1) and try self.patternShape(data.rhs, depth + 1),
            .number_literal, .string_literal, .char_literal, .enum_literal => return true,
            .field_access => if (try Literal.qualifiedEnum(self.module, self.ir, node) != null) return true,
            .negation => if (ast.nodes.items(.tag)[data.rhs] == .number_literal) return true,
            .identifier => {
                const name = ast.tokenSlice(ast.nodes.items(.main_token)[node]);
                if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false") or self.wildcard(node)) return true;
            },
            else => {},
        }
        _ = try self.fail(.unsupported_match_pattern, node, null);
        return false;
    }

    // Only a single canonical bool has finite coverage in this bounded slice.
    // Validate even the final pattern: lowering may omit its comparison only
    // after this proof, never merely because it is the last source prong.
    fn booleanPatternCoverage(self: *Context, node: Ast.Node.Index, ty: Ir.TypeId, depth: usize) Error!?u2 {
        if (depth >= max_recursive_depth) {
            _ = try self.fail(.body_nesting_limit, node, null);
            return null;
        }
        const ast = &self.module.source.ast;
        if (ast.nodes.items(.tag)[node] == .bit_or) {
            const data = ast.nodes.items(.data)[node];
            const left = (try self.booleanPatternCoverage(data.lhs, ty, depth + 1)) orelse return null;
            const right = (try self.booleanPatternCoverage(data.rhs, ty, depth + 1)) orelse return null;
            return left | right;
        }
        if (self.wildcard(node)) return 3;
        return switch (try Literal.check(self.module, self.ir, node, ty, null)) {
            .constant => |id| if (self.ir.getConstant(id).?.value.boolean) @as(u2, 2) else @as(u2, 1),
            .failed => null,
            .not_literal => blk: {
                _ = try self.fail(.unsupported_match_pattern, node, null);
                break :blk null;
            },
        };
    }

    const EnumCoverage = struct { fresh: bool, previous: ?Module.Span = null };

    fn enumPatternCoverage(self: *Context, node: Ast.Node.Index, ty: Ir.TypeId, covered: []?Module.Span, depth: usize) Error!?EnumCoverage {
        if (depth >= max_recursive_depth) {
            _ = try self.fail(.body_nesting_limit, node, null);
            return null;
        }
        const ast = &self.module.source.ast;
        if (ast.nodes.items(.tag)[node] == .bit_or) {
            const data = ast.nodes.items(.data)[node];
            const left = (try self.enumPatternCoverage(data.lhs, ty, covered, depth + 1)) orelse return null;
            const right = (try self.enumPatternCoverage(data.rhs, ty, covered, depth + 1)) orelse return null;
            return .{ .fresh = left.fresh or right.fresh, .previous = left.previous orelse right.previous };
        }
        const location = self.irSpan(node);
        const pattern_span: Module.Span = location;
        if (self.wildcard(node)) {
            var coverage: EnumCoverage = .{ .fresh = false };
            for (covered) |*entry| {
                if (entry.*) |previous| {
                    if (coverage.previous == null) coverage.previous = previous;
                } else {
                    entry.* = pattern_span;
                    coverage.fresh = true;
                }
            }
            return coverage;
        }
        switch (try Literal.check(self.module, self.ir, node, ty, null)) {
            .failed => return null,
            .not_literal => {
                _ = try self.fail(.unsupported_match_pattern, node, null);
                return null;
            },
            .constant => |constant| {
                const index = self.ir.getConstant(constant).?.value.enum_tag;
                const previous = covered[index];
                if (previous == null) covered[index] = pattern_span;
                return .{ .fresh = previous == null, .previous = previous };
            },
        }
    }

    // Patterns contain no effects. Comparisons use the subject snapshots, never
    // revisit subject ASTs, and reuse verified short-circuit boolean regions.
    fn matchPattern(self: *Context, node: Ast.Node.Index, subject: MatchSubject, depth: usize) Error!?MatchResult {
        if (depth >= max_recursive_depth) {
            _ = try self.fail(.body_nesting_limit, node, null);
            return null;
        }
        const boolean = try self.ir.internType(.boolean);
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        if (ast.nodes.items(.tag)[node] == .bit_or) {
            const left = (try self.matchPattern(data.lhs, subject, depth + 1)) orelse return null;
            const start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
            const right = (try self.matchPattern(data.rhs, subject, depth + 1)) orelse return null;
            return .{ .ty = boolean, .value = if (self.emitter) |emitter| try emitter.append(boolean, self.irSpan(node), .{ .short_circuit = .{
                .kind = .@"or",
                .lhs = left.value.?,
                .rhs_start = start,
                .rhs = right.value.?,
            } }) else null };
        }
        if (self.tagged(subject.ty) and ast.nodes.items(.tag)[node] == .enum_literal) {
            const index = self.fieldIndex(subject.ty, ast.tokenSlice(ast.nodes.items(.main_token)[node])) orelse {
                _ = try self.fail(.unknown_field, node, null);
                return null;
            };
            return .{ .ty = boolean, .value = if (self.emitter) |emitter| try emitter.append(boolean, self.irSpan(node), .{ .union_tag = .{ .base = subject.value.?, .index = index } }) else null };
        }
        if (ast.nodes.items(.tag)[node] == .enum_literal and self.ir.getType(subject.ty).? != .enumeration) {
            _ = try self.fail(.unsupported_match_pattern, node, null);
            return null;
        }
        const constant = if (self.wildcard(node))
            try self.ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } })
        else switch (try Literal.check(self.module, self.ir, node, subject.ty, null)) {
            .constant => |constant| constant,
            .failed => return null,
            .not_literal => {
                _ = try self.fail(.unsupported_match_pattern, node, null);
                return null;
            },
        };
        var value: ?Ir.ValueId = null;
        if (self.emitter) |emitter| {
            value = try emitter.append(if (self.wildcard(node)) boolean else subject.ty, self.irSpan(node), .{ .constant = constant });
            if (!self.wildcard(node)) value = try emitter.append(boolean, self.irSpan(node), .{ .binary = .{ .op = .eq, .lhs = subject.value.?, .rhs = value.? } });
        }
        return .{ .ty = boolean, .value = value };
    }

    fn bindMatchCapture(self: *Context, prong: Ast.full.MatchCase.Components, subjects: []const MatchSubject) Error!bool {
        const token = prong.capture_token orelse return true;
        const ast = &self.module.source.ast;
        if (subjects.len != 1 or !self.tagged(subjects[0].ty) or prong.values.len != 1 or ast.nodes.items(.tag)[prong.values[0]] != .enum_literal) {
            _ = try self.fail(.invalid_match_capture, prong.target_expr, null);
            return false;
        }
        const name = ast.tokenSlice(token);
        if (std.mem.startsWith(u8, name, "@\"") or std.mem.eql(u8, name, "_")) {
            _ = try self.fail(.invalid_match_capture, prong.target_expr, null);
            return false;
        }
        if (self.conflict(name)) |previous| {
            try self.module.diagnostics.append(self.module.allocator, .{ .tag = .unsupported_shadowing, .span = self.module.tokenSpan(token), .related = previous });
            return false;
        }
        const index = self.fieldIndex(subjects[0].ty, ast.tokenSlice(ast.nodes.items(.main_token)[prong.values[0]])) orelse return false;
        const ty = self.ir.getType(subjects[0].ty).?.record.fields[index].ty;
        const value = if (self.emitter) |emitter| try emitter.append(ty, self.irSpan(prong.target_expr), .{ .union_payload = .{ .base = subjects[0].value.?, .index = index } }) else null;
        try self.bindLocal(name, .{ .known = .{ .ty = ty }, .name_span = self.module.tokenSpan(token), .value = value });
        return true;
    }

    fn matchProngs(self: *Context, owner: Ast.Node.Index, cases: []const Ast.Node.Index, subjects: []const MatchSubject, hint: ?Known, depth: usize) Error!?MatchResult {
        if (depth >= max_recursive_depth) {
            _ = try self.fail(.body_nesting_limit, owner, null);
            return null;
        }
        const ast = &self.module.source.ast;
        const prong = ast.fullMatchCase(cases[0]).?.ast;
        if (cases.len == 1) {
            const start = self.locals.items.len;
            defer self.restoreScope(start);
            if (!try self.bindMatchCapture(prong, subjects)) return null;
            const ty = (try self.expression(prong.target_expr, hint, depth + 1)) orelse return null;
            if (!self.terminated) try self.copyNode(prong.target_expr, ty);
            return .{ .ty = ty, .value = if (self.emitter) |emitter| try emitter.value(prong.target_expr) else null };
        }
        var condition: ?Ir.ValueId = null;
        const boolean = try self.ir.internType(.boolean);
        for (prong.values, subjects, 0..) |pattern, subject, i| {
            const start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
            const part = (try self.matchPattern(pattern, subject, depth + 1)) orelse return null;
            if (self.emitter) |emitter| condition = if (i == 0) part.value.? else try emitter.append(boolean, self.irSpan(pattern), .{ .short_circuit = .{
                .kind = .@"and",
                .lhs = condition.?,
                .rhs_start = start,
                .rhs = part.value.?,
            } });
        }
        const then_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
        const scope = self.locals.items.len;
        if (!try self.bindMatchCapture(prong, subjects)) return null;
        const then_ty = try self.expression(prong.target_expr, hint, depth + 1);
        self.restoreScope(scope);
        if (then_ty == null) return null;
        const then_terminal = self.terminated;
        if (!then_terminal) try self.copyNode(prong.target_expr, then_ty.?);
        const then_value = if (self.emitter) |emitter| try emitter.value(prong.target_expr) else null;
        self.terminated = false;
        const else_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
        const rest = (try self.matchProngs(owner, cases[1..], subjects, hint orelse if (!then_terminal) Known{ .ty = then_ty.? } else null, depth + 1)) orelse return null;
        const else_terminal = self.terminated;
        self.terminated = then_terminal and else_terminal;
        const merged = if (self.terminated) try self.ir.internType(.void) else if (then_terminal) rest.ty else then_ty.?;
        if (!then_terminal and !else_terminal and then_ty.? != rest.ty) {
            _ = try self.fail(.expression_type_mismatch, cases[1], self.span(prong.target_expr));
            return null;
        }
        const value = if (self.emitter) |emitter| try emitter.append(merged, self.irSpan(cases[0]), .{ .branch = .{
            .condition = condition.?,
            .then_start = then_start,
            .else_start = else_start,
            .then_value = then_value.?,
            .else_value = rest.value.?,
            .returns = self.terminated,
        } }) else null;
        if (self.terminated) self.terminal = value;
        return .{ .ty = merged, .value = value };
    }

    fn matchExpression(self: *Context, node: Ast.Node.Index, expected: ?Known, hint: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
        const match = ast.fullMatch(node).?.ast;
        for (match.cases) |case| {
            const patterns = ast.fullMatchCase(case).?.ast.values;
            const fallback = patterns.len == 1 and self.wildcard(patterns[0]);
            if (!fallback and patterns.len != match.subjects.len) return self.fail(.match_pattern_count, case, null);
            for (patterns) |pattern| if (!try self.patternShape(pattern, depth + 1)) return null;
        }
        const subjects = try self.module.allocator.alloc(MatchSubject, match.subjects.len);
        defer self.module.allocator.free(subjects);
        for (match.subjects, subjects) |subject, *snapshot| {
            const ty = (try self.expression(subject, null, depth + 1)) orelse return null;
            if (self.terminated) return if (expected) |want| want.ty else ty;
            const scalar = self.ir.getType(ty).?;
            if (scalar != .boolean and scalar != .integer and scalar != .string and scalar != .rune and !(subjects.len == 1 and (self.tagged(ty) or scalar == .enumeration))) return self.fail(.unsupported_match_subject, subject, null);
            snapshot.* = .{ .ty = ty, .value = if (self.emitter) |emitter| try emitter.value(subject) else null };
        }
        const union_match = subjects.len == 1 and self.tagged(subjects[0].ty);
        if (!union_match) for (match.cases) |case| {
            if (ast.fullMatchCase(case).?.ast.capture_token != null) return self.fail(.invalid_match_capture, case, null);
        };
        if (union_match) {
            const fields = self.ir.getType(subjects[0].ty).?.record.fields;
            const covered = try self.module.allocator.alloc(?Module.Span, fields.len);
            defer self.module.allocator.free(covered);
            @memset(covered, null);
            var fallback: ?Module.Span = null;
            for (match.cases) |case| {
                const prong = ast.fullMatchCase(case).?.ast;
                const pattern = prong.values[0];
                if (fallback) |previous| return self.fail(.match_covered_union_prong, pattern, previous);
                if (self.wildcard(pattern)) {
                    if (prong.capture_token != null) return self.fail(.invalid_match_capture, case, null);
                    var all = true;
                    for (covered) |entry| if (entry == null) {
                        all = false;
                    };
                    if (all) return self.fail(.match_covered_union_prong, pattern, covered[0]);
                    fallback = self.span(pattern);
                    continue;
                }
                if (ast.nodes.items(.tag)[pattern] != .enum_literal) return self.fail(.unsupported_match_pattern, pattern, null);
                const index = self.fieldIndex(subjects[0].ty, ast.tokenSlice(ast.nodes.items(.main_token)[pattern])) orelse return self.fail(.unknown_field, pattern, null);
                if (covered[index]) |previous| return self.fail(.match_covered_union_prong, pattern, previous);
                covered[index] = self.span(pattern);
            }
            if (fallback == null) for (covered) |entry| {
                if (entry == null) return self.fail(.match_missing_variants, node, null);
            };
        } else if (subjects.len == 1 and self.ir.getType(subjects[0].ty).? == .enumeration) {
            const tags = self.ir.getType(subjects[0].ty).?.enumeration.tags;
            const covered = try self.module.allocator.alloc(?Module.Span, tags.len);
            defer self.module.allocator.free(covered);
            @memset(covered, null);
            for (match.cases) |case| {
                const pattern = ast.fullMatchCase(case).?.ast.values[0];
                const coverage = (try self.enumPatternCoverage(pattern, subjects[0].ty, covered, depth + 1)) orelse return null;
                if (!coverage.fresh) return self.fail(.match_covered_enum_prong, pattern, coverage.previous);
            }
            for (covered) |entry| if (entry == null) return self.fail(.match_missing_variants, node, null);
        } else if (subjects.len == 1 and self.ir.getType(subjects[0].ty).? == .boolean) {
            var covered: u2 = 0;
            var first_true: ?Module.Span = null;
            var first_false: ?Module.Span = null;
            for (match.cases) |case| {
                const pattern = ast.fullMatchCase(case).?.ast.values[0];
                const coverage = (try self.booleanPatternCoverage(pattern, subjects[0].ty, depth + 1)) orelse return null;
                if (coverage & ~covered == 0) return self.fail(.match_covered_boolean_prong, pattern, if (coverage & 2 != 0) first_true else first_false);
                if (coverage & 2 != 0 and first_true == null) first_true = self.span(pattern);
                if (coverage & 1 != 0 and first_false == null) first_false = self.span(pattern);
                covered |= coverage;
            }
            if (covered != 3) return self.fail(switch (covered) {
                0 => .match_missing_boolean_values,
                1 => .match_missing_true,
                2 => .match_missing_false,
                3 => unreachable,
            }, node, null);
        } else {
            if (match.cases.len == 0) return self.fail(.match_missing_catchall, node, null);
            for (match.cases, 0..) |case, i| {
                const patterns = ast.fullMatchCase(case).?.ast.values;
                const all = self.catchall(patterns);
                if (all and i + 1 != match.cases.len) return self.fail(.match_catchall_not_last, case, null);
                if (!all and i + 1 == match.cases.len) return self.fail(.match_missing_catchall, case, null);
            }
        }
        const result = (try self.matchProngs(node, match.cases, subjects, hint, depth + 1)) orelse return null;
        if (self.emitter) |emitter| try emitter.values.put(emitter.allocator, node, result.value.?);
        return self.fit(result.ty, expected, node);
    }

    fn interpolationError(self: *Context, tag: Module.Diagnostic.Tag, location: Module.Span) Error!?Ir.TypeId {
        try self.module.diagnostics.append(self.module.allocator, .{ .tag = tag, .span = location });
        return null;
    }

    fn stringChunk(self: *Context, raw: []const u8, location: Module.Span, ty: Ir.TypeId) Error!?Ir.ConstId {
        const bytes = try std.mem.concat(self.module.allocator, u8, &.{ "\"", raw, "\"" });
        defer self.module.allocator.free(bytes);
        var decoded = std.Io.Writer.Allocating.init(self.module.allocator);
        defer decoded.deinit();
        const result = string_literal.parseWrite(&decoded.writer, bytes) catch return error.OutOfMemory;
        const issue: ?Literal.StringIssue = switch (result) {
            .success => Literal.stringIssue(bytes, decoded.written()),
            .failure => |failure| .{ .offset = switch (failure) {
                .empty_char_literal => unreachable,
                inline else => |offset| offset,
            }, .tag = Module.Diagnostic.Tag.invalid_literal },
        };
        if (issue) |bad| {
            const start = location.start + @as(u32, @intCast(bad.offset - 1));
            _ = try self.interpolationError(bad.tag, .{ .source = location.source, .start = start, .end = @min(location.end, start + 1) });
            return null;
        }
        return try self.ir.internConstant(.{ .ty = ty, .value = .{ .string = decoded.written() } });
    }

    fn stringInterpolation(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const location = self.span(node);
        const bytes = self.module.sourceSlice(location).?;
        const string = try self.ir.internType(.string);
        var result: ?Ir.ValueId = null;
        var offset: usize = 1;
        var chunk: usize = 1;
        const data = self.module.source.ast.nodes.items(.data)[node];
        const expressions = self.module.source.ast.extra_data[data.lhs..data.rhs];
        var placeholder: usize = 0;
        while (offset < bytes.len) {
            if (offset < bytes.len - 1 and bytes[offset] == '\\') {
                switch (string_literal.parseEscapeSequence(bytes, &offset)) {
                    .success => continue,
                    .failure => return self.interpolationError(.invalid_literal, location),
                }
            }
            if (offset == bytes.len - 1 or bytes[offset] == '{' or bytes[offset] == '}') {
                if (offset > chunk) {
                    const constant = (try self.stringChunk(bytes[chunk..offset], .{
                        .source = location.source,
                        .start = location.start + @as(u32, @intCast(chunk)),
                        .end = location.start + @as(u32, @intCast(offset)),
                    }, string)) orelse return null;
                    if (self.emitter) |emitter| {
                        const value = try emitter.append(string, self.irSpan(node), .{ .constant = constant });
                        result = if (result) |left| try emitter.append(string, self.irSpan(node), .{ .binary = .{ .op = .add, .lhs = left, .rhs = value } }) else value;
                    }
                }
                if (offset == bytes.len - 1) break;
                const brace = offset;
                offset += 1;
                const start = offset;
                while (offset < bytes.len - 1 and bytes[offset] != '}') : (offset += 1) {}
                if (bytes[brace] != '{' or offset == bytes.len - 1 or placeholder >= expressions.len or expressions[placeholder] == 0)
                    return self.interpolationError(.invalid_interpolation, .{ .source = location.source, .start = location.start + @as(u32, @intCast(brace)), .end = location.start + @as(u32, @intCast(@min(offset + 1, bytes.len - 1))) });
                const embedded = expressions[placeholder];
                placeholder += 1;
                const ty = (try self.expression(embedded, null, depth + 1)) orelse return null;
                if (self.terminated) return self.fit(string, expected, node);
                const scalar = self.ir.getType(ty).?;
                if (scalar != .boolean and scalar != .integer and scalar != .string and scalar != .rune and scalar != .record and scalar != .enumeration) return self.interpolationError(.unsupported_interpolation_type, .{ .source = location.source, .start = location.start + @as(u32, @intCast(start)), .end = location.start + @as(u32, @intCast(offset)) });
                if (!try self.convertPrintable(embedded, ty)) return null;
                if (self.emitter) |emitter| {
                    const converted = try emitter.append(string, self.irSpan(node), .{ .to_string = try emitter.value(embedded) });
                    result = if (result) |left| try emitter.append(string, self.irSpan(node), .{ .binary = .{ .op = .add, .lhs = left, .rhs = converted } }) else converted;
                }
                offset += 1;
                chunk = offset;
            } else offset += 1;
        }
        if (self.emitter) |emitter| try emitter.values.put(emitter.allocator, node, result orelse return error.InvalidValueId);
        return self.fit(string, expected, node);
    }

    fn rangeValue(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        const unit = try self.ir.internType(.void);
        var known = (try self.probe(data.lhs, depth + 1)) orelse try self.probe(data.rhs, depth + 1);
        if (known == null) {
            // Only literal decoding participates in this range-specific default;
            // arithmetic expressions are deliberately not constant-folded here.
            const wide = try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 128 } });
            var negative = false;
            for ([_]Ast.Node.Index{ data.lhs, data.rhs }) |bound| {
                switch (try Literal.check(self.module, self.ir, bound, wide, null)) {
                    .failed => return null,
                    .not_literal => {
                        // A terminal bound needs no iteration type: its control
                        // transfer belongs to the enclosing scope. Also diagnose
                        // unknown names before reporting missing numeric context.
                        if (try self.expression(bound, null, depth + 1) == null) return null;
                        if (self.terminated) return if (expected) |want| want.ty else unit;
                        return self.fail(.literal_needs_type, bound, null);
                    },
                    .constant => |id| negative = negative or self.ir.getConstant(id).?.value.signed_integer < 0,
                }
            }
            known = .{ .ty = try self.ir.internType(.{ .integer = .{ .signedness = if (negative) .signed else .unsigned, .bits = Ir.native_pointer_bits } }) };
        }
        const index_type = known.?.ty;
        if (self.ir.getType(index_type).? != .integer) return self.fail(.operator_not_supported, node, known.?.annotation);
        if (try self.expression(data.lhs, known, depth + 1) == null) return null;
        if (self.terminated) return if (expected) |want| want.ty else unit;
        if (try self.expression(data.rhs, known, depth + 1) == null) return null;
        if (self.terminated) return if (expected) |want| want.ty else unit;
        const ty = try self.ir.internType(.{ .range = index_type });
        if (self.emitter) |emitter| try emitter.record(node, ty, self.irSpan(node), .{ .range_init = .{
            .start = try emitter.value(data.lhs),
            .end = try emitter.value(data.rhs),
            .inclusive = ast.tokens.items(.tag)[ast.nodes.items(.main_token)[node]] == .period_period_equal,
        } });
        return self.fit(ty, expected, node);
    }

    fn rangeExpression(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        const ast = &self.module.source.ast;
        const loop = ast.fullRange(node).?;
        const unit = try self.ir.internType(.void);
        const by_ref = ast.nodes.items(.tag)[loop.binding] == .ref_of;
        const source_type = if (by_ref) reference: {
            if (ast.nodes.items(.tag)[loop.source] != .identifier) return self.fail(.unsupported_iteration, loop.source, null);
            const binding = self.bindings.get(ast.tokenSlice(ast.nodes.items(.main_token)[loop.source])) orelse return self.fail(.unsupported_iteration, loop.source, null);
            if (binding.borrowed or self.ir.getType(binding.known.ty).? != .array) return self.fail(.unsupported_iteration, loop.source, null);
            if (self.reservedOrigin(binding.name_span)) |origin| return self.fail(.borrow_conflict, loop.source, origin);
            // A loan selects the live owner storage, never an ordinary array clone.
            if (self.emitter) |emitter| try emitter.record(loop.source, binding.known.ty, self.irSpan(loop.source), .{ .local_load = binding.slot.? });
            break :reference binding.known.ty;
        } else (try self.expression(loop.source, null, depth + 1)) orelse return null;
        if (self.terminated) return if (expected) |want| want.ty else unit;
        const source = self.ir.getType(source_type).?;
        if (source != .range and source != .array and source != .string) return self.fail(.unsupported_iteration, loop.source, null);
        const sequence = source != .range;
        const index_type = if (sequence) try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } }) else source.range;
        const element_type = switch (source) {
            .array => |ty| ty,
            .string => try self.ir.internType(.rune),
            .range => |ty| ty,
            else => unreachable,
        };
        const known: ?Known = .{ .ty = element_type };
        const owner: ?Binding = if (ast.nodes.items(.tag)[loop.source] == .identifier) self.bindings.get(ast.tokenSlice(ast.nodes.items(.main_token)[loop.source])) else null;
        if (by_ref and (source != .array or owner == null or owner.?.borrowed)) return self.fail(.unsupported_iteration, loop.source, null);
        if (by_ref) {
            if (self.mutationOrigin(owner.?)) |origin| return self.fail(.borrow_conflict, loop.source, origin);
        }
        var iteration_reservation: Reservation = .{ .owner = if (owner) |binding| if (binding.loan_owner) |loan| loan.owner else binding.name_span else self.irSpan(node), .origin = self.irSpan(node), .parent = self.reservation, .reads_allowed = !by_ref };
        const parent_reservation = self.reservation;
        defer self.reservation = parent_reservation;
        var loan: ?Ir.ValueId = null;
        if (by_ref) if (self.emitter) |emitter| {
            loan = try emitter.append(unit, self.irSpan(node), .{ .borrow_reserve = owner.?.slot.? });
        };
        var start_value: Ir.ValueId = undefined;
        var end_value: Ir.ValueId = undefined;
        var inclusive: Ir.ValueId = undefined;
        if (self.emitter) |emitter| {
            const value = try emitter.value(loop.source);
            if (sequence) {
                start_value = try emitter.append(index_type, self.irSpan(node), .{ .constant = try self.ir.internConstant(.{ .ty = index_type, .value = .{ .signed_integer = 0 } }) });
                end_value = try emitter.append(index_type, self.irSpan(node), .{ .iteration_len = value });
                inclusive = try emitter.append(try self.ir.internType(.boolean), self.irSpan(node), .{ .constant = try self.ir.internConstant(.{ .ty = try self.ir.internType(.boolean), .value = .{ .boolean = false } }) });
            } else {
                start_value = try emitter.append(index_type, self.irSpan(node), .{ .range_start = value });
                end_value = try emitter.append(index_type, self.irSpan(node), .{ .range_end = value });
                inclusive = try emitter.append(try self.ir.internType(.boolean), self.irSpan(node), .{ .range_inclusive = value });
            }
        }
        const capture = if (by_ref) ast.nodes.items(.data)[loop.binding].rhs else loop.binding;
        const binding_token = ast.nodes.items(.main_token)[capture];
        const name = ast.tokenSlice(binding_token);
        const discard = std.mem.eql(u8, name, "_");
        if (std.mem.startsWith(u8, name, "@\"")) return self.fail(.unsupported_name, node, null);
        const related = self.conflict(name);
        if (!discard and (related != null or isPrimitive(name))) {
            try self.module.diagnostics.append(self.module.allocator, .{ .tag = .unsupported_shadowing, .span = self.module.tokenSpan(binding_token), .related = related });
            return null;
        }
        const scope = self.locals.items.len;
        defer self.restoreScope(scope);
        var context: LoopContext = .{
            .allows_value = loop.else_expr != 0,
            .result = if (loop.else_expr != 0) expected orelse try self.probe(node, depth) else .{ .ty = unit },
        };
        const parent = self.loop_context;
        self.loop_context = &context;
        defer self.loop_context = parent;
        var counter: Ir.ValueId = undefined;
        var condition: Ir.ValueId = undefined;
        var current: ?Ir.ValueId = null;
        var condition_start: u32 = 0;
        var done: ?Ir.ValueId = null;
        const boolean = try self.ir.internType(.boolean);
        if (self.emitter) |emitter| {
            counter = try emitter.append(index_type, self.irSpan(node), .{ .local_slot = start_value });
            if (!sequence) {
                const initial = try emitter.append(boolean, self.irSpan(node), .{ .constant = try self.ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = false } }) });
                done = try emitter.append(boolean, self.irSpan(node), .{ .local_slot = initial });
            }
            condition_start = @intCast(emitter.instructions.items.len);
            const active = if (done) |slot| active: {
                const loaded = try emitter.append(boolean, self.irSpan(node), .{ .local_load = slot });
                break :active try emitter.append(boolean, self.irSpan(node), .{ .unary = .{ .op = .bool_not, .operand = loaded } });
            } else null;
            const rhs_start: u32 = @intCast(emitter.instructions.items.len);
            const loaded = try emitter.append(index_type, self.irSpan(node), .{ .local_load = counter });
            const less = try emitter.append(boolean, self.irSpan(node), .{ .binary = .{ .op = .lt, .lhs = loaded, .rhs = end_value } });
            condition = less;
            if (!sequence) {
                const equal = try emitter.append(boolean, self.irSpan(node), .{ .binary = .{ .op = .eq, .lhs = loaded, .rhs = end_value } });
                const equal_inclusive = try emitter.append(boolean, self.irSpan(node), .{ .branch = .{ .condition = equal, .then_start = @intCast(emitter.instructions.items.len), .else_start = @intCast(emitter.instructions.items.len), .then_value = inclusive, .else_value = equal } });
                condition = try emitter.append(boolean, self.irSpan(node), .{ .branch = .{ .condition = less, .then_start = @intCast(emitter.instructions.items.len), .else_start = @intCast(emitter.instructions.items.len), .then_value = less, .else_value = equal_inclusive } });
            }
            if (active) |lhs| condition = try emitter.append(boolean, self.irSpan(node), .{ .short_circuit = .{ .kind = .@"and", .lhs = lhs, .rhs_start = rhs_start, .rhs = condition } });
        }
        const body_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
        var element_loan: ?Ir.ValueId = null;
        if (self.emitter) |emitter| {
            const index = try emitter.append(index_type, self.irSpan(node), .{ .local_load = counter });
            if (by_ref) {
                element_loan = try emitter.append(unit, self.irSpan(node), .{ .borrow_start = .{ .reservation = loan.?, .index = index } });
            } else current = if (source == .array)
                try emitter.append(element_type, self.irSpan(node), .{ .array_index = .{ .array = try emitter.value(loop.source), .index = index } })
            else if (source == .string)
                try emitter.append(element_type, self.irSpan(node), .{ .string_decode = .{ .array = try emitter.value(loop.source), .index = index } })
            else
                index;
        }
        if (!discard) try self.bindLocal(name, .{ .known = known.?, .name_span = self.module.tokenSpan(binding_token), .value = current, .mutable = by_ref and owner.?.mutable, .borrowed = by_ref, .borrow = element_loan, .loan_owner = if (by_ref) .{ .owner = owner.?.name_span, .origin = self.irSpan(node) } else null });
        if (source == .array and owner != null) self.reservation = &iteration_reservation;
        if (loop.else_expr != 0 and context.result == null) context.result = try self.probeBreaks(loop.body, depth + 1);
        if (try self.expression(loop.body, .{ .ty = unit }, depth + 1) == null) return null;
        self.terminated = false;
        var continuation_start: ?u32 = null;
        var continuation: ?Ir.ValueId = null;
        if (self.emitter) |emitter| {
            continuation_start = @intCast(emitter.instructions.items.len);
            const old = try emitter.append(index_type, self.irSpan(node), .{ .local_load = counter });
            var at_end: ?Ir.ValueId = null;
            var then_start: u32 = 0;
            var exhausted: ?Ir.ValueId = null;
            if (done) |slot| {
                at_end = try emitter.append(boolean, self.irSpan(node), .{ .binary = .{ .op = .eq, .lhs = old, .rhs = end_value } });
                then_start = @intCast(emitter.instructions.items.len);
                const yes = try emitter.append(boolean, self.irSpan(node), .{ .constant = try self.ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } }) });
                exhausted = try emitter.append(unit, self.irSpan(node), .{ .local_store = .{ .slot = slot, .value = yes } });
            }
            const increment_start: u32 = @intCast(emitter.instructions.items.len);
            const one = try emitter.append(index_type, self.irSpan(node), .{ .constant = try self.ir.internConstant(.{ .ty = index_type, .value = if (self.ir.getType(index_type).?.integer.signedness == .signed) .{ .signed_integer = 1 } else .{ .unsigned_integer = 1 } }) });
            // Only current < end <= MAX reaches the increment. Endpoint completion
            // sets done instead of a source break, preserving exhaustion routing.
            const next = if (source == .string)
                try emitter.append(index_type, self.irSpan(node), .{ .string_next = .{ .array = try emitter.value(loop.source), .index = old } })
            else
                try emitter.append(index_type, self.irSpan(node), .{ .binary = .{ .op = .add, .lhs = old, .rhs = one } });
            continuation = try emitter.append(unit, self.irSpan(node), .{ .local_store = .{ .slot = counter, .value = next } });
            if (at_end) |test_value| continuation = try emitter.append(unit, self.irSpan(node), .{ .branch = .{
                .condition = test_value,
                .then_start = then_start,
                .else_start = increment_start,
                .then_value = exhausted.?,
                .else_value = continuation,
            } });
        }
        self.restoreScope(scope);
        self.loop_context = parent;
        self.reservation = parent_reservation;
        const else_start: ?u32 = if (loop.else_expr != 0 and self.emitter != null) @intCast(self.emitter.?.instructions.items.len) else null;
        const exhaustion = if (loop.else_expr != 0) try self.expression(loop.else_expr, context.result, depth + 1) else unit;
        if (exhaustion == null) return null;
        const else_terminal = self.terminated;
        if (loop.else_expr != 0 and !else_terminal) {
            if (context.result == null) context.result = .{ .ty = exhaustion.? };
            try self.copyNode(loop.else_expr, exhaustion.?);
        }
        self.terminated = loop.else_expr != 0 and else_terminal and !context.has_break;
        const result = if (self.terminated) unit else if (context.result) |value| value.ty else unit;
        if (self.emitter) |emitter| {
            try emitter.record(node, result, self.irSpan(node), .{ .loop = .{
                .condition_start = condition_start,
                .body_start = body_start,
                .continuation_start = continuation_start,
                .else_start = else_start,
                .else_value = if (loop.else_expr != 0) try emitter.value(loop.else_expr) else null,
                .returns = self.terminated,
                .condition = condition,
                .body = try emitter.value(loop.body),
                .continuation = continuation,
            } });
            if (self.terminated) self.terminal = try emitter.value(node);
        }
        return self.fit(result, expected, node);
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
        const ast_early = &self.module.source.ast;
        const tag_early = ast_early.nodes.items(.tag)[node];
        const base_early = ast_early.nodes.items(.data)[node].lhs;
        if ((tag_early == .array_access or tag_early == .container_decl_arg) and
            (ast_early.nodes.items(.tag)[base_early] == .enum_literal or try self.qualifiedUnion(base_early) != null))
            return self.unionInit(node, expected, depth);
        // Explicit record constructors already carry their type. Resolve it once,
        // rather than probing and then retrying a failed concrete specialization.
        if (tag_early == .container_decl_arg) return self.recordInit(node, expected, depth);
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
        const ast = &self.module.source.ast;
        const data = ast.nodes.items(.data)[node];
        switch (ast.nodes.items(.tag)[node]) {
            .ref_of => return self.fail(.unsupported_borrow, node, null),
            .enum_literal => return self.fail(.enum_needs_type, node, null),
            .unwrap => return self.propagate(node, expected, depth),
            .match => return self.matchExpression(node, expected, hint, depth),
            .function_pipe => {
                const unit = try self.ir.internType(.void);
                if (self.printing(data.rhs) != null) {
                    const arg = (try self.expression(data.lhs, null, depth + 1)) orelse return null;
                    if (!self.terminated and !try self.convertPrintable(data.lhs, arg)) return null;
                    return self.fit(unit, expected, node);
                }
                // Exactly rhs(lhs): evaluate the callee before the sole argument.
                const callee = (try self.expression(data.rhs, null, depth + 1)) orelse return null;
                if (self.terminated) return if (expected) |want| want.ty else unit;
                const ty = self.ir.getType(callee).?;
                if (ty != .function) return self.fail(.not_callable, data.rhs, null);
                if (ty.function.params.len != 1) return self.fail(.argument_count_mismatch, node, null);
                if (try self.expression(data.lhs, .{ .ty = ty.function.params[0] }, depth + 1) == null) return null;
                if (!self.terminated) try self.copyNode(data.lhs, ty.function.params[0]);
                return self.finishCall(node, data.rhs, ty.function.result, expected);
            },
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
                if (try self.staticMethod(node)) |id| {
                    const known = (try self.globalType(id)) orelse return self.fail(.unresolved_value_type, node, null);
                    return self.fit(known.ty, expected, node);
                }
                const previous_base = self.borrow_base;
                self.borrow_base = if (ast.nodes.items(.tag)[node] == .field_access) data.lhs else null;
                const base = (try self.expression(data.lhs, null, depth + 1)) orelse {
                    self.borrow_base = previous_base;
                    return null;
                };
                self.borrow_base = previous_base;
                if (self.terminated) return if (expected) |want| want.ty else base;
                const array = self.ir.getType(base).?;
                if (array == .record and ast.nodes.items(.tag)[node] == .field_access) {
                    if (array.record.tagged) return self.fail(.unsupported_body_expression, node, null);
                    const index = self.fieldIndex(base, ast.tokenSlice(data.rhs)) orelse return self.fail(.unknown_field, node, null);
                    return self.fit(array.record.fields[index].ty, expected, node);
                }
                if (array != .array and array != .string) return self.fail(.unsupported_body_expression, node, null);
                if (ast.nodes.items(.tag)[node] == .field_access) {
                    if (!std.mem.eql(u8, ast.tokenSlice(data.rhs), "len")) return self.fail(.unsupported_body_expression, node, null);
                    return self.fit(try self.ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } }), expected, node);
                }
                const index = try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } });
                if (try self.expression(data.rhs, .{ .ty = index }, depth + 1) == null) return null;
                const element = if (array == .string) try self.ir.internType(.rune) else array.array;
                if (self.terminated) return if (expected) |want| want.ty else element;
                return self.fit(element, expected, node);
            },
            .identifier => {
                const name = self.module.sourceSlice(self.span(node)).?;
                if (self.bindings.get(name)) |binding| {
                    if (self.reservedOrigin(binding.name_span)) |origin| return self.fail(.borrow_conflict, node, origin);

                    return self.fit(binding.known.ty, expected, node);
                }
                if (self.valueDeclaration(name)) |id| {
                    const declaration = self.getDeclaration(id);
                    if (isPrimitive(name)) return self.fail(.unsupported_shadowing, node, declaration.name_span);
                    if (declaration.is_type_alias) return self.fail(.not_a_value, node, declaration.name_span);
                    const known = (try self.globalType(id)) orelse return self.fail(.unresolved_value_type, node, declaration.name_span);
                    return self.fit(known.ty, expected, node);
                }
                if (std.mem.startsWith(u8, name, "@\"")) return self.fail(.unsupported_name, node, null);
                return self.fail(if (isPrimitive(name)) .unsupported_body_expression else .unknown_name, node, null);
            },
            .@"break", .@"continue" => {
                const is_break = ast.nodes.items(.tag)[node] == .@"break";
                if (data.lhs != 0) return self.fail(.unsupported_loop_control, node, null);
                const loop = self.loop_context orelse return self.fail(.loop_control_outside_loop, node, null);
                const unit = try self.ir.internType(.void);
                if (is_break) {
                    if (data.rhs != 0) {
                        if (!loop.allows_value) return self.fail(.conditional_value_break, node, null);
                        const ty = (try self.expression(data.rhs, loop.result, depth + 1)) orelse return null;
                        if (self.terminated) return if (expected) |want| want.ty else unit;
                        if (loop.result == null) loop.result = .{ .ty = ty };
                        loop.valued_break = self.ir.getType(ty).? != .void or loop.valued_break;
                        try self.copyNode(data.rhs, ty);
                    } else {
                        if (loop.result) |want| {
                            if (want.ty != unit) return self.fail(.expression_type_mismatch, node, want.annotation);
                        } else loop.result = .{ .ty = unit };
                    }
                    loop.has_break = true;
                }
                if (self.emitter) |emitter| {
                    try emitter.record(node, unit, self.irSpan(node), if (is_break) .{ .loop_break = if (data.rhs != 0) try emitter.value(data.rhs) else null } else .loop_continue);
                    self.terminal = try emitter.value(node);
                }
                self.terminated = true;
                return if (expected) |want| want.ty else unit;
            },
            .slice => {
                const bounds = ast.slice(node).ast;
                if (ast.tokens.items(.tag)[ast.nodes.items(.main_token)[data.rhs]] == .period_period_equal)
                    return self.fail(.inclusive_string_slice, data.rhs, null);
                const base = (try self.expression(bounds.sliced, null, depth + 1)) orelse return null;
                if (self.terminated) return if (expected) |want| want.ty else base;
                if (self.ir.getType(base).? != .string) return self.fail(.string_slice_requires_string, bounds.sliced, null);
                const index = try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } });
                for ([_]Ast.Node.Index{ bounds.start, bounds.end }) |bound| {
                    if (bound == 0) continue;
                    if (try self.expression(bound, .{ .ty = index }, depth + 1) == null) return null;
                    if (self.terminated) return if (expected) |want| want.ty else base;
                }
                return self.fit(base, expected, node);
            },
            .range => return self.rangeValue(node, expected, depth),
            .loop_range => return self.rangeExpression(node, expected, depth),
            .loop_condition, .loop_cont, .loop_else => {
                const loop = ast.fullLoop(node).?;
                const unit = try self.ir.internType(.void);
                const boolean = try self.ir.internType(.boolean);
                var context: LoopContext = .{ .allows_value = loop.condition == 0 or loop.else_expr != 0, .result = if (loop.condition == 0 or loop.else_expr != 0) hint else .{ .ty = unit } };
                const parent = self.loop_context;
                self.loop_context = &context;
                defer self.loop_context = parent;
                const condition_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
                const condition = if (loop.condition != 0) try self.expression(loop.condition, .{ .ty = boolean }, depth + 1) else boolean;
                // A terminal condition never reaches the body or exhaustion.
                // Its owned breaks can still complete this loop; continues only
                // restart the condition. Snapshot before checking other regions.
                const condition_terminal = self.terminated and !context.has_break;
                const condition_value: ?Ir.ValueId = if (self.emitter) |emitter|
                    if (condition == null) null else if (loop.condition != 0) try emitter.value(loop.condition) else try emitter.append(boolean, self.irSpan(node), .{
                        .constant = try self.ir.internConstant(.{ .ty = boolean, .value = .{ .boolean = true } }),
                    })
                else
                    null;
                self.terminated = false;
                const body_start: u32 = if (self.emitter) |emitter| @intCast(emitter.instructions.items.len) else 0;
                const body = try self.expression(loop.body, .{ .ty = unit }, depth + 1);
                self.terminated = false;
                const continuation_start: ?u32 = if (loop.continuation != 0 and self.emitter != null) @intCast(self.emitter.?.instructions.items.len) else null;
                const continuation = if (loop.continuation != 0) try self.expression(loop.continuation, .{ .ty = unit }, depth + 1) else unit;
                self.terminated = false;
                if (condition == null or body == null or continuation == null) return null;
                // Exhaustion is a sibling value region, but not part of this
                // loop's break/continue scope. Its controls belong to the parent.
                self.loop_context = parent;
                const else_start: ?u32 = if (loop.else_expr != 0 and self.emitter != null) @intCast(self.emitter.?.instructions.items.len) else null;
                const exhaustion = if (loop.else_expr != 0) try self.expression(loop.else_expr, context.result, depth + 1) else unit;
                if (exhaustion == null) return null;
                const else_terminal = self.terminated;
                if (loop.else_expr != 0 and !else_terminal) {
                    if (context.result == null) context.result = .{ .ty = exhaustion.? };
                    try self.copyNode(loop.else_expr, exhaustion.?);
                }
                self.terminated = condition_terminal or ((loop.condition == 0 or (loop.else_expr != 0 and else_terminal)) and !context.has_break);
                const result = if (self.terminated) unit else if (context.result) |known| known.ty else unit;
                if (loop.else_expr == 0 and result != unit and !context.valued_break) return self.fail(.loop_missing_value_break, node, null);
                if (self.emitter) |emitter| try emitter.record(node, result, self.irSpan(node), .{ .loop = .{
                    .unconditional = loop.condition == 0,
                    .condition_start = condition_start,
                    .body_start = body_start,
                    .continuation_start = continuation_start,
                    .else_start = else_start,
                    .else_value = if (loop.else_expr != 0) try emitter.value(loop.else_expr) else null,
                    .returns = self.terminated,
                    .condition = condition_value.?,
                    .body = try emitter.value(loop.body),
                    .continuation = if (loop.continuation != 0) try emitter.value(loop.continuation) else null,
                } });
                if (self.terminated) if (self.emitter) |emitter| {
                    self.terminal = try emitter.value(node);
                };
                return self.fit(result, expected, node);
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
                if (ast.nodes.items(.tag)[data.lhs] == .field_access) return self.fieldAssignment(node, expected, depth);
                if (ast.nodes.items(.tag)[data.lhs] == .array_access) return self.elementAssignment(node, expected, depth);
                if (ast.nodes.items(.tag)[data.lhs] != .identifier) return self.fail(.unsupported_assignment_target, data.lhs, null);
                const name = self.module.sourceSlice(self.span(data.lhs)).?;
                const binding = self.bindings.get(name) orelse {
                    if (self.module.lookup(name)) |id| return self.fail(.immutable_assignment, data.lhs, self.module.getDeclaration(id).?.name_span);
                    return self.fail(.unknown_name, data.lhs, null);
                };
                if (self.mutationOrigin(binding)) |origin| return self.fail(.borrow_conflict, data.lhs, origin);

                const reservation: Reservation = .{ .owner = if (binding.loan_owner) |loan| loan.owner else binding.name_span, .origin = if (binding.loan_owner) |loan| loan.origin else self.irSpan(node), .parent = self.reservation };
                const parent = self.reservation;
                if (binding.borrowed) self.reservation = &reservation;
                defer self.reservation = parent;
                if (!binding.mutable) return self.fail(.immutable_assignment, data.lhs, binding.name_span);
                const tag = ast.nodes.items(.tag)[node];
                const compound = tag != .assign;
                const ty = self.ir.getType(binding.known.ty).?;
                const array_append = tag == .assign_add and ty == .array;
                const string_concat = tag == .assign_add and ty == .string;
                if (compound and !array_append and !string_concat and ty != .integer) return self.fail(.operator_not_supported, node, binding.known.annotation);
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
                        if (binding.borrowed) {
                            try emitter.record(node, unit, self.irSpan(node), .{ .borrow_store = .{ .borrow = binding.borrow.?, .value = value, .path = &.{} } });
                        } else try emitter.record(node, unit, self.irSpan(node), .{ .local_store = .{ .slot = binding.slot.?, .value = value } });
                    }
                }
                return self.fit(unit, expected, node);
            },
            .number_literal => return self.fail(.literal_needs_type, node, null),
            .string_literal => return self.stringInterpolation(node, expected, depth),
            .char_literal => unreachable, // Checked by Literal.check with its inferred rune type.
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
            .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor => {
                const integer_context = if (expected) |want| (if (self.ir.getType(want.ty).? == .integer) expected else null) else null;
                const operand = (try self.probe(data.lhs, depth + 1)) orelse (try self.probe(data.rhs, depth + 1)) orelse integer_context orelse return self.missingOperandType(node, data.lhs, data.rhs, expected, depth);
                if (self.ir.getType(operand.ty).? != .integer and !(self.ir.getType(operand.ty).? == .string and ast.nodes.items(.tag)[node] == .add)) return self.fail(.operator_not_supported, node, operand.annotation);
                const lhs = try self.expression(data.lhs, operand, depth + 1);
                const rhs = try self.expression(data.rhs, operand, depth + 1);
                if (lhs == null or rhs == null) return null;
                return self.fit(operand.ty, expected, node);
            },
            .in => {
                const right = try self.probe(data.rhs, depth + 1);
                const left = try self.probe(data.lhs, depth + 1);
                const element: Known = if (right) |known| blk: {
                    const array = self.ir.getType(known.ty).?;
                    if (array != .array) return self.fail(.operator_not_supported, node, known.annotation);
                    break :blk .{ .ty = array.array, .annotation = known.annotation };
                } else if (left) |known| blk: {
                    if (!Ir.isArrayElement(self.ir.getType(known.ty).?)) return self.fail(.operator_not_supported, node, known.annotation);
                    break :blk known;
                } else {
                    // Diagnose the operands without letting the boolean result
                    // supply an integer default or an array element type.
                    if (try self.expression(data.lhs, null, depth + 1) == null) return null;
                    if (try self.expression(data.rhs, null, depth + 1) == null) return null;
                    return self.fail(.operator_not_supported, node, null);
                };
                if (self.ir.getType(element.ty).? == .record) return self.fail(.operator_not_supported, node, element.annotation);
                if (try self.expression(data.lhs, element, depth + 1) == null) return null;
                const array: Known = .{ .ty = try self.ir.internType(.{ .array = element.ty }), .annotation = element.annotation };
                if (try self.expression(data.rhs, array, depth + 1) == null) return null;
                return self.fit(try self.ir.internType(.boolean), expected, node);
            },
            .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => {
                const operand = (try self.probe(data.lhs, depth + 1)) orelse (try self.probe(data.rhs, depth + 1)) orelse return self.missingOperandType(node, data.lhs, data.rhs, null, depth);
                const ty = self.ir.getType(operand.ty).?;
                const equality = ast.nodes.items(.tag)[node] == .equal_equal or ast.nodes.items(.tag)[node] == .bang_equal;
                if (ty != .integer and !((ty == .boolean or ty == .string or ty == .rune or ty == .enumeration) and equality)) return self.fail(.operator_not_supported, node, operand.annotation);
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
                if (self.isAscription(call.ast.fn_expr)) {
                    if (call.ast.args.len != 2) return self.fail(.argument_count_mismatch, node, null);
                    const ascribed = (try self.ascribedType(call)) orelse return null;
                    if (try self.expression(call.ast.args[1], ascribed, depth + 1) == null) return null;
                    return self.fit(ascribed.ty, expected, node);
                }
                if (self.isAssertion(call.ast.fn_expr)) {
                    if (call.ast.args.len < 1 or call.ast.args.len > 2) return self.fail(.argument_count_mismatch, node, null);
                    const unit = try self.ir.internType(.void);
                    for (call.ast.args, 0..) |arg, index| {
                        const argument_type = try self.ir.internType(if (index == 0) .boolean else .string);
                        if (try self.expression(arg, .{ .ty = argument_type }, depth + 1) == null) return null;
                        if (self.terminated) return if (expected) |want| want.ty else unit;
                    }
                    if (self.emitter) |emitter| {
                        const start = self.span(call.ast.fn_expr).start;
                        var line: usize = 1;
                        var line_start: usize = 0;
                        for (ast.source[0..start], 0..) |byte, index| {
                            if (byte == '\n') {
                                line += 1;
                                line_start = index + 1;
                            }
                        }
                        const location = try std.fmt.allocPrint(emitter.allocator, "{s}:{d}:{d}", .{ self.module.source.path, line, start - line_start + 1 });
                        try emitter.record(node, unit, self.irSpan(node), .{ .assertion = .{
                            .condition = try emitter.value(call.ast.args[0]),
                            .message = if (call.ast.args.len == 2) try emitter.value(call.ast.args[1]) else null,
                            .location = location,
                        } });
                    }
                    return self.fit(unit, expected, node);
                }
                if (self.isPanic(call.ast.fn_expr)) {
                    if (call.ast.args.len != 1) return self.fail(.argument_count_mismatch, node, null);
                    const string = try self.ir.internType(.string);
                    if (try self.expression(call.ast.args[0], .{ .ty = string }, depth + 1) == null) return null;
                    if (!self.terminated) {
                        const unit = try self.ir.internType(.void);
                        if (self.emitter) |emitter| {
                            try emitter.record(node, unit, self.irSpan(node), .{ .panic = try emitter.value(call.ast.args[0]) });
                            self.terminal = try emitter.value(node);
                        }
                        self.terminated = true;
                    }
                    return if (expected) |want| want.ty else try self.ir.internType(.void);
                }
                if (self.printing(call.ast.fn_expr) != null) {
                    var valid = true;
                    for (call.ast.args) |arg| {
                        const arg_type = (try self.expression(arg, null, depth + 1)) orelse {
                            valid = false;
                            continue;
                        };
                        if (!self.terminated and !try self.convertPrintable(arg, arg_type)) valid = false;
                    }
                    if (!valid) return null;
                    return self.fit(try self.ir.internType(.void), expected, node);
                }
                if (ast.nodes.items(.tag)[call.ast.fn_expr] == .field_access and try self.staticMethod(call.ast.fn_expr) == null) {
                    const field = ast.nodes.items(.data)[call.ast.fn_expr];
                    const receiver = (try self.expression(field.lhs, null, depth + 1)) orelse return null;
                    if (self.terminated) return if (expected) |want| want.ty else try self.ir.internType(.void);
                    if (self.receiverMethod(receiver, ast.tokenSlice(field.rhs))) |method| {
                        const signature = (try self.globalType(method)) orelse return self.fail(.unresolved_value_type, call.ast.fn_expr, null);
                        const function = self.ir.getType(signature.ty).?.function;
                        const related = self.getDeclaration(method).name_span;
                        if (function.params.len == 0 or function.params[0] != receiver) return self.fail(.invalid_method_receiver, call.ast.fn_expr, related);
                        if (call.ast.args.len != function.params.len - 1) return self.fail(.argument_count_mismatch, node, related);
                        try self.copyNode(field.lhs, receiver);
                        var valid = true;
                        for (call.ast.args, function.params[1..]) |arg, param| {
                            if (try self.expression(arg, .{ .ty = param, .annotation = related }, depth + 1) == null) {
                                valid = false;
                            } else if (!self.terminated) try self.copyNode(arg, param);
                        }
                        if (!valid) return null;
                        if (!self.terminated) {
                            const is_noreturn = self.getDeclaration(method).noreturn;
                            if (self.emitter) |emitter| {
                                const callee = try emitter.append(signature.ty, self.irSpan(call.ast.fn_expr), .{ .function_ref = emitter.functions[@backingInt(method)].? });
                                const args = try emitter.allocator.alloc(Ir.ValueId, function.params.len);
                                args[0] = try emitter.value(field.lhs);
                                for (call.ast.args, args[1..]) |arg, *value| value.* = try emitter.value(arg);
                                try emitter.record(node, function.result, self.irSpan(node), .{ .call = .{ .callee = callee, .args = args, .noreturn = is_noreturn } });
                                if (is_noreturn) self.terminal = try emitter.value(node);
                            }
                            self.terminated = is_noreturn;
                        }
                        return self.fit(function.result, expected, node);
                    }
                    // All supported fields are non-callable. Do not reinterpret a
                    // matching field (including array/string len) as a method.
                    const container = self.ir.getType(receiver).?;
                    if (self.fieldIndex(receiver, ast.tokenSlice(field.rhs)) != null or
                        ((container == .array or container == .string) and std.mem.eql(u8, ast.tokenSlice(field.rhs), "len")))
                        return self.fail(.not_callable, node, null);
                    if (container == .record or container == .enumeration) return self.fail(.unknown_field, call.ast.fn_expr, null);
                    return self.fail(.unsupported_body_expression, call.ast.fn_expr, null);
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
                return self.finishCall(node, call.ast.fn_expr, ty.function.result, expected);
            },
            else => return self.fail(.unsupported_body_expression, node, null),
        }
    }
};

test "instance calls diagnose signatures arity exact identity and preserve fields" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let R=struct{let f:()->bool=true\n}\nlet main:()={let r=R[]\nr.f()}", .tag = .invalid_method_receiver },
        .{ .source = "let R=struct{let f:(n:int)->bool=true\n}\nlet main:()={let r=R[]\nr.f()}", .tag = .invalid_method_receiver },
        .{ .source = "let S=struct{}\nlet R=struct{let f:(s:S)->bool=true\n}\nlet main:()={let r=R[]\nr.f()}", .tag = .invalid_method_receiver },
        .{ .source = "let R=struct{let f:(r:R,n:int)->bool=true\n}\nlet main:()={let r=R[]\nr.f()}", .tag = .argument_count_mismatch },
        .{ .source = "let R=struct{let f:(r:R,n:int)->bool=true\n}\nlet main:()={let r=R[]\nr.f(true)}", .tag = .literal_type_mismatch },
        .{ .source = "let R=struct{}\nlet f:(r:R)->bool=true\nlet main:()={let r=R[]\nr.f()}", .tag = .unknown_field },
        .{ .source = "let R=struct{f:bool=true,}\nlet main:()={let r=R[]\nr.f()}", .tag = .not_callable },
        .{ .source = "let main:()={let s=\"hi\"\ns.len()}", .tag = .not_callable },
        .{ .source = "let main:()={let a:[]int=[1]\na.len()}", .tag = .not_callable },
        .{ .source = "let main:()={let s=\"hi\"\ns.close()}", .tag = .unsupported_body_expression },
        .{ .source = "let main:()={let a:[]int=[1]\na.close()}", .tag = .unsupported_body_expression },
        .{ .source = "let R=struct{let f:(r:R)->bool=true\n}\nlet main:()={let r=R[]\nlet f=r.f\n}", .tag = .unknown_field },
        .{ .source = "let K=enum{x,let f:(k:K)->bool=true\n}\nlet main:()={let k=K.x\nlet f=k.f\n}", .tag = .unsupported_body_expression },
        .{ .source = "let K=enum{x,let eql:(a:K,b:K)->bool=a==b\n}\nlet S=enum{x,}\nlet main:()={let k=K.x\nk.eql(S.x)}", .tag = .literal_type_mismatch },
        .{ .source = "let R=struct{@[noreturn]\nlet stop:(r:R)=@panic(\"x\")\n}\nlet main:()={R[].stop()\n@print()}", .tag = .unreachable_code },
        .{ .source = "let R=struct{@[noreturn]\nlet stop:(r:R)=@panic(\"x\")\n}\n@[noreturn]\nlet main:()={let f=R.stop\nf(R[])}", .tag = .noreturn_completion },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "methods.nov", case.source);
        defer module.deinit();
        errdefer std.debug.print("instance parse {s}: {any} {any}\n", .{ case.source, module.source.ast.errors, module.diagnostics.items });
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        errdefer std.debug.print("instance source {s}: {any}\n", .{ case.source, module.diagnostics.items });
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "noreturn declarations reject nonvoid results returns and normal completion" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "@[noreturn]\nlet f:()->int=@panic(\"x\")", .tag = .noreturn_result },
        .{ .source = "@[noreturn]\nlet f:()={}", .tag = .noreturn_completion },
        .{ .source = "@[noreturn]\nlet f:()=return", .tag = .noreturn_return },
        .{ .source = "@[noreturn]\nlet f:()=return @panic(\"x\")", .tag = .noreturn_return },
        .{ .source = "@[noreturn]\nlet f:()=if true {@panic(\"x\")}", .tag = .noreturn_completion },
        .{ .source = "@[noreturn]\nlet f:()=loop {break}", .tag = .noreturn_completion },
        .{ .source = "@[noreturn]\nlet f:()=loop true {}", .tag = .noreturn_completion },
        .{ .source = "@[noreturn]\nlet f:(g:()->void)=g()", .tag = .noreturn_completion },
        .{ .source = "@[noreturn]\nlet stop:()=@panic(\"x\")\n@[noreturn]\nlet f:()={let g=stop\ng()}", .tag = .noreturn_completion },
        .{ .source = "let f:()->noreturn=@panic(\"x\")", .tag = .unknown_type },
        .{ .source = "let f:()->int=@print()", .tag = .expression_type_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "noreturn.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        errdefer std.debug.print("noreturn source {s}: {any}\n", .{ case.source, module.diagnostics.items });
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "noreturn attributes require unique argumentless function placement" {
    for ([_][]const u8{
        "@[noreturn]\n@[noreturn]\nlet f:()=loop {}",
        "@[noreturn(1)]\nlet f:()=loop {}",

        "@[noreturn]\nlet n:int=1",
        "@[noreturn]\nlet A=struct{}",
        "@[noreturn]\nlet mut f:()=loop {}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "attributes.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_attribute, module.diagnostics.items[0].tag);
    }
}

test "terminal loop conditions preserve termination without swallowing owned controls" {
    const prelude = "@[noreturn]\nlet stop:()=@panic(\"stop\")\n";
    const Case = struct { body: []const u8, annotation: []const u8 = "@[noreturn]\n", tag: ?Module.Diagnostic.Tag = null };
    for ([_]Case{
        .{ .body = "loop @panic(\"stop\") {}" },
        .{ .body = "loop stop() {}" },
        .{ .body = "loop {if true {@panic(\"stop\")} else {stop()}} {}" },
        .{ .body = "loop stop() : @print(\"bad\") {break} else {@print(\"bad\")}" },
        .{ .body = "loop {loop stop() {}} {}" },
        .{ .body = "loop {loop {break} {}\nstop()} {}" },
        .{ .body = "loop {continue} {break}" },
        .{ .body = "loop {if true {continue} else {stop()}} {break}" },
        .{ .body = "loop {return} {}", .annotation = "" },
        .{ .body = "{loop @panic(\"stop\") {}\n@println(\"tail\")}", .annotation = "", .tag = .unreachable_code },
        .{ .body = "{loop stop() {}\n@println(\"tail\")}", .tag = .unreachable_code },
        .{ .body = "{loop {if true {stop()} else {stop()}} {}\n@println(\"tail\")}", .tag = .unreachable_code },
        .{ .body = "{loop {loop stop() {}} {}\n@println(\"tail\")}", .tag = .unreachable_code },
        .{ .body = "{loop {continue} {}\n@println(\"tail\")}", .annotation = "", .tag = .unreachable_code },
        .{ .body = "loop {break} {}", .tag = .noreturn_completion },
        .{ .body = "loop {if true {break} else {stop()}} {}", .tag = .noreturn_completion },
        .{ .body = "loop {if true {continue} else {break}} {}", .tag = .noreturn_completion },
        .{ .body = "{loop {break} {}\n@println(\"tail\")}", .annotation = "" },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prelude, case.annotation, "let main:()=", case.body });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "condition.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        errdefer std.debug.print("condition source {s}: {any}\n", .{ source, module.diagnostics.items });
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        if (case.tag) |tag| {
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        } else {
            try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            try std.testing.expect(sema.lowering_complete);
        }
    }
}

test "empty noreturn attribute parentheses are rejected by parser" {
    var module = try Module.init(std.testing.allocator, "attribute.nov", "@[noreturn()]\nlet f:()=loop {}");
    defer module.deinit();
    try std.testing.expect(module.source.ast.errors.len != 0);
}

test "noreturn direct recursion methods loops and terminal operands lower" {
    const source =
        \\@[noreturn]
        \\let stop:()=@panic("stop")
        \\@[noreturn]
        \\let recurse:()=recurse()
        \\@[noreturn]
        \\let spin:()=loop {loop {break}
        \\    continue}
        \\let A=struct{
        \\    @[public]
        \\    @[noreturn]
        \\    let stop:()=stop()
        \\}
        \\@[noreturn]
        \\let method:()=A.stop()
        \\@[noreturn]
        \\let choose:(b:bool)=if b {stop()} else {spin()}
        \\let value:(b:bool)->int=match b {true=>stop()
        \\    false=>7}
        \\let indirect:()={let g:()->void=stop
        \\    g()
        \\    @print("still statically reachable")}
    ;
    var module = try Module.init(std.testing.allocator, "noreturn.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    errdefer std.debug.print("noreturn diagnostics: {any}\n", .{module.diagnostics.items});
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expect(sema.entries[0].noreturn);
    try std.testing.expect(sema.ir.functions.items[0].noreturn);
}

test "assert requires bool and optional exact string without conversion" {
    const cases = [_]struct { expression: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .expression = "@assert()", .tag = .argument_count_mismatch },
        .{ .expression = "@assert(true,\"a\",\"b\")", .tag = .argument_count_mismatch },
        .{ .expression = "@assert(1)", .tag = .literal_type_mismatch },
        .{ .expression = "@assert(\"true\")", .tag = .literal_type_mismatch },
        .{ .expression = "@assert(true,false)", .tag = .literal_type_mismatch },
        .{ .expression = "@assert(true,@as(int,1))", .tag = .expression_type_mismatch },
        .{ .expression = "@assert", .tag = .unsupported_body_expression },
    };
    for (cases) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f:()={s}", .{case.expression});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "assert.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
    }
}

test "test declarations cannot be called or extracted from any ordinary body" {
    for ([_][]const u8{
        "@[test]\nlet check:()={}\nlet main:()=check()",
        "@[test]\nlet check:()={}\nlet main:()={let callback=check\ncallback()}",
        "@[test]\nlet check:()=check()",
        "@[test]\nlet check:()={}\n@[test]\nlet other:()={let callback=check\ncallback()}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "test.nov", source);
        defer module.deinit();
        module.tests_enabled = true;
        var result = try Lowering.lower(&module);
        defer result.deinit();
        var rejected = false;
        for (module.diagnostics.items) |diagnostic| if (diagnostic.tag == .test_not_callable) {
            rejected = true;
        };
        try std.testing.expect(rejected);
    }
}

test "panic requires exactly one exact string argument" {
    const Case = struct { expression: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .expression = "@panic()", .tag = .argument_count_mismatch },
        .{ .expression = "@panic(\"a\", \"b\")", .tag = .argument_count_mismatch },
        .{ .expression = "@panic(true)", .tag = .literal_type_mismatch },
        .{ .expression = "@panic(1)", .tag = .literal_type_mismatch },
        .{ .expression = "@panic(@as(int, 1))", .tag = .expression_type_mismatch },
        .{ .expression = "@panic(@print())", .tag = .expression_type_mismatch },
        .{ .expression = "@panic([true])", .tag = .expression_type_mismatch },
        .{ .expression = "@panic", .tag = .unsupported_body_expression },
        .{ .expression = "{@panic(\"stop\")\n@print(\"unreachable\")}", .tag = .unreachable_code },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let main: () = {s}", .{case.expression});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "panic.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        errdefer std.debug.print("panic case {s}: {any}\n", .{ source, module.diagnostics.items });
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "ascription diagnoses arity types exact mismatches and interpolation source spans" {
    const Case = struct { expression: []const u8, tag: Module.Diagnostic.Tag, token: []const u8 };
    for ([_]Case{
        .{ .expression = "@as()", .tag = .argument_count_mismatch, .token = "(" },
        .{ .expression = "@as(int)", .tag = .argument_count_mismatch, .token = "(" },
        .{ .expression = "@as(int, 1, 2)", .tag = .argument_count_mismatch, .token = "(" },
        .{ .expression = "@as(missing, 1)", .tag = .unknown_type, .token = "missing" },
        .{ .expression = "@as(true, 1)", .tag = .not_a_type, .token = "true" },
        .{ .expression = "@as(1, 1)", .tag = .unsupported_type, .token = "1" },
        .{ .expression = "@as(bool, 1)", .tag = .literal_type_mismatch, .token = "1" },
        .{ .expression = "@as(int, true)", .tag = .literal_type_mismatch, .token = "true" },
        .{ .expression = "@as(int, @as(uint, 1))", .tag = .expression_type_mismatch, .token = "(" },
        .{ .expression = "@as(i16, @as(i8, 1))", .tag = .expression_type_mismatch, .token = "(" },
        .{ .expression = "@as([]int, [true])", .tag = .literal_type_mismatch, .token = "true" },
        .{ .expression = "@as((int) -> int, true)", .tag = .literal_type_mismatch, .token = "true" },
        .{ .expression = "@as(void, true)", .tag = .literal_type_mismatch, .token = "true" },
        .{ .expression = "@as(int, @print())", .tag = .expression_type_mismatch, .token = "(" },
        .{ .expression = "@as(u128, 340282366920938463463374607431768211456)", .tag = .literal_out_of_range, .token = "340282366920938463463374607431768211456" },
        .{ .expression = "@as(int, -@as(uint, 1))", .tag = .unsupported_body_expression, .token = "-" },
        .{ .expression = "@as", .tag = .unsupported_body_expression, .token = "@as" },
        .{ .expression = "\"é{@as(u8, 256)}\"", .tag = .literal_out_of_range, .token = "256" },
        .{ .expression = "\"{@as(missing, 1)}\"", .tag = .unknown_type, .token = "missing" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> void = @println({s})", .{case.expression});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "as-error.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(diagnostic.span).?);
    }
}

test "ascription contextual bounds cover every integer width without defaults or conversions" {
    for ([_][]const u8{ "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "i128", "u128", "int", "uint" }) |name| {
        const bits: u8 = if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) @intCast(Ir.native_pointer_bits) else try std.fmt.parseInt(u8, name[1..], 10);
        const signed = name[0] == 'i';
        const max: u128 = if (bits == 128 and !signed) std.math.maxInt(u128) else (@as(u128, 1) << @intCast(bits - @as(u8, @intFromBool(signed)))) - 1;
        for ([_]bool{ false, true }) |overflow| {
            if (overflow and max == std.math.maxInt(u128)) continue;
            const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> {s} = @as({s}, {d})", .{ name, name, max + @intFromBool(overflow) });
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "as-bounds.nov", source);
            defer module.deinit();
            var sema = try Sema.analyzeFunctions(&module);
            defer sema.deinit();
            try std.testing.expectEqual(overflow, module.hasErrors());
            if (overflow) try std.testing.expectEqual(Module.Diagnostic.Tag.literal_out_of_range, module.diagnostics.items[0].tag);
        }
        const minimum_magnitude: u128 = if (signed) @as(u128, 1) << @intCast(bits - 1) else 0;
        for ([_]bool{ false, true }) |overflow| {
            const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> {s} = @as({s}, -{d})", .{ name, name, minimum_magnitude + @intFromBool(overflow) });
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "as-minimum.nov", source);
            defer module.deinit();
            var sema = try Sema.analyzeFunctions(&module);
            defer sema.deinit();
            try std.testing.expectEqual(overflow, module.hasErrors());
            if (overflow) try std.testing.expectEqual(Module.Diagnostic.Tag.literal_out_of_range, module.diagnostics.items[0].tag);
        }
    }
}

test "interpolation diagnoses names grammar types scopes and exact source bytes" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag, token: []const u8 };
    for ([_]Case{
        .{ .source = "let f: (n: int) -> string = \"{missing}\"", .tag = .unknown_name, .token = "missing" },
        .{ .source = "let f: () -> string = {let text = \"{text}\"\ntext}", .tag = .unknown_name, .token = "text" },
        .{ .source = "let f: () -> string = {let text = \"{later}\"\nlet later = true\ntext}", .tag = .unknown_name, .token = "later" },
        .{ .source = "let f: () -> string = {if true {let hidden = true\n}\n\"{hidden}\"}", .tag = .unknown_name, .token = "hidden" },
        .{ .source = "let f: (n: int) -> string = \"{}\"", .tag = .invalid_interpolation, .token = "{}" },
        .{ .source = "let f: (n: int) -> string = \"{n\"", .tag = .invalid_interpolation, .token = "{n" },
        .{ .source = "let f: (n: int) -> string = \"}\"", .tag = .invalid_interpolation, .token = "}" },
        .{ .source = "let f: (n: int) -> string = \"{n:02}\"", .tag = .invalid_interpolation, .token = "{n:02}" },
        .{ .source = "let f: (n: int) -> string = \"{n()}\"", .tag = .not_callable, .token = "(" },
        .{ .source = "let f: (n: int) -> string = \"{{n}}\"", .tag = .invalid_interpolation, .token = "{{n}" },
        .{ .source = "let f: (n: int) -> string = \"{1}\"", .tag = .literal_needs_type, .token = "1" },
        .{ .source = "let f: (n: int) -> string = \"{if}\"", .tag = .invalid_interpolation, .token = "{if}" },
        .{ .source = "let f: () -> string = \"{'a'}\"", .tag = .invalid_interpolation, .token = "{'a'}" },
        .{ .source = "let f: () -> string = \"{'é'}\"", .tag = .invalid_interpolation, .token = "{'é'}" },
        .{ .source = "let f: () -> string = \"{'\\n'}\"", .tag = .invalid_interpolation, .token = "{'\\n'}" },
        .{ .source = "let f: (n: int) -> string = \"{f(n, n)}\"", .tag = .argument_count_mismatch, .token = "(" },
        .{ .source = "let f: (n: int) -> string = \"é{n + missing}\"", .tag = .unknown_name, .token = "missing" },
        .{ .source = "let f: (n: int) -> string = \"{f(true)}\"", .tag = .literal_type_mismatch, .token = "true" },
        .{ .source = "let f: (n: int) -> string = \"{f(\\\"text\\\")}\"", .tag = .invalid_interpolation, .token = "{f(\\\"text\\\")}" },
        .{ .source = "let f: (n: int) -> string = \"{f(n); n}\"", .tag = .invalid_interpolation, .token = "{f(n); n}" },
        .{ .source = "let f: (n: int) -> string = \"{n + (1}\"", .tag = .invalid_interpolation, .token = "{n + (1}" },
        .{ .source = "let f: (n: int) -> string = \"{f(n) n}\"", .tag = .invalid_interpolation, .token = "{f(n) n}" },
        .{ .source = "let f: (n: int) -> string = \"{return n}\"", .tag = .invalid_interpolation, .token = "{return n}" },
        .{ .source = "let f: (n: int) -> string = \"{n}}\"", .tag = .invalid_interpolation, .token = "}" },
        .{ .source = "let unit: () = {}\nlet f: () -> string = \"{unit()}\"", .tag = .unsupported_interpolation_type, .token = "unit()" },
        .{ .source = "let f: (n: []int) -> string = \"{n}\"", .tag = .unsupported_interpolation_type, .token = "n" },
        .{ .source = "let f: () -> string = \"{f}\"", .tag = .unsupported_interpolation_type, .token = "f" },
        .{ .source = "let f: (n: () -> int) -> string = \"{n}\"", .tag = .unsupported_interpolation_type, .token = "n" },
        .{ .source = "let f: (n: int) -> int = \"{n}\"", .tag = .expression_type_mismatch, .token = "\"{n}\"" },
        .{ .source = "let f: (n: int) -> string = \"{n}a\\xFF\"", .tag = .invalid_literal, .token = "\\" },
        .{ .source = "let f: (n: int) -> string = \"a\\xFF{n}\"", .tag = .invalid_literal, .token = "\\" },
        .{ .source = "let f: (n: int) -> string = \"{n}\xff\"", .tag = .invalid_literal, .token = "\xff" },
        .{ .source = "let f: (n: int) -> string = {let n = true\n\"{n}\"}", .tag = .unsupported_shadowing, .token = "n" },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "interpolation-error.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(diagnostic.span).?);
    }
}

test "record interpolation uses print signature diagnostics at the placeholder source" {
    for ([_]struct { declaration: []const u8, related: bool = false }{
        .{ .declaration = "let R=struct{s:string,}" },
        .{ .declaration = "let R=struct{s:string,}\nlet toString:(r:R)->string=r.s" },
        .{ .declaration = "let R=struct{s:string,\nlet toString:()->string=\"wrong\"\n}", .related = true },
        .{ .declaration = "let R=struct{s:string,\nlet toString:(r:R,extra:string)->string=r.s+extra\n}", .related = true },
        .{ .declaration = "let R=struct{s:string,\nlet toString:(r:R)->bool=true\n}", .related = true },
        .{ .declaration = "let Other=struct{s:string,}\nlet R=struct{s:string,\nlet toString:(r:Other)->string=r.s\n}", .related = true },
        .{ .declaration = "let R=union{item:string,}" },
        .{ .declaration = "let G:(T:type)->type=union{item:T,}\nlet R=G(string)" },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ case.declaration, "\nlet Alias=R\nlet f:(r:Alias)->string=\"prefix:{r}\"" });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "record-interpolation-error.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(Module.Diagnostic.Tag.invalid_to_string, diagnostic.tag);
        try std.testing.expectEqualStrings("r", module.sourceSlice(diagnostic.span).?);
        try std.testing.expectEqual(case.related, diagnostic.related != null);
        if (diagnostic.related) |related| try std.testing.expectEqualStrings("toString", module.sourceSlice(related).?);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "union constructors captures and coverage diagnose exact scalar boundaries" {
    const prefix = "let U=union{ok:int,err:string,none,flag:bool,}\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:()->U=.ok[true]", .tag = .literal_type_mismatch },
        .{ .source = "let f:()->U=1", .tag = .literal_type_mismatch },
        .{ .source = "let f:()->U=U.ok[]", .tag = .invalid_union_constructor },
        .{ .source = "let f:()->U=.none[true]", .tag = .invalid_union_constructor },
        .{ .source = "let f:()->U=U[]", .tag = .invalid_union_constructor },
        .{ .source = "let f:()->U=.missing[]", .tag = .unknown_field },
        .{ .source = "let f:()={let x=.ok[1]\n}", .tag = .union_needs_type },
        .{ .source = "let f:(u:U)->int=match u{.ok=>|v|v}", .tag = .match_missing_variants },
        .{ .source = "let f:(u:U)->int=match u{.ok=>0\n.ok=>1\n_=>2}", .tag = .match_covered_union_prong },
        .{ .source = "let f:(u:U)->int=match u{_=>0\n.none=>1}", .tag = .match_covered_union_prong },
        .{ .source = "let f:(u:U)->int=match u{.ok=>0\n.err=>0\n.none=>0\n.flag=>0\n_=>0}", .tag = .match_covered_union_prong },
        .{ .source = "let f:(u:U)->int=match u{.ok=>|v|v\n_=>v}", .tag = .unknown_name },
        .{ .source = "let f:(u:U)->void=match u{.ok=>|v|{v=2}\n_=>{}}", .tag = .immutable_assignment },
        .{ .source = "let f:(u:U)->int=match u{.ok=>|u|0\n_=>0}", .tag = .unsupported_shadowing },
        .{ .source = "let f:(u:U)->int=match u{_=>|v|0}", .tag = .invalid_match_capture },
        .{ .source = "let f:(u:U)->int=match u{.ok|.err=>|v|0\n_=>0}", .tag = .unsupported_match_pattern },
        .{ .source = "let f:(u:U)->int=match u{.bad=>0\n_=>0}", .tag = .unknown_field },
        .{ .source = "let f:(u:U)->int=match u,u{_,_=>0}", .tag = .unsupported_match_subject },
        .{ .source = "let f:(u:U)->int=u.ok", .tag = .unsupported_body_expression },
        .{ .source = "let V=union{ok:int,}\nlet f:()->V=U.ok[1]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(n:i8)->U=.ok[n]", .tag = .expression_type_mismatch },
        .{ .source = "let f:()->U=.ok[\"x\"]", .tag = .literal_type_mismatch },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "union-error.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        if (module.diagnostics.items.len != 1 or module.diagnostics.items[0].tag != case.tag) std.debug.print("union case {s}: {any} parse={any}\n", .{ case.source, module.diagnostics.items, module.source.ast.errors });
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
    }
}

test "union captures infer branch types without leaking and preserve contextual constructors" {
    for ([_][]const u8{
        "let f:(u:U)->int=match u{.ok=>|n|n\n.err=>0\n.none=>0}",
        "let f:(u:U)={let n=match u{.ok=>|n|n\n_=>0}\n@println(n)}",
        "let f:(u:U)->U=match u{.ok=>|n|.ok[n]\n_=>.none[]}",
        "let f:(u:U)->int=match u{.ok=>|n|{return n}\n_=>1}",
        "let f:(u:U)->int=match u{.ok=>|n|match u{.ok=>|m|n+m\n_=>n}\n_=>0}",
        "let f:(u:U)={let n=match u{.ok=>0\n.err=>0\n.none=>|v|{let unit:void=v\n@as(int,1)}}\n@println(n)}",
    }) |body| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ "let U=union{ok:int,err:string,none,}\n", body });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "union.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        if (module.hasErrors()) std.debug.print("union body {s}: {any}\n", .{ body, module.diagnostics.items });
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
    }
}

test "boolean match coverage diagnoses missing values covered prongs and invalid final patterns" {
    const Case = struct { patterns: []const u8, tag: Module.Diagnostic.Tag, token: []const u8, related: ?[]const u8 = null };
    for ([_]Case{
        .{ .patterns = "", .tag = .match_missing_boolean_values, .token = "match" },
        .{ .patterns = "true => 1", .tag = .match_missing_false, .token = "match" },
        .{ .patterns = "false | false => 1", .tag = .match_missing_true, .token = "match" },
        .{ .patterns = "true => 1\ntrue | true => 2\nfalse => 3", .tag = .match_covered_boolean_prong, .token = "|", .related = "true" },
        .{ .patterns = "false => 1\nfalse => 2\n_ => 3", .tag = .match_covered_boolean_prong, .token = "false", .related = "false" },
        .{ .patterns = "true => 1\nfalse => 2\n_ => 3", .tag = .match_covered_boolean_prong, .token = "_", .related = "true" },
        .{ .patterns = "_ | false => 1\ntrue => 2", .tag = .match_covered_boolean_prong, .token = "true", .related = "|" },
        .{ .patterns = "true | false => 1\nfalse => 2", .tag = .match_covered_boolean_prong, .token = "false", .related = "|" },
        .{ .patterns = "true => 1\n1 => 2", .tag = .literal_type_mismatch, .token = "1" },
        .{ .patterns = "_ | 1 => 1", .tag = .literal_type_mismatch, .token = "1" },
        .{ .patterns = "true, false => 1", .tag = .match_pattern_count, .token = "=>" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let Flag = bool\nlet f: (b: Flag) -> int = match b {{{s}}}", .{case.patterns});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "bool-match-error.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.token, module.sourceSlice(diagnostic.span).?);
        if (case.related) |related| {
            try std.testing.expectEqualStrings(related, module.sourceSlice(diagnostic.related.?).?);
            try std.testing.expect(diagnostic.related.?.start < diagnostic.span.start);
        }
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
    }
}

test "boolean match coverage retains exact bidirectional types scopes and nonboolean restrictions" {
    for ([_][]const u8{
        "let f: (b: bool) -> int = match b {true => 1\nfalse => 2}",
        "let f: (b: bool) -> int = match b {false | false => 1\ntrue => 2}",
        "let f: (b: bool) -> int = match b {true => 1\ntrue | false => 2}",
        "let f: (b: bool) -> int = match b {false => 1\nfalse | _ => 2}",
        "let f: () -> int = match true {true => 1\n_ => 2}",
        "let f: (b: bool, n: int) -> void = {let x = match b {true => 1\nfalse => n}\n@println(x)}",
        "let f: (b: bool, a: []int) -> void = {let x = match b {true => []\nfalse => a}\n@println(x.len)}",
        "let f: (b: bool, n: int) -> void = {let x = match b {true => {return}\nfalse => match b {true => 1\nfalse => n}}\n@println(x)}",
        "let f: () -> int = match {return 7} {true => 1\nfalse => 2}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bool-match.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
    }
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: () = {let x = match true {true => 1\nfalse => 2}\n@println(x)}", .tag = .literal_needs_type },
        .{ .source = "let f: (b: bool) -> i8 = match b {true => 1\nfalse => 128}", .tag = .literal_out_of_range },
        .{ .source = "let f: (b: bool) -> int = match b {true => 1\nfalse => false}", .tag = .literal_type_mismatch },
        .{ .source = "let f: (b: bool) -> int = match b {true => {let private: int = 1\nprivate}\nfalse => private}", .tag = .unknown_name },
        .{ .source = "let f: (b: bool) -> int = match b, b {true, true => 1\ntrue, false => 2\nfalse, true => 3\nfalse, false => 4}", .tag = .match_missing_catchall },
        .{ .source = "let f: (n: u8) -> int = match n {0 | 255 => 1}", .tag = .match_missing_catchall },
        .{ .source = "let f: (s: string) -> int = match s {\"\" => 1}", .tag = .match_missing_catchall },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "bool-match-error.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "full integer domain coverage still requires a final catchall" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll("let f: (n: u8) -> int = match n {\n");
    for (0..256) |n| try source.writer.print("{d} => 1\n", .{n});
    try source.writer.writeAll("}");
    var module = try Module.init(std.testing.allocator, "integer-match.nov", source.written());
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.match_missing_catchall, module.diagnostics.items[0].tag);
    try std.testing.expect(!sema.lowering_complete);
}

test "match and pipes reject unsupported patterns arity defaults and inexact types" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: (n: int) -> int = match n {}", .tag = .match_missing_catchall },
        .{ .source = "let f: (n: int) -> int = match n {0 => 1}", .tag = .match_missing_catchall },
        .{ .source = "let f: (n: int) -> int = match n {_ => 1\n0 => 2}", .tag = .match_catchall_not_last },
        .{ .source = "let f: (n: int) -> int = match n, n {0 => 1\n_ => 2}", .tag = .match_pattern_count },
        .{ .source = "let f: (n: int) -> int = match n {0, 1 => 1\n_ => 2}", .tag = .match_pattern_count },
        .{ .source = "let f: (n: int) -> int = match n {capture => 1\n_ => 2}", .tag = .unsupported_match_pattern },
        .{ .source = "let f: (n: int) -> int = match n {.value => 1\n_ => 2}", .tag = .unsupported_match_pattern },
        .{ .source = "let f: (n: int) -> int = match n {0..3 => 1\n_ => 2}", .tag = .unsupported_match_pattern },
        .{ .source = "let f: (n: int) -> int = match n {{0 | 1} => 1\n_ => 2}", .tag = .unsupported_match_pattern },
        .{ .source = "let f: (n: int) -> int = match n {n + 1 => 1\n_ => 2}", .tag = .unsupported_match_pattern },
        .{ .source = "let f: (n: []int) -> int = match n {_ => 2}", .tag = .unsupported_match_subject },
        .{ .source = "let f: () -> int = match 1 {_ => 2}", .tag = .literal_needs_type },
        .{ .source = "let f: (n: u8) -> int = match n {-1 => 1\n_ => 2}", .tag = .literal_out_of_range },
        .{ .source = "let f: (n: i8) -> int = match n {128 => 1\n_ => 2}", .tag = .literal_out_of_range },
        .{ .source = "let f: (n: bool) -> int = match n {1 => 1\n_ => 2}", .tag = .literal_type_mismatch },
        .{ .source = "let f: (n: int) -> int = match n {0 => true\n_ => 2}", .tag = .literal_type_mismatch },
        .{ .source = "let f: (n: int) -> int = match n {0 => {let private: int = 2\nprivate}\n_ => private}", .tag = .unknown_name },
        .{ .source = "let f: (n: int) -> int = n\nlet g: () -> int = true |> f", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> int = 1\nlet g: () -> int = 1 |> f", .tag = .argument_count_mismatch },
        .{ .source = "let f: (a: int, b: int) -> int = a\nlet g: () -> int = 1 |> f", .tag = .argument_count_mismatch },
        .{ .source = "let g: () -> int = true |> false", .tag = .not_callable },
        .{ .source = "let g: () -> void = 1 |> @println", .tag = .literal_needs_type },
        .{ .source = "let g: () -> void = [true] |> @println", .tag = .unsupported_body_expression },
        .{ .source = "let f: (n: i8) -> i8 = n\nlet g: (n: i16) -> i8 = n |> f", .tag = .expression_type_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "match-error.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        if (module.diagnostics.items.len != 1 or module.diagnostics.items[0].tag != case.tag) {
            std.debug.print("unexpected diagnostics for {s}: {any}\n", .{ case.source, module.diagnostics.items });
        }
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
    }
}

test "bitwise binaries infer exact integer operands through late result hints" {
    for ([_][]const u8{ "&", "|", "^" }) |op| {
        inline for ([_][]const u8{
            "let f: () -> i8 = 1 {s} 2",
            "let f: (n: u64) -> u64 = 1 {s} n",
            "let f: (n: int) -> int = n {s} 1",
            "let f: (n: i16) -> void = {{let x = {{1 {s} n}}\n@println(x)}}",
            "let f: (n: i16) -> void = {{let x = if true {{1 {s} 2}} else {{n}}\n@println(x)}}",
            "let f: (n: i16) -> void = {{let x = match true {{true => 1 {s} 2\n_ => n}}\n@println(x)}}",
            "let f: (n: i16) -> void = {{let x = loop {{if false {{break 1 {s} 2}}\nbreak n}}\n@println(x)}}",
            "let f: (n: i16) -> void = {{let x = loop {{let skipped = {{break 1 {s} n}} & n\n}}\n@println(x)}}",
            "let f: (n: i16) -> void = {{let x = loop {{let skipped = n {s} {{break n}}\n}}\n@println(x)}}",
        }) |template| {
            const source = try std.fmt.allocPrint(std.testing.allocator, template, .{op});
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "bitwise.nov", source);
            defer module.deinit();
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expect(!module.hasErrors());
            try std.testing.expect(sema.lowering_complete);
            try sema.ir.verify();
        }
    }
}

test "bitwise binaries reject defaults nonintegers mismatches and literal bounds" {
    for ([_][]const u8{ "&", "|", "^" }) |op| {
        inline for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
            .{ .source = "let f: () -> void = {{let n = 1 {s} 2\n}}", .tag = .literal_needs_type },
            .{ .source = "let f: () -> int = 1 {s} missing", .tag = .unknown_name },
            .{ .source = "let f: () -> void = {{let n = 1 {s} missing\n}}", .tag = .unknown_name },
            .{ .source = "let f: () -> i8 = 128 {s} 0", .tag = .literal_out_of_range },
            .{ .source = "let f: () -> u8 = 0 {s} -1", .tag = .literal_out_of_range },
            .{ .source = "let f: (a: i8, b: i16) -> i8 = a {s} b", .tag = .expression_type_mismatch },
            .{ .source = "let f: (a: u8, b: i8) -> u8 = a {s} b", .tag = .expression_type_mismatch },
            .{ .source = "let f: (a: i8) -> i16 = a {s} 1", .tag = .expression_type_mismatch },
            .{ .source = "let f: () -> bool = true {s} false", .tag = .operator_not_supported },
            .{ .source = "let f: () -> string = \"a\" {s} \"b\"", .tag = .operator_not_supported },
            .{ .source = "let f: (a: []int) -> []int = a {s} a", .tag = .operator_not_supported },
            .{ .source = "let f: (a: () -> int) -> int = a {s} a", .tag = .operator_not_supported },
            .{ .source = "let f: (a: int) -> int = match a {{{{0 {s} 1}} => 2\n_ => 3}}", .tag = .unsupported_match_pattern },
        }) |case| {
            const source = try std.fmt.allocPrint(std.testing.allocator, case.source, .{op});
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "bitwise-error.nov", source);
            defer module.deinit();
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
            try std.testing.expect(module.sourceSlice(module.diagnostics.items[0].span).?.len != 0);
            try std.testing.expect(!sema.lowering_complete);
            try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
        }
    }
}

test "membership infers exact scalar operands without numeric defaults" {
    for ([_][]const u8{
        "let f: (a: []i8) -> bool = -128 in a",
        "let f: (n: u16) -> bool = n in [1, 2, n]",
        "let f: (n: int) -> bool = n in []",
        "let f: () -> bool = true in [true, false]",
        "let f: () -> bool = false in []",
        "let f: (a: []uint) -> void = {let found = 1 in a\n@println(found)}",
        "let f: (a: []i32, b: bool) -> bool = 1 in if b {a} else {[]}",
        "let f: (n: i64, b: bool) -> bool = if b {1} else {n} in [n]",
        "let f: () -> int = loop {let found = {break 7} in [true]\n}",
        "let f: () -> int = loop {let found = true in {break 7}\n}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "membership.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
    }
}

test "membership rejects defaults widening nonscalar elements and nonboolean results" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: () -> bool = 1 in [1, 2]", .tag = .literal_needs_type },
        .{ .source = "let f: () -> bool = 1 in []", .tag = .literal_needs_type },
        .{ .source = "let f: (a: []i8, n: i16) -> bool = n in a", .tag = .expression_type_mismatch },
        .{ .source = "let f: (a: []u8, n: i8) -> bool = n in a", .tag = .expression_type_mismatch },
        .{ .source = "let f: (a: []i8) -> bool = 128 in a", .tag = .literal_out_of_range },
        .{ .source = "let f: (a: []u8) -> bool = -1 in a", .tag = .literal_out_of_range },
        .{ .source = "let f: (a: []bool) -> bool = 1 in a", .tag = .literal_type_mismatch },
        .{ .source = "let f: (n: int) -> bool = n in [true]", .tag = .expression_type_mismatch },
        .{ .source = "let f: (a: []int) -> bool = true in a", .tag = .literal_type_mismatch },
        .{ .source = "let f: (a: []int) -> int = 1 in a", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> bool = true in false", .tag = .operator_not_supported },
        .{ .source = "let f: () -> bool = \"a\" in \"abc\"", .tag = .operator_not_supported },
        .{ .source = "let A=struct{s:string,}\nlet f: (a: A) -> bool = a in []", .tag = .operator_not_supported },
        .{ .source = "let f: (a: []int) -> bool = a in []", .tag = .operator_not_supported },
        .{ .source = "let f: () -> bool = {} in []", .tag = .operator_not_supported },
        .{ .source = "let f: (a: []int) -> bool = missing in a", .tag = .unknown_name },
        .{ .source = "let f: (n: int) -> bool = n in missing", .tag = .unknown_name },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "membership-error.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(module.sourceSlice(module.diagnostics.items[0].span).?.len != 0);
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
    }
}

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
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
}

test "string equality infers exact operands and a boolean result" {
    var module = try Module.init(std.testing.allocator, "equality.nov",
        \\let constant: string = "a"
        \\let id: (s: string) -> string = s
        \\let f: (s: string, b: bool) -> bool = {
        \\    let same = "a" == "a"
        \\    let different = constant != id(s)
        \\    let branch = if b {s} else {"b"} == "a"
        \\    same and different or branch
        \\}
    );
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "string comparisons reject ordered mixed and nonboolean result types" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: () -> bool = \"a\" == true", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> bool = true != \"a\"", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> bool = \"a\" == 1", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> bool = 1 != \"a\"", .tag = .literal_type_mismatch },
        .{ .source = "let f: (s: string, n: int) -> bool = s == n", .tag = .expression_type_mismatch },
        .{ .source = "let f: (s: string, n: int) -> bool = n != s", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> string = \"a\" == \"b\"", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> bool = \"a\" < \"b\"", .tag = .operator_not_supported },
        .{ .source = "let f: () -> bool = \"a\" <= \"b\"", .tag = .operator_not_supported },
        .{ .source = "let f: () -> bool = \"a\" > \"b\"", .tag = .operator_not_supported },
        .{ .source = "let f: () -> bool = \"a\" >= \"b\"", .tag = .operator_not_supported },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "comparison.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "string compounds retain mutability operator and operand diagnostics" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: () -> void = {let s = \"a\"\ns += \"b\"}", .tag = .immutable_assignment },
        .{ .source = "let f: (s: string) -> void = {s += \"b\"}", .tag = .immutable_assignment },
        .{ .source = "let s: string = \"a\"\nlet f: () -> void = {s += \"b\"}", .tag = .immutable_assignment },
        .{ .source = "let f: () -> void = {let mut s = \"a\"\ns += true}", .tag = .literal_type_mismatch },
        .{ .source = "let f: (n: int) -> void = {let mut s = \"a\"\ns += n}", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> void = {let mut s = true\ns += \"a\"}", .tag = .operator_not_supported },
        .{ .source = "let f: () -> void = {let mut s: int = 1\ns += \"a\"}", .tag = .literal_type_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "compound.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        if (case.tag == .immutable_assignment) try std.testing.expect(module.diagnostics.items[0].related != null);
        try std.testing.expect(!sema.lowering_complete);
    }
    for ([_][]const u8{ "-=", "*=", "/=", "%=" }) |op| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> void = {{let mut s: string = \"a\"\ns {s} \"b\"}}", .{op});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "compound.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.operator_not_supported, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "string scalar indexing has exact rune inference through contexts constructors and generic methods" {
    const source =
        \\let G:(T:type)->type=struct{value:T,
        \\    let byte:(self:@This(),i:int)->rune=self.value[i]
        \\}
        \\let U=union{text:string,}
        \\let text:()->string="é\0z"
        \\let take:(b:rune)->rune=b
        \\let f:(s:string,a:[]string,i:int,b:bool)->bool={
        \\    let inferred=s[i]
        \\    let sum=s[0]
        \\    let bytes=['\0',s[-1],'ÿ']
        \\    let branch=if b {'\0'} else {s[0]}
        \\    let matched=match b {true=>s[0]
        \\        false=>'ÿ'}
        \\    let looped=loop {break s[0]}
        \\    let alias=&s
        \\    @assert(b and alias[0]=='\0' or s[-1]=='ÿ')
        \\    @assert(text()[0]=='é')
        \\    @assert(text()[1]=='\0')
        \\    @assert(text()[2]=='z')
        \\    @assert(text()[-1]=='z')
        \\    @println(sum,bytes[0],branch,matched,looped,a[0][1],s.len)
        \\    let wrapped:U=U.text[s]
        \\    @println(match wrapped {.text=>|v|v[0]})
        \\    take(inferred)==G(string)[.value=s].byte(i)
        \\}
    ;
    var module = try Module.init(std.testing.allocator, "string-byte.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    errdefer std.debug.print("string scalar diagnostics: {any}\n", .{module.diagnostics.items});
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    var indexed: usize = 0;
    for (sema.ir.functions.items) |function| {
        for (function.body.?.instructions) |instruction| {
            if (instruction.op != .array_index) continue;
            const base = function.body.?.instructions[@backingInt(instruction.op.array_index.array)];
            if (sema.ir.getType(base.ty).? != .string) continue;
            indexed += 1;
            const ty = sema.ir.getType(instruction.ty).?;
            try std.testing.expect(ty == .rune);
            const index = function.body.?.instructions[@backingInt(instruction.op.array_index.index)];
            try std.testing.expectEqual(Ir.Type.Integer{ .signedness = .signed, .bits = Ir.native_pointer_bits }, sema.ir.getType(index.ty).?.integer);
        }
    }
    try std.testing.expect(indexed >= 15);
}

test "string slicing semantic types inference and rejection" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let f:(s:string)->string=s[..=2]", .tag = .inclusive_string_slice },
        .{ .source = "let f:(s:string)->string=s[0..=]", .tag = .inclusive_string_slice },
        .{ .source = "let f:(s:[]u8)->string=s[..]", .tag = .string_slice_requires_string },
        .{ .source = "let f:(s:rune)->string=s[..]", .tag = .string_slice_requires_string },
        .{ .source = "let f:(s:string,n:uint)->string=s[n..]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string,n:u8)->string=s[..n]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string,n:i8)->string=s[n..]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->string=s[..true]", .tag = .literal_type_mismatch },
        .{ .source = "let f:(s:string)->string=s['a'..]", .tag = .literal_type_mismatch },
        .{ .source = "let f:(s:string)->rune=s[..]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->[]u8=s[..]", .tag = .expression_type_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "slice-error.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
    var module = try Module.init(std.testing.allocator, "slice-types.nov",
        \\let G:(T:type)->type=struct{value:T,
        \\    let cut:(s:G(T))->string=s.value[..]
        \\}
        \\let U=union{text:string,}
        \\let f:(s:string,b:bool)->string={let alias=&s
        \\    let view=alias[..]
        \\    let a=[view,s[1..]]
        \\    let u:U=.text[s[..1]]
        \\    let chosen=if b {s[..]}else{view}
        \\    let matched=match b {true=>s[..]
        \\        false=>view}
        \\    let looped=loop {break s[..]}
        \\    @println(a[0],chosen,matched,looped,match u {.text=>|v|v[..]})
        \\    G(string)[.value=s].cut()}
    );
    defer module.deinit();
    errdefer std.debug.print("slice parse: {any}\n", .{module.source.ast.errors});
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    errdefer std.debug.print("slice types: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    for (sema.ir.functions.items) |function| {
        for (function.body.?.instructions, 0..) |instruction, at| {
            if (instruction.op != .string_slice) continue;
            const slice = instruction.op.string_slice;
            try std.testing.expect(sema.ir.getType(instruction.ty).? == .string);
            try std.testing.expect(@backingInt(slice.base) < at);
            for ([_]?Ir.ValueId{ slice.start, slice.end }) |bound| if (bound) |value| {
                try std.testing.expect(@backingInt(slice.base) < @backingInt(value));
                try std.testing.expect(@backingInt(value) < at);
                try std.testing.expectEqual(Ir.Type.Integer{ .signedness = .signed, .bits = Ir.native_pointer_bits }, sema.ir.getType(function.body.?.instructions[@backingInt(value)].ty).?.integer);
            };
        }
    }
}

test "string slicing terminal operands skip later bounds and slice IR" {
    for ([_][]const u8{
        "let f:()->string=@panic(\"stop\")[missing..missing]",
        "let f:()->string=\"a\"[{return \"done\"}..missing]",
        "let f:()->bool=\"a\"[0..@panic(\"stop\")]",
        "let f:()->string=loop {\"a\"[..{break \"done\"}]}",
        "let f:()->string=loop {\"a\"[{continue}..missing]}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "slice-terminal.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
        for (sema.ir.functions.items) |function|
            for (function.body.?.instructions) |instruction| try std.testing.expect(instruction.op != .string_slice);
    }
}

test "string scalar indexing rejects nonnative indices and nonrune result contexts" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:(s:string,i:uint)->rune=s[i]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string,i:u8)->rune=s[i]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string,i:i8)->rune=s[i]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->rune=s[true]", .tag = .literal_type_mismatch },
        .{ .source = "let f:(s:string)->rune=s[\"0\"]", .tag = .literal_type_mismatch },
        .{ .source = "let f:(s:string)->int=s[0]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->i8=s[0]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->uint=s[0]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->string=s[0]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->bool=s[0]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(s:string)->u8=s[0]", .tag = .expression_type_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "string-byte-error.nov", case.source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "string scalar indexing preserves once only left to right evaluation and pre index snapshots" {
    var module = try Module.init(std.testing.allocator, "string-byte-effects.nov",
        \\let base:()->string={@print("B")
        \\    "old"}
        \\let index:()->int={@print("I")
        \\    return -1}
        \\let f:()->rune=base()[index()]
        \\let snapshot:()->rune={let mut s="old"
        \\    s[{s="new"
        \\        index()}]}
    );
    defer module.deinit();
    errdefer std.debug.print("string scalar effects diagnostics: {any}\nparse errors: {any}\n", .{ module.diagnostics.items, module.source.ast.errors });
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    var checked: usize = 0;
    for (sema.ir.functions.items) |function| {
        const is_snapshot = std.mem.eql(u8, function.name, "snapshot");
        if (!is_snapshot and !std.mem.eql(u8, function.name, "f")) continue;
        const body = function.body.?;
        var calls: usize = 0;
        var stores: usize = 0;
        var store_at: usize = 0;
        var indices: usize = 0;
        for (body.instructions, 0..) |instruction, at| {
            if (instruction.op == .call) calls += 1;
            if (instruction.op == .local_store) {
                stores += 1;
                store_at = at;
            }
            if (instruction.op != .array_index) continue;
            indices += 1;
            const access = instruction.op.array_index;
            try std.testing.expect(@backingInt(access.array) < @backingInt(access.index));
            try std.testing.expect(@backingInt(access.index) < at);
            try std.testing.expect(body.instructions[@backingInt(access.index)].op == .call);
            if (is_snapshot) {
                try std.testing.expect(body.instructions[@backingInt(access.array)].op == .local_load);
                try std.testing.expect(@backingInt(access.array) < store_at);
                try std.testing.expect(store_at < @backingInt(access.index));
            } else try std.testing.expect(body.instructions[@backingInt(access.array)].op == .call);
        }
        try std.testing.expectEqual(@as(usize, 1), indices);
        try std.testing.expectEqual(@as(usize, if (is_snapshot) 1 else 2), calls);
        try std.testing.expectEqual(@as(usize, if (is_snapshot) 1 else 0), stores);
        checked += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), checked);
}

test "string scalar indexing terminal base and index operands skip unreachable operations" {
    for ([_][]const u8{
        "let f:()->rune=@panic(\"stop\")[missing]",
        "let f:()->string=\"a\"[{return \"done\"}]",
        "let f:()->bool=\"a\"[@panic(\"stop\")]",
        "let f:()->rune=loop {\"a\"[{break 'a'}]}",
        "let f:()->rune=loop {\"a\"[{continue}]}",
        "let f:(b:bool)->bool=b and \"a\"[{return false}]=='a'",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "string-byte-terminal.nov", source);
        defer module.deinit();
        errdefer std.debug.print("string scalar terminal source: {s}\ndiagnostics: {any}\n", .{ source, module.diagnostics.items });
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
        for (sema.ir.functions.items) |function| {
            for (function.body.?.instructions) |instruction| try std.testing.expect(instruction.op != .array_index);
        }
    }
}

fn testStringByteAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "string-byte-oom.nov",
        \\let f:(s:string,a:[]string)->rune={let alias=&s
        \\    let values=['\0',alias[-1],a[0][1]]
        \\    values[0]}
    );
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "string scalar indexing cleans up every allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testStringByteAllocationFailures, .{});
}

test "string scalar elements remain immutable through mutable bindings and whole string borrows" {
    for ([_][]const u8{ "=", "+=", "-=", "*=", "/=", "%=" }) |op| {
        for ([_][]const u8{ "let s=\"a\"", "let mut s=\"a\"", "let mut owner=\"a\"\nlet s=&owner" }) |binding| {
            const source = try std.fmt.allocPrint(std.testing.allocator, "let f:()={{{s}\ns[0]{s}1}}", .{ binding, op });
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "string-byte-mutation.nov", source);
            defer module.deinit();
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_assignment_target, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        }
    }
    for ([_][]const u8{
        "let s=\"a\"\nlet byte=&s[0]",
        "let mut s=\"a\"\nlet byte=&s[0]",
        "let mut owner=\"a\"\nlet s=&owner\nlet byte=&s[0]",
    }) |binding| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f:()={{{s}\n}}", .{binding});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "string-byte-borrow.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_borrow, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "strings reject byte mutation inclusive slicing ordering and eager globals" {
    for ([_][]const u8{
        "let f: () -> void = {let mut x = [\"a\"]\nx[0] += \"b\"}",
        "let f: () -> void = {let mut x = \"a\"\nx[0] = \"b\"}",
        "let f: () -> void = @println(\"a\"[0..=1])",
        "let f: () -> void = {let mut x = \"a\"\nx.len = 1}",
        "let f: () -> void = {let mut x = \"a\"\nx -= \"b\"}",
        "let f: () -> bool = \"a\" < \"b\"",
        "let f: () -> string = \"a\" - \"b\"",
        "let f: () -> string = 1 + \"b\"",
        "let f: () -> string = \"a\" + 1",
        "let x: string = \"a\" + \"b\"",
        "let mut x: string = \"a\"",
        "let x = @as(string, \"a\" + \"b\")",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "unsupported-string.nov", source);
        defer module.deinit();
        if (module.hasErrors()) continue;
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "managed arrays reject unsupported payloads nominal mismatches and record membership" {
    for ([_][]const u8{
        "let A=struct{s:string,}\nlet B=struct{s:string,}\nlet f:(a:A,b:B)={let x=[a,b]\n}",
        "let A=struct{s:string,}\nlet f:(a:A)->bool=a in [a]",
        "let A=struct{s:string,}\nlet f:(a:A)->bool=a in []",
        "let A=struct{s:string,}\nlet f:(a:A)={let mut x=[a]\nx[0]+=a}",
        "let A=struct{s:string,}\nlet f:(a:A)={let mut x=[a]\nx[0].s=\"bad\"}",
        "let A=struct{s:string,}\nlet f:(a:A)={let x:[]A=[]\nlet y:[]string=x\n}",
        "let A=struct{s:[]string,}",
        "let U=union{s:string,none,}\nlet f:(a:[]U)={}",
        "let E=enum{a,b,}\nlet f:(a:[]E)={}",
        "let f:(a:[]()->string)={}",
        "let f:(a:[][]string)={}",
        "let O:(T:type)->type=union{some:T,none,}\nlet f:(a:O([]string))={}",
        "let R:(T:type,E:type)->type=union{ok:T,err:E,}\nlet f:(a:R([]string,string))={}",
        "let f:()={let a:[]string=[]\nlet b:[]bool=[]\nlet c:[]string=b\n}",
        "let f:()={let a=[\"a\",true]\n}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "bad-managed-array.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
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
            try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        }
    }
}

test "range bounds require exact integer types and immutable lexical bindings" {
    for ([_]struct { body: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .body = "loop i in 0..2 {i = 1}", .tag = .immutable_assignment },
        .{ .body = "loop i in 0..2 {i += 1}", .tag = .immutable_assignment },
        .{ .body = "{loop i in 0..2 {}\n@println(i)}", .tag = .unknown_name },
        .{ .body = "{let i: int = 0\nloop i in 0..2 {}}", .tag = .unsupported_shadowing },
        .{ .body = "loop int in 0..2 {}", .tag = .unsupported_shadowing },
        .{ .body = "loop i in 0..2 {let i: int = 1\n}", .tag = .unsupported_shadowing },
        .{ .body = "{let a: i8 = -1\nlet b: i16 = 2\nloop i in a..b {}}", .tag = .expression_type_mismatch },
        .{ .body = "{let a: i8 = 1\nlet b: u8 = 2\nloop i in a..b {}}", .tag = .expression_type_mismatch },
        .{ .body = "{let a: i8 = -1\nloop i in a..128 {}}", .tag = .literal_out_of_range },
        .{ .body = "{let a: u8 = 1\nloop i in -1..a {}}", .tag = .literal_out_of_range },
        .{ .body = "loop i in true..false {}", .tag = .operator_not_supported },
        .{ .body = "loop i in 1 + 1..3 {}", .tag = .literal_needs_type },
        .{ .body = "loop i in missing..3 {}", .tag = .unknown_name },
        .{ .body = "loop i in 0..2 {break 1}", .tag = .conditional_value_break },
        .{ .body = "{let value: uint = loop i in 0..2 {}\n}", .tag = .expression_type_mismatch },
        .{ .body = "loop i in 0..2 {true}", .tag = .literal_type_mismatch },
        .{ .body = "loop i in 0..2 {continue\n@println(i)}", .tag = .unreachable_code },
        .{ .body = "loop i in 0..2 {@println(_)}", .tag = .unknown_name },
    }) |case| {
        for ([_][]const u8{ "..", "..=" }) |operator| {
            const body = try std.mem.replaceOwned(u8, std.testing.allocator, case.body, "..", operator);
            defer std.testing.allocator.free(body);
            const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> void = {s}", .{body});
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "range.nov", source);
            defer module.deinit();
            errdefer std.debug.print("range case: {s}\n", .{case.body});
            try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        }
    }
}

test "range else rejects incompatible results missing context and escaped indices or controls" {
    for ([_]struct { body: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .body = "loop i in 0..2 {} else {@println(i)}", .tag = .unknown_name },
        .{ .body = "loop i in i..2 {} else {}", .tag = .unknown_name },
        .{ .body = "loop _ in 0..2 {} else {break}", .tag = .loop_control_outside_loop },
        .{ .body = "loop _ in 0..2 {} else {continue}", .tag = .loop_control_outside_loop },
        .{ .body = "{let a = loop _ in 0..2 {break} else {true}\n}", .tag = .expression_type_mismatch },
        .{ .body = "{let a: int = loop _ in 0..2 {break} else {1}\n}", .tag = .expression_type_mismatch },
        .{ .body = "{let a = loop _ in 0..2 {break 1} else {2}\n}", .tag = .literal_needs_type },
        .{ .body = "{let x: i8 = 1\nlet y: i16 = 2\nlet a = loop _ in 0..2 {break x} else {y}\n}", .tag = .expression_type_mismatch },
        .{ .body = "{let a = loop _ in 0..2 {break []} else {[]}\n}", .tag = .array_literal_needs_type },
        .{ .body = "loop _ in 0..2 {} else {return}\n@println(true)", .tag = .unreachable_code },
    }) |case| {
        for ([_][]const u8{ "..", "..=" }) |operator| {
            const body = try std.mem.replaceOwned(u8, std.testing.allocator, case.body, "..", operator);
            defer std.testing.allocator.free(body);
            const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> void = {{{s}}}", .{body});
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "range-else.nov", source);
            defer module.deinit();
            errdefer std.debug.print("range else case: {s}\n", .{source});
            try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        }
    }
}

test "range literal defaults are native checked and never widen typed bounds" {
    const max: u128 = (@as(u128, 1) << Ir.native_pointer_bits) - 1;
    const min: i128 = -(@as(i128, 1) << (Ir.native_pointer_bits - 1));
    for ([_][]const u8{
        try std.fmt.allocPrint(std.testing.allocator, "loop _ in 0..{d} {{}}", .{max + 1}),
        try std.fmt.allocPrint(std.testing.allocator, "loop _ in {d}..0 {{}}", .{min - 1}),
        try std.fmt.allocPrint(std.testing.allocator, "loop _ in -1..{d} {{}}", .{@as(u128, 1) << (Ir.native_pointer_bits - 1)}),
    }) |body| {
        defer std.testing.allocator.free(body);
        for ([_][]const u8{ "..", "..=" }) |operator| {
            const bounds = try std.mem.replaceOwned(u8, std.testing.allocator, body, "..", operator);
            defer std.testing.allocator.free(bounds);
            const source = try std.mem.concat(std.testing.allocator, u8, &.{ "let f: () = ", bounds });
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "range.nov", source);
            defer module.deinit();
            var sema = try Lowering.lower(&module);
            defer sema.deinit();
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(Module.Diagnostic.Tag.literal_out_of_range, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        }
    }
}

test "condition loops diagnose controls types scope and unreachable tails" {
    for ([_]struct { body: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .body = "{break}", .tag = .loop_control_outside_loop },
        .{ .body = "{continue}", .tag = .loop_control_outside_loop },
        .{ .body = "loop true {break 1}", .tag = .conditional_value_break },
        .{ .body = "loop true {break :outer}", .tag = .unsupported_loop_control },
        .{ .body = "loop true {continue :outer}", .tag = .unsupported_loop_control },
        .{ .body = "loop 1 {}", .tag = .literal_type_mismatch },
        .{ .body = "loop {true}", .tag = .literal_type_mismatch },
        .{ .body = "{let n: int = loop {break}\n}", .tag = .expression_type_mismatch },
        .{ .body = "loop {break 1}", .tag = .literal_type_mismatch },
        .{ .body = "loop {continue :outer}", .tag = .unsupported_loop_control },
        .{ .body = "loop {continue\n@println(true)}", .tag = .unreachable_code },
        .{ .body = "{loop {break}\ncontinue}", .tag = .loop_control_outside_loop },
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "terminal value loops need no break value or never type" {
    for ([_][]const u8{ "loop {}", "loop {continue}", "loop {return 1}" }) |body| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () -> int = {s}", .{body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "loop.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    }
}

test "loop values infer from typed breaks with exact branch types and scopes" {
    const source =
        \\let id: (n: int) -> int = n
        \\let choose: (c: bool, n: int) -> int = loop {
        \\    if c {break 1}
        \\    let local = n
        \\    break local
        \\}
        \\let main: () -> void = {
        \\    let value = loop {let n: int = 0
        \\        break n}
        \\    let flag = loop {break true}
        \\    let text = loop {break "result" + "!"}
        \\    let array = loop {break [value, 1]}
        \\    let callback = loop {break id}
        \\    let nested = loop {let inner = loop {break callback(value)}
        \\        break inner}
        \\    let arithmetic = 1 + loop {break value}
        \\    let mut count: int = 0
        \\    loop count < 3 : {count += 1} {
        \\        if count == 1 {continue}
        \\        @println(count)
        \\    }
        \\    let from_update: int = loop : {break 7} {continue}
        \\    @println(value, flag, text, array[-1], nested, arithmetic, from_update, choose(false, 9))
        \\}
    ;
    var module = try Module.init(std.testing.allocator, "loop-values.nov", source);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "break operand probes do not borrow discarded nested loop result types" {
    for ([_][]const u8{
        "loop true {break n}",
        "loop {if false {break n}\nfalse} {} else {2}",
        "loop false : {break n} {} else {2}",
    }) |inner| {
        const source = try std.fmt.allocPrint(
            std.testing.allocator,
            "let f: () -> void = {{let n: i8 = 7\nlet x = loop true {{if false {{break 1}}\nbreak {{let unused = {s}\n2}}}} else {{3}}\n}}",
            .{inner},
        );
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "operand-scope.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.literal_needs_type, module.diagnostics.items[0].tag);
    }
}

test "conditional loop else results use exact bidirectional types and ignore terminal paths" {
    const source =
        \\let identity: (n: int) -> int = n
        \\let pick: (n: int) -> int = loop false {break 1} else {n}
        \\let out: () -> int = loop false {} else {return 7}
        \\let terminal: () -> int = loop true {break 4} else {return 8}
        \\let main: () -> void = {
        \\    let n: i16 = 3
        \\    let a = loop true {break 1} else {n}
        \\    let b = loop false {break n} else {2}
        \\    let c = loop {if false {break n}
        \\        true} {} else {1}
        \\    let d = loop false : if true {break n} {} else {2}
        \\    let e = loop true {break [1]} else {let seed: i8 = 2
        \\        [seed]}
        \\    let f = loop false {break identity} else {identity}
        \\    let g = loop false {break []} else {[true]}
        \\    let h = loop false {} else {loop true {break "x"} else {"y"}}
        \\    let i = loop {loop false {break} else {break 3}
        \\        break n}
        \\    @println(a, b, c, d, e[0], f(1), g.len, h, i)
        \\}
    ;
    var module = try Module.init(std.testing.allocator, "else.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    for (module.diagnostics.items) |diagnostic| std.debug.print("else diagnostic {s}\n", .{@tagName(diagnostic.tag)});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "loop value and continuation diagnostics preserve exact types and lexical scopes" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let f: () -> void = loop false {} else {break}", .tag = .loop_control_outside_loop },
        .{ .source = "let f: () -> void = loop false {} else {continue}", .tag = .loop_control_outside_loop },
        .{ .source = "let f: () -> bool = loop true {break} else {false}", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> bool = loop true {break true} else {1}", .tag = .literal_type_mismatch },
        .{ .source = "let f: (a: i8, b: i16) -> i8 = loop true {break a} else {b}", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> void = {let x = loop false {break 1} else {2}\n}", .tag = .literal_needs_type },
        .{ .source = "let f: () -> void = {let x = loop false {break []} else {[]}\n}", .tag = .array_literal_needs_type },
        .{ .source = "let f: () -> void = loop false {let hidden = true\n} else {@println(hidden)}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = loop {let hidden = true\nhidden} {} else {@println(hidden)}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = loop false : {let hidden = true\n} {} else {@println(hidden)}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = {loop false {} else {let hidden = true\n}\n@println(hidden)}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = {loop false {} else {return}\n@println(true)}", .tag = .unreachable_code },
        .{ .source = "let f: () -> void = loop true {break} else {return {}\n@println(true)}", .tag = .unreachable_code },
        .{ .source = "let f: () -> void = loop true {loop false {} else {break true}}", .tag = .conditional_value_break },
        .{ .source = "let f: () -> void = {let x = loop {break 1}\n}", .tag = .literal_needs_type },
        .{ .source = "let f: () -> int = loop {if true {break} else {break 1}}", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> int = loop {if true {break 1} else {break false}}", .tag = .literal_type_mismatch },
        .{ .source = "let f: (a: i8, b: i16) -> void = {let x = loop {if true {break a} else {break b}}\n}", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> int = loop {loop {break true}}", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> void = {loop true : {} {let hidden: int = 1\n}\n@println(hidden)}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = loop true : @println(hidden) {let hidden: int = 1\n}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = loop {let hidden = true\nhidden} : @println(hidden) {}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = loop true : true {}", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> void = loop true : {break 1} {}", .tag = .conditional_value_break },
        .{ .source = "let f: () -> void = {loop false : {let hidden = true\n} {}\n@println(hidden)}", .tag = .unknown_name },
        .{ .source = "let f: () -> void = {loop {if true {break missing}}}", .tag = .unknown_name },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "bad-loop.nov", case.source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        errdefer std.debug.print("loop diagnostic case: {s}\n", .{case.source});
        try std.testing.expect(module.hasErrors());
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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

        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
    var sema = try Sema.analyzeFunctions(&module);
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
        var sema = try Sema.analyzeFunctions(&module);
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
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Sema.analyzeFunctions(&module);
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

        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Sema.analyzeFunctions(&module);
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
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
    }
}

test "local shadowing reports both declarations and locals do not leak to other functions" {
    var module = try Module.init(std.testing.allocator, "scope.nov", "let first: () -> void = {let secret: i32 = 1\n{let secret: i32 = 2\n}\n}\nlet second: () -> i32 = secret");
    defer module.deinit();
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 2), module.diagnostics.items.len);
    const diagnostic = module.diagnostics.items[0];
    try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_shadowing, diagnostic.tag);
    try std.testing.expectEqualStrings("secret", module.sourceSlice(diagnostic.span).?);
    try std.testing.expectEqualStrings("secret", module.sourceSlice(diagnostic.related.?).?);
    try std.testing.expect(diagnostic.related.?.start < diagnostic.span.start);
    try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_name, module.diagnostics.items[1].tag);
}

test "rune frontend supports scalar boundaries matches printing and borrows" {
    for ([_][]const u8{
        "let Rune = rune\nlet identity: (x: Rune) -> Rune = x\nlet f: () -> rune = {let callback: (rune) -> rune = identity\nlet value = 'é'\ncallback(@as(Rune, value))}",
        "let R = struct{value: rune = 'é',}\nlet U = union{value: rune,}\nlet f: () -> rune = {let r = R[]\nlet u: U = .value[r.value]\nmatch u {.value => |v| v}}",
        "let f: (r: rune) -> rune = match r {'é' | '\\xE9' => '😀'\n_ => r}",
        "let f: () -> bool = {let mut r = 'a'\nr = 'é'\nlet b = &r\nb = '😀'\nb == '😀'}",
        "let f: () -> rune = {let mut xs: []rune = ['a', 'é']\nxs[0] = '😀'\nlet b = &xs[1]\nb = '\\0'\nb}",
        "let f: () -> rune = {let xs = ['é', '😀']\nxs[0]}",
        "let f: (r: rune) -> string = {let value = '😀'\n@println(r, value, '\\xE9')\n\"{r}:{value}\"}",
        "let f: () -> bool = 'é' != '😀'",
    }) |source| {
        errdefer std.debug.print("rune frontend fixture: {s}\n", .{source});
        var module = try Module.init(std.testing.allocator, "rune.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
    }
}

test "rune frontend specializes standard Option Result and generic struct fields" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "rune.nov", .data =
        \\let standard = @import("std")
        \\let Option = standard.Option
        \\let Result = standard.Result
        \\let Box: (T: type) -> type = struct { value: T, }
        \\let fallback: () -> rune = '😀'
        \\let recover: (error: rune) -> rune = error
        \\let run: () -> rune = {
        \\    let box = Box(rune)[.value = 'é']
        \\    let option: Option(rune) = .some[box.value]
        \\    let result: Result(rune, rune) = .ok[option.unwrapOr('a')]
        \\    let empty: Option(rune) = .none[]
        \\    let error: Result(rune, rune) = .err['é']
        \\    @println(empty.unwrapOrElse(fallback), error.unwrapOrElse(recover))
        \\    result.unwrapOr('\0')
        \\}
    });
    const path = try tmp.dir.realPathFileAlloc(io, "rune.nov", allocator);
    defer allocator.free(path);
    const lib = try std.Io.Dir.cwd().realPathFileAlloc(io, "lib", allocator);
    defer allocator.free(lib);
    var comp = try Compilation.load(allocator, io, path, .{ .lib_dir = lib });
    defer comp.deinit();
    var sema = try Lowering.lowerCompilation(&comp);
    defer sema.deinit();
    try std.testing.expect(!comp.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "rune frontend rejects conversions operators ranges and malformed literals" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: () -> rune = 97", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> rune = @as(rune, 97)", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> rune = \"a\"", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> int = 'a'", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> string = 'a'", .tag = .literal_type_mismatch },
        .{ .source = "let f: (x: int) -> rune = @as(rune, x)", .tag = .expression_type_mismatch },
        .{ .source = "let f: (r: rune) -> int = @as(int, r)", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> bool = 'a' == 97", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> bool = 'a' == \"a\"", .tag = .literal_type_mismatch },
        .{ .source = "let f: () -> bool = 'a' < 'b'", .tag = .operator_not_supported },
        .{ .source = "let f: () -> rune = 'a' + 'b'", .tag = .operator_not_supported },
        .{ .source = "let f: () -> rune = ~'a'", .tag = .operator_not_supported },
        .{ .source = "let f: () = {let range = 'a'..'z'\n}", .tag = .operator_not_supported },
        .{ .source = "let f: () = loop r in 'a'..='z' {}", .tag = .operator_not_supported },
        .{ .source = "let f: (r: rune) = loop value in r {}", .tag = .unsupported_iteration },
        .{ .source = "let f: () -> rune = 'a' & 'b'", .tag = .operator_not_supported },
        .{ .source = "let f: () = {let mut r = 'a'\nr += 'b'}", .tag = .operator_not_supported },
        .{ .source = "let f: () = {let mut xs: []rune = ['a']\nxs[0] += 'b'}", .tag = .operator_not_supported },
        .{ .source = "let f: (r: rune) -> rune = match r {'a'..='z' => r\n_ => 'a'}", .tag = .unsupported_match_pattern },
        .{ .source = "let f: () -> rune = ''", .tag = .invalid_literal },
        .{ .source = "let f: () -> rune = 'ab'", .tag = .invalid_literal },
        .{ .source = "let f: () -> rune = 'é'", .tag = .invalid_literal },
        .{ .source = "let f: () -> rune = '\\u{D800}'", .tag = .invalid_literal },
        .{ .source = "let f: () -> rune = '\\u{110000}'", .tag = .invalid_literal },
        .{ .source = "let f: () -> rune = '\xff'", .tag = .invalid_literal },
    }) |case| {
        errdefer std.debug.print("rune negative fixture: {s}\n", .{case.source});
        var module = try Module.init(std.testing.allocator, "rune-error.nov", case.source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expect(module.sourceSlice(module.diagnostics.items[0].span) != null);
    }
}

test "rune frontend diagnostics and bodies clean up on every allocation failure" {
    for ([_][]const u8{
        "let f: (r: rune) -> string = {let value = '😀'\n\"{r}:{value}\"}",
        "let f: (r: rune) -> rune = match r {'é' => '😀'\n_ => r}",
        "let f: () -> rune = '\\u{D800}'",
    }) |source| try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "printing accepts typed scalar arguments and returns void" {
    var module = try Module.init(std.testing.allocator, "print.nov", "let empty: () -> void = @print()\nlet line: () -> void = @println()\nlet out: (a: i8, b: u64, c: bool) -> void = @println(a, b, c, true, false, a + 1)");
    defer module.deinit();
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(Sema.Status.body_checked, entry.status);
}

test "printing rejects unresolved literals non-scalars unsupported builtins and nonvoid results" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f: () -> void = @println(1)", .tag = .literal_needs_type },
        .{ .source = "let n: int = 1\nlet f: () -> void = @println(2)", .tag = .literal_needs_type },
        .{ .source = "let n: uint = 1\nlet f: () -> int = n", .tag = .expression_type_mismatch },
        .{ .source = "let f: () -> void = @println(-1)", .tag = .literal_needs_type },
        .{ .source = "let f: () -> void = @println('ab')", .tag = .invalid_literal },
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
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
    }
}

test "function parameters literals arithmetic and forward calls type-check" {
    var module = try Module.init(std.testing.allocator, "functions.nov", "let caller: () -> i32 = add(1, 2)\nlet add: (a: i32, b: i32) -> i32 = {a + b}\nlet minimum: () -> i8 = -{128}\nlet empty: () = {}\nlet overflow_pending: () -> i8 = 127 + 1");
    defer module.deinit();
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| {
        try std.testing.expectEqual(Sema.Status.body_checked, entry.status);
        try std.testing.expect(entry.constant == null);
    }
    try sema.ir.verify();
}

test "mutual recursion and function-typed parameters use known signatures" {
    var module = try Module.init(std.testing.allocator, "recursive.nov", "let first: (x: i32) -> i32 = second(x)\nlet second: (x: i32) -> i32 = first(x)\nlet apply: (callback: (i32) -> i32, value: i32) -> i32 = callback(value)\nlet identity: (x: i32) -> i32 = x\nlet factory: () -> (i32) -> i32 = identity\nlet caller: () -> i32 = factory()(42)");
    defer module.deinit();
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(Sema.Status.body_checked, entry.status);
    try sema.ir.verify();
}

test "boolean expressions and comparisons infer operand literals from parameters" {
    var module = try Module.init(std.testing.allocator, "booleans.nov", "let less: (value: i32) -> bool = 0 < value\nlet both: (a: bool, b: bool) -> bool = a and !b\nlet equal: () -> bool = true == false\nlet bits: (value: u32) -> u32 = ~value\nlet negative: (value: i32) -> i32 = -value");
    defer module.deinit();
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    for (sema.entries) |entry| try std.testing.expectEqual(Sema.Status.body_checked, entry.status);
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
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try std.testing.expectEqual(Sema.Status.failed, sema.entries[sema.entries.len - 1].status);
        try sema.ir.verify();
    }
}

test "control flow and parameter shadowing stay explicitly unsupported" {
    for ([_][]const u8{
        "let f: () -> i32 = label: {42}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "unsupported.nov", source);
        defer module.deinit();
        var sema = try Sema.analyzeFunctions(&module);
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
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_shadowing, module.diagnostics.items[0].tag);
    }
}

test "unconstrained operands still diagnose unknown names and unsupported shapes" {
    var module = try Module.init(std.testing.allocator, "names.nov", "let f: () -> bool = missing == other");
    defer module.deinit();
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 2), module.diagnostics.items.len);
    for (module.diagnostics.items) |diagnostic| try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_name, diagnostic.tag);
    var unsupported = try Module.init(std.testing.allocator, "local.nov", "let f: () -> bool = {\nlet local = 1\nlocal\n} == 2");
    defer unsupported.deinit();
    var unsupported_sema = try Sema.analyzeFunctions(&unsupported);
    defer unsupported_sema.deinit();
    try std.testing.expectEqual(@as(usize, 1), unsupported.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.literal_needs_type, unsupported.diagnostics.items[0].tag);
}

test "parameter bindings do not leak between functions and annotated globals remain readable" {
    var module = try Module.init(std.testing.allocator, "scope.nov", "let global: i32 = 42\nlet read: () -> i32 = global\nlet first: (secret: i32) -> i32 = secret\nlet second: () -> i32 = secret");
    defer module.deinit();
    var sema = try Sema.analyzeFunctions(&module);
    defer sema.deinit();
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_name, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(Sema.Status.constant_checked, sema.entries[0].status);
    try std.testing.expectEqual(Sema.Status.body_checked, sema.entries[1].status);
    try std.testing.expectEqual(Sema.Status.body_checked, sema.entries[2].status);
    try std.testing.expectEqual(Sema.Status.failed, sema.entries[3].status);
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
        var sema = try Sema.analyzeFunctions(&module);
        defer sema.deinit();
        if (i == 0) {
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(Module.Diagnostic.Tag.body_nesting_limit, module.diagnostics.items[0].tag);
        } else {
            try std.testing.expect(!module.hasErrors());
            try std.testing.expectEqual(Sema.Status.body_checked, sema.entries[0].status);
        }
    }
}

fn testAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom.nov", source);
    defer module.deinit();
    var sema = Sema.analyzeFunctions(&module) catch |err| {
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
        "let f: (p: int) -> void = {let value = loop {if true {break 1}\nlet local = p\nbreak local}\n@println(value)}",
        "let f: (p: int) -> void = {let value = loop {if true {break 1}\nlet local: missing = p\nbreak local}\n}",
        "let f: () -> void = loop false : {let hidden = true\n} {@println(hidden)}",
        "let f: () -> int = loop {if true {break 1} else {break false}}",
    }) |source| {
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "iteration record loans respect owner mutability and reject capability escapes" {
    for ([_]struct { body: []const u8, tag: ?Module.Diagnostic.Tag = null }{
        .{ .body = "let mut a=[R[.inner=Cell[.n=1,.s=\"x\"]]]\nloop &r in a {r.inner.n+=2\nr.inner.s+=\"!\"\nr.inner=Cell[.n=3,.s=\"new\"]\nr=R[.inner=Cell[.n=4,.s=\"row\"]]}" },
        .{ .body = "let a=[R[.inner=Cell[.n=1,.s=\"x\"]]]\nloop &r in a {r.inner.n+=2}", .tag = .immutable_assignment },
        .{ .body = "let mut a=[R[.inner=Cell[.n=1,.s=\"x\"]]]\nloop r in a {r.inner.n=2}", .tag = .immutable_assignment },
        .{ .body = "let a=[R[.inner=Cell[.n=1,.s=\"x\"]]]\nloop &r in a {let copy=r\n@println(copy.inner.n)}" },
        .{ .body = "let mut a=[R[.inner=Cell[.n=1,.s=\"x\"]]]\nloop &r in a {return &r}", .tag = .unsupported_borrow },
        .{ .body = "let mut a=[R[.inner=Cell[.n=1,.s=\"x\"]]]\nloop &r in a {let escaped=[&r]\n}", .tag = .unsupported_borrow },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let Cell=struct{{n:int,s:string,}}\nlet R=struct{{inner:Cell,}}\nlet f:()={{\n{s}\n}}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "iteration-record.nov", source);
        defer module.deinit();
        errdefer std.debug.print("iteration record fixture: {s}\n", .{case.body});
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        if (case.tag) |tag| {
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
        } else {
            try std.testing.expect(!module.hasErrors());
            try std.testing.expect(sema.lowering_complete);
            try sema.ir.verify();
        }
    }
}

test "iteration validates copies scoped loans source protection and unsupported iterables" {
    const cases = [_]struct { body: []const u8, tag: ?Module.Diagnostic.Tag = null }{
        .{ .body = "let a:[]i8=[1,2]\nloop x in a {let n:i8=x\n@println(a[0])}" },
        .{ .body = "let a:[]int=[1,2]\nloop x in a {loop y in a {@println(x,y)}}" },
        .{ .body = "let mut a:[]int=[1]\nlet b=a\nloop x in a {let mut copy=x\ncopy+=1\n@println(copy,b[0])}" },
        .{ .body = "let mut s=\"é😀\"\nloop r in s {let n:rune=r\ns=\"other\"\n@println(n)}" },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {x+=1\nlet copy=x\n@println(copy)}\na+=[3]" },
        .{ .body = "let a:[]int=[1]\nloop &x in a {@println(x)}" },
        .{ .body = "let a:[]int=[]\nloop _ in a {} else {}\nloop &x in a {} else {@println(a.len)}" },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {x=loop _ in 0..1 {break x+1} else {x}}" },
        .{ .body = "let mut a:[]int=[1]\nlet r=&a\nloop x in r {@println(x)}" },
        .{ .body = "let a:[]int=[1]\nloop x in a {x=2}", .tag = .immutable_assignment },
        .{ .body = "let a:[]int=[1]\nloop &x in a {x+=2}", .tag = .immutable_assignment },
        .{ .body = "let mut a:[]int=[1]\nloop x in a {a[0]=2}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop x in a {a[0]+=2}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop x in a {a=[2]}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop x in a {a+=[2]}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop x in a {let r=&a[0]\nr=2}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop x in a {loop &y in a {}}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {@println(a[0])}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {@println(a.len)}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {a=[2]}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {a+=[2]}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {let r=&a[0]\n}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {loop y in a {}}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {let r=&x\n}", .tag = .unsupported_borrow },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {}\n@println(x)", .tag = .unknown_name },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {} else {@println(x)}", .tag = .unknown_name },
        .{ .body = "loop &x in [true] {}", .tag = .unsupported_iteration },
        .{ .body = "loop &x in \"é\" {}", .tag = .unsupported_iteration },
        .{ .body = "loop &x in 0..2 {}", .tag = .unsupported_iteration },
        .{ .body = "let a:[]int=[1]\nlet r=&a\nloop &x in r {}", .tag = .unsupported_iteration },
        .{ .body = "loop x in true {}", .tag = .unsupported_iteration },
        .{ .body = "loop x in f {}", .tag = .unsupported_iteration },
        .{ .body = "let mut a:[]int=[1]\nlet r=&a[0]\nloop x in a {r+=1}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nlet r=&a\nloop &x in a {@println(r[0])}", .tag = .borrow_conflict },
        .{ .body = "let mut a:[]int=[1]\nloop x in a {let r=&x\n@println(r)}" },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {let copy=x\n@println(copy)}\nlet r=&a[0]\nr=3" },
        .{ .body = "let mut a:[]int=[1]\nloop &x in a {x+=1\n@println(a)}", .tag = .borrow_conflict },
        .{ .body = "loop x in [true] {break true}", .tag = .conditional_value_break },
    };
    for (cases) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f:()={{\n{s}\n}}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "iteration.nov", source);
        defer module.deinit();
        errdefer std.debug.print("iteration case: {s}\n", .{case.body});
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        if (case.tag) |tag| {
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
            try std.testing.expect(!sema.lowering_complete);
        } else {
            try std.testing.expect(!module.hasErrors());
            try std.testing.expect(sema.lowering_complete);
            try sema.ir.verify();
        }
    }
}

test "reusable range values preserve exact inference and reject unsupported value uses" {
    const cases = [_]struct { body: []const u8, accepted: bool }{
        .{ .body = "{let r=0..2\nlet copy=r\nloop i in copy {let n: uint=i\n}}", .accepted = true },
        .{ .body = "{let r=-1..2\nloop i in r {let n: int=i\n}}", .accepted = true },
        .{ .body = "{let a: i8=-1\nlet r=a..2\nloop i in r {let n: i8=i\n}}", .accepted = true },
        .{ .body = "{let r=if true {0..2} else {0..=3}\nloop i in r {}}", .accepted = true },
        .{ .body = "{let r=loop _ in 0..1 {break 0..2} else {1..=2}\nloop i in r {}}", .accepted = true },
        .{ .body = "{let r=0..2\nloop _ in r {}\nloop _ in r {}}", .accepted = true },
        .{ .body = "{let r=1..2\nloop i in r {let n: int=i\n}}", .accepted = false },
        .{ .body = "{let r=-1..2\nloop i in r {let n: uint=i\n}}", .accepted = false },
        .{ .body = "{let a: i8=1\nlet b: i16=2\nlet r=a..b\n}", .accepted = false },
        .{ .body = "{let mut r=0..2\nr=-1..2}", .accepted = false },
        .{ .body = "{let mut r=0..2\nr.start=1}", .accepted = false },
        .{ .body = "{let r=0..2\n@println(r.start)}", .accepted = false },
        .{ .body = "{let r=0..2\nlet items=[r]\n}", .accepted = false },
        .{ .body = "{let r=true..false\n}", .accepted = false },
        .{ .body = "{loop i in true {}}", .accepted = false },
        .{ .body = "{let r=0..2\nloop i in r {} else {@println(i)}}", .accepted = false },
        .{ .body = "{let r=0..2\nlet n=42\n}", .accepted = false },
        .{ .body = "{let r=if true {@as(i8, 0)..2} else {0..2}\n}", .accepted = false },
        .{ .body = "{let r=0..2\nlet s=r..r\n}", .accepted = false },
        .{ .body = "{let r=1 + 1..3\n}", .accepted = false },
    };
    for (cases) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f: () = {s}", .{case.body});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "range-value-types.nov", source);
        defer module.deinit();
        errdefer std.debug.print("range value case: {s}\n", .{case.body});
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(case.accepted, sema.lowering_complete);
        try std.testing.expectEqual(!case.accepted, module.hasErrors());
        if (case.accepted) try sema.ir.verify();
        for (module.diagnostics.items) |diagnostic| {
            try std.testing.expect(diagnostic.span.start < diagnostic.span.end);
            try std.testing.expect(diagnostic.span.end <= source.len);
        }
    }
}

test "assert borrow CFG preserves branch loop continuation and fallback boundaries" {
    for ([_]struct { body: []const u8, conflict: bool = false }{
        .{ .body = "loop {@println(r)\nn=1\n@assert(true)}", .conflict = true },
        .{ .body = "loop {@println(r)\nn=1\n@assert(false)}", .conflict = true },
        .{ .body = "if flag {n=1\n@assert(true)} else {@println(r)}" },
        .{ .body = "if flag {@println(r)} else {n=1\n@assert(true)}" },
        .{ .body = "if flag {n=1\n@assert(false)} else {@println(r)}" },
        .{ .body = "if flag {n=1\n@assert(true)} else {@assert(true)}\n@println(r)", .conflict = true },
        .{ .body = "loop flag {@println(r)\nn=1\n@assert(true)}", .conflict = true },
        .{ .body = "loop flag : @assert(true) {@println(r)\nn=1}", .conflict = true },
        .{ .body = "loop flag : @println(r) {n=1\n@assert(true)}", .conflict = true },
        .{ .body = "loop {@println(r)\nif flag {n=1\n@assert(true)} else {continue}}", .conflict = true },
        .{ .body = "loop {if flag {loop {break}\n@println(r)\nn=1\n@assert(true)} else {continue}}", .conflict = true },
        .{ .body = "if flag {loop false {} else {n=1\n@assert(true)}} else {@println(r)}" },
        .{ .body = "loop false {} else {n=1\n@assert(true)}\n@println(r)", .conflict = true },
        .{ .body = "loop false {} else {@println(r)\n@assert(true)}\nn=1" },
        .{ .body = "@assert(r==0,{n=1\n\"snapshot\"})" },
        .{ .body = "@assert(false,{n=1\n\"{r}\"})", .conflict = true },
        .{ .body = "@assert(true,{n=1\n\"{r}\"})", .conflict = true },
        .{ .body = "@assert(false,\"{r}\")\nn=1" },
        .{ .body = "@assert(true)\nn=1\n@println(r)", .conflict = true },
        .{ .body = "@assert(false)\nn=1\n@println(r)", .conflict = true },
        .{ .body = "if flag {n=1\n@assert(true,{return})} else {@println(r)}" },
    }) |case| try testAssertBorrowCfg(std.testing.allocator, case.body, case.conflict);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAssertBorrowCfg, .{ "loop {@println(r)\nn=1\n@assert(true)}", true });
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAssertBorrowCfg, .{ "if flag {n=1\n@assert(true)} else {@println(r)}", false });
}

fn testAssertBorrowCfg(allocator: std.mem.Allocator, body: []const u8, conflict: bool) !void {
    const source = try std.fmt.allocPrint(allocator, "let f:(flag:bool)={{let mut n:int=0\nlet r=&n\n{s}\n}}", .{body});
    defer allocator.free(source);
    var module = try Module.init(allocator, "assert-borrow.nov", source);
    defer module.deinit();

    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    var result = try Lowering.lower(&module);
    defer result.deinit();
    if (conflict) {
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(Module.Diagnostic.Tag.borrow_conflict, module.diagnostics.items[0].tag);
        try std.testing.expectEqualStrings("&n", module.sourceSlice(module.diagnostics.items[0].related.?).?);
        try std.testing.expect(!result.lowering_complete);
    } else {
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        try result.ir.verify();
    }
}

test "static integer borrowing types CFG last use and bounded escapes" {
    const Case = struct { body: []const u8, diagnostic: ?Module.Diagnostic.Tag = null };
    for ([_]Case{
        .{ .body = "let y=&x; y+=1; y-=1; y*=2; y/=2; y%=2; @println(x)" },
        .{ .body = "let earlier=x; let y=&x; y+=1; @println(earlier,x); x=5" },
        .{ .body = "let y=&x; if flag {y+=1} else {@println(x)}; x=3" },
        .{ .body = "if flag {let y=&x; y+=1} else {let z=&x; z*=2}; @println(x)" },
        .{ .body = "let y=&x; loop flag {y+=1}; @println(x)" },
        .{ .body = "let y=&x; loop {y+=1; @println(x); break}; x=2" },
        .{ .body = "loop i in @as(int,0)..3 {let y=&x; y+=i; @println(x)}" },
        .{ .body = "let y=&x; loop {if flag {y+=1; break} else {@println(x); return}}; @println(x)" },
        .{ .body = "let y=&x; let b=flag and {y+=1; true}; @println(x,b)" },
        .{ .body = "let y=&x; let b=flag and {x=5; return}; y+=1; @println(b,x)" },
        .{ .body = "let y=&x; y+={return}" },
        .{ .body = "let y=&x; y+={@panic(\"stop\")}" },
        .{ .body = "let r=&a[0]; let y=&x; y+=1; r.n+=2; @println(x,a[0].n)" },
        .{ .body = "let y=&x; y+={let r=&a[0]; r.n+=1; 2}; @println(x,a[0].n)" },
        .{ .body = "let y=&x; @println(x); y+=1", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; x=5; y+=1", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; let z=&x; z+=1; y+=1", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; y+={x=5; 1}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; y+={x=5; return}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; loop {y+={x=5; break}}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; loop {y+={x=5; continue}}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; y+={@println(x); @panic(\"stop\")}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; loop {y+=1; @println(x); if flag {break}}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; loop {if flag {@println(x); continue} else {y+=1; break}}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; if flag {y+=1} else {@println(x)}; y+=1", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; let b=flag and {x=5; true}; y+=1; @println(b)", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; loop x==1 {y+=1}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; loop flag : @println(x) {y+=1}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; y+={let z=&x; z+=1; 1}", .diagnostic = .borrow_conflict },
        .{ .body = "let y=&x; y+=y" },
        .{ .body = "let y=&x; y=1" },
        .{ .body = "let y=&x; let z=y" },
        .{ .body = "let y=&x; @println(y)" },
        .{ .body = "let y=&x; return y", .diagnostic = .expression_type_mismatch },
        .{ .body = "let y=&x; let z=&y", .diagnostic = .unsupported_borrow },
        .{ .body = "let mut y=&x", .diagnostic = .unsupported_borrow },
        .{ .body = "let y:int=&x", .diagnostic = .unsupported_borrow },
        .{ .body = "let fixed:int=1; let y=&fixed" },
        .{ .body = "let mut b=true; let y=&b" },
        .{ .body = "let y=&a" },
        .{ .body = "let y=&x; y.n+=1", .diagnostic = .unsupported_assignment_target },
        .{ .body = "let y=&x; y+=@as(i8,1)", .diagnostic = .expression_type_mismatch },
    }) |case| {
        const text = try std.testing.allocator.dupe(u8, case.body);
        defer std.testing.allocator.free(text);
        for (text) |*byte| if (byte.* == ';') {
            byte.* = '\n';
        };
        const source = try std.fmt.allocPrint(std.testing.allocator, "let R=struct{{n:int=0,}}\nlet f:(flag:bool)={{let mut x:int=1\nlet mut a=[R[]]\n{s}\n}}", .{text});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "integer-borrow.nov", source);
        defer module.deinit();
        errdefer std.debug.print("integer borrow case: {s}; diagnostics: {any}\n", .{ case.body, module.diagnostics.items });
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var result = try Lowering.lower(&module);
        defer result.deinit();
        if (case.diagnostic) |tag| {
            try std.testing.expect(module.hasErrors());
            try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
            if (tag == .borrow_conflict) {
                const origin = module.diagnostics.items[0].related.?;
                try std.testing.expectEqualStrings("&x", module.sourceSlice(origin).?);
            }
        } else {
            try std.testing.expect(!module.hasErrors());
            try std.testing.expect(result.lowering_complete);
            try result.ir.verify();
        }
    }
    for ([_][]const u8{ "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64", "i128", "uint", "int" }) |name| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let f:()={{let mut x:{s}=1\nlet y=&x\ny+=1\ny-=1\ny*=2\ny/=2\ny%=2\n@println(x)}}", .{name});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "integer-width-borrow.nov", source);
        defer module.deinit();
        var result = try Lowering.lower(&module);
        defer result.deinit();
        errdefer std.debug.print("integer width {s}: {any}\n", .{ name, module.diagnostics.items });
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        try result.ir.verify();
    }
}

test "static borrowing structured last use conflicts and bounded escapes" {
    const Case = struct { body: []const u8, diagnostic: ?Module.Diagnostic.Tag = null };
    for ([_]Case{
        .{ .body = "let r=&a[0]; r.x+=1; @println(a.len,a[0].x)" },
        .{ .body = "let r=&a[0]; if flag {r.x+=1} else {@println(a.len)}; @println(a.len)" },
        .{ .body = "if flag {let r=&a[0]; r.x=1} else {let s=&a[0]; s.x=2}; @println(a.len)" },
        .{ .body = "loop i in 0..3 {let r=&a[0]; r.x+=1; @println(i,a.len)}" },
        .{ .body = "let r=&a[0]; loop {r.x+=1; break}; @println(a.len)" },
        .{ .body = "let r=&a[0]; loop {if flag {r.x+=1; break} else {@println(a.len); return}}; @println(a.len)" },
        .{ .body = "let r=&a[0]; let independent=[R[]]; let copy=independent; r.x=copy[0].x" },
        .{ .body = "let r=&a[0]; loop flag {r.x+=1; @println(a.len); break}; @println(a.len)" },
        .{ .body = "let r=&a[0]; loop flag {if flag {r.x+=1; @println(a.len); break} else {return}}" },
        .{ .body = "let r=&a[0]; loop flag {r.x+=1}; @println(a.len)" },
        .{ .body = "let r=&a[0]; let n:int={loop {r.x+=1; break}; @println(a.len); 1}; @println(n)" },
        .{ .body = "let r=&a[0]; loop {r.x+=1; loop {break}; @println(a.len); break}" },
        .{ .body = "let r=&a[0]; if flag {r.x+=1} else {@println(a.len)}; r.x+=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; loop {r.x+=1; loop {@println(a.len); break}; if flag {break}}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; loop {a.len==1} {r.x+=1}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; loop flag : @println(a.len) {r.x+=1}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; let b=flag and {a=[R[]]; return}; r.x=1; @println(b)" },
        .{ .body = "let r=&a[0]; let b=flag and {r.x=1; true}; @println(a.len,b)" },
        .{ .body = "let r=&a[0]; match flag {true=>r.x=1; false=>@println(a.len)}; @println(a.len)" },
        .{ .body = "let r=&a[0]; match flag {true=>r.x=1; false=>@println(a.len)}; r.x=2", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; return r", .diagnostic = .expression_type_mismatch },
        .{ .body = "let r=&a[0]; @println(a.len); r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; let b=a; r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; a=[R[]]; r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; a+=[R[]]; r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; a[0]=R[]; r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; let s=&a[-1]; r.x=1; s.x=2", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; r.x+={a=[R[]]; 1}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; r.x={a=[R[]]; 1}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[{a=[R[]]; 0}]; r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[{a=[R[]]; return}]; r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[{@println(a.len); 0}]; r.x=1", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; loop {r.x+=1; @println(a.len); if flag {break}}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; loop {r.x+=1; if flag {@println(a.len); continue} else {break}}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; loop {if flag {@println(a.len); continue} else {r.x+=1; break}}", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; let b=flag and {a=[R[]]; true}; r.x=1; @println(b)", .diagnostic = .borrow_conflict },
        .{ .body = "let r=&a[0]; let alias=r; alias.x=1", .diagnostic = .immutable_assignment },
        .{ .body = "let r=&a[0]; consume(r)" },
        .{ .body = "let r=&a[0]; let b:[]R=[r,r]" },
        .{ .body = "let r=&a[0]; let b={r}" },
        .{ .body = "let r=&a[0]; let s=&r.x", .diagnostic = .unsupported_borrow },
        .{ .body = "let r=&a[0]; r=R[]" },
        .{ .body = "let immutable=[R[]]; let r=&immutable[0]; @println(r.x)" },
        .{ .body = "let immutable=[R[]]; let r=&immutable[0]; r.x=1", .diagnostic = .immutable_assignment },
    }) |case| {
        const body_text = try std.testing.allocator.dupe(u8, case.body);
        defer std.testing.allocator.free(body_text);
        for (body_text) |*byte| if (byte.* == 59) {
            byte.* = 10;
        };
        const source = try std.fmt.allocPrint(std.testing.allocator, "let R=struct{{x:int=0,}}\nlet consume:(r:R)={{}}\nlet f:(flag:bool)={{let mut a=[R[]]\n{s}\n}}", .{body_text});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "borrow.nov", source);
        defer module.deinit();
        errdefer std.debug.print("borrow case: {s}; diagnostics: {any}\n", .{ case.body, module.diagnostics.items });
        errdefer std.debug.print("parse: {any} source: {s}", .{ module.source.ast.errors, source });
        var result = try Lowering.lower(&module);
        defer result.deinit();
        if (case.diagnostic) |tag| {
            try std.testing.expect(module.hasErrors());
            try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
            if (tag == .borrow_conflict or tag == .borrow_escape) try std.testing.expect(module.diagnostics.items[0].related != null);
        } else {
            try std.testing.expect(!module.hasErrors());
            try std.testing.expect(result.lowering_complete);
            try result.ir.verify();
        }
    }
}

test "general borrowing readonly parameters exact types conflicts and unsupported capabilities" {
    const Case = struct { ty: []const u8, init: []const u8, replacement: []const u8 };
    for ([_]Case{
        .{ .ty = "bool", .init = "true", .replacement = "false" },
        .{ .ty = "int", .init = "@as(int,1)", .replacement = "2" },
        .{ .ty = "string", .init = "\"a\"+\"b\"", .replacement = "\"c\"" },
        .{ .ty = "E", .init = "E.a", .replacement = ".b" },
        .{ .ty = "R", .init = "R[.s=\"a\"]", .replacement = "R[.s=\"b\"]" },
        .{ .ty = "U", .init = "U.some[\"a\"]", .replacement = ".none[]" },
        .{ .ty = "G(string)", .init = "G(string).some[\"a\"]", .replacement = ".none[]" },
        .{ .ty = "[]int", .init = "[@as(int,1)]", .replacement = "[2]" },
        .{ .ty = "(int)->int", .init = "inc", .replacement = "inc" },
    }) |case| {
        inline for (0..7) |mode| {
            const body = try std.fmt.allocPrint(std.testing.allocator, switch (mode) {
                0 => "let r=&x\nlet copy=r\nconsume(r)\nreturn copy",
                1 => "let r=&x\nr={s}\nreturn x",
                2 => "let r=&x\nlet copy=x\nconsume(r)\nreturn copy",
                3 => "let r=&x\nlet s=&x\nconsume(r)\nreturn s",
                4 => "let mut local=x\nlet r=&local\nr={s}\nreturn local",
                5 => "let local=x\nlet r=&local\nr={s}\nreturn local",
                else => "let local=x\nlet r=&local\nlet s=&r\nreturn s",
            }, if (mode == 1 or mode == 4 or mode == 5) .{case.replacement} else .{});
            defer std.testing.allocator.free(body);
            const source = try std.fmt.allocPrint(std.testing.allocator, "let E=enum{{a,b,}}\nlet R=struct{{s:string,}}\nlet U=union{{some:string,none,}}\nlet G:(T:type)->type=union{{some:T,none,}}\nlet inc:(x:int)->int=x+1\nlet consume:(v:{s})={{}}\nlet f:(x:{s})->{s}={{{s}\n}}\nlet entry:()={{let v:{s}={s}\nconsume(f(v))}}", .{ case.ty, case.ty, case.ty, body, case.ty, case.init });
            defer std.testing.allocator.free(source);
            var module = try Module.init(std.testing.allocator, "general-readonly.nov", source);
            defer module.deinit();
            var result = try Lowering.lower(&module);
            defer result.deinit();
            errdefer std.debug.print("case {s} mode {d}: {any}\n{s}\n", .{ case.ty, mode, module.diagnostics.items, source });
            if (mode == 0 or mode == 4) {
                try std.testing.expect(!module.hasErrors());
                try std.testing.expect(result.lowering_complete);
                try result.ir.verify();
            } else {
                const expected: Module.Diagnostic.Tag = switch (mode) {
                    1, 5 => .immutable_assignment,
                    2, 3 => .borrow_conflict,
                    else => .unsupported_borrow,
                };
                try std.testing.expect(module.hasErrors());
                try std.testing.expectEqual(expected, module.diagnostics.items[0].tag);
            }
        }
    }
}

test "general borrowing rejects void nonlocal capabilities wrong exact types and invalid operations" {
    const Case = struct { source: []const u8, diagnostic: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:()={let x:void={}\nlet r=&x\n}", .diagnostic = .unsupported_borrow },
        .{ .source = "let f:(x:void)={let r=&x\n}", .diagnostic = .invalid_parameter_type },
        .{ .source = "let x=true\nlet f:()={let r=&x\n}", .diagnostic = .unsupported_borrow },
        .{ .source = "let f:()={let mut x=true\nlet r=&x\nr=1}", .diagnostic = .literal_type_mismatch },
        .{ .source = "let f:()={let mut x:int=1\nlet r=&x\nr=@as(i8,2)}", .diagnostic = .expression_type_mismatch },
        .{ .source = "let R=struct{n:int,}\nlet S=struct{n:int,}\nlet f:()={let mut x=R[.n=1]\nlet r=&x\nr=S[.n=2]}", .diagnostic = .expression_type_mismatch },
        .{ .source = "let f:()={let mut x=true\nlet r=&x\nr+=true}", .diagnostic = .operator_not_supported },
        .{ .source = "let f:()={let mut x=\"a\"\nlet r=&x\nr-=\"b\"}", .diagnostic = .operator_not_supported },
        .{ .source = "let f:()={let x=[true]\nlet r=&x[0]\nr=false}", .diagnostic = .immutable_assignment },
        .{ .source = "let f:(x:[]string)={let r=&x[0]\nr+=\"b\"}", .diagnostic = .immutable_assignment },
        .{ .source = "let R=struct{n:int=0,}\nlet f:(x:R)={let r=&x\nr.n=1}", .diagnostic = .immutable_assignment },
        .{ .source = "let R=struct{n:int=0,}\nlet f:()={let mut x=R[]\nlet r=&x\nx.n=1\n@println(r.n)}", .diagnostic = .borrow_conflict },
        .{ .source = "let f:(x:bool)={let r=&x\n@println(x)\n@println(r)}", .diagnostic = .borrow_conflict },
        .{ .source = "let f:()={let x=true\nlet r=&x\n@println(x)\n@println(r)}", .diagnostic = .borrow_conflict },
        .{ .source = "let f:()={let mut x=[true]\nlet r=&x\nr[0]={x=[false]\nreturn}}", .diagnostic = .borrow_conflict },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "general-negative.nov", case.source);
        defer module.deinit();
        errdefer std.debug.print("{s}: {any}\n", .{ case.source, module.diagnostics.items });
        var result = try Lowering.lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(case.diagnostic, module.diagnostics.items[0].tag);
    }
}

test "general borrowing reserves record owners through terminal field assignment RHS" {
    for ([_][]const u8{ "n", "inner.n" }) |field| {
        for ([_][]const u8{ "=", "+=" }) |outer_op| {
            for ([_][]const u8{ "=", "+=" }) |inner_op| {
                for ([_][]const u8{ "return .none[]", "break", "continue", "@panic(\"stop\")", "stop().!" }) |exit| {
                    for ([_]bool{ true, false }) |conflicts| {
                        const owner = if (conflicts) "x" else "independent";
                        const source = try std.fmt.allocPrint(std.testing.allocator, "let I=struct{{n:int=0,}}\nlet R=struct{{n:int=0,inner:I,}}\nlet O:(T:type)->type=union{{some:T,none,}}\nlet stop:()->O(int)=.none[]\nlet f:(flag:bool)->O(int)={{let mut x=R[.inner=I[]]\nlet mut independent=R[.inner=I[]]\nlet r=&x\nloop {{if flag {{r.{s}{s}{{{s}.{s}{s}2\n{s}}}}} else {{break}}}}\n.some[0]}}", .{ field, outer_op, owner, field, inner_op, exit });
                        defer std.testing.allocator.free(source);
                        var module = try Module.init(std.testing.allocator, "terminal-field-reservation.nov", source);
                        defer module.deinit();
                        errdefer std.debug.print("{s}: {any}\n", .{ source, module.diagnostics.items });
                        var result = try Lowering.lower(&module);
                        defer result.deinit();
                        if (conflicts) {
                            try std.testing.expect(module.hasErrors());
                            const diagnostic = module.diagnostics.items[0];
                            try std.testing.expectEqual(Module.Diagnostic.Tag.borrow_conflict, diagnostic.tag);
                            try std.testing.expectEqualStrings("x", module.sourceSlice(diagnostic.span).?);
                            try std.testing.expectEqualStrings("&x", module.sourceSlice(diagnostic.related.?).?);
                        } else {
                            try std.testing.expect(!module.hasErrors());
                            try std.testing.expect(result.lowering_complete);
                            try result.ir.verify();
                        }
                    }
                }
            }
        }
    }
}
