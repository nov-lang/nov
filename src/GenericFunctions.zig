const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const Compilation = @import("Compilation.zig");
const TypeResolver = @import("TypeResolver.zig");
const Lowering = @import("Lowering.zig");
const Sema = @import("Sema.zig");
const TestAllocations = @import("TestAllocations.zig");

pub const max_terms = 8192;
pub const max_depth = 128;
pub const max_specialization_depth = 64;
pub const max_specializations = 1024;
pub const Capture = struct { name: []const u8, ty: Ir.TypeId, template: ?Ref = null };
pub const LocalTemplate = struct {
    proto: Ast.Node.Index,
    body: Ast.Node.Index,
    outer_bindings: []const Module.TypeBinding = &.{},
    captures: []const Capture = &.{},
    outer_variables: []const Variable = &.{},
    lexical_locals: []const Local = &.{},
    container: ?Module.DeclId = null,
    concrete_type: ?u32 = null,
};
pub const Ref = struct {
    module: *Module,
    id: Module.DeclId = @fromBackingInt(0),
    container_type: ?Ir.TypeId = null,
    local: ?*const LocalTemplate = null,

    pub fn function(self: Ref) struct { proto_node: Ast.Node.Index, body: Ast.Node.Index } {
        if (self.local) |local| return .{ .proto_node = local.proto, .body = local.body };
        const f = self.module.source.ast.declFunction(self.module.getDeclaration(self.id).?.node).?;
        return .{ .proto_node = f.proto_node, .body = f.body };
    }

    pub fn span(self: Ref) Module.Span {
        if (self.local) |local| return self.module.tokenSpan(self.module.source.ast.firstToken(local.proto));
        return self.module.getDeclaration(self.id).?.name_span;
    }

    fn isNoreturn(self: Ref) bool {
        return self.local == null and self.module.getDeclaration(self.id).?.noreturn;
    }
};

fn staticReference(module: *Module, node: Ast.Node.Index) ?Ref {
    const ast = &module.source.ast;
    if (ast.nodes.items(.tag)[node] != .field_access) return null;
    const data = ast.nodes.items(.data)[node];
    const name = ast.tokenSlice(data.rhs);
    var base: Ref = if (module.compilation) |comp| blk: {
        const ref = comp.reference(module, data.lhs) orelse return null;
        break :blk .{ .module = comp.getModule(ref.source).?, .id = ref.local };
    } else blk: {
        if (ast.nodes.items(.tag)[data.lhs] != .identifier) return null;
        break :blk .{ .module = module, .id = module.lookup(ast.tokenSlice(ast.firstToken(data.lhs))) orelse return null };
    };
    for (0..max_depth) |_| {
        const decl = base.module.getDeclaration(base.id).?;
        if (decl.is_record) {
            if (decl.is_generic) return null;
            const id = base.module.lookupMethod(base.id, name) orelse return null;
            const method = base.module.getDeclaration(id).?;
            if (base.module != module and !method.public) return null;
            return .{ .module = base.module, .id = id };
        }
        const full = base.module.source.ast.decl(decl.node).ast;
        if (full.is_mutable or full.type_node != 0 or full.init_node == 0) return null;
        if (module.compilation) |comp| {
            const ref = comp.reference(base.module, full.init_node) orelse return null;
            base = .{ .module = comp.getModule(ref.source).?, .id = ref.local };
        } else {
            const owner_ast = &base.module.source.ast;
            if (owner_ast.nodes.items(.tag)[full.init_node] != .identifier) return null;
            base.id = base.module.lookup(owner_ast.tokenSlice(owner_ast.firstToken(full.init_node))) orelse return null;
        }
    }
    return null;
}

/// Follow immutable aliases in their defining scopes, not the caller's scope.
pub fn target(module: *Module, node: Ast.Node.Index) ?Ref {
    return targetWith(module, node, true);
}

fn targetWith(module: *Module, node: Ast.Node.Index, generic_only: bool) ?Ref {
    var owner = module;
    var current = node;
    for (0..max_depth) |_| {
        const ast = &owner.source.ast;
        const ref: Ref = if (owner.compilation) |comp| blk: {
            const r = comp.reference(owner, current) orelse break :blk (staticReference(owner, current) orelse return null);
            break :blk .{ .module = comp.getModule(r.source).?, .id = r.local };
        } else blk: {
            if (ast.nodes.items(.tag)[current] == .field_access) break :blk staticReference(owner, current) orelse return null;
            if (ast.nodes.items(.tag)[current] != .identifier) return null;
            break :blk .{ .module = owner, .id = owner.lookup(ast.tokenSlice(ast.firstToken(current))) orelse return null };
        };
        const declaration = ref.module.getDeclaration(ref.id).?;
        if ((declaration.is_generic_function or (!generic_only and !declaration.is_generic and !declaration.is_type_alias)) and ref.module.source.ast.declFunction(declaration.node) != null) return ref;
        const full = ref.module.source.ast.decl(declaration.node).ast;
        if (full.is_mutable or full.type_node != 0 or full.init_node == 0) return null;
        owner = ref.module;
        current = full.init_node;
    }
    return null;
}

pub const Argument = struct { ty: ?Ir.TypeId, span: Module.Span, template: ?Ref = null, terminal: bool = false };
const Term = struct {
    span: Module.Span,
    value: union(enum) {
        unknown,
        never,
        link: *Term,
        known: Ir.TypeId,
        array: *Term,
        function: Function,
        nominal: Nominal,
    },
};
const Nominal = struct { module: *Module, id: Module.DeclId, args: []const *Term };
const Function = struct { params: []const *Term, result: *Term };
const Variable = struct { name: []const u8, term: *Term };
const Local = struct { name: []const u8, term: *Term, template: ?Ref = null };
const Loop = struct { result: *Term, has_break: bool = false, has_continue: bool = false, allows_value: bool };
const Frame = struct {
    ref: Ref,
    variables: []const Variable,
    locals: std.ArrayList(Local) = .empty,
    result: *Term,
    container: ?Nominal = null,
    loop: ?*Loop = null,
};

/// Generates only type constraints. It never evaluates arguments, checks a body
/// speculatively, captures a value, or emits IR instructions.
pub fn infer(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, expected: ?Argument, call_span: Module.Span) Ir.BuildError!?[]const Module.TypeBinding {
    return inferInner(caller, ir, ref, arguments, expected, call_span) catch |err| switch (err) {
        error.TooManyEntries => null,
        else => return err,
    };
}

fn inferInner(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, expected: ?Argument, call_span: Module.Span) Ir.BuildError!?[]const Module.TypeBinding {
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = call_span };
    const frame = (try solver.constrain(ref, arguments, expected)) orelse return null;
    for (frame.variables) |variable| {
        if (!Solver.resolved(variable.term, 0)) {
            try solver.fail(if (solver.unsupported != null) .generic_function_inference_unsupported else .generic_function_ambiguous, solver.unsupported orelse call_span, variable.term.span);
            return null;
        }
    }
    const bindings = try caller.allocator.alloc(Module.TypeBinding, frame.variables.len);
    var transferred = false;
    defer if (!transferred) caller.allocator.free(bindings);
    for (frame.variables, bindings) |variable, *binding| {
        const ty = (try solver.materialize(variable.term, 0)) orelse {
            if (!solver.failed) try caller.diagnostics.append(caller.allocator, .{ .tag = .generic_function_ambiguous, .span = call_span, .related = variable.term.span });
            return null;
        };
        binding.* = .{ .name = variable.name, .ty = ty };
    }
    transferred = true;
    return bindings;
}

/// A call with a terminal operand never demands its callee. Resolve only the
/// reachable parameter contexts; unbound variables need no specialization.
/// The caller owns the returned slice, which retains the full signature arity.
pub fn terminalParameters(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, call_span: Module.Span) Ir.BuildError!?[]?Ir.TypeId {
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = call_span };
    _ = (solver.constrain(ref, arguments, null) catch |err| switch (err) {
        error.TooManyEntries => return null,
        else => return err,
    }) orelse return null;
    const params = try caller.allocator.alloc(?Ir.TypeId, solver.parameters.len);
    errdefer caller.allocator.free(params);
    solver.read_only = true;
    var terminal = false;
    for (arguments, solver.parameters, params) |arg, param, *ty| {
        ty.* = if (!terminal and Solver.resolved(param, 0)) try solver.materialize(param, 0) else null;
        terminal = terminal or arg.terminal;
    }
    return params;
}

/// A speculative result query may intern canonical structural and nominal
/// layouts, but never publishes methods, function specializations, declaration
/// IDs, or worklist requests.
pub fn probeResult(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, call_span: Module.Span) Ir.BuildError!?Ir.TypeId {
    const caller_start = caller.diagnostics.items.len;
    defer caller.diagnostics.shrinkRetainingCapacity(caller_start);
    const owner_start = ref.module.diagnostics.items.len;
    defer ref.module.diagnostics.shrinkRetainingCapacity(owner_start);
    const starts: []usize = if (caller.compilation) |comp| try caller.allocator.alloc(usize, comp.modules.items.len) else &.{};
    defer if (starts.len != 0) caller.allocator.free(starts);
    if (caller.compilation) |comp| for (comp.modules.items, starts) |module, *start| {
        start.* = module.diagnostics.items.len;
    };
    defer if (caller.compilation) |comp| {
        for (comp.modules.items, starts) |module, start| module.diagnostics.shrinkRetainingCapacity(start);
    };
    return probeResultInner(caller, ir, ref, arguments, call_span) catch |err| switch (err) {
        error.TooManyEntries => null,
        else => return err,
    };
}

fn probeResultInner(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, call_span: Module.Span) Ir.BuildError!?Ir.TypeId {
    for (arguments) |arg| if (arg.terminal) return null;
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = call_span, .read_only = true };
    const frame = (try solver.constrain(ref, arguments, null)) orelse return null;
    for (frame.variables) |variable| if (!Solver.resolved(variable.term, 0)) return null;
    return solver.materialize(frame.result, 0);
}

fn testSpecialization(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "generic-functions.nov",
        \\let first: (items: []`T) -> `T = items[0]
        \\let add: (x: `a, y: `b) -> `c = x + y
        \\let alias = add
        \\let unused: (x: `T) -> `T = missing
        \\let main: () = {
        \\let xs: []uint = [1, 2]
        \\let a = first(xs)
        \\let b = add(@as(uint, 1), 2)
        \\let c: uint = alias(1, 2)
        \\let s = add("a", "b")
        \\@print(a, b, c, s)
        \\}
    );
    defer module.deinit();
    const original_count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(original_count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("backtick diagnostics: {any}, syntax: {any}\n", .{ module.diagnostics.items, module.source.ast.errors });
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    var specializations: usize = 0;
    for (module.declarations.items) |decl| if (decl.function_template != null) {
        specializations += 1;
    };
    try std.testing.expectEqual(@as(usize, 3), specializations);
    try sema.ir.verify();
    var repeated = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(original_count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer repeated.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(original_count + 3, module.declarations.items.len);
    try std.testing.expectEqual(sema.ir.functions.items.len, repeated.ir.functions.items.len);
    try sema.ir.verify();
    try repeated.ir.verify();
}

test "backtick generic function exact constraints demand and canonical aliases" {
    try testSpecialization(std.testing.allocator);
}

test "backtick generic function specialization allocation failures" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testSpecialization, .{});
}

const Solver = struct {
    allocator: std.mem.Allocator,
    caller: *Module,
    ir: *Ir,
    call_span: Module.Span,
    count: usize = 0,
    failed: bool = false,
    active: std.ArrayList(Ref) = .empty,
    read_only: bool = false,
    unsupported: ?Module.Span = null,
    parameters: []const *Term = &.{},

    fn constrain(self: *Solver, ref: Ref, arguments: []const Argument, expected: ?Argument) Ir.BuildError!?Frame {
        const container: ?Nominal = if (ref.container_type) |ty| blk: {
            const nominal = root(try self.fromType(ty, self.call_span, 0));
            break :blk if (nominal.value == .nominal) nominal.value.nominal else null;
        } else null;
        const instance = (try self.signature(ref, container, 0)) orelse return null;
        var frame = instance.frame;
        const signature_type = instance.function;
        self.parameters = signature_type.params;
        if (signature_type.params.len != arguments.len) {
            try self.fail(.argument_count_mismatch, self.call_span, ref.span());
            return null;
        }
        for (arguments, signature_type.params) |arg, param| {
            if (arg.terminal) return if (self.failed) null else frame;
            if (arg.ty) |ty| try self.unify(param, try self.fromType(ty, arg.span, 0), arg.span, 0);
            if (arg.template) |callback| {
                const callback_container: ?Nominal = if (callback.container_type) |ty| blk: {
                    const nominal = root(try self.fromType(ty, arg.span, 0));
                    break :blk if (nominal.value == .nominal) nominal.value.nominal else null;
                } else null;
                const candidate = (try self.signature(callback, callback_container, 0)) orelse return null;
                var callback_frame = candidate.frame;
                try self.unify(param, try self.term(.{ .function = candidate.function }, arg.span), arg.span, 0);
                if (!resolved(param, 0)) try self.body(&callback_frame, 0);
            }
        }
        if (!ref.isNoreturn()) {
            if (expected) |arg| if (arg.ty) |ty| try self.unify(signature_type.result, try self.fromType(ty, arg.span, 0), arg.span, 0);
        }
        try self.body(&frame, 0);
        return if (self.failed) null else frame;
    }

    fn resolved(term_value: *Term, depth: usize) bool {
        if (depth >= max_depth) return false;
        return switch (root(term_value).value) {
            .unknown, .never => false,
            .link => unreachable,
            .known => true,
            .array => |element| resolved(element, depth + 1),
            .function => |f| blk: {
                for (f.params) |param| if (!resolved(param, depth + 1)) break :blk false;
                break :blk resolved(f.result, depth + 1);
            },
            .nominal => |n| blk: {
                for (n.args) |arg| if (!resolved(arg, depth + 1)) break :blk false;
                break :blk true;
            },
        };
    }

    fn fail(self: *Solver, tag: Module.Diagnostic.Tag, span: Module.Span, related: ?Module.Span) Ir.BuildError!void {
        if (self.failed) return;
        self.failed = true;
        if (self.read_only) return;
        try self.caller.diagnostics.append(self.caller.allocator, .{ .tag = tag, .span = span, .related = related });
    }

    fn term(self: *Solver, value: @FieldType(Term, "value"), span: Module.Span) Ir.BuildError!*Term {
        if (self.count >= max_terms) {
            try self.fail(.generic_function_limit, self.call_span, span);
            // Keep traversals finite even after a limit has been diagnosed.
            return error.TooManyEntries;
        }
        self.count += 1;
        const result = try self.allocator.create(Term);
        result.* = .{ .value = value, .span = span };
        return result;
    }

    fn root(t: *Term) *Term {
        var current = t;
        while (current.value == .link) current = current.value.link;
        return current;
    }

    fn fromType(self: *Solver, ty: Ir.TypeId, span: Module.Span, depth: usize) Ir.BuildError!*Term {
        if (depth >= max_depth) {
            try self.fail(.generic_function_limit, self.call_span, span);
            return self.term(.unknown, span);
        }
        return switch (self.ir.getType(ty).?) {
            .array => |element| self.term(.{ .array = try self.fromType(element, span, depth + 1) }, span),
            .function => |f| blk: {
                const params = try self.allocator.alloc(*Term, f.params.len);
                for (f.params, params) |param, *out| out.* = try self.fromType(param, span, depth + 1);
                break :blk self.term(.{ .function = .{ .params = params, .result = try self.fromType(f.result, span, depth + 1) } }, span);
            },
            .record => |r| blk: {
                if (r.type_args.len == 0) break :blk self.term(.{ .known = ty }, span);
                const owner = if (self.caller.compilation) |comp| comp.getModule(r.source).? else self.caller;
                const args = try self.allocator.alloc(*Term, r.type_args.len);
                for (r.type_args, args) |arg, *out| out.* = try self.fromType(arg, span, depth + 1);
                break :blk self.term(.{ .nominal = .{ .module = owner, .id = @fromBackingInt(r.identity), .args = args } }, span);
            },
            else => self.term(.{ .known = ty }, span),
        };
    }

    fn occurs(t: *Term, in: *Term, depth: usize) bool {
        const r = root(in);
        if (t == r or depth >= max_depth) return true;
        return switch (r.value) {
            .array => |element| occurs(t, element, depth + 1),
            .function => |f| blk: {
                for (f.params) |p| if (occurs(t, p, depth + 1)) break :blk true;
                break :blk occurs(t, f.result, depth + 1);
            },
            .nominal => |n| blk: {
                for (n.args) |arg| if (occurs(t, arg, depth + 1)) break :blk true;
                break :blk false;
            },
            else => false,
        };
    }

    fn unify(self: *Solver, a: *Term, b: *Term, span: Module.Span, depth: usize) Ir.BuildError!void {
        if (self.failed) return;
        if (depth >= max_depth) return self.fail(.generic_function_limit, self.call_span, span);
        const x = root(a);
        const y = root(b);
        if (x == y or x.value == .never or y.value == .never) return;
        if (x.value == .unknown or y.value == .unknown) {
            const variable = if (x.value == .unknown) x else y;
            const value = if (x.value == .unknown) y else x;
            if (occurs(variable, value, 0)) return self.fail(.generic_function_conflict, span, variable.span);
            variable.value = .{ .link = value };
            return;
        }
        if (std.meta.activeTag(x.value) == std.meta.activeTag(y.value)) switch (x.value) {
            .known => |ty| if (ty == y.value.known) return,
            .array => |element| return self.unify(element, y.value.array, span, depth + 1),
            .function => |f| {
                const g = y.value.function;
                if (f.params.len == g.params.len) {
                    for (f.params, g.params) |p, q| try self.unify(p, q, span, depth + 1);
                    return self.unify(f.result, g.result, span, depth + 1);
                }
            },
            .nominal => |n| {
                const m = y.value.nominal;
                if (n.module == m.module and n.id == m.id and n.args.len == m.args.len) {
                    for (n.args, m.args) |p, q| try self.unify(p, q, span, depth + 1);
                    return;
                }
            },
            else => unreachable,
        };
        try self.fail(.generic_function_conflict, y.span, x.span);
    }

    fn signatureVariablesBound(frame: *Frame, node: Ast.Node.Index) bool {
        const ast = &frame.ref.module.source.ast;
        for (ast.tokens.items(.tag)[ast.firstToken(node) .. ast.lastToken(node) + 1], ast.firstToken(node)..) |tag, token| {
            if (tag != .type_variable) continue;
            const name = ast.tokenSlice(@intCast(token));
            for (frame.variables) |variable| {
                if (std.mem.eql(u8, variable.name, name)) break;
            } else return false;
        }
        return true;
    }

    fn typeNode(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const span = module.tokenSpan(ast.firstToken(node));
        if (depth >= max_depth) {
            try self.fail(.generic_function_limit, self.call_span, span);
            return self.term(.unknown, span);
        }
        switch (ast.nodes.items(.tag)[node]) {
            .identifier => if (frame.container) |container| {
                const name = ast.tokenSlice(ast.firstToken(node));
                const owner_ast = &container.module.source.ast;
                const owner_decl = container.module.getDeclaration(container.id).?;
                const full = owner_ast.decl(owner_decl.node).ast;
                if (owner_decl.is_generic) {
                    var iterator = owner_ast.fullFnProto(full.type_node).?.iterate(owner_ast);
                    for (container.args) |arg| {
                        const param = iterator.next().?;
                        if (std.mem.eql(u8, owner_ast.tokenSlice(param.name_token.?), name)) return arg;
                    }
                }
                for (owner_ast.fullStruct(full.init_node).?) |member| {
                    if (!TypeResolver.isThisAlias(owner_ast, member)) continue;
                    if (std.mem.eql(u8, owner_ast.tokenSlice(owner_ast.decl(member).ast.let_token + 1), name)) return self.term(.{ .nominal = container }, span);
                }
            },
            .type_variable => {
                const name = ast.tokenSlice(ast.firstToken(node));
                for (frame.variables) |v| if (std.mem.eql(u8, v.name, name)) return v.term;
                try self.fail(.unbound_type_variable, span, null);
                return self.term(.unknown, span);
            },
            .array_type => return self.term(.{ .array = try self.typeNode(frame, ast.nodes.items(.data)[node].rhs, depth + 1) }, span),
            .fn_proto, .fn_proto_one => {
                const proto = ast.fullFnProto(node).?;
                const params = try self.allocator.alloc(*Term, proto.ast.params.len);
                var iterator = proto.iterate(ast);
                for (params) |*param| param.* = try self.typeNode(frame, iterator.next().?.type_expr, depth + 1);
                const result = if (proto.ast.return_type) |r| try self.typeNode(frame, r, depth + 1) else try self.fromType(try self.ir.internType(.void), span, depth + 1);
                return self.term(.{ .function = .{ .params = params, .result = result } }, span);
            },
            .call, .call_one => {
                const call = ast.fullCall(node).?;
                if (frame.container != null and call.ast.args.len == 0 and std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This")) return self.term(.{ .nominal = frame.container.? }, span);
                if (TypeResolver.genericReference(module, call.ast.fn_expr)) |ref| {
                    const owner = if (module.compilation) |comp| comp.getModule(ref.source).? else module;
                    const declaration = owner.getDeclaration(ref.local).?;
                    const proto = owner.source.ast.fullFnProto(owner.source.ast.decl(declaration.node).ast.type_node).?;
                    if (call.ast.args.len != proto.ast.params.len) {
                        try self.fail(.generic_argument_count, span, declaration.name_span);
                        return self.term(.unknown, span);
                    }
                    const args = try self.allocator.alloc(*Term, call.ast.args.len);
                    for (call.ast.args, args) |arg, *out| out.* = try self.typeNode(frame, arg, depth + 1);
                    return self.term(.{ .nominal = .{ .module = owner, .id = ref.local, .args = args } }, span);
                }
            },
            else => {},
        }
        const decl = if (frame.ref.local == null) module.getDeclaration(frame.ref.id) else null;
        const start = module.diagnostics.items.len;
        defer if (self.read_only) module.diagnostics.shrinkRetainingCapacity(start);
        const ty = if (frame.container != null)
            try TypeResolver.resolveInScopeReadOnly(module, self.ir, node, null, null, &.{})
        else
            try TypeResolver.resolveInScopeReadOnly(module, self.ir, node, if (frame.ref.local) |local| local.container else decl.?.container, if (frame.ref.local) |local| local.concrete_type else decl.?.concrete_type, if (frame.ref.local) |local| local.outer_bindings else decl.?.function_bindings);
        return if (ty) |known| self.fromType(known, span, depth + 1) else self.term(.unknown, span);
    }

    fn signature(self: *Solver, ref: Ref, container: ?Nominal, depth: usize) Ir.BuildError!?struct { frame: Frame, function: Function } {
        if (depth >= max_depth or self.failed) return null;
        const module = ref.module;
        const ast = &module.source.ast;
        const function = ref.function();
        var variables: std.ArrayList(Variable) = .empty;
        for (ast.tokens.items(.tag)[ast.firstToken(function.proto_node) .. ast.lastToken(function.proto_node) + 1], ast.firstToken(function.proto_node)..) |tag, token| {
            if (tag != .type_variable) continue;
            const name = ast.tokenSlice(@intCast(token));
            var found = false;
            for (variables.items) |v| if (std.mem.eql(u8, v.name, name)) {
                found = true;
                break;
            };
            if (!found) {
                var variable_term = try self.term(.unknown, module.tokenSpan(@intCast(token)));
                if (ref.local) |local| {
                    for (local.outer_variables) |variable| {
                        if (std.mem.eql(u8, variable.name, name)) {
                            variable_term = variable.term;
                            break;
                        }
                    }
                    for (local.outer_bindings) |binding| {
                        if (std.mem.eql(u8, binding.name, name)) {
                            variable_term = try self.fromType(binding.ty, module.tokenSpan(@intCast(token)), 0);
                            break;
                        }
                    }
                }
                try variables.append(self.allocator, .{ .name = name, .term = variable_term });
            }
        }
        if (ref.local) |local| for (local.outer_bindings) |binding| {
            var found = false;
            for (variables.items) |v| if (std.mem.eql(u8, v.name, binding.name)) {
                found = true;
                break;
            };
            if (!found) try variables.append(self.allocator, .{ .name = binding.name, .term = try self.fromType(binding.ty, ref.span(), 0) });
        };
        if (ref.local) |local| for (local.outer_variables) |variable| {
            var found = false;
            for (variables.items) |v| if (std.mem.eql(u8, v.name, variable.name)) {
                found = true;
                break;
            };
            if (!found) try variables.append(self.allocator, variable);
        };
        var frame: Frame = .{ .ref = ref, .variables = variables.items, .result = undefined, .container = container };
        if (ref.local) |local| try frame.locals.appendSlice(self.allocator, local.lexical_locals);
        if (ref.local) |local| for (local.captures) |capture| {
            try frame.locals.append(self.allocator, .{ .name = capture.name, .term = try self.fromType(capture.ty, ref.span(), 0), .template = capture.template });
        };
        const signature_term = try self.typeNode(&frame, function.proto_node, depth + 1);
        if (self.failed or signature_term.value != .function) return null;
        const f = signature_term.value.function;
        frame.result = f.result;
        var iterator = ast.fullFnProto(function.proto_node).?.iterate(ast);
        for (f.params) |param| {
            const p = iterator.next().?;
            if (p.name_token) |token| try frame.locals.append(self.allocator, .{ .name = ast.tokenSlice(token), .term = param });
        }
        return .{ .frame = frame, .function = f };
    }

    fn body(self: *Solver, frame: *Frame, depth: usize) Ir.BuildError!void {
        if (self.failed) return;
        for (self.active.items) |ref| if (ref.module == frame.ref.module and ref.id == frame.ref.id and ref.local == frame.ref.local) return;
        if (depth >= max_specialization_depth) return self.fail(.generic_function_limit, self.call_span, null);
        try self.active.append(self.allocator, frame.ref);
        defer _ = self.active.pop();
        const ast = &frame.ref.module.source.ast;
        const function = frame.ref.function();
        if (function.body != 0) try self.unify(frame.result, try self.expression(frame, function.body, depth + 1), frame.ref.module.tokenSpan(ast.firstToken(function.body)), 0);
    }

    const CallTarget = struct { ref: Ref, container: ?Nominal = null, receiver: ?*Term = null };

    fn callTarget(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!?CallTarget {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        if (ast.nodes.items(.tag)[node] == .identifier) {
            const name = ast.tokenSlice(ast.firstToken(node));
            var i = frame.locals.items.len;
            while (i != 0) {
                i -= 1;
                const local = frame.locals.items[i];
                if (std.mem.eql(u8, local.name, name)) return if (local.template) |ref| .{ .ref = ref, .container = frame.container } else null;
            }
            const decl = if (frame.ref.local == null) module.getDeclaration(frame.ref.id) else null;
            const scope = if (frame.ref.local) |local| local.container else decl.?.container;
            const concrete = if (frame.ref.local) |local| local.concrete_type else decl.?.concrete_type;
            if (scope) |container| if (module.lookupConcreteMethod(container, concrete, name)) |id| {
                if (ast.declFunction(module.getDeclaration(id).?.node) == null) return null;
                return .{ .ref = .{ .module = module, .id = id }, .container = frame.container };
            };
        }
        if (ast.nodes.items(.tag)[node] == .field_access) {
            const base = ast.nodes.items(.data)[node].lhs;
            if (ast.nodes.items(.tag)[base] == .identifier) {
                const name = ast.tokenSlice(ast.firstToken(base));
                for (frame.locals.items) |local| if (std.mem.eql(u8, local.name, name)) return null;
            }
        }
        if (targetWith(module, node, false)) |ref| return .{ .ref = ref };
        if (ast.nodes.items(.tag)[node] == .field_access) {
            const data = ast.nodes.items(.data)[node];
            if (ast.fullCall(data.lhs)) |base_call| {
                if (TypeResolver.genericReference(module, base_call.ast.fn_expr) == null and !std.mem.eql(u8, ast.tokenSlice(ast.firstToken(base_call.ast.fn_expr)), "@This")) return null;
                const base = root(try self.typeNode(frame, data.lhs, depth + 1));
                if (base.value == .nominal) {
                    const nominal = base.value.nominal;
                    const id = nominal.module.lookupMethod(nominal.id, ast.tokenSlice(data.rhs)) orelse return null;
                    const method = nominal.module.getDeclaration(id).?;
                    if (nominal.module.source.ast.declFunction(method.node) == null or (nominal.module != module and !method.public)) return null;
                    return .{ .ref = .{ .module = nominal.module, .id = id }, .container = nominal };
                }
            }
        }
        return null;
    }

    fn instanceCallTarget(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!?CallTarget {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        if (ast.nodes.items(.tag)[node] != .field_access) return null;
        const data = ast.nodes.items(.data)[node];
        const name = ast.tokenSlice(data.rhs);
        const receiver = root(try self.expression(frame, data.lhs, depth + 1));
        var container: ?Nominal = null;
        var owner: *Module = undefined;
        var identity: Module.DeclId = undefined;
        switch (receiver.value) {
            .nominal => |n| {
                container = n;
                owner = n.module;
                identity = n.id;
                const owner_ast = &owner.source.ast;
                for (owner_ast.fullStruct(owner_ast.decl(owner.getDeclaration(identity).?.node).ast.init_node).?) |member| {
                    if (owner_ast.nodes.items(.tag)[member] == .container_field and std.mem.eql(u8, owner_ast.tokenSlice(owner_ast.firstToken(member)), name)) return null;
                }
            },
            .known => |ty| {
                const nominal = self.ir.getType(ty).?;
                const source = switch (nominal) {
                    .record => |r| blk: {
                        for (r.fields) |field_value| if (std.mem.eql(u8, field_value.name, name)) return null;
                        break :blk r.source;
                    },
                    .enumeration => |e| e.source,
                    else => return null,
                };
                owner = if (module.compilation) |comp| comp.getModule(source).? else module;
                identity = @fromBackingInt(@intCast(if (nominal == .record) nominal.record.identity else nominal.enumeration.identity));
            },
            else => return null,
        }
        const id = owner.lookupMethod(identity, name) orelse return null;
        const decl = owner.getDeclaration(id).?;
        if (owner.source.ast.declFunction(decl.node) == null or (owner != module and !decl.public)) return null;
        return .{ .ref = .{ .module = owner, .id = id }, .container = container, .receiver = receiver };
    }

    fn closureReference(self: *Solver, frame: *Frame, proto: Ast.Node.Index, body_node: Ast.Node.Index) Ir.BuildError!Ref {
        const local = try self.allocator.create(LocalTemplate);
        const declaration = if (frame.ref.local == null) frame.ref.module.getDeclaration(frame.ref.id) else null;
        local.* = .{
            .proto = proto,
            .body = body_node,
            .outer_variables = frame.variables,
            .lexical_locals = try self.allocator.dupe(Local, frame.locals.items),
            .container = if (frame.ref.local) |outer| outer.container else declaration.?.container,
            .concrete_type = if (frame.ref.local) |outer| outer.concrete_type else declaration.?.concrete_type,
        };
        return .{ .module = frame.ref.module, .local = local };
    }

    fn closure(self: *Solver, frame: *Frame, proto_node: Ast.Node.Index, body_node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const span = module.tokenSpan(ast.firstToken(proto_node));
        if (!signatureVariablesBound(frame, proto_node)) {
            const ref = try self.closureReference(frame, proto_node, body_node);
            const instance = (try self.signature(ref, frame.container, depth + 1)) orelse return self.term(.unknown, span);
            var child = instance.frame;
            try self.body(&child, depth + 1);
            return self.term(.{ .function = instance.function }, span);
        }
        const signature_type = try self.typeNode(frame, proto_node, depth + 1);
        if (self.failed or signature_type.value != .function) return self.term(.unknown, span);
        const function = signature_type.value.function;
        var child: Frame = .{ .ref = frame.ref, .variables = frame.variables, .result = function.result, .container = frame.container };
        try child.locals.appendSlice(self.allocator, frame.locals.items);
        var iterator = ast.fullFnProto(proto_node).?.iterate(ast);
        for (function.params) |param| {
            if (iterator.next().?.name_token) |token| try child.locals.append(self.allocator, .{ .name = ast.tokenSlice(token), .term = param });
        }
        try self.unify(function.result, try self.expression(&child, body_node, depth + 1), module.tokenSpan(ast.firstToken(body_node)), 0);
        return signature_type;
    }

    fn field(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const data = ast.nodes.items(.data)[node];
        const span = module.tokenSpan(ast.firstToken(node));
        const name = ast.tokenSlice(data.rhs);
        const base = root(try self.expression(frame, data.lhs, depth + 1));
        if (base.value == .never) return base;
        if (base.value == .nominal) {
            const nominal = base.value.nominal;
            const owner_ast = &nominal.module.source.ast;
            const decl = nominal.module.getDeclaration(nominal.id).?;
            for (owner_ast.fullStruct(owner_ast.decl(decl.node).ast.init_node).?) |member| {
                if (owner_ast.nodes.items(.tag)[member] != .container_field or !std.mem.eql(u8, owner_ast.tokenSlice(owner_ast.firstToken(member)), name)) continue;
                const type_node = owner_ast.nodes.items(.data)[member].lhs;
                if (type_node == 0) return self.fromType(try self.ir.internType(.void), span, 0);
                var field_frame: Frame = .{ .ref = .{ .module = nominal.module, .id = nominal.id }, .variables = &.{}, .container = nominal, .result = undefined };
                return self.typeNode(&field_frame, type_node, depth + 1);
            }
        }
        if (base.value == .known) {
            const ty = self.ir.getType(base.value.known).?;
            if (ty == .record) {
                for (ty.record.fields) |member| if (std.mem.eql(u8, member.name, name)) return self.fromType(member.ty, span, 0);
            }
            if (ty == .string and std.mem.eql(u8, name, "len")) return self.fromType(try self.ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } }), span, 0);
        }
        if (base.value == .array and std.mem.eql(u8, name, "len")) return self.fromType(try self.ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } }), span, 0);
        self.unsupported = self.unsupported orelse span;
        return self.term(.unknown, span);
    }

    fn loopExpression(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const span = module.tokenSpan(ast.firstToken(node));
        const loop = ast.fullLoop(node).?;
        const unit = try self.fromType(try self.ir.internType(.void), span, 0);
        var context: Loop = .{ .result = try self.term(.unknown, span), .allows_value = loop.condition == 0 or loop.else_expr != 0 };
        if (!context.allows_value) try self.unify(context.result, unit, span, 0);
        const parent = frame.loop;
        frame.loop = &context;
        defer frame.loop = parent;
        if (loop.condition != 0) {
            const condition = try self.expression(frame, loop.condition, depth + 1);
            if (root(condition).value == .never) return if (context.has_break) context.result else condition;
            try self.unify(condition, try self.fromType(try self.ir.internType(.boolean), span, 0), span, 0);
        }
        const body_value = try self.expression(frame, loop.body, depth + 1);
        try self.unify(body_value, unit, span, 0);
        if (loop.continuation != 0 and (root(body_value).value != .never or context.has_continue)) {
            try self.unify(try self.expression(frame, loop.continuation, depth + 1), unit, span, 0);
        }
        // Exhaustion belongs to the parent loop's control scope.
        frame.loop = parent;
        if (loop.else_expr != 0) {
            const exhaustion = try self.expression(frame, loop.else_expr, depth + 1);
            try self.unify(context.result, exhaustion, span, 0);
            if (root(exhaustion).value == .never and !context.has_break) return exhaustion;
        } else if (loop.condition == 0 and !context.has_break) {
            return self.term(.never, span);
        } else if (!context.has_break) {
            try self.unify(context.result, unit, span, 0);
        }
        return context.result;
    }

    fn expression(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const span = module.tokenSpan(ast.firstToken(node));
        if (depth >= max_depth) try self.fail(.generic_function_limit, self.call_span, span);
        if (self.failed) return self.term(.unknown, span);
        const data = ast.nodes.items(.data)[node];
        if (try self.callTarget(frame, node, depth + 1)) |candidate| {
            if (candidate.ref.local != null or candidate.ref.module.getDeclaration(candidate.ref.id).?.is_generic_function) {
                const instance = (try self.signature(candidate.ref, candidate.container, depth + 1)) orelse return self.term(.unknown, span);
                var callback_frame = instance.frame;
                try self.body(&callback_frame, depth + 1);
                return self.term(.{ .function = instance.function }, span);
            }
        }
        switch (ast.nodes.items(.tag)[node]) {
            .identifier => {
                const name = ast.tokenSlice(ast.firstToken(node));
                var i = frame.locals.items.len;
                while (i != 0) {
                    i -= 1;
                    if (std.mem.eql(u8, frame.locals.items[i].name, name)) return frame.locals.items[i].term;
                }
                if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) return self.fromType(try self.ir.internType(.boolean), span, 0);
                if (module.lookup(name)) |id| {
                    const decl = module.getDeclaration(id).?;
                    if (!decl.is_generic_function and !decl.is_type_alias) {
                        const full = ast.decl(decl.node).ast;
                        if (full.type_node != 0) {
                            var global_frame = frame.*;
                            global_frame.ref = .{ .module = module, .id = id };
                            return self.typeNode(&global_frame, full.type_node, depth + 1);
                        }
                    }
                }
            },
            .string_literal => return self.fromType(try self.ir.internType(.string), span, 0),
            .char_literal => return self.fromType(try self.ir.internType(.rune), span, 0),
            .number_literal => return self.term(.unknown, span),
            .field_access => return self.field(frame, node, depth + 1),
            .bool_not, .bool_and, .bool_or => {
                const boolean = try self.fromType(try self.ir.internType(.boolean), span, 0);
                const unary = ast.nodes.items(.tag)[node] == .bool_not;
                if (!unary) {
                    const lhs = try self.expression(frame, data.lhs, depth + 1);
                    if (root(lhs).value == .never) return lhs;
                    try self.unify(boolean, lhs, span, 0);
                }
                const rhs = try self.expression(frame, data.rhs, depth + 1);
                if (unary and root(rhs).value == .never) return rhs;
                try self.unify(boolean, rhs, span, 0);
                // A short-circuit's other path remains live even if its RHS exits.
                return boolean;
            },
            .negation, .bit_not => return self.expression(frame, data.rhs, depth + 1),
            .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => {
                const lhs = try self.expression(frame, data.lhs, depth + 1);
                if (root(lhs).value == .never) return lhs;
                const rhs = try self.expression(frame, data.rhs, depth + 1);
                if (root(rhs).value == .never) return rhs;
                try self.unify(lhs, rhs, span, 0);
                return switch (ast.nodes.items(.tag)[node]) {
                    .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => self.fromType(try self.ir.internType(.boolean), span, 0),
                    else => lhs,
                };
            },
            .array_access => {
                const base = try self.expression(frame, data.lhs, depth + 1);
                const known = root(base);
                if (known.value == .never) return base;
                const index = try self.expression(frame, data.rhs, depth + 1);
                if (root(index).value == .never) return index;
                try self.unify(index, try self.fromType(try self.ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } }), span, 0), span, 0);
                if (known.value == .known and self.ir.getType(known.value.known).? == .string) return self.fromType(try self.ir.internType(.rune), span, 0);
                const element = try self.term(.unknown, span);
                try self.unify(base, try self.term(.{ .array = element }, span), span, 0);
                return element;
            },
            .array_init, .array_init_comma, .array_init_two, .array_init_two_comma => {
                const element = try self.term(.unknown, span);
                const buf = [_]Ast.Node.Index{ data.lhs, data.rhs };
                const elements = switch (ast.nodes.items(.tag)[node]) {
                    .array_init, .array_init_comma => ast.extra_data[data.lhs..data.rhs],
                    else => buf[0 .. @as(usize, @intFromBool(data.lhs != 0)) + @as(usize, @intFromBool(data.rhs != 0))],
                };
                for (elements) |item| {
                    const value = try self.expression(frame, item, depth + 1);
                    if (root(value).value == .never) return value;
                    try self.unify(element, value, span, 0);
                }
                return self.term(.{ .array = element }, span);
            },
            .block, .block_two, .block_newline, .block_two_newline => {
                const scope = frame.locals.items.len;
                defer frame.locals.shrinkRetainingCapacity(scope);
                var buffer: [2]Ast.Node.Index = undefined;
                var result = try self.fromType(try self.ir.internType(.void), span, 0);
                for (ast.fullBlock(&buffer, node).?.ast.statements) |statement| {
                    if (ast.fullDecl(statement)) |decl| {
                        var named_function = false;
                        if (ast.fullFnProto(decl.ast.type_node)) |proto| {
                            var iterator = proto.iterate(ast);
                            while (iterator.next()) |param| if (param.name_token != null) {
                                named_function = true;
                            };
                        }
                        const closure_proto = if (named_function) decl.ast.type_node else if (decl.ast.init_node != 0 and ast.nodes.items(.tag)[decl.ast.init_node] == .function_expr) ast.nodes.items(.data)[decl.ast.init_node].lhs else 0;
                        if (closure_proto != 0 and !signatureVariablesBound(frame, closure_proto)) {
                            const closure_body = if (named_function) decl.ast.init_node else ast.nodes.items(.data)[decl.ast.init_node].rhs;
                            const ref = try self.closureReference(frame, closure_proto, closure_body);
                            try frame.locals.append(self.allocator, .{ .name = ast.tokenSlice(decl.ast.let_token + 1 + @intFromBool(decl.ast.is_mutable)), .term = try self.term(.unknown, span), .template = ref });
                            result = try self.fromType(try self.ir.internType(.void), span, 0);
                            continue;
                        }
                        const value = if (decl.ast.init_node == 0)
                            try self.term(.unknown, span)
                        else if (named_function)
                            try self.closure(frame, decl.ast.type_node, decl.ast.init_node, depth + 1)
                        else
                            try self.expression(frame, decl.ast.init_node, depth + 1);
                        if (root(value).value == .never) return value;
                        if (decl.ast.type_node != 0) {
                            if (ast.fullFnProto(decl.ast.type_node) != null and !signatureVariablesBound(frame, decl.ast.type_node)) {
                                try self.fail(.generic_function_context, module.tokenSpan(ast.firstToken(decl.ast.type_node)), null);
                            } else try self.unify(value, try self.typeNode(frame, decl.ast.type_node, depth + 1), module.tokenSpan(ast.firstToken(statement)), 0);
                        }
                        try frame.locals.append(self.allocator, .{ .name = ast.tokenSlice(decl.ast.let_token + 1 + @intFromBool(decl.ast.is_mutable)), .term = value });
                        result = try self.fromType(try self.ir.internType(.void), span, 0);
                    } else result = try self.expression(frame, statement, depth + 1);
                    if (root(result).value == .never) return result;
                }
                return result;
            },
            .@"if", .if_else => {
                const branch = ast.fullIf(node).?.ast;
                const condition = try self.expression(frame, branch.cond_expr, depth + 1);
                if (root(condition).value == .never) return condition;
                try self.unify(condition, try self.fromType(try self.ir.internType(.boolean), span, 0), span, 0);
                const then = try self.expression(frame, branch.then_expr, depth + 1);
                const otherwise = if (branch.else_expr == 0) try self.fromType(try self.ir.internType(.void), span, 0) else try self.expression(frame, branch.else_expr, depth + 1);
                try self.unify(then, otherwise, span, 0);
                return if (root(then).value == .never) otherwise else then;
            },
            .@"return" => {
                const value = if (data.rhs == 0) try self.fromType(try self.ir.internType(.void), span, 0) else try self.expression(frame, data.rhs, depth + 1);
                try self.unify(frame.result, value, span, 0);
                return self.term(.never, span);
            },
            .@"break", .@"continue" => {
                const loop = frame.loop orelse {
                    try self.fail(.loop_control_outside_loop, span, null);
                    return self.term(.never, span);
                };
                if (data.lhs != 0) try self.fail(.unsupported_loop_control, span, null);
                if (ast.nodes.items(.tag)[node] == .@"break") {
                    const value = if (data.rhs == 0) try self.fromType(try self.ir.internType(.void), span, 0) else try self.expression(frame, data.rhs, depth + 1);
                    if (root(value).value != .never) {
                        if (data.rhs != 0 and !loop.allows_value) try self.fail(.conditional_value_break, span, null);
                        try self.unify(loop.result, value, span, 0);
                        loop.has_break = true;
                    }
                } else loop.has_continue = true;
                return self.term(.never, span);
            },
            .loop_condition, .loop_cont, .loop_else => return self.loopExpression(frame, node, depth + 1),
            .function_expr => return self.closure(frame, data.lhs, data.rhs, depth + 1),
            .call, .call_one => {
                const call = ast.fullCall(node).?;
                if (std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@as") and call.ast.args.len == 2) {
                    const ty = try self.typeNode(frame, call.ast.args[0], depth + 1);
                    const value = try self.expression(frame, call.ast.args[1], depth + 1);
                    if (root(value).value == .never) return value;
                    try self.unify(ty, value, span, 0);
                    return ty;
                }
                const callee_name = ast.tokenSlice(ast.firstToken(call.ast.fn_expr));
                if (ast.nodes.items(.tag)[call.ast.fn_expr] == .builtin_literal and
                    (std.mem.eql(u8, callee_name, "@panic") or std.mem.eql(u8, callee_name, "@print") or std.mem.eql(u8, callee_name, "@println") or std.mem.eql(u8, callee_name, "@assert")))
                {
                    for (call.ast.args, 0..) |arg, index| {
                        const value = try self.expression(frame, arg, depth + 1);
                        if (root(value).value == .never) return value;
                        if (std.mem.eql(u8, callee_name, "@panic") or std.mem.eql(u8, callee_name, "@assert")) {
                            const ty = try self.ir.internType(if (std.mem.eql(u8, callee_name, "@assert") and index == 0) .boolean else .string);
                            try self.unify(value, try self.fromType(ty, span, 0), span, 0);
                        }
                    }
                    return if (std.mem.eql(u8, callee_name, "@panic")) self.term(.never, span) else self.fromType(try self.ir.internType(.void), span, 0);
                }
                if ((try self.callTarget(frame, call.ast.fn_expr, depth)) orelse (try self.instanceCallTarget(frame, call.ast.fn_expr, depth))) |callee_target| {
                    if (try self.signature(callee_target.ref, callee_target.container, depth + 1)) |instance| {
                        var child = instance.frame;
                        const f = instance.function;
                        const offset: usize = @intFromBool(callee_target.receiver != null);
                        if (f.params.len == call.ast.args.len + offset) {
                            if (callee_target.receiver) |receiver| try self.unify(f.params[0], receiver, span, 0);
                            for (f.params[offset..], call.ast.args) |param, arg| {
                                const value = try self.expression(frame, arg, depth + 1);
                                if (root(value).value == .never) return value;
                                try self.unify(param, value, module.tokenSpan(ast.firstToken(arg)), 0);
                            }
                            if (callee_target.ref.isNoreturn()) return self.term(.never, span);
                            if (callee_target.ref.local != null or callee_target.ref.module.getDeclaration(callee_target.ref.id).?.is_generic_function) try self.body(&child, depth + 1);
                            return f.result;
                        }
                    }
                }
                const callee = root(try self.expression(frame, call.ast.fn_expr, depth + 1));
                if (callee.value == .never) return callee;
                if (callee.value == .function and callee.value.function.params.len == call.ast.args.len) {
                    const f = callee.value.function;
                    for (f.params, call.ast.args) |param, arg| {
                        const value = try self.expression(frame, arg, depth + 1);
                        if (root(value).value == .never) return value;
                        try self.unify(param, value, span, 0);
                    }
                    return f.result;
                }
            },
            else => {},
        }
        self.unsupported = self.unsupported orelse span;
        return self.term(.unknown, span);
    }

    fn materialize(self: *Solver, term_value: *Term, depth: usize) Ir.BuildError!?Ir.TypeId {
        if (depth >= max_depth) {
            try self.fail(.generic_function_limit, self.call_span, term_value.span);
            return null;
        }
        const t = root(term_value);
        return switch (t.value) {
            .unknown, .never => null,
            .link => unreachable,
            .known => |ty| ty,
            .array => |element| if (try self.materialize(element, depth + 1)) |ty| try self.ir.internType(.{ .array = ty }) else null,
            .function => |f| blk: {
                const params = try self.allocator.alloc(Ir.TypeId, f.params.len);
                for (f.params, params) |param, *out| out.* = (try self.materialize(param, depth + 1)) orelse break :blk null;
                const result = (try self.materialize(f.result, depth + 1)) orelse break :blk null;
                break :blk try self.ir.internType(.{ .function = .{ .params = params, .result = result } });
            },
            .nominal => |n| blk: {
                const args = try self.allocator.alloc(Ir.TypeId, n.args.len);
                for (n.args, args) |arg, *out| out.* = (try self.materialize(arg, depth + 1)) orelse break :blk null;
                if (self.read_only) break :blk try TypeResolver.instantiateArgumentsReadOnly(n.module, self.ir, n.id, args, 0);
                break :blk try TypeResolver.instantiateArguments(n.module, self.ir, n.id, args, 0);
            },
        };
    }
};

test "backtick generic function rejects ambiguous conflicting unbound and unsupported uses" {
    const prefix = "let add:(x:`a,y:`b)->`c=x+y\nlet id:(x:`T)->`T=x\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let main:()={let x=add(1,2)\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={let x=add(@as(uint,1),@as(int,2))\n}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={let x=add(\"s\",@as(uint,2))\n}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={let f=id\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={let f:(uint)->int=id\n}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={let f:(uint,uint)->uint=id\n}", .tag = .argument_count_mismatch },
        .{ .source = "let main:()={let f:uint=id\n}", .tag = .generic_function_context },
        .{ .source = "let main:()={let f=[id,id]\n}", .tag = .array_literal_needs_type },
        .{ .source = "let main:()={let f=if true {id} else {id}\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let apply:(f:(`T)->`U,x:`T)->`U=f(x)\nlet main:()={let x=apply(id,42)\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let apply:(f:(`T)->`U,x:`T)->`U=f(x)\nlet main:()={let x:int=apply(id,@as(uint,42))\n}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={@println(id)}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={id}", .tag = .generic_function_context },
        .{ .source = "let main:()={let mut f:(x:`T)->`T=x\n}", .tag = .generic_function_context },
        .{ .source = "let main:()={let mut f=(x:`T)->`T{x}\n}", .tag = .generic_function_context },
        .{ .source = "let x:`T=true\nlet main:()={}", .tag = .unbound_type_variable },
        .{ .source = "let bad:(x:`T)->`T={let y:`U=x\ny}\nlet main:()={let x=bad(true)\n}", .tag = .unbound_type_variable },
        .{ .source = "let wrong:(x:`T)->`T=missing\nlet main:()={let x=wrong(true)\n}", .tag = .unknown_name },
        .{ .source = "let wrong:(x:`T)->`T=x+x\nlet main:()={let x=wrong(true)\n}", .tag = .operator_not_supported },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "negative.nov", source);
        defer module.deinit();
        errdefer std.debug.print("backtick negative {s}: {any}, {any}\n", .{ case.source, module.source.ast.errors, module.diagnostics.items });
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        var found = false;
        for (module.diagnostics.items) |diagnostic| {
            if (diagnostic.tag == case.tag) found = true;
        }
        try std.testing.expect(found);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "backtick generic function nested arrays functions nominal containers recursion and sibling calls" {
    const source =
        \\let Box:(T:type)->type=struct{value:T,}
        \\let unwrap:(box:Box(`T))->`T=box.value
        \\let apply:(f:(`T)->`U,x:`T)->`U=f(x)
        \\let twice:(x:`T)->`U=add(x,x)
        \\let add:(x:`a,y:`b)->`c=x+y
        \\let first:(items:[]`T)->`T=items[0]
        \\let identity:(x:`T)->`T=x
        \\let even:(n:uint,x:`T)->`T=if n==0 {x} else {odd(n-1,x)}
        \\let odd:(n:uint,x:`T)->`T=if n==0 {x} else {even(n-1,x)}
        \\let inc:(x:uint)->uint=x+1
        \\let NS=struct{let id:(x:`T)->`T=x
        \\}
        \\let alias=NS.id
        \\let G:(T:type)->type=struct{
        \\let sibling:(x:`U,y:T)->`U=x
        \\let call:(x:`U,y:T)->`V=sibling(x,y)
        \\}
        \\let main:()={
        \\let nested:[][]uint=[[1],[2]]
        \\let a:uint=first(first(nested))
        \\let b:uint=twice(2)
        \\let c=apply(inc,3)
        \\let d=unwrap(Box(uint)[.value=4])
        \\let e=even(3,"ok")
        \\let f=alias(true)
        \\@print(a,b,c,d,e,f,NS.id(false),G(uint).call("container",1))
        \\}
    ;
    var module = try Module.init(std.testing.allocator, "nested.nov", source);
    defer module.deinit();
    errdefer std.debug.print("backtick nested: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

fn imported(allocator: std.mem.Allocator) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/root.nov",
        \\let lib=@import("lib.nov")
        \\let alias=lib.alias
        \\let method=lib.NS.id
        \\let main:()={let a:uint=alias(1,2)
        \\let b=lib.add(@as(uint,3),4)
        \\let cb:(uint,uint)->uint=alias
        \\let other:(bool)->bool=method
        \\let direct:(bool)->bool=lib.NS.id
        \\let pubref:(string)->string=lib.id
        \\@print(a,b,cb(1,2),other(true),direct(false),pubref("ok"),lib.id("ok"),method(true),lib.NS.id(false))}
    );
    const library = try comp.addSource("/app/lib.nov",
        \\@[public]
        \\let add:(x:`a,y:`b)->`c=x+y
        \\@[public]
        \\let alias=add
        \\@[public]
        \\let id:(x:`T)->`T=x
        \\@[public]
        \\let NS=struct{
        \\@[public]
        \\let id:(x:`T)->`T=x
        \\}
    );
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = library, .named = false, .span = module.getDeclaration(local).?.name_span });
    const original_count = comp.declarations.items.len;
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        try std.testing.expectEqual(original_count, comp.declarations.items.len);
        for (comp.modules.items) |owner| {
            try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
            for (owner.declarations.items) |decl| try std.testing.expect(decl.function_template == null and decl.concrete_type == null);
        }
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!comp.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    var count: usize = 0;
    for (comp.getModule(library).?.declarations.items) |decl| {
        if (decl.function_template != null) count += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), count);
    try sema.ir.verify();
    var repeated = Lowering.lowerCompilation(&comp) catch |err| {
        try std.testing.expectEqual(original_count, comp.declarations.items.len);
        for (comp.modules.items) |owner| try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
        return err;
    };
    defer repeated.deinit();
    try std.testing.expect(!comp.hasErrors());
    try std.testing.expectEqual(original_count + 3, comp.declarations.items.len);
    try std.testing.expectEqual(sema.ir.functions.items.len, repeated.ir.functions.items.len);
    try sema.ir.verify();
    try repeated.ir.verify();
}

fn testInferenceFailureAllocations(allocator: std.mem.Allocator, source: []const u8, expected: Module.Diagnostic.Tag) !void {
    var module = try Module.init(allocator, "failure.nov", source);
    defer module.deinit();
    if (module.source.ast.errors.len != 0) std.debug.print("inference failure parse {s}: {any}\n", .{ source, module.source.ast.errors });
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!sema.lowering_complete);
    var found = false;
    for (module.diagnostics.items) |diagnostic| if (diagnostic.tag == expected) {
        found = true;
    };
    if (!found) std.debug.print("expected {any}: {any}\n", .{ expected, module.diagnostics.items });
    try std.testing.expect(found);
}

test "backtick unresolved conflict and unbound diagnostics roll back under OOM" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:(x:`T)->`U=1\nlet main:()={let a=f(true)\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let f:(x:`T,y:`T)->`T=x\nlet main:()={let a=f(true,\"s\")\n}", .tag = .generic_function_conflict },
        .{ .source = "let f:(x:`T)->`T={let a:`U=x\na}\nlet main:()={let a=f(true)\n}", .tag = .unbound_type_variable },
        .{ .source = "let f:(x:`T)->`T={let mut a=(v:`U)->`U{v}\nx}\nlet main:()={let a=f(true)\n}", .tag = .generic_function_context },
    }) |case| {
        try testInferenceFailureAllocations(std.testing.allocator, case.source, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ case.source, case.tag });
    }
}

test "backtick expansion term and type depth limits diagnose without publishing IR" {
    try testInferenceFailureAllocations(std.testing.allocator, "let grow:(x:`T)->uint=grow([x])\nlet main:()={let a=grow(true)\n}", .generic_function_limit);
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll("let f:(x:`T)->[]`T=[");
    for (0..max_terms) |i| try source.writer.writeAll(if (i == 0) "true" else ",true");
    try source.writer.writeAll("]\nlet main:()={let a=f(true)\n}");
    try testInferenceFailureAllocations(std.testing.allocator, source.written(), .generic_function_limit);
    source.clearRetainingCapacity();
    try source.writer.writeAll("let f:(x:");
    for (0..max_depth) |_| try source.writer.writeAll("[]");
    try source.writer.writeAll("`T)->`T=x\nlet main:()={let a=f(true)\n}");
    try testInferenceFailureAllocations(std.testing.allocator, source.written(), .generic_function_limit);
}

test "backtick specialization count is bounded per defining module" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll("let id:(x:`T)->`T=x\n");
    for (0..max_specializations + 1) |i| try source.writer.print("let R{d}=struct{{}}\n", .{i});
    try source.writer.writeAll("let main:()={\n");
    for (0..max_specializations + 1) |i| try source.writer.print("let a{d}=id(R{d}[])\n", .{ i, i });
    try source.writer.writeAll("}");
    try testInferenceFailureAllocations(std.testing.allocator, source.written(), .generic_function_limit);
}

fn testLocalClosureProbe(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "closure-probe.nov", "let main:()={let f=(x:`T)->`T{let ordinary=(v:`T)->`T{v}\nif saved {ordinary(x)} else {x}}\n}");
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const ast = &module.source.ast;
    var buffer: [2]Ast.Node.Index = undefined;
    const statement = ast.fullBlock(&buffer, ast.declFunction(ast.rootDecls()[0]).?.body).?.ast.statements[0];
    const function = ast.functionExpr(ast.decl(statement).ast.init_node).?;
    const boolean = try ir.internType(.boolean);
    const local: LocalTemplate = .{ .proto = function.proto.ast.proto_node, .body = function.body, .captures = &.{.{ .name = "saved", .ty = boolean }} };
    const ref: Ref = .{ .module = &module, .local = &local };
    const declarations = module.declarations.items.len;
    for (0..3) |_| {
        try std.testing.expectEqual(boolean, (try probeResult(&module, &ir, ref, &.{.{ .ty = boolean, .span = ref.span() }}, ref.span())).?);
        try std.testing.expectEqual(declarations, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
        try std.testing.expectEqual(@as(usize, 0), ir.local_generic_instances.items.len);
    }
}

test "generic closures readonly probes never demand code IDs instances or environments under OOM" {
    try testLocalClosureProbe(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLocalClosureProbe, .{});
}

fn testClosureMethodScopeImports(allocator: std.mem.Allocator) !void {
    var lowered = blk: {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/review/main.nov",
            \\let api=@import("api.nov")
            \\let main:()={@println(api.Alias[].method(true),api.Alias.static(false),api.Generic(bool)[].method(true))}
        );
        const library = try comp.addSource("/review/api.nov",
            \\let R=struct{
            \\ let helper:(input:bool)->int=1
            \\ @[public]
            \\ let method:(self:R,input:`A)->`U={let f=(x:`B)->`C{helper(x)}
            \\  f(input)}
            \\ @[public]
            \\ let static:(input:`A)->`U={let f=(x:`B)->`C{let nested=(other:`D)->`E{helper(other)}
            \\  nested(x)}
            \\  f(input)}
            \\}
            \\let Box:(Element:type)->type=struct{
            \\ let Self=@This()
            \\ let helper:(input:Element)->int=2
            \\ @[public]
            \\ let method:(self:Self,input:`A)->`U={let f=(x:`B)->`C{helper(x)}
            \\  f(input)}
            \\}
            \\@[public]
            \\let Alias=R
            \\@[public]
            \\let Generic=Box
        );
        const module = comp.getModule(root).?;
        const local = module.lookup("api").?;
        try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = library, .named = false, .span = module.getDeclaration(local).?.name_span });
        const count = comp.declarations.items.len;
        var result = Lowering.lowerCompilation(&comp) catch |err| {
            try std.testing.expectEqual(count, comp.declarations.items.len);
            for (comp.modules.items) |owner| try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
            return err;
        };
        errdefer result.deinit();
        if (comp.hasErrors()) for (comp.modules.items) |owner| std.debug.print("closure method scope: {any} {any}\n", .{ owner.source.ast.errors, owner.diagnostics.items });
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer lowered.deinit();
    try lowered.ir.verify();
}

test "generic closures review imported aliases retain transitive static generic method scope under OOM" {
    try testClosureMethodScopeImports(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testClosureMethodScopeImports, .{});
}

test "generic closures block probes restore names and retain lexical errors under OOM" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let id:(x:`A)->`A=x\nlet main:()={@print(id({let f=(x:`T)->`T{x}\nf(true)}))\n@print(f)}", .tag = .unknown_name },
        .{ .source = "let id:(x:`A)->`A=x\nlet main:()={let a:int=7\n@print(id({let f=(x:`T)->`T{let a:int=9\nx}\nf(true)}))}", .tag = .unsupported_shadowing },
        .{ .source = "let id:(x:`A)->`A=x\nlet main:()={let a:int=7\n@print(id({let f=(a:`T)->`T{a}\nf(true)}))}", .tag = .unsupported_shadowing },
        .{ .source = "let id:(x:`A)->`A=x\nlet main:()=@print(id({let f=(x:`T)->`T{f(x)}\nf(true)}))", .tag = .unknown_name },
    }) |case| {
        try testInferenceFailureAllocations(std.testing.allocator, case.source, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ case.source, case.tag });
    }
}

test "generic closures review no shadowing is independent of captures and demand order under OOM" {
    for ([_][]const u8{
        "let main:()={let a:int=7\nlet f=(x:`T)->`T{let a:int=9\nx}\n@print(f(true))}",
        "let main:()={let a:int=7\nlet f=(x:`T)->`T{@print(a)\nlet a:int=9\nx}\n@print(f(true))}",
        "let main:()={let a:int=7\nlet f=(x:`T)->`T{let a:int=9\nx}\n@print(f(@as(int,1)),f(true))}",
        "let main:()={let a:int=7\nlet f=(x:`T)->`T{let g=(a:bool)->bool{a}\nx}\n@print(f(true))}",
        "let main:()={let a:int=7\nlet f=(x:`T)->`T{let g=(a:`U)->`U{a}\nx}\n@print(f(true))}",
        "let main:()={let a:int=7\nlet f=(x:`T)->`T{let g=(v:`U)->`U{let a:int=9\nv}\ng(x)}\n@print(f(true))}",
        "let a:int=7\nlet main:()={let f=(x:`T)->`T{let a:int=9\nx}\n@print(f(true))}",
        "let outer:(input:`A)->`A={let f=(x:`B)->`B{let input:int=9\nx}\nf(input)}\nlet main:()=@print(outer(true))",
        "let Box:(Element:type)->type=struct{let Self=@This()\nlet method:(self:Self,input:`A)->`A={let f=(x:`B)->`B{let Element:int=9\nx}\nf(input)}\n}\nlet main:()=@print(Box(bool)[].method(true))",
        "let Box:(Element:type)->type=struct{let Self=@This()\nlet method:(self:Self,input:`A)->`A={let f=(x:`B)->`B{let Self:int=9\nx}\nf(input)}\n}\nlet main:()=@print(Box(bool)[].method(true))",
    }) |source| {
        try testInferenceFailureAllocations(std.testing.allocator, source, .unsupported_shadowing);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ source, Module.Diagnostic.Tag.unsupported_shadowing });
    }
}

test "generic closures diagnose exact constraints ambiguity locals and capture scopes under OOM" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let main:()={let f=(x:`T,y:`T)->`T{x}\nf(true,@as(int,1))}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={let f=(x:`T)->`T{x}\nlet g=f\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={let f:(x:`T)->`T=x\nf(1)\n@print()}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={let f:(x:`T)->`T=unknown\n@print(f(true))}", .tag = .unknown_name },
        .{ .source = "let main:()={let f:(x:`T)->`T=f(x)\n@print(f(true))}", .tag = .unknown_name },
        .{ .source = "let main:()={let x:int=1\nlet f=(x:`T)->`T{x}\nf(true)}", .tag = .unsupported_shadowing },
        .{ .source = "let main:()={let f=(x:`T)->`T{{let hidden:int=1\n}\nx}\n@print(f(true))\n@print(hidden)}", .tag = .unknown_name },
        .{ .source = "let main:()={let mut n:int=1\nlet loan=&n\nlet f=(x:`T)->`T{@print(loan)\nx}\nf(true)}", .tag = .unsupported_borrow },
    }) |case| {
        try testInferenceFailureAllocations(std.testing.allocator, case.source, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ case.source, case.tag });
    }
}

test "backtick inferred generic closures are rejected at module scope" {
    var module = try Module.init(std.testing.allocator, "closure.nov", "let f=(value:`T)->`T{value}");
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.generic_function_context, module.diagnostics.items[0].tag);
}

test "backtick exact conflicts retain both runtime argument source spans" {
    const source = "let f:(x:`T,y:`T)->`T=x\nlet main:()={let a=f(@as(uint,1),@as(int,2))\n}";
    var module = try Module.init(std.testing.allocator, "conflict.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!sema.lowering_complete);
    const diagnostic = module.diagnostics.items[0];
    try std.testing.expectEqual(Module.Diagnostic.Tag.generic_function_conflict, diagnostic.tag);
    try std.testing.expectEqualStrings("@as(int,2)", module.sourceSlice(diagnostic.span).?);
    try std.testing.expectEqualStrings("@as(uint,1)", module.sourceSlice(diagnostic.related.?).?);
}

fn testReviewConstraints(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "review.nov",
        \\let Box:(T:type)->type=struct{value:T,}
        \\let unwrap:(x:Box(`T))->`U=x.value
        \\let invert:(x:`T)->`U=!x
        \\let size:(x:`T)->`U=x.len
        \\let make:(value:`T)->()->`U=()->`U{value}
        \\let makeNamed:(value:`T)->(bool)->`U={let captured:(ignored:bool)->`U=value
        \\captured}
        \\let G:(T:type)->type=struct{let id:(x:`U)->`U=x
        \\let plain:(x:T)->T=x
        \\}
        \\let wrap:(x:`T)->`U=G(uint).id(x)
        \\let passthrough:(x:`T)->`T=x
        \\let wrapPlain:(x:`T)->`U=G(uint).plain(1)
        \\let main:()={let box=Box(string)[.value="field"]
        \\let closure=make("captured")
        \\let named=makeNamed("named")
        \\@println(unwrap(box),invert(true),size("abc"),size([true,false]),closure(),wrap("ok"),passthrough(G(uint).plain(7)),wrapPlain("s"),named(false))}
    );
    defer module.deinit();
    const original_count = module.declarations.items.len;
    for (0..2) |_| {
        var sema = Lowering.lower(&module) catch |err| {
            try std.testing.expectEqual(original_count, module.declarations.items.len);
            try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        defer sema.deinit();
        if (module.hasErrors()) std.debug.print("review constraints: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        var count: usize = 0;
        for (module.declarations.items) |decl| if (decl.function_template != null) {
            count += 1;
        };
        try std.testing.expectEqual(@as(usize, 10), count);
        try sema.ir.verify();
    }
}

test "backtick review boolean fields lengths closure capture and concrete container call constraints" {
    try testReviewConstraints(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testReviewConstraints, .{});
}

fn testSpeculativeFailure(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "probe.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!sema.lowering_complete);
    try std.testing.expectEqual(count, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.generic_function_ambiguous, module.diagnostics.items[0].tag);
    if (module.lookup("bad")) |id| try std.testing.expectEqual(Sema.Status.generic, sema.getEntry(id).?.status);
}

test "backtick speculative nested calls do not publish or demand specializations under OOM" {
    for ([_][]const u8{
        "let bad:(x:`T)->`T=missing\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(bad(true),1)\n}",
        "let bad:(x:`T)->`T=missing\nlet first:(x:(bool)->bool,y:`B)->bool=x(true)\nlet main:()={let a=first(bad,1)\n}",
        "let id:(x:`T)->`T=x\nlet apply:(f:(`T)->`U,x:`T)->`U=f(x)\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(apply(id,true),1)\n}",
        "let id:(x:`T)->`T=x\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(@as((bool)->bool,id),1)\n}",
        "let G:(T:type)->type=struct{let id:(x:`U,y:T)->`U=x\n}\nlet first:(x:(bool,uint)->bool,y:`B)->bool=x(true,1)\nlet main:()={let a=first(G(uint).id,1)\n}",
        "let G:(T:type)->type=struct{let id:(x:`U)->`U=x\n}\nlet wrap:(x:`T)->`U=G(uint).id(x)\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(wrap(\"s\"),1)\n}",
        "let G:(T:type)->type=struct{let id:(x:`U)->`U=x\n}\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(G(uint).id(\"s\"),1)\n}",
        "let Box:(T:type)->type=struct{value:T,let id:(x:`U)->`U=x\n}\nlet id:(x:`T)->`T=x\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(id(Box(uint)[.value=1]),1)\n}",
        "let Box:(T:type)->type=struct{value:T,let id:(x:`U)->`U=x\n}\nlet make:(x:`T)->Box(`T)=Box(`T)[.value=x]\nlet get:(x:Box(`T))->`T=x.value\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(get(make(true)),1)\n}",
    }) |source| {
        try testSpeculativeFailure(std.testing.allocator, source);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testSpeculativeFailure, .{source});
    }
}

test "backtick noreturn generic results are rejected before specialization publication" {
    const source = "@[noreturn]\nlet forever:(x:`T)->`T=loop {}\nlet main:()={forever(true)}";
    try testInferenceFailureAllocations(std.testing.allocator, source, .noreturn_result);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ source, Module.Diagnostic.Tag.noreturn_result });
    var module = try Module.init(std.testing.allocator, "noreturn.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expectEqual(count, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
}

test "backtick unsupported body constraint inference has an explicit diagnostic" {
    try testInferenceFailureAllocations(std.testing.allocator, "let f:(x:`T)->`U=match x{true=>\"yes\"\nfalse=>\"no\"}\nlet main:()={let a=f(true)\n}", .generic_function_inference_unsupported);
}

test "backtick abandoned probes do not consume the specialization count limit" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll("let id:(x:`T)->`T=x\n");
    for (0..max_specializations + 1) |i| try source.writer.print("let R{d}=struct{{}}\n", .{i});
    try source.writer.writeAll("let first:(");
    for (0..max_specializations + 1) |i| try source.writer.print("x{d}:`A{d},", .{ i, i });
    try source.writer.writeAll("last:`B)->`A0=x0\nlet main:()={let a=first(");
    for (0..max_specializations + 1) |i| try source.writer.print("id(R{d}[]),", .{i});
    try source.writer.writeAll("1)\n}");
    try testSpeculativeFailure(std.testing.allocator, source.written());
}

fn testImportedSpeculativeFailure(allocator: std.mem.Allocator, invalid_layout: bool) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/probe.nov", "let lib=@import(\"lib.nov\")\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(lib.bad(true),1)\n}");
    const library = try comp.addSource("/app/lib.nov", if (invalid_layout)
        "let Box:(T:type)->type=struct{value:Missing,}\n@[public]\nlet bad:(x:`T)->Box(`T)=Box(`T)[.value=x]"
    else
        "@[public]\nlet bad:(x:`T)->`T=missing");
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = library, .named = false, .span = module.getDeclaration(local).?.name_span });
    const count = comp.declarations.items.len;
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        try std.testing.expectEqual(count, comp.declarations.items.len);
        for (comp.modules.items) |owner| try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!sema.lowering_complete);
    try std.testing.expectEqual(count, comp.declarations.items.len);
    try std.testing.expectEqual(count, comp.declaration_ids.count());
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.generic_function_ambiguous, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(@as(usize, 0), comp.getModule(library).?.diagnostics.items.len);
    for (comp.modules.items) |owner| for (owner.declarations.items) |decl| try std.testing.expect(decl.function_template == null and decl.concrete_type == null);
}

test "backtick imported speculative calls retain source IDs and do not demand bodies under OOM" {
    for ([_]bool{ false, true }) |invalid_layout| {
        try testImportedSpeculativeFailure(std.testing.allocator, invalid_layout);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testImportedSpeculativeFailure, .{invalid_layout});
    }
}

fn testDeferredContainerMethods(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "layouts.nov",
        \\let Child:(T:type)->type=struct{value:T,let plain:(x:T)->T=x
        \\}
        \\let Parent:(T:type)->type=struct{child:Child(T),let id:(x:`U)->`U=x
        \\}
        \\let P=Parent(uint)
    );
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const node = module.source.ast.decl(module.getDeclaration(module.lookup("P").?).?.node).ast.init_node;
    const count = module.declarations.items.len;
    const probed = (try TypeResolver.resolveInScopeReadOnly(&module, &ir, node, null, null, &.{})).?;
    try std.testing.expectEqual(count, module.declarations.items.len);
    const resolved_type = (try TypeResolver.resolve(&module, &ir, node)).?;
    try std.testing.expectEqual(probed, resolved_type);
    try std.testing.expectEqual(count + 2, module.declarations.items.len);
    try std.testing.expect(ir.getType(resolved_type).?.record.methods_published);
    const child = ir.getType(resolved_type).?.record.fields[0].ty;
    try std.testing.expect(ir.getType(child).?.record.methods_published);
    try std.testing.expectEqual(resolved_type, (try TypeResolver.resolve(&module, &ir, node)).?);
    try std.testing.expectEqual(count + 2, module.declarations.items.len);
    try std.testing.expect(!module.hasErrors());
}

test "backtick probe layouts defer nested container methods and publish canonical siblings once under OOM" {
    try testDeferredContainerMethods(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testDeferredContainerMethods, .{});
}

fn testFreshNominal(allocator: std.mem.Allocator, warm: bool) !void {
    const source = try std.mem.concat(allocator, u8, &.{
        "let Box:(T:type)->type=struct{value:T,}\nlet make:(x:`T)->Box(`T)=Box(`T)[.value=x]\nlet get:(x:Box(`T))->`T=x.value\n",
        if (warm) "let Seed=Box(bool)\n" else "",
        "let main:()={let result=get(make(true))\n@println(result)}",
    });
    defer allocator.free(source);
    var module = try Module.init(allocator, "nominal.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    for (0..2) |_| {
        var sema = Lowering.lower(&module) catch |err| {
            try std.testing.expectEqual(count, module.declarations.items.len);
            try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        defer sema.deinit();
        if (module.hasErrors()) std.debug.print("fresh nominal: {any}\n", .{module.diagnostics.items});
        try std.testing.expect(sema.lowering_complete);
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(count + 2, module.declarations.items.len);
        try sema.ir.verify();
    }
}

test "backtick fresh nominal nested call inference is order independent canonical and OOM safe" {
    for ([_]bool{ false, true }) |warm| {
        try testFreshNominal(std.testing.allocator, warm);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testFreshNominal, .{warm});
    }
}

const terminal_source =
    \\@[noreturn]
    \\let stop:()=@panic("stop")
    \\let NS=struct{@[noreturn]
    \\let stop:()=@panic("static")
    \\}
    \\let sink:(a:bool,b:bool)->bool=a
    \\@[noreturn]
    \\let genericStop:(x:`T)=@panic("generic")
    \\let stopAlias=genericStop
    \\let aliasForward:(x:`T)->`T=stopAlias(x)+x
    \\let forward:(x:`T)->`T=stop()
    \\let staticForward:(x:`T)->`T=NS.stop()
    \\let panicForward:(x:`T)->`T=@panic("panic")
    \\let leftExit:(x:`T)->`T=stop()+x
    \\let rightExit:(x:`T)->`T=x+stop()
    \\let skipArgs:(x:`T)->`T=sink(stop(),x)
    \\let choose:(flag:bool,x:`T)->`U=if flag {stop()} else {x}
    \\let boolRhs:(x:`T)->`U=x and stop()
    \\let finish:()->`T=return
    \\let returnOperand:(x:`T)->`T=return stop()
    \\let loopValue:(x:`T)->`U=loop {break x}
    \\let spin:(x:`T)->`T=loop {continue}
    \\let breakReturn:(x:`T)->`T=loop {break {return x}}
    \\let conditionReturn:(x:`T)->`T=loop {return x} {}
    \\let makeVoid:(x:`T)->()->`U=()->`U{return}
    \\let makeReturning:(value:`T)->(bool)->`U=(flag:bool)->`U{if flag {return value} else {stop()}}
    \\let localScope:(x:`T)->`U=loop {let closure=()->void{return}
    \\closure()
    \\break x}
    \\let main:()={
    \\let a=if false {forward(true)} else {true}
    \\let b=if false {staticForward("s")} else {"s"}
    \\let c=if false {panicForward(true)} else {true}
    \\let d=if false {leftExit("s")} else {"s"}
    \\let e=if false {rightExit("s")} else {"s"}
    \\let f=if false {skipArgs("s")} else {"s"}
    \\let g=if false {returnOperand(true)} else {true}
    \\let h=if false {spin(true)} else {true}
    \\let alias=if false {aliasForward("alias")} else {"alias"}
    \\let empty=makeVoid(true)
    \\empty()
    \\let captured=makeReturning("captured")
    \\@println(a,b,c,d,e,f,g,h,choose(false,"live"),boolRhs(false),loopValue("break"),breakReturn("return"),conditionReturn("condition"),captured(true),localScope("scope"),alias)
    \\finish()}
;

fn testTerminalConstraints(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "terminal.nov", terminal_source);
    defer module.deinit();
    for (module.source.ast.errors) |err| std.debug.print("terminal parse: {any} at {s}\n", .{ err, module.source.ast.tokenSlice(err.token) });
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("terminal constraints: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "backtick bottom constraints preserve live branches return and loop scopes under OOM" {
    try testTerminalConstraints(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTerminalConstraints, .{});
}

test "backtick void remains distinct from bottom and bare returns constrain void" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let finish:()->`T=return\nlet main:()={let value:bool=finish()\n}", .tag = .generic_function_conflict },
        .{ .source = "let noop:()={}\nlet f:(x:`T)->`T=noop()\nlet main:()={let value=f(true)\n}", .tag = .generic_function_conflict },
        .{ .source = "let f:(x:`T)->`T=@print()\nlet main:()={let value=f(true)\n}", .tag = .generic_function_conflict },
        .{ .source = "@[noreturn]\nlet stop:()=@panic(\"stop\")\nlet f:(x:`T)->`T={let alias=stop\nalias()}\nlet main:()={let value=f(true)\n}", .tag = .generic_function_conflict },
        .{ .source = "let f:(x:`T)->`T=if x {return} else {true}\nlet main:()={let value=f(true)\n}", .tag = .generic_function_conflict },
        .{ .source = "let f:(x:`T)->`T=loop {break}\nlet main:()={let value=f(true)\n}", .tag = .generic_function_conflict },
        .{ .source = "let f:(x:`T)->`T=loop {let closure=()->void{break}\nbreak x}\nlet main:()={let value=f(true)\n}", .tag = .loop_control_outside_loop },
        .{ .source = "let f:(x:`T)->`T=loop {let closure=()->void{continue}\nbreak x}\nlet main:()={let value=f(true)\n}", .tag = .loop_control_outside_loop },
    }) |case| {
        try testInferenceFailureAllocations(std.testing.allocator, case.source, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ case.source, case.tag });
    }
}

fn testImportedTerminalConstraints(allocator: std.mem.Allocator) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/terminal.nov",
        \\let lib=@import("lib.nov")
        \\let forward:(x:`T)->`T=lib.stop()
        \\let staticForward:(x:`T)->`T=lib.NS.stop()
        \\let main:()={let a=if false {forward(true)} else {true}
        \\let b=if false {staticForward("s")} else {"s"}
        \\@println(a,b)}
    );
    const library = try comp.addSource("/app/lib.nov",
        \\@[public]
        \\@[noreturn]
        \\let stop:()=@panic("stop")
        \\@[public]
        \\let NS=struct{@[public]
        \\@[noreturn]
        \\let stop:()=@panic("static")
        \\}
    );
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = library, .named = false, .span = module.getDeclaration(local).?.name_span });
    const count = comp.declarations.items.len;
    for (0..2) |_| {
        var sema = Lowering.lowerCompilation(&comp) catch |err| {
            try std.testing.expectEqual(count, comp.declarations.items.len);
            for (comp.modules.items) |owner| try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
            return err;
        };
        defer sema.deinit();
        if (comp.hasErrors()) for (comp.modules.items) |owner| std.debug.print("imported terminal: {any} {any}\n", .{ owner.source.ast.errors, owner.diagnostics.items });
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try std.testing.expectEqual(count + 2, comp.declarations.items.len);
        try sema.ir.verify();
    }
}

test "backtick named static imported noreturn constraints preserve canonical demand and OOM rollback" {
    try testImportedTerminalConstraints(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testImportedTerminalConstraints, .{});
}

test "backtick imported public aliases share canonical function specializations under OOM" {
    try imported(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, imported, .{});
}

fn testContextualValues(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "contextual.nov",
        \\let identity:(x:`T)->`T=x
        \\let alias=identity
        \\let Callback=(uint)->uint
        \\let apply:(f:(`T)->`U,x:`T)->`U=f(x)
        \\let reverse:(x:`T,f:(`T)->`U)->`U=f(x)
        \\let chain:(f:(`T)->`U,g:(`U)->`V,x:`T)->`V=g(f(x))
        \\let fixed:(f:(uint)->uint)->uint=f(42)
        \\let result:()->(uint)->uint=identity
        \\let inside:(x:`T)->`T={let cb:(`T)->`T=identity
        \\cb(x)}
        \\let NS=struct{let id:(x:`T)->`T=x
        \\}
        \\let G:(T:type)->type=struct{let pick:(x:`U,y:T)->`U=x
        \\}
        \\let main:()={
        \\let cb:Callback=alias
        \\let cast=@as((uint)->uint,identity)
        \\let mut slot:Callback=identity
        \\slot=alias
        \\let branch:Callback=if true {identity} else {alias}
        \\let funcs:[]Callback=[identity,alias]
        \\let ns:Callback=NS.id
        \\let method:(uint,bool)->uint=G(bool).pick
        \\let a=apply(identity,@as(uint,42))
        \\let b:uint=apply(alias,42)
        \\let c:uint=reverse(42,identity)
        \\let d:uint=chain(identity,alias,42)
        \\@println(cb(a),cast(b),slot(c),branch(d),funcs[0](1),fixed(identity),result()(2),inside(a),ns(3),method(4,true))}
    );
    defer module.deinit();
    const original = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(original, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("contextual values: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    var identities: usize = 0;
    for (module.declarations.items) |decl| if (decl.function_template == module.lookup("identity")) {
        identities += 1;
    };
    try std.testing.expectEqual(@as(usize, 1), identities);
}

test "backtick contextual values exact signatures bidirectional callbacks and canonical demand under OOM" {
    try testContextualValues(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testContextualValues, .{});
}

test "backtick contextual values consume canonical specialization budget" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll("let id:(x:`T)->`T=x\nlet alias=id\n");
    for (0..max_specializations + 1) |i| try source.writer.print("let R{d}=struct{{}}\n", .{i});
    try source.writer.writeAll("let main:()={\n");
    for (0..max_specializations + 1) |i| {
        try source.writer.print("let callback{d}:(R{d})->R{d}=id\nlet other{d}:(R{d})->R{d}=alias\n", .{ i, i, i, i, i, i });
    }
    try source.writer.writeAll("}");
    try testInferenceFailureAllocations(std.testing.allocator, source.written(), .generic_function_limit);
}

test "backtick contextual abandoned callback probes do not consume generic budget" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    try source.writer.writeAll("let id:(x:`T)->`T=x\n");
    for (0..max_specializations + 1) |i| try source.writer.print("let R{d}=struct{{}}\n", .{i});
    try source.writer.writeAll("let first:(");
    for (0..max_specializations + 1) |i| try source.writer.print("f{d}:(R{d})->R{d},", .{ i, i, i });
    try source.writer.writeAll("last:`B)->bool=true\nlet main:()={let a=first(");
    for (0..max_specializations + 1) |_| try source.writer.writeAll("id,");
    try source.writer.writeAll("1)\n}");
    try testSpeculativeFailure(std.testing.allocator, source.written());
}

fn testContextualVisibility(allocator: std.mem.Allocator, expression: []const u8, public: bool) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const text = try std.mem.concat(allocator, u8, &.{ "let lib=@import(\"lib.nov\")\nlet main:()={let cb:(bool)->bool=", expression, "\n@println(cb(true))}" });
    defer allocator.free(text);
    const root = try comp.addSource("/app/main.nov", text);
    const library = try comp.addSource("/app/lib.nov",
        \\let hidden:(x:`T)->`T=x
        \\@[public]
        \\let alias=hidden
        \\@[public]
        \\let NS=struct{let hidden:(x:`T)->`T=x
        \\@[public]
        \\let alias=hidden
        \\}
        \\@[public]
        \\let G:(T:type)->type=struct{let hidden:(x:`U)->`U=x
        \\@[public]
        \\let exposed:(x:`U)->`U=x
        \\}
    );
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = library, .named = false, .span = module.getDeclaration(local).?.name_span });
    const count = comp.declarations.items.len;
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        try std.testing.expectEqual(count, comp.declarations.items.len);
        for (comp.modules.items) |owner| try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();

    if (comp.hasErrors() == public) for (comp.modules.items) |owner| std.debug.print("visibility owner: {any} {any}\n", .{ owner.source.ast.errors, owner.diagnostics.items });
    try std.testing.expectEqual(!public, comp.hasErrors());
    try std.testing.expectEqual(public, sema.lowering_complete);
    var specializations: usize = 0;
    for (comp.getModule(library).?.declarations.items) |decl| if (decl.function_template != null) {
        specializations += 1;
    };
    try std.testing.expectEqual(@as(usize, @intFromBool(public)), specializations);
    if (public) try sema.ir.verify();
}

fn testInstanceProbe(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "instance-probe.nov",
        \\let Box:(T:type)->type=struct{value:T,
        \\let map:(object:Box(T),f:(T)->`U)->Box(`U)=Box(`U)[.value=f(object.value)]
        \\let get:(object:Box(T))->T=object.value
        \\}
        \\let id:(x:`U)->`U=x
        \\let wrap:(x:Box(`T))->Box(`T)=x.map(id)
        \\let get:(x:Box(`T))->`T=x.get()
        \\let B=Box(bool)
    );
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const count = module.declarations.items.len;
    const alias = module.source.ast.decl(module.getDeclaration(module.lookup("B").?).?.node).ast.init_node;
    const box = (try TypeResolver.resolveInScopeReadOnly(&module, &ir, alias, null, null, &.{})).?;
    for ([_][]const u8{ "wrap", "get" }, [_]Ir.TypeId{ box, try ir.internType(.boolean) }) |name, expected| {
        const result = (try probeResult(&module, &ir, .{ .module = &module, .id = module.lookup(name).? }, &.{.{ .ty = box, .span = module.getDeclaration(module.lookup(name).?).?.name_span }}, module.getDeclaration(module.lookup(name).?).?.name_span)).?;
        try std.testing.expectEqual(expected, result);
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        try std.testing.expect(!ir.getType(box).?.record.methods_published);
    }
}

test "backtick instance cold readonly probes return nominal and ordinary method types without publication under OOM" {
    try testInstanceProbe(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInstanceProbe, .{});
}

fn testInstanceImports(allocator: std.mem.Allocator, public: bool) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const source = if (public)
        "let lib=@import(\"lib.nov\")\nlet A=lib.Alias\nlet main:()={let b=lib.make()\nlet x=b.map(lib.callback)\nlet y=A(bool).map(b,lib.callback)\nlet z=lib.plain().apply(true)\n@println(x.value,y.value,z)}"
    else
        "let lib=@import(\"lib.nov\")\nlet main:()={let x=lib.make().hidden(true)\n}";
    const root = try comp.addSource("/app/main.nov", source);
    const library = try comp.addSource("/app/lib.nov",
        \\let Hidden:(T:type)->type=struct{value:T,
        \\@[public]
        \\let map:(object:Hidden(T),f:(T)->`U)->Hidden(`U)=Hidden(`U)[.value=f(object.value)]
        \\let hidden:(object:Hidden(T),x:`U)->`U=x
        \\}
        \\let Plain=struct{@[public]
        \\let apply:(object:Plain,x:`T)->`T=x
        \\}
        \\let identity:(x:`T)->`T=x
        \\@[public]
        \\let callback=identity
        \\@[public]
        \\let Alias=Hidden
        \\@[public]
        \\let make:()->Hidden(bool)=Hidden(bool)[.value=true]
        \\@[public]
        \\let plain:()->Plain=Plain[]
    );
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = library, .named = false, .span = module.getDeclaration(local).?.name_span });
    const count = comp.declarations.items.len;
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        try std.testing.expectEqual(count, comp.declarations.items.len);
        for (comp.modules.items) |owner| try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (comp.hasErrors() == public) for (comp.modules.items) |owner| std.debug.print("instance imports: {any} {any}\n", .{ owner.source.ast.errors, owner.diagnostics.items });
    try std.testing.expectEqual(!public, comp.hasErrors());
    try std.testing.expectEqual(public, sema.lowering_complete);
    var maps: usize = 0;
    const owner = comp.getModule(library).?;
    for (owner.declarations.items) |decl| if (decl.function_template != null and std.mem.eql(u8, owner.sourceSlice(decl.name_span).?, "map")) {
        maps += 1;
    };
    try std.testing.expectEqual(@as(usize, @intFromBool(public)), maps);
    if (public) try sema.ir.verify();
}

test "backtick instance imported aliases private result wrappers public methods and canonical IDs under OOM" {
    for ([_]bool{ false, true }) |public| {
        try testInstanceImports(std.testing.allocator, public);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInstanceImports, .{public});
    }
}

fn testInstanceInference(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "instance-inference.nov",
        \\let Box:(T:type)->type=struct{value:T,
        \\let map:(object:Box(T),convert:(T)->`U)->Box(`U)=Box(`U)[.value=convert(object.value)]
        \\let choose:(object:Box(T),convert:(T)->`U,other:`U)->`U=convert(object.value)
        \\}
        \\let Plain=struct{let apply:(object:Plain,x:`T)->`T=x
        \\}
        \\let identity:(x:`V)->`V=x
        \\let alias=identity
        \\let toBool:(x:uint)->bool=x==7
        \\let B=Box(bool)
        \\let forward:(x:`T)->`T={let box=Box(`T)[.value=x]
        \\box.choose(identity,x)}
        \\let main:()={let b=B[.value=true]
        \\let x:Box(bool)=b.map(alias)
        \\let y:Box(bool)=B.map(b,identity)
        \\let z=b.choose(identity,true)
        \\let number=Box(uint)[.value=7]
        \\let first=Box(uint).map(number,toBool)
        \\let second=number.map(toBool)
        \\let w=Plain[].apply(true)
        \\let v=forward(true)
        \\@println(x.value,y.value,z,w,v,first.value,second.value)}
    );
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("instance errors: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    var maps: usize = 0;
    for (module.declarations.items) |decl| {
        if (decl.function_template != null and std.mem.eql(u8, module.sourceSlice(decl.name_span).?, "map")) maps += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), maps);
}

fn testTerminalOperands(allocator: std.mem.Allocator, source: []const u8, expected: ?Module.Diagnostic.Tag) !void {
    var module = try Module.init(allocator, "terminal-operands.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();

    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    if (expected) |tag| {
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
    } else {
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        try sema.ir.verify();
    }
    for (module.declarations.items) |decl| if (decl.function_template != null)
        try std.testing.expect(!std.mem.eql(u8, module.sourceSlice(decl.name_span).?, "f"));
}

fn testTerminalImports(allocator: std.mem.Allocator) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/main.nov",
        \\let lib=@import("lib.nov")
        \\let main:()->uint=lib.R[].f(true,if true{return 1}else{return 2},lib.hidden)
    );
    const library = try comp.addSource("/app/lib.nov",
        \\let hidden:(x:`T)->`T=missing
        \\@[public]
        \\let R=struct{@[public]
        \\let f:(object:R,a:`T,b:`T,cb:(`T)->`T)->`T=missing
        \\}
    );
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    try comp.imports.append(allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?, .target = library, .named = false, .span = module.getDeclaration(local).?.name_span });
    const count = comp.declarations.items.len;
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        try std.testing.expectEqual(count, comp.declarations.items.len);
        for (comp.modules.items) |owner| try std.testing.expectEqual(@as(usize, 0), owner.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!comp.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqual(count, comp.declarations.items.len);
    for (comp.modules.items) |owner| for (owner.declarations.items) |decl|
        try std.testing.expect(decl.function_template == null);
    try sema.ir.verify();
}

test "backtick instance terminal operands ignore private suffix callbacks and roll back imported OOM" {
    try testTerminalImports(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTerminalImports, .{});
}

fn testTerminalProbe(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "terminal-probe.nov",
        \\let Box:(T:type)->type=struct{value:T,
        \\let f:(object:Box(T),a:`U,b:`U,c:`U)->`U=missing
        \\}
        \\let B=Box(bool)
    );
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const count = module.declarations.items.len;
    const alias = module.source.ast.decl(module.getDeclaration(module.lookup("B").?).?.node).ast.init_node;
    const box = (try TypeResolver.resolveInScopeReadOnly(&module, &ir, alias, null, null, &.{})).?;
    const id = module.lookupMethod(module.lookup("Box").?, "f").?;
    const ref: Ref = .{ .module = &module, .id = id, .container_type = box };
    const span = module.getDeclaration(id).?.name_span;
    const boolean = try ir.internType(.boolean);
    const string = try ir.internType(.string);
    const args = [_]Argument{
        .{ .ty = box, .span = span },
        .{ .ty = boolean, .span = span },
        .{ .ty = null, .span = span, .terminal = true },
        .{ .ty = string, .span = span },
    };
    try std.testing.expectEqual(@as(?Ir.TypeId, null), try probeResult(&module, &ir, ref, &args, span));
    const params = (try terminalParameters(&module, &ir, ref, &args, span)).?;
    defer allocator.free(params);
    try std.testing.expectEqualSlices(?Ir.TypeId, &.{ box, boolean, boolean, null }, params);
    try std.testing.expectEqual(count, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    try std.testing.expect(!ir.getType(box).?.record.methods_published);
}

test "backtick instance terminal operands readonly prefix constraints publish no methods signatures or IDs under OOM" {
    try testTerminalProbe(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTerminalProbe, .{});
}

test "backtick instance terminal operands stop constraints and demand at first exit including both branches under OOM" {
    const prelude = "let R=struct{let f:(object:R,a:`T,b:`T,c:`T)->`T=missing\n}\n";
    for ([_][]const u8{
        "let main:()=R[].f(true,@panic(\"stop\"),\"unreachable\")",
        "let main:()=R[].f(true,@panic(\"stop\"),unknown)",
        "let main:()=R[].f(true,loop{},unknown)",
        "let main:()->bool=R[].f(true,loop{},\"wrong\")",
        "let main:()->uint=R[].f(loop{},unknown,unknown)",
        "let main:()->uint=R.f(R[],true,loop{},\"wrong\")",
        "let main:()->uint=R[].f(if true{return 1}else{return 2},true,unknown)",
        "let main:()->uint=R[].f(true,if false{return 1}else{return 2},\"wrong\")",
        "let main:()->uint=R.f(R[],if true{return 1}else{return 2},true,unknown)",
        "let main:()=R.f(R[],true,@panic(\"stop\"),unknown)",
        "let main:()->uint=R[].f({return 1},true,unknown)",
        "@[noreturn]\nlet main:()=R[].f(if true{@panic(\"left\")}else{@panic(\"right\")},true,unknown)",
    }) |tail| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prelude, tail });
        defer std.testing.allocator.free(source);
        try testTerminalOperands(std.testing.allocator, source, null);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTerminalOperands, .{ source, @as(?Module.Diagnostic.Tag, null) });
    }
    for ([_]struct { tail: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .tail = "let main:()=R[].f(true,@panic(\"stop\"))", .tag = .argument_count_mismatch },
        .{ .tail = "let main:()=R[].f(true,@panic(\"stop\"),unknown,unknown)", .tag = .argument_count_mismatch },
        .{ .tail = "let main:()=R[].f(1,@panic(\"stop\"),true)", .tag = .literal_needs_type },
        .{ .tail = "let main:()=R[].f(true,\"wrong\",@panic(\"stop\"))", .tag = .generic_function_conflict },
        .{ .tail = "@[noreturn]\nlet main:()=R[].f(if true{return}else{return},true,unknown)", .tag = .noreturn_return },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prelude, case.tail });
        defer std.testing.allocator.free(source);
        try testTerminalOperands(std.testing.allocator, source, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTerminalOperands, .{ source, @as(?Module.Diagnostic.Tag, case.tag) });
    }
    for ([_][]const u8{
        "let f:(a:`T,b:`T,c:`T)->`T=missing\nlet main:()=f(true,@panic(\"stop\"),\"wrong\")",
        "let f:(a:`T,b:`T)->`T=missing\nlet main:()->uint=f(if true{return 1}else{return 2},unknown)",
        "let id:(x:`T)->`T=x\nlet R=struct{let f:(object:R,cb:(bool)->bool,x:`T,y:`T)->`T=missing\n}\nlet main:()->uint=R[].f(id,if true{return 1}else{return 2},unknown)",
        "let id:(x:`T)->`T=missing\nlet f:(x:`T,cb:(`T)->`T)->`T=missing\nlet main:()->uint=f(if true{return 1}else{return 2},id)",
        "let id:(x:`T)->`T=x\nlet R=struct{let f:(object:R,cb:(`T)->`T,a:`T,b:`T,c:`T)->`T=missing\n}\nlet main:()=R[].f(id,true,@panic(\"stop\"),\"wrong\")",
        "let f:(x:uint,y:`T)->`T=missing\nlet main:()=f(1,@panic(\"stop\"))",
        "let Box:(T:type)->type=struct{value:T,let f:(object:Box(T),a:`U,b:`U,c:`U)->`U=missing\n}\nlet main:()=Box(bool)[.value=true].f(true,@panic(\"stop\"),unknown)",
    }) |source| {
        try testTerminalOperands(std.testing.allocator, source, null);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTerminalOperands, .{ source, @as(?Module.Diagnostic.Tag, null) });
    }
}

test "backtick instance inference receiver callbacks nominal results static canonical IDs and OOM" {
    try testInstanceInference(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInstanceInference, .{});
}

test "backtick instance abandoned fresh nominal probes publish no methods signatures IDs or demand under OOM" {
    for ([_][]const u8{
        "let Box:(T:type)->type=struct{value:T,let map:(object:Box(T),f:(T)->`U)->Box(`U)=Box(`U)[.value=f(object.value)]\n}\nlet make:(x:`T)->Box(`T)=Box(`T)[.value=x]\nlet id:(x:`T)->`T=x\nlet first:(a:`A,b:`B)->`A=a\nlet main:()={let x=first(make(true).map(id),1)\n}",
        "let Box:(T:type)->type=struct{value:T,let get:(object:Box(T))->T=object.value\n}\nlet make:(x:`T)->Box(`T)=Box(`T)[.value=x]\nlet first:(a:`A,b:`B)->`A=a\nlet main:()={let x=first(make(true).get(),1)\n}",
    }) |source| {
        try testSpeculativeFailure(std.testing.allocator, source);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testSpeculativeFailure, .{source});
    }
}

test "backtick instance conflicts unbound variables unsupported values arity and noreturn diagnose under OOM" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let R=struct{let f:(object:R,x:`T,y:`T)->`T=x\n}\nlet main:()={let x=R[].f(true,\"s\")\n}", .tag = .generic_function_conflict },
        .{ .source = "let R=struct{let f:(object:R)->`T=1\n}\nlet main:()={let x=R[].f()\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let R=struct{let f:(object:R,x:`T)->`T={let y:`U=x\ny}\n}\nlet main:()={let x=R[].f(true)\n}", .tag = .unbound_type_variable },
        .{ .source = "let R=struct{let f:(object:R,x:`T)->`T=x\n}\nlet main:()={let x=R[].f()\n}", .tag = .argument_count_mismatch },
        .{ .source = "let R=struct{let f:(object:R,x:`T)->`T=x\n}\nlet main:()={let f=R[].f\n}", .tag = .unknown_field },
        .{ .source = "let R=struct{@[noreturn]\nlet stop:(object:R,x:`T)->`T=loop {}\n}\nlet main:()={R[].stop(true)}", .tag = .noreturn_result },
        .{ .source = "let R=struct{@[noreturn]\nlet stop:(object:R,x:`T)=@panic(\"stop\")\n}\nlet main:()={R[].stop(true)\n@print()}", .tag = .unreachable_code },
    }) |case| {
        try testInferenceFailureAllocations(std.testing.allocator, case.source, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ case.source, case.tag });
    }
}

test "backtick contextual imported public aliases retain private template provenance and visibility under OOM" {
    for ([_][]const u8{ "lib.hidden", "lib.NS.hidden", "lib.G(uint).hidden", "lib.alias", "lib.NS.alias", "lib.G(uint).exposed" }, 0..) |expression, i| {
        try testContextualVisibility(std.testing.allocator, expression, i >= 3);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testContextualVisibility, .{ expression, i >= 3 });
    }
}
