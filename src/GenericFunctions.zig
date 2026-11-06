const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const Compilation = @import("Compilation.zig");
const TypeResolver = @import("TypeResolver.zig");
const Lowering = @import("Lowering.zig");
const Sema = @import("Sema.zig");
const TestAllocations = @import("TestAllocations.zig");
const BodyChecker = @import("BodyChecker.zig");
const primitives = @import("primitives.zig");

pub const max_terms = 8192;
pub const max_depth = 128;
pub const max_specialization_depth = 64;
pub const max_specializations = 1024;
pub const Capture = struct { name: []const u8, ty: Ir.TypeId, template: ?Ref = null, mutable: bool = false };
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
    // Borrowed semantic entries for exact established globals. Solver queries
    // only read them; they never request types or demand declarations.
    analysis: ?*const Sema = null,
    types: []const ?Ir.TypeId = &.{},

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

pub const Argument = struct {
    ty: ?Ir.TypeId,
    span: Module.Span,
    template: ?Ref = null,
    terminal: bool = false,
    // A fallback candidate, not an exact constraint. Structural defaults fill
    // only unresolved leaves after other arguments, expected result, and body.
    default_type: ?Ir.TypeId = null,
};
const Term = struct {
    span: Module.Span,
    value: union(enum) {
        unknown,
        never,
        link: *Term,
        known: Ir.TypeId,
        array: *Term,
        range: *Term,
        reference: struct { child: *Term, mutable: bool },
        function: Function,
        nominal: Nominal,
    },
};
const Nominal = struct { module: *Module, id: Module.DeclId, args: []const *Term };
const Function = struct { params: []const *Term, result: *Term };
const Variable = struct { name: []const u8, term: *Term };
const Local = struct { name: []const u8, term: *Term, template: ?Ref = null, mutable: bool = false, query_global: bool = false };
const Loop = struct { result: *Term, has_break: bool = false, has_continue: bool = false, allows_value: bool };
const Frame = struct {
    ref: Ref,
    variables: []const Variable,
    locals: std.ArrayList(Local) = .empty,
    result: *Term,
    container: ?Nominal = null,
    loop: ?*Loop = null,
    returns_value: bool = false,
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
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = call_span, .analysis = ref.analysis, .types = ref.types };
    const frame = (try solver.constrain(ref, arguments, expected)) orelse return null;
    try solver.applyDefaults();
    if (solver.failed) return null;
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

/// Infer a closure's entire concrete function type without emitting code or
/// capturing values. The descriptor borrows the caller's lexical captures and
/// bindings; the returned canonical type remains valid after this query.
pub fn inferClosure(caller: *Module, ir: *Ir, ref: Ref, expected: ?Ir.TypeId, span: Module.Span) Ir.BuildError!?Ir.TypeId {
    return inferClosureInner(caller, ir, ref, expected, span, false) catch |err| switch (err) {
        error.TooManyEntries => null,
        else => return err,
    };
}

pub fn queryFunction(caller: *Module, ir: *Ir, ref: Ref, span: Module.Span) Ir.BuildError!?Ir.TypeId {
    return inferClosureInner(caller, ir, ref, null, span, true) catch |err| switch (err) {
        error.TooManyEntries => null,
        else => return err,
    };
}

fn inferClosureInner(caller: *Module, ir: *Ir, ref: Ref, expected: ?Ir.TypeId, span: Module.Span, querying: bool) Ir.BuildError!?Ir.TypeId {
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = span, .analysis = ref.analysis, .types = ref.types };
    solver.readonly_layouts = true;
    solver.querying = querying;
    const container = try solver.referenceContainer(ref);
    const instance = (try solver.signature(ref, container, 0)) orelse return null;
    const signature = try solver.term(.{ .function = instance.function }, ref.span());
    if (expected) |ty| try solver.unify(signature, try solver.fromType(ty, span, 0), span, 0);
    var frame = instance.frame;
    try solver.body(&frame, 0);
    try solver.finishBodies();
    if (solver.failed) return null;
    try solver.applyDefaults();
    if (solver.failed) return null;
    if (!Solver.resolved(signature, 0)) {
        try solver.fail(if (solver.unsupported != null) .generic_function_inference_unsupported else .generic_function_ambiguous, solver.unsupported orelse span, ref.span());
        return null;
    }
    const ty = try solver.materialize(signature, 0);
    if (ty == null and !solver.failed) try solver.fail(.generic_function_ambiguous, span, ref.span());
    return ty;
}

/// Infer a concrete closure callee from all arguments and its expected result
/// together, without publishing a runtime generic specialization.
pub fn inferClosureCall(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, expected: ?Argument, span: Module.Span) Ir.BuildError!?Ir.TypeId {
    return inferClosureCallInner(caller, ir, ref, arguments, expected, span) catch |err| switch (err) {
        error.TooManyEntries => null,
        else => return err,
    };
}

fn inferClosureCallInner(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, expected: ?Argument, span: Module.Span) Ir.BuildError!?Ir.TypeId {
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = span, .analysis = ref.analysis, .types = ref.types, .readonly_layouts = true };
    const frame = (try solver.constrain(ref, arguments, expected)) orelse return null;
    try solver.applyDefaults();
    if (solver.failed) return null;
    const signature = try solver.term(.{ .function = .{ .params = solver.parameters, .result = frame.result } }, ref.span());
    if (!Solver.resolved(signature, 0)) {
        try solver.fail(if (solver.unsupported != null) .generic_function_inference_unsupported else .generic_function_ambiguous, solver.unsupported orelse span, ref.span());
        return null;
    }
    const ty = try solver.materialize(signature, 0);
    if (ty == null and !solver.failed) try solver.fail(.generic_function_ambiguous, span, ref.span());
    return ty;
}

/// A call with a terminal operand never demands its callee. Resolve only the
/// reachable parameter contexts; unbound variables need no specialization.
/// The caller owns the returned slice, which retains the full signature arity.
pub fn terminalParameters(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, call_span: Module.Span) Ir.BuildError!?[]?Ir.TypeId {
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = call_span, .analysis = ref.analysis, .types = ref.types };
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
    return probeResultWithDefaults(caller, ir, ref, arguments, call_span, false);
}

/// Readonly fallback query, used only after nondefaulting sibling probes fail.
/// Like probeResult, it never publishes or demands specializations.
pub fn probeDefaultResult(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, call_span: Module.Span) Ir.BuildError!?Ir.TypeId {
    return probeResultWithDefaults(caller, ir, ref, arguments, call_span, true);
}

fn probeResultWithDefaults(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, call_span: Module.Span, defaults: bool) Ir.BuildError!?Ir.TypeId {
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
    return probeResultInner(caller, ir, ref, arguments, call_span, defaults) catch |err| switch (err) {
        error.TooManyEntries => null,
        else => return err,
    };
}

fn probeResultInner(caller: *Module, ir: *Ir, ref: Ref, arguments: []const Argument, call_span: Module.Span, defaults: bool) Ir.BuildError!?Ir.TypeId {
    for (arguments) |arg| if (arg.terminal) return null;
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = call_span, .read_only = true, .analysis = ref.analysis, .types = ref.types };
    const frame = (try solver.constrain(ref, arguments, null)) orelse return null;
    if (defaults) try solver.applyDefaults();
    if (solver.failed) return null;
    for (frame.variables) |variable| if (!Solver.resolved(variable.term, 0)) return null;
    return solver.materialize(frame.result, 0);
}

/// Probe sibling expressions in one constraint graph. Materializing each sibling
/// separately loses exact/default provenance inside nominal and function types.
/// The scope is a descriptor only; no signature or body is demanded for it.
pub fn probeExpressions(caller: *Module, ir: *Ir, scope: Ref, nodes: []const Ast.Node.Index, result_type: Ir.TypeId, defaults: bool) Ir.BuildError!?Ir.TypeId {
    const caller_start = caller.diagnostics.items.len;
    defer caller.diagnostics.shrinkRetainingCapacity(caller_start);
    const starts: []usize = if (caller.compilation) |comp| try caller.allocator.alloc(usize, comp.modules.items.len) else &.{};
    defer if (starts.len != 0) caller.allocator.free(starts);
    if (caller.compilation) |comp| for (comp.modules.items, starts) |module, *start| {
        start.* = module.diagnostics.items.len;
    };
    defer if (caller.compilation) |comp| {
        for (comp.modules.items, starts) |module, start| module.diagnostics.shrinkRetainingCapacity(start);
    };
    return probeExpressionsInner(caller, ir, scope, nodes, result_type, defaults) catch |err| switch (err) {
        error.TooManyEntries => null,
        else => return err,
    };
}

fn probeExpressionsInner(caller: *Module, ir: *Ir, scope: Ref, nodes: []const Ast.Node.Index, result_type: Ir.TypeId, defaults: bool) Ir.BuildError!?Ir.TypeId {
    var arena = std.heap.ArenaAllocator.init(caller.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = caller, .ir = ir, .call_span = scope.span(), .read_only = true, .analysis = scope.analysis, .types = scope.types };
    const local = scope.local.?;
    const variables = try solver.allocator.alloc(Variable, local.outer_bindings.len);
    for (local.outer_bindings, variables) |binding, *variable| variable.* = .{ .name = binding.name, .term = try solver.fromType(binding.ty, solver.call_span, 0) };
    var frame: Frame = .{ .ref = scope, .variables = variables, .result = try solver.fromType(result_type, solver.call_span, 0) };
    if (local.concrete_type) |ty| {
        const concrete = Solver.root(try solver.fromType(@fromBackingInt(ty), solver.call_span, 0));
        if (concrete.value == .nominal) frame.container = concrete.value.nominal;
    }
    for (local.captures) |capture| try frame.locals.append(solver.allocator, .{ .name = capture.name, .term = try solver.fromType(capture.ty, solver.call_span, 0), .template = capture.template, .mutable = capture.mutable });
    const result = try solver.term(.unknown, solver.call_span);
    for (nodes) |node| try solver.unify(result, try solver.expression(&frame, node, 0), caller.tokenSpan(caller.source.ast.firstToken(node)), 0);
    try solver.finishBodies();
    if (solver.failed) return null;
    if (defaults) try solver.applyDefaults();
    if (solver.failed) return null;
    return solver.materialize(result, 0);
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

fn testNestedIntegerDefaults(allocator: std.mem.Allocator, depth: usize) !void {
    const nested = try allocator.alloc(u8, depth * 4 + 1);
    defer allocator.free(nested);
    for (0..depth) |i| @memcpy(nested[i * 3 ..][0..3], "id(");
    nested[depth * 3] = '1';
    @memset(nested[depth * 3 + 1 ..], ')');
    const source = try std.fmt.allocPrint(allocator, "let id:(x:`T)->`T=x\nlet main:()={{let n={s}\nlet exact:int=n\nlet x:u8=2\nlet narrow:u8={s}\nlet items=[{s},x]\nlet exact_items:[]u8=items\nlet branch=if true {{{s}}} else {{x}}\nlet exact_branch:u8=branch\n}}", .{ nested, nested, nested, nested });
    defer allocator.free(source);
    var module = try Module.init(allocator, "nested-integer-defaults.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("nested integer defaults: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

fn testGenericLocalReferences(allocator: std.mem.Allocator) !void {
    const source =
        \\let read:(x:`T)->`T={let p=&x
        \\p.*}
        \\let annotated:(x:`T)->`T={let p:&`T=&x
        \\p.*}
        \\let downgrade:(x:`T)->`T={let mut local=x
        \\let p:&`T=&local
        \\p.*}
        \\let change:(x:`T,y:`T)->`T={let mut first=x
        \\let mut second=y
        \\let mut p=&first
        \\p.* =x
        \\if true {p=&second} else {p=&first}
        \\loop _ in 0..1 {p=&second}
        \\p.*}
        \\let choose:(x:`T,y:`T,flag:bool)->`T={let mut first=x
        \\let mut second=y
        \\let mut p:&`T=&first
        \\let selected=flag and {p=&second
        \\true}
        \\@println(selected,@TypeOf(p))
        \\p.*}
        \\let main:()={
        \\let a:u8=read(1)
        \\let b:u8=annotated(a)
        \\let c:u8=downgrade(b)
        \\let d:u8=change(a,c)
        \\let chosen:u8=choose(a,d,true)
        \\let closure=(v:`T)->`T{let p=&v
        \\p.*}
        \\let e:u8=closure(d)
        \\@println(e)
        \\}
    ;
    var module = try Module.init(allocator, "generic-local-references.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    errdefer if (module.hasErrors()) std.debug.print("generic local references: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "generic explicit local references retain child constraints under OOM" {
    try testGenericLocalReferences(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testGenericLocalReferences, .{});
}

test "integer defaults bound nested identity probe work and preserve late siblings" {
    try testNestedIntegerDefaults(std.testing.allocator, 20);
}

test "integer defaults nested query modes roll back under OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testNestedIntegerDefaults, .{3});
}

fn testNominalIntegerDefaults(allocator: std.mem.Allocator) !void {
    const source =
        \\let Box:(T:type)->type=struct{value:T,}
        \\let make:(x:`T)->Box(`T)=Box(`T)[.value=x]
        \\let get:(x:Box(`T))->`T=x.value
        \\let getArray:(xs:[]Box(`T))->`T=xs[0].value
        \\let getNested:(xs:[][]Box(`T))->`T=xs[0][0].value
        \\let choose:(box:Box(`T),other:`T)->`T=box.value
        \\let main:()={
        \\let n=get(make(1))
        \\let exact:int=n
        \\let a=getArray([make(1)])
        \\let exact_a:int=a
        \\let b=getNested([[make(1)]])
        \\let exact_b:int=b
        \\let narrow:u8=get(make(1))
        \\let narrow_a:u8=getArray([make(1)])
        \\let narrow_b:u8=getNested([[make(1)]])
        \\let sibling=choose(make(1),narrow)
        \\let exact_sibling:u8=sibling
        \\}
    ;
    var module = try Module.init(allocator, "nominal-integer-defaults.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("nominal integer defaults: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "integer defaults descend matching nominal arguments and nested arrays under OOM" {
    try testNominalIntegerDefaults(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testNominalIntegerDefaults, .{});
}

fn testGlobalIntegerDefaults(allocator: std.mem.Allocator) !void {
    const source =
        \\let g=1
        \\let negative=-1
        \\let Record=struct{g:int,}
        \\let globalRecord:()->Record=Record[.g]
        \\let small=@as(u8,1)
        \\let make:()->`T=g
        \\let getNegative:()->`T=negative
        \\let getSmall:()->`T=small
        \\let same:(x:`T)->`T=g
        \\let main:()={
        \\let n=make()
        \\let exact:int=n
        \\let neg=getNegative()
        \\let exact_neg:int=neg
        \\let narrowed=getSmall()
        \\let exact_narrowed:u8=narrowed
        \\let constrained=same(1)
        \\let exact_constrained:int=constrained
        \\let local=()->`T{g}
        \\let closed=local()
        \\let exact_closed:int=closed
        \\}
    ;
    var module = try Module.init(allocator, "global-integer-defaults.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("global integer defaults: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "integer defaults consume established global types as exact generic constraints under OOM" {
    try testGlobalIntegerDefaults(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testGlobalIntegerDefaults, .{});
    for ([_][]const u8{
        "let g=1\nlet make:()->`T=g\nlet main:()={let n:u8=make()\n}",
        "let g=1\nlet make:(x:`T)->`T=g\nlet main:()={let n=make(@as(u8,1))\n}",
        "let g=@as(u8,1)\nlet make:()->`T=g\nlet main:()={let n:int=make()\n}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "global-exact-conflict.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.generic_function_conflict, module.diagnostics.items[0].tag);
        try std.testing.expectEqualStrings("g", module.sourceSlice(module.diagnostics.items[0].span).?);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ source, Module.Diagnostic.Tag.generic_function_conflict });
    }
}

test "float generic defaults shared independent contextual callbacks and nested containers" {
    const source =
        \\let id:(x:`T)->`T=x
        \\let same:(x:`T,y:`T)->`T=x
        \\let add:(x:`A,y:`B)->`C=x+y
        \\let first:(x:`A,y:`B)->`A=x
        \\let make:()->`T=2.5
        \\let apply:(f:(`T)->`U,x:`T)->`U=f(x)
        \\let body:(x:`T)->`T={let y:f32=x
        \\y}
        \\let Box:(T:type)->type=struct{value:T,let get:(self:Box(T))->T=self.value
        \\}
        \\let main:()={
        \\let n:f32=1
        \\let a=id(2.5)
        \\let exact_a:float=a
        \\let b:f32=id(2.5)
        \\let c=same(1,2.5)
        \\let exact_c:float=c
        \\let d=same(2.5,1)
        \\let exact_d:float=d
        \\let e=add(1,2.5)
        \\let exact_e:float=e
        \\let f=add(2.5,1)
        \\let exact_f:float=f
        \\let g=first(1,2.5)
        \\let exact_g:int=g
        \\let h=first(2.5,1)
        \\let exact_h:float=h
        \\let i=same(2.5,n)
        \\let exact_i:f32=i
        \\let j:f32=add(1,2.5)
        \\let k=make()
        \\let exact_k:float=k
        \\let l:f32=make()
        \\let m=apply(id,2.5)
        \\let exact_m:float=m
        \\let callback:f32=apply(id,2.5)
        \\let constrained=body(2.5)
        \\let exact_constrained:f32=constrained
        \\let nested=same([[1]],[[2.5]])
        \\let exact_nested:[][]float=nested
        \\let context_nested:[][]f32=id([[1,2.5]])
        \\let closure=(x:`T)->`T{x+0.5}
        \\let closed=closure(1)
        \\let exact_closed:float=closed
        \\let contextual_closed:f32=closure(1)
        \\let piped:f32=2.5 |> id
        \\let correlated=same(id(1),2.5)
        \\let exact_correlated:float=correlated
        \\let ordinary=id(1)+2.5
        \\let exact_ordinary:float=ordinary
        \\let array=[id(1),2.5]
        \\let exact_array:[]float=array
        \\let wrapped=id(Box(f32)[.value=1])
        \\let field:f32=wrapped.get()
        \\@println(a,b,nested,closed,wrapped)
        \\}
    ;
    var module = try Module.init(std.testing.allocator, "float-generics.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    for (module.diagnostics.items) |diagnostic| std.debug.print("float generic: {any} at {d}: {s}\n", .{ diagnostic.tag, diagnostic.span.start, module.sourceSlice(diagnostic.span).? });
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

fn testFloatDefaultsAllocationFailures(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "float-oom.nov",
        \\let add:(x:`A,y:`B)->`C=x+y
        \\let id:(x:`T)->`T=x
        \\let main:()={let a=add(1,2.5)
        \\let b:f32=add(1,2.5)
        \\let c=id([[1,2.5]])
        \\@println(a,b,c)}
    );
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "float generic defaults roll back every allocation failure" {
    try testFloatDefaultsAllocationFailures(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testFloatDefaultsAllocationFailures, .{});
}

test "float generic defaults never override exact variables or API constraints" {
    const prelude = "let same:(x:`T,y:`T)->`T=x\nlet add:(x:`A,y:`B)->`C=x+y\nlet id:(x:`T)->`T=x\n";
    for ([_][]const u8{
        "let f:(x:int)->int=same(x,2.5)",
        "let f:(x:int)->int=add(x,2.5)",
        "let f:(x:f32,y:f64)->f32=same(x,y)",
        "let f:(x:f32,y:f64)->f32=add(x,y)",
        "let f:()->int=id(2.5)",
        "let f:()->f32=id(true)",
        "let f:(x:int)->f32=id(x)",
        "let f:(x:[]int)->[]int=same(x,[2.5])",
        "let f:()->bool=@assert(id(2.5))",
        "let f:(a:[]int)->int=a[id(1.5)]",
        "let f:()->f32={let a:f64=id(2.5)\na}",
    }) |tail| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prelude, tail });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "float-generic-errors.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        errdefer std.debug.print("float generic negative: {s}\n", .{tail});
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "integer defaults defer generic variables until arguments results and bodies constrain them" {
    const source =
        \\let id: (x: `T) -> `T = x
        \\let same: (x: `T, y: `T) -> `T = x
        \\let add: (x: `A, y: `B) -> `C = x + y
        \\let first: (x: `A, y: `B) -> `A = x
        \\let make: () -> `T = 1
        \\let apply: (f: (`T) -> `U, x: `T) -> `U = f(x)
        \\let main: () = {
        \\let x: u8 = 7
        \\let a = id(1)
        \\let exact_a: int = a
        \\let b: u8 = id(1)
        \\let c = same(1, x)
        \\let exact_c: u8 = c
        \\let d = same(x, 1)
        \\let exact_d: u8 = d
        \\let e = add(1, x)
        \\let exact_e: u8 = e
        \\let f: u8 = add(1, 2)
        \\let g = add(1, 2)
        \\let exact_g: int = g
        \\let h = first(1, x)
        \\let exact_h: int = h
        \\let i: u8 = first(1, 2)
        \\let j = id([1, 2])
        \\let exact_j: []int = j
        \\let k = same([[1]], [[x]])
        \\let exact_k: [][]u8 = k
        \\let l: [][]u8 = id([[1]])
        \\let m = [id(1), x]
        \\let exact_m: []u8 = m
        \\let n = if true {id(1)} else {x}
        \\let exact_n: u8 = n
        \\let o = same(id(1), x)
        \\let exact_o: u8 = o
        \\let p: u8 = id(id(1))
        \\let q = make()
        \\let exact_q: int = q
        \\let r: u8 = make()
        \\let callback = apply(id, 1)
        \\let exact_callback: int = callback
        \\let narrow_callback: u8 = apply(id, 1)
        \\let closure = (v: `T) -> `T {v}
        \\let closed = closure(1)
        \\let exact_closed: int = closed
        \\let narrow_closed: u8 = closure(1)
        \\let piped = 1 |> id
        \\let exact_piped: int = piped
        \\let narrow_pipe: u8 = 1 |> id
        \\let piped_array = [1 |> id, x]
        \\let exact_piped_array: []u8 = piped_array
        \\let compared = id(1) == x
        \\@println(id(1), id(-1), id(1 + 2), id(if true {1} else {2}), id(loop {break 1}))
        \\}
    ;
    var module = try Module.init(std.testing.allocator, "generic-integer-defaults.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("generic integer defaults: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "integer defaults check generic native bounds only after contextual inference" {
    const min: i128 = -(@as(i128, 1) << (Ir.native_pointer_bits - 1));
    const max = -min - 1;
    for ([_]i128{ min, min + 1, max, max + 1, min - 1, -min }) |value| {
        const literal = try std.fmt.allocPrint(std.testing.allocator, "{d}", .{value});
        defer std.testing.allocator.free(literal);
        const source = try std.fmt.allocPrint(std.testing.allocator, "let id:(x:`T)->`T=x\nlet main:()={{let n=id({s})\nlet exact:int=n\n}}", .{literal});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "generic-default-bounds.nov", source);
        defer module.deinit();
        var sema = try Lowering.lower(&module);
        defer sema.deinit();
        if (value >= min and value <= max) {
            try std.testing.expect(sema.lowering_complete);
            try std.testing.expect(!module.hasErrors());
            try sema.ir.verify();
        } else {
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            try std.testing.expectEqual(Module.Diagnostic.Tag.literal_out_of_range, module.diagnostics.items[0].tag);
            try std.testing.expectEqualStrings(literal, module.sourceSlice(module.diagnostics.items[0].span).?);
        }
    }
    const source = try std.fmt.allocPrint(std.testing.allocator, "let id:(x:`T)->`T=x\nlet same:(x:`T,y:`T)->`T=x\nlet main:()={{let wide:u128=id({d})\nlet sibling=same({d},wide)\nlet exact:u128=sibling\n}}", .{ max + 1, max + 1 });
    defer std.testing.allocator.free(source);
    var module = try Module.init(std.testing.allocator, "generic-context-bounds.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

fn testIntegerDefaultProbe(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "generic-default-probe.nov", "let id:(x:`T)->`T=x");
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const ref: Ref = .{ .module = &module, .id = module.lookup("id").? };
    const span = ref.span();
    const integer = try BodyChecker.defaultIntegerType(&ir);
    const arguments = [_]Argument{.{ .ty = null, .span = span, .default_type = integer }};
    const count = module.declarations.items.len;
    try std.testing.expectEqual(@as(?Ir.TypeId, null), try probeResult(&module, &ir, ref, &arguments, span));
    try std.testing.expectEqual(@as(?Ir.TypeId, integer), try probeDefaultResult(&module, &ir, ref, &arguments, span));
    try std.testing.expectEqual(count, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
}

test "integer defaults remain separate from readonly generic result probes under OOM" {
    try testIntegerDefaultProbe(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testIntegerDefaultProbe, .{});
}

fn testIntegerDefaultInference(allocator: std.mem.Allocator) !void {
    const source =
        \\let id:(x:`T)->`T=x
        \\let first:(x:`A,y:`B)->`A=x
        \\let main:()={
        \\let a=first(id(1),[2])
        \\let exact:int=a
        \\let b:u8=first(1,[2])
        \\@println(b,1 |> id)
        \\}
    ;
    var module = try Module.init(allocator, "generic-default-oom.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "integer defaults generic argument fallback and pipes roll back under OOM" {
    try testIntegerDefaultInference(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testIntegerDefaultInference, .{});
}

fn testTypeOfSignatures(allocator: std.mem.Allocator) !void {
    const source =
        \\let id:(x:`T)->@TypeOf(x)={let copy:@TypeOf(x)=x
        \\copy}
        \\let expression:(x:`T,y:@TypeOf(x+1))->@TypeOf(y+1)=y
        \\let same:(x:`T,y:@TypeOf(x))->@TypeOf(y)=y
        \\let apply:(x:`T,f:(v:`T)->@TypeOf(v))->@TypeOf(x)=f(x)
        \\let make:(x:`T)->()->@TypeOf(x)=()->`T {let copy:@TypeOf(x)=x
        \\copy}
        \\let main:()={
        \\let n=id(1)
        \\let exact:int=n
        \\let narrow:u8=id(2)
        \\let expression_result:u8=expression(1,narrow)
        \\let callback:(u16)->u16=id
        \\let a:u8=same(narrow,3)
        \\let b:u16=apply(4,id)
        \\let closure=make(narrow)
        \\let c:u8=closure()
        \\let local:(x:`U)->@TypeOf(x)={let y:@TypeOf(x)=x
        \\y}
        \\let d:bool=local(true)
        \\@println(exact,a,b,c,d,callback(5))
        \\}
    ;
    var module = try Module.init(allocator, "typeof-generics.nov", source);
    defer module.deinit();
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("typeof generics: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

fn testTypeOfProbe(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "typeof-probe.nov", "let id:(x:`T)->@TypeOf(x)=x");
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    const ref: Ref = .{ .module = &module, .id = module.lookup("id").? };
    const span = ref.span();
    const integer = try BodyChecker.defaultIntegerType(&ir);
    const arguments = [_]Argument{.{ .ty = null, .span = span, .default_type = integer }};
    const count = module.declarations.items.len;
    try std.testing.expectEqual(@as(?Ir.TypeId, null), try probeResult(&module, &ir, ref, &arguments, span));
    try std.testing.expectEqual(@as(?Ir.TypeId, integer), try probeDefaultResult(&module, &ir, ref, &arguments, span));
    try std.testing.expectEqual(count, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.local_generic_instances.items.len);
}

fn testStorableFieldProbe(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "field-probe.nov",
        \\let R=struct{callback:(int)->int,}
        \\let query:(r:R)->@TypeOf(r.callback(1))=missing
        \\let invoke:(r:`T)->int=r.callback(1)+1
    );
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const count = module.declarations.items.len;
    const query = module.getDeclaration(module.lookup("query").?).?;
    const signature = (try TypeResolver.resolveInScopeReadOnly(&module, &ir, module.source.ast.declFunction(query.node).?.proto_node, null, null, &.{})).?;
    const function = ir.getType(signature).?.function;
    const integer = try BodyChecker.defaultIntegerType(&ir);
    try std.testing.expectEqual(integer, function.result);
    const ref: Ref = .{ .module = &module, .id = module.lookup("invoke").? };
    const result = try probeResult(&module, &ir, ref, &.{.{ .ty = function.params[0], .span = ref.span() }}, ref.span());
    try std.testing.expectEqual(@as(?Ir.TypeId, integer), result);
    try std.testing.expectEqual(count, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.local_generic_instances.items.len);
    try std.testing.expect(!ir.getType(function.params[0]).?.record.methods_published);
}

test "storable fields strict readonly signatures and generic probes publish no code under OOM" {
    try testStorableFieldProbe(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testStorableFieldProbe, .{});
}

test "TypeOf readonly default queries preserve scopes and publication under OOM" {
    try testTypeOfProbe(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfProbe, .{});
}

test "TypeOf generic signatures share parameter constraints and preserve closure types under OOM" {
    try testTypeOfSignatures(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfSignatures, .{});
}

test "backtick generic function exact constraints demand and canonical aliases" {
    try testSpecialization(std.testing.allocator);
}

test "backtick generic function specialization allocation failures" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testSpecialization, .{});
}

/// Check an expression using only a snapshot of lexical type metadata. No
/// semantic demands, runtime captures, loans, stores, or specializations occur.
pub fn queryExpression(module: *Module, ir: *Ir, node: Ast.Node.Index, bindings: []const Module.TypeBinding, values: []const Module.TypeBinding, captures: []const Capture) Ir.BuildError!?Ir.TypeId {
    const span = module.tokenSpan(module.source.ast.firstToken(node));
    if (module.expression_query_depth == max_depth) {
        try module.diagnostics.append(module.allocator, .{ .tag = .type_query_limit, .span = span });
        return null;
    }
    for (module.expression_query_stack[0..module.expression_query_depth]) |active| if (active == node) {
        try module.diagnostics.append(module.allocator, .{ .tag = .type_query_limit, .span = span });
        return null;
    };
    module.expression_query_stack[module.expression_query_depth] = node;
    module.expression_query_depth += 1;
    defer module.expression_query_depth -= 1;
    var arena = std.heap.ArenaAllocator.init(module.allocator);
    defer arena.deinit();
    var solver: Solver = .{ .allocator = arena.allocator(), .caller = module, .ir = ir, .call_span = span, .readonly_layouts = true, .querying = true };
    const descriptor: LocalTemplate = .{ .proto = 0, .body = node, .outer_bindings = bindings };
    var frame: Frame = .{ .ref = .{ .module = module, .local = &descriptor }, .variables = &.{}, .result = try solver.term(.unknown, span) };
    for (bindings) |binding| {
        const variable = try solver.fromType(binding.ty, span, 0);
        const variables = try solver.allocator.alloc(Variable, frame.variables.len + 1);
        @memcpy(variables[0..frame.variables.len], frame.variables);
        variables[frame.variables.len] = .{ .name = binding.name, .term = variable };
        frame.variables = variables;
    }
    for (values) |value| try frame.locals.append(solver.allocator, .{ .name = value.name, .term = try solver.fromType(value.ty, span, 0), .query_global = module.lookup(value.name) != null });
    for (captures) |capture| try frame.locals.append(solver.allocator, .{ .name = capture.name, .term = try solver.fromType(capture.ty, span, 0), .template = capture.template, .mutable = capture.mutable });
    const result = try solver.expression(&frame, node, 0);
    try solver.finishBodies();
    try solver.applyDefaults();
    if (solver.failed) return null;
    if (!Solver.resolved(result, 0) or solver.unsupported != null) {
        try solver.fail(.unresolved_value_type, solver.unsupported orelse span, null);
        return null;
    }
    return solver.materialize(result, 0);
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
    readonly_layouts: bool = false,
    querying: bool = false,
    query_operations: std.ArrayList(struct { tag: Ast.Node.Tag, operand: *Term, span: Module.Span }) = .empty,
    query_literals: std.ArrayList(struct { operand: *Term, module: *Module, node: Ast.Node.Index }) = .empty,
    query_strings: std.ArrayList(struct { module: *Module, node: Ast.Node.Index }) = .empty,
    query_printables: std.ArrayList(struct { operand: *Term, module: *Module, node: Ast.Node.Index, interpolation: bool = true }) = .empty,
    query_types: std.ArrayList(*Term) = .empty,
    query_locals: std.ArrayList(struct { operand: *Term, module: *Module, node: Ast.Node.Index }) = .empty,
    query_assignments: std.ArrayList(struct { operand: *Term, module: *Module, node: Ast.Node.Index }) = .empty,
    query_unions: std.ArrayList(struct { payload_type: *Term, module: *Module, node: Ast.Node.Index, payload: Ast.Node.Index, enum_payload: bool }) = .empty,
    query_matches: std.ArrayList(struct { module: *Module, node: Ast.Node.Index, subjects: []const *Term }) = .empty,
    query_statements: std.ArrayList(struct { operand: *Term, span: Module.Span }) = .empty,
    query_places: std.ArrayList(struct { operand: *Term, kind: enum { array, record, mutable_reference, borrow_array }, span: Module.Span, length: bool = false }) = .empty,
    analysis: ?*const Sema = null,
    types: []const ?Ir.TypeId = &.{},
    unsupported: ?Module.Span = null,
    pending_bodies: std.ArrayList(struct { frame: Frame, depth: usize, querying: bool }) = .empty,
    projections: std.ArrayList(struct { base: *Term, result: *Term, name: ?[]const u8, span: Module.Span, iteration: bool = false }) = .empty,
    parameters: []const *Term = &.{},
    defaults: std.ArrayList(struct { term: *Term, ty: Ir.TypeId }) = .empty,
    float_defaults_only: bool = false,

    // Connect signatures before inspecting operations that need concrete operand
    // types. In particular, callback-first arguments must see later siblings.
    fn deferBody(self: *Solver, frame: Frame, depth: usize) Ir.BuildError!void {
        try self.pending_bodies.append(self.allocator, .{ .frame = frame, .depth = depth, .querying = self.querying });
    }

    fn finishBodies(self: *Solver) Ir.BuildError!void {
        var index: usize = 0;
        while (index < self.pending_bodies.items.len and !self.failed) : (index += 1) {
            // body() can append more work and relocate pending_bodies.
            var pending = self.pending_bodies.items[index];
            const was_querying = self.querying;
            self.querying = pending.querying;
            defer self.querying = was_querying;
            try self.body(&pending.frame, pending.depth);
        }
        self.pending_bodies.clearRetainingCapacity();
        // Projections may depend on another callback's result or on another
        // projection. Revisit only these operations, not bodies or defaults.
        var progress = true;
        while (progress and !self.failed) {
            progress = false;
            var i: usize = 0;
            while (i < self.projections.items.len) {
                const projection = self.projections.items[i];
                if (root(projection.base).value == .unknown) {
                    i += 1;
                    continue;
                }
                const value = if (projection.iteration)
                    try self.iterationElement(projection.base, projection.span)
                else if (projection.name) |name|
                    try self.fieldType(projection.base, name, projection.span, 0)
                else if (root(projection.base).value == .reference)
                    root(projection.base).value.reference.child
                else
                    try self.term(.unknown, projection.span);
                try self.unify(projection.result, value, projection.span, 0);
                _ = self.projections.swapRemove(i);
                progress = true;
            }
        }
    }

    fn establishedType(self: *Solver, ref: Ref) ?Ir.TypeId {
        if (ref.local != null) return null;
        if (self.analysis) |analysis| {
            const index = analysis.entryIndex(ref.module, ref.id);
            if (index < analysis.entries.len) if (analysis.entries[index].type_id) |ty| return ty;
        }
        const index = if (ref.module.compilation) |comp|
            @backingInt(comp.declaration_ids.get(.{ .source = ref.module.source.id, .local = ref.id }) orelse return null)
        else
            @backingInt(ref.id);
        return if (index < self.types.len) self.types[index] else null;
    }

    fn referenceContainer(self: *Solver, ref: Ref) Ir.BuildError!?Nominal {
        const ty = ref.container_type orelse if (ref.local) |local| blk: {
            break :blk if (local.concrete_type) |concrete| @as(Ir.TypeId, @fromBackingInt(concrete)) else return null;
        } else return null;
        const nominal = root(try self.fromType(ty, ref.span(), 0));
        return if (nominal.value == .nominal) nominal.value.nominal else null;
    }

    // Defaults fill only unresolved leaves after all exact constraints. A
    // candidate must never conflict with a contextual type that already won.
    fn applyDefaults(self: *Solver) Ir.BuildError!void {
        // Apply floating leaves first, including inside containers. Shared soft
        // integer candidates then yield without ever changing an exact type.
        self.float_defaults_only = true;
        for (self.defaults.items) |candidate| try self.defaultTerm(candidate.term, candidate.ty, 0);
        self.float_defaults_only = false;
        for (self.defaults.items) |candidate| try self.defaultTerm(candidate.term, candidate.ty, 0);
        if (self.failed) return;
        const was_readonly_layouts = self.readonly_layouts;
        self.readonly_layouts = true;
        defer self.readonly_layouts = was_readonly_layouts;
        {
            const was_querying = self.querying;
            self.querying = true;
            defer self.querying = was_querying;
            for (self.query_types.items) |intermediate| {
                if (try self.materialize(intermediate, 0) == null) {
                    if (!self.failed) try self.fail(switch (root(intermediate).value) {
                        .array => .array_literal_needs_type,
                        .function => .generic_function_ambiguous,
                        else => .unresolved_value_type,
                    }, intermediate.span, null);
                    return;
                }
            }
        }
        for (self.query_locals.items) |local| {
            const ty = (try self.materialize(local.operand, 0)) orelse {
                try self.fail(.unresolved_value_type, local.operand.span, null);
                return;
            };
            if (!try BodyChecker.validateLocalType(local.module, self.ir, local.node, ty)) {
                self.failed = true;
                return;
            }
        }
        for (self.query_assignments.items) |assignment| {
            if (root(assignment.operand).value == .reference and !BodyChecker.isReferenceRebind(&assignment.module.source.ast, assignment.node)) {
                try self.fail(.unsupported_borrow, assignment.module.tokenSpan(assignment.module.source.ast.firstToken(assignment.node)), null);
                return;
            }
        }
        for (self.query_unions.items) |constructor| {
            const ty = (try self.materialize(constructor.payload_type, 0)) orelse {
                try self.fail(.unresolved_value_type, constructor.payload_type.span, null);
                return;
            };
            if ((constructor.payload == 0) != (self.ir.getType(ty).? == .void)) {
                try self.fail(.invalid_union_constructor, constructor.module.tokenSpan(constructor.module.source.ast.firstToken(constructor.node)), null);
                return;
            }
        }
        for (self.query_places.items) |place| {
            const operand = root(place.operand);
            const valid = switch (place.kind) {
                .array, .borrow_array => operand.value == .array,
                .record => (operand.value == .nominal and !self.tagged(operand)) or (operand.value == .known and self.ir.getType(operand.value.known).? == .record and !self.ir.getType(operand.value.known).?.record.tagged),
                .mutable_reference => operand.value == .reference and operand.value.reference.mutable,
            };
            if (!valid) try self.fail(switch (place.kind) {
                .borrow_array => .unsupported_borrow,
                .mutable_reference => .immutable_assignment,
                .array => .unsupported_assignment_target,
                .record => if (place.length and (operand.value == .array or (operand.value == .known and self.ir.getType(operand.value.known).? == .string))) .unsupported_array_length_write else .unsupported_assignment_target,
            }, place.span, null);
        }
        for (self.query_statements.items) |statement| {
            const operand = root(statement.operand);
            if (operand.value != .never and (operand.value != .known or self.ir.getType(operand.value.known).? != .void)) try self.fail(.unused_block_value, statement.span, null);
        }
        if (self.failed) return;
        if (self.query_literals.items.len != 0 or self.query_strings.items.len != 0 or self.query_matches.items.len != 0 or self.query_printables.items.len != 0 or self.query_unions.items.len != 0) {
            // Resolve all type metadata before copying the pool. Literal decoding
            // and shared pattern validation then own only disposable constants.
            for (self.query_matches.items) |match| for (match.subjects) |subject| {
                _ = try self.materialize(subject, 0);
            };
            for (self.query_printables.items) |printable| {
                _ = try self.materialize(printable.operand, 0);
            }
            var validation = try BodyChecker.QueryValidation.init(self.ir);
            defer validation.deinit();
            for (self.query_unions.items) |constructor| if (constructor.enum_payload) {
                const ty = (try self.materialize(constructor.payload_type, 0)) orelse return;
                if (!try validation.enumOperand(constructor.module, constructor.payload, ty)) self.failed = true;
            };
            for (self.query_literals.items) |literal| {
                const operand = root(literal.operand);
                if (operand.value != .known) continue;
                if (!try validation.literal(literal.module, literal.node, operand.value.known)) self.failed = true;
            }
            for (self.query_strings.items) |string| if (!try validation.stringSyntax(string.module, string.node)) {
                self.failed = true;
            };
            for (self.query_matches.items) |match| {
                const types = try self.allocator.alloc(Ir.TypeId, match.subjects.len);
                for (match.subjects, types) |subject, *ty| ty.* = (try self.materialize(subject, 0)) orelse {
                    try self.fail(.unresolved_value_type, subject.span, null);
                    return;
                };
                if (!try validation.matchPatterns(match.module, match.node, types)) self.failed = true;
            }
            for (self.query_printables.items) |printable| {
                const ty = (try self.materialize(printable.operand, 0)) orelse {
                    try self.fail(.unresolved_value_type, printable.operand.span, null);
                    return;
                };
                const valid = if (printable.interpolation) try validation.printable(printable.module, printable.node, ty) else try validation.printArgument(printable.module, printable.node, ty);
                if (!valid) self.failed = true;
            }
        }
        for (self.query_operations.items) |operation| {
            const operand = root(operation.operand);
            if (operand.value != .known) {
                try self.fail(.operator_not_supported, operation.span, null);
                continue;
            }
            const ty = self.ir.getType(operand.value.known).?;
            if (operation.tag == .negation or operation.tag == .bit_not) {
                if (BodyChecker.unaryTypeIssue(ty, operation.tag)) |issue| try self.fail(issue, operation.span, null);
                continue;
            }
            const valid = switch (operation.tag) {
                .equal_equal, .bang_equal => ty == .integer or ty == .float or ty == .boolean or ty == .rune or ty == .string or ty == .enumeration,
                .less_than, .greater_than, .less_or_equal, .greater_or_equal => ty == .integer or ty == .float,
                .bit_not, .bit_and, .bit_or, .bit_xor, .mod, .assign_mod => ty == .integer,
                .add, .assign_add => ty == .integer or ty == .float or ty == .string,
                else => ty == .integer or ty == .float,
            };
            if (!valid) try self.fail(.operator_not_supported, operation.span, null);
        }
    }

    fn retainFloatDefaults(self: *Solver, value: *Term, depth: usize) void {
        if (depth >= max_depth) return;
        switch (value.value) {
            .known => |ty| if (self.ir.getType(ty).? != .float) {
                value.value = .unknown;
            },
            .array, .range => |element| self.retainFloatDefaults(element, depth + 1),
            .reference => |reference| self.retainFloatDefaults(reference.child, depth + 1),
            .nominal => |n| for (n.args) |arg| {
                self.retainFloatDefaults(arg, depth + 1);
            },
            .function => |f| {
                for (f.params) |param| self.retainFloatDefaults(param, depth + 1);
                self.retainFloatDefaults(f.result, depth + 1);
            },
            else => {},
        }
    }

    fn defaultTerm(self: *Solver, value: *Term, ty: Ir.TypeId, depth: usize) Ir.BuildError!void {
        if (depth >= max_depth) return;
        const t = root(value);
        switch (t.value) {
            .unknown => {
                const fallback = try self.fromType(ty, t.span, depth + 1);
                if (self.float_defaults_only) self.retainFloatDefaults(fallback, depth + 1);
                t.value = .{ .link = fallback };
            },
            .array => |element| switch (self.ir.getType(ty).?) {
                .array => |fallback| try self.defaultTerm(element, fallback, depth + 1),
                else => {},
            },
            .range => |element| switch (self.ir.getType(ty).?) {
                .range => |fallback| try self.defaultTerm(element, fallback, depth + 1),
                else => {},
            },
            .reference => |reference| switch (self.ir.getType(ty).?) {
                .reference => |fallback| if (reference.mutable == fallback.mutable or !reference.mutable) {
                    try self.defaultTerm(reference.child, fallback.child, depth + 1);
                },
                else => {},
            },
            .function => |function| switch (self.ir.getType(ty).?) {
                .function => |fallback| {
                    if (function.params.len != fallback.params.len) return;
                    for (function.params, fallback.params) |param, candidate| try self.defaultTerm(param, candidate, depth + 1);
                    try self.defaultTerm(function.result, fallback.result, depth + 1);
                },
                else => {},
            },
            .nominal => |nominal| switch (self.ir.getType(ty).?) {
                .record => |record| {
                    if (record.source != nominal.module.source.id or record.identity != @backingInt(nominal.id) or record.type_args.len != nominal.args.len) return;
                    for (nominal.args, record.type_args) |arg, fallback| try self.defaultTerm(arg, fallback, depth + 1);
                },
                else => {},
            },
            else => {},
        }
    }

    fn constrain(self: *Solver, ref: Ref, arguments: []const Argument, expected: ?Argument) Ir.BuildError!?Frame {
        const container = try self.referenceContainer(ref);
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
            if (arg.ty) |ty| try self.unifyArgument(param, try self.fromType(ty, arg.span, 0), arg.span);
            if (arg.default_type) |ty| try self.defaults.append(self.allocator, .{ .term = param, .ty = ty });
            if (arg.template) |callback| {
                const callback_container = try self.referenceContainer(callback);
                const candidate = (try self.signature(callback, callback_container, 0)) orelse return null;
                try self.unify(param, try self.term(.{ .function = candidate.function }, arg.span), arg.span, 0);
                const proto = callback.module.source.ast.fullFnProto(callback.function().proto_node).?;
                var iterator = proto.iterate(&callback.module.source.ast);
                var inferred = proto.ast.infer_result;
                while (iterator.next()) |p| inferred = inferred or p.inferred;
                if (inferred or !resolved(param, 0)) try self.deferBody(candidate.frame, 0);
            }
        }
        if (!ref.isNoreturn()) {
            if (expected) |arg| if (arg.ty) |ty| try self.unify(signature_type.result, try self.fromType(ty, arg.span, 0), arg.span, 0);
        }
        try self.body(&frame, 0);
        try self.finishBodies();
        return if (self.failed) null else frame;
    }

    fn resolved(term_value: *Term, depth: usize) bool {
        if (depth >= max_depth) return false;
        return switch (root(term_value).value) {
            .unknown, .never => false,
            .link => unreachable,
            .known => true,
            .array, .range => |element| resolved(element, depth + 1),
            .reference => |reference| resolved(reference.child, depth + 1),
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
        if (self.querying) switch (value) {
            .array, .range, .reference, .function, .nominal => try self.query_types.append(self.allocator, result),
            else => {},
        };
        return result;
    }

    fn root(t: *Term) *Term {
        var current = t;
        while (current.value == .link) current = current.value.link;
        return current;
    }

    fn fromType(self: *Solver, ty: Ir.TypeId, span: Module.Span, depth: usize) Ir.BuildError!*Term {
        if (self.querying and self.ir.getType(ty).? == .record and !try TypeResolver.completeRecordReadOnly(self.caller, self.ir, ty)) {
            self.failed = true;
            return self.term(.unknown, span);
        }
        if (depth >= max_depth) {
            try self.fail(.generic_function_limit, self.call_span, span);
            return self.term(.unknown, span);
        }
        return switch (self.ir.getType(ty).?) {
            .array => |element| self.term(.{ .array = try self.fromType(element, span, depth + 1) }, span),
            .range => |element| self.term(.{ .range = try self.fromType(element, span, depth + 1) }, span),
            .reference => |reference| self.term(.{ .reference = .{ .child = try self.fromType(reference.child, span, depth + 1), .mutable = reference.mutable } }, span),
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
            .array, .range => |element| occurs(t, element, depth + 1),
            .reference => |reference| occurs(t, reference.child, depth + 1),
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

    // Qualifier weakening is local to an argument position, never a general
    // unification rule (function types and substitutions remain invariant).
    fn unifyArgument(self: *Solver, param: *Term, argument: *Term, span: Module.Span) Ir.BuildError!void {
        const formal = root(param);
        const actual = root(argument);
        if (formal.value == .reference and actual.value == .reference and !formal.value.reference.mutable and actual.value.reference.mutable)
            return self.unify(formal.value.reference.child, actual.value.reference.child, span, 0);
        return self.unify(param, argument, span, 0);
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
            .range => |element| return self.unify(element, y.value.range, span, depth + 1),
            .reference => |reference| {
                if (reference.mutable == y.value.reference.mutable) return self.unify(reference.child, y.value.reference.child, span, depth + 1);
            },
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
            .inferred_type => return self.term(.unknown, span),
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
            .ref_type => {
                const data = ast.nodes.items(.data)[node];
                return self.term(.{ .reference = .{ .child = try self.typeNode(frame, data.rhs, depth + 1), .mutable = data.lhs != 0 } }, span);
            },
            .fn_proto, .fn_proto_one => {
                const proto = ast.fullFnProto(node).?;
                const params = try self.allocator.alloc(*Term, proto.ast.params.len);
                const base = frame.locals.items.len;
                defer frame.locals.shrinkRetainingCapacity(base);
                var iterator = proto.iterate(ast);
                for (params) |*param| {
                    const syntax = iterator.next().?;
                    param.* = try self.typeNode(frame, syntax.type_expr, depth + 1);
                    if (syntax.name_token) |token| try frame.locals.append(self.allocator, .{ .name = ast.tokenSlice(token), .term = param.* });
                }
                const result = if (proto.ast.return_type) |r| try self.typeNode(frame, r, depth + 1) else if (proto.ast.infer_result) try self.term(.unknown, span) else try self.fromType(try self.ir.internType(.void), span, depth + 1);
                return self.term(.{ .function = .{ .params = params, .result = result } }, span);
            },
            .call, .call_one => {
                const call = ast.fullCall(node).?;
                if (std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@TypeOf")) {
                    if (call.ast.args.len != 1) {
                        try self.fail(.argument_count_mismatch, span, null);
                        return self.term(.unknown, span);
                    }
                    const operand = call.ast.args[0];
                    if (ast.nodes.items(.tag)[operand] == .identifier) {
                        const name = ast.tokenSlice(ast.firstToken(operand));
                        var i = frame.locals.items.len;
                        while (i != 0) {
                            i -= 1;
                            const local = frame.locals.items[i];
                            if (!std.mem.eql(u8, local.name, name)) continue;
                            if (local.template != null) {
                                try self.fail(.not_a_value, module.tokenSpan(ast.firstToken(operand)), null);
                                return self.term(.unknown, span);
                            }
                            return local.term;
                        }
                        if (module.lookup(name)) |id| {
                            const declaration = module.getDeclaration(id).?;
                            if (!declaration.is_type_alias and !declaration.is_namespace and !declaration.is_generic and !declaration.is_generic_function) {
                                if (self.analysis) |analysis| {
                                    const index = analysis.entryIndex(module, id);
                                    if (index < analysis.entries.len) if (analysis.entries[index].type_id) |ty| return self.fromType(ty, span, depth + 1);
                                }
                                const index = if (module.compilation) |comp|
                                    @backingInt(comp.declaration_ids.get(.{ .source = module.source.id, .local = id }) orelse return self.term(.unknown, span))
                                else
                                    @backingInt(id);
                                if (index < self.types.len) if (self.types[index]) |ty| return self.fromType(ty, span, depth + 1);
                            }
                        }
                    }
                    const was_querying = self.querying;
                    self.querying = true;
                    defer self.querying = was_querying;
                    return self.expression(frame, operand, depth + 1);
                }
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
        if (ty) |known| return self.fromType(known, span, depth + 1);
        self.failed = true;
        return self.term(.unknown, span);
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
            try frame.locals.append(self.allocator, .{ .name = capture.name, .term = try self.fromType(capture.ty, ref.span(), 0), .template = capture.template, .mutable = capture.mutable });
        };
        const established = self.establishedType(ref);
        const signature_term = if (established) |ty| try self.fromType(ty, ref.span(), depth + 1) else try self.typeNode(&frame, function.proto_node, depth + 1);
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
        if (function.body != 0) {
            const value = try self.expressionExpected(frame, function.body, frame.result, depth + 1);
            try self.unify(frame.result, value, frame.ref.module.tokenSpan(ast.firstToken(function.body)), 0);
            const proto = ast.fullFnProto(function.proto_node).?;
            if (proto.ast.infer_result and root(value).value == .never and !frame.returns_value and root(frame.result).value == .unknown)
                try self.unify(frame.result, try self.fromType(try self.ir.internType(.void), frame.ref.span(), 0), frame.ref.span(), 0);
        }
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
        if (self.querying) {
            const names = try self.allocator.alloc(BodyChecker.LexicalName, frame.locals.items.len);
            for (frame.locals.items, names) |binding, *name| name.* = .{ .name = binding.name, .span = binding.term.span };
            if (!try BodyChecker.validateClosureParameterNames(frame.ref.module, proto, names)) self.failed = true;
        }
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
        const span = frame.ref.module.tokenSpan(frame.ref.module.source.ast.firstToken(proto_node));
        const ref = try self.closureReference(frame, proto_node, body_node);
        const instance = (try self.signature(ref, frame.container, depth + 1)) orelse return self.term(.unknown, span);
        try self.deferBody(instance.frame, depth + 1);
        return self.term(.{ .function = instance.function }, span);
    }

    fn enumTag(self: *Solver, value: *Term, name: []const u8, span: Module.Span) Ir.BuildError!bool {
        const base = root(value);
        if (base.value != .known or self.ir.getType(base.value.known).? != .enumeration) return false;
        for (self.ir.getType(base.value.known).?.enumeration.tags) |tag| if (std.mem.eql(u8, tag.name, name)) return true;
        try self.fail(.unknown_field, span, null);
        return true;
    }

    fn field(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const data = ast.nodes.items(.data)[node];
        const span = module.tokenSpan(ast.firstToken(node));
        const name = ast.tokenSlice(data.rhs);
        if (self.querying) {
            var type_qualifier = false;
            if (ast.nodes.items(.tag)[data.lhs] == .identifier) {
                const qualifier_name = ast.tokenSlice(ast.firstToken(data.lhs));
                if (module.lookup(qualifier_name)) |id| type_qualifier = module.getDeclaration(id).?.is_type_alias;
                for (frame.locals.items) |local| if (std.mem.eql(u8, local.name, qualifier_name)) {
                    type_qualifier = false;
                };
            } else if (module.compilation) |comp| {
                if (comp.reference(module, data.lhs)) |ref| type_qualifier = comp.getModule(ref.source).?.getDeclaration(ref.local).?.is_type_alias;
            }
            if (type_qualifier) {
                const qualifier = try self.typeNode(frame, data.lhs, depth + 1);
                if (try self.enumTag(qualifier, name, span)) return qualifier;
            }
        }
        if (self.querying and module.compilation != null) {
            var lexical = false;
            var base_node = data.lhs;
            while (ast.nodes.items(.tag)[base_node] == .field_access) base_node = ast.nodes.items(.data)[base_node].lhs;
            if (ast.nodes.items(.tag)[base_node] == .identifier) {
                const base_name = ast.tokenSlice(ast.firstToken(base_node));
                for (frame.locals.items) |local| if (std.mem.eql(u8, local.name, base_name)) {
                    lexical = true;
                };
            }
            if (!lexical and module.compilation.?.reference(module, node) != null) {
                if (try TypeResolver.queryValue(module, self.ir, node)) |ty| return self.fromType(ty, span, depth);
                self.failed = true;
                return self.term(.unknown, span);
            }
        }
        const base = root(try self.expression(frame, data.lhs, depth + 1));
        if (self.tagged(base)) {
            if (self.querying) try self.fail(.unsupported_body_expression, span, null);
            self.unsupported = self.unsupported orelse span;
            return self.term(.unknown, span);
        }
        return self.fieldType(base, name, span, depth);
    }

    fn fieldType(self: *Solver, base_value: *Term, name: []const u8, span: Module.Span, depth: usize) Ir.BuildError!*Term {
        const base = root(base_value);
        if (base.value == .unknown) {
            const result = try self.term(.unknown, span);
            try self.projections.append(self.allocator, .{ .base = base, .result = result, .name = name, .span = span });
            return result;
        }
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
        if (self.querying) try self.fail(.unknown_field, span, null);
        self.unsupported = self.unsupported orelse span;
        return self.term(.unknown, span);
    }

    fn rangeSource(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const data = ast.nodes.items(.data)[node];
        const span = module.tokenSpan(ast.firstToken(node));
        const element = try self.term(.unknown, span);
        var literals = true;
        var negative = false;
        for ([_]Ast.Node.Index{ data.lhs, data.rhs }) |bound| {
            if (bound == 0) continue;
            const negated = ast.nodes.items(.tag)[bound] == .negation;
            const literal = if (negated) ast.nodes.items(.data)[bound].rhs else bound;
            if (ast.nodes.items(.tag)[literal] != .number_literal) {
                literals = false;
                continue;
            }
            switch (std.zig.parseNumberLiteral(ast.tokenSlice(ast.firstToken(literal)))) {
                .int => |value| negative = negative or (negated and value != 0),
                .big_int => negative = negative or negated,
                else => literals = false,
            }
        }
        // Range literal fallback precedes ordinary integer literal defaults,
        // but still yields to every exact body or signature constraint.
        if (literals) try self.defaults.append(self.allocator, .{ .term = element, .ty = try self.ir.internType(.{ .integer = .{ .signedness = if (negative) .signed else .unsigned, .bits = Ir.native_pointer_bits } }) });
        const start = try self.expression(frame, data.lhs, depth + 1);
        if (root(start).value == .never) return start;
        try self.unify(element, start, span, 0);
        if (data.rhs != 0) {
            const end = try self.expression(frame, data.rhs, depth + 1);
            if (root(end).value == .never) return end;
            try self.unify(element, end, span, 0);
        }
        return self.term(.{ .range = element }, span);
    }

    fn iterationElement(self: *Solver, value: *Term, span: Module.Span) Ir.BuildError!*Term {
        return switch (root(value).value) {
            .array, .range => |element| element,
            .known => |ty| if (self.ir.getType(ty).? == .string)
                self.fromType(try self.ir.internType(.rune), span, 0)
            else
                self.term(.unknown, span),
            .unknown => element: {
                const element = try self.term(.unknown, span);
                try self.projections.append(self.allocator, .{ .base = value, .result = element, .name = null, .span = span, .iteration = true });
                break :element element;
            },
            else => self.term(.unknown, span),
        };
    }

    fn iterationExpression(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const span = module.tokenSpan(ast.firstToken(node));
        const loop = ast.fullRange(node).?;
        const sources = ast.iterationSources(node);
        const bindings = ast.iterationBindings(node);
        const values = try self.allocator.alloc(*Term, sources.len);
        // Sources execute in the enclosing lexical and control scopes. In
        // particular, a terminal prefix must not constrain later sources.
        for (sources, values) |source, *value| {
            value.* = if (ast.nodes.items(.tag)[source] == .range)
                try self.rangeSource(frame, source, depth + 1)
            else
                try self.expression(frame, source, depth + 1);
            if (root(value.*).value == .never) return value.*;
        }
        const scope = frame.locals.items.len;
        defer frame.locals.shrinkRetainingCapacity(scope);
        const unit = try self.fromType(try self.ir.internType(.void), span, 0);
        var context: Loop = .{ .result = try self.term(.unknown, span), .allows_value = loop.else_expr != 0 };
        if (!context.allows_value) try self.unify(context.result, unit, span, 0);
        const parent = frame.loop;
        frame.loop = &context;
        defer frame.loop = parent;
        for (bindings, sources, values) |binding, source, value| {
            const by_ref = ast.nodes.items(.tag)[binding] == .ref_of;
            const capture = if (by_ref) ast.nodes.items(.data)[binding].rhs else binding;
            const element = try self.iterationElement(value, span);
            var mutable = false;
            if (by_ref and ast.nodes.items(.tag)[source] == .identifier) {
                const name = ast.tokenSlice(ast.firstToken(source));
                // Consult only owners, never bindings introduced by this zip.
                for (frame.locals.items[0..scope]) |local| {
                    if (std.mem.eql(u8, local.name, name)) mutable = local.mutable;
                }
            }
            const term_value = if (by_ref)
                try self.term(.{ .reference = .{ .child = element, .mutable = mutable } }, span)
            else
                element;
            try frame.locals.append(self.allocator, .{ .name = ast.tokenSlice(ast.firstToken(capture)), .term = term_value });
        }
        try self.unify(try self.expression(frame, loop.body, depth + 1), unit, span, 0);
        frame.locals.shrinkRetainingCapacity(scope);
        frame.loop = parent;
        if (loop.else_expr != 0) {
            const exhaustion = try self.expression(frame, loop.else_expr, depth + 1);
            try self.unify(context.result, exhaustion, span, 0);
            if (root(exhaustion).value == .never and !context.has_break) return exhaustion;
        }
        return context.result;
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

    fn tagged(self: *Solver, value: *Term) bool {
        return switch (root(value).value) {
            .nominal => |n| n.module.source.ast.isUnion(n.module.source.ast.decl(n.module.getDeclaration(n.id).?.node).ast.init_node),
            .known => |ty| self.ir.getType(ty).? == .record and self.ir.getType(ty).?.record.tagged,
            else => false,
        };
    }

    fn constructorType(self: *Solver, frame: *Frame, base: Ast.Node.Index, expected: ?*Term, depth: usize) Ir.BuildError!?*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        if (ast.nodes.items(.tag)[base] == .enum_literal) return expected;
        if (ast.nodes.items(.tag)[base] != .field_access) return null;
        const qualifier = ast.nodes.items(.data)[base].lhs;
        if (ast.fullCall(qualifier)) |call| {
            if (TypeResolver.genericReference(module, call.ast.fn_expr) == null and !std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This")) return null;
        } else {
            if (ast.nodes.items(.tag)[qualifier] == .identifier) {
                const name = ast.tokenSlice(ast.firstToken(qualifier));
                for (frame.locals.items) |local| if (std.mem.eql(u8, local.name, name)) return null;
            }
            if (module.compilation) |comp| {
                const ref = comp.reference(module, qualifier) orelse return null;
                if (!comp.getModule(ref.source).?.getDeclaration(ref.local).?.is_type_alias) return null;
            } else {
                if (ast.nodes.items(.tag)[qualifier] != .identifier) return null;
                const id = module.lookup(ast.tokenSlice(ast.firstToken(qualifier))) orelse return null;
                if (!module.getDeclaration(id).?.is_type_alias and !TypeResolver.isThisAlias(ast, module.getDeclaration(id).?.node)) return null;
            }
        }
        const ty = try self.typeNode(frame, qualifier, depth + 1);
        return if (self.tagged(ty)) ty else null;
    }

    fn unionConstructor(self: *Solver, frame: *Frame, node: Ast.Node.Index, ty: *Term, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const data = ast.nodes.items(.data)[node];
        const span = module.tokenSpan(ast.firstToken(node));
        if (!self.tagged(ty)) {
            self.unsupported = self.unsupported orelse span;
            return self.term(.unknown, span);
        }
        const base = data.lhs;
        const name = ast.tokenSlice(if (ast.nodes.items(.tag)[base] == .enum_literal) ast.nodes.items(.main_token)[base] else ast.nodes.items(.data)[base].rhs);
        const payload_type = try self.fieldType(ty, name, span, depth + 1);
        if (self.querying) {
            const payload = (try BodyChecker.unionOperand(module, node)) orelse {
                self.failed = true;
                return ty;
            };
            const enum_payload = ast.nodes.items(.tag)[node] == .container_decl_arg and payload != 0;
            try self.query_unions.append(self.allocator, .{ .payload_type = payload_type, .module = module, .node = node, .payload = payload, .enum_payload = enum_payload });
            if (payload != 0 and !enum_payload) {
                const value = try self.expressionExpected(frame, payload, payload_type, depth + 1);
                try self.unify(payload_type, value, span, 0);
            }
            return ty;
        }
        if (ast.nodes.items(.tag)[node] == .array_access or ast.nodes.items(.tag)[node] == .slice) {
            const operand = (try BodyChecker.unionOperand(module, node)) orelse {
                self.failed = true;
                return ty;
            };
            const payload = try self.expressionExpected(frame, operand, payload_type, depth + 1);
            if (root(payload).value == .never) return payload;
            try self.unify(payload_type, payload, span, 0);
        }
        // Container syntax has no executable payload: its lone `.tag` is an
        // enum literal. BodyChecker validates that literal and constructor arity.
        return ty;
    }

    fn writablePlace(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!void {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const span = module.tokenSpan(ast.firstToken(node));
        if (depth >= max_depth) return self.fail(.generic_function_limit, span, null);
        const data = ast.nodes.items(.data)[node];
        switch (ast.nodes.items(.tag)[node]) {
            .identifier => {
                const name = ast.tokenSlice(ast.firstToken(node));
                var i = frame.locals.items.len;
                while (i != 0) {
                    i -= 1;
                    const local = frame.locals.items[i];
                    if (!std.mem.eql(u8, local.name, name)) continue;
                    if (!local.mutable or local.query_global) try self.fail(.immutable_assignment, span, null);
                    return;
                }
                try self.fail(.immutable_assignment, span, null);
            },
            .deref => {
                if (ast.nodes.items(.tag)[data.lhs] != .identifier) return self.fail(.unsupported_assignment_target, span, null);
                const reference = root(try self.expression(frame, data.lhs, depth + 1));
                if (reference.value == .reference and !reference.value.reference.mutable) return self.fail(.immutable_assignment, span, null);
                try self.query_places.append(self.allocator, .{ .operand = reference, .kind = .mutable_reference, .span = span });
            },
            .field_access, .array_access => {
                const base_tag = ast.nodes.items(.tag)[data.lhs];
                if (base_tag != .identifier and base_tag != .field_access and base_tag != .deref) return self.fail(.unsupported_assignment_target, span, null);
                const base = root(try self.expression(frame, data.lhs, depth + 1));
                if (base.value != .unknown) {
                    if (ast.nodes.items(.tag)[node] == .array_access and base.value != .array) return self.fail(.unsupported_assignment_target, span, null);
                    if (ast.nodes.items(.tag)[node] == .field_access and base.value != .nominal and !(base.value == .known and self.ir.getType(base.value.known).? == .record)) {
                        const length = std.mem.eql(u8, ast.tokenSlice(data.rhs), "len") and (base.value == .array or (base.value == .known and self.ir.getType(base.value.known).? == .string));
                        return self.fail(if (length) .unsupported_array_length_write else .unsupported_assignment_target, span, null);
                    }
                }
                try self.query_places.append(self.allocator, .{ .operand = base, .kind = if (ast.nodes.items(.tag)[node] == .field_access) .record else .array, .span = span, .length = ast.nodes.items(.tag)[node] == .field_access and std.mem.eql(u8, ast.tokenSlice(data.rhs), "len") });
                try self.writablePlace(frame, data.lhs, depth + 1);
            },
            else => try self.fail(.unsupported_assignment_target, span, null),
        }
    }

    fn printExpression(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const tag = ast.nodes.items(.tag)[node];
        var type_syntax = tag == .array_type or tag == .fn_proto or tag == .fn_proto_one or tag == .type_variable;
        if (tag == .identifier) {
            const name = ast.tokenSlice(ast.firstToken(node));
            type_syntax = primitives.isPrimitive(name);
            if (module.lookup(name)) |id| type_syntax = module.getDeclaration(id).?.is_type_alias;
            for (frame.locals.items) |local| if (std.mem.eql(u8, local.name, name)) {
                type_syntax = false;
            };
        } else if (tag == .field_access) {
            if (module.compilation) |comp| if (comp.reference(module, node)) |ref| {
                type_syntax = comp.getModule(ref.source).?.getDeclaration(ref.local).?.is_type_alias;
            };
        } else if (ast.fullCall(node)) |call| {
            const name = ast.tokenSlice(ast.firstToken(call.ast.fn_expr));
            type_syntax = std.mem.eql(u8, name, "@TypeOf") or std.mem.eql(u8, name, "@This") or TypeResolver.genericReference(module, call.ast.fn_expr) != null;
        }
        if (type_syntax) {
            _ = try self.typeNode(frame, node, depth + 1);
            return self.fromType(try self.ir.internType(.string), module.tokenSpan(ast.firstToken(node)), 0);
        }
        return self.expression(frame, node, depth);
    }

    fn expression(self: *Solver, frame: *Frame, node: Ast.Node.Index, depth: usize) Ir.BuildError!*Term {
        return self.expressionExpected(frame, node, null, depth);
    }

    fn expressionExpected(self: *Solver, frame: *Frame, node: Ast.Node.Index, expected: ?*Term, depth: usize) Ir.BuildError!*Term {
        const module = frame.ref.module;
        const ast = &module.source.ast;
        const span = module.tokenSpan(ast.firstToken(node));
        if (depth >= max_depth) try self.fail(.generic_function_limit, self.call_span, span);
        if (self.failed) return self.term(.unknown, span);
        const data = ast.nodes.items(.data)[node];
        if (self.querying and ast.nodes.items(.tag)[node] == .identifier) {
            const name = ast.tokenSlice(ast.firstToken(node));
            var i = frame.locals.items.len;
            while (i != 0) {
                i -= 1;
                const local = frame.locals.items[i];
                if (!std.mem.eql(u8, local.name, name)) continue;
                if (local.template != null) {
                    if (expected) |context| if (root(context).value == .function) break;
                    try self.fail(.not_a_value, span, null);
                    return self.term(.unknown, span);
                }
                return local.term;
            }
            if (module.lookup(name)) |id| {
                const declaration = module.getDeclaration(id).?;
                const callback_context = if (expected) |context| root(context).value == .function else false;
                if ((declaration.is_generic_function and !callback_context) or declaration.is_type_alias or declaration.is_namespace or declaration.is_generic) {
                    try self.fail(.not_a_value, span, null);
                    return self.term(.unknown, span);
                }
            }
        }
        if (try self.callTarget(frame, node, depth + 1)) |candidate| {
            if (candidate.ref.local != null or candidate.ref.module.getDeclaration(candidate.ref.id).?.is_generic_function or
                candidate.ref.module.source.ast.fullFnProto(candidate.ref.function().proto_node).?.ast.infer_result)
            {
                const instance = (try self.signature(candidate.ref, candidate.container, depth + 1)) orelse return self.term(.unknown, span);
                const signature_term = try self.term(.{ .function = instance.function }, span);
                if (self.querying and expected != null) try self.unify(signature_term, expected.?, span, 0);
                if (!self.querying or TypeResolver.hasInferredSignature(&candidate.ref.module.source.ast, candidate.ref.function().proto_node)) try self.deferBody(instance.frame, depth + 1);
                return signature_term;
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
                if (self.querying) {
                    if (try TypeResolver.queryValue(module, self.ir, node)) |ty| return self.fromType(ty, span, 0);
                    self.failed = true;
                    return self.term(.unknown, span);
                }
                if (module.lookup(name)) |id| {
                    const decl = module.getDeclaration(id).?;
                    if (!decl.is_generic_function and !decl.is_type_alias) {
                        const full = ast.decl(decl.node).ast;
                        if (full.type_node != 0) {
                            var global_frame = frame.*;
                            global_frame.ref = .{ .module = module, .id = id };
                            return self.typeNode(&global_frame, full.type_node, depth + 1);
                        }
                        if (self.analysis) |analysis| {
                            const index = analysis.entryIndex(module, id);
                            if (index < analysis.entries.len) {
                                const entry = analysis.entries[index];
                                if (entry.status == .constant_checked) if (entry.type_id) |ty| return self.fromType(ty, span, 0);
                            }
                        }
                        const index = if (module.compilation) |comp|
                            @backingInt(comp.declaration_ids.get(.{ .source = module.source.id, .local = id }) orelse return self.term(.unknown, span))
                        else
                            @backingInt(id);
                        if (index < self.types.len) if (self.types[index]) |ty| return self.fromType(ty, span, 0);
                    }
                }
            },
            .enum_literal => if (self.querying) {
                if (expected) |context| {
                    const name = ast.tokenSlice(ast.nodes.items(.main_token)[node]);
                    if (try self.enumTag(context, name, span)) return context;
                    if (self.tagged(context)) {
                        const payload = root(try self.fieldType(context, name, span, depth + 1));
                        if (payload.value != .known or self.ir.getType(payload.value.known).? != .void) try self.fail(.invalid_union_constructor, span, null);
                        return context;
                    }
                }
                try self.fail(.enum_needs_type, span, null);
                return self.term(.unknown, span);
            },
            .string_literal => {
                if (self.querying) {
                    try self.query_strings.append(self.allocator, .{ .module = module, .node = node });
                    for (ast.extra_data[data.lhs..data.rhs]) |embedded| {
                        if (embedded == 0) continue; // The shared syntax validator diagnoses malformed placeholders.
                        const value = try self.printExpression(frame, embedded, depth + 1);
                        try self.query_printables.append(self.allocator, .{ .operand = value, .module = module, .node = embedded });
                    }
                }
                return self.fromType(try self.ir.internType(.string), span, 0);
            },
            .char_literal => {
                const value = try self.fromType(try self.ir.internType(.rune), span, 0);
                if (self.querying) try self.query_literals.append(self.allocator, .{ .operand = value, .module = module, .node = node });
                return value;
            },
            .number_literal => {
                const value = try self.term(.unknown, span);
                if (self.querying) try self.query_literals.append(self.allocator, .{ .operand = value, .module = module, .node = node });
                switch (std.zig.parseNumberLiteral(ast.tokenSlice(ast.firstToken(node)))) {
                    .int, .big_int => try self.defaults.append(self.allocator, .{ .term = value, .ty = try BodyChecker.defaultIntegerType(self.ir) }),
                    .float => try self.defaults.append(self.allocator, .{ .term = value, .ty = try BodyChecker.defaultFloatType(self.ir) }),
                    else => {},
                }
                return value;
            },
            .ref_of => {
                const operand = data.rhs;
                if (self.querying and ast.nodes.items(.tag)[operand] != .identifier and ast.nodes.items(.tag)[operand] != .array_access) {
                    try self.fail(.unsupported_borrow, span, null);
                    return self.term(.unknown, span);
                }
                const owner = if (ast.nodes.items(.tag)[operand] == .array_access) ast.nodes.items(.data)[operand].lhs else operand;
                var mutable = false;
                var lexical = false;
                if (ast.nodes.items(.tag)[owner] == .identifier) {
                    const name = ast.tokenSlice(ast.firstToken(owner));
                    for (frame.locals.items) |local| {
                        if (!std.mem.eql(u8, local.name, name)) continue;
                        mutable = local.mutable;
                        lexical = !local.query_global and local.template == null;
                    }
                }
                if (self.querying) {
                    if (!lexical) {
                        try self.fail(.unsupported_borrow, span, null);
                        return self.term(.unknown, span);
                    }
                    if (ast.nodes.items(.tag)[operand] == .array_access) try self.query_places.append(self.allocator, .{ .operand = try self.expression(frame, owner, depth + 1), .kind = .borrow_array, .span = span });
                }
                return self.term(.{ .reference = .{ .child = try self.expression(frame, operand, depth + 1), .mutable = mutable } }, span);
            },
            .deref => {
                const operand = root(try self.expression(frame, data.lhs, depth + 1));
                if (operand.value == .reference) return operand.value.reference.child;
                if (self.querying and operand.value != .unknown) try self.fail(.unsupported_borrow, span, null);
                const result = try self.term(.unknown, span);
                if (operand.value == .unknown) try self.projections.append(self.allocator, .{ .base = operand, .result = result, .name = null, .span = span });
                return result;
            },
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
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => {
                const lhs = try self.expression(frame, data.lhs, depth + 1);
                if (root(lhs).value == .never) return lhs;
                if (self.querying) {
                    try self.writablePlace(frame, data.lhs, depth + 1);
                    try self.query_assignments.append(self.allocator, .{ .operand = lhs, .module = module, .node = node });
                }
                const rhs = try self.expression(frame, data.rhs, depth + 1);
                if (root(rhs).value == .never) return rhs;
                if (self.querying) {
                    if (ast.nodes.items(.tag)[node] != .assign) try self.query_operations.append(self.allocator, .{ .tag = ast.nodes.items(.tag)[node], .operand = lhs, .span = span });
                }
                try self.unifyArgument(lhs, rhs, span);
                return self.fromType(try self.ir.internType(.void), span, 0);
            },
            .negation, .bit_not => {
                const numbers = self.query_literals.items.len;
                const operand = try self.expression(frame, data.rhs, depth + 1);
                var literal_node = data.rhs;
                var buffer: [2]Ast.Node.Index = undefined;
                while (ast.fullBlock(&buffer, literal_node)) |block| {
                    if (ast.firstToken(literal_node) != block.ast.lbrace or block.ast.statements.len != 1) break;
                    literal_node = block.ast.statements[0];
                }
                if (self.querying and ast.nodes.items(.tag)[node] == .negation and ast.nodes.items(.tag)[literal_node] == .number_literal and self.query_literals.items.len == numbers + 1) self.query_literals.items[numbers].node = node;
                if (self.querying) try self.query_operations.append(self.allocator, .{ .tag = ast.nodes.items(.tag)[node], .operand = operand, .span = span });
                return operand;
            },
            .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => {
                const lhs = try self.expression(frame, data.lhs, depth + 1);
                if (root(lhs).value == .never) return lhs;
                const rhs = try self.expression(frame, data.rhs, depth + 1);
                if (root(rhs).value == .never) return rhs;
                try self.unify(lhs, rhs, span, 0);
                if (self.querying) try self.query_operations.append(self.allocator, .{ .tag = ast.nodes.items(.tag)[node], .operand = lhs, .span = span });
                return switch (ast.nodes.items(.tag)[node]) {
                    .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => self.fromType(try self.ir.internType(.boolean), span, 0),
                    else => lhs,
                };
            },
            .array_access => {
                if (try self.constructorType(frame, data.lhs, expected, depth + 1)) |ty|
                    return self.unionConstructor(frame, node, ty, depth + 1);
                // Context-free union syntax is still not ordinary indexing.
                if (ast.nodes.items(.tag)[data.lhs] == .enum_literal) {
                    self.unsupported = self.unsupported orelse span;
                    return self.term(.unknown, span);
                }
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
            .slice => {
                if (try self.constructorType(frame, data.lhs, expected, depth + 1)) |ty|
                    return self.unionConstructor(frame, node, ty, depth + 1);
                const slice = ast.slice(node).ast;
                const string = try self.fromType(try self.ir.internType(.string), span, 0);
                try self.unify(string, try self.expression(frame, slice.sliced, depth + 1), span, 0);
                const index = try self.fromType(try BodyChecker.defaultIntegerType(self.ir), span, 0);
                for ([_]Ast.Node.Index{ slice.start, slice.end }) |bound| if (bound != 0) {
                    try self.unify(index, try self.expressionExpected(frame, bound, index, depth + 1), span, 0);
                };
                return string;
            },
            .container_decl_arg => {
                if (try self.constructorType(frame, data.lhs, expected, depth + 1)) |ty|
                    return self.unionConstructor(frame, node, ty, depth + 1);
                const init = ast.fullStructInit(node).?;
                const record = try self.typeNode(frame, init.type_node, depth + 1);
                const concrete = (try self.materialize(record, 0)) orelse return self.term(.unknown, span);
                const ty = self.ir.getType(concrete).?;
                if (ty != .record or ty.record.tagged) {
                    try self.fail(.invalid_union_constructor, span, null);
                    return record;
                }
                const seen = try self.allocator.alloc(bool, ty.record.fields.len);
                @memset(seen, false);
                for (init.fields) |field_node| {
                    const name = ast.tokenSlice(ast.nodes.items(.main_token)[field_node]);
                    for (ty.record.fields, 0..) |field_value, i| {
                        if (!std.mem.eql(u8, field_value.name, name)) continue;
                        if (seen[i]) try self.fail(.duplicate_field, span, null);
                        seen[i] = true;
                        const rhs = ast.nodes.items(.data)[field_node].rhs;
                        const field_term = try self.fromType(field_value.ty, span, 0);
                        try self.unifyArgument(field_term, try self.expressionExpected(frame, if (rhs == 0) field_node else rhs, field_term, depth + 1), span);
                        break;
                    } else try self.fail(.unknown_field, span, null);
                }
                for (seen, ty.record.fields) |supplied, field_value| if (!supplied and field_value.default == null) {
                    try self.fail(.missing_field, span, field_value.span);
                };
                return record;
            },
            .match => {
                if (!self.querying) {
                    self.unsupported = self.unsupported orelse span;
                    return self.term(.unknown, span);
                }
                const match = ast.fullMatch(node).?.ast;
                const subjects = try self.allocator.alloc(*Term, match.subjects.len);
                for (subjects, match.subjects) |*subject, syntax| subject.* = try self.expression(frame, syntax, depth + 1);
                const result = expected orelse try self.term(.unknown, span);
                try self.query_matches.append(self.allocator, .{ .module = module, .node = node, .subjects = subjects });
                for (match.cases) |case| {
                    const prong = ast.fullMatchCase(case).?.ast;
                    const scope = frame.locals.items.len;
                    defer frame.locals.shrinkRetainingCapacity(scope);
                    if (prong.capture_token) |token| {
                        const name = ast.tokenSlice(token);
                        if (subjects.len != 1 or prong.values.len != 1 or ast.nodes.items(.tag)[prong.values[0]] != .enum_literal or std.mem.eql(u8, name, "_") or std.mem.startsWith(u8, name, "@\"")) {
                            try self.fail(.invalid_match_capture, module.tokenSpan(token), null);
                            return result;
                        }
                        for (frame.locals.items) |local| if (std.mem.eql(u8, local.name, name)) {
                            try self.fail(.unsupported_shadowing, module.tokenSpan(token), null);
                        };
                        const payload = try self.fieldType(subjects[0], ast.tokenSlice(ast.nodes.items(.main_token)[prong.values[0]]), span, depth + 1);
                        try frame.locals.append(self.allocator, .{ .name = name, .term = payload });
                    }
                    try self.unify(result, try self.expressionExpected(frame, prong.target_expr, result, depth + 1), span, 0);
                }
                return result;
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
                const statements = ast.fullBlock(&buffer, node).?.ast.statements;
                for (statements, 0..) |statement, statement_index| {
                    if (ast.fullDecl(statement)) |decl| {
                        if (self.querying) {
                            const name = ast.tokenSlice(decl.ast.let_token + 1 + @intFromBool(decl.ast.is_mutable));
                            var related: ?Module.Span = null;
                            if (module.lookup(name)) |id| related = module.getDeclaration(id).?.name_span;
                            for (frame.locals.items) |local| if (std.mem.eql(u8, local.name, name)) {
                                related = local.term.span;
                            };
                            if (!try BodyChecker.validateLocalSyntax(module, statement, related)) {
                                self.failed = true;
                                return self.term(.unknown, span);
                            }
                        }
                        var named_function = false;
                        if (ast.fullFnProto(decl.ast.type_node)) |proto| {
                            var iterator = proto.iterate(ast);
                            while (iterator.next()) |param| if (param.name_token != null) {
                                named_function = true;
                            };
                        }
                        const closure_proto = if (named_function) decl.ast.type_node else if (decl.ast.init_node != 0 and ast.nodes.items(.tag)[decl.ast.init_node] == .function_expr) ast.nodes.items(.data)[decl.ast.init_node].lhs else 0;
                        if (closure_proto != 0 and !signatureVariablesBound(frame, closure_proto)) {
                            if (self.querying and decl.ast.is_mutable) {
                                try self.fail(.generic_function_context, module.tokenSpan(ast.firstToken(statement)), null);
                                return self.term(.unknown, span);
                            }
                            const closure_body = if (named_function) decl.ast.init_node else ast.nodes.items(.data)[decl.ast.init_node].rhs;
                            const ref = try self.closureReference(frame, closure_proto, closure_body);
                            try frame.locals.append(self.allocator, .{ .name = ast.tokenSlice(decl.ast.let_token + 1 + @intFromBool(decl.ast.is_mutable)), .term = try self.term(.unknown, span), .template = ref });
                            result = try self.fromType(try self.ir.internType(.void), span, 0);
                            continue;
                        }
                        const context = if (decl.ast.type_node != 0 and signatureVariablesBound(frame, decl.ast.type_node)) try self.typeNode(frame, decl.ast.type_node, depth + 1) else null;
                        const value = if (decl.ast.init_node == 0)
                            try self.term(.unknown, span)
                        else if (named_function)
                            try self.closure(frame, decl.ast.type_node, decl.ast.init_node, depth + 1)
                        else
                            try self.expressionExpected(frame, decl.ast.init_node, context, depth + 1);
                        if (root(value).value == .never) {
                            if (self.querying and statement_index + 1 < statements.len) try self.fail(.unreachable_code, module.tokenSpan(ast.firstToken(statements[statement_index + 1])), null);
                            return value;
                        }
                        if (decl.ast.type_node != 0) {
                            if (ast.fullFnProto(decl.ast.type_node) != null and !signatureVariablesBound(frame, decl.ast.type_node)) {
                                try self.fail(.generic_function_context, module.tokenSpan(ast.firstToken(decl.ast.type_node)), null);
                            } else {
                                const annotation = context orelse try self.typeNode(frame, decl.ast.type_node, depth + 1);
                                const inferred = root(value);
                                const annotated = root(annotation);
                                if (ast.nodes.items(.tag)[decl.ast.init_node] == .ref_of and inferred.value == .reference and annotated.value == .reference and inferred.value.reference.mutable and !annotated.value.reference.mutable) {
                                    try self.unify(inferred.value.reference.child, annotated.value.reference.child, span, 0);
                                    inferred.value = annotated.value;
                                } else try self.unify(value, annotation, module.tokenSpan(ast.firstToken(statement)), 0);
                            }
                        }
                        if (self.querying) try self.query_locals.append(self.allocator, .{ .operand = value, .module = module, .node = statement });
                        try frame.locals.append(self.allocator, .{ .name = ast.tokenSlice(decl.ast.let_token + 1 + @intFromBool(decl.ast.is_mutable)), .term = value, .mutable = decl.ast.is_mutable });
                        result = try self.fromType(try self.ir.internType(.void), span, 0);
                    } else {
                        result = try self.expressionExpected(frame, statement, if (!self.querying or statement_index + 1 == statements.len) expected else null, depth + 1);
                        if (self.querying and statement_index + 1 < statements.len) try self.query_statements.append(self.allocator, .{ .operand = result, .span = module.tokenSpan(ast.firstToken(statement)) });
                    }
                    if (root(result).value == .never) {
                        if (self.querying and statement_index + 1 < statements.len) try self.fail(.unreachable_code, module.tokenSpan(ast.firstToken(statements[statement_index + 1])), null);
                        return result;
                    }
                }
                return result;
            },
            .@"if", .if_else => {
                const branch = ast.fullIf(node).?.ast;
                const condition = try self.expression(frame, branch.cond_expr, depth + 1);
                if (root(condition).value == .never) return condition;
                try self.unify(condition, try self.fromType(try self.ir.internType(.boolean), span, 0), span, 0);
                const then = try self.expressionExpected(frame, branch.then_expr, expected, depth + 1);
                const otherwise = if (branch.else_expr == 0) try self.fromType(try self.ir.internType(.void), span, 0) else try self.expressionExpected(frame, branch.else_expr, expected, depth + 1);
                try self.unify(then, otherwise, span, 0);
                return if (root(then).value == .never) otherwise else then;
            },
            .@"return" => {
                const value = if (data.rhs == 0) try self.fromType(try self.ir.internType(.void), span, 0) else try self.expressionExpected(frame, data.rhs, frame.result, depth + 1);
                if (data.rhs != 0 and root(value).value != .never) frame.returns_value = true;
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
            .loop_range => return self.iterationExpression(frame, node, depth + 1),
            .range => {
                if (data.rhs == 0) {
                    self.unsupported = self.unsupported orelse span;
                    return self.term(.unknown, span);
                }
                return self.rangeSource(frame, node, depth + 1);
            },
            .function_expr => return self.closure(frame, data.lhs, data.rhs, depth + 1),
            .call, .call_one, .function_pipe => {
                const pipe_args = [_]Ast.Node.Index{data.lhs};
                const call: Ast.full.Call = if (ast.nodes.items(.tag)[node] == .function_pipe)
                    .{ .ast = .{ .fn_expr = data.rhs, .args = &pipe_args, .lparen = ast.nodes.items(.main_token)[node] } }
                else
                    ast.fullCall(node).?;
                if (std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@as") and call.ast.args.len == 2) {
                    const ty = try self.typeNode(frame, call.ast.args[0], depth + 1);
                    const value = try self.expressionExpected(frame, call.ast.args[1], ty, depth + 1);
                    if (root(value).value == .never) return value;
                    try self.unify(ty, value, span, 0);
                    return ty;
                }
                const callee_name = ast.tokenSlice(ast.firstToken(call.ast.fn_expr));
                if (self.querying and ((std.mem.eql(u8, callee_name, "@panic") and call.ast.args.len != 1) or (std.mem.eql(u8, callee_name, "@assert") and (call.ast.args.len == 0 or call.ast.args.len > 2)))) {
                    try self.fail(.argument_count_mismatch, span, null);
                    return self.term(.unknown, span);
                }
                if (ast.nodes.items(.tag)[call.ast.fn_expr] == .builtin_literal and
                    (std.mem.eql(u8, callee_name, "@panic") or std.mem.eql(u8, callee_name, "@print") or std.mem.eql(u8, callee_name, "@println") or std.mem.eql(u8, callee_name, "@assert")))
                {
                    for (call.ast.args, 0..) |arg, index| {
                        const printing = self.querying and (std.mem.eql(u8, callee_name, "@print") or std.mem.eql(u8, callee_name, "@println"));
                        const value = if (printing) try self.printExpression(frame, arg, depth + 1) else try self.expression(frame, arg, depth + 1);
                        if (printing) try self.query_printables.append(self.allocator, .{ .operand = value, .module = module, .node = arg, .interpolation = false });
                        if (root(value).value == .never) return value;
                        if (std.mem.eql(u8, callee_name, "@panic") or std.mem.eql(u8, callee_name, "@assert")) {
                            const ty = try self.ir.internType(if (std.mem.eql(u8, callee_name, "@assert") and index == 0) .boolean else .string);
                            try self.unify(value, try self.fromType(ty, span, 0), span, 0);
                        }
                    }
                    return if (!self.querying and std.mem.eql(u8, callee_name, "@panic")) self.term(.never, span) else self.fromType(try self.ir.internType(.void), span, 0);
                }
                if ((try self.callTarget(frame, call.ast.fn_expr, depth)) orelse (try self.instanceCallTarget(frame, call.ast.fn_expr, depth))) |callee_target| {
                    if (try self.signature(callee_target.ref, callee_target.container, depth + 1)) |instance| {
                        var child = instance.frame;
                        const f = instance.function;
                        const offset: usize = @intFromBool(callee_target.receiver != null);
                        if (f.params.len == call.ast.args.len + offset) {
                            if (callee_target.receiver) |receiver| try self.unify(f.params[0], receiver, span, 0);
                            for (f.params[offset..], call.ast.args) |param, arg| {
                                const value = try self.expressionExpected(frame, arg, param, depth + 1);
                                if (root(value).value == .never) return value;
                                try self.unifyArgument(param, value, module.tokenSpan(ast.firstToken(arg)));
                            }
                            if (!self.querying and callee_target.ref.isNoreturn()) return self.term(.never, span);
                            const needs_body = if (self.querying)
                                TypeResolver.hasInferredSignature(&callee_target.ref.module.source.ast, callee_target.ref.function().proto_node)
                            else
                                callee_target.ref.local != null or callee_target.ref.module.getDeclaration(callee_target.ref.id).?.is_generic_function or
                                    (self.establishedType(callee_target.ref) == null and callee_target.ref.module.source.ast.fullFnProto(callee_target.ref.function().proto_node).?.ast.infer_result);
                            if (needs_body) try self.body(&child, depth + 1);
                            return f.result;
                        }
                        if (self.querying) {
                            try self.fail(.argument_count_mismatch, span, null);
                            return self.term(.unknown, span);
                        }
                    }
                }
                const callee = root(try self.expression(frame, call.ast.fn_expr, depth + 1));
                if (callee.value == .never) return callee;
                if (callee.value == .function and callee.value.function.params.len == call.ast.args.len) {
                    const f = callee.value.function;
                    for (f.params, call.ast.args) |param, arg| {
                        const value = try self.expressionExpected(frame, arg, param, depth + 1);
                        if (root(value).value == .never) return value;
                        try self.unifyArgument(param, value, span);
                    }
                    return f.result;
                }
                if (self.querying) try self.fail(if (callee.value == .function) .argument_count_mismatch else .not_callable, span, null);
            },
            else => {},
        }
        if (self.querying) try self.fail(.unsupported_body_expression, span, null);
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
            .reference => |reference| blk: {
                const child = (try self.materialize(reference.child, depth + 1)) orelse break :blk null;
                if (!Ir.isArrayElement(self.ir.getType(child).?)) {
                    if (self.querying) try self.fail(.unsupported_borrow, t.span, null);
                    break :blk null;
                }
                break :blk try self.ir.internType(.{ .reference = .{ .child = child, .mutable = reference.mutable } });
            },
            .array => |element| blk: {
                const ty = (try self.materialize(element, depth + 1)) orelse break :blk null;
                if (!Ir.isArrayElement(self.ir.getType(ty).?)) {
                    try self.fail(.unsupported_type, self.call_span, t.span);
                    break :blk null;
                }
                break :blk try self.ir.internType(.{ .array = ty });
            },
            .range => |element| blk: {
                const ty = (try self.materialize(element, depth + 1)) orelse break :blk null;
                if (self.ir.getType(ty).? != .integer) break :blk null;
                break :blk try self.ir.internType(.{ .range = ty });
            },
            .function => |f| blk: {
                const params = try self.allocator.alloc(Ir.TypeId, f.params.len);
                for (f.params, params) |param, *out| {
                    out.* = (try self.materialize(param, depth + 1)) orelse break :blk null;
                    if (self.ir.getType(out.*).? == .void) {
                        try self.fail(.invalid_parameter_type, self.call_span, param.span);
                        break :blk null;
                    }
                }
                const result = (try self.materialize(f.result, depth + 1)) orelse break :blk null;
                if (self.ir.getType(result).? == .reference) {
                    try self.fail(.borrow_escape, self.call_span, f.result.span);
                    break :blk null;
                }
                break :blk try self.ir.internType(.{ .function = .{ .params = params, .result = result } });
            },
            .nominal => |n| blk: {
                const args = try self.allocator.alloc(Ir.TypeId, n.args.len);
                for (n.args, args) |arg, *out| out.* = (try self.materialize(arg, depth + 1)) orelse break :blk null;
                if (self.read_only or self.readonly_layouts) break :blk try TypeResolver.instantiateArgumentsReadOnly(n.module, self.ir, n.id, args, 0);
                break :blk try TypeResolver.instantiateArguments(n.module, self.ir, n.id, args, 0);
            },
        };
    }
};

fn testReferenceParameterInference(allocator: std.mem.Allocator) !void {
    const source =
        \\let set:(p:&mut`T,v:`T)={p.* =v}
        \\let gets:(p:&`T)->`T=p.*
        \\let forward:(p:&mut`T,v:`T)={set(p,v)}
        \\let via:(p:&`T)->`T=gets(p)
        \\let R=struct{n:int=0,let put:(p:&mut`T,v:`T)={set(p,v)}
        \\let get:(self:R,p:&`T)->`T=gets(p)
        \\}
        \\let main:()={
        \\let mut n:u8=1
        \\let p=&n
        \\set(p,2)
        \\forward(p,3)
        \\let a:u8=gets(p)
        \\let fixed:u8=9
        \\let b:u8=via(&fixed)
        \\let c:u8=p |> gets
        \\let d:u8=&fixed |> gets
        \\let setter:(&mut u8,u8)->void=set
        \\setter(p,4)
        \\let read:(&u8)->u8=gets
        \\let local=(q:&mut`T,v:`T){set(q,v)}
        \\local(p,5)
        \\R.put(p,6)
        \\let receiver=R[]
        \\let e:u8=receiver.get(p)
        \\let mut items:[]u8=[0]
        \\set(&items[0],7)
        \\@println(a,b,c,d,e,read(p),gets(&items[0]))
        \\}
    ;
    var module = try Module.init(allocator, "generic-reference-parameters.nov", source);
    defer module.deinit();
    errdefer if (module.hasErrors()) std.debug.print("generic reference source: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    var analysis = try Lowering.lower(&module);
    defer analysis.deinit();
    errdefer if (module.hasErrors()) std.debug.print("generic reference parameters: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(analysis.lowering_complete);
    try analysis.ir.verify();
}

test "reference parameters generic structural inference methods pipes and substitutions under OOM" {
    try testReferenceParameterInference(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testReferenceParameterInference, .{});
}

fn testReferenceParameterInferenceError(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "generic-reference-parameter-errors.nov", source);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    var analysis = try Lowering.lower(&module);
    defer analysis.deinit();
    try std.testing.expect(module.hasErrors());
    try std.testing.expect(!analysis.lowering_complete);
}

test "reference parameters generic qualifiers invariant callbacks and escapes under OOM" {
    for ([_][]const u8{
        "let set:(p:&mut`T,v:`T)={p.* =v}\nlet f:(p:&int)={set(p,1)}",
        "let set:(p:&mut`T,v:`T)={p.* =v}\nlet f:()={let n:int=0\nset(&n,1)}",
        "let take:(f:(&`T)->`T)->`T={loop {}}\nlet write:(p:&mut int)->int=p.*\nlet f:()->int=take(write)",
        "let escape:(p:&`T)->&`T=p\nlet f:()={let n:int=1\nlet result=escape(&n)\n}",
        "let copy:(p:&`T)->`T={let q=p\nq.*}\nlet f:()={let n:int=1\nlet result=copy(&n)\n}",
        "let capture:(p:&`T)->()->`T=()->`T{p.*}\nlet f:()={let n:int=1\nlet result=capture(&n)\n}",
    }) |source| {
        try testReferenceParameterInferenceError(std.testing.allocator, source);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testReferenceParameterInferenceError, .{source});
    }
}

fn testNonStorableInference(allocator: std.mem.Allocator, source: []const u8, tag: Module.Diagnostic.Tag) !void {
    var module = try Module.init(allocator, "non-storable.nov", source);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    const count = module.declarations.items.len;
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(error.OutOfMemory, err);
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!sema.lowering_complete);
    try std.testing.expectEqual(count, module.declarations.items.len);
    var found = false;
    for (module.diagnostics.items) |diagnostic| if (diagnostic.tag == tag) {
        try std.testing.expect(diagnostic.span.end > diagnostic.span.start);
        try std.testing.expect(module.sourceSlice(diagnostic.span) != null);
        found = true;
    };
    if (!found) std.debug.print("non-storable {s}: {any}\n", .{ source, module.diagnostics.items });
    try std.testing.expect(found);
    try sema.ir.verify();
}

test "non-storable array inference diagnoses before publication and rolls back OOM" {
    const map = "let map:(items:[]`T,convert:(`T)->`U)->[]`U=[]\n";
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = map ++ "let main:()={let result=map(@as([]int,[1]),(value:int)->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let make:(f:()->`T)->[]`T=[]\nlet main:()={let result=make(()->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let make:(f:()->`T)->[][]`T=[]\nlet main:()={let result=make(()->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let make:(f:()->`T)->`U=[f()]\nlet main:()={let result=make(()->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let make:(f:()->`T)->`U=[[f()]]\nlet main:()={let result=make(()->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let make:(f:()->`T)->`U=(value:`T)->void{}\nlet main:()={let result=make(()->void{})\n}", .tag = .invalid_parameter_type },
        .{ .source = map ++ "let main:()={let result=map(@as([]int,[1]),(value:int)->type{int})\n}", .tag = .unsupported_type },
        .{ .source = "let main:()={let result:[]void=[]\n}", .tag = .unsupported_type },
    }) |case| {
        try testNonStorableInference(std.testing.allocator, case.source, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testNonStorableInference, .{ case.source, case.tag });
    }
}

fn testNonStorableProbe(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "probe-array.nov",
        \\let make:(f:()->`T)->[][]`T=[]
    );
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const unit = try ir.internType(.void);
    const callback = try ir.internType(.{ .function = .{ .params = &.{}, .result = unit } });
    const id = module.lookup("make").?;
    const ref: Ref = .{ .module = &module, .id = id };
    const count = module.declarations.items.len;
    for (0..2) |_| {
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try probeResult(&module, &ir, ref, &.{.{ .ty = callback, .span = ref.span() }}, ref.span()));
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
        try std.testing.expectEqual(@as(usize, 0), ir.local_generic_instances.items.len);
        const proto = module.source.ast.declFunction(module.getDeclaration(id).?.node).?.proto_node;
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try TypeResolver.resolveInScopeReadOnly(&module, &ir, proto, null, null, &.{.{ .name = "`T", .ty = unit }}));
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_type, module.diagnostics.items[0].tag);
        module.diagnostics.clearRetainingCapacity();
        try ir.verify();
    }
}

test "non-storable array readonly probes do not publish under OOM" {
    try testNonStorableProbe(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testNonStorableProbe, .{});
}

fn testUnionIndexInference(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "union-index.nov",
        \\let Option:(T:type)->type=union{some:T,none,}
        \\let Color=enum{red,blue,}
        \\let Box=struct{value:uint,}
        \\let findIndex:(items:[]`T,predicate:(`T)->bool)->Option(uint)={
        \\let mut index:uint=0
        \\loop item in items{if predicate(item){return .some[index]}
        \\index+=1}
        \\.none[]}
        \\let find:(items:[]`T)->Option(`T)={loop item in items{return .some[item]}
        \\.none[]}
        \\let qualified:(x:`T)->Option(`T)=Option(`T).some[x]
        \\let color:(x:`T)->Option(Color)=.some[.red]
        \\let boxed:(x:`T)->Option(Box)=.some[Box[.value=7]]
        \\let first:(items:[]`T,index:int)->`T=items[index]
        \\let main:()={
        \\let found:Option(uint)=findIndex(@as([]int,[1,2]),(x:int)->bool{x==2})
        \\let value:Option(uint)=find(@as([]uint,[7]))
        \\let q:Option(uint)=qualified(@as(uint,7))
        \\let c:Option(Color)=color(true)
        \\let b:Option(Box)=boxed(true)
        \\let n:uint=first(@as([]uint,[7]),0)
        \\}
    );
    defer module.deinit();
    const count = module.declarations.items.len;
    if (module.hasErrors()) std.debug.print("union index parse: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    var sema = Lowering.lower(&module) catch |err| {
        try std.testing.expectEqual(count, module.declarations.items.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("union index: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "union constructors preserve uint payloads independently of ordinary int indexing under OOM" {
    try testUnionIndexInference(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testUnionIndexInference, .{});
    const source = "let at:(items:[]`T,index:uint)->`T=items[index]\nlet main:()->bool=at([true],0)";
    try testInferenceFailureAllocations(std.testing.allocator, source, .generic_function_conflict);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferenceFailureAllocations, .{ source, Module.Diagnostic.Tag.generic_function_conflict });
}

test "real std array findIndex preserves uint constructor payloads under OOM" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data =
        \\let std=@import("std")
        \\let main:()={
        \\let found:std.Option(uint)=std.array.findIndex(@as([]int,[1,2]),(x:int)->bool{x==2})
        \\let missing:std.Option(uint)=std.array.findIndex(@as([]uint,[]),(x:uint)->bool{true})
        \\let value:std.Option(uint)=std.array.find(@as([]uint,[7]),(x:uint)->bool{true})
        \\@println(found,missing,value)
        \\}
    });
    const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
    defer allocator.free(path);
    const lib = try std.Io.Dir.cwd().realPathFileAlloc(io, "lib", allocator);
    defer allocator.free(lib);
    try testNonStorableImport(allocator, path, lib, null);
    try TestAllocations.checkAllAllocationFailures(allocator, testNonStorableImport, .{ path, lib, @as(?Module.Diagnostic.Tag, null) });
}

fn expectNoImportSpecializations(comp: *const Compilation, count: usize) !void {
    try std.testing.expectEqual(count, comp.declarations.items.len);
    try std.testing.expectEqual(count, comp.declaration_ids.count());
    for (comp.declarations.items, 0..) |ref, i| {
        try std.testing.expectEqual(@as(Compilation.DeclId, @fromBackingInt(@intCast(i))), comp.declaration_ids.get(ref).?);
    }
    for (comp.modules.items) |module| {
        for (module.declarations.items) |decl| try std.testing.expect(decl.function_template == null and decl.concrete_type == null);
    }
}

fn testNonStorableImport(allocator: std.mem.Allocator, path: []const u8, lib: []const u8, tag: ?Module.Diagnostic.Tag) !void {
    var comp = try Compilation.load(allocator, std.testing.io, path, .{ .lib_dir = lib });
    defer comp.deinit();
    try std.testing.expect(!comp.hasErrors());
    const count = comp.declarations.items.len;
    var sema = Lowering.lowerCompilation(&comp) catch |err| {
        try std.testing.expectEqual(error.OutOfMemory, err);
        try expectNoImportSpecializations(&comp, count);
        for (comp.modules.items) |module| try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    if (tag) |expected| {
        try std.testing.expect(!sema.lowering_complete);
        try expectNoImportSpecializations(&comp, count);
        var found = false;
        for (comp.modules.items) |module| for (module.diagnostics.items) |diagnostic| {
            if (diagnostic.tag != expected) continue;
            const owner = comp.getModule(diagnostic.span.source).?;
            try std.testing.expect(diagnostic.span.end > diagnostic.span.start);
            try std.testing.expect(owner.sourceSlice(diagnostic.span) != null);
            found = true;
        };
        if (!found) for (comp.modules.items) |module| std.debug.print("array import {s}: {any}\n", .{ module.source.path, module.diagnostics.items });
        try std.testing.expect(found);
    } else {
        if (comp.hasErrors()) for (comp.modules.items) |module| std.debug.print("array import {s}: {any}\n", .{ module.source.path, module.diagnostics.items });
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(sema.lowering_complete);
    }
    try sema.ir.verify();
}

test "union array payloads real std map andThen find and readonly calls under OOM" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data =
        \\let std=@import("std")
        \\let O=std.Option
        \\let R=std.Result
        \\let expand:(x:int)->[]int=[x,x+1]
        \\let next:(x:int)->O([]int)=.some[[x]]
        \\let nextResult:(x:int)->R([]int,[]string)=.ok[[x]]
        \\let original:()->O(int)=.some[1]
        \\let inferred:(x:@TypeOf(original().map(expand)))->O([]int)=x
        \\let main:()={
        \\let option=original().map(expand)
        \\let chained=original().andThen(next)
        \\let result:R(int,[]string)=.ok[1]
        \\let mapped=result.map(expand)
        \\let bound=result.andThen(nextResult)
        \\let remapped=R(int,string).err["bad"].mapErr((s:string)->[]string{[s]})
        \\let found=std.array.find(@as([][]int,[[1],[2]]),(xs:[]int)->bool{xs[0]==2})
        \\@println(option,chained,mapped,bound,remapped,found,inferred(option))
        \\}
    });
    const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
    defer allocator.free(path);
    const lib = try std.Io.Dir.cwd().realPathFileAlloc(io, "lib", allocator);
    defer allocator.free(lib);
    try testNonStorableImport(allocator, path, lib, null);
    try TestAllocations.checkAllAllocationFailures(allocator, testNonStorableImport, .{ path, lib, @as(?Module.Diagnostic.Tag, null) });
}

test "value payloads std map andThen find function elements and readonly under OOM" {
    try testNestedUnionImport(
        \\let std=@import("std")
        \\let O=std.Option
        \\let F=(int)->int
        \\let id:(x:`T)->`T=x
        \\let make:(seed:int)->F=(n){seed+n}
        \\let next:(seed:int)->O(F)=.some[make(seed)]
        \\let original:()->O(int)=.some[1]
        \\let quoted:(x:@TypeOf(original().map(make)))->O(F)=x
        \\let use:(o:O(F))->O(int)={let f=o.!
        \\.some[f(2)]}
        \\let main:()={let a=original().map(make)
        \\let b=original().andThen(next)
        \\let fs:[]F=[id,make(3)]
        \\let found=std.array.find(fs,(f:F)->bool{f(0)==3})
        \\@println(use(quoted(a)),use(b),use(found))}
    );
}

test "value payloads std range map andThen find and readonly under OOM" {
    try testNestedUnionImport(
        \\let std=@import("std")
        \\let O=std.Option
        \\let Range=@TypeOf(0..1)
        \\let make:(n:uint)->Range=0..=n
        \\let next:(n:uint)->O(Range)=.some[0..=n]
        \\let inferred:(n:`T)->O(@TypeOf(0..1))=.some[0..=n]
        \\let original:()->O(uint)=.some[3]
        \\let quoted:(x:@TypeOf(original().andThen(next)))->O(Range)=x
        \\let use:(o:O(Range))->O(uint)={let r=o.!
        \\let mut n:uint=0
        \\loop v in r {n+=v}
        \\.some[n]}
        \\let main:()={let a=original().map(make)
        \\let b=original().andThen(next)
        \\let c=original().andThen(inferred)
        \\let mapped=original().map((n){O(Range).some[0..=n]})
        \\let rs:[]Range=[0..1,0..=2]
        \\let found=std.array.find(rs,(r:Range)->bool{loop _ in r {return true}
        \\false})
        \\@println(use(a),use(quoted(b)),use(c),use(found))}
    );
}

test "nested unions std Option map union outputs and readonly under OOM" {
    try testNestedUnionImport(
        \\let std=@import("std")
        \\let O=std.Option
        \\let R=std.Result
        \\let convert:(x:int)->R(int,string)=.ok[x]
        \\let make:()->O(int)=.some[1]
        \\let quoted:(x:@TypeOf(make().map(convert)))->O(R(int,string))=x
        \\let main:()={@println(quoted(make().map(convert)))}
    );
}

test "nested unions std Result map union outputs under OOM" {
    try testNestedUnionImport(
        \\let std=@import("std")
        \\let O=std.Option
        \\let R=std.Result
        \\let convert:(x:int)->O(int)=.some[x]
        \\let main:()={let x:R(int,string)=.ok[1]
        \\let nested:R(O(int),string)=x.map(convert)
        \\@println(nested)}
    );
}

test "nested unions std Option andThen union outputs under OOM" {
    try testNestedUnionImport(
        \\let std=@import("std")
        \\let O=std.Option
        \\let R=std.Result
        \\let next:(x:int)->O(R(int,string))=.some[.ok[x]]
        \\let make:()->O(int)=.some[1]
        \\let quoted:(x:@TypeOf(make().andThen(next)))->O(R(int,string))=x
        \\let main:()={let nested:O(R(int,string))=make().andThen(next)
        \\@println(quoted(nested))}
    );
}

test "nested unions std Result andThen union outputs under OOM" {
    try testNestedUnionImport(
        \\let std=@import("std")
        \\let O=std.Option
        \\let R=std.Result
        \\let next:(x:int)->R(O(int),string)=.ok[.some[x]]
        \\let make:()->R(int,string)=.ok[1]
        \\let quoted:(x:@TypeOf(make().andThen(next)))->R(O(int),string)=x
        \\let main:()={let nested:R(O(int),string)=make().andThen(next)
        \\@println(quoted(nested))}
    );
}

test "nested unions std Result mapErr union outputs and readonly under OOM" {
    try testNestedUnionImport(
        \\let std=@import("std")
        \\let O=std.Option
        \\let R=std.Result
        \\let convert:(s:string)->O(string)=.some[s]
        \\let make:()->R(int,string)=.err["bad"]
        \\let quoted:(x:@TypeOf(make().mapErr(convert)))->R(int,O(string))=x
        \\let main:()={@println(quoted(make().mapErr(convert)))}
    );
}

fn testNestedUnionImport(source: []const u8) !void {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data = source });
    const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
    defer allocator.free(path);
    const lib = try std.Io.Dir.cwd().realPathFileAlloc(io, "lib", allocator);
    defer allocator.free(lib);
    try testNonStorableImport(allocator, path, lib, null);
    try TestAllocations.checkAllAllocationFailures(allocator, testNonStorableImport, .{ path, lib, @as(?Module.Diagnostic.Tag, null) });
}

test "failed lowering rolls back imported signature methods and earlier generic demands under OOM" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "producer.nov", .data =
        \\@[public]
        \\let Box:(T:type)->type=struct{
        \\ value:T,
        \\ let get:(self:Self)->T=self.value
        \\}
        \\@[public]
        \\let ordinary:(value:Box(int))->int=value.get()
        \\@[public]
        \\let id:(value:`T)->`T=value
        \\@[public]
        \\let make:(f:()->`T)->[]`T=[]
    });
    const lib = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(lib);
    for ([_][]const u8{
        // Annotation failure after an ordinary imported signature publishes Box methods.
        "let p=@import(\"producer.nov\")\nlet invalid:(x:[]void)={}\nlet main:()={}\n",
        // Body failure after both signature publication and a successful generic call.
        "let p=@import(\"producer.nov\")\nlet main:()={let good=p.id(1)\nlet bad=p.make(()->void{})\n}\n",
    }) |source| {
        try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data = source });
        const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
        defer allocator.free(path);
        try testNonStorableImport(allocator, path, lib, .unsupported_type);
        try TestAllocations.checkAllAllocationFailures(allocator, testNonStorableImport, .{ path, lib, @as(?Module.Diagnostic.Tag, .unsupported_type) });
    }
}

test "non-storable array real imports diagnose and preserve void returning function elements" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "producer.nov", .data =
        \\@[public]
        \\let make:(f:()->`T)->[]`T=[]
        \\@[public]
        \\let nested:(f:()->`T)->[][]`T=[]
        \\@[public]
        \\let inferred:(f:()->`T)->`U=[f()]
        \\@[public]
        \\let callback:(f:()->`T)->`U=(value:`T)->void{}
    });
    const lib = try std.Io.Dir.cwd().realPathFileAlloc(io, "lib", allocator);
    defer allocator.free(lib);
    for ([_]struct { source: []const u8, tag: ?Module.Diagnostic.Tag }{
        .{ .source = "let std=@import(\"std\")\nlet main:()={let result=std.array.map(@as([]int,[1]),(value:int)->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let p=@import(\"producer.nov\")\nlet main:()={let result=p.make(()->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let p=@import(\"producer.nov\")\nlet main:()={let result=p.nested(()->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let p=@import(\"producer.nov\")\nlet main:()={let result=p.inferred(()->void{})\n}", .tag = .unsupported_type },
        .{ .source = "let p=@import(\"producer.nov\")\nlet main:()={let result=p.callback(()->void{})\n}", .tag = .invalid_parameter_type },
        .{ .source = "let std=@import(\"std\")\nlet main:()={let result=std.array.map(@as([]int,[1]),(value:int)->type{int})\n}", .tag = .unsupported_type },
        .{ .source =
        \\let std=@import("std")
        \\let main:()={
        \\let numbers=std.array.map(@as([]int,[1]),(value:int)->int{value+1})
        \\let callbacks=std.array.map(numbers,(value:int)->()->void{()->void{}})
        \\let recovered=std.array.map(callbacks,(f:()->void)->int{f()
        \\1})
        \\@println(recovered[0])
        \\}
        , .tag = null },
    }) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
        defer allocator.free(path);
        try testNonStorableImport(allocator, path, lib, case.tag);
        if (case.tag != null) try TestAllocations.checkAllAllocationFailures(allocator, testNonStorableImport, .{ path, lib, case.tag });
    }
}

test "backtick generic function rejects ambiguous conflicting unbound and unsupported uses" {
    const prefix = "let add:(x:`a,y:`b)->`c=x+y\nlet id:(x:`T)->`T=x\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let main:()={let x=add([],[])\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={let x=add(@as(uint,1),@as(int,2))\n}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={let x=add(\"s\",@as(uint,2))\n}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={let f=id\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={let f:(uint)->int=id\n}", .tag = .generic_function_conflict },
        .{ .source = "let main:()={let f:(uint,uint)->uint=id\n}", .tag = .argument_count_mismatch },
        .{ .source = "let main:()={let f:uint=id\n}", .tag = .generic_function_context },
        .{ .source = "let main:()={let f=[id,id]\n}", .tag = .array_literal_needs_type },
        .{ .source = "let main:()={let f=if true {id} else {id}\n}", .tag = .generic_function_ambiguous },
        .{ .source = "let apply:(f:(`T)->`U,x:`T)->`U=f(x)\nlet main:()={let x=apply(id,[])\n}", .tag = .generic_function_ambiguous },
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
        .{ .source = "let f:(x:`T)->`U=[]\nlet main:()={let a=f(true)\n}", .tag = .generic_function_ambiguous },
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
        .{ .source = "let main:()={let f:(x:`T)->`T=x\nf([])\n@print()}", .tag = .generic_function_ambiguous },
        .{ .source = "let main:()={let f:(x:`T)->`T=unknown\n@print(f(true))}", .tag = .unknown_name },
        .{ .source = "let main:()={let f:(x:`T)->`T=f(x)\n@print(f(true))}", .tag = .unknown_name },
        .{ .source = "let main:()={let x:int=1\nlet f=(x:`T)->`T{x}\nf(true)}", .tag = .unsupported_shadowing },
        .{ .source = "let main:()={let f=(x:`T)->`T{{let hidden:int=1\n}\nx}\n@print(f(true))\n@print(hidden)}", .tag = .unknown_name },
        .{ .source = "let main:()={let mut n:int=1\nlet loan=&n\nlet f=(x:`T)->`T{@print(loan.*)\nx}\nf(true)}", .tag = .unsupported_borrow },
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
        "let bad:(x:`T)->`T=missing\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(bad(true),[])\n}",
        "let bad:(x:`T)->`T=missing\nlet first:(x:(bool)->bool,y:`B)->bool=x(true)\nlet main:()={let a=first(bad,[])\n}",
        "let id:(x:`T)->`T=x\nlet apply:(f:(`T)->`U,x:`T)->`U=f(x)\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(apply(id,true),[])\n}",
        "let id:(x:`T)->`T=x\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(@as((bool)->bool,id),[])\n}",
        "let G:(T:type)->type=struct{let id:(x:`U,y:T)->`U=x\n}\nlet first:(x:(bool,uint)->bool,y:`B)->bool=x(true,1)\nlet main:()={let a=first(G(uint).id,[])\n}",
        "let G:(T:type)->type=struct{let id:(x:`U)->`U=x\n}\nlet wrap:(x:`T)->`U=G(uint).id(x)\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(wrap(\"s\"),[])\n}",
        "let G:(T:type)->type=struct{let id:(x:`U)->`U=x\n}\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(G(uint).id(\"s\"),[])\n}",
        "let Box:(T:type)->type=struct{value:T,let id:(x:`U)->`U=x\n}\nlet id:(x:`T)->`T=x\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(id(Box(uint)[.value=1]),[])\n}",
        "let Box:(T:type)->type=struct{value:T,let id:(x:`U)->`U=x\n}\nlet make:(x:`T)->Box(`T)=Box(`T)[.value=x]\nlet get:(x:Box(`T))->`T=x.value\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(get(make(true)),[])\n}",
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
    try source.writer.writeAll("[])\n}");
    try testSpeculativeFailure(std.testing.allocator, source.written());
}

fn testImportedSpeculativeFailure(allocator: std.mem.Allocator, invalid_layout: bool) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/probe.nov", "let lib=@import(\"lib.nov\")\nlet first:(x:`A,y:`B)->`A=x\nlet main:()={let a=first(lib.bad(true),[])\n}");
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
    try source.writer.writeAll("[])\n}");
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
        .{ .tail = "let main:()=R[].f(@as(int,1.5),@panic(\"stop\"),true)", .tag = .literal_type_mismatch },
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
        "let Box:(T:type)->type=struct{value:T,let map:(object:Box(T),f:(T)->`U)->Box(`U)=Box(`U)[.value=f(object.value)]\n}\nlet make:(x:`T)->Box(`T)=Box(`T)[.value=x]\nlet id:(x:`T)->`T=x\nlet first:(a:`A,b:`B)->`A=a\nlet main:()={let x=first(make(true).map(id),[])\n}",
        "let Box:(T:type)->type=struct{value:T,let get:(object:Box(T))->T=object.value\n}\nlet make:(x:`T)->Box(`T)=Box(`T)[.value=x]\nlet first:(a:`A,b:`B)->`A=a\nlet main:()={let x=first(make(true).get(),[])\n}",
    }) |source| {
        try testSpeculativeFailure(std.testing.allocator, source);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testSpeculativeFailure, .{source});
    }
}

test "backtick instance conflicts unbound variables unsupported values arity and noreturn diagnose under OOM" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let R=struct{let f:(object:R,x:`T,y:`T)->`T=x\n}\nlet main:()={let x=R[].f(true,\"s\")\n}", .tag = .generic_function_conflict },
        .{ .source = "let R=struct{let f:(object:R)->`T=[]\n}\nlet main:()={let x=R[].f()\n}", .tag = .generic_function_ambiguous },
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

fn testInferredClosureSignatures(allocator: std.mem.Allocator) !void {
    const source =
        \\let apply:(f:(`T)->`U,x:`T)->`U=f(x)
        \\let main:()={
        \\let identity=(x){x}
        \\let length=(x){x.len}
        \\let load=(x){x.*}
        \\let number=(){1}
        \\let captured=(){saved}
        \\let nested=(x){let read=(y){y.len}
        \\read(x)}
        \\let separate=(x,y){y}
        \\let unused=(x){true}
        \\let calculated=(x){x+1}
        \\let unit=(){@print(1)
        \\}
        \\}
        \\let get:(f:()->`T)->`T=f()
    ;
    var module = try Module.init(allocator, "inferred-closures.nov", source);
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(source.len);
    const ast = &module.source.ast;
    var buffer: [2]Ast.Node.Index = undefined;
    const statements = ast.fullBlock(&buffer, ast.declFunction(ast.rootDecls()[1]).?.body).?.ast.statements;
    const boolean = try ir.internType(.boolean);
    const string = try ir.internType(.string);
    const uint = try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } });
    const reference = try ir.internType(.{ .reference = .{ .child = boolean, .mutable = false } });
    const integer = try BodyChecker.defaultIntegerType(&ir);
    const float = try BodyChecker.defaultFloatType(&ir);
    for (statements, 0..) |statement, index| {
        const expression = ast.functionExpr(ast.decl(statement).ast.init_node).?;
        const local: LocalTemplate = .{ .proto = expression.proto.ast.proto_node, .body = expression.body, .captures = &.{.{ .name = "saved", .ty = boolean }} };
        const ref: Ref = .{ .module = &module, .local = &local };
        const params: []const Ir.TypeId = switch (index) {
            0, 7 => &.{boolean},
            1, 5 => &.{string},
            2 => &.{reference},
            6 => &.{ string, boolean },
            8 => &.{float},
            else => &.{},
        };
        const result = switch (index) {
            1, 5 => uint,
            3, 8 => float,
            9 => try ir.internType(.void),
            else => boolean,
        };
        const expected = try ir.internType(.{ .function = .{ .params = params, .result = result } });
        try std.testing.expectEqual(expected, (try inferClosure(&module, &ir, ref, expected, ref.span())).?);
        if (index == 3) {
            const inferred = (try inferClosure(&module, &ir, ref, null, ref.span())).?;
            try std.testing.expectEqual(integer, ir.getType(inferred).?.function.result);
        }
        if (index == 8) {
            const inferred = (try inferClosure(&module, &ir, ref, null, ref.span())).?;
            const function = ir.getType(inferred).?.function;
            try std.testing.expectEqual(integer, function.params[0]);
            try std.testing.expectEqual(integer, function.result);
        }
        if (index == 0 or index == 7) {
            try std.testing.expect(try inferClosure(&module, &ir, ref, null, ref.span()) == null);
            try std.testing.expectEqual(Module.Diagnostic.Tag.generic_function_ambiguous, module.diagnostics.items[0].tag);
            module.diagnostics.clearRetainingCapacity();
        }
        if (index == 0) {
            const wrong = try ir.internType(.{ .function = .{ .params = &.{boolean}, .result = string } });
            try std.testing.expect(try inferClosure(&module, &ir, ref, wrong, ref.span()) == null);
            try std.testing.expectEqual(Module.Diagnostic.Tag.generic_function_conflict, module.diagnostics.items[0].tag);
            module.diagnostics.clearRetainingCapacity();
        }
        if (index == 3) {
            const args = [_]Argument{.{ .ty = null, .span = ref.span(), .template = ref }};
            // No-argument callback results remain soft in readonly probes.
            const get: Ref = .{ .module = &module, .id = module.lookup("get").? };
            try std.testing.expect(try probeResult(&module, &ir, get, &args, ref.span()) == null);
            try std.testing.expectEqual(integer, (try probeDefaultResult(&module, &ir, get, &args, ref.span())).?);
            try std.testing.expect(try probeResult(&module, &ir, get, &args, ref.span()) == null);
        }
        if (index == 1 or index == 2) {
            const apply: Ref = .{ .module = &module, .id = module.lookup("apply").? };
            const args = [_]Argument{
                .{ .ty = null, .span = ref.span(), .template = ref },
                .{ .ty = params[0], .span = ref.span() },
            };
            // The readonly probe must constrain the callback from the later
            // argument, but must not apply numeric fallback candidates.
            try std.testing.expectEqual(result, (try probeResult(&module, &ir, apply, &args, apply.span())).?);
            const bindings = (try infer(&module, &ir, apply, &args, null, apply.span())).?;
            defer allocator.free(bindings);
            try std.testing.expectEqual(params[0], bindings[0].ty);
            try std.testing.expectEqual(result, bindings[1].ty);
        }
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
        try std.testing.expectEqual(@as(usize, 0), ir.local_generic_instances.items.len);
    }
}

const InferredReviewResult = enum { integer, uint, unit, nominal, ambiguous, invalid_parameter, established };

fn testInferredClosureReview(allocator: std.mem.Allocator, source: []const u8, result: InferredReviewResult) !void {
    var module = try Module.init(allocator, "inferred-review.nov", source);
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(source.len);
    const declarations = module.declarations.items.len;
    // These invariants also hold when an injected allocation failure abandons
    // materialization after an earlier nominal layout was interned.
    defer std.debug.assert(module.declarations.items.len == declarations);
    defer std.debug.assert(ir.functions.items.len == 0);
    defer std.debug.assert(ir.local_generic_instances.items.len == 0);
    const types = try allocator.alloc(?Ir.TypeId, declarations);
    defer allocator.free(types);
    @memset(types, null);
    if (result == .established) types[@backingInt(module.lookup("g").?)] = try ir.internType(.{ .function = .{ .params = &.{}, .result = try BodyChecker.defaultIntegerType(&ir) } });
    const ref: Ref = .{ .module = &module, .id = module.lookup("f").?, .types = types };
    const inferred = try inferClosure(&module, &ir, ref, null, ref.span());
    if (result == .ambiguous or result == .invalid_parameter) {
        try std.testing.expect(inferred == null);
        try std.testing.expectEqual(if (result == .ambiguous) Module.Diagnostic.Tag.generic_function_ambiguous else Module.Diagnostic.Tag.invalid_parameter_type, module.diagnostics.items[0].tag);
        return;
    }
    try std.testing.expect(inferred != null);
    const function = ir.getType(inferred.?).?.function;
    switch (result) {
        .integer, .established => try std.testing.expectEqual(try BodyChecker.defaultIntegerType(&ir), function.result),
        .uint => try std.testing.expectEqual(try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } }), function.result),
        .unit => try std.testing.expectEqual(try ir.internType(.void), function.result),
        .nominal => {
            try std.testing.expectEqual(function.params[0], function.result);
            try std.testing.expect(ir.getType(function.result).? == .record);
        },
        .ambiguous, .invalid_parameter => unreachable,
    }
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
}

test "inferred closures review readonly layouts forward calls deferred projections and terminal void under OOM" {
    for ([_]struct { source: []const u8, result: InferredReviewResult }{
        .{ .source = "let Box:(T:type)->type=struct{value:T,let method:(x:T)->T=x\n}\nlet f=(x:Box(int)){x}", .result = .nominal },
        .{ .source = "let g=(x:int){x+1}\nlet f=(x:int){return g(x)}", .result = .integer },
        .{ .source = "let f=(x:int){return g(x)}\nlet g=(x:int){x+1}", .result = .integer },
        .{ .source = "let g=(){}\nlet f=(){return g()}", .result = .unit },
        .{ .source = "let f=(){return g()}\nlet g=(){}", .result = .unit },
        .{ .source = "let both:(f:(`T)->`U,g:()->`T)->`U=f(g())\nlet f=(){both((x){x.len},(){\"abc\"})}", .result = .uint },
        .{ .source = "let both:(g:()->`T,f:(`T)->`U)->`U=f(g())\nlet f=(){both((){\"abc\"},(x){x.len})}", .result = .uint },
        .{ .source = "@[noreturn]\nlet f=(){@panic(\"bad\")}", .result = .unit },
        .{ .source = "let f=(){loop{}}", .result = .unit },
        .{ .source = "let f=(){return f()}", .result = .ambiguous },
        .{ .source = "let f=(){return g()}\nlet g=(){return f()}", .result = .ambiguous },
        .{ .source = "let f=(x:void){x}", .result = .invalid_parameter },
        .{ .source = "let f=(){return g()}\nlet g=(){missing}", .result = .established },
    }) |case| {
        try testInferredClosureReview(std.testing.allocator, case.source, case.result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferredClosureReview, .{ case.source, case.result });
    }
}

fn testInferredClosureMutation(allocator: std.mem.Allocator, source: []const u8, unit_result: bool) !void {
    var module = try Module.init(allocator, "inferred-mutation.nov", source);
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(source.len);
    const ast = &module.source.ast;
    var buffer: [2]Ast.Node.Index = undefined;
    const statements = ast.fullBlock(&buffer, ast.declFunction(ast.rootDecls()[0]).?.body).?.ast.statements;
    const expression = ast.functionExpr(ast.decl(statements[1]).ast.init_node).?;
    const integer = try BodyChecker.defaultIntegerType(&ir);
    const local: LocalTemplate = .{ .proto = expression.proto.ast.proto_node, .body = expression.body, .captures = &.{.{ .name = "n", .ty = integer, .mutable = true }} };
    const ref: Ref = .{ .module = &module, .local = &local };
    const ty = (try inferClosure(&module, &ir, ref, null, ref.span())).?;
    try std.testing.expectEqual(if (unit_result) try ir.internType(.void) else integer, ir.getType(ty).?.function.result);
    for (ir.getType(ty).?.function.params) |param| try std.testing.expectEqual(integer, param);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
}

test "inferred closures infer mutable capture assignments without callback context under OOM" {
    for ([_]struct { source: []const u8, unit_result: bool = false }{
        .{ .source = "let main:()={let mut n:int=0\nlet next=(){n+=1\nn}\n@println(next())\n}" },
        .{ .source = "let main:()={let mut n:int=0\nlet next=(x:int){n+=x\nn}\n@println(next(1))\n}" },
        .{ .source = "let main:()={let mut n:int=0\nlet next=(x){n+=x\nn}\n@println(next(1))\n}" },
        .{ .source = "let main:()={let mut n:int=0\nlet next=(){n+=1}\nnext()\n}", .unit_result = true },
        .{ .source = "let main:()={let mut n:int=0\nlet next=(x){n=x}\nnext(1)\n}", .unit_result = true },
    }) |case| {
        try testInferredClosureMutation(std.testing.allocator, case.source, case.unit_result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferredClosureMutation, .{ case.source, case.unit_result });
    }
}

test "inferred closures concrete signatures callback-first references defaults captures and OOM" {
    try testInferredClosureSignatures(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInferredClosureSignatures, .{});
}

fn testZipInference(allocator: std.mem.Allocator, source: []const u8, result_type: Ir.Type) !void {
    var module = try Module.init(allocator, "zip-inference.nov", source);
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(source.len);
    const declarations = module.declarations.items.len;
    defer std.debug.assert(module.declarations.items.len == declarations);
    defer std.debug.assert(ir.functions.items.len == 0);
    defer std.debug.assert(ir.local_generic_instances.items.len == 0);
    if (module.source.ast.errors.len != 0) std.debug.print("zip parse: {s}\n{any}\n", .{ source, module.source.ast.errors });
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    const ref: Ref = .{ .module = &module, .id = module.lookup("f").? };
    const inferred = (try inferClosure(&module, &ir, ref, null, ref.span())).?;
    try std.testing.expectEqual(try ir.internType(result_type), ir.getType(inferred).?.function.result);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    const apply: Ref = .{ .module = &module, .id = module.lookup("apply").? };
    const args = [_]Argument{.{ .ty = null, .span = ref.span(), .template = ref }};
    const diagnostics = module.diagnostics.items.len;
    _ = try probeResult(&module, &ir, apply, &args, apply.span());
    try std.testing.expectEqual(try ir.internType(result_type), (try probeDefaultResult(&module, &ir, apply, &args, apply.span())).?);
    try std.testing.expectEqual(diagnostics, module.diagnostics.items.len);
}

test "zip solver inferred closure returns sources references indices and readonly OOM" {
    const uint: Ir.Type = .{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } };
    const int: Ir.Type = .{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } };
    for ([_]struct { source: []const u8, result: Ir.Type }{
        .{ .source = "let f=(){loop a,b in [true],[false] {return b}\ntrue}", .result = .boolean },
        .{ .source = "let f=(){loop a,b,i in \"é\",\"😀\",0.. {break b} else {'x'}}", .result = .rune },
        .{ .source = "let f=(){loop a,i in [true],0.. {break i} else {0}}", .result = uint },
        .{ .source = "let f=(){loop i,a in -1..,[true] {break i} else {0}}", .result = int },
        .{ .source = "let f=(){let r=0..=2\nloop a,i in [true],r {break i} else {0}}", .result = uint },
        .{ .source = "let f=(){let mut a=[true]\nlet b=[false]\nloop &x,&y,i in a,b,0.. {let write:(p:&mut bool)={}\nwrite(x)\nlet read:(p:&bool)->bool=p.*\nbreak read(y)} else {true}}", .result = .boolean },
        .{ .source = "let f=(){loop {loop a,b in [true],[false] {} else {break true}}}", .result = .boolean },
        .{ .source = "let f=(){loop a,b in {return true\n[false]},missing {let bad:bool=1\n}\nfalse}", .result = .boolean },
        .{ .source = "let f=(){loop a,b in [true],[false] {return true} else {return false}\nmissing}", .result = .boolean },
        .{ .source = "let f=(){let a=true\nloop a,b in [false],[true] {} else {a}}", .result = .boolean },
        .{ .source = "let f=(){let source=(){\"é\"}\nloop a,b in source(),[true] {break a} else {'x'}}", .result = .rune },
        .{ .source = "let f=(){loop {loop a,b in {break true},missing {}}}", .result = .boolean },
        .{ .source = "let f=(){loop a in \"é\" {break a} else {'x'}}", .result = .rune },
        .{ .source = "let f=(){loop a,b in 0..2,0..=1 {break a} else {0}}", .result = uint },
        .{ .source = "let f=(){let start:u8=1\nloop a,i in [true],start.. {break i} else {0}}", .result = .{ .integer = .{ .signedness = .unsigned, .bits = 8 } } },
        .{ .source = "let both:(f:(`T)->`U,g:()->`T)->`U=f(g())\nlet f=(){both((s){loop r,b in s,[true] {break r} else {'x'}},(){\"é\"})}", .result = .rune },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let apply:(f:()->`T)->`T=f()\n{s}", .{case.source});
        defer std.testing.allocator.free(source);
        try testZipInference(std.testing.allocator, source, case.result);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testZipInference, .{ source, case.result });
    }
}

fn testZipGenericArrays(allocator: std.mem.Allocator) !void {
    const source =
        \\let first:(a:[]`T,b:[]`U,f:(`T,`U)->`V,fallback:`V)->`V=loop x,y,i in a,b,0.. {break f(x,y)} else {fallback}
        \\let main:()={let result=first([true],["é"],(x,y){loop r,i in y,0.. {return x}
        \\false},false)
        \\let exact:bool=result
        \\}
    ;
    var module = try Module.init(allocator, "zip-generic.nov", source);
    defer module.deinit();
    var sema = try Lowering.lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "zip solver generic arrays and callback body results under OOM" {
    try testZipGenericArrays(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testZipGenericArrays, .{});
}

test "backtick contextual imported public aliases retain private template provenance and visibility under OOM" {
    for ([_][]const u8{ "lib.hidden", "lib.NS.hidden", "lib.G(uint).hidden", "lib.alias", "lib.NS.alias", "lib.G(uint).exposed" }, 0..) |expression, i| {
        try testContextualVisibility(std.testing.allocator, expression, i >= 3);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testContextualVisibility, .{ expression, i >= 3 });
    }
}
