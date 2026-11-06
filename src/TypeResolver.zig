const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const Compilation = @import("Compilation.zig");
const primitives = @import("primitives.zig");
const Literal = @import("Literal.zig");
const GenericFunctions = @import("GenericFunctions.zig");
const BodyChecker = @import("BodyChecker.zig");

pub const Error = Ir.InternError;

pub fn hasInferredSignature(ast: *const Ast, node: Ast.Node.Index) bool {
    const proto = ast.fullFnProto(node) orelse return false;
    if (proto.ast.infer_result) return true;
    var iterator = proto.iterate(ast);
    while (iterator.next()) |param| if (param.inferred) return true;
    return false;
}

/// Resolve expression signatures from their bodies; declaration annotations
/// without an arrow retain their explicit void result.
pub fn resolveFunction(module: *Module, ir: *Ir, ref: GenericFunctions.Ref) Error!?Ir.TypeId {
    const function = ref.function();
    const declaration = module.getDeclaration(ref.id).?;
    if (!hasInferredSignature(&module.source.ast, function.proto_node)) return resolveInScope(module, ir, function.proto_node, declaration.container, declaration.concrete_type, declaration.function_bindings);
    const descriptor: GenericFunctions.LocalTemplate = .{ .proto = function.proto_node, .body = function.body, .outer_bindings = declaration.function_bindings, .container = declaration.container, .concrete_type = declaration.concrete_type };
    var query = ref;
    query.local = &descriptor;
    // This read-only query interns types; body/IR construction errors cannot
    // arise here. Preserve the resolver's narrower public error contract.
    return BodyChecker.inferClosureType(module, ir, query, null, declaration.name_span) catch |err| return @errorCast(err);
}

/// Resolves an explicit, nonzero type node in a syntax-valid AST, without
/// evaluating runtime values or bodies. Aliases have canonical identity.
/// Semantic failures append source diagnostics and return null. Child types and
/// diagnostics may remain after OOM; callers needing a transaction must own and
/// discard the output IR and roll back diagnostics, as Sema does.
pub fn resolve(module: *Module, ir: *Ir, node: Ast.Node.Index) Error!?Ir.TypeId {
    return resolveWith(module, ir, node, &.{}, 0);
}

pub fn resolveInContainer(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32) Error!?Ir.TypeId {
    return resolveInScope(module, ir, node, owner, concrete, &.{});
}

pub fn resolveInScope(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32, function_bindings: []const Binding) Error!?Ir.TypeId {
    return resolveScope(module, ir, node, owner, concrete, function_bindings, &.{}, true);
}

/// Interns canonical type layouts without publishing container methods or IDs.
/// A later non-speculative resolution completes method publication on demand.
pub fn resolveInScopeReadOnly(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32, function_bindings: []const Binding) Error!?Ir.TypeId {
    return resolveScope(module, ir, node, owner, concrete, function_bindings, &.{}, false);
}

/// Value bindings are visible only to @TypeOf, never as ordinary type aliases.
pub fn resolveInScopeWithValues(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32, type_bindings: []const Binding, value_bindings: []const Binding) Error!?Ir.TypeId {
    return resolveScope(module, ir, node, owner, concrete, type_bindings, value_bindings, true);
}

pub fn resolveInScopeWithValuesReadOnly(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32, type_bindings: []const Binding, value_bindings: []const Binding) Error!?Ir.TypeId {
    return resolveScope(module, ir, node, owner, concrete, type_bindings, value_bindings, false);
}

pub fn resolveInScopeWithCaptures(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32, type_bindings: []const Binding, values: []const Binding, captures: []const GenericFunctions.Capture, publish_methods: bool) Error!?Ir.TypeId {
    return resolveScopeCaptures(module, ir, node, owner, concrete, type_bindings, values, captures, publish_methods);
}

fn resolveScope(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32, function_bindings: []const Binding, value_bindings: []const Binding, publish_methods: bool) Error!?Ir.TypeId {
    return resolveScopeCaptures(module, ir, node, owner, concrete, function_bindings, value_bindings, &.{}, publish_methods);
}

fn resolveScopeCaptures(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32, function_bindings: []const Binding, value_bindings: []const Binding, captures: []const GenericFunctions.Capture, publish_methods: bool) Error!?Ir.TypeId {
    const container = owner orelse return resolveEnvironment(module, ir, node, .{ .bindings = function_bindings, .values = value_bindings, .captures = captures, .expansions = 0, .publish_methods = publish_methods });
    var bindings: std.ArrayList(Binding) = .empty;
    defer bindings.deinit(module.allocator);
    try bindings.appendSlice(module.allocator, function_bindings);
    const declaration = module.getDeclaration(container).?;
    const full = module.source.ast.decl(declaration.node).ast;
    const this_type = if (concrete) |id| @as(Ir.TypeId, @fromBackingInt(@intCast(id))) else (try resolveWithMode(module, ir, full.init_node, &.{}, 0, publish_methods)) orelse return null;
    try bindings.append(module.allocator, .{ .name = "@This", .ty = this_type });
    if (concrete != null) {
        const args = ir.getType(this_type).?.record.type_args;
        var iterator = module.source.ast.fullFnProto(full.type_node).?.iterate(&module.source.ast);
        for (args) |arg| try bindings.append(module.allocator, .{ .name = module.source.ast.tokenSlice(iterator.next().?.name_token.?), .ty = arg });
    }
    for (module.source.ast.fullStruct(full.init_node).?) |member| {
        const alias = module.source.ast.fullDecl(member) orelse continue;
        if (!isThisAlias(&module.source.ast, member)) continue;
        try bindings.append(module.allocator, .{ .name = module.source.ast.tokenSlice(alias.ast.let_token + 1), .ty = this_type });
    }
    var expansions: usize = 0;
    if (concrete) |id| {
        for (module.declarations.items) |method| if (method.concrete_type == id) {
            expansions = method.specialization_depth;
            break;
        };
    }
    return resolveEnvironment(module, ir, node, .{ .bindings = bindings.items, .values = value_bindings, .captures = captures, .expansions = expansions, .publish_methods = publish_methods });
}

pub fn isThisAlias(ast: *const Ast, node: Ast.Node.Index) bool {
    const decl = ast.fullDecl(node) orelse return false;
    if (decl.ast.is_mutable or decl.ast.type_node != 0 or decl.ast.init_node == 0) return false;
    const call = ast.fullCall(decl.ast.init_node) orelse return false;
    return call.ast.args.len == 0 and std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This");
}

pub const Binding = Module.TypeBinding;
const ValueQueryMap = std.AutoHashMapUnmanaged(Compilation.DeclRef, union(enum) { visiting, complete: ?Ir.TypeId });
const Environment = struct { bindings: []const Binding, values: []const Binding = &.{}, captures: []const GenericFunctions.Capture = &.{}, expansions: usize, publish_methods: bool = true, query_depth: usize = 0, value_queries: ?*ValueQueryMap = null };
pub const max_instantiation_depth = 64;

fn resolveWith(module: *Module, ir: *Ir, node: Ast.Node.Index, bindings: []const Binding, expansions: usize) Error!?Ir.TypeId {
    return resolveWithMode(module, ir, node, bindings, expansions, true);
}

fn resolveWithMode(module: *Module, ir: *Ir, node: Ast.Node.Index, bindings: []const Binding, expansions: usize, publish_methods: bool) Error!?Ir.TypeId {
    return resolveEnvironment(module, ir, node, .{ .bindings = bindings, .expansions = expansions, .publish_methods = publish_methods });
}

fn resolveEnvironment(module: *Module, ir: *Ir, node: Ast.Node.Index, env: Environment) Error!?Ir.TypeId {
    if (env.value_queries != null) return resolveEnvironmentInner(module, ir, node, env);
    var queries: ValueQueryMap = .empty;
    defer queries.deinit(module.allocator);
    var shared = env;
    shared.value_queries = &queries;
    return resolveEnvironmentInner(module, ir, node, shared);
}

fn resolveEnvironmentInner(module: *Module, ir: *Ir, node: Ast.Node.Index, env: Environment) Error!?Ir.TypeId {
    if (env.query_depth >= max_alias_depth) return diagnose(module, node, .type_query_limit);
    if (env.expansions >= max_instantiation_depth) return diagnose(module, node, .generic_limit);
    var aliases: AliasMap = .empty;
    defer aliases.deinit(module.allocator);
    const Task = union(enum) {
        visit: struct { module: *Module, node: Ast.Node.Index, depth: usize = 0, alias_depth: usize = 0, local: bool = true },
        finish: Compilation.DeclRef,
    };
    var work: std.ArrayList(Task) = .empty;
    defer work.deinit(module.allocator);
    try work.append(module.allocator, .{ .visit = .{ .module = module, .node = node } });
    while (work.pop()) |task| switch (task) {
        .finish => |ref| {
            const owner = if (module.compilation) |comp| comp.getModule(ref.source).? else module;
            const initializer = owner.source.ast.decl(owner.getDeclaration(ref.local).?.node).ast.init_node;
            const ty = try resolveDepth(owner, ir, initializer, 0, &aliases, .{ .bindings = &.{}, .expansions = env.expansions, .publish_methods = env.publish_methods, .query_depth = env.query_depth, .value_queries = env.value_queries });
            try aliases.put(module.allocator, ref, .{ .ty = ty });
        },
        .visit => |visit| {
            if (visit.depth >= max_type_depth) continue; // resolveDepth diagnoses this path.
            const owner = visit.module;
            const ast = &owner.source.ast;
            if (ast.fullFnProto(visit.node)) |proto| {
                var iterator = proto.iterate(ast);
                while (iterator.next()) |param| {
                    if (!param.inferred) try work.append(module.allocator, .{ .visit = .{ .module = owner, .node = param.type_expr, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
                }
                if (proto.ast.return_type) |result| try work.append(module.allocator, .{ .visit = .{ .module = owner, .node = result, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
            } else if (ast.nodes.items(.tag)[visit.node] == .array_type or ast.nodes.items(.tag)[visit.node] == .ref_type) {
                try work.append(module.allocator, .{ .visit = .{ .module = owner, .node = ast.nodes.items(.data)[visit.node].rhs, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
            } else if (ast.fullCall(visit.node)) |call| {
                if (isTypeOf(ast, call)) continue;
                for (call.ast.args) |arg| try work.append(module.allocator, .{ .visit = .{ .module = owner, .node = arg, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
            } else if (ast.nodes.items(.tag)[visit.node] == .identifier or ast.nodes.items(.tag)[visit.node] == .field_access) {
                const name = ast.tokenSlice(ast.firstToken(visit.node));
                if (visit.local and ast.nodes.items(.tag)[visit.node] == .identifier and lookupBinding(env.bindings, name) != null) continue;
                // Resolve each edge in its defining scope, retaining namespace visibility.
                const ref: Compilation.DeclRef = if (owner.compilation) |comp| comp.reference(owner, visit.node) orelse continue else .{
                    .source = owner.source.id,
                    .local = if (ast.nodes.items(.tag)[visit.node] == .identifier) owner.lookup(name) orelse continue else continue,
                };
                const target = if (owner.compilation) |comp| comp.getModule(ref.source).? else owner;
                const declaration = target.getDeclaration(ref.local).?;
                if (!declaration.is_type_alias or declaration.is_generic) continue;
                if (aliases.get(ref)) |entry| {
                    if (entry.visiting) {
                        try owner.diagnostics.append(owner.allocator, .{ .tag = .type_alias_cycle, .span = owner.tokenSpan(ast.firstToken(visit.node)), .related = declaration.name_span });
                    }
                    continue;
                }
                if (visit.alias_depth >= max_alias_depth) {
                    try owner.diagnostics.append(owner.allocator, .{ .tag = .type_alias_limit, .span = owner.tokenSpan(ast.firstToken(visit.node)), .related = declaration.name_span });
                    try aliases.put(module.allocator, ref, .{});
                    continue;
                }
                try aliases.put(module.allocator, ref, .{ .visiting = true });
                try work.append(module.allocator, .{ .finish = ref });
                try work.append(module.allocator, .{ .visit = .{ .module = target, .node = target.source.ast.decl(declaration.node).ast.init_node, .alias_depth = visit.alias_depth + 1, .local = false } });
            }
        },
    };
    return resolveDepth(module, ir, node, 0, &aliases, env);
}

// Count all explicit type nodes on a path, including the root and leaf.
const max_type_depth = 128;
pub const max_alias_depth = 128;
// Share visiting/completed state across namespace edges. Restarting resolution
// at each imported alias makes branching cycles exponential despite depth limits.
const AliasMap = std.AutoHashMapUnmanaged(Compilation.DeclRef, struct { visiting: bool = false, ty: ?Ir.TypeId = null });

fn diagnose(module: *Module, node: Ast.Node.Index, tag: Module.Diagnostic.Tag) Error!?Ir.TypeId {
    try module.diagnostics.append(module.allocator, .{
        .tag = tag,
        .span = module.tokenSpan(module.source.ast.firstToken(node)),
    });
    return null;
}

fn resolveDepth(module: *Module, ir: *Ir, node: Ast.Node.Index, depth: usize, aliases: *const AliasMap, env: Environment) Error!?Ir.TypeId {
    if (depth >= max_type_depth) return diagnose(module, node, .type_nesting_limit);
    const ast = &module.source.ast;
    if (ast.nodes.items(.tag)[node] == .type_variable) {
        const name = ast.tokenSlice(ast.firstToken(node));
        return lookupBinding(env.bindings, name) orelse diagnose(module, node, .unbound_type_variable);
    }
    if (ast.fullFnProto(node)) |proto| {
        if (hasInferredSignature(ast, node)) return diagnose(module, node, .generic_function_ambiguous);
        var params: std.ArrayList(Ir.TypeId) = .empty;
        defer params.deinit(module.allocator);
        var names: std.StringHashMapUnmanaged(Module.Span) = .empty;
        defer names.deinit(module.allocator);
        var values: std.ArrayList(Binding) = .empty;
        defer values.deinit(module.allocator);
        try values.appendSlice(module.allocator, env.values);
        var signature_env = env;
        var valid = true;
        var iterator = proto.iterate(ast);
        while (iterator.next()) |param| {
            if (param.name_token) |token| {
                const span = module.tokenSpan(token);
                const name = ast.tokenSlice(token);
                if (std.mem.startsWith(u8, name, "@\"")) {
                    try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_name, .span = span });
                    valid = false;
                } else {
                    const entry = try names.getOrPut(module.allocator, name);
                    if (entry.found_existing) {
                        try module.diagnostics.append(module.allocator, .{
                            .tag = .duplicate_parameter,
                            .span = span,
                            .related = entry.value_ptr.*,
                        });
                        valid = false;
                    } else {
                        entry.value_ptr.* = span;
                    }
                }
            }
            signature_env.values = values.items;
            if (try resolveDepth(module, ir, param.type_expr, depth + 1, aliases, signature_env)) |ty| {
                if (ir.getType(ty).? == .void) {
                    _ = try diagnose(module, param.type_expr, .invalid_parameter_type);
                    valid = false;
                } else {
                    try params.append(module.allocator, ty);
                    if (param.name_token) |token| try values.append(module.allocator, .{ .name = ast.tokenSlice(token), .ty = ty });
                }
            } else {
                valid = false;
            }
        }
        signature_env.values = values.items;
        const result = if (proto.ast.return_type) |return_node|
            try resolveDepth(module, ir, return_node, depth + 1, aliases, signature_env)
        else
            try ir.internType(.void);
        if (result != null and ir.getType(result.?).? == .reference) {
            _ = try diagnose(module, proto.ast.return_type.?, .borrow_escape);
            valid = false;
        }
        if (!valid or result == null) return null;
        return try ir.internType(.{ .function = .{ .params = params.items, .result = result.? } });
    }
    if (ast.nodes.items(.tag)[node] == .ref_type) {
        const data = ast.nodes.items(.data)[node];
        const child = (try resolveDepth(module, ir, data.rhs, depth + 1, aliases, env)) orelse return null;
        if (!Ir.isArrayElement(ir.getType(child).?)) return diagnose(module, node, .unsupported_borrow);
        return try ir.internType(.{ .reference = .{ .child = child, .mutable = data.lhs != 0 } });
    }
    if (ast.nodes.items(.tag)[node] == .array_type) {
        const element = (try resolveDepth(module, ir, ast.nodes.items(.data)[node].rhs, depth + 1, aliases, env)) orelse return null;
        if (ir.getType(element).? == .reference) return diagnose(module, node, .borrow_escape);
        if (!Ir.isArrayElement(ir.getType(element).?)) return diagnose(module, node, .unsupported_type);
        return try ir.internType(.{ .array = element });
    }
    if (ast.fullCall(node)) |call| {
        if (isTypeOf(ast, call)) {
            if (call.ast.args.len != 1) return diagnose(module, node, .argument_count_mismatch);
            return resolveValueType(module, ir, call.ast.args[0], env);
        }
        if (call.ast.args.len == 0 and std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This")) {
            return lookupBinding(env.bindings, "@This") orelse diagnose(module, node, .unsupported_type);
        }
        return instantiate(module, ir, call, node, depth, aliases, env);
    }
    if (ast.fullStruct(node) != null) {
        for (module.declarations.items, 0..) |declaration, i| {
            if (declaration.is_record and ast.decl(declaration.node).ast.init_node == node) {
                if (ast.tokens.items(.tag)[ast.nodes.items(.main_token)[node]] == .keyword_enum) return resolveEnum(module, ir, @intCast(i), node);
                return try ir.internType(.{ .record = .{ .source = module.source.id, .identity = @intCast(i), .fields = &.{}, .public = declaration.public, .tagged = ast.isUnion(node) } });
            }
        }
        return diagnose(module, node, .unsupported_type);
    }
    if (ast.nodes.items(.tag)[node] == .field_access) {
        const comp = module.compilation orelse return diagnose(module, node, .unsupported_type);
        const ref = comp.reference(module, node) orelse return diagnose(module, node, .unknown_type);
        const owner = comp.getModule(ref.source).?;
        const declaration = owner.getDeclaration(ref.local).?;
        if (declaration.is_generic) return diagnose(module, node, .generic_needs_arguments);
        if (!declaration.is_type_alias) return diagnose(module, node, .not_a_type);
        return if (aliases.get(ref)) |entry| entry.ty else null;
    }
    if (ast.nodes.items(.tag)[node] != .identifier) return diagnose(module, node, .unsupported_type);
    const name = ast.tokenSlice(ast.firstToken(node));
    if (lookupBinding(env.bindings, name)) |ty| return ty;
    if (module.lookup(name)) |id| {
        if (primitives.isPrimitive(name)) return diagnose(module, node, .unsupported_type);
        if (module.getDeclaration(id).?.is_generic) return diagnose(module, node, .generic_needs_arguments);
        if (module.getDeclaration(id).?.is_type_alias) return if (aliases.get(.{ .source = module.source.id, .local = id })) |entry| entry.ty else null;
        return diagnose(module, node, .not_a_type);
    }
    if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) return diagnose(module, node, .not_a_type);
    if (std.mem.eql(u8, name, "string")) return try ir.internType(.string);
    if (std.mem.eql(u8, name, "rune")) return try ir.internType(.rune);
    if (std.mem.eql(u8, name, "bool")) return try ir.internType(.boolean);
    if (std.mem.eql(u8, name, "void")) return try ir.internType(.void);
    if (std.mem.eql(u8, name, "float")) return try ir.internType(.{ .float = Ir.native_float_bits });
    if (std.mem.eql(u8, name, "f32")) return try ir.internType(.{ .float = 32 });
    if (std.mem.eql(u8, name, "f64")) return try ir.internType(.{ .float = 64 });
    if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "uint")) {
        return try ir.internType(.{ .integer = .{
            .signedness = if (name[0] == 'i') .signed else .unsigned,
            .bits = Ir.native_pointer_bits,
        } });
    }
    inline for (.{ "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128" }) |integer| {
        if (std.mem.eql(u8, name, integer)) return try ir.internType(.{ .integer = .{
            .signedness = if (integer[0] == 'i') .signed else .unsigned,
            .bits = comptime std.fmt.parseInt(u16, integer[1..], 10) catch unreachable,
        } });
    }
    return diagnose(module, node, if (primitives.isPrimitive(name)) .unsupported_type else .unknown_type);
}

fn isTypeOf(ast: *const Ast, call: Ast.full.Call) bool {
    return ast.nodes.items(.tag)[call.ast.fn_expr] == .builtin_literal and
        std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@TypeOf");
}

fn resolveValueType(module: *Module, ir: *Ir, node: Ast.Node.Index, env: Environment) Error!?Ir.TypeId {
    if (env.query_depth >= max_alias_depth) return diagnose(module, node, .type_query_limit);
    const ast = &module.source.ast;
    if (ast.nodes.items(.tag)[node] == .identifier) {
        const name = ast.tokenSlice(ast.firstToken(node));
        var i = env.captures.len;
        while (i != 0) {
            i -= 1;
            const capture = env.captures[i];
            if (!std.mem.eql(u8, capture.name, name)) continue;
            if (capture.template != null) return diagnose(module, node, .not_a_value);
            return capture.ty;
        }
        if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) return try ir.internType(.boolean);
    } else if (ast.nodes.items(.tag)[node] != .field_access or !isImportedValue(module, node, env)) {
        return GenericFunctions.queryExpression(module, ir, node, env.bindings, env.values, env.captures) catch |err| return @errorCast(err);
    }
    var owner = module;
    const id = switch (ast.nodes.items(.tag)[node]) {
        .identifier => blk: {
            const name = ast.tokenSlice(ast.firstToken(node));
            // Last binding wins, so parameter and lexical scopes can shadow globals.
            var i = env.values.len;
            while (i > 0) {
                i -= 1;
                if (std.mem.eql(u8, env.values[i].name, name)) return env.values[i].ty;
            }
            if (lookupBinding(env.bindings, name) != null) return diagnose(module, node, .not_a_value);
            break :blk module.lookup(name) orelse return diagnose(module, node, if (primitives.isPrimitive(name)) .invalid_type_query_operand else .unknown_name);
        },
        .field_access => blk: {
            var base = node;
            while (ast.nodes.items(.tag)[base] == .field_access) base = ast.nodes.items(.data)[base].lhs;
            if (ast.nodes.items(.tag)[base] != .identifier) return diagnose(module, node, .invalid_type_query_operand);
            const name = ast.tokenSlice(ast.firstToken(base));
            for (env.values) |binding| if (std.mem.eql(u8, binding.name, name)) return diagnose(module, node, .invalid_type_query_operand);
            const comp = module.compilation orelse return diagnose(module, node, .invalid_type_query_operand);
            const ref = comp.reference(module, node) orelse return diagnose(module, node, .unknown_name);
            owner = comp.getModule(ref.source).?;
            break :blk ref.local;
        },
        else => return diagnose(module, node, .invalid_type_query_operand),
    };
    const declaration = owner.getDeclaration(id).?;
    if (declaration.is_type_alias or declaration.is_namespace or declaration.is_generic or declaration.is_generic_function or (declaration.is_test and !owner.testEnabled())) return diagnose(module, node, .not_a_value);
    // Retain failures as well as successes across sibling queries. An active-path
    // guard alone re-expands a branching cyclic signature exponentially.
    const ref: Compilation.DeclRef = .{ .source = owner.source.id, .local = id };
    const queries = env.value_queries.?;
    if (queries.get(ref)) |query| return switch (query) {
        .visiting => diagnose(module, node, .type_query_limit),
        .complete => |ty| ty,
    };
    try queries.put(module.allocator, ref, .visiting);
    const ty = try resolveGlobalValueType(module, ir, node, owner, id, env);
    queries.getPtr(ref).?.* = .{ .complete = ty };
    return ty;
}

fn isImportedValue(module: *Module, node: Ast.Node.Index, env: Environment) bool {
    const ast = &module.source.ast;
    var base = node;
    while (ast.nodes.items(.tag)[base] == .field_access) base = ast.nodes.items(.data)[base].lhs;
    if (ast.nodes.items(.tag)[base] != .identifier) return false;
    const name = ast.tokenSlice(ast.firstToken(base));
    for (env.values) |binding| if (std.mem.eql(u8, binding.name, name)) return false;
    for (env.captures) |capture| if (std.mem.eql(u8, capture.name, name)) return false;
    const comp = module.compilation orelse return false;
    return comp.reference(module, node) != null;
}

/// Global metadata lookup shared by constraint-only expression queries.
pub fn queryValue(module: *Module, ir: *Ir, node: Ast.Node.Index) Error!?Ir.TypeId {
    var queries: ValueQueryMap = .empty;
    defer queries.deinit(module.allocator);
    return resolveValueType(module, ir, node, .{ .bindings = &.{}, .expansions = 0, .publish_methods = false, .value_queries = &queries });
}

fn resolveGlobalValueType(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: *Module, id: Module.DeclId, env: Environment) Error!?Ir.TypeId {
    const declaration = owner.getDeclaration(id).?;
    const full = owner.source.ast.decl(declaration.node).ast;
    if (owner.source.ast.declFunction(declaration.node)) |function| {
        if (hasInferredSignature(&owner.source.ast, function.proto_node)) return GenericFunctions.queryFunction(owner, ir, .{ .module = owner, .id = id }, declaration.name_span) catch |err| return @errorCast(err);
    }
    const type_node = if (owner.source.ast.declFunction(declaration.node)) |function| function.proto_node else full.type_node;
    const global_env: Environment = .{ .bindings = &.{}, .expansions = env.expansions, .publish_methods = false, .query_depth = env.query_depth + 1, .value_queries = env.value_queries };
    if (type_node != 0) return resolveEnvironment(owner, ir, type_node, global_env);
    if (full.init_node == 0) return diagnose(module, node, .unresolved_value_type);
    const global_ast = &owner.source.ast;
    var initializer = full.init_node;
    var group_buffer: [2]Ast.Node.Index = undefined;
    while (global_ast.fullBlock(&group_buffer, initializer)) |block| {
        if (global_ast.firstToken(initializer) != block.ast.lbrace or block.ast.statements.len != 1) return diagnose(module, node, .unresolved_value_type);
        initializer = block.ast.statements[0];
    }
    if (global_ast.fullCall(initializer)) |call| {
        if (global_ast.nodes.items(.tag)[call.ast.fn_expr] == .builtin_literal and std.mem.eql(u8, global_ast.tokenSlice(global_ast.firstToken(call.ast.fn_expr)), "@as")) {
            if (call.ast.args.len != 2) return diagnose(owner, initializer, .argument_count_mismatch);
            return resolveEnvironment(owner, ir, call.ast.args[0], global_env);
        }
    }
    switch (global_ast.nodes.items(.tag)[initializer]) {
        .string_literal => return try ir.internType(.string),
        .char_literal => return try ir.internType(.rune),
        .identifier => {
            const name = global_ast.tokenSlice(global_ast.firstToken(initializer));
            if (owner.lookup(name) == null and (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false"))) return try ir.internType(.boolean);
            return resolveValueType(owner, ir, initializer, global_env);
        },
        .number_literal, .negation => {
            if (global_ast.nodes.items(.tag)[initializer] == .negation) {
                initializer = global_ast.nodes.items(.data)[initializer].rhs;
                var buffer: [2]Ast.Node.Index = undefined;
                while (global_ast.fullBlock(&buffer, initializer)) |block| {
                    if (global_ast.firstToken(initializer) != block.ast.lbrace or block.ast.statements.len != 1) return diagnose(module, node, .unresolved_value_type);
                    initializer = block.ast.statements[0];
                }
                if (global_ast.nodes.items(.tag)[initializer] != .number_literal) return diagnose(module, node, .unresolved_value_type);
            }
            switch (std.zig.parseNumberLiteral(global_ast.tokenSlice(global_ast.firstToken(initializer)))) {
                .int, .big_int => return try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = Ir.native_pointer_bits } }),
                .float => return try ir.internType(.{ .float = Ir.native_float_bits }),
                else => return diagnose(module, node, .unresolved_value_type),
            }
        },
        else => return GenericFunctions.queryExpression(owner, ir, full.init_node, &.{}, &.{}, &.{}) catch |err| return @errorCast(err),
    }
}

fn resolveEnum(module: *Module, ir: *Ir, identity: u32, node: Ast.Node.Index) Error!?Ir.TypeId {
    for (ir.types.items, 0..) |ty, i| if (ty == .enumeration and ty.enumeration.source == module.source.id and ty.enumeration.identity == identity) return @fromBackingInt(@intCast(i));
    const declaration = module.getDeclaration(@fromBackingInt(@intCast(identity))).?;
    const ast = &module.source.ast;
    var tags: std.ArrayList(Ir.EnumTag) = .empty;
    defer tags.deinit(module.allocator);
    var valid = true;
    for (ast.fullStruct(node).?) |member| {
        if (ast.nodes.items(.tag)[member] != .container_field) continue;
        const data = ast.nodes.items(.data)[member];
        const token = ast.nodes.items(.main_token)[member];
        const span = module.tokenSpan(token);
        const name = ast.tokenSlice(token);
        if (data.lhs != 0 or data.rhs != 0) {
            _ = try diagnose(module, member, .unsupported_enum_value);
            valid = false;
        }
        if (std.mem.startsWith(u8, name, "@\"")) {
            _ = try diagnose(module, member, .unsupported_name);
            valid = false;
        }
        var duplicate = module.lookupMethod(@fromBackingInt(@intCast(identity)), name) != null;
        for (tags.items) |tag| if (std.mem.eql(u8, name, tag.name)) {
            duplicate = true;
        };
        if (duplicate) {
            _ = try diagnose(module, member, .duplicate_field);
            valid = false;
        }
        try tags.append(module.allocator, .{ .name = name, .span = span });
    }
    if (tags.items.len == 0) return diagnose(module, node, .empty_enum);
    if (!valid) return null;
    return try ir.internType(.{ .enumeration = .{ .source = module.source.id, .identity = identity, .name = declaration.name, .tags = tags.items, .public = declaration.public } });
}

fn lookupBinding(bindings: []const Binding, name: []const u8) ?Ir.TypeId {
    for (bindings) |binding| if (std.mem.eql(u8, binding.name, name)) return binding.ty;
    return null;
}

/// Only ordinary named container templates and aliases to them are callable here.
pub fn genericTarget(module: *const Module, node: Ast.Node.Index) ?Module.DeclId {
    return (genericReference(module, node) orelse return null).local;
}

pub fn genericReference(module: *const Module, node: Ast.Node.Index) ?Compilation.DeclRef {
    var owner = module;
    var current = node;
    for (0..max_alias_depth) |_| {
        const ref: Compilation.DeclRef = if (owner.compilation) |comp| comp.reference(owner, current) orelse return null else .{
            .source = owner.source.id,
            .local = if (owner.source.ast.nodes.items(.tag)[current] == .identifier) owner.lookup(owner.source.ast.tokenSlice(owner.source.ast.firstToken(current))) orelse return null else return null,
        };
        if (owner.compilation) |comp| owner = comp.getModule(ref.source).?;
        const declaration = owner.getDeclaration(ref.local).?;
        if (!declaration.is_generic) return null;
        if (declaration.is_record) return ref;
        current = owner.source.ast.decl(declaration.node).ast.init_node;
    }
    return null;
}

fn namedType(ast: *const Ast, node: Ast.Node.Index, name: []const u8) bool {
    return node != 0 and ast.nodes.items(.tag)[node] == .identifier and std.mem.eql(u8, ast.tokenSlice(ast.firstToken(node)), name);
}

pub fn validateGeneric(module: *Module, id: Module.DeclId) Error!bool {
    const ast = &module.source.ast;
    const declaration = module.getDeclaration(id).?;
    const full = ast.decl(declaration.node).ast;
    const start = module.diagnostics.items.len;
    if (primitives.isPrimitive(declaration.name)) {
        try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_shadowing, .span = declaration.name_span });
    }
    const proto = ast.fullFnProto(full.type_node) orelse {
        _ = try diagnose(module, declaration.node, .invalid_generic_declaration);
        return false;
    };
    const tagged = ast.isUnion(full.init_node);
    const is_struct = ast.fullStruct(full.init_node) != null and ast.tokens.items(.tag)[ast.nodes.items(.main_token)[full.init_node]] == .keyword_struct;
    if (full.is_mutable or declaration.container != null or (!tagged and !is_struct) or proto.ast.params.len == 0 or !namedType(ast, proto.ast.return_type orelse 0, "type")) {
        _ = try diagnose(module, declaration.node, .invalid_generic_declaration);
        return false;
    }
    var names: std.StringHashMapUnmanaged(Module.Span) = .empty;
    defer names.deinit(module.allocator);
    var iterator = proto.iterate(ast);
    while (iterator.next()) |param| {
        const token = param.name_token orelse {
            _ = try diagnose(module, param.type_expr, .invalid_generic_declaration);
            continue;
        };
        if (!namedType(ast, param.type_expr, "type")) {
            _ = try diagnose(module, param.type_expr, .invalid_generic_declaration);
            continue;
        }
        const name = ast.tokenSlice(token);
        const span = module.tokenSpan(token);
        if (primitives.isPrimitive(name) or module.lookup(name) != null or std.mem.startsWith(u8, name, "@\"")) {
            try module.diagnostics.append(module.allocator, .{ .tag = .unsupported_shadowing, .span = span });
            continue;
        }
        const entry = try names.getOrPut(module.allocator, name);
        if (entry.found_existing) try module.diagnostics.append(module.allocator, .{ .tag = .duplicate_parameter, .span = span, .related = entry.value_ptr.* }) else entry.value_ptr.* = span;
    }
    var fields: std.StringHashMapUnmanaged(Module.Span) = .empty;
    defer fields.deinit(module.allocator);
    for (ast.fullStruct(full.init_node).?) |member| {
        if (ast.nodes.items(.tag)[member] != .container_field) {
            const member_decl = ast.fullDecl(member);
            if (member_decl) |decl| {
                const member_name = ast.tokenSlice(decl.ast.let_token + 1 + @as(u32, @intFromBool(decl.ast.is_mutable)));
                if (names.contains(member_name)) _ = try diagnose(module, member, .unsupported_shadowing);
            }
            if (isThisAlias(ast, member)) continue;
            const method = ast.fullDecl(member);
            if (method == null or method.?.ast.is_mutable or ast.declFunction(member) == null)
                _ = try diagnose(module, member, .unsupported_generic_method);
            continue;
        }
        const token = ast.nodes.items(.main_token)[member];
        const name = ast.tokenSlice(token);
        const span = module.tokenSpan(token);
        if (module.lookupMethod(id, name) != null or names.contains(name)) _ = try diagnose(module, member, .duplicate_field);
        const entry = try fields.getOrPut(module.allocator, name);
        if (entry.found_existing) try module.diagnostics.append(module.allocator, .{ .tag = .duplicate_field, .span = span, .related = entry.value_ptr.* }) else entry.value_ptr.* = span;
        const data = ast.nodes.items(.data)[member];
        if (tagged and data.rhs != 0) _ = try diagnose(module, data.rhs, .unsupported_union_default);
    }
    if (tagged and fields.count() == 0) _ = try diagnose(module, full.init_node, .empty_union);
    return module.diagnostics.items.len == start;
}

fn instantiate(module: *Module, ir: *Ir, call: Ast.full.Call, node: Ast.Node.Index, depth: usize, aliases: *const AliasMap, env: Environment) Error!?Ir.TypeId {
    const ref = genericReference(module, call.ast.fn_expr) orelse {
        const ast = &module.source.ast;
        if (ast.nodes.items(.tag)[call.ast.fn_expr] == .identifier) {
            if (module.lookup(ast.tokenSlice(ast.firstToken(call.ast.fn_expr)))) |target| {
                if (module.getDeclaration(target).?.is_generic) return diagnose(module, node, .generic_limit);
            }
        }
        return diagnose(module, node, .not_a_generic);
    };
    const owner = if (module.compilation) |comp| comp.getModule(ref.source).? else module;
    const declaration = owner.getDeclaration(ref.local).?;
    const proto = owner.source.ast.fullFnProto(owner.source.ast.decl(declaration.node).ast.type_node).?;
    if (call.ast.args.len != proto.ast.params.len) return diagnose(module, node, .generic_argument_count);
    const args = try module.allocator.alloc(Ir.TypeId, call.ast.args.len);
    defer module.allocator.free(args);
    for (call.ast.args, args) |arg, *ty| ty.* = (try resolveDepth(module, ir, arg, depth + 1, aliases, env)) orelse return null;
    return instantiateArgumentsMode(owner, ir, ref.local, args, env.expansions, env.publish_methods);
}

pub fn instantiateArguments(module: *Module, ir: *Ir, id: Module.DeclId, args: []const Ir.TypeId, expansions: usize) Error!?Ir.TypeId {
    return instantiateArgumentsMode(module, ir, id, args, expansions, true);
}

/// Complete an ordinary nominal's field metadata without demanding methods or
/// bodies. Nested nominal identities remain lazy, avoiding recursive layout walks.
pub fn completeRecordReadOnly(module: *Module, ir: *Ir, ty: Ir.TypeId) Error!bool {
    const record = ir.getType(ty).?.record;
    if (record.type_args.len != 0 or record.fields.len != 0 or record.instance_state == .building) return true;
    // A field's type query carries @This in its bindings. Converting that
    // binding must not re-enter completion of the layout being queried.
    ir.types.items[@backingInt(ty)].record.instance_state = .building;
    defer ir.types.items[@backingInt(ty)].record.instance_state = record.instance_state;
    const owner = if (module.compilation) |comp| comp.getModule(record.source).? else module;
    const id: Module.DeclId = @fromBackingInt(record.identity);
    const declaration = owner.getDeclaration(id).?;
    const ast = &owner.source.ast;
    var fields: std.ArrayList(Ir.Field) = .empty;
    defer fields.deinit(owner.allocator);
    for (ast.fullStruct(ast.decl(declaration.node).ast.init_node).?) |member| {
        if (ast.nodes.items(.tag)[member] != .container_field) continue;
        const data = ast.nodes.items(.data)[member];
        const field_ty = if (data.lhs == 0) try ir.internType(.void) else (try resolveInScopeReadOnly(owner, ir, data.lhs, id, null, &.{})) orelse return false;
        const ft = ir.getType(field_ty).?;
        if (ft == .reference or !(if (record.tagged) Ir.isUnionPayload(ft) else Ir.isRecordField(ft))) {
            _ = try diagnose(owner, member, .unsupported_type);
            return false;
        }
        const token = ast.nodes.items(.main_token)[member];
        const span = owner.tokenSpan(token);
        var default: ?Ir.ConstId = null;
        if (data.rhs != 0) switch (try Literal.check(owner, ir, data.rhs, field_ty, span)) {
            .constant => |constant| default = constant,
            .failed => return false,
            .not_literal => {
                _ = try diagnose(owner, data.rhs, .invalid_field_default);
                return false;
            },
        };
        try fields.append(owner.allocator, .{ .name = ast.tokenSlice(token), .ty = field_ty, .span = span, .default = default });
    }
    try ir.setRecordFields(ty, fields.items);
    return true;
}

pub fn instantiateArgumentsReadOnly(module: *Module, ir: *Ir, id: Module.DeclId, args: []const Ir.TypeId, expansions: usize) Error!?Ir.TypeId {
    return instantiateArgumentsMode(module, ir, id, args, expansions, false);
}

fn instantiateArgumentsMode(module: *Module, ir: *Ir, id: Module.DeclId, args: []const Ir.TypeId, expansions: usize, publish_methods: bool) Error!?Ir.TypeId {
    const declaration = module.getDeclaration(id).?;
    const node = declaration.node;
    if (!try validateGeneric(module, id)) return null;
    const ast = &module.source.ast;
    const full = ast.decl(node).ast;
    const proto = ast.fullFnProto(full.type_node).?;
    var existing: ?Ir.TypeId = null;
    for (ir.types.items, 0..) |ty, i| {
        if (ty != .record or ty.record.source != module.source.id or ty.record.identity != @backingInt(id) or !std.mem.eql(Ir.TypeId, ty.record.type_args, args)) continue;
        switch (ty.record.instance_state) {
            .complete => {
                if (!publish_methods or ty.record.methods_published) return @as(Ir.TypeId, @fromBackingInt(@intCast(i)));
                existing = @fromBackingInt(@intCast(i));
            },
            .building => return diagnose(module, node, .generic_cycle),
            // A speculative body probe may have rolled back its diagnostics.
            // Retry failed layouts so the real check reports the original cause.
            .failed => existing = @fromBackingInt(@intCast(i)),
        }
        break;
    }
    const tagged = ast.isUnion(full.init_node);
    const ty = existing orelse try ir.internType(.{ .record = .{ .source = module.source.id, .identity = @backingInt(id), .fields = &.{}, .tagged = tagged, .public = declaration.public, .type_args = args, .instance_state = .building } });
    ir.types.items[@backingInt(ty)].record.instance_state = .building;
    var complete = false;
    defer if (!complete) {
        ir.types.items[@backingInt(ty)].record.instance_state = .failed;
    };
    var bindings: std.ArrayList(Binding) = .empty;
    defer bindings.deinit(module.allocator);
    var iterator = proto.iterate(ast);
    for (args) |arg| try bindings.append(module.allocator, .{ .name = ast.tokenSlice(iterator.next().?.name_token.?), .ty = arg });
    try bindings.append(module.allocator, .{ .name = "@This", .ty = ty });
    for (ast.fullStruct(full.init_node).?) |member| {
        if (isThisAlias(ast, member)) try bindings.append(module.allocator, .{ .name = ast.tokenSlice(ast.fullDecl(member).?.ast.let_token + 1), .ty = ty });
    }
    var fields: std.ArrayList(Ir.Field) = .empty;
    defer fields.deinit(module.allocator);
    for (ast.fullStruct(full.init_node).?) |member| {
        if (ast.nodes.items(.tag)[member] != .container_field) continue;
        const data = ast.nodes.items(.data)[member];
        if (!tagged and data.lhs == 0) return diagnose(module, member, .unsupported_type);
        const field_ty = if (data.lhs == 0) try ir.internType(.void) else (try resolveWithMode(module, ir, data.lhs, bindings.items, expansions + 1, publish_methods)) orelse return null;
        const ft = ir.getType(field_ty).?;
        if (ft == .reference) return diagnose(module, member, .borrow_escape);
        if (ft == .record and ft.record.instance_state == .building) return diagnose(module, member, .generic_cycle);
        if (!(if (tagged) Ir.isUnionPayload(ft) else Ir.isRecordField(ft))) return diagnose(module, member, .unsupported_type);
        const token = ast.nodes.items(.main_token)[member];
        const span = module.tokenSpan(token);
        var default: ?Ir.ConstId = null;
        if (data.rhs != 0) {
            if (ft == .record) return diagnose(module, data.rhs, .invalid_field_default);
            switch (try Literal.check(module, ir, data.rhs, field_ty, span)) {
                .constant => |constant| default = constant,
                .failed => return null,
                .not_literal => return diagnose(module, data.rhs, .invalid_field_default),
            }
        }
        try fields.append(module.allocator, .{ .name = ast.tokenSlice(token), .ty = field_ty, .span = span, .default = default });
    }
    try ir.setRecordFields(ty, fields.items);
    ir.types.items[@backingInt(ty)].record.instance_state = .complete;
    complete = true;
    if (!publish_methods) return ty;
    // Specialized declarations retain source ASTs but have distinct stable IDs.
    // Publish all siblings together before any signature or body is checked.
    const count = module.declarations.items.len;
    for (0..count) |i| {
        var method = module.declarations.items[i];
        if (method.container != id or method.concrete_type != null) continue;
        var published = false;
        for (module.declarations.items) |other| {
            if (other.node == method.node and other.concrete_type == @backingInt(ty) and other.function_template == null) {
                published = true;
                break;
            }
        }
        if (published) continue;
        if (module.declarations.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
        method.concrete_type = @backingInt(ty);
        method.specialization_depth = expansions + 1;
        try module.declarations.append(module.allocator, method);
        if (module.compilation) |comp| {
            _ = comp.internDeclaration(.{ .source = module.source.id, .local = @fromBackingInt(@intCast(module.declarations.items.len - 1)) }) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.TooManyDeclarations => return error.TooManyEntries,
            };
        }
    }
    ir.types.items[@backingInt(ty)].record.methods_published = true;
    return ty;
}

fn testTypeNode(module: *const Module, name: []const u8) Ast.Node.Index {
    return module.source.ast.decl(module.getDeclaration(module.lookup(name).?).?.node).ast.type_node;
}

fn testResolve(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "types.nov", source);
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    for (module.declarations.items) |declaration| {
        const node = module.source.ast.decl(declaration.node).ast.type_node;
        if (node != 0) _ = try resolve(&module, &ir, node);
    }
    try ir.verify();
}

test "explicit primitive types have canonical identities and widths" {
    var module = try Module.init(std.testing.allocator, "primitives.nov",
        \\let a: bool
        \\let b: void
        \\let c: i8
        \\let d: i16
        \\let e: i32
        \\let f: i64
        \\let g: i128
        \\let h: u8
        \\let i: u16
        \\let j: u32
        \\let k: u64
        \\let l: u128
        \\let m: rune
    );
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    try std.testing.expect(!module.hasErrors());
    const expected = [_]Ir.Type{
        .boolean,                                                 .void,
        .{ .integer = .{ .signedness = .signed, .bits = 8 } },    .{ .integer = .{ .signedness = .signed, .bits = 16 } },
        .{ .integer = .{ .signedness = .signed, .bits = 32 } },   .{ .integer = .{ .signedness = .signed, .bits = 64 } },
        .{ .integer = .{ .signedness = .signed, .bits = 128 } },  .{ .integer = .{ .signedness = .unsigned, .bits = 8 } },
        .{ .integer = .{ .signedness = .unsigned, .bits = 16 } }, .{ .integer = .{ .signedness = .unsigned, .bits = 32 } },
        .{ .integer = .{ .signedness = .unsigned, .bits = 64 } }, .{ .integer = .{ .signedness = .unsigned, .bits = 128 } },
        .rune,
    };
    for (module.declarations.items, expected) |declaration, ty| {
        const node = module.source.ast.decl(declaration.node).ast.type_node;
        const id = (try resolve(&module, &ir, node)).?;
        try std.testing.expectEqual(try ir.internType(ty), id);
        try std.testing.expectEqual(id, (try resolve(&module, &ir, node)).?);
    }
    try std.testing.expect(!module.hasErrors());
    try ir.verify();
}

test "float types aliases and TypeOf globals resolve without evaluating constants" {
    var module = try Module.init(std.testing.allocator, "float-types.nov",
        \\let Narrow = f32
        \\let Wide = f64
        \\let a: Narrow
        \\let b: Wide
        \\let c: float
        \\let fraction = -{0.5}
        \\let grouped = {0x1.8p2}
        \\let narrowed = @as(f32, 1)
        \\let d: @TypeOf(fraction)
        \\let e: @TypeOf(grouped)
        \\let f: @TypeOf(narrowed)
        \\let g: (f32, float) -> f64
    );
    defer module.deinit();
    try module.classifyAliases();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const narrow = try ir.internType(.{ .float = 32 });
    const wide = try ir.internType(.{ .float = 64 });
    const native = try ir.internType(.{ .float = Ir.native_float_bits });
    const expected = [_]Ir.TypeId{ narrow, wide, native, native, native, narrow };
    for ([_][]const u8{ "a", "b", "c", "d", "e", "f" }, expected) |name, ty| {
        try std.testing.expectEqual(ty, (try resolve(&module, &ir, testTypeNode(&module, name))).?);
    }
    const signature = (try resolve(&module, &ir, testTypeNode(&module, "g"))).?;
    try std.testing.expectEqualSlices(Ir.TypeId, &.{ narrow, native }, ir.getType(signature).?.function.params);
    try std.testing.expectEqual(wide, ir.getType(signature).?.function.result);
    try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    try std.testing.expect(!module.hasErrors());
    try ir.verify();
}

test "native integer aliases canonicalize with fixed width types and signatures" {
    const source = try std.fmt.allocPrint(std.testing.allocator, "let a: int\nlet b: i{d}\nlet c: uint\nlet d: u{d}\nlet e: (int, (uint) -> int) -> uint\nlet f: (i{d}, (u{d}) -> i{d}) -> u{d}", .{ @bitSizeOf(usize), @bitSizeOf(usize), @bitSizeOf(usize), @bitSizeOf(usize), @bitSizeOf(usize), @bitSizeOf(usize) });
    defer std.testing.allocator.free(source);
    var module = try Module.init(std.testing.allocator, "native.nov", source);
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    var ids: [6]Ir.TypeId = undefined;
    for (module.declarations.items, &ids) |declaration, *id| {
        id.* = (try resolve(&module, &ir, module.source.ast.decl(declaration.node).ast.type_node)).?;
    }
    try std.testing.expectEqual(ids[0], ids[1]);
    try std.testing.expectEqual(ids[2], ids[3]);
    try std.testing.expectEqual(ids[4], ids[5]);
    try std.testing.expect(!module.hasErrors());
    try ir.verify();
}

test "function signatures canonicalize names omission and nested types without bodies" {
    var module = try Module.init(std.testing.allocator, "functions.nov",
        \\let zero: () = unknown_body()
        \\let zero_explicit: () -> void
        \\let one: (i32) -> bool
        \\let named: (value: i32) -> bool
        \\let many: (i32, flag: bool, u64) -> i128
        \\let nested: (callback: (x: i32) -> bool) -> ()
        \\let nested_again: ((i32) -> bool) -> () -> void
    );
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    try std.testing.expect(!module.hasErrors());
    var ids: [7]Ir.TypeId = undefined;
    for (module.declarations.items, &ids) |declaration, *id| {
        id.* = (try resolve(&module, &ir, module.source.ast.decl(declaration.node).ast.type_node)).?;
    }
    try std.testing.expectEqual(ids[0], ids[1]);
    try std.testing.expectEqual(ids[2], ids[3]);
    try std.testing.expectEqual(ids[5], ids[6]);
    const zero = ir.getType(ids[0]).?.function;
    try std.testing.expectEqual(@as(usize, 0), zero.params.len);
    try std.testing.expect(ir.getType(zero.result).? == .void);
    const one = ir.getType(ids[2]).?.function;
    try std.testing.expectEqual(@as(usize, 1), one.params.len);
    try std.testing.expectEqual(try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 32 } }), one.params[0]);
    try std.testing.expect(ir.getType(one.result).? == .boolean);
    const many = ir.getType(ids[4]).?.function;
    try std.testing.expectEqualSlices(Ir.TypeId, &.{
        one.params[0],
        try ir.internType(.boolean),
        try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 64 } }),
    }, many.params);
    try std.testing.expectEqual(try ir.internType(.{ .integer = .{ .signedness = .signed, .bits = 128 } }), many.result);
    const nested = ir.getType(ids[5]).?.function;
    try std.testing.expectEqualSlices(Ir.TypeId, &.{ids[2]}, nested.params);
    try std.testing.expectEqual(ids[0], nested.result);
    try std.testing.expect(!module.hasErrors());
    // Resolution has already freed its temporary parameter lists.
    try std.testing.expectEqualSlices(Ir.TypeId, &.{ids[2]}, ir.getType(ids[5]).?.function.params);
    try ir.verify();
}

test "unsupported unknown and non-type forms have source diagnostics" {
    const Case = struct { spelling: []const u8, tag: Module.Diagnostic.Tag, span: []const u8 };
    const cases = [_]Case{
        .{ .spelling = "Missing", .tag = .unknown_type, .span = "Missing" },
        .{ .spelling = "alias", .tag = .not_a_type, .span = "alias" },
        .{ .spelling = "true", .tag = .not_a_type, .span = "true" },
        .{ .spelling = "false", .tag = .not_a_type, .span = "false" },
        .{ .spelling = "&void", .tag = .unsupported_borrow, .span = "&" },
        .{ .spelling = "42", .tag = .unsupported_type, .span = "42" },
        .{ .spelling = "i7", .tag = .unknown_type, .span = "i7" },
    };
    for (cases) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let alias = 1\nlet value: {s}", .{case.spelling});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "bad.nov", source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(case.tag, diagnostic.tag);
        try std.testing.expectEqualStrings(case.span, module.sourceSlice(diagnostic.span).?);
        try std.testing.expect(diagnostic.related == null);
    }
    for ([_][]const u8{ "f16", "f80", "f128", "type" }) |name| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let value: {s}", .{name});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "unsupported.nov", source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_type, module.diagnostics.items[0].tag);
        try std.testing.expectEqualStrings(name, module.sourceSlice(module.diagnostics.items[0].span).?);
        try std.testing.expectEqual(@as(usize, 0), ir.types.items.len);
    }
}

test "malformed signatures collect duplicate void unknown and result failures" {
    var module = try Module.init(std.testing.allocator, "parameters.nov", "let value: (x: Missing, x: void, x: bool, void, (a: void)) -> Absent");
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
    const expected = [_]Module.Diagnostic.Tag{
        .unknown_type,        .duplicate_parameter,    .invalid_parameter_type,
        .duplicate_parameter, .invalid_parameter_type, .invalid_parameter_type,
        .unknown_type,
    };
    try std.testing.expectEqual(expected.len, module.diagnostics.items.len);
    const first_name = module.tokenSpan(module.source.ast.fullFnProto(testTypeNode(&module, "value")).?.lparen + 1);
    for (module.diagnostics.items, expected) |diagnostic, tag| {
        try std.testing.expectEqual(tag, diagnostic.tag);
        if (tag == .duplicate_parameter) {
            try std.testing.expectEqual(first_name, diagnostic.related.?);
            try std.testing.expectEqualStrings("x", module.sourceSlice(diagnostic.span).?);
            try std.testing.expect(first_name.start < diagnostic.span.start);
        } else if (tag == .invalid_parameter_type) {
            try std.testing.expectEqualStrings("void", module.sourceSlice(diagnostic.span).?);
        }
    }
    for (ir.types.items) |ty| try std.testing.expect(ty != .function);
    try ir.verify();
}

test "individual parameter and result failures never intern a successful signature" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let value: (@\"x\": i32)", .tag = .unsupported_name },
        .{ .source = "let value: (callback: (@\"x\": i32))", .tag = .unsupported_name },
        .{ .source = "let value: (void)", .tag = .invalid_parameter_type },
        .{ .source = "let value: (x: void)", .tag = .invalid_parameter_type },
        .{ .source = "let value: (Missing)", .tag = .unknown_type },
        .{ .source = "let value: (i32) -> Missing", .tag = .unknown_type },
        .{ .source = "let value: (i32) -> f16", .tag = .unsupported_type },
        .{ .source = "let value: (true)", .tag = .not_a_type },
        .{ .source = "let value: ((void))", .tag = .invalid_parameter_type },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "invalid-signature.nov", case.source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        for (ir.types.items) |ty| try std.testing.expect(ty != .function);
        try ir.verify();
    }
}

test "duplicate names alone invalidate signatures but nested scopes are independent" {
    for ([_][]const u8{ "let value: (x: i32, x: bool)", "let value: (x: (x: i32), y: (x: i32))" }, 0..) |source, i| {
        var module = try Module.init(std.testing.allocator, "names.nov", source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expect(!module.hasErrors());
        const result = try resolve(&module, &ir, testTypeNode(&module, "value"));
        try std.testing.expectEqual(i == 1, result != null);
        try std.testing.expectEqual(@as(usize, if (i == 0) 1 else 0), module.diagnostics.items.len);
        try ir.verify();
    }
}

test "recursive type depth is bounded for parameter and result paths" {
    for ([_]bool{ false, true }) |parameter_path| {
        for ([_]usize{ 127, 128, 256 }) |count| {
            var source = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer source.deinit();
            try source.writer.writeAll("let value: ");
            for (0..count) |_| try source.writer.writeAll(if (parameter_path) "(" else "() -> ");
            try source.writer.writeAll("bool");
            if (parameter_path) for (0..count) |_| try source.writer.writeAll(")");
            var module = try Module.init(std.testing.allocator, "depth.nov", source.written());
            defer module.deinit();
            var ir = Ir.init(std.testing.allocator);
            defer ir.deinit();
            try std.testing.expect(!module.hasErrors());
            const result = try resolve(&module, &ir, testTypeNode(&module, "value"));
            try std.testing.expectEqual(count == 127, result != null);
            if (count >= 128) {
                try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
                const diagnostic = module.diagnostics.items[0];
                try std.testing.expectEqual(Module.Diagnostic.Tag.type_nesting_limit, diagnostic.tag);
                const expected_start = "let value: ".len + 128 * @as(usize, if (parameter_path) 1 else "() -> ".len);
                try std.testing.expectEqual(expected_start, diagnostic.span.start);
            }
            try ir.verify();
        }
    }
}

test "module IR temporary signatures and diagnostics clean up on every allocation failure" {
    for ([_][]const u8{
        "let value: ()",
        "let value: (x: i32, bool, u128) -> i64\nlet again: (i32, bool, u128) -> i64",
        "let value: (callback: (x: i32) -> bool) -> (u8) -> void",
        "let value: (x: Missing, x: void, x: bool, void) -> Absent",
        "let value: (x: i32, x: bool)",
        "let value: int",
    }) |source| {
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testResolve, .{source});
    }
}

fn testTypeOfExpressions(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "typeof-expressions.nov",
        \\let effect:()->int=missing
        \\let id:(x:`T)->`T=x
        \\let apply:(x:`T,f:(`T)->`U)->`U=missing
        \\let R=struct{x:u8,let method:()->bool=missing
        \\}
        \\let Box:(T:type)->type=struct{x:T,let method:()->bool=missing
        \\}
        \\let arr:[]u8=[1]
        \\let a:@TypeOf(1/0)
        \\let b:@TypeOf(1+2.0)
        \\let c:@TypeOf(effect())
        \\let d:@TypeOf(arr[999])
        \\let e:@TypeOf(R[.x=1].x)
        \\let f:@TypeOf("abc"[999..1000])
        \\let g:@TypeOf(if true {1} else {2})
        \\let h:@TypeOf(match true {true=>1
        \\false=>2})
        \\let i:@TypeOf(loop {break 1})
        \\let j:@TypeOf(id(@as(u16,1)))
        \\let k:@TypeOf([1,2.0])
        \\let l:@TypeOf(@panic("never evaluated"))
        \\let m:@TypeOf(Box(u16)[.x=1])
        \\let n:@TypeOf(apply(@as(u8,1),id))
        \\let o:@TypeOf(apply(@as(u8,1),(v){v+1}))
    );
    defer module.deinit();
    try module.classifyAliases();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const declarations = module.declarations.items.len;
    if (module.hasErrors()) std.debug.print("TypeOf expression parse: {any} {any}\n", .{ module.source.ast.errors, module.diagnostics.items });
    try std.testing.expect(!module.hasErrors());
    for ([_][]const u8{ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o" }) |name| {
        const ty = try resolve(&module, &ir, testTypeNode(&module, name));
        errdefer std.debug.print("TypeOf expression {s}: {any}\n", .{ name, module.diagnostics.items });
        try std.testing.expect(ty != null);
        const expected = if (std.mem.eql(u8, name, "b")) try BodyChecker.defaultFloatType(&ir) else if (std.mem.eql(u8, name, "d") or std.mem.eql(u8, name, "e") or std.mem.eql(u8, name, "n") or std.mem.eql(u8, name, "o")) try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 8 } }) else if (std.mem.eql(u8, name, "f")) try ir.internType(.string) else if (std.mem.eql(u8, name, "j")) try ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = 16 } }) else if (std.mem.eql(u8, name, "k")) try ir.internType(.{ .array = try BodyChecker.defaultFloatType(&ir) }) else if (std.mem.eql(u8, name, "l")) try ir.internType(.void) else if (std.mem.eql(u8, name, "m")) ty.? else try BodyChecker.defaultIntegerType(&ir);
        try std.testing.expectEqual(expected, ty.?);
    }
    try std.testing.expectEqual(declarations, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.expression_query_depth);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
}

fn testValuePayloadQueries(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "value-payload-queries.nov",
        \\let O:(T:type)->type=union{some:T,none,
        \\let unused:()->Missing=missing
        \\}
        \\let F=(int)->int
        \\let Range=@TypeOf(0..1)
        \\let id:(x:`T)->`T=x
        \\let wrap:(x:`T)->O(`T)=missing
        \\let callback:()->F=missing
        \\let a:@TypeOf(O(F).some[id])
        \\let b:@TypeOf(wrap(callback()))
        \\let c:@TypeOf(O(Range).some[0..=2])
        \\let d:@TypeOf(wrap(0..1))
    );
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    try module.classifyAliases();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(module.source.ast.source.len);
    const count = module.declarations.items.len;
    var function_option: ?Ir.TypeId = null;
    var range_option: ?Ir.TypeId = null;
    for ([_][]const u8{ "a", "b", "c", "d" }, 0..) |name, i| {
        const ty = (try resolveInScopeReadOnly(&module, &ir, testTypeNode(&module, name), null, null, &.{})) orelse {
            std.debug.print("value payload query {s}: {any}\n", .{ name, module.diagnostics.items });
            return error.TestUnexpectedResult;
        };
        const record = ir.getType(ty).?.record;
        try std.testing.expect(record.tagged and !record.methods_published);
        try std.testing.expect(record.instance_state == .complete);
        if (i < 2) {
            try std.testing.expect(ir.getType(record.fields[0].ty).? == .function);
            if (function_option) |expected| try std.testing.expectEqual(expected, ty);
            function_option = ty;
        } else {
            try std.testing.expect(ir.getType(record.fields[0].ty).? == .range);
            if (range_option) |expected| try std.testing.expectEqual(expected, ty);
            range_option = ty;
        }
    }
    try std.testing.expectEqual(count, module.declarations.items.len);
    for (module.declarations.items) |decl| try std.testing.expect(decl.function_template == null and decl.concrete_type == null);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.local_generic_instances.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.expression_query_depth);
    try std.testing.expect(!module.hasErrors());
}

fn testInlineRangeQuery(allocator: std.mem.Allocator, expression: []const u8, expected: ?Module.Diagnostic.Tag) !void {
    const source = try std.fmt.allocPrint(allocator, "let U=union{{range:@TypeOf(0..1),number:int,}}\nlet Alias=U\n" ++
        "let O:(T:type)->type=union{{some:T,none,}}\n" ++
        "let take:(u:U)->U=missing\nlet id:(x:`T)->`T=missing\nlet query:@TypeOf({s})", .{expression});
    defer allocator.free(source);
    var module = try Module.init(allocator, "inline-range-query.nov", source);
    defer module.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    try module.classifyAliases();
    const count = module.declarations.items.len;
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(source.len);
    const result = try resolveInScopeReadOnly(&module, &ir, testTypeNode(&module, "query"), null, null, &.{});
    errdefer std.debug.print("inline range query {s}: {any}\n", .{ expression, module.diagnostics.items });
    if (expected) |tag| {
        try std.testing.expect(result == null);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
    } else {
        try std.testing.expect(result != null);
        try std.testing.expect(!module.hasErrors());
    }
    try std.testing.expectEqual(count, module.declarations.items.len);
    for (module.declarations.items) |decl| try std.testing.expect(decl.function_template == null and decl.concrete_type == null);
    for (ir.types.items) |ty| if (ty == .record) try std.testing.expect(!ty.record.methods_published);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.local_generic_instances.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.expression_query_depth);
}

test "inline range payloads TypeOf validates constructors without publication under OOM" {
    for ([_]struct { expression: []const u8, tag: ?Module.Diagnostic.Tag = null }{
        .{ .expression = "Alias.range[0..=3]" },
        .{ .expression = "take(.range[0..3])" },
        .{ .expression = "id(O(@TypeOf(0..1)).some[0..=3])" },
        .{ .expression = "\"abc\"[0..2]" },
        .{ .expression = "U.range[..3]", .tag = .invalid_union_constructor },
        .{ .expression = "U.range[0..]", .tag = .invalid_union_constructor },
        .{ .expression = "take(.range[..])", .tag = .invalid_union_constructor },
        .{ .expression = "id(O(@TypeOf(0..1)).some[..=3])", .tag = .invalid_union_constructor },
        .{ .expression = "U.number[0..2]", .tag = .generic_function_conflict },
        .{ .expression = "U.range[-1..2]", .tag = .literal_out_of_range },
        .{ .expression = "U.range[0..missing]", .tag = .unknown_name },
        .{ .expression = "U.range[0..true]", .tag = .generic_function_conflict },
    }) |case| {
        try testInlineRangeQuery(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testInlineRangeQuery, .{ case.expression, case.tag });
    }
}

test "value payloads TypeOf inference is strictly readonly under OOM" {
    try testValuePayloadQueries(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testValuePayloadQueries, .{});
}

test "TypeOf expressions check constraints without evaluation or method publication under OOM" {
    try testTypeOfExpressions(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfExpressions, .{});
}

test "TypeOf expressions reject invalid operators names arguments and bounded cycles" {
    for ([_][]const u8{
        "let a:@TypeOf(true+false)",
        "let a:@TypeOf(1+missing)",
        "let f:(x:bool)->int=missing\nlet a:@TypeOf(f(1))",
        "let f:(x:u8)->int=missing\nlet a:@TypeOf(f(999))",
        "let f=(){true+false}\nlet a:@TypeOf(f)",
        "let f:(x:int)->int=missing\nlet a:@TypeOf(f(missing))",
        "let f:(x:int)->int=missing\nlet a:@TypeOf(f())",
        "let f:(x:`T)->`T=x\nlet a:@TypeOf(f)",
        "let a:@TypeOf(1.*)",
        "let a:@TypeOf(1.x)",
        "let a:@TypeOf(@panic())",
        "let x:@TypeOf(x+1)\nlet a:@TypeOf(x)",
        "let x=y+z\nlet y=x+z\nlet z=x+y\nlet a:@TypeOf(x)",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "typeof-invalid-expression.nov", source);
        defer module.deinit();
        try module.classifyAliases();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        errdefer std.debug.print("TypeOf invalid expression: {s}\n{any}\n", .{ source, module.diagnostics.items });
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "a")));
        try std.testing.expect(module.diagnostics.items.len > 0);
        try std.testing.expect(module.diagnostics.items.len <= max_alias_depth);
        try std.testing.expectEqual(@as(usize, 0), module.expression_query_depth);
    }
}

fn testTypeOfValidationCase(allocator: std.mem.Allocator, expression: []const u8, expected: ?Module.Diagnostic.Tag) !void {
    const source = try std.fmt.allocPrint(allocator, "let g:int=1\nlet E=enum{{one,two,}}\nlet U=union{{a:int,b,e:E,}}\nlet fn_value:()->int=missing\nlet take_ref:(r:&int)->int=missing\nlet no_body:(int:int)->int=missing\nlet R=struct{{let toString:(r:R)->string=missing\n}}\nlet query_result:@TypeOf({s})", .{expression});
    defer allocator.free(source);
    var module = try Module.init(allocator, "typeof-validation.nov", source);
    defer module.deinit();
    try module.classifyAliases();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    ir.source_len = @intCast(source.len);
    const declarations = module.declarations.items.len;
    if (module.hasErrors()) std.debug.print("TypeOf validation parse {s}: {any} {any}\n", .{ expression, module.source.ast.errors, module.diagnostics.items });
    try std.testing.expect(!module.hasErrors());
    const result = try resolve(&module, &ir, testTypeNode(&module, "query_result"));
    errdefer if (module.diagnostics.items.len != 0) {
        std.debug.print("TypeOf validation {s}: {any}\n", .{ expression, module.diagnostics.items });
    };
    if (expected) |tag| {
        try std.testing.expect(result == null);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(tag, module.diagnostics.items[0].tag);
    } else {
        try std.testing.expect(result != null);
        try std.testing.expect(!module.hasErrors());
    }
    try std.testing.expectEqual(declarations, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    try std.testing.expectEqual(@as(usize, 0), ir.functions.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.expression_query_depth);
    for (ir.types.items) |ty| if (ty == .record) {
        try std.testing.expect(!ty.record.methods_published);
    };
}

test "TypeOf validation interpolation and literal decoding are readonly under OOM" {
    const Case = struct { expression: []const u8, tag: ?Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .expression = "\"{missing}\"", .tag = .unknown_name },
        .{ .expression = "\"{true+false}\"", .tag = .operator_not_supported },
        .{ .expression = "\"{}\"", .tag = .invalid_interpolation },
        .{ .expression = "'ab'", .tag = .invalid_literal },
        .{ .expression = "@as(f32,1e100)", .tag = .literal_out_of_range },
        .{ .expression = "\"{1/0} {R[]} {int}\"", .tag = null },
        .{ .expression = "@as(i8,-128)", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation classifies writable places and local borrow sources under OOM" {
    const Case = struct { expression: []const u8, tag: ?Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .expression = "{let a=1\nlet p=&a\np.*=2}", .tag = .immutable_assignment },
        .{ .expression = "{let mut s=\"a\"\ns[0]='x'}", .tag = .unsupported_assignment_target },
        .{ .expression = "{let mut a=[1]\na.len=2}", .tag = .unsupported_array_length_write },
        .{ .expression = "{let mut s=\"a\"\ns.len=2}", .tag = .unsupported_array_length_write },
        .{ .expression = "{let s=\"a\"\nlet p=&s[0]\np}", .tag = .unsupported_borrow },
        .{ .expression = "&g", .tag = .unsupported_borrow },
        .{ .expression = "{let a=[1]\nlet p=&a[999]\np}", .tag = null },
        .{ .expression = "{let mut a=1\nlet p=&a\np.*=2\na}", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation shares match pattern and coverage rules under OOM" {
    const Case = struct { expression: []const u8, tag: ?Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .expression = "match 1.0 {_=>1}", .tag = .unsupported_match_subject },
        .{ .expression = "match true {missing=>1\n_=>2}", .tag = .unsupported_match_pattern },
        .{ .expression = "match true {true=>1\ntrue=>2\nfalse=>3}", .tag = .match_covered_boolean_prong },
        .{ .expression = "match true {true|false=>1}", .tag = null },
        .{ .expression = "match 1 {true=>1\n_=>2}", .tag = .literal_type_mismatch },
        .{ .expression = "match E.one {.one=>1\n.one=>2\n.two=>3}", .tag = .match_covered_enum_prong },
        .{ .expression = "match E.one {.one|.two=>1}", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation checks every union constructor form and supplied payload under OOM" {
    for ([_]struct { expression: []const u8, tag: ?Module.Diagnostic.Tag }{
        .{ .expression = "U.a[]", .tag = .invalid_union_constructor },
        .{ .expression = "U.a[.bad=missing]", .tag = .invalid_union_constructor },
        .{ .expression = "U.b[.bad=missing]", .tag = .invalid_union_constructor },
        .{ .expression = "U.a[missing]", .tag = .unknown_name },
        .{ .expression = "U.b[1]", .tag = .invalid_union_constructor },
        .{ .expression = "U.e[.bad]", .tag = .unknown_field },
        .{ .expression = "U.e[.one,]", .tag = .invalid_union_constructor },
        .{ .expression = "U.a[1]", .tag = null },
        .{ .expression = "U.b[]", .tag = null },
        .{ .expression = "U.e[.one]", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation shares ordinary local declaration restrictions under OOM" {
    for ([_]struct { expression: []const u8, tag: ?Module.Diagnostic.Tag }{
        .{ .expression = "{let x=1\nlet x=true\nx}", .tag = .unsupported_shadowing },
        .{ .expression = "{let x:int\nx}", .tag = .missing_initializer },
        .{ .expression = "{let mut x:void={}\nx}", .tag = .unsupported_local_declaration },
        .{ .expression = "{let x={}\n1}", .tag = .unsupported_local_declaration },
        .{ .expression = "{let _=1\n2}", .tag = .unsupported_local_declaration },
        .{ .expression = "{let int=1\nint}", .tag = .unsupported_shadowing },
        .{ .expression = "{let g=1\ng}", .tag = .unsupported_shadowing },
        .{ .expression = "{let x:void={}\n1}", .tag = null },
        .{ .expression = "{let x=1\nlet y=x+1\ny}", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation retains invalid intermediate types after projections and unused locals under OOM" {
    for ([_]struct { expression: []const u8, tag: ?Module.Diagnostic.Tag }{
        .{ .expression = "{let x=1\nlet p=&x\nlet pp=&p\npp.*.*}", .tag = .unsupported_borrow },
        .{ .expression = "{let x=[{}]\n1}", .tag = .unsupported_type },
        .{ .expression = "{let f:(x:void)={}\n1}", .tag = .invalid_parameter_type },
        .{ .expression = "{let f:(p:&int)->&int=p\n1}", .tag = .borrow_escape },
        .{ .expression = "{let x=[]\n1}", .tag = .array_literal_needs_type },
        .{ .expression = "{let x=[1]\nlet p=&x\np.*[999]}", .tag = null },
        .{ .expression = "{let f:(x:int)->int=x\n1}", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation checks print arguments and unsigned negation without converter demands under OOM" {
    for ([_]struct { expression: []const u8, tag: ?Module.Diagnostic.Tag }{
        .{ .expression = "@println({})", .tag = .unsupported_body_expression },
        .{ .expression = "@println(fn_value)", .tag = .unsupported_body_expression },
        .{ .expression = "@print([fn_value])", .tag = .unsupported_body_expression },
        .{ .expression = "-@as(u8,1)", .tag = .unsupported_body_expression },
        .{ .expression = "-@as(u8,0)", .tag = .unsupported_body_expression },
        .{ .expression = "@println(R[],1,int)", .tag = null },
        .{ .expression = "-@as(i8,1)", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation restricts reference storage without banning reference forwarding under OOM" {
    for ([_]struct { expression: []const u8, tag: ?Module.Diagnostic.Tag }{
        .{ .expression = "{let x=1\nlet p=&x\nlet q=p\nq.*}", .tag = .unsupported_borrow },
        .{ .expression = "{let x=1\nlet p=&x\nlet q:&int=p\nq.*}", .tag = .unsupported_borrow },
        .{ .expression = "{let x=1\nlet p=&x\nlet mut q=&x\nq=p\nq.*}", .tag = .unsupported_borrow },
        .{ .expression = "{let x=1\nlet p=&x\nlet mut q=&x\nq={p}\nq.*}", .tag = .unsupported_borrow },
        .{ .expression = "{let x=1\nlet y=2\nlet mut q=&x\nq=&y\nq.*}", .tag = null },
        .{ .expression = "{let mut x=1\nlet mut y=2\nlet mut q:&int=&x\nq=&y\nq.*}", .tag = null },
        .{ .expression = "{let x=1\nlet p=&x\ntake_ref(p)}", .tag = null },
        .{ .expression = "{let x=1\ntake_ref(&x)}", .tag = null },
        .{ .expression = "{let x=1\nlet p=&x\nlet get:(r:&`T)->`T=r.*\nget(p)}", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation shares closure and template parameter scopes under OOM" {
    for ([_]struct { expression: []const u8, tag: ?Module.Diagnostic.Tag }{
        .{ .expression = "(x:int,x:int)->int{x}", .tag = .duplicate_parameter },
        .{ .expression = "(int:int)->int{int}", .tag = .unsupported_shadowing },
        .{ .expression = "{let id:(x:`T,x:`T)->`T=x\nid(1,2)}", .tag = .duplicate_parameter },
        .{ .expression = "{let id:(x:`T,x:`T)->`T=x\n1}", .tag = .duplicate_parameter },
        .{ .expression = "{let x=1\nlet f=(x:int)->int{x}\nf(1)}", .tag = .unsupported_shadowing },
        .{ .expression = "{let x=1\nlet id:(x:`T)->`T=x\nid(1)}", .tag = .unsupported_shadowing },
        .{ .expression = "(x:int)->int{let f=(x:int)->int{x}\nf(1)}", .tag = .unsupported_shadowing },
        .{ .expression = "{let id:(int:`T)->`T=int\nid(1)}", .tag = .unsupported_shadowing },
        .{ .expression = "{let first=(v:int)->int{v}\nlet second=(v:int)->int{v}\nsecond(first(1))}", .tag = null },
        .{ .expression = "{let id:(v:`T)->`T=v\nid(1)}", .tag = null },
        .{ .expression = "no_body(1)", .tag = null },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, case.tag });
    }
}

test "TypeOf validation rejects unused values and unreachable block tails under OOM" {
    for ([_]struct { expression: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .expression = "{1\n2}", .tag = .unused_block_value },
        .{ .expression = "loop {break 1\nmissing()}", .tag = .unreachable_code },
        .{ .expression = "loop {continue\nmissing()}", .tag = .unreachable_code },
    }) |case| {
        try testTypeOfValidationCase(std.testing.allocator, case.expression, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfValidationCase, .{ case.expression, @as(?Module.Diagnostic.Tag, case.tag) });
    }
}

fn testTypeOfGlobals(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "typeof.nov",
        \\let T = @TypeOf(number)
        \\let number = -42
        \\let flag = true
        \\let text = "hello"
        \\let rune_value = 'x'
        \\let explicitly: u8 = 2
        \\let effect: () -> bool = true
        \\let pending: u16 = effect()
        \\let a: T = 0
        \\let b: @TypeOf(flag) = false
        \\let c: @TypeOf(text) = ""
        \\let d: @TypeOf(rune_value) = 'a'
        \\let e: @TypeOf(explicitly) = 0
        \\let f: @TypeOf(effect) = effect
        \\let g: @TypeOf(pending) = 0
        \\let signature: (x: int, y: @TypeOf(x)) -> @TypeOf(y)
    );
    defer module.deinit();
    try module.classifyAliases();
    try std.testing.expect(module.getDeclaration(module.lookup("T").?).?.is_type_alias);
    try std.testing.expect(!module.getDeclaration(module.lookup("flag").?).?.is_type_alias);
    var ir = Ir.init(allocator);
    defer ir.deinit();
    for ([_][]const u8{ "a", "b", "c", "d", "e", "f", "g", "signature" }) |name| {
        try std.testing.expect((try resolve(&module, &ir, testTypeNode(&module, name))) != null);
    }
    try std.testing.expectEqual(@as(usize, 0), ir.constants.items.len);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
}

test "TypeOf resolves global bindings aliases scalar defaults and signatures without evaluation" {
    try testTypeOfGlobals(std.testing.allocator);
}

test "TypeOf globals clean up after allocation failure" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testTypeOfGlobals, .{});
}

test "TypeOf diagnoses arity unknown names types expressions and query cycles" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let a: @TypeOf()", .tag = .argument_count_mismatch },
        .{ .source = "let a: @TypeOf(x, x)", .tag = .argument_count_mismatch },
        .{ .source = "let a: @TypeOf(missing)", .tag = .unknown_name },
        .{ .source = "let a: @TypeOf(int)", .tag = .invalid_type_query_operand },
        .{ .source = "let T = int\nlet a: @TypeOf(T)", .tag = .not_a_value },

        .{ .source = "let x: @TypeOf(x)\nlet a: @TypeOf(x)", .tag = .type_query_limit },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "typeof.nov", case.source);
        defer module.deinit();
        try module.classifyAliases();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "a")));
        try std.testing.expect(module.diagnostics.items.len > 0);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
    }
}

test "TypeOf imported values respect visibility and reject namespaces and types" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const root = try comp.addSource("/typeof/main.nov",
        \\let lib = @import("lib.nov")
        \\let a: @TypeOf(lib.visible)
        \\let b: @TypeOf(lib.hidden)
        \\let c: @TypeOf(lib)
        \\let d: @TypeOf(lib.T)
    );
    const library = try comp.addSource("/typeof/lib.nov", "@[public]\nlet visible = true\nlet hidden = 42\n@[public]\nlet T = int");
    comp.setRoot(root);
    const module = comp.getModule(root).?;
    const local = module.lookup("lib").?;
    try comp.imports.append(std.testing.allocator, .{
        .declaration = comp.declaration_ids.get(.{ .source = root, .local = local }).?,
        .target = library,
        .named = false,
        .span = module.getDeclaration(local).?.name_span,
    });
    try comp.prepareSemantics();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const ty = (try resolve(module, &ir, testTypeNode(module, "a"))).?;
    try std.testing.expect(ir.getType(ty).? == .boolean);
    for ([_][]const u8{ "b", "c", "d" }) |name| {
        const before = module.diagnostics.items.len;
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(module, &ir, testTypeNode(module, name)));
        try std.testing.expect(module.diagnostics.items.len > before);
    }
}

fn testLocalReferenceTypes(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "references.nov", "let a: &int\nlet b: &mut int\nlet c: &f64\nlet d: &[]int\nlet e: &(() -> void)\nlet query: @TypeOf(p)\nlet Alias = int\nlet alias_ref: &Alias\nlet signature: (p: &mut int, q: @TypeOf(p)) -> void\nlet callbacks: [](p: &int) -> void\nlet indirect: (p: @TypeOf(query)) -> void");
    defer module.deinit();
    try module.classifyAliases();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    const readonly = (try resolve(&module, &ir, testTypeNode(&module, "a"))).?;
    const writable = (try resolve(&module, &ir, testTypeNode(&module, "b"))).?;
    try std.testing.expect(readonly != writable);
    try std.testing.expect(!ir.getType(readonly).?.reference.mutable);
    try std.testing.expect(ir.getType(writable).?.reference.mutable);
    try std.testing.expectEqual(ir.getType(readonly).?.reference.child, ir.getType(writable).?.reference.child);
    for ([_][]const u8{ "c", "d", "e" }) |name| {
        const ty = (try resolve(&module, &ir, testTypeNode(&module, name))).?;
        try std.testing.expect(ir.getType(ty).? == .reference);
    }
    try std.testing.expectEqual(readonly, (try resolve(&module, &ir, testTypeNode(&module, "alias_ref"))).?);
    const values = &[_]Binding{.{ .name = "p", .ty = writable }};
    try std.testing.expectEqual(writable, (try resolveInScopeWithValuesReadOnly(&module, &ir, testTypeNode(&module, "query"), null, null, &.{}, values)).?);
    const signature = (try resolve(&module, &ir, testTypeNode(&module, "signature"))).?;
    try std.testing.expectEqualSlices(Ir.TypeId, &.{ writable, writable }, ir.getType(signature).?.function.params);
    const callbacks = (try resolve(&module, &ir, testTypeNode(&module, "callbacks"))).?;
    try std.testing.expect(ir.getType(ir.getType(callbacks).?.array).? == .function);
    const indirect = (try resolveInScopeWithValuesReadOnly(&module, &ir, testTypeNode(&module, "indirect"), null, null, &.{}, &.{.{ .name = "query", .ty = readonly }})).?;
    try std.testing.expectEqual(readonly, ir.getType(indirect).?.function.params[0]);
    try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    try ir.verify();
}

test "local reference type resolution and TypeOf value bindings survive OOM" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLocalReferenceTypes, .{});
}

test "local reference types reject void reborrows and result or array escapes" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let value: &void", .tag = .unsupported_borrow },
        .{ .source = "let value: &mut void", .tag = .unsupported_borrow },
        .{ .source = "let value: & &int", .tag = .unsupported_borrow },
        .{ .source = "let value: &mut &int", .tag = .unsupported_borrow },
        .{ .source = "let value: []&int", .tag = .borrow_escape },
        .{ .source = "let value: () -> &int", .tag = .borrow_escape },
        .{ .source = "let value: &(() -> &int)", .tag = .borrow_escape },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "escape.nov", case.source);
        defer module.deinit();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolve(&module, &ir, testTypeNode(&module, "value")));
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        for (ir.types.items) |ty| try std.testing.expect(ty != .function and ty != .array);
        try ir.verify();
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testResolve, .{case.source});
    }
}

test "local reference TypeOf descriptors cannot escape indirectly into results arrays or nominal fields" {
    for ([_][]const u8{
        "let value: () -> @TypeOf(p)",
        "let value: []@TypeOf(p)",
        "let G: (T: type) -> type = struct { item: T, }\nlet value: G(@TypeOf(p))",
        "let G: (T: type) -> type = union { item: T, }\nlet value: G(@TypeOf(p))",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "indirect.nov", source);
        defer module.deinit();
        try module.classifyAliases();
        var ir = Ir.init(std.testing.allocator);
        defer ir.deinit();
        const integer = try ir.internType(.{ .integer = .{ .bits = 32, .signedness = .signed } });
        const reference = try ir.internType(.{ .reference = .{ .child = integer, .mutable = false } });
        const values = &[_]Binding{.{ .name = "p", .ty = reference }};
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolveInScopeWithValues(&module, &ir, testTypeNode(&module, "value"), null, null, &.{}, values));
        try std.testing.expectEqual(Module.Diagnostic.Tag.borrow_escape, module.diagnostics.items[0].tag);
    }
}

test "TypeOf value bindings are separate from type aliases in readonly resolution" {
    var module = try Module.init(std.testing.allocator, "typeof.nov", "let a: @TypeOf(x)\nlet b: x");
    defer module.deinit();
    var ir = Ir.init(std.testing.allocator);
    defer ir.deinit();
    const boolean = try ir.internType(.boolean);
    const values = &[_]Binding{.{ .name = "x", .ty = boolean }};
    try std.testing.expectEqual(boolean, (try resolveInScopeWithValuesReadOnly(&module, &ir, testTypeNode(&module, "a"), null, null, &.{}, values)).?);
    try std.testing.expectEqual(@as(?Ir.TypeId, null), try resolveInScopeWithValues(&module, &ir, testTypeNode(&module, "b"), null, null, &.{}, values));
    try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_type, module.diagnostics.items[0].tag);
}
