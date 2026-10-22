const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const primitives = @import("primitives.zig");

pub const Error = Ir.InternError;

/// Resolves an explicit, nonzero type node in a syntax-valid AST, without
/// evaluating runtime values or bodies. Aliases have canonical identity.
/// Semantic failures append source diagnostics and return null. Child types and
/// diagnostics may remain after OOM; callers needing a transaction must own and
/// discard the output IR and roll back diagnostics, as Sema does.
pub fn resolve(module: *Module, ir: *Ir, node: Ast.Node.Index) Error!?Ir.TypeId {
    return resolveWith(module, ir, node, &.{}, 0);
}

pub fn resolveInContainer(module: *Module, ir: *Ir, node: Ast.Node.Index, owner: ?Module.DeclId, concrete: ?u32) Error!?Ir.TypeId {
    const container = owner orelse return resolve(module, ir, node);
    var bindings: std.ArrayList(Binding) = .empty;
    defer bindings.deinit(module.allocator);
    const declaration = module.getDeclaration(container).?;
    const full = module.ast.decl(declaration.node).ast;
    const this_type = if (concrete) |id| @as(Ir.TypeId, @enumFromInt(id)) else (try resolve(module, ir, full.init_node)) orelse return null;
    try bindings.append(module.allocator, .{ .name = "@This", .ty = this_type });
    if (concrete != null) {
        const args = ir.getType(this_type).?.record.type_args;
        var iterator = module.ast.fullFnProto(full.type_node).?.iterate(&module.ast);
        for (args) |arg| try bindings.append(module.allocator, .{ .name = module.ast.tokenSlice(iterator.next().?.name_token.?), .ty = arg });
    }
    for (module.ast.fullStruct(full.init_node).?) |member| {
        const alias = module.ast.fullDecl(member) orelse continue;
        if (!isThisAlias(&module.ast, member)) continue;
        try bindings.append(module.allocator, .{ .name = module.ast.tokenSlice(alias.ast.let_token + 1), .ty = this_type });
    }
    var expansions: usize = 0;
    if (concrete) |id| {
        for (module.declarations.items) |method| if (method.concrete_type == id) {
            expansions = method.specialization_depth;
            break;
        };
    }
    return resolveWith(module, ir, node, bindings.items, expansions);
}

pub fn isThisAlias(ast: *const Ast, node: Ast.Node.Index) bool {
    const decl = ast.fullDecl(node) orelse return false;
    if (decl.ast.is_mutable or decl.ast.type_node != 0 or decl.ast.init_node == 0) return false;
    const call = ast.fullCall(decl.ast.init_node) orelse return false;
    return call.ast.args.len == 0 and std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This");
}

const Binding = struct { name: []const u8, ty: Ir.TypeId };
const Environment = struct { bindings: []const Binding, expansions: usize };
pub const max_instantiation_depth = 64;

fn resolveWith(module: *Module, ir: *Ir, node: Ast.Node.Index, bindings: []const Binding, expansions: usize) Error!?Ir.TypeId {
    if (expansions >= max_instantiation_depth) return diagnose(module, node, .generic_limit);
    var aliases: AliasMap = .empty;
    defer aliases.deinit(module.allocator);
    const Task = union(enum) {
        visit: struct { node: Ast.Node.Index, depth: usize = 0, alias_depth: usize = 0, local: bool = true },
        finish: Module.DeclId,
    };
    var work: std.ArrayList(Task) = .empty;
    defer work.deinit(module.allocator);
    try work.append(module.allocator, .{ .visit = .{ .node = node } });
    while (work.pop()) |task| switch (task) {
        .finish => |id| {
            const initializer = module.ast.decl(module.getDeclaration(id).?.node).ast.init_node;
            const ty = try resolveDepth(module, ir, initializer, 0, &aliases, .{ .bindings = &.{}, .expansions = expansions });
            try aliases.put(module.allocator, id, .{ .ty = ty });
        },
        .visit => |visit| {
            if (visit.depth >= max_type_depth) continue; // resolveDepth diagnoses this path.
            const ast = &module.ast;
            if (ast.fullFnProto(visit.node)) |proto| {
                var iterator = proto.iterate(ast);
                while (iterator.next()) |param| try work.append(module.allocator, .{ .visit = .{ .node = param.type_expr, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
                if (proto.ast.return_type) |result| try work.append(module.allocator, .{ .visit = .{ .node = result, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
            } else if (ast.nodes.items(.tag)[visit.node] == .array_type) {
                try work.append(module.allocator, .{ .visit = .{ .node = ast.nodes.items(.data)[visit.node].rhs, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
            } else if (ast.fullCall(visit.node)) |call| {
                for (call.ast.args) |arg| try work.append(module.allocator, .{ .visit = .{ .node = arg, .depth = visit.depth + 1, .alias_depth = visit.alias_depth, .local = visit.local } });
            } else if (ast.nodes.items(.tag)[visit.node] == .identifier) {
                const name = ast.tokenSlice(ast.firstToken(visit.node));
                if (visit.local and lookupBinding(bindings, name) != null) continue;
                const id = module.lookup(ast.tokenSlice(ast.firstToken(visit.node))) orelse continue;
                const declaration = module.getDeclaration(id).?;
                if (!declaration.is_type_alias or declaration.is_generic) continue;
                if (aliases.get(id)) |entry| {
                    if (entry.visiting) {
                        try module.diagnostics.append(module.allocator, .{ .tag = .type_alias_cycle, .span = module.tokenSpan(ast.firstToken(visit.node)), .related = declaration.name_span });
                    }
                    continue;
                }
                if (visit.alias_depth >= max_alias_depth) {
                    try module.diagnostics.append(module.allocator, .{ .tag = .type_alias_limit, .span = module.tokenSpan(ast.firstToken(visit.node)), .related = declaration.name_span });
                    try aliases.put(module.allocator, id, .{});
                    continue;
                }
                try aliases.put(module.allocator, id, .{ .visiting = true });
                try work.append(module.allocator, .{ .finish = id });
                try work.append(module.allocator, .{ .visit = .{ .node = ast.decl(declaration.node).ast.init_node, .alias_depth = visit.alias_depth + 1, .local = false } });
            }
        },
    };
    return resolveDepth(module, ir, node, 0, &aliases, .{ .bindings = bindings, .expansions = expansions });
}

// Count all explicit type nodes on a path, including the root and leaf.
const max_type_depth = 128;
pub const max_alias_depth = 128;
const AliasMap = std.AutoHashMapUnmanaged(Module.DeclId, struct { visiting: bool = false, ty: ?Ir.TypeId = null });

fn diagnose(module: *Module, node: Ast.Node.Index, tag: Module.Diagnostic.Tag) Error!?Ir.TypeId {
    try module.diagnostics.append(module.allocator, .{
        .tag = tag,
        .span = module.tokenSpan(module.ast.firstToken(node)),
    });
    return null;
}

fn resolveDepth(module: *Module, ir: *Ir, node: Ast.Node.Index, depth: usize, aliases: *const AliasMap, env: Environment) Error!?Ir.TypeId {
    if (depth >= max_type_depth) return diagnose(module, node, .type_nesting_limit);
    const ast = &module.ast;
    if (ast.fullFnProto(node)) |proto| {
        var params: std.ArrayList(Ir.TypeId) = .empty;
        defer params.deinit(module.allocator);
        var names: std.StringHashMapUnmanaged(Module.Span) = .empty;
        defer names.deinit(module.allocator);
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
            if (try resolveDepth(module, ir, param.type_expr, depth + 1, aliases, env)) |ty| {
                if (ir.getType(ty).? == .void) {
                    _ = try diagnose(module, param.type_expr, .invalid_parameter_type);
                    valid = false;
                } else {
                    try params.append(module.allocator, ty);
                }
            } else {
                valid = false;
            }
        }
        const result = if (proto.ast.return_type) |return_node|
            try resolveDepth(module, ir, return_node, depth + 1, aliases, env)
        else
            try ir.internType(.void);
        if (!valid or result == null) return null;
        return try ir.internType(.{ .function = .{ .params = params.items, .result = result.? } });
    }
    if (ast.nodes.items(.tag)[node] == .array_type) {
        const element = (try resolveDepth(module, ir, ast.nodes.items(.data)[node].rhs, depth + 1, aliases, env)) orelse return null;
        if (!Ir.isArrayElement(ir.getType(element).?)) return diagnose(module, node, .unsupported_type);
        return try ir.internType(.{ .array = element });
    }
    if (ast.fullCall(node)) |call| {
        if (call.ast.args.len == 0 and std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@This")) {
            return lookupBinding(env.bindings, "@This") orelse diagnose(module, node, .unsupported_type);
        }
        return instantiate(module, ir, call, node, depth, aliases, env);
    }
    if (ast.fullStruct(node) != null) {
        for (module.declarations.items, 0..) |declaration, i| {
            if (declaration.is_record and ast.decl(declaration.node).ast.init_node == node) {
                if (ast.tokens.items(.tag)[ast.nodes.items(.main_token)[node]] == .keyword_enum) return resolveEnum(module, ir, @intCast(i), node);
                return try ir.internType(.{ .record = .{ .identity = @intCast(i), .fields = &.{}, .public = declaration.public, .tagged = ast.isUnion(node) } });
            }
        }
        return diagnose(module, node, .unsupported_type);
    }
    if (ast.nodes.items(.tag)[node] != .identifier) return diagnose(module, node, .unsupported_type);
    const name = ast.tokenSlice(ast.firstToken(node));
    if (lookupBinding(env.bindings, name)) |ty| return ty;
    if (module.lookup(name)) |id| {
        if (primitives.isPrimitive(name)) return diagnose(module, node, .unsupported_type);
        if (module.getDeclaration(id).?.is_generic) return diagnose(module, node, .generic_needs_arguments);
        if (module.getDeclaration(id).?.is_type_alias) return if (aliases.get(id)) |entry| entry.ty else null;
        return diagnose(module, node, .not_a_type);
    }
    if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) return diagnose(module, node, .not_a_type);
    if (std.mem.eql(u8, name, "string")) return try ir.internType(.string);
    if (std.mem.eql(u8, name, "bool")) return try ir.internType(.boolean);
    if (std.mem.eql(u8, name, "void")) return try ir.internType(.void);
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

fn resolveEnum(module: *Module, ir: *Ir, identity: u32, node: Ast.Node.Index) Error!?Ir.TypeId {
    for (ir.types.items, 0..) |ty, i| if (ty == .enumeration and ty.enumeration.identity == identity) return @enumFromInt(i);
    const declaration = module.getDeclaration(@enumFromInt(identity)).?;
    const ast = &module.ast;
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
        var duplicate = module.lookupMethod(@enumFromInt(identity), name) != null;
        for (tags.items) |tag| if (std.mem.eql(u8, name, tag.name)) {
            duplicate = true;
        };
        if (duplicate) {
            _ = try diagnose(module, member, .duplicate_field);
            valid = false;
        }
        try tags.append(module.allocator, .{ .name = name, .span = .{ .start = span.start, .end = span.end } });
    }
    if (tags.items.len == 0) return diagnose(module, node, .empty_enum);
    if (!valid) return null;
    return try ir.internType(.{ .enumeration = .{ .identity = identity, .name = declaration.name, .tags = tags.items, .public = declaration.public } });
}

fn lookupBinding(bindings: []const Binding, name: []const u8) ?Ir.TypeId {
    for (bindings) |binding| if (std.mem.eql(u8, binding.name, name)) return binding.ty;
    return null;
}

/// Only ordinary named union templates and aliases to those templates are callable here.
pub fn genericTarget(module: *const Module, node: Ast.Node.Index) ?Module.DeclId {
    var current = node;
    const ast = &module.ast;
    for (0..max_alias_depth) |_| {
        if (ast.nodes.items(.tag)[current] != .identifier) return null;
        const id = module.lookup(ast.tokenSlice(ast.firstToken(current))) orelse return null;
        const declaration = module.getDeclaration(id).?;
        if (!declaration.is_generic) return null;
        if (declaration.is_record) return id;
        current = ast.decl(declaration.node).ast.init_node;
    }
    return null;
}

fn namedType(ast: *const Ast, node: Ast.Node.Index, name: []const u8) bool {
    return node != 0 and ast.nodes.items(.tag)[node] == .identifier and std.mem.eql(u8, ast.tokenSlice(ast.firstToken(node)), name);
}

pub fn validateGeneric(module: *Module, id: Module.DeclId) Error!bool {
    const ast = &module.ast;
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
    if (full.is_mutable or declaration.container != null or !ast.isUnion(full.init_node) or proto.ast.params.len == 0 or !namedType(ast, proto.ast.return_type orelse 0, "type")) {
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
            if (method == null or method.?.ast.is_mutable or ast.fullFnProto(method.?.ast.type_node) == null)
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
        if (data.rhs != 0) _ = try diagnose(module, data.rhs, .unsupported_union_default);
    }
    if (fields.count() == 0) _ = try diagnose(module, full.init_node, .empty_union);
    return module.diagnostics.items.len == start;
}

fn instantiate(module: *Module, ir: *Ir, call: Ast.full.Call, node: Ast.Node.Index, depth: usize, aliases: *const AliasMap, env: Environment) Error!?Ir.TypeId {
    const id = genericTarget(module, call.ast.fn_expr) orelse {
        const ast = &module.ast;
        if (ast.nodes.items(.tag)[call.ast.fn_expr] == .identifier) {
            if (module.lookup(ast.tokenSlice(ast.firstToken(call.ast.fn_expr)))) |target| {
                if (module.getDeclaration(target).?.is_generic) return diagnose(module, node, .generic_limit);
            }
        }
        return diagnose(module, node, .not_a_generic);
    };
    if (!try validateGeneric(module, id)) return null;
    const ast = &module.ast;
    const declaration = module.getDeclaration(id).?;
    const full = ast.decl(declaration.node).ast;
    const proto = ast.fullFnProto(full.type_node).?;
    if (call.ast.args.len != proto.ast.params.len) return diagnose(module, node, .generic_argument_count);
    const args = try module.allocator.alloc(Ir.TypeId, call.ast.args.len);
    defer module.allocator.free(args);
    for (call.ast.args, args) |arg, *ty| ty.* = (try resolveDepth(module, ir, arg, depth + 1, aliases, env)) orelse return null;
    var existing: ?Ir.TypeId = null;
    for (ir.types.items, 0..) |ty, i| {
        if (ty != .record or ty.record.identity != @intFromEnum(id) or !std.mem.eql(Ir.TypeId, ty.record.type_args, args)) continue;
        switch (ty.record.instance_state) {
            .complete => return @as(Ir.TypeId, @enumFromInt(i)),
            .building => return diagnose(module, node, .generic_cycle),
            // A speculative body probe may have rolled back its diagnostics.
            // Retry failed layouts so the real check reports the original cause.
            .failed => existing = @enumFromInt(i),
        }
        break;
    }
    const ty = existing orelse try ir.internType(.{ .record = .{ .identity = @intFromEnum(id), .fields = &.{}, .tagged = true, .public = declaration.public, .type_args = args, .instance_state = .building } });
    ir.types.items[@intFromEnum(ty)].record.instance_state = .building;
    var complete = false;
    defer if (!complete) {
        ir.types.items[@intFromEnum(ty)].record.instance_state = .failed;
    };
    const bindings = try module.allocator.alloc(Binding, args.len);
    defer module.allocator.free(bindings);
    var iterator = proto.iterate(ast);
    for (args, bindings) |arg, *binding| binding.* = .{ .name = ast.tokenSlice(iterator.next().?.name_token.?), .ty = arg };
    var fields: std.ArrayList(Ir.Field) = .empty;
    defer fields.deinit(module.allocator);
    for (ast.fullStruct(full.init_node).?) |member| {
        if (ast.nodes.items(.tag)[member] != .container_field) continue;
        const data = ast.nodes.items(.data)[member];
        const field_ty = if (data.lhs == 0) try ir.internType(.void) else (try resolveWith(module, ir, data.lhs, bindings, env.expansions + 1)) orelse return null;
        const ft = ir.getType(field_ty).?;
        if (!Ir.isUnionPayload(ft)) return diagnose(module, member, .unsupported_type);
        const token = ast.nodes.items(.main_token)[member];
        const span = module.tokenSpan(token);
        try fields.append(module.allocator, .{ .name = ast.tokenSlice(token), .ty = field_ty, .span = .{ .start = span.start, .end = span.end } });
    }
    try ir.setRecordFields(ty, fields.items);
    ir.types.items[@intFromEnum(ty)].record.instance_state = .complete;
    complete = true;
    // Specialized declarations retain source ASTs but have distinct stable IDs.
    // Publish all siblings together before any signature or body is checked.
    const count = module.declarations.items.len;
    for (0..count) |i| {
        var method = module.declarations.items[i];
        if (method.container != id or method.concrete_type != null) continue;
        if (module.declarations.items.len == std.math.maxInt(u32)) return error.TooManyEntries;
        method.concrete_type = @intFromEnum(ty);
        method.specialization_depth = env.expansions + 1;
        try module.declarations.append(module.allocator, method);
    }
    return ty;
}

fn testTypeNode(module: *const Module, name: []const u8) Ast.Node.Index {
    return module.ast.decl(module.getDeclaration(module.lookup(name).?).?.node).ast.type_node;
}

fn testResolve(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "types.nov", source);
    defer module.deinit();
    var ir = Ir.init(allocator);
    defer ir.deinit();
    try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
    for (module.declarations.items) |declaration| {
        const node = module.ast.decl(declaration.node).ast.type_node;
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
    };
    for (module.declarations.items, expected) |declaration, ty| {
        const node = module.ast.decl(declaration.node).ast.type_node;
        const id = (try resolve(&module, &ir, node)).?;
        try std.testing.expectEqual(try ir.internType(ty), id);
        try std.testing.expectEqual(id, (try resolve(&module, &ir, node)).?);
    }
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
        id.* = (try resolve(&module, &ir, module.ast.decl(declaration.node).ast.type_node)).?;
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
        id.* = (try resolve(&module, &ir, module.ast.decl(declaration.node).ast.type_node)).?;
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
        .{ .spelling = "*i32", .tag = .unsupported_type, .span = "*" },
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
    for ([_][]const u8{ "float", "f16", "f32", "f64", "f80", "f128", "rune", "type" }) |name| {
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
    const first_name = module.tokenSpan(module.ast.fullFnProto(testTypeNode(&module, "value")).?.lparen + 1);
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
        .{ .source = "let value: (i32) -> float", .tag = .unsupported_type },
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
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testResolve, .{source});
    }
}
