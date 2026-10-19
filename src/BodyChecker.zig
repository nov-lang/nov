const std = @import("std");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const Literal = @import("Literal.zig");
const isPrimitive = @import("primitives.zig").isPrimitive;

pub const Error = Ir.InternError;
const Known = struct { ty: Ir.TypeId, annotation: ?Module.Span = null };
const Binding = struct { known: Known };
// Bounds recursive checking/probing, not the iterative literal-grouping walk.
const max_recursive_depth = 128;

/// Checks one function against the complete annotation table. Does not evaluate
/// operations, choose overflow behavior, or claim other declarations are checked.
/// Requires the complete, valid annotation table produced by Sema/TypeResolver.
/// The caller owns rollback of diagnostics and IR allocations on failure.
pub fn check(module: *Module, ir: *Ir, types: []const ?Ir.TypeId, id: Module.DeclId) Error!bool {
    if (types.len != module.declarations.items.len) return error.InvalidTypeId;
    const declaration = module.getDeclaration(id) orelse return error.InvalidTypeId;
    const type_id = types[@intFromEnum(id)] orelse return false;
    const ty = ir.getType(type_id) orelse return error.InvalidTypeId;
    if (ty != .function) return error.InvalidTypeId;
    const full = module.ast.decl(declaration.node);
    const proto = module.ast.fullFnProto(full.ast.type_node) orelse return error.InvalidTypeId;
    if (proto.ast.params.len != ty.function.params.len) return error.InvalidTypeId;
    var context: Context = .{ .module = module, .ir = ir, .types = types };
    defer context.bindings.deinit(module.allocator);
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
            try context.bindings.put(module.allocator, name, .{ .known = .{
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
    return (try context.expression(full.ast.init_node, .{ .ty = ty.function.result, .annotation = annotation }, 0)) != null;
}

const Context = struct {
    module: *Module,
    ir: *Ir,
    types: []const ?Ir.TypeId,
    bindings: std.StringHashMapUnmanaged(Binding) = .empty,

    fn span(self: *Context, node: Ast.Node.Index) Module.Span {
        return self.module.tokenSpan(self.module.ast.nodes.items(.main_token)[node]);
    }

    fn fail(self: *Context, tag: Module.Diagnostic.Tag, node: Ast.Node.Index, related: ?Module.Span) Error!?Ir.TypeId {
        try self.module.diagnostics.append(self.module.allocator, .{ .tag = tag, .span = self.span(node), .related = related });
        return null;
    }

    fn fit(self: *Context, ty: Ir.TypeId, expected: ?Known, node: Ast.Node.Index) Error!?Ir.TypeId {
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
            .block, .block_two, .block_newline, .block_two_newline => blk: {
                var buffer: [2]Ast.Node.Index = undefined;
                const block = ast.fullBlock(&buffer, node).?;
                if (ast.firstToken(node) != block.ast.lbrace or block.ast.statements.len > 1) break :blk null;
                if (block.ast.statements.len == 0) break :blk .{ .ty = try self.ir.internType(.void) };
                break :blk try self.probe(block.ast.statements[0], depth + 1);
            },
            .negation, .bit_not => try self.probe(data.rhs, depth + 1),
            .add, .sub, .mul, .div, .mod => (try self.probe(data.lhs, depth + 1)) orelse try self.probe(data.rhs, depth + 1),
            .bool_not, .bool_and, .bool_or, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal => .{ .ty = try self.ir.internType(.boolean) },
            .call, .call_one => blk: {
                const call = ast.fullCall(node).?;
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
            .block, .block_two, .block_newline, .block_two_newline => {
                var buffer: [2]Ast.Node.Index = undefined;
                const block = ast.fullBlock(&buffer, node).?;
                if (ast.firstToken(node) == block.ast.lbrace and block.ast.statements.len == 1 and ast.nodes.items(.tag)[block.ast.statements[0]] != .decl) {
                    return self.validateUnconstrained(block.ast.statements[0], depth + 1);
                }
            },
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

    fn expression(self: *Context, node: Ast.Node.Index, expected: ?Known, depth: usize) Error!?Ir.TypeId {
        if (depth >= max_recursive_depth) return self.fail(.body_nesting_limit, node, null);
        const hint = expected orelse try self.probe(node, depth);
        if (hint) |known| {
            switch (try Literal.check(self.module, self.ir, node, known.ty, known.annotation)) {
                .constant => return known.ty,
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
            .number_literal => return self.fail(.literal_needs_type, node, null),
            .char_literal, .string_literal => return self.fail(.unsupported_literal, node, null),
            .block, .block_two, .block_newline, .block_two_newline => {
                var buffer: [2]Ast.Node.Index = undefined;
                const block = ast.fullBlock(&buffer, node).?;
                if (ast.firstToken(node) != block.ast.lbrace or block.ast.statements.len > 1) return self.fail(.unsupported_body_expression, node, null);
                if (block.ast.statements.len == 0) return self.fit(try self.ir.internType(.void), expected, node);
                if (ast.nodes.items(.tag)[block.ast.statements[0]] == .decl) return self.fail(.unsupported_body_expression, node, null);
                return self.expression(block.ast.statements[0], expected, depth + 1);
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
                if (try self.fit(boolean, expected, node) == null) return null;
                const operand: Known = .{ .ty = boolean, .annotation = if (expected) |want| want.annotation else null };
                const lhs = if (ast.nodes.items(.tag)[node] != .bool_not) try self.expression(data.lhs, operand, depth + 1) else boolean;
                const rhs = try self.expression(data.rhs, operand, depth + 1);
                if (lhs == null or rhs == null) return null;
                return boolean;
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
                const callee = (try self.expression(call.ast.fn_expr, null, depth + 1)) orelse return null;
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

test "local declarations control flow and shadowing stay explicitly unsupported" {
    for ([_][]const u8{
        "let f: () -> i32 = {\nlet local = 1\nlocal\n}",
        "let f: () -> i32 = label: {42}",
        "let f: (value: i32) -> i32 = {return value}",
        "let f: (value: i32) = {value += 1}",
        "let f: () -> i32 = {1\n2}",
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
    try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_body_expression, unsupported.diagnostics.items[0].tag);
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
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}
