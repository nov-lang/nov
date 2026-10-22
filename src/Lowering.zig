const std = @import("std");
const CEmitter = @import("CEmitter.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Ir = @import("Ir.zig");
const BodyChecker = @import("BodyChecker.zig");

pub const Error = Sema.Error;

/// Lowers a closed, bounded module: immutable literal globals and supported
/// expression-body functions. Owns the returned IR independently of the AST.
/// Arithmetic is represented, not evaluated; backend numeric policy is pending.
pub fn lower(module: *Module) Error!Sema {
    const diagnostics_start = module.diagnostics.items.len;
    errdefer module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
    var result = try Sema.analyzeFunctions(module);
    errdefer result.deinit();
    if (module.hasErrors()) return result;
    result.ir.source_len = @intCast(module.ast.source.len);
    for (module.declarations.items, result.entries) |declaration, entry| {
        if (entry.status == .type_alias or entry.status == .generic) continue;
        if (module.ast.decl(declaration.node).ast.is_mutable or (entry.status != .body_checked and entry.status != .constant_checked)) {
            const full = module.ast.decl(declaration.node).ast;
            const tag: Module.Diagnostic.Tag = if (!full.is_mutable and full.type_node == 0 and full.init_node != 0) blk: {
                const init_tag = module.ast.nodes.items(.tag)[full.init_node];
                break :blk if (init_tag == .number_literal or (init_tag == .negation and module.ast.nodes.items(.tag)[module.ast.nodes.items(.data)[full.init_node].rhs] == .number_literal)) .literal_needs_type else .unsupported_global_initializer;
            } else .unsupported_lowering;
            try module.diagnostics.append(module.allocator, .{ .tag = tag, .span = declaration.name_span });
        }
    }
    if (module.hasErrors()) return result;
    const types = try result.allocator.alloc(?Ir.TypeId, result.entries.len);
    defer result.allocator.free(types);
    const constants = try result.allocator.alloc(?Ir.ConstId, result.entries.len);
    defer result.allocator.free(constants);
    const functions = try result.allocator.alloc(?Ir.FunctionId, result.entries.len);
    defer result.allocator.free(functions);
    @memset(functions, null);
    for (module.declarations.items, result.entries, types, constants, 0..) |declaration, *entry, *ty, *constant, i| {
        ty.* = entry.type_id;
        constant.* = entry.constant;
        if (entry.status != .body_checked) continue;
        const span: Ir.Span = .{ .start = declaration.name_span.start, .end = declaration.name_span.end };
        const qualified = if (declaration.container) |owner| try std.fmt.allocPrint(result.allocator, "{s}.{s}", .{ module.getDeclaration(owner).?.name, declaration.name }) else null;
        defer if (qualified) |name| result.allocator.free(name);
        const function = try result.ir.addFunction(qualified orelse declaration.name, entry.type_id.?, span);
        result.ir.functions.items[@intFromEnum(function)].public = declaration.public;
        entry.function = function;
        functions[i] = function;
    }
    // Function identities exist before bodies, so calls may be forward/recursive.
    for (result.entries, 0..) |*entry, i| {
        if (entry.status != .body_checked) continue;
        entry.status = if (try BodyChecker.lower(module, &result.ir, types, constants, functions, @enumFromInt(i))) .lowered else .failed;
    }
    if (module.hasErrors()) {
        // Never expose dangling placeholders or a partly lowered program as ready.
        result.ir.clearFunctions();
        for (result.entries) |*entry| {
            entry.function = null;
            if (entry.status == .lowered) entry.status = .body_checked;
        }
    } else {
        try result.ir.verify();
        result.lowering_complete = true;
    }
    return result;
}

test "generic union instances canonicalize aliases arguments and nominal declarations" {
    const source = "let G:(T:type,E:type)->type=union{ok:T,err:E,}\n" ++
        "let Other:(T:type,E:type)->type=union{ok:T,err:E,}\n" ++
        "let Factory=Forward\nlet Forward=G\nlet Count=int\nlet A=Factory(Count,string)\nlet B=G(int,string)\nlet C=G(string,int)\nlet D=Other(int,string)\nlet Alias=A\n" ++
        "let id:(v:Alias)->B=v\nlet make:()->A=G(int,string).ok[7]";
    var module = try Module.init(std.testing.allocator, "instances.nov", source);
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    if (module.hasErrors()) std.debug.print("generic errors: {any}, parse: {any}\n", .{ module.diagnostics.items, module.ast.errors });
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const a = sema.getEntry(module.lookup("A").?).?.alias_type.?;
    try std.testing.expectEqual(a, sema.getEntry(module.lookup("B").?).?.alias_type.?);
    try std.testing.expectEqual(a, sema.getEntry(module.lookup("Alias").?).?.alias_type.?);
    try std.testing.expect(a != sema.getEntry(module.lookup("C").?).?.alias_type.?);
    try std.testing.expect(a != sema.getEntry(module.lookup("D").?).?.alias_type.?);
    var instances: usize = 0;
    for (sema.ir.types.items) |ty| if (ty == .record and ty.record.type_args.len != 0) {
        instances += 1;
    };
    try std.testing.expectEqual(@as(usize, 3), instances);
    try sema.ir.verify();
}

test "generic union expansion and template alias traversal have explicit limits" {
    for ([_]bool{ false, true }) |alias_chain| {
        var source = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer source.deinit();
        if (alias_chain) {
            try source.writer.writeAll("let G:(T:type)->type=union{ok:T,}\n");
            for (0..130) |i| try source.writer.print("let A{d}=A{d}\n", .{ i, i + 1 });
            try source.writer.writeAll("let A130=G\nlet R=A0(int)");
        } else {
            for (0..65) |i| try source.writer.print("let G{d}:(T:type)->type=union{{ok:G{d}(T),}}\n", .{ i, i + 1 });
            try source.writer.writeAll("let G65:(T:type)->type=union{ok:T,}\nlet R=G0(bool)");
        }
        var module = try Module.init(std.testing.allocator, "generic-limit.nov", source.written());
        defer module.deinit();
        var result = try lower(&module);
        defer result.deinit();
        var found = false;
        for (module.diagnostics.items) |d| if (d.tag == .generic_limit) {
            found = true;
        };
        if (!found) std.debug.print("limit diagnostics: {any}\n", .{module.diagnostics.items});
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
    }
}

test "generic unions and propagation diagnose exact types unsupported forms and cycles under OOM" {
    const prefix = "let G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet R=G(int,string)\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let A=G(int)", .tag = .generic_argument_count },
        .{ .source = "let A=G(int,string,bool)", .tag = .generic_argument_count },
        .{ .source = "let f:(v:G)={}", .tag = .generic_needs_arguments },
        .{ .source = "let A=G(true,string)", .tag = .not_a_type },
        .{ .source = "let A=G(Missing,string)", .tag = .unknown_type },
        .{ .source = "let Other:(T:type,E:type)->type=union{ok:T,err:E,}\nlet f:()->Other(int,string)=R.ok[1]", .tag = .expression_type_mismatch },
        .{ .source = "let f:()->R=7", .tag = .literal_type_mismatch },
        .{ .source = "let f:()->R={return 7}", .tag = .literal_type_mismatch },
        .{ .source = "let f:(r:R)->int=r.!", .tag = .invalid_propagation_return },
        .{ .source = "let f:(r:R)->R={return r.!}", .tag = .expression_type_mismatch },
        .{ .source = "let f:(r:G(int,i8))->G(bool,i16)=.ok[r.! == 1]", .tag = .propagation_error_mismatch },
        .{ .source = "let f:(r:R)->R=.ok[r.!.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let f:(r:R)->G(int,bool)=.ok[r.!]", .tag = .propagation_error_mismatch },
        .{ .source = "let f:()->R=.ok[true.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let U=union{ok:int,err:string,extra,}\nlet f:(u:U)->R=.ok[u.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let U=union{some:int,err:string,}\nlet f:(u:U)->R=.ok[u.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let f:(r:R)->G(bool,string)=.ok[r.!]", .tag = .expression_type_mismatch },
        .{ .source = "let Cycle:(T:type)->type=union{ok:Cycle(T),}\nlet X=Cycle(int)", .tag = .generic_cycle },
        .{ .source = "let Loop=G(Loop,string)", .tag = .type_alias_cycle },
        .{ .source = "let A=B\nlet B=A\nlet R2=G(A,string)", .tag = .type_alias_cycle },
        .{ .source = "let A=Missing\nlet R2=G(A,string)", .tag = .unknown_type },
        .{ .source = "let Apply=G\nlet A=B\nlet B=A\nlet f:()->R=Apply(A,string).ok[1]", .tag = .type_alias_cycle },
        .{ .source = "let Bad:(T:int)->type=union{ok:T,}", .tag = .invalid_generic_declaration },
        .{ .source = "let Bad:(T:type)->int=union{ok:T,}", .tag = .invalid_generic_declaration },
        .{ .source = "let Bad:(T:type,T:type)->type=union{ok:T,}", .tag = .duplicate_parameter },
        .{ .source = "let Bad:(T:type)->type=union{ok:T,\nlet m:(x:T)->T=x\n}", .tag = .unsupported_generic_method },
        .{ .source = "let Bad:(T:type)->type=union{ok:T,ok:T,}", .tag = .duplicate_field },
        .{ .source = "let Bad:(T:type)->type=union{ok:T=1,}", .tag = .unsupported_union_default },
        .{ .source = "let A=G((int)->int,string)", .tag = .unsupported_type },
        .{ .source = "let A=G([]int,string)", .tag = .unsupported_type },
        .{ .source = "let f:()={let x={let bad:G((int)->int,string)=R.ok[1]\nbad}\n}", .tag = .unsupported_type },
        .{ .source = "let U=union{ok:int,err:bool,}\nlet f:(u:U)->R=.ok[u.!]", .tag = .propagation_error_mismatch },
        .{ .source = "let int:(T:type)->type=union{ok:T,}", .tag = .unsupported_shadowing },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "generic-error.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var result = try lower(&module);
        defer result.deinit();
        var found = false;
        for (module.diagnostics.items) |d| if (d.tag == case.tag) {
            found = true;
        };
        if (!found) std.debug.print("generic case {s}: {any}\n", .{ case.source, module.diagnostics.items });
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
    }
}

test "unsupported union declarations and rejected matches roll back under OOM" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let U=union{}", .tag = .empty_union },
        .{ .source = "let U=union{ok:int=0,}", .tag = .unsupported_union_default },
        .{ .source = "let U=union{ok:int,ok:string,}", .tag = .duplicate_field },
        .{ .source = "let U=union{ok:[]int,}", .tag = .unsupported_type },
        .{ .source = "let R=struct{n:int,}\nlet U=union{ok:R,}", .tag = .unsupported_type },
        .{ .source = "let U=union{ok:U,}", .tag = .unsupported_type },
        .{ .source = "let U=union{ok:()->int,}", .tag = .unsupported_type },
        .{ .source = "let U=union{ok:int,\nlet constant:int=1\n}", .tag = .unsupported_declaration },
        .{ .source = "let U=union{ok:int,}\nlet f:()=@println(U.ok[1])", .tag = .invalid_to_string },
        .{ .source = "let U=union{ok:int,\nlet toString:(u:U)->bool=true\n}\nlet f:()=@println(U.ok[1])", .tag = .invalid_to_string },
        .{ .source = "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|n|n}", .tag = .match_missing_variants },
        .{ .source = "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>0\n.ok=>1\n_=>2}", .tag = .match_covered_union_prong },
        .{ .source = "let U=union{ok:int,none,}\nlet f:(u:U)->int=match u{.ok=>|n|n\n_=>n}", .tag = .unknown_name },
        .{ .source = "let U=union{ok:i8,}\nlet f:()->U=.ok[128]", .tag = .literal_out_of_range },
        .{ .source = "let U=union{ok:u128,}\nlet f:()->U=.ok[340282366920938463463374607431768211456]", .tag = .literal_out_of_range },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "union-errors.nov", case.source);
        defer module.deinit();
        var result = try lower(&module);
        defer result.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var found = false;
        for (module.diagnostics.items) |diagnostic| if (diagnostic.tag == case.tag) {
            found = true;
        };
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{case.source});
    }
}

test "nominal records reject mismatches malformed constructors defaults fields and printing signatures" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let A=struct{x:int,}\nlet B=struct{x:int,}\nlet f:(a:A)->B=a", .tag = .expression_type_mismatch },
        .{ .source = "let A=struct{x:int,}\nlet f:(a:A)=@println(a)", .tag = .invalid_to_string },
        .{ .source = "let A=struct{x:int,}\nlet toString:(a:A)->string=\"not a method\"\nlet f:(a:A)=@println(a)", .tag = .invalid_to_string },
        .{ .source = "let A=struct{x:int,\nlet toString:(b:B)->string=\"wrong nominal receiver\"\n}\nlet B=struct{x:int,}\nlet f:(a:A)=@println(a)", .tag = .invalid_to_string },
        .{ .source = "let A=struct{x:int,\nlet toString:(a:A)->bool=true\n}\nlet f:(a:A)=@println(a)", .tag = .invalid_to_string },
        .{ .source = "let A=struct{x:int,\nlet toString:()->string=\"bad\"\n}\nlet f:(a:A)=@println(a)", .tag = .invalid_to_string },
        .{ .source = "let A=struct{x:int,}\nlet B=struct{x:int,}\nlet f:(a:B)=@println(a)\nlet main:()=f(A[.x=1])", .tag = .invalid_to_string },
        .{ .source = "let A=struct{x:int,}\nlet f:()->A=A[]", .tag = .missing_field },
        .{ .source = "let A=struct{x:int,}\nlet f:()->A=A[.x=1,.x=2]", .tag = .duplicate_field },
        .{ .source = "let A=struct{x:int,}\nlet f:()->A=A[.y=1]", .tag = .unknown_field },
        .{ .source = "let A=struct{x:int,}\nlet f:()->A=A[.x=true]", .tag = .literal_type_mismatch },
        .{ .source = "let A=struct{x:i8=128,}", .tag = .literal_out_of_range },
        .{ .source = "let A=struct{x:int=1+2,}", .tag = .invalid_field_default },
        .{ .source = "let A=struct{x:string=\"{true}\",}", .tag = .invalid_field_default },
        .{ .source = "let A=struct{x:int,x:int,}", .tag = .duplicate_field },
        .{ .source = "let A=struct{x:[]int,}", .tag = .unsupported_type },
        .{ .source = "let A=struct{x:()->int,}", .tag = .unsupported_type },
        .{ .source = "let A=struct{x:A,}", .tag = .unsupported_type },
        .{ .source = "let A=struct{x:B,}\nlet B=struct{x:A,}", .tag = .unsupported_type },
        .{ .source = "let A=struct{x:int,}\nlet f:(a:A)={a.x=1}", .tag = .unsupported_assignment_target },
        .{ .source = "let A=struct{x:int,}\nlet f:(a:A)->int=a.y", .tag = .unknown_field },
        .{ .source = "let A=struct{x:int,}\nlet f:(a:[]A)= {}", .tag = .unsupported_type },
        .{ .source = "let A=struct{x:int=0,}\nlet a=A[]", .tag = .unsupported_global_initializer },
        .{ .source = "let A=struct{\nlet B=struct{x:int,}\n}", .tag = .unsupported_declaration },
        .{ .source = "let int=struct{}", .tag = .unsupported_shadowing },
        .{ .source = "let A=struct{\nlet constant:int=1\n}", .tag = .unsupported_declaration },
        .{ .source = "let A=struct{\nlet Alias=int\n}", .tag = .unsupported_declaration },
        .{ .source = "let A=struct{x:Cycle,}\nlet Cycle=Other\nlet Other=Cycle", .tag = .type_alias_cycle },
        .{ .source = "let A=struct{x:int=0,}\nlet B=struct{x:int=0,}\nlet f:(a:B)={}\nlet main:()=f(A[])", .tag = .expression_type_mismatch },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "record-error.nov", case.source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.ast.errors.len);
        var result = try lower(&module);
        defer result.deinit();
        try std.testing.expect(!result.lowering_complete);
        var found = false;
        for (module.diagnostics.items) |diagnostic| if (diagnostic.tag == case.tag) {
            found = true;
        };
        if (!found) std.debug.print("missing {t}: {s}\n", .{ case.tag, case.source });
        try std.testing.expect(found);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{case.source});
    }
}

test "record verifier rejects corrupt field counts indices types defaults spans and nominal identities" {
    var module = try Module.init(std.testing.allocator, "record-ir.nov", "let A=struct{x:int=0,}\nlet B=struct{x:int=0,}\nlet f:(a:A)->int=a.x\nlet g:()->B=B[]");
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(result.lowering_complete);
    const a = result.entries[0].alias_type.?;
    const b = result.entries[1].alias_type.?;
    try std.testing.expect(a != b);
    for (result.ir.functions.items) |function| {
        for (@constCast(function.body.?.instructions)) |*instruction| {
            const saved = instruction.*;
            defer instruction.* = saved;
            if (instruction.op == .field_read) {
                instruction.op.field_read.index = 99;
                try std.testing.expectError(error.InvalidOperation, result.ir.verify());
                instruction.* = saved;
                instruction.ty = try result.ir.internType(.boolean);
                try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
            } else if (instruction.op == .record_init) {
                instruction.op.record_init = &.{};
                try std.testing.expectError(error.InvalidOperation, result.ir.verify());
            }
        }
    }
    const fields = @constCast(result.ir.getType(a).?.record.fields);
    const saved = fields[0];
    fields[0].ty = b;
    try std.testing.expectError(error.InvalidArrayElement, result.ir.verify());
    fields[0] = saved;
    fields[0].default = @enumFromInt(999);
    try std.testing.expectError(error.ConstantTypeMismatch, result.ir.verify());
    fields[0] = saved;
    fields[0].span.end = std.math.maxInt(u32);
    try std.testing.expectError(error.InvalidTypeId, result.ir.verify());
    fields[0] = saved;
    try result.ir.verify();
}

fn testRejectedMatchAllocations(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "invalid-match.nov", source);
    defer module.deinit();
    var sema = lower(&module) catch |err| {
        if (err == error.OutOfMemory) try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(module.hasErrors());
    try std.testing.expect(!sema.lowering_complete);
    try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
}

test "rejected matches and pipes roll back diagnostics on allocation failure" {
    for ([_][]const u8{
        "let f: (n: int) -> string = \"{missing}\"",
        "let f: (n: int) -> string = \"{n:02}\"",
        "let f: (n: int) -> string = \"{n}\\xFF\"",
        "let f: (n: []int) -> string = \"{n}\"",
        "let f: (n: int) -> int = match n {capture => 1\n_ => 2}",
        "let f: (n: int) -> int = match n {0 => 1}",
        "let f: (n: int) -> int = match n {0 => {let private: int = 1\nprivate}\n_ => true}",
        "let f: (n: int) -> int = n\nlet main: () = true |> f |> @println",
    }) |source| try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
}

test "rejected boolean matches roll back diagnostics on allocation failure" {
    for ([_][]const u8{
        "let f: (b: bool) -> int = match b {}",
        "let f: (b: bool) -> int = match b {true => 1}",
        "let f: (b: bool) -> int = match b {false => 1}",
        "let f: (b: bool) -> int = match b {true => 1\ntrue | true => 2\nfalse => 3}",
        "let f: (b: bool) -> int = match b {true => 1\nfalse | 1 => 2}",
    }) |source| try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
}

test "rejected bitwise binaries roll back diagnostics on allocation failure" {
    for ([_][]const u8{
        "let f: () -> int = 1 & missing",
        "let f: (n: i8) -> i8 = n | 128",
        "let f: () -> bool = true ^ false",
    }) |source| try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
}

test "bitwise binary operands and spans remain owned after source release" {
    const source = "let f: (a: i8, b: i8) -> i8 = a & b ^ a | b";
    var sema = blk: {
        var module = try Module.init(std.testing.allocator, "bitwise.nov", source);
        defer module.deinit();
        var result = try lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    const body = sema.ir.functions.items[0].body.?;
    var count: usize = 0;
    for (body.instructions, 0..) |instruction, i| {
        if (instruction.op != .binary) continue;
        const binary = instruction.op.binary;
        try std.testing.expectEqual(([_]Ir.BinaryOp{ .bit_and, .bit_xor, .bit_or })[count], binary.op);
        try std.testing.expectEqualStrings(([_][]const u8{ "a & b", "a & b ^ a", "a & b ^ a | b" })[count], source[instruction.span.start..instruction.span.end]);
        try std.testing.expect(@intFromEnum(binary.lhs) < i);
        try std.testing.expect(@intFromEnum(binary.rhs) < i);
        try std.testing.expectEqual(instruction.ty, body.instructions[@intFromEnum(binary.lhs)].ty);
        try std.testing.expectEqual(instruction.ty, body.instructions[@intFromEnum(binary.rhs)].ty);
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), count);
}

test "interpolation chunks and ordered conversions own storage after source release" {
    var module = try Module.init(std.testing.allocator, "interpolation.nov", "let f: (n: int, s: string) -> string = \"é\\0{n}:{s}/{n}😀\"");
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    var conversions: usize = 0;
    var chunks: usize = 0;
    for (sema.ir.functions.items[0].body.?.instructions, 0..) |instruction, i| {
        if (instruction.op == .to_string) {
            try std.testing.expect(@intFromEnum(instruction.op.to_string) < i);
            conversions += 1;
        }
        if (instruction.op == .constant) {
            const bytes = sema.ir.getConstant(instruction.op.constant).?.value.string;
            try std.testing.expect(std.mem.eql(u8, bytes, "é\x00") or std.mem.eql(u8, bytes, ":") or std.mem.eql(u8, bytes, "/") or std.mem.eql(u8, bytes, "😀"));
            chunks += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 3), conversions);
    try std.testing.expectEqual(@as(usize, 4), chunks);
}

test "interpolated call IR owns storage after releasing source and fragment tokens" {
    var module = try Module.init(std.testing.allocator, "calls.nov", "let id: (s: string) -> string = s\nlet add: (a: int, b: int) -> int = a + b\nlet f: (n: int, s: string) -> string = \"é\\0{id(s)}:{add(n, add(1, 2))}😀\"");
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    const c = try CEmitter.emit(std.testing.allocator, &sema.ir, null);
    defer std.testing.allocator.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "nov_add") != null);
}

test "interpolation conversions cannot reference sibling private match values" {
    var module = try Module.init(std.testing.allocator, "private-interpolation.nov", "let f: (n: int) -> string = match n {0 => \"{n}\"\n_ => \"{n}\"}");
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[0].body.?;
    var first: ?Ir.ValueId = null;
    for (@constCast(body.instructions), 0..) |*instruction, i| {
        if (instruction.op != .to_string) continue;
        if (first) |sibling| {
            const saved = instruction.op.to_string;
            instruction.op.to_string = sibling;
            try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
            instruction.op.to_string = saved;
        } else first = @enumFromInt(i);
    }
    try std.testing.expect(first != null);
    try sema.ir.verify();
}

test "interpolation globals remain unsupported nonliteral initializers" {
    for ([_][]const u8{ "let n: int = 1\nlet text: string = \"{n}\"", "let text: string = {\"{true}\"}" }) |source| {
        var module = try Module.init(std.testing.allocator, "global.nov", source);
        defer module.deinit();
        var sema = try lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_global_interpolation, module.diagnostics.items[0].tag);
    }
}

test "match reuses branch verification for sibling privacy and terminal metadata" {
    var module = try Module.init(std.testing.allocator, "match.nov",
        \\let f: (n: int) -> int = match n {
        \\    0 => {let private: int = n + 1
        \\        private}
        \\    1 => n + 2
        \\    _ => n + 3
        \\}
        \\let g: (n: int) -> int = match n {
        \\    0 => {return 1}
        \\    _ => {return 2}
        \\}
    );
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[0].body.?;
    const merge = &@constCast(body.instructions)[@intFromEnum(body.result)];
    try std.testing.expect(merge.op == .branch);
    const saved = merge.op.branch;
    merge.op.branch.then_value = saved.else_value.?;
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    merge.op.branch = saved;
    merge.op.branch.else_value = saved.then_value;
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    merge.op.branch = saved;
    merge.op.branch.condition = saved.then_value;
    try std.testing.expectError(error.InvalidRegion, sema.ir.verify());
    merge.op.branch = saved;
    const terminal_body = sema.ir.functions.items[1].body.?;
    const terminal = &@constCast(terminal_body.instructions)[@intFromEnum(terminal_body.result)];
    try std.testing.expect(terminal.op.branch.returns);
    terminal.op.branch.returns = false;
    try std.testing.expectError(error.InvalidReturn, sema.ir.verify());
    terminal.op.branch.returns = true;
    try sema.ir.verify();
}

test "exhaustive boolean match keeps private branches and terminal metadata" {
    var module = try Module.init(std.testing.allocator, "boolean-match.nov",
        \\let f: (b: bool) -> int = match b {
        \\    true => {let private: int = 1
        \\        private}
        \\    false => 2
        \\}
        \\let g: (b: bool) -> int = match b {
        \\    true => {return 3}
        \\    false => {return 4}
        \\}
        \\let h: (b: bool) -> int = match b {true | false => 5}
    );
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[0].body.?;
    const merge = &@constCast(body.instructions)[@intFromEnum(body.result)];
    try std.testing.expect(merge.op == .branch);
    const saved = merge.op.branch;
    merge.op.branch.else_value = saved.then_value;
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    merge.op.branch = saved;
    merge.op.branch.then_value = saved.else_value.?;
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    merge.op.branch = saved;
    const terminal_body = sema.ir.functions.items[1].body.?;
    const terminal = &@constCast(terminal_body.instructions)[@intFromEnum(terminal_body.result)];
    try std.testing.expect(terminal.op.branch.returns);
    terminal.op.branch.returns = false;
    try std.testing.expectError(error.InvalidReturn, sema.ir.verify());
    terminal.op.branch.returns = true;
    // A universal alternative needs neither a synthetic fallback nor a branch.
    for (sema.ir.functions.items[2].body.?.instructions) |instruction| try std.testing.expect(instruction.op != .branch);
    try sema.ir.verify();
}

test "exhaustion controls belong to parent loops and IR outlives the source" {
    var module = try Module.init(std.testing.allocator, "else.nov",
        \\let f: () -> []int = loop : {} {
        \\    loop false {} else {break [7, 8]}
        \\}
        \\let g: () -> void = loop true : {} {
        \\    loop false {} else {continue}
        \\}
        \\let h: () -> string = loop false {} else {return "a" + "b"}
    );
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
    for (sema.ir.functions.items[0..2], 0..) |function, f| {
        const body = function.body.?;
        var outer: usize = 0;
        for (body.instructions, 0..) |instruction, i| {
            if (instruction.op == .loop) outer = i;
        }
        try std.testing.expect(body.instructions[outer].op == .loop);
        var controls: usize = 0;
        for (body.instructions, 0..) |instruction, i| {
            if (instruction.op != .loop_break and instruction.op != .loop_continue) continue;
            controls += 1;
            try std.testing.expectEqual(outer, Ir.enclosingLoop(body, i).?);
            try std.testing.expectEqual(f == 0, instruction.op == .loop_break);
        }
        try std.testing.expectEqual(@as(usize, 1), controls);
        // The completed inner loop is terminal in its parent's body, not the
        // owner of the control in its exhaustion region.
        const inner_id = body.instructions[outer].op.loop.body;
        const inner = body.instructions[@intFromEnum(inner_id)];
        try std.testing.expect(inner.op.loop.returns);
        try std.testing.expect(Ir.terminates(body, inner_id));
    }
    const body = sema.ir.functions.items[2].body.?;
    const index = @intFromEnum(body.result);
    try std.testing.expect(body.instructions[index].op.loop.returns);
    @constCast(body.instructions)[index].op.loop.returns = false;
    try std.testing.expectError(error.InvalidReturn, sema.ir.verify());
    @constCast(body.instructions)[index].op.loop.returns = true;
    try sema.ir.verify();
}

test "range snapshots private counter and iteration values retain verified ownership" {
    try testRangeOwnership(false);
}

test "inclusive range guarded continuation retains source-free ownership and privacy" {
    try testRangeOwnership(true);
}

fn testRangeOwnership(inclusive: bool) !void {
    var sema = owned: {
        const source = try std.mem.replaceOwned(u8, std.testing.allocator,
            \\let f: (a: i8, b: i8) -> void = {
            \\    loop i in a..b {let mut local: i8 = i
            \\        local += 1
            \\        if i == 0 {continue}
            \\    }
            \\}
        , "..", if (inclusive) "..=" else "..");
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "range.nov", source);
        defer module.deinit();
        var result = try lower(&module);
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(result.lowering_complete);
        break :owned result;
    };
    defer sema.deinit();
    try sema.ir.verify();
    const body = sema.ir.functions.items[0].body.?;
    const instructions = @constCast(body.instructions);
    var owner: usize = 0;
    for (instructions, 0..) |instruction, i| {
        if (instruction.op == .loop) owner = i;
        try std.testing.expect(instruction.span.end > instruction.span.start);
    }
    const loop = instructions[owner].op.loop;
    const counter = instructions[loop.body_start].op.local_load;
    try std.testing.expect(@intFromEnum(counter) < loop.condition_start);
    try std.testing.expect(instructions[@intFromEnum(counter)].op == .local_slot);
    const condition = @intFromEnum(if (inclusive) instructions[@intFromEnum(loop.condition)].op.short_circuit.rhs else loop.condition);
    const endpoint = instructions[condition].op.binary.rhs;
    try std.testing.expect(@intFromEnum(endpoint) < @intFromEnum(counter));
    try std.testing.expect(instructions[loop.body_start].op == .local_load);
    const private_current: Ir.ValueId = @enumFromInt(loop.body_start);
    // A body-private snapshot must not leak into a sibling continuation,
    // and a body-local slot cannot replace the enclosing counter identity.
    const continuation = loop.continuation_start.?;
    try std.testing.expectEqual(if (inclusive) Ir.BinaryOp.le else Ir.BinaryOp.lt, instructions[condition].op.binary.op);
    if (inclusive) {
        try std.testing.expectEqual(Ir.BinaryOp.eq, instructions[continuation + 1].op.binary.op);
        try std.testing.expect(instructions[continuation + 2].op == .constant);
        try std.testing.expect(instructions[continuation + 3].op == .local_store);
        try std.testing.expectEqual(Ir.BinaryOp.add, instructions[continuation + 5].op.binary.op);
        try std.testing.expect(instructions[continuation + 7].op == .branch);
        for (instructions) |instruction| try std.testing.expect(instruction.op != .loop_break);
    }
    const saved = instructions[continuation];
    instructions[continuation].op = .{ .unary = .{ .op = .negate, .operand = private_current } };
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    instructions[continuation] = saved;
    instructions[continuation].op.local_load = @enumFromInt(loop.body_start + 1);
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    instructions[continuation] = saved;
    instructions[condition].op.binary.rhs = @enumFromInt(owner);
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    instructions[condition].op.binary.rhs = endpoint;
    try sema.ir.verify();
}

test "range else regions keep index continuation and fallback values private" {
    for ([_][]const u8{ "..", "..=" }) |operator| {
        const source = try std.mem.replaceOwned(u8, std.testing.allocator,
            \\let f: (a: i8, b: i8) -> i8 = loop i in a..b {
            \\    if i == a {break i}
            \\} else {let fallback: i8 = 7
            \\    fallback}
        , "..", operator);
        defer std.testing.allocator.free(source);
        var sema = owned: {
            var module = try Module.init(std.testing.allocator, "range-else.nov", source);
            defer module.deinit();
            var result = try lower(&module);
            errdefer result.deinit();
            try std.testing.expect(!module.hasErrors());
            try std.testing.expect(result.lowering_complete);
            break :owned result;
        };
        defer sema.deinit();
        try sema.ir.verify();
        const body = sema.ir.functions.items[0].body.?;
        const instructions = @constCast(body.instructions);
        var owner: usize = 0;
        for (instructions, 0..) |instruction, i| if (instruction.op == .loop) {
            owner = i;
        };
        const saved = instructions[owner].op.loop;
        instructions[owner].op.loop.else_value = @enumFromInt(saved.body_start);
        try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
        instructions[owner].op.loop = saved;
        instructions[owner].op.loop.else_start = saved.body_start;
        try std.testing.expectError(error.InvalidRegion, sema.ir.verify());
        instructions[owner].op.loop = saved;
        try sema.ir.verify();
    }
}

test "loop continuation regions and value breaks outlive source storage" {
    var module = try Module.init(std.testing.allocator, "loop-values.nov",
        \\let f: (n: int) -> []int = {
        \\    let mut i: int = 0
        \\    let result = loop : {i += 1} {
        \\        if i < n {continue}
        \\        break [i, n]
        \\    }
        \\    result
        \\}
    );
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    const body = sema.ir.functions.items[0].body.?;
    var loop_owner: ?usize = null;
    var break_value: ?Ir.ValueId = null;
    for (body.instructions, 0..) |instruction, index| switch (instruction.op) {
        .loop => |r| {
            loop_owner = index;
            try std.testing.expect(r.unconditional);
            try std.testing.expect(r.condition_start < r.body_start);
            try std.testing.expect(r.body_start < r.continuation_start.?);
            try std.testing.expect(r.continuation_start.? < index);
            try std.testing.expect(sema.ir.getType(instruction.ty).? == .array);
            try std.testing.expectEqualStrings("loop : {i += 1}", module.ast.source[instruction.span.start .. instruction.span.start + 15]);
        },
        .loop_break => |value| break_value = value,
        else => {},
    };
    try std.testing.expect(loop_owner != null);
    try std.testing.expect(break_value != null);
    try std.testing.expectEqual(loop_owner.?, Ir.enclosingLoop(body, @intFromEnum(break_value.?) + 1).?);
    try std.testing.expect(body.instructions[@intFromEnum(break_value.?)].op == .array_clone);
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    try sema.ir.verify();
}

test "string compound reuses binary add with old load before RHS replacement" {
    var module = try Module.init(std.testing.allocator, "compound.nov",
        \\let rhs: () -> string = "right" + "!"
        \\let f: (s: string) -> string = {
        \\    let mut x = s
        \\    x += {x = "replacement"
        \\        rhs()}
        \\    x
        \\}
    );
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[1].body.?;
    var adds: usize = 0;
    var stores: usize = 0;
    for (body.instructions, 0..) |instruction, i| switch (instruction.op) {
        .local_store => stores += 1,
        .binary => |binary| {
            adds += 1;
            try std.testing.expectEqual(@as(usize, 1), stores);
            try std.testing.expectEqual(Ir.BinaryOp.add, binary.op);
            try std.testing.expect(sema.ir.getType(instruction.ty).? == .string);
            try std.testing.expect(body.instructions[@intFromEnum(binary.lhs)].op == .local_load);
            try std.testing.expect(body.instructions[@intFromEnum(binary.rhs)].op == .call);
            try std.testing.expect(@intFromEnum(binary.lhs) < @intFromEnum(binary.rhs));
            var replacement: ?usize = null;
            for (body.instructions[0..i], 0..) |prior, j| {
                if (prior.op == .local_store) replacement = j;
            }
            try std.testing.expect(@intFromEnum(binary.lhs) < replacement.?);
            try std.testing.expect(replacement.? < @intFromEnum(binary.rhs));
            try std.testing.expectEqual(@as(Ir.ValueId, @enumFromInt(i)), body.instructions[i + 1].op.local_store.value);
        },
        else => {},
    };
    try std.testing.expectEqual(@as(usize, 1), adds);
    try std.testing.expectEqual(@as(usize, 2), stores);
    try sema.ir.verify();
}

test "indexed compounds reuse one snapshot and index with old read before RHS" {
    var module = try Module.init(std.testing.allocator, "compound.nov",
        \\let index: () -> int = -1
        \\let rhs: () -> int = 3
        \\let main: () -> void = {let mut a: []int = [10, 20]
        \\    a[index()] += rhs()}
    );
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[2].body.?;
    var read: ?Ir.ValueId = null;
    var calls: usize = 0;
    var sets: usize = 0;
    for (body.instructions, 0..) |instruction, i| switch (instruction.op) {
        .call => {
            calls += 1;
            try std.testing.expectEqual(calls == 2, read != null);
        },
        .array_index => |r| {
            try std.testing.expect(read == null);
            try std.testing.expectEqual(@as(usize, 1), calls);
            try std.testing.expect(body.instructions[@intFromEnum(r.array)].op == .array_clone);
            try std.testing.expect(body.instructions[@intFromEnum(r.index)].op == .call);
            try std.testing.expect(@intFromEnum(r.array) < @intFromEnum(r.index));
            read = @enumFromInt(i);
        },
        .array_set => |r| {
            sets += 1;
            const old = body.instructions[@intFromEnum(read.?)].op.array_index;
            try std.testing.expectEqual(old.array, r.array);
            try std.testing.expectEqual(old.index, r.index);
            const binary = body.instructions[@intFromEnum(r.value)].op.binary;
            try std.testing.expectEqual(Ir.BinaryOp.add, binary.op);
            try std.testing.expectEqual(read.?, binary.lhs);
            try std.testing.expect(@intFromEnum(read.?) < @intFromEnum(binary.rhs));
            try std.testing.expectEqual(@as(usize, 2), calls);
            try std.testing.expect(body.instructions[i + 1].op == .local_store);
            try std.testing.expectEqual(@as(Ir.ValueId, @enumFromInt(i)), body.instructions[i + 1].op.local_store.value);
        },
        else => {},
    };
    try std.testing.expectEqual(@as(usize, 1), sets);
    try sema.ir.verify();
}

test "locals alias evaluated values and nested results survive scope exit" {
    var module = try Module.init(std.testing.allocator, "locals.nov",
        \\let id: (x: i32) -> i32 = x
        \\let f: () -> i32 = {
        \\    let callback: (i32) -> i32 = id
        \\    let x: i32 = callback(7)
        \\    let y: i32 = {let inner: i32 = x + x
        \\        inner}
        \\    @println(y)
        \\    y + x
        \\}
        \\let empty: () -> void = {let unused: i32 = id(9)
        \\}
    );
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[1].body.?;
    try std.testing.expectEqual(@as(usize, 6), body.instructions.len);
    try std.testing.expect(body.instructions[2].op == .call);
    const twice = body.instructions[3].op.binary;
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(twice.lhs));
    try std.testing.expectEqual(twice.lhs, twice.rhs);
    const sum = body.instructions[5].op.binary;
    try std.testing.expectEqual(@as(u32, 3), @intFromEnum(sum.lhs));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(sum.rhs));
    const empty = sema.ir.functions.items[2].body.?;
    try std.testing.expect(empty.instructions[2].op == .call);
    try std.testing.expect(sema.ir.getConstant(empty.instructions[3].op.constant).?.value == .void);
    module.deinit();
    try sema.ir.verify();
}

test "ascription aliases child IR values without casts or extra array boundaries" {
    var module = try Module.init(std.testing.allocator, "as.nov", "let f: (n: int) -> int = @as(int, @as(int, n))\nlet a: () -> []int = @as([]int, [1])\nlet fun: ((int) -> int) -> (int) -> int = @as((int) -> int, f)");
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    module.deinit();
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    const scalar = sema.ir.functions.items[0].body.?;
    try std.testing.expectEqual(@as(usize, 1), scalar.instructions.len);
    try std.testing.expect(scalar.instructions[0].op == .parameter);
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(scalar.result));
    var copies: usize = 0;
    for (sema.ir.functions.items[1].body.?.instructions) |instruction| {
        if (instruction.op == .array_clone) copies += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), copies);
    try sema.ir.verify();
    const c = try CEmitter.emit(std.testing.allocator, &sema.ir, null);
    defer std.testing.allocator.free(c);
    try std.testing.expect(std.mem.indexOf(u8, c, "typedef") != null);
}

test "printing lowers owned ordered arguments and operation spans" {
    var module = try Module.init(std.testing.allocator, "print.nov", "let id: (x: i32) -> i32 = x\nlet main: () -> void = @println(id(1), id(2), true)");
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[1].body.?;
    const instruction = body.instructions[body.instructions.len - 1];
    try std.testing.expect(instruction.op.print.newline);
    try std.testing.expectEqualStrings("@println(id(1), id(2), true)", module.ast.source[instruction.span.start..instruction.span.end]);
    const args = instruction.op.print.args;
    try std.testing.expectEqual(@as(usize, 3), args.len);
    try std.testing.expect(@intFromEnum(args[0]) < @intFromEnum(args[1]));
    try std.testing.expect(@intFromEnum(args[1]) < @intFromEnum(args[2]));
    try std.testing.expect(body.instructions[@intFromEnum(args[0])].op == .call);
    try std.testing.expect(body.instructions[@intFromEnum(args[1])].op == .call);
    try sema.ir.verify();
}

test "checked parameter arithmetic lowers into typed ordered values" {
    var module = try Module.init(std.testing.allocator, "add.nov", "let add: (a: i32, b: i32) -> i32 = a + b");
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqual(Sema.Status.lowered, sema.entries[0].status);
    const function = sema.ir.getFunction(sema.entries[0].function.?).?;
    try std.testing.expectEqualStrings("add", function.name);
    const body = function.body.?;
    try std.testing.expectEqual(@as(usize, 3), body.instructions.len);
    try std.testing.expectEqual(@as(u32, 0), body.instructions[0].op.parameter);
    try std.testing.expectEqual(@as(u32, 1), body.instructions[1].op.parameter);
    try std.testing.expectEqual(Ir.BinaryOp.add, body.instructions[2].op.binary.op);
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(body.instructions[2].op.binary.lhs));
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(body.instructions[2].op.binary.rhs));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(body.result));
    const span = body.instructions[2].span;
    try std.testing.expectEqualStrings("a + b", module.ast.source[span.start..span.end]);
    try sema.ir.verify();
}

test "callee then left-to-right arguments are explicit in instruction order" {
    var module = try Module.init(std.testing.allocator, "order.nov", "let base: i32 = 10\nlet f: (x: i32) -> i32 = x\nlet combine: (a: i32, b: i32) -> i32 = a + b\nlet main: () -> i32 = combine(f(base), f(2))");
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const function = sema.ir.getFunction(sema.getEntry(module.lookup("main").?).?.function.?).?;
    const body = function.body.?;
    var calls: [3]Ir.ValueId = undefined;
    var count: usize = 0;
    for (body.instructions, 0..) |instruction, i| {
        if (instruction.op != .call) continue;
        calls[count] = @enumFromInt(i);
        const callee = body.instructions[@intFromEnum(instruction.op.call.callee)].op.function_ref;
        try std.testing.expectEqualStrings(if (count < 2) "f" else "combine", sema.ir.getFunction(callee).?.name);
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), count);
    const outer = body.instructions[@intFromEnum(calls[2])].op.call;
    try std.testing.expectEqualSlices(Ir.ValueId, calls[0..2], outer.args);
    try std.testing.expect(@intFromEnum(outer.callee) < @intFromEnum(calls[0]));
    try sema.ir.verify();
}

test "forward recursive indirect and function-valued calls retain signatures" {
    var module = try Module.init(std.testing.allocator, "calls.nov", "let first: (x: i32) -> i32 = second(x)\nlet second: (x: i32) -> i32 = first(x)\nlet apply: (callback: (i32) -> i32, value: i32) -> i32 = callback(value)\nlet factory: () -> (i32) -> i32 = first\nlet main: () -> i32 = factory()(1)");
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(usize, 5), sema.ir.functions.items.len);
    for (sema.entries) |entry| try std.testing.expectEqual(Sema.Status.lowered, entry.status);
    const first = sema.ir.getFunction(sema.getEntry(module.lookup("first").?).?.function.?).?;
    try std.testing.expectEqual(sema.getEntry(module.lookup("second").?).?.function.?, first.body.?.instructions[0].op.function_ref);
    const apply = sema.ir.getFunction(sema.getEntry(module.lookup("apply").?).?.function.?).?;
    try std.testing.expectEqual(@as(u32, 0), apply.body.?.instructions[0].op.parameter);
    try sema.ir.verify();
}

test "unit comparisons and unary operations lower without arithmetic evaluation" {
    var module = try Module.init(std.testing.allocator, "ops.nov", "let empty: () = {}\nlet call: () = empty()\nlet less: (x: i32) -> bool = x < 1\nlet invert: (x: bool) -> bool = !x\nlet negate: (x: i32) -> i32 = -x\nlet bits: (x: u32) -> u32 = ~x\nlet overflow_pending: () -> i8 = 127 + 1");
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    const function = sema.ir.getFunction(sema.getEntry(module.lookup("overflow_pending").?).?.function.?).?;
    try std.testing.expectEqual(@as(usize, 3), function.body.?.instructions.len);
    try std.testing.expectEqual(Ir.BinaryOp.add, function.body.?.instructions[2].op.binary.op);
    try sema.ir.verify();
}

test "loop regions own repeated conditions bodies and spans after source destruction" {
    var module = try Module.init(std.testing.allocator, "loop.nov",
        \\let f: () -> void = {let mut n: int = 0
        \\    loop n < 2 {n += 1
        \\        if n == 1 {continue} else {break}}}
    );
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    const body = sema.ir.functions.items[0].body.?;
    const loop = body.instructions[@intFromEnum(body.result)];
    const region = loop.op.loop;
    try std.testing.expectEqualStrings("loop n < 2 {n += 1\n        if n == 1 {continue} else {break}}", module.ast.source[loop.span.start..loop.span.end]);
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expect(body.instructions[region.condition_start].op == .local_load);
    try std.testing.expect(Ir.terminates(body, region.body));
    try std.testing.expect(!Ir.terminates(body, body.result));
    try sema.ir.verify();
}

test "unconditional loops reuse verified regions with source bounded true constants" {
    var module = try Module.init(std.testing.allocator, "loop.nov", "let f: () -> void = loop {break}");
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    const body = sema.ir.functions.items[0].body.?;
    const loop = body.instructions[@intFromEnum(body.result)];
    const region = loop.op.loop;
    const condition = body.instructions[@intFromEnum(region.condition)];
    try std.testing.expectEqualStrings("loop {break}", module.ast.source[loop.span.start..loop.span.end]);
    try std.testing.expectEqual(loop.span, condition.span);
    try std.testing.expectEqual(true, sema.ir.constants.items[@intFromEnum(condition.op.constant)].value.boolean);
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqual(region.condition_start + 1, region.body_start);
    try std.testing.expect(!Ir.terminates(body, body.result));
    try sema.ir.verify();
}

test "return spans and owned terminal regions survive source destruction" {
    var module = try Module.init(std.testing.allocator, "return.nov",
        \\let f: (c: bool) -> int = if c {{return 42}} else {return 43}
        \\let unit: () -> void = return
    );
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    for (sema.ir.functions.items[0].body.?.instructions) |instruction| {
        if (instruction.op == .return_value) try std.testing.expect(std.mem.startsWith(u8, module.ast.source[instruction.span.start..instruction.span.end], "return 4"));
    }
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    const body = sema.ir.functions.items[0].body.?;
    try std.testing.expect(Ir.terminates(body, body.result));
    try std.testing.expect(body.instructions[@intFromEnum(body.result)].op.branch.returns);
    try std.testing.expect(sema.ir.functions.items[1].body.?.instructions[0].op.return_value == null);
    try sema.ir.verify();
}

test "if lowering owns branch ranges results and spans after source destruction" {
    var module = try Module.init(std.testing.allocator, "if.nov",
        \\let f: (c: bool, n: int) -> int = if c {n} else if c {1} else {2}
        \\let main: () -> void = if true {@println(f(false, 3))}
    );
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    defer sema.deinit();
    const failed = module.hasErrors();
    const body = sema.ir.functions.items[0].body.?;
    const merge = body.instructions[body.instructions.len - 1];
    const r = merge.op.branch;
    try std.testing.expectEqualStrings("if c {n} else if c {1} else {2}", module.ast.source[merge.span.start..merge.span.end]);
    module.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqual(@as(u32, 1), r.then_start);
    try std.testing.expectEqual(@as(u32, 2), r.else_start);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(r.then_value));
    try std.testing.expect(body.instructions[@intFromEnum(r.else_value.?)].op == .branch);
    const main_body = sema.ir.functions.items[1].body.?;
    const no_else = main_body.instructions[main_body.instructions.len - 1].op.branch;
    try std.testing.expect(no_else.else_value == null);
    try std.testing.expectEqual(main_body.instructions.len - 1, no_else.else_start);
    try sema.ir.verify();
}

test "short-circuit expressions lower explicit conditional RHS regions" {
    for ([_][]const u8{ "and", "or" }) |operator| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let side: () -> bool = true\nlet main: () -> bool = false {s} side()", .{operator});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "short.nov", source);
        defer module.deinit();
        var sema = try lower(&module);
        defer sema.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        const body = sema.ir.functions.items[1].body.?;
        const merge = body.instructions[body.instructions.len - 1];
        const region = merge.op.short_circuit;
        try std.testing.expectEqualStrings(operator, @tagName(region.kind));
        try std.testing.expectEqual(@as(u32, 1), region.rhs_start);
        try std.testing.expectEqual(@as(u32, 0), @intFromEnum(region.lhs));
        try std.testing.expect(body.instructions[@intFromEnum(region.rhs)].op == .call);
        try std.testing.expectEqualStrings(source[std.mem.indexOf(u8, source, "false").?..], module.ast.source[merge.span.start..merge.span.end]);
        try sema.ir.verify();
    }
}

test "inferred global constants and inline uses outlive source storage" {
    const source = "let text = @as(string, \"hé\\0\")\nlet n = @as(int, @as(int, -42))\nlet yes = true\nlet f: () -> string = text\nlet g: () -> int = n\nlet h: () -> bool = yes";
    var module = try Module.init(std.testing.allocator, "owned-global.nov", source);
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    const failed = module.hasErrors();
    module.deinit();
    defer sema.deinit();
    try std.testing.expect(!failed);
    try std.testing.expect(sema.lowering_complete);
    for (sema.ir.functions.items, 0..) |function, i| {
        try std.testing.expectEqual(@as(usize, 1), function.body.?.instructions.len);
        try std.testing.expectEqual(sema.entries[i].constant.?, function.body.?.instructions[0].op.constant);
    }
    try std.testing.expectEqualStrings("hé\x00", sema.ir.getConstant(sema.entries[0].constant.?).?.value.string);
    try sema.ir.verify();
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "inferred global boundary rejects computation blocks chains and runtime effects" {
    for ([_][]const u8{
        "let value: int = @as(int, 1 + 2)",
        "let value: int = @as(int, {1})",
        "let value: int = @as(int, missing())",
        "let value: int = @as(int, {@println(true)\n1})",
        "let value: int = @as(int, other)\nlet other: int = 1",
        "let value: []int = @as([]int, [1])",
        "let value: void = @as(void, {})",
        "let value: Fn = @as(Fn, f)\nlet Fn = () -> int\nlet f: () -> int = 1",
        "let value: uint = @as(int, 1)",
        "let value: uint = @as(uint, @as(int, 1))",
        "let value: string = @as(string, \"{true}\")",
        "let value = @as(int, 1 + 2)",
        "let value = @as(int, {1})",
        "let value = {true}",
        "let value = @as(void, {})",
        "let value = @as([]int, [1])",
        "let value = @as(int, missing())",
        "let value = @as(int, {@println(true)\n1})",
        "let value = @as(int, {return 1})",
        "let value = other\nlet other = true",
        "let f: () -> int = 1\nlet value = @as(() -> int, f)",
        "let mut value = true",
        "let mut value = @as(int, 1)",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "global-boundary.nov", source);
        defer module.deinit();
        var sema = try lower(&module);
        defer sema.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
        for (sema.entries) |entry| try std.testing.expect(entry.function == null);
        try sema.ir.verify();
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
    for ([_][]const u8{ "let n = 1", "let n = -1" }) |source| {
        var module = try Module.init(std.testing.allocator, "untyped-global.nov", source);
        defer module.deinit();
        var sema = try lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(Module.Diagnostic.Tag.literal_needs_type, module.diagnostics.items[0].tag);
        try std.testing.expect(sema.entries[0].type_id == null and sema.entries[0].constant == null);
        try std.testing.expect(!sema.lowering_complete);
    }
}

test "deferred and mutable globals cannot silently become executable constants" {
    for ([_][]const u8{
        "let value: i32 = 1 + 2\nlet f: () -> i32 = 1",
        "let mut value: i32 = 1\nlet f: () -> i32 = value",
        "let mut f: () -> i32 = 1",
        "let unit: void = {}\nlet f: () -> void = unit",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "global.nov", source);
        defer module.deinit();
        var sema = try lower(&module);
        defer sema.deinit();
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        try std.testing.expectEqual(Module.Diagnostic.Tag.unsupported_lowering, module.diagnostics.items[0].tag);
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
    }
}

test "lowered functions names and call arguments outlive source and scratch arenas" {
    var module = try Module.init(std.testing.allocator, "owned.nov", "let id: (x: i32) -> i32 = x\nlet main: () -> i32 = id(42)\nlet out: () -> void = @println(id(1), true)");
    var sema = lower(&module) catch |err| {
        module.deinit();
        return err;
    };
    module.deinit();
    defer sema.deinit();
    const function = sema.ir.functions.items[1];
    try std.testing.expectEqualStrings("main", function.name);
    try std.testing.expectEqual(@as(usize, 1), function.body.?.instructions[2].op.call.args.len);
    try sema.ir.verify();
}

test "grouped values alias child values with operation-origin spans" {
    var module = try Module.init(std.testing.allocator, "groups.nov", "let id: (x: i32) -> i32 = {x}\nlet factory: () -> (i32) -> i32 = {id}\nlet main: () -> i32 = factory()({1 + 2})\nlet empty: () = {{}}\nlet minimum: () -> i8 = -{{128}}");
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const id_body = sema.ir.getFunction(sema.getEntry(module.lookup("id").?).?.function.?).?.body.?;
    try std.testing.expectEqual(@as(usize, 1), id_body.instructions.len);
    const id_span = id_body.instructions[0].span;
    try std.testing.expectEqualStrings("x", module.ast.source[id_span.start..id_span.end]);
    const factory = sema.ir.getFunction(sema.getEntry(module.lookup("factory").?).?.function.?).?.body.?;
    try std.testing.expectEqual(sema.getEntry(module.lookup("id").?).?.function.?, factory.instructions[0].op.function_ref);
    const main = sema.ir.getFunction(sema.getEntry(module.lookup("main").?).?.function.?).?.body.?;
    try std.testing.expectEqual(@as(usize, 6), main.instructions.len);
    try std.testing.expectEqual(Ir.BinaryOp.add, main.instructions[4].op.binary.op);
    const add_span = main.instructions[4].span;
    try std.testing.expectEqualStrings("1 + 2", module.ast.source[add_span.start..add_span.end]);
    try std.testing.expectEqual(@as(u32, 4), @intFromEnum(main.instructions[5].op.call.args[0]));
    const empty = sema.ir.getFunction(sema.getEntry(module.lookup("empty").?).?.function.?).?.body.?;
    try std.testing.expectEqual(@as(usize, 1), empty.instructions.len);
    try std.testing.expect(sema.ir.getConstant(empty.instructions[0].op.constant).?.value == .void);
    const minimum = sema.ir.getFunction(sema.getEntry(module.lookup("minimum").?).?.function.?).?.body.?;
    const minimum_span = minimum.instructions[0].span;
    try std.testing.expectEqualStrings("-{{128}}", module.ast.source[minimum_span.start..minimum_span.end]);
    try sema.ir.verify();
}

test "successful empty lowering is distinct from source-error rollback" {
    var empty = try Module.init(std.testing.allocator, "empty.nov", "");
    defer empty.deinit();
    var success = try lower(&empty);
    defer success.deinit();
    try std.testing.expect(success.lowering_complete);
    var invalid = try Module.init(std.testing.allocator, "invalid.nov", "let f: (x: bool) -> i32 = x");
    defer invalid.deinit();
    var failure = try lower(&invalid);
    defer failure.deinit();
    try std.testing.expect(invalid.hasErrors());
    try std.testing.expect(!failure.lowering_complete);
    try std.testing.expectEqual(@as(usize, 0), failure.ir.functions.items.len);
}

fn testAllocationFailures(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "oom.nov", source);
    defer module.deinit();
    var sema = lower(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer sema.deinit();
    try sema.ir.verify();
}

test "lowering ownership and source diagnostic rollback survive allocation failures" {
    for ([_][]const u8{
        "",
        "let f: (a: i32, b: i32) -> i32 = a + b\nlet main: () -> i32 = f(1, f(2, 3))",
        "let first: () = {}\nlet last: () -> bool = true and false",
        "let id: (n: int) -> int = n\nlet f: (n: int) -> string = \"{id(id(n + 1))}:{n}\\0\"",
        "let f: (n: int) -> string = \"{n + missing}\"",
        "let f: (n: int) -> string = \"{f(n) n}\"",
        "let f: (n: int) -> string = \"{f(n + (1)}\"",
        "let f: (x: bool) -> bool = x and {let y: bool = x or false\n@println(y)\ny and x}\nlet main: () -> void = @println(f(true), false or f(false))",
        "let f: (x: bool) -> bool = if x {let y: bool = x or false\n@println(y)\ny} else if !x {true and x} else {false}\nlet main: () -> void = {if f(true) {@println(true)}\n}",
        "let f: () -> void = if false {missing} else {}",
        "let f: (c: bool) -> int = {if c {{return 1}}\nreturn 2}",
        "let f: () -> int = return false",
        "let f: () -> int = {return 1\n@println(true)}",
        "let f: () -> int = return",
        "let f: () -> bool = false and missing",
        "let id: (x: i32) -> i32 = x\nlet main: () -> void = @println(true, id(1), id(2))",
        "let main: () -> void = @println(true, missing, 1)",
        "let mut value: i32 = 1\nlet main: () -> i32 = value",
        "let id: (x: i32) -> i32 = x\nlet main: () -> void = {let x: i32 = id(1)\n@println({let y: i32 = x + x\ny})\n}",
        "let main: () -> void = {let value: i32 = 1\n{let value: i32 = 2\n}\n}",
        "let main: () -> void = {let x: i32 = 1\n@println(x)\nfalse and true\n}",
        "let main: () -> void = {let mut x: int = 1\nx += if true {2} else {3}\n@println(x)\n}",
        "let main: () -> void = {let x: int = 1\nx = 2}",
        "let main: () -> void = {let mut x: int = 1\nx = false}",
        "let main: () -> void = {let mut x: int = 1\nx.field = 2}",
        "let f: () -> void = {let mut n: int = 0\nloop n < 2 {let fresh: int = n\nn += 1\nloop false {continue}\nif fresh == 0 {continue} else {break}}}",
        "let f: () -> void = loop true {break 1}",
        "let f: () -> void = loop false {missing}",
        "let f: () -> void = loop true {{break}\n@println(true)}",
        "let f: (a: []int) -> []int = {let mut b: []int = a\nb += b\nb[-1] = 7\nb}",
        "let f: () -> void = {let mut a: []bool = [true]\na.len = 2}",
        "let f: () -> void = {let a: []int = [true]\n}",
        "let f: (c: bool) -> void = {let mut a: []int = [1]\na[{a = []\n0}] += if c {return} else {a = [9]\n2}\na[0] -= 1\na[0] *= 3\na[0] /= 2\na[0] %= 5}",
        "let f: () -> void = {let mut a: []bool = [true]\na[0] %= false}",
        "let f: (a: []i16) -> bool = -3 in a",
        "let f: (n: u8) -> bool = n in [1, 2]",
        "let f: () -> bool = true in []",
        "let f: () -> bool = 1 in [1, 2]",
        "let f: (n: i16, a: []i8) -> bool = n in a",
    }) |source| {
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}
