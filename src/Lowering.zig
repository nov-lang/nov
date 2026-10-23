const std = @import("std");
const CEmitter = @import("CEmitter.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Ir = @import("Ir.zig");
const BodyChecker = @import("BodyChecker.zig");

const Compilation = @import("Compilation.zig");

pub const Error = Sema.Error;

/// Whole-graph lowering uses one semantic table and one set of IR pools.
/// The returned owner remains valid after the Compilation is destroyed.
pub fn lowerCompilation(comp: *Compilation) Error!Sema {
    const starts = try comp.allocator.alloc(usize, comp.modules.items.len);
    defer comp.allocator.free(starts);
    for (comp.modules.items, starts) |module, *start| start.* = module.diagnostics.items.len;
    errdefer for (comp.modules.items, starts) |module, start| module.diagnostics.shrinkRetainingCapacity(start);
    var result = try Sema.analyzeCompilation(comp);
    errdefer result.deinit();
    if (comp.hasErrors()) return result;
    for (comp.declarations.items, result.entries) |ref, entry| {
        const module = comp.getModule(ref.source).?;
        const declaration = module.getDeclaration(ref.local).?;
        if (entry.status == .namespace or entry.status == .type_alias or entry.status == .generic or entry.status == .unrequested) continue;
        if (module.source.ast.decl(declaration.node).ast.is_mutable or (entry.status != .body_checked and entry.status != .constant_checked)) {
            try module.diagnostics.append(comp.allocator, .{ .tag = deferredDiagnostic(module, declaration), .span = declaration.name_span });
        }
    }
    if (comp.hasErrors()) return result;
    const types = try comp.allocator.alloc(?Ir.TypeId, result.entries.len);
    defer comp.allocator.free(types);
    const constants = try comp.allocator.alloc(?Ir.ConstId, result.entries.len);
    defer comp.allocator.free(constants);
    const functions = try comp.allocator.alloc(?Ir.FunctionId, result.entries.len);
    defer comp.allocator.free(functions);
    @memset(functions, null);
    for (comp.declarations.items, result.entries, types, constants, 0..) |ref, *entry, *ty, *constant, i| {
        const module = comp.getModule(ref.source).?;
        const declaration = module.getDeclaration(ref.local).?;
        ty.* = entry.type_id;
        constant.* = entry.constant;
        if (entry.status != .body_checked) continue;
        const qualified = if (declaration.container) |owner| if (declaration.concrete_type) |concrete|
            try std.fmt.allocPrint(comp.allocator, "{s}[{d}].{s}", .{ module.getDeclaration(owner).?.name, concrete, declaration.name })
        else
            try std.fmt.allocPrint(comp.allocator, "{s}.{s}", .{ module.getDeclaration(owner).?.name, declaration.name }) else null;
        defer if (qualified) |name| comp.allocator.free(name);
        const function = try result.ir.addFunction(qualified orelse declaration.name, entry.type_id.?, declaration.name_span);
        result.ir.functions.items[@intFromEnum(function)].public = declaration.public;
        result.ir.functions.items[@intFromEnum(function)].noreturn = entry.noreturn;
        entry.function = function;
        functions[i] = function;
    }
    for (comp.declarations.items, result.entries) |ref, *entry| {
        if (entry.status != .body_checked) continue;
        entry.status = if (try BodyChecker.lower(comp.getModule(ref.source).?, &result.ir, types, constants, functions, ref.local)) .lowered else .failed;
    }
    if (comp.hasErrors()) {
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

/// Lowers a closed, bounded module: immutable literal globals and supported
/// expression-body functions. Owns the returned IR independently of the AST.
/// Arithmetic is represented, not evaluated; backend numeric policy is pending.
pub fn lower(module: *Module) Error!Sema {
    const diagnostics_start = module.diagnostics.items.len;
    errdefer module.diagnostics.shrinkRetainingCapacity(diagnostics_start);
    var result = try Sema.analyzeFunctions(module);
    errdefer result.deinit();
    if (module.hasErrors()) return result;
    result.ir.source_len = @intCast(module.source.ast.source.len);
    for (module.declarations.items, result.entries) |declaration, entry| {
        if (entry.status == .namespace or entry.status == .type_alias or entry.status == .generic or entry.status == .unrequested) continue;
        if (module.source.ast.decl(declaration.node).ast.is_mutable or (entry.status != .body_checked and entry.status != .constant_checked)) {
            try module.diagnostics.append(module.allocator, .{ .tag = deferredDiagnostic(module, declaration), .span = declaration.name_span });
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
        const span: Ir.Span = declaration.name_span;
        const qualified = if (declaration.container) |owner| if (declaration.concrete_type) |concrete|
            try std.fmt.allocPrint(result.allocator, "{s}[{d}].{s}", .{ module.getDeclaration(owner).?.name, concrete, declaration.name })
        else
            try std.fmt.allocPrint(result.allocator, "{s}.{s}", .{ module.getDeclaration(owner).?.name, declaration.name }) else null;
        defer if (qualified) |name| result.allocator.free(name);
        const function = try result.ir.addFunction(qualified orelse declaration.name, entry.type_id.?, span);
        result.ir.functions.items[@intFromEnum(function)].public = declaration.public;
        result.ir.functions.items[@intFromEnum(function)].noreturn = entry.noreturn;
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

fn deferredDiagnostic(module: *const Module, declaration: Module.Declaration) Module.Diagnostic.Tag {
    const full = module.source.ast.decl(declaration.node).ast;
    if (full.is_mutable or full.type_node != 0 or full.init_node == 0) return .unsupported_lowering;
    const ast = &module.source.ast;
    const tag = ast.nodes.items(.tag)[full.init_node];
    return if (tag == .number_literal or (tag == .negation and ast.nodes.items(.tag)[ast.nodes.items(.data)[full.init_node].rhs] == .number_literal)) .literal_needs_type else .unsupported_global_initializer;
}

const demand_result_source =
    \\@[public]
    \\let Result:(T:type,E:type)->type=union{ok:T,err:E,
    \\let Self=@This()
    \\@[public]
    \\let unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v
    \\.err=>fallback}
    \\@[public]
    \\let unwrapOrElse:(self:Self,fallback:(E)->T)->T=match self{.ok=>|v|v
    \\.err=>|e|fallback(e)}
    \\}
;

test "demand generic methods preserve void payload construction propagation and valid siblings" {
    const source = demand_result_source ++ "\n" ++
        \\let Unit=Result(void,string)
        \\let Empty=Result(int,void)
        \\let Both=Result(void,void)
        \\let unit:(b:bool)->Unit=if b{.ok[]}else{.err["err"]}
        \\let run:(b:bool)->Result(int,string)={let v:void=unit(b).!
        \\.ok[2]}
        \\let empty:()->Empty=.err[]
        \\let remap:()->Result(bool,void)=.ok[empty().! == 0]
        \\let both:()->Both=.ok[]
        \\let use:(r:Empty)->int=r.unwrapOr(7)
    ;
    var module = try Module.init(std.testing.allocator, "demand-void.nov", source);
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(result.lowering_complete);
    try std.testing.expect(!module.hasErrors());
    try std.testing.expectEqual(@as(usize, 7), result.ir.functions.items.len);
    try result.ir.verify();
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "demand generic methods do not resolve unused signatures bodies or expanding instances" {
    const source = "let G:(T:type)->type=union{ok:bool,\nlet bad:(x:Missing)->T=missing\nlet next:()->G((T)->T)=G((T)->T).next()\n}\nlet R=G(int)\nlet make:()->R=.ok[true]";
    var module = try Module.init(std.testing.allocator, "demand-unused.nov", source);
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(result.lowering_complete);
    try std.testing.expectEqual(@as(usize, 1), result.ir.functions.items.len);
    var instances: usize = 0;
    for (result.ir.types.items) |ty| if (ty == .record and ty.record.type_args.len != 0) {
        instances += 1;
    };
    try std.testing.expectEqual(@as(usize, 1), instances);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "demand generic methods diagnose requested void parameter signatures including function references" {
    for ([_][]const u8{
        "let R=Result(void,string)\nlet use:()={let f=R.unwrapOr\n}",
        "let R=Result(void,string)\nlet use:(r:R)=r.unwrapOr({})",
        "let R=Result(int,void)\nlet use:()={let f=R.unwrapOrElse\n}",
        "let R=Result(int,void)\nlet use:(r:R)={let f=match true{true=>R.unwrapOrElse\nfalse=>R.unwrapOrElse}\n}",
    }) |body| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ demand_result_source, "\n", body });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "demand-error.nov", source);
        defer module.deinit();
        var result = try lower(&module);
        defer result.deinit();
        var found = false;
        for (module.diagnostics.items) |d| if (d.tag == .invalid_parameter_type) {
            found = true;
        };
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
    }
}

test "generic methods reject inexact types leaked bindings and invalid concrete bodies" {
    const prefix = "let G:(T:type,E:type)->type=union{ok:T,err:E,\nlet Self=@This()\nlet unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v\n.err=>fallback}\nlet unwrapOrElse:(self:Self,fallback:(E)->T)->T=match self{.ok=>|v|v\n.err=>|e|fallback(e)}\n}\nlet R=G(int,string)\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:(r:R)->int=r.unwrapOr(true)", .tag = .literal_type_mismatch },
        .{ .source = "let f:(r:R)->int=r.unwrapOr()", .tag = .argument_count_mismatch },
        .{ .source = "let cb:(x:bool)->int=0\nlet f:(r:R)->int=r.unwrapOrElse(cb)", .tag = .expression_type_mismatch },
        .{ .source = "let S=G(int,bool)\nlet f:(s:S)->int=R.unwrapOr(s,0)", .tag = .expression_type_mismatch },
        .{ .source = "let Other:(T:type,E:type)->type=union{ok:T,err:E,}\nlet f:(s:Other(int,string))->int=R.unwrapOr(s,0)", .tag = .expression_type_mismatch },
        .{ .source = "let f:(x:T)={}", .tag = .unknown_type },
        .{ .source = "let f:(x:Self)={}", .tag = .unknown_type },
        .{ .source = "let Bad:(T:type)->type=union{ok:T,\nlet bad:()->T=true\n}\nlet B=Bad(int)\nlet use:()->int=B.bad()", .tag = .literal_type_mismatch },
        .{ .source = "let Bad:(T:type)->type=union{ok:T,\nlet m:(T:int)={}\n}\nlet B=Bad(int)\nlet use:()=B.m(0)", .tag = .unsupported_shadowing },
        .{ .source = "let Bad:(T:type)->type=union{ok:T,\nlet m:()={let T:int=1\n}\n}\nlet B=Bad(int)\nlet use:()=B.m()", .tag = .unsupported_shadowing },
        .{ .source = "let Bad:(T:type)->type=union{ok:T,\n@[noreturn]\nlet m:()->T=@panic(\"bad\")\n}\nlet B=Bad(int)\nlet use:()=B.m()", .tag = .noreturn_result },
        .{ .source = "let Bad:(T:type)->type=union{ok:T,\nlet toString:(self:@This())->T=match self{.ok=>|v|v}\n}\nlet B=Bad(int)\nlet f:(b:B)=@println(b)", .tag = .invalid_to_string },
        .{ .source = "let A=struct{x:int,}\nlet B=struct{x:int,}\nlet f:(r:G(A,string),b:B)->A=r.unwrapOr(b)", .tag = .expression_type_mismatch },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "generic-method-error.nov", source);
        defer module.deinit();
        if (module.hasErrors()) std.debug.print("invalid method case {s}: parse {any} collect {any}\n", .{ case.source, module.source.ast.errors, module.diagnostics.items });
        var result = try lower(&module);
        defer result.deinit();
        var found = false;
        for (module.diagnostics.items) |d| if (d.tag == case.tag) {
            found = true;
        };
        if (!found) std.debug.print("method error {s}: {any}\n", .{ case.source, module.diagnostics.items });
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
    }
}

test "generic method instantiation has a bounded expanding signature worklist" {
    var module = try Module.init(std.testing.allocator, "method-limit.nov", "let G:(T:type)->type=union{ok:bool,\nlet next:()->G((T)->T)=G((T)->T).next()\n}\nlet R=G(int)\nlet use:()={let x=R.next()\n}");
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(!result.lowering_complete);
    var found = false;
    for (module.diagnostics.items) |d| if (d.tag == .generic_limit) {
        found = true;
    };
    try std.testing.expect(found);
}

test "generic method bodies retain type parameters in annotations ascriptions and new instances" {
    const source =
        \\let G:(T:type,E:type)->type=union{ok:T,err:E,
        \\let Self=@This()
        \\let id:(self:Self,x:T)->T={let y:T=x
        \\@as(T,y)}
        \\let wrap:(self:Self,x:T)->Self=Self.ok[x]
        \\let flag:(self:Self)->G(bool,E)={let result:G(bool,E)=G(bool,E).ok[true]
        \\result}
        \\}
        \\let R=G(int,string)
        \\let S=G(string,int)
        \\let run:(r:R,s:S)->int={let flag=r.flag()
        \\@println(flag.id(true),s.wrap("s").id("t"))
        \\@println("{G(i8,bool).id(G(i8,bool).ok[1],2)}")
        \\G(int,string).id(r.wrap(1),2)}
    ;
    var module = try Module.init(std.testing.allocator, "method-scope.nov", source);
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    if (module.hasErrors()) std.debug.print("method scope: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "generic method tables reset between analyses without changing canonical identity" {
    var module = try Module.init(std.testing.allocator, "methods.nov", "let G:(T:type)->type=union{ok:T,\nlet get:(self:@This())->T=match self{.ok=>|v|v}\n}\nlet I=int\nlet A=G(I)\nlet B=G(int)\nlet C=G(bool)\nlet run:(a:A,b:B,c:C)->int={@println(c.get())\na.get()+B.get(b)}");
    defer module.deinit();
    var first = try lower(&module);
    defer first.deinit();
    try std.testing.expect(first.lowering_complete);
    const count = module.declarations.items.len;
    var second = try lower(&module);
    defer second.deinit();
    try std.testing.expect(second.lowering_complete);
    try std.testing.expectEqual(count, module.declarations.items.len);
    try std.testing.expectEqual(@as(usize, 3), second.ir.functions.items.len);
    try std.testing.expectEqual(first.getEntry(module.lookup("A").?).?.alias_type, first.getEntry(module.lookup("B").?).?.alias_type);
    try std.testing.expect(first.getEntry(module.lookup("A").?).?.alias_type != first.getEntry(module.lookup("C").?).?.alias_type);
    try first.ir.verify();
    try second.ir.verify();
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
    if (module.hasErrors()) std.debug.print("generic errors: {any}, parse: {any}\n", .{ module.diagnostics.items, module.source.ast.errors });
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

test "record payload generic identities canonicalize aliases but preserve nominal distinctions" {
    const source = "let G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet Factory=G\n" ++
        "let A=Factory(Alias,ErrorAlias)\nlet B=G(File,Error)\nlet C=G(Other,Error)\nlet D=G(File,Other)\n" ++
        "let Alias=File\nlet ErrorAlias=Error\nlet File=struct{s:string,}\nlet Other=struct{s:string,}\nlet Error=struct{s:string,}\n" ++
        "let id:(a:A)->B=a\nlet make:()->A=.ok[Alias[.s=\"value\"]]";
    var module = try Module.init(std.testing.allocator, "record-identities.nov", source);
    defer module.deinit();
    var sema = try lower(&module);
    defer sema.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const a = sema.getEntry(module.lookup("A").?).?.alias_type.?;
    try std.testing.expectEqual(a, sema.getEntry(module.lookup("B").?).?.alias_type.?);
    for ([_][]const u8{ "C", "D" }) |name| try std.testing.expect(a != sema.getEntry(module.lookup(name).?).?.alias_type.?);
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

test "Option propagation accepts structural families exact contexts inference and void under OOM" {
    const source =
        \\let O:(T:type)->type=union{none,some:T,}
        \\let A=union{some:u8,none,}
        \\let B=union{none:void,some:string,}
        \\let use:(n:u8)->u8=n
        \\let inferred:(o:A)->B={let n=o.!
        \\let sum=1+o.!
        \\let exact:u8=use(n+sum)
        \\.some["{exact}"]}
        \\let reverse:(o:O(u8))->A=.some[o.!]
        \\let unit:(o:O(void))->B={o.!
        \\.some["unit"]}
        \\let returned:(o:O(string))->B={return .some[o.!]}
    ;
    var module = try Module.init(std.testing.allocator, "option.nov", source);
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    if (module.hasErrors()) std.debug.print("option errors: {any}\n", .{module.diagnostics.items});
    try std.testing.expect(result.lowering_complete);
    try result.ir.verify();
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
}

test "Option propagation rejects malformed shapes cross families and implicit conversions under OOM" {
    const prefix = "let O:(T:type)->type=union{some:T,none,}\nlet R:(T:type,E:type)->type=union{ok:T,err:E,}\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:(o:O(int))->int=o.!", .tag = .invalid_propagation_return },
        .{ .source = "let f:(o:O(int))->O(int)=o.!", .tag = .expression_type_mismatch },
        .{ .source = "let f:(o:O(int))->O(uint)=.some[o.!]", .tag = .expression_type_mismatch },
        .{ .source = "let f:(o:O(int))->R(int,void)=.ok[o.!]", .tag = .invalid_propagation_return },
        .{ .source = "let f:(r:R(int,void))->O(int)=.some[r.!]", .tag = .invalid_propagation_return },
        .{ .source = "let U=union{some:int,none:int,}\nlet f:(u:U)->O(int)=.some[u.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let U=union{some:int,}\nlet f:(u:U)->O(int)=.some[u.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let U=union{some:int,none,extra,}\nlet f:(u:U)->O(int)=.some[u.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let U=union{ok:int,none,}\nlet f:(u:U)->O(int)=.some[u.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let U=union{some:int,err,}\nlet f:(u:U)->O(int)=.some[u.!]", .tag = .invalid_propagation_operand },
        .{ .source = "let U=union{some:int,none:bool,}\nlet f:(o:O(int))->U=.some[o.!]", .tag = .invalid_propagation_return },
        .{ .source = "let U=union{some:int,none,extra,}\nlet f:(o:O(int))->U=.some[o.!]", .tag = .invalid_propagation_return },
        .{ .source = "let A=struct{s:string,}\nlet B=struct{s:string,}\nlet f:(o:O(A))->O(B)=.some[o.!]", .tag = .expression_type_mismatch },
        .{ .source = "let A=struct{s:string,}\nlet B=struct{s:string,}\nlet f:(r:R(int,A))->R(int,B)=.ok[r.!]", .tag = .propagation_error_mismatch },
        .{ .source = "let f:(r:R(int,u8))->R(int,u16)=.ok[r.!]", .tag = .propagation_error_mismatch },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "option-error.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var result = try lower(&module);
        defer result.deinit();
        var found = false;
        for (module.diagnostics.items) |d| if (d.tag == case.tag) {
            found = true;
        };
        if (!found) std.debug.print("option error {s}: {any}\n", .{ case.source, module.diagnostics.items });
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
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
        .{ .source = "let Bad:(T:type)->type=union{ok:T,\nlet m=1\n}", .tag = .unsupported_generic_method },
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
        .{ .source = "let R=struct{n:[]int,}\nlet U=union{ok:R,}", .tag = .unsupported_type },
        .{ .source = "let R=struct{n:U,}\nlet U=union{ok:R,}", .tag = .unsupported_type },
        .{ .source = "let R=struct{n:R,}\nlet U=union{ok:R,}", .tag = .unsupported_type },
        .{ .source = "let R=struct{n:S,}\nlet S=struct{n:int,}\nlet U=union{ok:R,}", .tag = .unsupported_type },
        .{ .source = "let V=union{n:int,}\nlet U=union{ok:V,}", .tag = .unsupported_type },
        .{ .source = "let G:(T:type)->type=union{ok:T,}\nlet V=union{n:int,}\nlet U=G(V)", .tag = .unsupported_type },
        .{ .source = "let A=struct{n:int,}\nlet B=struct{n:int,}\nlet U=union{ok:A,}\nlet f:()->U=.ok[B[.n=1]]", .tag = .expression_type_mismatch },
        .{ .source = "let A=struct{n:int,}\nlet B=struct{n:int,}\nlet G:(T:type,E:type)->type=union{ok:T,err:E,}\nlet f:(r:G(int,A))->G(bool,B)=.ok[r.! == 0]", .tag = .propagation_error_mismatch },
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
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
            try std.testing.expectEqualStrings("loop : {i += 1}", module.source.ast.source[instruction.span.start .. instruction.span.start + 15]);
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
    try std.testing.expectEqualStrings("@println(id(1), id(2), true)", module.source.ast.source[instruction.span.start..instruction.span.end]);
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
    try std.testing.expectEqualStrings("a + b", module.source.ast.source[span.start..span.end]);
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
    try std.testing.expectEqualStrings("loop n < 2 {n += 1\n        if n == 1 {continue} else {break}}", module.source.ast.source[loop.span.start..loop.span.end]);
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
    try std.testing.expectEqualStrings("loop {break}", module.source.ast.source[loop.span.start..loop.span.end]);
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
        if (instruction.op == .return_value) try std.testing.expect(std.mem.startsWith(u8, module.source.ast.source[instruction.span.start..instruction.span.end], "return 4"));
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
    try std.testing.expectEqualStrings("if c {n} else if c {1} else {2}", module.source.ast.source[merge.span.start..merge.span.end]);
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
        try std.testing.expectEqualStrings(source[std.mem.indexOf(u8, source, "false").?..], module.source.ast.source[merge.span.start..merge.span.end]);
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
    try std.testing.expectEqualStrings("x", module.source.ast.source[id_span.start..id_span.end]);
    const factory = sema.ir.getFunction(sema.getEntry(module.lookup("factory").?).?.function.?).?.body.?;
    try std.testing.expectEqual(sema.getEntry(module.lookup("id").?).?.function.?, factory.instructions[0].op.function_ref);
    const main = sema.ir.getFunction(sema.getEntry(module.lookup("main").?).?.function.?).?.body.?;
    try std.testing.expectEqual(@as(usize, 6), main.instructions.len);
    try std.testing.expectEqual(Ir.BinaryOp.add, main.instructions[4].op.binary.op);
    const add_span = main.instructions[4].span;
    try std.testing.expectEqualStrings("1 + 2", module.source.ast.source[add_span.start..add_span.end]);
    try std.testing.expectEqual(@as(u32, 4), @intFromEnum(main.instructions[5].op.call.args[0]));
    const empty = sema.ir.getFunction(sema.getEntry(module.lookup("empty").?).?.function.?).?.body.?;
    try std.testing.expectEqual(@as(usize, 1), empty.instructions.len);
    try std.testing.expect(sema.ir.getConstant(empty.instructions[0].op.constant).?.value == .void);
    const minimum = sema.ir.getFunction(sema.getEntry(module.lookup("minimum").?).?.function.?).?.body.?;
    const minimum_span = minimum.instructions[0].span;
    try std.testing.expectEqualStrings("-{{128}}", module.source.ast.source[minimum_span.start..minimum_span.end]);
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

fn testImport(comp: *Compilation, source: @import("Source.zig").Id, name: []const u8, target: @import("Source.zig").Id, named: bool) !void {
    const module = comp.getModule(source).?;
    const local = module.lookup(name).?;
    try comp.imports.append(comp.allocator, .{ .declaration = comp.declaration_ids.get(.{ .source = source, .local = local }).?, .target = target, .named = named, .span = module.getDeclaration(local).?.name_span });
}

fn testImportProgram(allocator: std.mem.Allocator) !Sema {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/main.nov",
        \\let std=@import("std")
        \\let Result=std.Result
        \\let R=Result(string,string)
        \\let prefix="WRONG"
        \\let helper:()->string="WRONG"
        \\let main:()={let r:R=.ok["value"]
        \\    @println(r.unwrapOr("fallback"),r.label(),std.helper())}
    );
    const entry = try comp.addSource("/lib/std.nov",
        \\let result=@import("result.nov")
        \\@[public]
        \\let Result=result.Result
        \\@[public]
        \\let helper:()->string=result.helper()
        \\let main:()={}
    );
    const result = try comp.addSource("/lib/result.nov",
        \\let prefix="library:"
        \\let helper:()->string="helper"
        \\@[public]
        \\let Result:(T:type,E:type)->type=union{ok:T,err:E,
        \\let Self=@This()
        \\@[public]
        \\let unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v
        \\.err=>fallback}
        \\let label:(self:Self)->string=prefix+helper()
        \\}
    );
    try testImport(&comp, root, "std", entry, true);
    try testImport(&comp, entry, "result", result, false);
    comp.setRoot(root);
    var sema = try lowerCompilation(&comp);
    errdefer sema.deinit();
    if (comp.hasErrors()) {
        for (comp.modules.items) |module| for (module.diagnostics.items) |diagnostic| std.debug.print("{s}: {s}: {s}\n", .{ module.source.path, @tagName(diagnostic.tag), module.sourceSlice(diagnostic.span) orelse "?" });
    }
    try std.testing.expect(!comp.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    const main_entry = sema.getCompilationEntry(comp.root_main.?).?;
    try std.testing.expectEqual(root, sema.ir.getFunction(main_entry.function.?).?.span.source);
    try sema.ir.verify();
    return sema;
}

test "compilation imports lower reexported generic methods in defining scope and own source free IR" {
    var sema = try testImportProgram(std.testing.allocator);
    defer sema.deinit();
    try sema.ir.verify();
    const c = try CEmitter.emitProgramRoot(std.testing.allocator, &sema.ir, sema.entries[5].function, .{ .start = 0, .end = 0 }, null);
    defer std.testing.allocator.free(c);
    try std.testing.expect(c.len != 0);
}

test "compilation imports lower across every allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, struct {
        fn run(allocator: std.mem.Allocator) !void {
            var sema = try testImportProgram(allocator);
            defer sema.deinit();
        }
    }.run, .{});
}

test "compilation diamond aliases canonicalize while colliding record and enum locals remain nominal" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const root = try comp.addSource("/root.nov",
        \\let left=@import("left.nov")
        \\let right=@import("right.nov")
        \\let other=@import("other.nov")
        \\let A=left.Shared
        \\let B=right.Shared
        \\let C=other.Shared
        \\let E=left.Kind
        \\let F=other.Kind
        \\let main:()={let a:A=A[.value=true]
        \\    let b:B=a
        \\    @println(b.get(),left.answer(),other.answer())}
    );
    const left = try comp.addSource("/left.nov",
        \\let lib=@import("shared.nov")
        \\let Shared=lib.Shared
        \\let Kind=lib.Kind
        \\let answer:()->bool=lib.answer()
    );
    const right = try comp.addSource("/right.nov",
        \\let lib=@import("shared.nov")
        \\let Shared=lib.Shared
    );
    const shared = try comp.addSource("/shared.nov",
        \\let Shared=struct{value:bool,
        \\    let get:(self:Shared)->bool=self.value
        \\}
        \\let Kind=enum{one,two,}
        \\let answer:()->bool=true
    );
    const other = try comp.addSource("/other.nov",
        \\let Shared=struct{value:bool,
        \\    let get:(self:Shared)->bool=false
        \\}
        \\let Kind=enum{one,two,}
        \\let answer:()->bool=false
    );
    try testImport(&comp, root, "left", left, false);
    try testImport(&comp, root, "right", right, false);
    try testImport(&comp, root, "other", other, false);
    try testImport(&comp, left, "lib", shared, false);
    try testImport(&comp, right, "lib", shared, false);
    comp.setRoot(root);
    for (0..2) |_| {
        var sema = try lowerCompilation(&comp);
        defer sema.deinit();
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(sema.lowering_complete);
        const module = comp.getModule(root).?;
        const a = sema.entries[sema.entryIndex(module, module.lookup("A").?)].alias_type.?;
        const b = sema.entries[sema.entryIndex(module, module.lookup("B").?)].alias_type.?;
        const c = sema.entries[sema.entryIndex(module, module.lookup("C").?)].alias_type.?;
        try std.testing.expectEqual(a, b);
        try std.testing.expect(a != c);
        const e = sema.entries[sema.entryIndex(module, module.lookup("E").?)].alias_type.?;
        const f = sema.entries[sema.entryIndex(module, module.lookup("F").?)].alias_type.?;
        try std.testing.expect(e != f);
        try std.testing.expectEqual(sema.ir.getType(a).?.record.identity, sema.ir.getType(c).?.record.identity);
        try std.testing.expect(sema.ir.getType(a).?.record.source != sema.ir.getType(c).?.record.source);
    }
}

test "compilation named namespaces reject unexported names and diagnose imported bodies at their source" {
    for ([_]bool{ true, false }) |named| {
        var comp = Compilation.init(std.testing.allocator);
        defer comp.deinit();
        const root = try comp.addSource("/root.nov",
            \\let lib=@import("lib")
            \\let Alias=lib.Hidden
            \\let use:(x:Alias)->Alias=x
            \\let main:()={}
        );
        const lib = try comp.addSource("/lib.nov",
            \\let Hidden=struct{value:bool,}
            \\let broken:()->bool=missing
        );
        try testImport(&comp, root, "lib", lib, named);
        comp.setRoot(root);
        var sema = try lowerCompilation(&comp);
        defer sema.deinit();
        try std.testing.expect(comp.hasErrors());
        try std.testing.expect(!sema.lowering_complete);
        const module = comp.getModule(if (named) root else lib).?;
        try std.testing.expect(module.diagnostics.items.len != 0);
        try std.testing.expectEqual(if (named) root else lib, module.diagnostics.items[0].span.source);
    }
}

test "compilation namespace aliases direct generic applications and repeated diamonds share instances" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const root = try comp.addSource("/main.nov",
        \\let left=@import("left.nov")
        \\let right=@import("right.nov")
        \\let ns=left
        \\let A=ns.Result(bool,string)
        \\let B=right.Result(bool,string)
        \\let C=ns.types.Result(bool,string)
        \\let main:()={let a:A=left.Result(bool,string).ok[true]
        \\    let b:B=a
        \\    let c:C=b
        \\    @println(c.get(false))}
    );
    const left = try comp.addSource("/left.nov",
        \\let types=@import("types.nov")
        \\let Result=types.Result
    );
    const right = try comp.addSource("/right.nov",
        \\let types=@import("types.nov")
        \\let Result=types.Result
    );
    const types = try comp.addSource("/types.nov",
        \\let Result:(T:type,E:type)->type=union{ok:T,err:E,
        \\let Self=@This()
        \\let get:(self:Self,fallback:T)->T=match self{.ok=>|v|v
        \\.err=>fallback}
        \\}
    );
    try testImport(&comp, root, "left", left, false);
    try testImport(&comp, root, "right", right, false);
    try testImport(&comp, left, "types", types, false);
    try testImport(&comp, right, "types", types, false);
    comp.setRoot(root);
    var previous: ?Ir.TypeId = null;
    for (0..2) |_| {
        var sema = try lowerCompilation(&comp);
        defer sema.deinit();
        if (comp.hasErrors()) for (comp.modules.items) |m| for (m.diagnostics.items) |d| std.debug.print("{s}: {s}: {s}", .{ m.source.path, @tagName(d.tag), m.sourceSlice(d.span) orelse "?" });
        try std.testing.expect(!comp.hasErrors());
        const module = comp.getModule(root).?;
        const a = sema.entries[sema.entryIndex(module, module.lookup("A").?)].alias_type.?;
        for ([_][]const u8{ "B", "C" }) |name| try std.testing.expectEqual(a, sema.entries[sema.entryIndex(module, module.lookup(name).?)].alias_type.?);
        if (previous) |id| try std.testing.expectEqual(id, a);
        previous = a;
        var instances: usize = 0;
        for (sema.ir.types.items) |ty| if (ty == .record and ty.record.source == types) {
            instances += 1;
        };
        try std.testing.expectEqual(@as(usize, 1), instances);
    }
}

test "compilation in memory cycles reject before semantic analysis without namespace injection" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const root = try comp.addSource("/main.nov", "let other=@import(\"other.nov\")");
    const other = try comp.addSource("/other.nov", "let root=@import(\"main.nov\")");
    try testImport(&comp, root, "other", other, false);
    try testImport(&comp, other, "root", root, false);
    comp.setRoot(root);
    var sema = try lowerCompilation(&comp);
    defer sema.deinit();
    try std.testing.expect(comp.hasErrors());
    try std.testing.expectEqual(.import_cycle, comp.diagnostics.items[0].tag);
    try std.testing.expect(!sema.lowering_complete);
    try std.testing.expectEqual(@as(usize, 0), sema.ir.functions.items.len);
}

fn testCompilationGlobalDiagnostic(allocator: std.mem.Allocator, source: []const u8, expected: Module.Diagnostic.Tag, imported: bool) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/app/main.nov", if (imported) "let lib=@import(\"helper.nov\")" else source);
    const target = if (imported) try comp.addSource("/app/helper.nov", source) else root;
    if (imported) try testImport(&comp, root, "lib", target, false);
    comp.setRoot(root);
    var result = lowerCompilation(&comp) catch |err| {
        for (comp.modules.items) |module| try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer result.deinit();
    const module = comp.getModule(target).?;
    try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
    try std.testing.expectEqual(expected, module.diagnostics.items[0].tag);
    try std.testing.expectEqual(target, module.diagnostics.items[0].span.source);
    if (module.diagnostics.items[0].related) |note| try std.testing.expectEqual(target, note.source);
    try std.testing.expect(!result.lowering_complete);
    // Match the legacy CLI pipeline, including dependency diagnostics and spans.
    var standalone = try Module.init(allocator, "/standalone.nov", source);
    defer standalone.deinit();
    var dependencies = try @import("Dependencies.zig").analyze(&standalone);
    defer dependencies.deinit();
    if (!standalone.hasErrors()) {
        var lowered = try lower(&standalone);
        defer lowered.deinit();
    }
    const actual = module.diagnostics.items[0];
    const legacy = standalone.diagnostics.items[0];
    try std.testing.expectEqual(legacy.tag, actual.tag);
    try std.testing.expectEqual(legacy.span.start, actual.span.start);
    try std.testing.expectEqual(legacy.span.end, actual.span.end);
    try std.testing.expectEqual(legacy.related != null, actual.related != null);
    if (legacy.related) |note| {
        try std.testing.expectEqual(note.start, actual.related.?.start);
        try std.testing.expectEqual(note.end, actual.related.?.end);
    }
}

test "compilation deferred globals preserve standalone diagnostics root and imported source spans under OOM" {
    const cases = [_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let n=1", .tag = .literal_needs_type },
        .{ .source = "let n=-1", .tag = .literal_needs_type },
        .{ .source = "let n=@as(int,missing)", .tag = .unknown_name },
        .{ .source = "let n=@as(int,missing())", .tag = .unsupported_dependency_expression },
        .{ .source = "let n=@as(int,{1})", .tag = .unsupported_global_initializer },
        .{ .source = "let n=absent", .tag = .unknown_name },
        .{ .source = "let x=y\nlet y=x", .tag = .value_cycle },
        .{ .source = "let mut n:int=1", .tag = .unsupported_lowering },
        .{ .source = "let n={let x=1\nx}", .tag = .unsupported_dependency_expression },
    };
    for (cases) |case| for ([_]bool{ false, true }) |imported| {
        try testCompilationGlobalDiagnostic(std.testing.allocator, case.source, case.tag, imported);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testCompilationGlobalDiagnostic, .{ case.source, case.tag, imported });
    };
}

fn testQualifiedEnumGlobal(allocator: std.mem.Allocator, source: []const u8, expected: ?Module.Diagnostic.Tag) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    _ = try comp.addSource("/unrelated.nov", "let marker=true");
    const root = try comp.addSource("/app/main.nov", source);
    const api = try comp.addSource("/lib/api.nov",
        \\let enums=@import("enums.nov")
        \\@[public]
        \\let E=enums.E
        \\@[public]
        \\let Other=enums.Other
        \\@[public]
        \\let value=true
        \\let hidden=true
    );
    const enums = try comp.addSource("/lib/enums.nov",
        \\let E=enum{a,b,
        \\let unmarked:(self:E)->bool=true
        \\@[public]
        \\let published:(self:E)->bool=false
        \\}
        \\let Other=enum{a,b,}
    );
    try testImport(&comp, root, "lib", api, true);
    try testImport(&comp, api, "enums", enums, false);
    comp.setRoot(root);
    var result = lowerCompilation(&comp) catch |err| {
        for (comp.modules.items) |module| try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer result.deinit();
    const module = comp.getModule(root).?;
    if (expected) |tag| {
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
        const diagnostic = module.diagnostics.items[0];
        try std.testing.expectEqual(tag, diagnostic.tag);
        try std.testing.expectEqual(root, diagnostic.span.source);
        try std.testing.expect(module.sourceSlice(diagnostic.span) != null);
        if (tag == .literal_type_mismatch) {
            try std.testing.expectEqual(root, diagnostic.related.?.source);
            try std.testing.expectEqualStrings("lib", module.sourceSlice(diagnostic.related.?).?);
        }
        if (tag == .unknown_field) try std.testing.expectEqualStrings("missing", module.sourceSlice(diagnostic.span).?);
    } else {
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(result.lowering_complete);
        try result.ir.verify();
        for (module.declarations.items, 0..) |decl, i| {
            if (!std.mem.eql(u8, decl.name, "x")) continue;
            const entry = result.getCompilationEntry(comp.declaration_ids.get(.{ .source = root, .local = @enumFromInt(i) }).?).?;
            try std.testing.expectEqual(Sema.Status.constant_checked, entry.status);
            const nominal = result.ir.getType(entry.type_id.?).?.enumeration;
            try std.testing.expectEqual(enums, nominal.source);
            try std.testing.expectEqual(@as(u32, 0), nominal.identity);
        }
    }
    // Named namespaces gate top-level exports. Existing nominal method lookup
    // retains public metadata but does not enforce a separate access policy.
    const namespace = comp.declaration_ids.get(.{ .source = root, .local = module.lookup("lib").? }).?;
    try std.testing.expect(comp.lookupNamespace(namespace, "hidden") == null);
    const defining = comp.getModule(enums).?;
    const owner = defining.lookup("E").?;
    try std.testing.expect(!defining.getDeclaration(defining.lookupMethod(owner, "unmarked").?).?.public);
    try std.testing.expect(defining.getDeclaration(defining.lookupMethod(owner, "published").?).?.public);
}

test "namespace qualified enum globals support annotations inference aliases reexports and exact nominal diagnostics under OOM" {
    const cases = [_]struct { source: []const u8, expected: ?Module.Diagnostic.Tag = null }{
        .{ .source =
        \\let lib=@import("api")
        \\let x:lib.E=lib.E.a
        \\let main:()=@println(x==lib.E.a)
        },
        .{ .source =
        \\let lib=@import("api")
        \\let x=lib.E.a
        \\let main:()=@println(x==lib.E.a)
        },
        .{ .source =
        \\let lib=@import("api")
        \\let ns=lib
        \\let Alias=lib.E
        \\let x:Alias=ns.E.b
        \\let main:()=@println(x==Alias.b)
        },
        .{ .source =
        \\let lib=@import("api")
        \\let Alias=lib.E
        \\let x=Alias.a
        \\let main:()=@println(x==lib.E.a)
        },
        .{ .source =
        \\let lib=@import("api")
        \\let x:lib.E=lib.Other.a
        , .expected = .literal_type_mismatch },
        .{ .source =
        \\let lib=@import("api")
        \\let x=lib.E.missing
        , .expected = .unknown_field },
        .{ .source =
        \\let lib=@import("api")
        \\let x=lib.value.flag
        , .expected = .unsupported_dependency_expression },
        .{ .source =
        \\let lib=@import("api")
        \\let x=lib.E.unmarked
        , .expected = .unsupported_global_initializer },
    };
    for (cases) |case| {
        try testQualifiedEnumGlobal(std.testing.allocator, case.source, case.expected);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testQualifiedEnumGlobal, .{ case.source, case.expected });
    }
}

test "named namespace exported enum methods retain current unmarked and public access behavior" {
    try testQualifiedEnumGlobal(std.testing.allocator,
        \\let lib=@import("api")
        \\let x:lib.E=lib.E.a
        \\let main:()=@println(x.unmarked(),lib.E.unmarked(x),x.published(),lib.E.published(x))
    , null);
}
