const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const CEmitter = @import("CEmitter.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Ir = @import("Ir.zig");
const BodyChecker = @import("BodyChecker.zig");

const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");
const Dependencies = @import("Dependencies.zig");

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
        if (entry.status == .ignored or entry.status == .namespace or entry.status == .type_alias or entry.status == .generic or entry.status == .unrequested) continue;
        if (module.source.ast.decl(declaration.node).ast.is_mutable or (entry.status != .body_checked and entry.status != .constant_checked and entry.status != .external)) {
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
        if (entry.status != .body_checked and entry.status != .external) continue;
        const qualified = if (declaration.container) |owner| if (declaration.concrete_type) |concrete|
            try std.fmt.allocPrint(comp.allocator, "{s}[{d}].{s}", .{ module.getDeclaration(owner).?.name, concrete, declaration.name })
        else
            try std.fmt.allocPrint(comp.allocator, "{s}.{s}", .{ module.getDeclaration(owner).?.name, declaration.name }) else null;
        defer if (qualified) |name| comp.allocator.free(name);
        const function = try result.ir.addFunction(qualified orelse declaration.name, entry.type_id.?, declaration.name_span);
        result.ir.functions.items[@backingInt(function)].is_test = declaration.is_test;
        result.ir.functions.items[@backingInt(function)].public = declaration.public;
        result.ir.functions.items[@backingInt(function)].noreturn = entry.noreturn;
        if (declaration.export_name) |name| try result.ir.setExportName(function, name);
        if (declaration.extern_name) |name| try result.ir.setExternName(function, name);
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
        if (entry.status == .ignored or entry.status == .namespace or entry.status == .type_alias or entry.status == .generic or entry.status == .unrequested) continue;
        if (module.source.ast.decl(declaration.node).ast.is_mutable or (entry.status != .body_checked and entry.status != .constant_checked and entry.status != .external)) {
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
        if (entry.status != .body_checked and entry.status != .external) continue;
        const span: Ir.Span = declaration.name_span;
        const qualified = if (declaration.container) |owner| if (declaration.concrete_type) |concrete|
            try std.fmt.allocPrint(result.allocator, "{s}[{d}].{s}", .{ module.getDeclaration(owner).?.name, concrete, declaration.name })
        else
            try std.fmt.allocPrint(result.allocator, "{s}.{s}", .{ module.getDeclaration(owner).?.name, declaration.name }) else null;
        defer if (qualified) |name| result.allocator.free(name);
        const function = try result.ir.addFunction(qualified orelse declaration.name, entry.type_id.?, span);
        result.ir.functions.items[@backingInt(function)].is_test = declaration.is_test;
        result.ir.functions.items[@backingInt(function)].public = declaration.public;
        result.ir.functions.items[@backingInt(function)].noreturn = entry.noreturn;
        if (declaration.export_name) |name| try result.ir.setExportName(function, name);
        if (declaration.extern_name) |name| try result.ir.setExternName(function, name);
        entry.function = function;
        functions[i] = function;
    }
    // Function identities exist before bodies, so calls may be forward/recursive.
    for (result.entries, 0..) |*entry, i| {
        if (entry.status != .body_checked) continue;
        entry.status = if (try BodyChecker.lower(module, &result.ir, types, constants, functions, @fromBackingInt(@intCast(i)))) .lowered else .failed;
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

fn testExportOwnership(allocator: std.mem.Allocator) !void {
    var sema = blk: {
        var module = try Module.init(allocator, "exports.nov", "@[export]\nlet add: (x: i32) -> i32 = x\n@[public]\n@[export(\"nov_\\x61dd\")]\nlet visible: () = {}\n@[public]\nlet internal: () = {}");
        defer module.deinit();
        var result = lower(&module) catch |err| {
            try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqualStrings("add", sema.ir.functions.items[0].export_name.?);
    try std.testing.expect(!sema.ir.functions.items[0].public);
    try std.testing.expectEqualStrings("nov_add", sema.ir.functions.items[1].export_name.?);
    try std.testing.expect(sema.ir.functions.items[1].public);
    try std.testing.expect(sema.ir.functions.items[2].export_name == null);
    try sema.ir.verify();
}

test "export lowering owns decoded names after source release and every allocation failure" {
    try testExportOwnership(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testExportOwnership, .{});
}

fn testCompilationExportOwnership(allocator: std.mem.Allocator) !void {
    var sema = blk: {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/app/root.nov", "let imported = @import(\"lib.nov\")\nlet main: () = {}");
        const imported = try comp.addSource("/app/lib.nov", "@[export(\"external_symbol\")]\nlet hidden: (x: uint) -> uint = x");
        try testImport(&comp, root, "imported", imported, false);
        var result = lowerCompilation(&comp) catch |err| {
            for (comp.modules.items) |module| try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        errdefer result.deinit();
        try std.testing.expect(!comp.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    var found = false;
    for (sema.ir.functions.items) |function| if (function.export_name) |name| {
        try std.testing.expectEqualStrings("external_symbol", name);
        try std.testing.expect(!function.public);
        found = true;
    };
    try std.testing.expect(found);
    try sema.ir.verify();
}

test "export compilation lowering owns imported nonpublic symbols under OOM" {
    try testCompilationExportOwnership(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testCompilationExportOwnership, .{});
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
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
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
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
    }
}

test "generic structs reject unsupported fields inexact defaults constructors and receivers" {
    const prefix = "let Box:(T:type)->type=struct{value:T,\nlet Self=@This()\nlet get:(self:Self)->T=self.value\n}\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let Bad=Box([][]int)", .tag = .unsupported_type },
        .{ .source = "let Bad=Box(void)", .tag = .unsupported_type },
        .{ .source = "let Bad=Box(()->int)", .tag = .unsupported_type },
        .{ .source = "let U=union{ok:int,}\nlet Bad=Box(U)", .tag = .unsupported_type },
        .{ .source = "let G:(T:type)->type=struct{items:[]T,}\nlet Bad=G([]int)", .tag = .unsupported_type },
        .{ .source = "let G:(T:type)->type=struct{value:T=true,}\nlet Bad=G(int)", .tag = .literal_type_mismatch },
        .{ .source = "let G:(T:type)->type=struct{value:T=256,}\nlet Bad=G(u8)", .tag = .literal_out_of_range },
        .{ .source = "let G:(T:type)->type=struct{value:T=Box(int)[.value=1],}\nlet Bad=G(Box(int))", .tag = .invalid_field_default },
        .{ .source = "let G:(T:type)->type=struct{value:T=unknown,}\nlet Bad=G(int)", .tag = .invalid_field_default },
        .{ .source = "let f:()={let x=Box(int)[.value=true]\n}", .tag = .literal_type_mismatch },
        .{ .source = "let f:()={let x=Box(int)[]\n}", .tag = .missing_field },
        .{ .source = "let f:()={let x=Box(int)[.other=1]\n}", .tag = .unknown_field },
        .{ .source = "let f:()={let x=Box(int)[.value=1,.value=2]\n}", .tag = .duplicate_field },
        .{ .source = "let f:(x:Box(int))->Box(bool)=x", .tag = .expression_type_mismatch },
        .{ .source = "let Other:(T:type)->type=struct{value:T,}\nlet f:(x:Box(int))->Other(int)=x", .tag = .expression_type_mismatch },
        .{ .source = "let f:(x:Box(bool))->int=Box(int).get(x)", .tag = .expression_type_mismatch },
        .{ .source = "let f:(x:Box(int))={x.value=2\n}", .tag = .immutable_assignment },
        .{ .source = "let f:()={let x=Box(int)[.value=1]\nx.value=2\n}", .tag = .immutable_assignment },
        .{ .source = "let G:(T:type)->type=struct{value:T,\nlet bad:()->T={let x:T=true\nx}\n}\nlet f:()->int=G(int).bad()", .tag = .literal_type_mismatch },
        .{ .source = "let G:(T:type)->type=struct{value:T,\nlet bad:()->T=true\n}\nlet f:()->int=G(int).bad()", .tag = .literal_type_mismatch },
        .{ .source = "let x:Box", .tag = .generic_needs_arguments },
        .{ .source = "let x:Box(int,bool)", .tag = .generic_argument_count },
        .{ .source = "let G:(n:int)->type=struct{value:int,}", .tag = .invalid_generic_declaration },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "generic-struct-error.nov", source);
        defer module.deinit();
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        var result = try lower(&module);
        defer result.deinit();
        var found = false;
        for (module.diagnostics.items) |d| if (d.tag == case.tag) {
            found = true;
        };
        if (!found) std.debug.print("generic struct error {s}: {any}\n", .{ case.source, module.diagnostics.items });
        try std.testing.expect(found);
        try std.testing.expect(!result.lowering_complete);
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
    }
}

test "generic structs check defaults only for used concrete types including late body instances" {
    for ([_][]const u8{
        "let G:(T:type)->type=struct{value:T=1,}\nlet run:()={let x=G(bool)[]\n}",
        "let G:(T:type)->type=struct{value:T=1,}\nlet run:()={let x=G(bool)[.value=true]\n}",
    }) |source| {
        for ([_]bool{ false, true }) |imported| {
            try testLateGenericLayout(std.testing.allocator, source, .literal_type_mismatch, imported);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLateGenericLayout, .{ source, @as(?Module.Diagnostic.Tag, .literal_type_mismatch), imported });
        }
    }
    const source = "let Unused:(T:type)->type=struct{value:T=unknown,}\nlet G:(T:type)->type=struct{value:T=true,\nlet typed:(x:T)->T={let y:T=x\ny}\n}\nlet run:()={let x=G(bool)[]\nlet y=G(bool).typed(x.value)\n}";
    try testLateGenericLayout(std.testing.allocator, source, null, false);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLateGenericLayout, .{ source, @as(?Module.Diagnostic.Tag, null), false });
}

test "generic structs late cycles expanding type arguments and layout depth retain source diagnostics" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let G:(T:type)->type=struct{value:G(T),}\nlet run:()={let x=G(int)[]\n}", .tag = .generic_cycle },
        .{ .source = "let G:(T:type)->type=struct{value:H(T),}\nlet H:(T:type)->type=struct{value:G(T),}\nlet run:()={let x=G(int)[]\n}", .tag = .generic_cycle },
        .{ .source = "let G:(T:type)->type=struct{value:@This(),}\nlet run:()={let x=G(int)[]\n}", .tag = .generic_cycle },
        .{ .source = "let G:(T:type)->type=struct{value:Self,\nlet Self=@This()\n}\nlet run:()={let x=G(int)[]\n}", .tag = .generic_cycle },
        .{ .source = "let G:(T:type)->type=struct{value:H(G(T)),}\nlet H:(T:type)->type=struct{value:T,}\nlet run:()={let x=G(int)[]\n}", .tag = .generic_cycle },
        .{ .source = "let G:(T:type)->type=struct{value:G((T)->T),}\nlet run:()={let x=G(int)[]\n}", .tag = .generic_limit },
    }) |case| {
        for ([_]bool{ false, true }) |imported| {
            try testLateGenericLayout(std.testing.allocator, case.source, case.tag, imported);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLateGenericLayout, .{ case.source, @as(?Module.Diagnostic.Tag, case.tag), imported });
        }
    }
    for ([_]usize{ Ir.max_record_depth - 1, Ir.max_record_depth }) |depth| {
        var source = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer source.deinit();
        for (0..depth - 1) |i| try source.writer.print("let R{d}=struct{{next:R{d},}}\n", .{ i, i + 1 });
        try source.writer.print("let R{d}=struct{{n:int=1,}}\n", .{depth - 1});
        try source.writer.writeAll("let G:(T:type)->type=struct{some:T,}\n@[public]\nlet run:()={let x=G(R0)[.some={return}]\n}");
        const expected: ?Module.Diagnostic.Tag = if (depth == Ir.max_record_depth) .record_layout_limit else null;
        for ([_]bool{ false, true }) |imported| {
            try testLateGenericLayout(std.testing.allocator, source.written(), expected, imported);
            if (expected != null) try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLateGenericLayout, .{ source.written(), expected, imported });
        }
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
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
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
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
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
    }
}

test "unsupported union declarations and rejected matches roll back under OOM" {
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let U=union{}", .tag = .empty_union },
        .{ .source = "let U=union{ok:int=0,}", .tag = .unsupported_union_default },
        .{ .source = "let U=union{ok:int,ok:string,}", .tag = .duplicate_field },
        .{ .source = "let U=union{ok:[]int,}", .tag = .unsupported_type },
        .{ .source = "let R=struct{n:[][]int,}\nlet U=union{ok:R,}", .tag = .unsupported_type },
        .{ .source = "let R=struct{n:U,}\nlet U=union{ok:R,}", .tag = .unsupported_type },
        .{ .source = "let R=struct{n:R,}\nlet U=union{ok:R,}", .tag = .record_layout_cycle },

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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{case.source});
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
        .{ .source = "let A=struct{x:[][]int,}", .tag = .unsupported_type },
        .{ .source = "let A=struct{x:()->int,}", .tag = .unsupported_type },
        .{ .source = "let A=struct{x:A,}", .tag = .record_layout_cycle },
        .{ .source = "let A=struct{x:B,}\nlet B=struct{x:A,}", .tag = .record_layout_cycle },
        .{ .source = "let A=struct{x:int,}\nlet f:(a:A)={a.x=1}", .tag = .immutable_assignment },
        .{ .source = "let A=struct{x:int,}\nlet f:(a:A)->int=a.y", .tag = .unknown_field },
        .{ .source = "let A=struct{x:int,}\nlet f:(a:[][]A)= {}", .tag = .unsupported_type },
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{case.source});
    }
}

test "field mutation rejects nonlocals immutable bindings unsupported operators and inexact types" {
    const prefix = "let K=enum{a,b,}\nlet Other=enum{a,b,}\nlet R=struct{x:i8=0,s:string=\"\",b:bool=false,k:K=.a,}\nlet U=union{x:i8,}\n";
    const Case = struct { source: []const u8, tag: Module.Diagnostic.Tag };
    for ([_]Case{
        .{ .source = "let f:(r:R)={r.x=1}", .tag = .immutable_assignment },
        .{ .source = "let f:()={let r=R[]\nr.x=1}", .tag = .immutable_assignment },
        .{ .source = "let f:()={R[].x=1}", .tag = .unsupported_assignment_target },
        .{ .source = "let f:()={let mut r=R[]\nr.s.len=1}", .tag = .unsupported_array_length_write },
        .{ .source = "let f:()={let mut r=R[]\nr.x.missing=1}", .tag = .unsupported_assignment_target },
        .{ .source = "let f:()={let mut u:U=.x[1]\nu.x=2}", .tag = .unsupported_assignment_target },
        .{ .source = "let f:()={let mut r=R[]\nr.missing=1}", .tag = .unknown_field },
        .{ .source = "let f:()={let mut r=R[]\nr.x=128}", .tag = .literal_out_of_range },
        .{ .source = "let f:()={let mut r=R[]\nr.x=true}", .tag = .literal_type_mismatch },
        .{ .source = "let f:(n:i16)={let mut r=R[]\nr.x=n}", .tag = .expression_type_mismatch },
        .{ .source = "let f:()={let mut r=R[]\nr.k=Other.a}", .tag = .literal_type_mismatch },
        .{ .source = "let f:()={let mut r=R[]\nr.b+=true}", .tag = .operator_not_supported },
        .{ .source = "let f:()={let mut r=R[]\nr.k+=.a}", .tag = .operator_not_supported },
        .{ .source = "let f:()={let mut r=R[]\nr.s-=\"x\"}", .tag = .operator_not_supported },
        .{ .source = "let f:()->i8={let mut r=R[]\nr.x=1}", .tag = .expression_type_mismatch },
    }) |case| {
        const source = try std.mem.concat(std.testing.allocator, u8, &.{ prefix, case.source });
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "field-errors.nov", source);
        defer module.deinit();
        var result = try lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        try std.testing.expectEqual(case.tag, module.diagnostics.items[module.diagnostics.items.len - 1].tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

test "field stores verify slot identity field bounds exact types prior operands spans and private regions" {
    var module = try Module.init(std.testing.allocator, "field-ir.nov",
        \\let R=struct{x:int=0,s:string="",}
        \\let U=union{x:int,}
        \\let f:()={let mut r=R[]
        \\    let mut u:U=.x[1]
        \\    let mut scalar:int=1
        \\    if true {let mut private=R[]
        \\        private.x=23}
        \\    r.x=7}
    );
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try result.ir.verify();
    const body = result.ir.functions.items[0].body.?;
    const instructions = @constCast(body.instructions);
    const last = &instructions[instructions.len - 1];
    try std.testing.expect(last.op == .field_store);
    const saved = last.*;
    defer last.* = saved;
    for (0..11) |case| {
        last.* = saved;
        switch (case) {
            0 => last.op.field_store.slot = @fromBackingInt(@intCast(0)),
            1 => last.op.field_store.slot = @fromBackingInt(@intCast(instructions.len)),
            2 => last.op.field_store.index = 99,
            3 => last.op.field_store.value = saved.op.field_store.slot,
            4 => last.op.field_store.value = @fromBackingInt(@intCast(instructions.len - 1)),
            5 => last.ty = instructions[@backingInt(saved.op.field_store.value)].ty,
            6 => last.op.field_store.index = 1,
            7 => last.span.end = std.math.maxInt(u32),
            10 => last.ty = @fromBackingInt(@intCast(std.math.maxInt(u32))),
            8, 9 => {
                for (instructions[0 .. instructions.len - 1]) |instruction| {
                    if (instruction.op == .field_store) {
                        if (case == 8) last.op.field_store.slot = instruction.op.field_store.slot else last.op.field_store.value = instruction.op.field_store.value;
                    }
                }
            },
            else => unreachable,
        }
        if (result.ir.verify()) |_| return error.ExpectedInvalidFieldStore else |_| {}
    }
    // Tagged and nonrecord mutable slots are valid slots, but never field bases.
    for (instructions, 0..) |instruction, i| {
        if (instruction.op != .local_slot) continue;
        const ty = result.ir.getType(instruction.ty).?;
        if (ty == .record and !ty.record.tagged) continue;
        last.* = saved;
        last.op.field_store.slot = @fromBackingInt(@intCast(i));
        try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    }
    last.* = saved;
    try result.ir.verify();
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
    try std.testing.expectError(error.ConstantTypeMismatch, result.ir.verify());
    fields[0] = saved;
    fields[0].default = @fromBackingInt(@intCast(999));
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
    }) |source| try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
}

test "rejected boolean matches roll back diagnostics on allocation failure" {
    for ([_][]const u8{
        "let f: (b: bool) -> int = match b {}",
        "let f: (b: bool) -> int = match b {true => 1}",
        "let f: (b: bool) -> int = match b {false => 1}",
        "let f: (b: bool) -> int = match b {true => 1\ntrue | true => 2\nfalse => 3}",
        "let f: (b: bool) -> int = match b {true => 1\nfalse | 1 => 2}",
    }) |source| try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
}

test "rejected bitwise binaries roll back diagnostics on allocation failure" {
    for ([_][]const u8{
        "let f: () -> int = 1 & missing",
        "let f: (n: i8) -> i8 = n | 128",
        "let f: () -> bool = true ^ false",
    }) |source| try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source});
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
        try std.testing.expect(@backingInt(binary.lhs) < i);
        try std.testing.expect(@backingInt(binary.rhs) < i);
        try std.testing.expectEqual(instruction.ty, body.instructions[@backingInt(binary.lhs)].ty);
        try std.testing.expectEqual(instruction.ty, body.instructions[@backingInt(binary.rhs)].ty);
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
            try std.testing.expect(@backingInt(instruction.op.to_string) < i);
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
        } else first = @fromBackingInt(@intCast(i));
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
    const merge = &@constCast(body.instructions)[@backingInt(body.result)];
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
    const terminal = &@constCast(terminal_body.instructions)[@backingInt(terminal_body.result)];
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
    const merge = &@constCast(body.instructions)[@backingInt(body.result)];
    try std.testing.expect(merge.op == .branch);
    const saved = merge.op.branch;
    merge.op.branch.else_value = saved.then_value;
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    merge.op.branch = saved;
    merge.op.branch.then_value = saved.else_value.?;
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    merge.op.branch = saved;
    const terminal_body = sema.ir.functions.items[1].body.?;
    const terminal = &@constCast(terminal_body.instructions)[@backingInt(terminal_body.result)];
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
        const inner = body.instructions[@backingInt(inner_id)];
        try std.testing.expect(inner.op.loop.returns);
        try std.testing.expect(Ir.terminates(body, inner_id));
    }
    const body = sema.ir.functions.items[2].body.?;
    const index = @backingInt(body.result);
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
    try std.testing.expect(@backingInt(counter) < loop.condition_start);
    try std.testing.expect(instructions[@backingInt(counter)].op == .local_slot);
    const condition = loop.condition_start + 3;
    const endpoint = instructions[condition].op.binary.rhs;
    try std.testing.expect(@backingInt(endpoint) < @backingInt(counter));
    try std.testing.expect(instructions[loop.body_start].op == .local_load);
    const private_current: Ir.ValueId = @fromBackingInt(@intCast(loop.body_start));
    // A body-private snapshot must not leak into a sibling continuation,
    // and a body-local slot cannot replace the enclosing counter identity.
    const continuation = loop.continuation_start.?;
    try std.testing.expectEqual(Ir.BinaryOp.lt, instructions[condition].op.binary.op);
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
    instructions[continuation].op.local_load = @fromBackingInt(@intCast(loop.body_start + 1));
    try std.testing.expectError(error.InvalidValueId, sema.ir.verify());
    instructions[continuation] = saved;
    instructions[condition].op.binary.rhs = @fromBackingInt(@intCast(owner));
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
        instructions[owner].op.loop.else_value = @fromBackingInt(@intCast(saved.body_start));
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
    try std.testing.expectEqual(loop_owner.?, Ir.enclosingLoop(body, @backingInt(break_value.?) + 1).?);
    try std.testing.expect(body.instructions[@backingInt(break_value.?)].op == .array_literal);
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
            try std.testing.expect(body.instructions[@backingInt(binary.lhs)].op == .local_load);
            try std.testing.expect(body.instructions[@backingInt(binary.rhs)].op == .call);
            try std.testing.expect(@backingInt(binary.lhs) < @backingInt(binary.rhs));
            var replacement: ?usize = null;
            for (body.instructions[0..i], 0..) |prior, j| {
                if (prior.op == .local_store) replacement = j;
            }
            try std.testing.expect(@backingInt(binary.lhs) < replacement.?);
            try std.testing.expect(replacement.? < @backingInt(binary.rhs));
            try std.testing.expectEqual(@as(Ir.ValueId, @fromBackingInt(@intCast(i))), body.instructions[i + 1].op.local_store.value);
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
            try std.testing.expect(body.instructions[@backingInt(r.array)].op == .array_clone);
            try std.testing.expect(body.instructions[@backingInt(r.index)].op == .call);
            try std.testing.expect(@backingInt(r.array) < @backingInt(r.index));
            read = @fromBackingInt(@intCast(i));
        },
        .array_set => |r| {
            sets += 1;
            const old = body.instructions[@backingInt(read.?)].op.array_index;
            try std.testing.expectEqual(old.array, r.array);
            try std.testing.expectEqual(old.index, r.index);
            const binary = body.instructions[@backingInt(r.value)].op.binary;
            try std.testing.expectEqual(Ir.BinaryOp.add, binary.op);
            try std.testing.expectEqual(read.?, binary.lhs);
            try std.testing.expect(@backingInt(read.?) < @backingInt(binary.rhs));
            try std.testing.expectEqual(@as(usize, 2), calls);
            try std.testing.expect(body.instructions[i + 1].op == .local_store);
            try std.testing.expectEqual(@as(Ir.ValueId, @fromBackingInt(@intCast(i))), body.instructions[i + 1].op.local_store.value);
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
    try std.testing.expectEqual(@as(u32, 2), @backingInt(twice.lhs));
    try std.testing.expectEqual(twice.lhs, twice.rhs);
    const sum = body.instructions[5].op.binary;
    try std.testing.expectEqual(@as(u32, 3), @backingInt(sum.lhs));
    try std.testing.expectEqual(@as(u32, 2), @backingInt(sum.rhs));
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
    try std.testing.expectEqual(@as(u32, 0), @backingInt(scalar.result));
    var copies: usize = 0;
    for (sema.ir.functions.items[1].body.?.instructions) |instruction| {
        if (instruction.op == .array_clone) copies += 1;
    }
    try std.testing.expectEqual(@as(usize, 0), copies);
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
    try std.testing.expect(@backingInt(args[0]) < @backingInt(args[1]));
    try std.testing.expect(@backingInt(args[1]) < @backingInt(args[2]));
    try std.testing.expect(body.instructions[@backingInt(args[0])].op == .call);
    try std.testing.expect(body.instructions[@backingInt(args[1])].op == .call);
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
    try std.testing.expectEqual(@as(u32, 0), @backingInt(body.instructions[2].op.binary.lhs));
    try std.testing.expectEqual(@as(u32, 1), @backingInt(body.instructions[2].op.binary.rhs));
    try std.testing.expectEqual(@as(u32, 2), @backingInt(body.result));
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
        calls[count] = @fromBackingInt(@intCast(i));
        const callee = body.instructions[@backingInt(instruction.op.call.callee)].op.function_ref;
        try std.testing.expectEqualStrings(if (count < 2) "f" else "combine", sema.ir.getFunction(callee).?.name);
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), count);
    const outer = body.instructions[@backingInt(calls[2])].op.call;
    try std.testing.expectEqualSlices(Ir.ValueId, calls[0..2], outer.args);
    try std.testing.expect(@backingInt(outer.callee) < @backingInt(calls[0]));
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
    const loop = body.instructions[@backingInt(body.result)];
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
    const loop = body.instructions[@backingInt(body.result)];
    const region = loop.op.loop;
    const condition = body.instructions[@backingInt(region.condition)];
    try std.testing.expectEqualStrings("loop {break}", module.source.ast.source[loop.span.start..loop.span.end]);
    try std.testing.expectEqual(loop.span, condition.span);
    try std.testing.expectEqual(true, sema.ir.constants.items[@backingInt(condition.op.constant)].value.boolean);
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
    try std.testing.expect(body.instructions[@backingInt(body.result)].op.branch.returns);
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
    try std.testing.expectEqual(@as(u32, 1), @backingInt(r.then_value));
    try std.testing.expect(body.instructions[@backingInt(r.else_value.?)].op == .branch);
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
        try std.testing.expectEqual(@as(u32, 0), @backingInt(region.lhs));
        try std.testing.expect(body.instructions[@backingInt(region.rhs)].op == .call);
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
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
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
    try std.testing.expectEqual(@as(u32, 4), @backingInt(main.instructions[5].op.call.args[0]));
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

fn testClosureRejection(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "closure-rejection.nov", source);
    defer module.deinit();
    errdefer if (module.source.ast.errors.len != 0) std.debug.print("closure rejection parse errors: {any}\n{s}\n", .{ module.source.ast.errors, source });

    try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
    var result = lower(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer result.deinit();
    try std.testing.expect(module.hasErrors());
    try std.testing.expect(!result.lowering_complete);
    try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
}

test "closures reject borrowed captures recursion inferred parameters foreign ABI and unsupported containers under OOM" {
    for ([_][]const u8{
        "let main:()={let mut n:int=1\nlet b=&n\nlet f=()->int {b}\n}",
        "let main:()={let f=(n:int)->int {f(n)}\n}",
        "let main:()={let f:(n:int)->int=f(n)\n}",
        "let main:()={let f=(int)->int {1}\n}",
        "let main:()={let f=(n:int)->int {true}\n}",
        "let main:()={let f=()->int {break}\n}",
        "let main:()={let f=()->int {unknown}\n}",
        "@[extern(\"callback\")]\nlet foreign:(f:()->int)->int\nlet main:()={let f=()->int {1}\n@println(foreign(f))}",
        "@[export]\nlet foreign:(f:()->int)->int=f()",
        "let R=struct{f:()->int,}\nlet main:()={}",
        "let main:()={let a:[]()->int=[]\n}",
        "let main:()={let mut n:int=0\nlet f=()->int {n}\nlet b=&n\n@println(f(),b)}",
        "let main:()={let mut n:int=0\nlet b=&n\nlet f=()->int {n}\n@println(b)}",
        "let main=(){let n:int=1\nlet f=(n:int)->int {n}\n}",
        "let main=(){let n:int=1\nlet f=()->int {let n:int=2\nn}\n}",
        "let main=(){let mut values:[]int=[1]\nlet change=(){values[0]+=1}\nloop item in values {change()}}",
        "let main=(){let mut values:[]int=[1]\nlet change=(){values[0]+=1}\nloop &item in values {change()\nitem+=1}}",
        "let main=(){let mut values:[]int=[1]\nloop &item in values {let read=()->int {item}\n}}",
    }) |source| {
        try testClosureRejection(std.testing.allocator, source);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testClosureRejection, .{source});
    }
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{source});
    }
}

fn testImport(comp: *Compilation, source: Source.Id, name: []const u8, target: Source.Id, named: bool) !void {
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
        \\@[public]
        \\let helper:()->string="helper"
        \\@[public]
        \\let Result:(T:type,E:type)->type=union{ok:T,err:E,
        \\let Self=@This()
        \\@[public]
        \\let unwrapOr:(self:Self,fallback:T)->T=match self{.ok=>|v|v
        \\.err=>fallback}
        \\@[public]
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
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, struct {
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
        \\@[public]
        \\let Shared=lib.Shared
        \\@[public]
        \\let Kind=lib.Kind
        \\@[public]
        \\let answer:()->bool=lib.answer()
    );
    const right = try comp.addSource("/right.nov",
        \\let lib=@import("shared.nov")
        \\@[public]
        \\let Shared=lib.Shared
    );
    const shared = try comp.addSource("/shared.nov",
        \\@[public]
        \\let Shared=struct{value:bool,
        \\    @[public]
        \\    let get:(self:Shared)->bool=self.value
        \\}
        \\@[public]
        \\let Kind=enum{one,two,}
        \\@[public]
        \\let answer:()->bool=true
    );
    const other = try comp.addSource("/other.nov",
        \\@[public]
        \\let Shared=struct{value:bool,
        \\    let get:(self:Shared)->bool=false
        \\}
        \\@[public]
        \\let Kind=enum{one,two,}
        \\@[public]
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
        const module = comp.getModule(root).?;
        try std.testing.expect(module.diagnostics.items.len != 0);
        try std.testing.expectEqual(root, module.diagnostics.items[0].span.source);
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
        \\@[public]
        \\let types=@import("types.nov")
        \\@[public]
        \\let Result=types.Result
    );
    const right = try comp.addSource("/right.nov",
        \\let types=@import("types.nov")
        \\@[public]
        \\let Result=types.Result
    );
    const types = try comp.addSource("/types.nov",
        \\@[public]
        \\let Result:(T:type,E:type)->type=union{ok:T,err:E,
        \\let Self=@This()
        \\@[public]
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
    var dependencies = try Dependencies.analyze(&standalone);
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
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testCompilationGlobalDiagnostic, .{ case.source, case.tag, imported });
    };
}

fn testQualifiedEnumGlobal(allocator: std.mem.Allocator, source: []const u8, expected: ?Module.Diagnostic.Tag, named: bool) !void {
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
        \\@[public]
        \\let E=enum{a,b,
        \\let unmarked:(self:E)->bool=true
        \\@[public]
        \\let published:(self:E)->bool=false
        \\}
        \\@[public]
        \\let Other=enum{a,b,}
    );
    try testImport(&comp, root, "lib", api, named);
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
        if (tag == .unknown_field) try std.testing.expectEqualStrings(if (std.mem.indexOf(u8, source, ".missing") != null) "missing" else "unmarked", module.sourceSlice(diagnostic.span).?);
    } else {
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expect(result.lowering_complete);
        try result.ir.verify();
        for (module.declarations.items, 0..) |decl, i| {
            if (!std.mem.eql(u8, decl.name, "x")) continue;
            const entry = result.getCompilationEntry(comp.declaration_ids.get(.{ .source = root, .local = @fromBackingInt(@intCast(i)) }).?).?;
            try std.testing.expectEqual(Sema.Status.constant_checked, entry.status);
            const nominal = result.ir.getType(entry.type_id.?).?.enumeration;
            try std.testing.expectEqual(enums, nominal.source);
            try std.testing.expectEqual(@as(u32, 0), nominal.identity);
        }
    }
    // Both import forms gate exports and preserve method visibility metadata.
    const namespace = comp.declaration_ids.get(.{ .source = root, .local = module.lookup("lib").? }).?;
    try std.testing.expect(comp.lookupNamespace(root, namespace, "hidden") == null);
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
        try testQualifiedEnumGlobal(std.testing.allocator, case.source, case.expected, true);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testQualifiedEnumGlobal, .{ case.source, case.expected, true });
    }
}

test "file and named namespace exported enum methods require explicit public access" {
    for ([_]bool{ false, true }) |named| {
        try testQualifiedEnumGlobal(std.testing.allocator,
            \\let lib=@import("api")
            \\let x:lib.E=lib.E.a
            \\let main:()=@println(x.published(),lib.E.published(x))
        , null, named);
        try testQualifiedEnumGlobal(std.testing.allocator,
            \\let lib=@import("api")
            \\let x:lib.E=lib.E.a
            \\let main:()=@println(x.unmarked())
        , .unknown_field, named);
        try testQualifiedEnumGlobal(std.testing.allocator,
            \\let lib=@import("api")
            \\let E=lib.E
            \\let x:E=E.a
            \\let main:()=@println(E.unmarked(x))
        , .not_a_value, named);
    }
}

test "nested records diagnose cycles defaults nominal replacements and invalid path bases under OOM" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "let A=struct{edge:Alias,}\nlet Alias=A", .tag = .record_layout_cycle },
        .{ .source = "let A=struct{to_b:Alias,}\nlet Alias=B\nlet B=struct{to_a:A,}", .tag = .record_layout_cycle },
        .{ .source = "let A=struct{b:B=B[],}\nlet B=struct{x:int=1,}", .tag = .invalid_field_default },
        .{ .source = "let A=struct{b:B,}\nlet B=struct{x:int=1,}\nlet f:()->A=A[]", .tag = .missing_field },
        .{ .source = "let A=struct{b:B,}\nlet B=struct{x:int=1,}\nlet C=struct{x:int=1,}\nlet f:()={let mut a=A[.b=B[]]\na.b=C[]}", .tag = .expression_type_mismatch },
        .{ .source = "let A=struct{b:B,}\nlet B=struct{x:int=1,}\nlet f:(a:A)={a.b.x=3}", .tag = .immutable_assignment },
        .{ .source = "let A=struct{b:B,}\nlet B=struct{x:int=1,}\nlet f:()={A[.b=B[]].b.x=3}", .tag = .unsupported_assignment_target },
        .{ .source = "let A=struct{b:B,}\nlet B=struct{x:int=1,}\nlet f:()={let mut a=A[.b=B[]]\na.b.missing=3}", .tag = .unknown_field },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "nested-invalid.nov", case.source);
        defer module.deinit();
        var result = try lower(&module);
        defer result.deinit();
        try std.testing.expect(module.hasErrors());
        var found = false;
        for (module.diagnostics.items) |diagnostic| {
            if (diagnostic.tag == case.tag) found = true;
        }
        try std.testing.expect(found);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{case.source});
    }
}

test "nested records field store paths are owned and verified after source release" {
    var result = blk: {
        var module = try Module.init(std.testing.allocator, "path.nov", "let A=struct{b:B,}\nlet B=struct{c:C,}\nlet C=struct{x:int=1,}\nlet f:()={let mut a=A[.b=B[.c=C[]]]\na.b.c.x=2}");
        defer module.deinit();
        const result = try lower(&module);
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer result.deinit();
    try result.ir.verify();
    const body = result.ir.functions.items[0].body.?;
    const instruction = &@constCast(body.instructions)[body.instructions.len - 1];
    const saved = instruction.op.field_store;
    try std.testing.expectEqualSlices(u32, &.{ 0, 0 }, saved.path);
    const path = @constCast(saved.path);
    path[0] = 999;
    try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    path[0] = 0;
    instruction.op.field_store.path = &.{ 0, 0, 0 };
    try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    instruction.op.field_store.path = &.{};
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instruction.op.field_store = saved;
    try result.ir.verify();
}

test "nested records imported cyclic layouts retain deterministic qualified diagnostics" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const root = try comp.addSource("/main.nov", "let types=@import(\"types.nov\")\nlet Outer=struct{inner:types.A,}\nlet main:()={}");
    const types = try comp.addSource("/types.nov", "@[public]\nlet A=struct{edge:Alias,}\nlet Alias=B\nlet B=struct{back:A,}");
    try testImport(&comp, root, "types", types, false);
    comp.setRoot(root);
    var result = try lowerCompilation(&comp);
    defer result.deinit();
    try std.testing.expect(!result.lowering_complete);
    const diagnostics = comp.getModule(types).?.diagnostics.items;
    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    try std.testing.expectEqual(Module.Diagnostic.Tag.record_layout_cycle, diagnostics[0].tag);
    try std.testing.expectEqual(types, diagnostics[0].span.source);
    try std.testing.expectEqualStrings("back", comp.sourceSlice(diagnostics[0].span).?);
    try std.testing.expectEqualStrings("edge", comp.sourceSlice(diagnostics[0].related.?).?);
}

test "nested records source nesting limit is bounded before IR or defaults become ready" {
    var source = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer source.deinit();
    for (0..Ir.max_record_depth) |i| try source.writer.print("let R{d}=struct{{next:R{d},}}\n", .{ i, i + 1 });
    try source.writer.print("let R{d}=struct{{n:int=1,}}", .{Ir.max_record_depth});
    var module = try Module.init(std.testing.allocator, "depth.nov", source.written());
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(!result.lowering_complete);
    try std.testing.expectEqual(Module.Diagnostic.Tag.record_layout_limit, module.diagnostics.items[0].tag);
    try std.testing.expectEqualStrings("next", module.sourceSlice(module.diagnostics.items[0].span).?);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedMatchAllocations, .{source.written()});
}

test "nested records imported integer leaf widths preserve exact qualified backend spans" {
    for ([_][]const u8{ "i128", "u128" }) |width| {
        var comp = Compilation.init(std.testing.allocator);
        defer comp.deinit();
        const root = try comp.addSource("/main.nov", "let types=@import(\"types.nov\")\nlet U=union{ok:types.Outer,none,}\nlet f:(cb:(U)->void)={}");
        const source = try std.fmt.allocPrint(std.testing.allocator, "@[public]\nlet Outer=struct{{left:Middle,right:Middle,}}\nlet Middle=struct{{leaf:Alias,}}\nlet Alias=Leaf\nlet Leaf=struct{{wide:{s},}}", .{width});
        defer std.testing.allocator.free(source);
        const types = try comp.addSource("/types.nov", source);
        try testImport(&comp, root, "types", types, false);
        comp.setRoot(root);
        var result = try lowerCompilation(&comp);
        defer result.deinit();
        try std.testing.expect(result.lowering_complete);
        var diagnostic: CEmitter.Diagnostic = undefined;
        try std.testing.expectError(error.UnsupportedIntegerWidth, CEmitter.emit(std.testing.allocator, &result.ir, &diagnostic));
        try std.testing.expectEqual(types, diagnostic.span.source);
        try std.testing.expectEqualStrings("wide", comp.sourceSlice(diagnostic.span).?);
    }
}

fn testLateGenericLayout(allocator: std.mem.Allocator, source: []const u8, expected: ?Module.Diagnostic.Tag, imported: bool) !void {
    if (imported) {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/main.nov", "let types=@import(\"types.nov\")\nlet main:()=types.run()");
        const types = try comp.addSource("/types.nov", source);
        try testImport(&comp, root, "types", types, false);
        comp.setRoot(root);
        var result = lowerCompilation(&comp) catch |err| {
            if (err == error.OutOfMemory) for (comp.modules.items) |module| try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        defer result.deinit();
        try std.testing.expectEqual(expected != null, comp.hasErrors());
        try std.testing.expectEqual(expected == null, result.lowering_complete);
        if (expected) |tag| {
            try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
            const diagnostics = comp.getModule(types).?.diagnostics.items;
            try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
            const diagnostic = diagnostics[0];
            try std.testing.expectEqual(tag, diagnostic.tag);
            try std.testing.expectEqual(types, diagnostic.span.source);
            if (tag == .record_layout_limit) {
                try std.testing.expectEqualStrings("some", comp.sourceSlice(diagnostic.span).?);
                try std.testing.expectEqualStrings("next", comp.sourceSlice(diagnostic.related.?).?);
            }
        }
    } else {
        var module = try Module.init(allocator, "late-layout.nov", source);
        defer module.deinit();
        var result = lower(&module) catch |err| {
            if (err == error.OutOfMemory) try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        defer result.deinit();
        try std.testing.expectEqual(expected != null, module.hasErrors());
        try std.testing.expectEqual(expected == null, result.lowering_complete);
        if (expected) |tag| {
            try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
            try std.testing.expectEqual(@as(usize, 1), module.diagnostics.items.len);
            const diagnostic = module.diagnostics.items[0];
            try std.testing.expectEqual(tag, diagnostic.tag);
            if (tag == .record_layout_limit) {
                try std.testing.expectEqualStrings("some", module.sourceSlice(diagnostic.span).?);
                try std.testing.expectEqualStrings("next", module.sourceSlice(diagnostic.related.?).?);
            }
        }
    }
}

test "nested records late body generic layouts diagnose depth before lowering publication" {
    for ([_]usize{ Ir.max_record_depth - 1, Ir.max_record_depth }) |depth| {
        var source = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer source.deinit();
        for (0..depth - 1) |i| try source.writer.print("let R{d}=struct{{next:R{d},}}\n", .{ i, i + 1 });
        try source.writer.print("let R{d}=struct{{n:int=1,}}\n", .{depth - 1});
        try source.writer.writeAll("let G:(T:type)->type=union{some:T,none,}\n@[public]\nlet run:()={let u:G(R0)=.none[]\n}");
        const expected: ?Module.Diagnostic.Tag = if (depth == Ir.max_record_depth) .record_layout_limit else null;
        for ([_]bool{ false, true }) |imported| {
            try testLateGenericLayout(std.testing.allocator, source.written(), expected, imported);
            if (expected != null) try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLateGenericLayout, .{ source.written(), expected, imported });
        }
    }
}

test "nested records late body recursive generic instances retain source diagnostics" {
    for ([_][]const u8{
        "let G:(T:type)->type=union{some:G(T),none,}\n@[public]\nlet run:()={let u:G(int)=.none[]\n}",
        "let G:(T:type)->type=union{some:H(T),none,}\nlet H:(T:type)->type=union{some:G(T),none,}\n@[public]\nlet run:()={let u:G(int)=.none[]\n}",
    }) |source| {
        for ([_]bool{ false, true }) |imported| {
            try testLateGenericLayout(std.testing.allocator, source, .generic_cycle, imported);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testLateGenericLayout, .{ source, @as(?Module.Diagnostic.Tag, .generic_cycle), imported });
        }
    }
}

test "static integer borrowing forged IR owner kinds exact types paths and source release" {
    var result = blk: {
        var module = try Module.init(std.testing.allocator, "integer-borrow-ir.nov", "let R=struct{n:int=0,}\nlet f:()={let mut a=[R[]]\nlet mut flag=true\nlet mut x:int=1\nlet y=&x\ny+=1\n@println(x,a.len,flag)}");
        defer module.deinit();
        const result = try lower(&module);
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer result.deinit();
    try result.ir.verify();
    const body = result.ir.functions.items[0].body.?;
    const instructions = @constCast(body.instructions);
    var reserve: usize = 0;
    var start: usize = 0;
    var read: usize = 0;
    var store: usize = 0;
    var array: Ir.ValueId = undefined;
    var boolean: Ir.ValueId = undefined;
    for (instructions, 0..) |instruction, i| switch (instruction.op) {
        .borrow_reserve => reserve = i,
        .borrow_start => start = i,
        .borrow_read => read = i,
        .borrow_store => store = i,
        .local_slot => switch (result.ir.getType(instruction.ty).?) {
            .array => array = @fromBackingInt(@intCast(i)),
            .boolean => boolean = @fromBackingInt(@intCast(i)),
            else => {},
        },
        else => {},
    };
    const saved_reserve = instructions[reserve];
    const saved_start = instructions[start];
    const saved_read = instructions[read];
    const saved_store = instructions[store];
    try std.testing.expect(saved_start.op.borrow_start.index == null);
    try std.testing.expectEqual(@as(usize, 0), saved_store.op.borrow_store.path.len);
    instructions[reserve].op.borrow_reserve = boolean;
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[reserve] = saved_reserve;
    instructions[reserve].op.borrow_reserve = array;
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[reserve] = saved_reserve;
    instructions[start].op.borrow_start.index = saved_reserve.op.borrow_reserve;
    try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    instructions[start] = saved_start;
    instructions[read].ty = try result.ir.internType(.{ .integer = .{ .signedness = .unsigned, .bits = Ir.native_pointer_bits } });
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[read] = saved_read;
    const path = [_]u32{0};
    instructions[store].op.borrow_store.path = &path;
    try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    instructions[store] = saved_store;
    instructions[store].op.borrow_store.value = instructions[@backingInt(boolean)].op.local_slot;
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[store] = saved_store;
    instructions[read].op = .{ .local_load = saved_reserve.op.borrow_reserve };
    try std.testing.expectError(error.BorrowConflict, result.ir.verify());
    instructions[read] = saved_read;
    try result.ir.verify();
}

test "static borrowing verified capability types owner paths index and regions survive source release" {
    var result = blk: {
        var module = try Module.init(std.testing.allocator, "borrow-ir.nov", "let A=struct{b:B,}\nlet B=struct{x:int=0,}\nlet f:()={let mut a=[A[.b=B[]]]\nlet r=&a[0]\nr.b.x+=1\n@println(a.len)}");
        defer module.deinit();
        const result = try lower(&module);
        try std.testing.expect(result.lowering_complete);
        break :blk result;
    };
    defer result.deinit();
    try result.ir.verify();
    const body = result.ir.functions.items[0].body.?;
    const instructions = @constCast(body.instructions);
    var reserve: usize = 0;
    var start: usize = 0;
    var read: usize = 0;
    var store: usize = 0;
    for (instructions, 0..) |instruction, i| switch (instruction.op) {
        .borrow_reserve => reserve = i,
        .borrow_start => start = i,
        .borrow_read => read = i,
        .borrow_store => store = i,
        else => {},
    };
    const saved = instructions[store];
    instructions[store].op.borrow_store.path = &.{};
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[store] = saved;
    try std.testing.expectEqualSlices(u32, &.{ 0, 0 }, saved.op.borrow_store.path);
    @constCast(saved.op.borrow_store.path)[0] = 99;
    try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    @constCast(saved.op.borrow_store.path)[0] = 0;
    instructions[store].op.borrow_store.borrow = @fromBackingInt(@intCast(reserve));
    try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    instructions[store] = saved;
    const old_read = instructions[read];
    instructions[read].ty = instructions[reserve].ty;
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[read] = old_read;
    const old_start = instructions[start];
    instructions[start].op.borrow_start.index = null;
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[start] = old_start;
    instructions[start].op.borrow_start.index = @fromBackingInt(@intCast(reserve));
    try std.testing.expectError(error.OperandTypeMismatch, result.ir.verify());
    instructions[start] = old_start;
    const old_reserve = instructions[reserve];
    instructions[reserve].op.borrow_reserve = @fromBackingInt(@intCast(0));
    try std.testing.expectError(error.InvalidOperation, result.ir.verify());
    instructions[reserve] = old_reserve;
    // A forged direct owner load during RHS evaluation must fail the same proof
    // as source lowering, even with all value/region types otherwise valid.
    const old = instructions[read];
    instructions[read].ty = instructions[@backingInt(old_reserve.op.borrow_reserve)].ty;
    instructions[read].op = .{ .local_load = old_reserve.op.borrow_reserve };
    // First check the standalone lifetime proof (later field operands now have
    // intentionally inconsistent types, which normal verification also rejects).
    try std.testing.expect(try result.ir.borrowConflict(body) != null);
    instructions[read] = old;
    try result.ir.verify();
}

test "static borrowing imported generic nominal owners preserve exact identities" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const root = try comp.addSource("/main.nov", "let types=@import(\"types.nov\")\nlet Alias=types.R(int)\nlet main:()={let mut a=[Alias[.x=1]]\nlet r=&a[0]\nr.x+=1\n@println(a[0].x)}");
    const types = try comp.addSource("/types.nov", "@[public]\nlet R:(T:type)->type=struct{x:T,}");
    try testImport(&comp, root, "types", types, true);
    comp.setRoot(root);
    var sema = try lowerCompilation(&comp);
    try std.testing.expect(!comp.hasErrors());
    try std.testing.expect(sema.lowering_complete);
    defer sema.deinit();
    try sema.ir.verify();
}

fn testBorrowRejectionAllocations(allocator: std.mem.Allocator, source: []const u8) !void {
    var module = try Module.init(allocator, "borrow-oom.nov", source);
    defer module.deinit();
    var result = lower(&module) catch |err| {
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        return err;
    };
    defer result.deinit();
    try std.testing.expect(module.hasErrors());
    try std.testing.expect(!result.lowering_complete);
}

test "static borrowing rejected conflicts and escapes roll back every allocation failure" {
    for ([_][]const u8{
        "let R=struct{x:int=0,}\nlet f:()={let mut a=[R[]]\nlet r=&a[0]\nr.x+={a=[R[]]\n1}}",
        "let R=struct{x:int=0,}\nlet f:()={let mut a=[R[]]\nlet r=&a[0]\nlet alias=&r\n@println(alias.x)}",
        "let R=struct{x:int=0,}\nlet f:()={let mut a=[R[]]\nlet r=&a[{a=[R[]]\n0}]\nr.x=1}",
    }) |source| try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testBorrowRejectionAllocations, .{source});
}

test "static borrowing verifier rejects well typed forged owner access and sibling capabilities" {
    var module = try Module.init(std.testing.allocator, "borrow-proof.nov", "let R=struct{x:int=0,}\nlet f:(flag:bool)={let mut a=[R[]]\nlet r=&a[0]\nr.x=1\n@println(a[0].x)\nif flag {let s=&a[0]\ns.x+=1} else {let t=&a[0]\nt.x+=1}}");
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(result.lowering_complete);
    const body = result.ir.functions.items[0].body.?;
    const instructions = @constCast(body.instructions);
    var outer: Ir.ValueId = undefined;
    var sibling: ?Ir.ValueId = null;
    var index: usize = 0;
    var reads: usize = 0;
    for (instructions, 0..) |instruction, i| {
        if (instruction.op == .borrow_start and index == 0) outer = @fromBackingInt(@intCast(i));
        if (instruction.op == .array_index) index = i;
        if (instruction.op == .borrow_read) {
            if (sibling == null) sibling = instruction.op.borrow_read;
            reads = i;
        }
    }
    const old = instructions[index];
    instructions[index].op = .{ .borrow_read = outer };
    try std.testing.expectError(error.BorrowConflict, result.ir.verify());
    instructions[index] = old;
    const old_read = instructions[reads];
    instructions[reads].op = .{ .borrow_read = sibling.? };
    try std.testing.expectError(error.InvalidValueId, result.ir.verify());
    instructions[reads] = old_read;
    try result.ir.verify();
}

test "append RHS projects only final reads and keeps assignment snapshots" {
    for ([_][]const u8{
        "let main:()={let mut a:[]int=[1]\na+=a\nlet b=a\na=b}",
        "let L=struct{a:[]int,}\nlet T=struct{leaf:L,}\nlet main:()={let mut a=T[.leaf=L[.a=[1]]]\na.leaf.a+=a.leaf.a\nlet b=a.leaf.a\na.leaf.a=b}",
        "let L=struct{a:[]int,}\nlet main:()={let mut a=L[.a=[1]]\nlet r=&a\nr.a+=r.a\nlet b=r.a\nr.a=b}",
        "let L=struct{a:[]int,}\nlet main:()={let mut a=[L[.a=[1]]]\nlet r=&a[0]\nr.a+=r.a\nlet b=r.a\nr.a=b}",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "append-rhs-ir.nov", source);
        defer module.deinit();
        var result = try lower(&module);
        defer result.deinit();
        try std.testing.expect(result.lowering_complete);
        const body = result.ir.functions.items[0].body.?;
        var appends: usize = 0;
        var snapshots: usize = 0;
        for (body.instructions) |instruction| switch (instruction.op) {
            .array_append => |append| {
                appends += 1;
                try std.testing.expect(body.instructions[@backingInt(append.lhs)].op == .array_clone);
                const rhs = body.instructions[@backingInt(append.rhs)].op;
                try std.testing.expect(rhs == .local_load or rhs == .field_read or rhs == .borrow_read);
            },
            .array_clone => snapshots += 1,
            else => {},
        };
        try std.testing.expectEqual(@as(usize, 1), appends);
        // Pre-RHS LHS, ordinary value binding, and plain assignment each detach.
        try std.testing.expectEqual(@as(usize, 3), snapshots);
        try result.ir.verify();
    }
}

test "static borrowing normal array loads clone immediately before later borrow effects" {
    var module = try Module.init(std.testing.allocator, "borrow-snapshot-ir.nov", "let R=struct{x:int=0,}\nlet main:()={let mut a=[R[],R[]]\nlet before=a\nlet value=a[{let r=&a[0]\nr.x=9\n0}]\na+={let r=&a[0]\nr.x=8\n[R[]]}\na[{let r=&a[0]\nr.x=7\n1}]={let r=&a[0]\nr.x=6\nR[]}\n@println(value.x,before[0].x,a[0].x)}");
    defer module.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(result.lowering_complete);
    const body = result.ir.functions.items[0].body.?;
    var loads: usize = 0;
    var snapshots: usize = 0;
    var reservations: usize = 0;
    var updates: usize = 0;
    var appends: usize = 0;
    for (body.instructions, 0..) |instruction, i| switch (instruction.op) {
        .local_load => {
            if (result.ir.getType(instruction.ty).? != .array) continue;
            loads += 1;
            const next = body.instructions[i + 1];
            if (next.op == .array_clone) {
                snapshots += 1;
                try std.testing.expectEqual(@as(Ir.ValueId, @fromBackingInt(@intCast(i))), next.op.array_clone);
            }
        },
        .array_index => |access| {
            const base = body.instructions[@backingInt(access.array)].op;
            try std.testing.expect(base == .array_clone or base == .local_load);
        },
        .array_set => |update| {
            updates += 1;
            try std.testing.expect(body.instructions[@backingInt(update.array)].op == .array_clone);
        },
        .array_append => |append| {
            appends += 1;
            try std.testing.expect(body.instructions[@backingInt(append.lhs)].op == .array_clone);
        },
        .borrow_reserve => |owner| {
            reservations += 1;
            // Borrowing names the actual owner slot, never one of these copies.
            try std.testing.expect(body.instructions[@backingInt(owner)].op == .local_slot);
        },
        else => {},
    };
    try std.testing.expect(loads > snapshots);
    try std.testing.expectEqual(@as(usize, 4), snapshots);
    try std.testing.expectEqual(@as(usize, 4), reservations);
    try std.testing.expectEqual(@as(usize, 1), updates);
    try std.testing.expectEqual(@as(usize, 1), appends);
    try result.ir.verify();
}

fn testExternOwnership(allocator: std.mem.Allocator) !void {
    var sema = blk: {
        var module = try Module.init(allocator, "extern.nov", "@[extern(\"nov_\\x61bs\")]\nlet foreign: (x: i32) -> i32\n@[public]\n@[extern]\nlet predicate: (x: bool) -> bool\n@[noreturn]\n@[extern]\nlet abort: ()\nlet call: (x: i32) -> i32 = foreign(x)\nlet indirect: (x: i32) -> i32 = { let f = foreign\nf(x) }\nlet stop: () = abort()");
        defer module.deinit();

        var result = lower(&module) catch |err| {
            try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        errdefer result.deinit();
        try std.testing.expect(!module.hasErrors());
        try std.testing.expectEqual(Sema.Status.external, result.getEntry(module.lookup("foreign").?).?.status);
        break :blk result;
    };
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    try std.testing.expectEqualStrings("nov_abs", sema.ir.functions.items[0].extern_name.?);
    try std.testing.expectEqualStrings("predicate", sema.ir.functions.items[1].extern_name.?);
    try std.testing.expect(!sema.ir.functions.items[0].public);
    try std.testing.expect(sema.ir.functions.items[1].public);
    try std.testing.expect(sema.ir.functions.items[2].noreturn);
    try sema.ir.verify();
}

test "extern bodyless typed direct and indirect calls own names after source release under OOM" {
    try testExternOwnership(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testExternOwnership, .{});
}

fn testCompilationExternOwnership(allocator: std.mem.Allocator) !void {
    var sema = blk: {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/app/root.nov", "let imported = @import(\"lib.nov\")\n@[extern(\"shared_symbol\")]\nlet local: (x: i32) -> i32\nlet caller: (x: i32) -> i32 = imported.foreign(x)");
        const imported = try comp.addSource("/app/lib.nov", "@[public]\n@[extern(\"shared_symbol\")]\nlet foreign: (x: i32) -> i32");
        try testImport(&comp, root, "imported", imported, false);
        var result = lowerCompilation(&comp) catch |err| {
            for (comp.modules.items) |module| try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
            return err;
        };
        errdefer result.deinit();
        try std.testing.expect(!comp.hasErrors());
        break :blk result;
    };
    defer sema.deinit();
    try std.testing.expect(sema.lowering_complete);
    var count: usize = 0;
    for (sema.ir.functions.items) |function| if (function.extern_name) |name| {
        try std.testing.expectEqualStrings("shared_symbol", name);
        count += 1;
    };
    try std.testing.expectEqual(@as(usize, 2), count);
    try sema.ir.verify();
}

test "extern compilation aliases and imported typed calls own names under OOM" {
    try testCompilationExternOwnership(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testCompilationExternOwnership, .{});
}

fn testSemanticTestModes(allocator: std.mem.Allocator) !void {
    var sema = blk: {
        var comp = Compilation.init(allocator);
        defer comp.deinit();
        const root = try comp.addSource("/app/tests.nov",
            \\let library = @import("library.nov")
            \\@[test]
            \\let main: () = {}
            \\@[public]
            \\@[test]
            \\let second: () -> void = {}
        );
        comp.setRoot(root);
        const imported = try comp.addSource("/app/library.nov",
            \\@[public]
            \\@[test]
            \\let ignored: (x: Missing) -> Missing = unknown
        );
        try testImport(&comp, root, "library", imported, false);
        const module = comp.getModule(root).?;
        // Even an accidentally enabled imported module must stay excluded.
        comp.getModule(imported).?.tests_enabled = true;
        try std.testing.expect(comp.root_main == null);
        for ([_]bool{ false, true, false, true }) |enabled| {
            module.tests_enabled = enabled;
            var result = try lowerCompilation(&comp);
            defer result.deinit();
            try std.testing.expect(!comp.hasErrors());
            try std.testing.expect(result.lowering_complete);
            try std.testing.expectEqual(@as(usize, if (enabled) 2 else 0), result.ir.functions.items.len);
            const first = result.entries[result.entryIndex(module, module.lookup("main").?)];
            try std.testing.expectEqual(if (enabled) Sema.Status.lowered else .ignored, first.status);
            try std.testing.expectEqual(enabled, first.function != null);
            const library = comp.getModule(imported).?;
            try std.testing.expectEqual(Sema.Status.ignored, result.entries[result.entryIndex(library, library.lookup("ignored").?)].status);
        }
        break :blk try lowerCompilation(&comp);
    };
    defer sema.deinit();
    try sema.ir.verify();
    try std.testing.expectEqualStrings("main", sema.ir.functions.items[0].name);
    try std.testing.expectEqualStrings("second", sema.ir.functions.items[1].name);
    for (sema.ir.functions.items) |function| try std.testing.expect(function.is_test);
}

test "semantic tests are root only source ordered source free and repeatable under OOM" {
    try testSemanticTestModes(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testSemanticTestModes, .{});
}

fn testSignatureImports(allocator: std.mem.Allocator, path: []const u8, enabled: bool, invalid_root: bool) !void {
    var sema = blk: {
        var comp = try Compilation.load(allocator, std.testing.io, path, .{});
        defer comp.deinit();
        try std.testing.expect(!comp.hasErrors());
        try std.testing.expectEqual(@as(usize, 2), comp.modules.items.len);
        try std.testing.expectEqual(@as(usize, 1), comp.imports.items.len);
        const root = comp.getModule(comp.root.?).?;
        root.tests_enabled = enabled;
        var result = try lowerCompilation(&comp);
        errdefer result.deinit();
        // Missing relative and named imports inside signatures never become
        // graph edges, even when Sema rejects an enabled root signature.
        try std.testing.expectEqual(@as(usize, 0), comp.diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 2), comp.modules.items.len);
        try std.testing.expectEqual(@as(usize, 1), comp.imports.items.len);
        for (comp.modules.items) |module| {
            if (module == root) continue;
            try std.testing.expect(!module.hasErrors());
            for (module.declarations.items, 0..) |_, i|
                try std.testing.expectEqual(Sema.Status.ignored, result.entries[result.entryIndex(module, @fromBackingInt(@intCast(i)))].status);
        }
        if (enabled and invalid_root) {
            try std.testing.expect(root.hasErrors());
            try std.testing.expect(!result.lowering_complete);
            for (root.diagnostics.items) |diagnostic| {
                try std.testing.expectEqual(root.source.id, diagnostic.span.source);
                try std.testing.expectEqual(Module.Diagnostic.Tag.unknown_type, diagnostic.tag);
            }
        } else {
            try std.testing.expect(!comp.hasErrors());
            try std.testing.expect(result.lowering_complete);
            try std.testing.expectEqual(@as(usize, if (enabled) 2 else 1), result.ir.functions.items.len);
        }
        break :blk result;
    };
    defer sema.deinit();
    if (sema.lowering_complete) try sema.ir.verify();
}

test "test signature imports stay outside source graph in root and dependencies under OOM" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "invalid-root.nov", .data =
        \\let library = @import("library.nov")
        \\@[test]
        \\let relative: (x: @import("missing.nov").T) = {}
        \\@[test]
        \\let named: () -> @import("std").T = {}
        \\let main: () = {}
    });
    try tmp.dir.writeFile(io, .{ .sub_path = "valid-root.nov", .data =
        \\let library = @import("library.nov")
        \\@[test]
        \\let valid: () = {}
        \\let main: () = {}
    });
    try tmp.dir.writeFile(io, .{ .sub_path = "library.nov", .data =
        \\@[test]
        \\let relative: (x: @import("missing-dependency.nov").T) = {}
        \\@[test]
        \\let named: () -> @import("std").T = {}
    });
    for ([_]bool{ false, true }) |invalid_root| {
        const path = try tmp.dir.realPathFileAlloc(io, if (invalid_root) "invalid-root.nov" else "valid-root.nov", allocator);
        defer allocator.free(path);
        for ([_]bool{ false, true }) |enabled| {
            try testSignatureImports(allocator, path, enabled, invalid_root);
            try TestAllocations.checkAllAllocationFailures(allocator, testSignatureImports, .{ path, enabled, invalid_root });
        }
    }
}

fn testIgnoredTestBody(allocator: std.mem.Allocator) !void {
    var module = try Module.init(allocator, "ignored-tests.nov",
        \\let Box: (T: type) -> type = struct { value: T, }
        \\@[test]
        \\let ignored: (x: Missing) -> Missing = {
        \\let x: Box(Unknown) = missing
        \\let library = @import("does-not-exist.nov")
        \\unknown()
        \\}
        \\let main: () = {}
    );
    defer module.deinit();
    var dependencies = try Dependencies.analyze(&module);
    defer dependencies.deinit();
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(!module.hasErrors());
    try std.testing.expect(result.lowering_complete);
    try std.testing.expectEqual(Sema.Status.ignored, result.getEntry(module.lookup("ignored").?).?.status);
    try std.testing.expectEqual(@as(usize, 1), result.ir.functions.items.len);
    try std.testing.expect(!result.ir.functions.items[0].is_test);
    try std.testing.expectEqual(@as(usize, 3), module.declarations.items.len);
}

test "ordinary builds ignore test signatures bodies and generic demands under OOM" {
    try testIgnoredTestBody(std.testing.allocator);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testIgnoredTestBody, .{});
}

fn testRejectedTest(allocator: std.mem.Allocator, source: []const u8, enabled: bool, expected: Module.Diagnostic.Tag) !void {
    var module = try Module.init(allocator, "rejected-test.nov", source);
    defer module.deinit();
    module.tests_enabled = enabled;
    var result = try lower(&module);
    defer result.deinit();
    try std.testing.expect(module.hasErrors());
    try std.testing.expect(!result.lowering_complete);
    try std.testing.expectEqual(expected, module.diagnostics.items[0].tag);
}

test "enabled tests validate signatures free names and reject ordinary references in both modes" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "@[test]\nlet sample: (x: i32) = {}", .tag = .invalid_test_signature },
        .{ .source = "@[test]\nlet sample: () -> i32 = 1", .tag = .invalid_test_signature },
        .{ .source = "let Box: (T: type) -> type = struct { value: T, }\n@[test]\nlet sample: () = {\nlet x: Box(Missing) = missing\n}", .tag = .unknown_type },
        .{ .source = "@[test]\nlet sample: () = unknown", .tag = .unknown_name },
    }) |case| {
        try testRejectedTest(std.testing.allocator, case.source, true, case.tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedTest, .{ case.source, true, case.tag });
    }
    const test_call = "@[test]\nlet first: () = {}\n@[test]\nlet second: () = first()";
    try testRejectedTest(std.testing.allocator, test_call, true, .test_not_callable);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedTest, .{ test_call, true, Module.Diagnostic.Tag.test_not_callable });
    for ([_]bool{ false, true }) |enabled| {
        for ([_][]const u8{
            "@[public]\n@[test]\nlet sample: () = {}\nlet ordinary: () = sample()",
            "@[test]\nlet sample: () = {}\nlet ordinary: () -> () -> void = sample",
        }) |source| {
            try testRejectedTest(std.testing.allocator, source, enabled, .test_not_callable);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testRejectedTest, .{ source, enabled, Module.Diagnostic.Tag.test_not_callable });
        }
    }
}

test "extern typed calls reject wrong arguments results and noreturn results" {
    for ([_]struct { source: []const u8, tag: Module.Diagnostic.Tag }{
        .{ .source = "@[extern]\nlet foreign: (x: i32) -> i32\nlet call: () -> i32 = foreign(true)", .tag = .literal_type_mismatch },
        .{ .source = "@[extern]\nlet foreign: (x: i32) -> i32\nlet call: () -> i32 = foreign()", .tag = .argument_count_mismatch },
        .{ .source = "@[extern]\nlet foreign: () -> i32\nlet call: () -> bool = foreign()", .tag = .expression_type_mismatch },
        .{ .source = "@[extern]\n@[noreturn]\nlet foreign: () -> i32", .tag = .noreturn_result },
    }) |case| {
        var module = try Module.init(std.testing.allocator, "bad-extern-call.nov", case.source);
        defer module.deinit();
        var sema = try lower(&module);
        defer sema.deinit();
        try std.testing.expect(!sema.lowering_complete);
        try std.testing.expectEqual(case.tag, module.diagnostics.items[0].tag);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, testAllocationFailures, .{case.source});
    }
}
