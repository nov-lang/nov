const std = @import("std");
const Compilation = @import("Compilation.zig");
const Lowering = @import("Lowering.zig");
const Module = @import("Module.zig");
const Sema = @import("Sema.zig");
const Source = @import("Source.zig");
const CEmitter = @import("CEmitter.zig");
const TestAllocations = @import("TestAllocations.zig");

const Case = struct {
    library: []const u8,
    root: []const u8,
    facade: []const u8 = "",
    named: bool = false,
    rejected: bool = false,
};

fn addImport(comp: *Compilation, source: Source.Id, name: []const u8, target: Source.Id, named: bool) !void {
    const module = comp.getModule(source).?;
    const local = module.lookup(name).?;
    try comp.imports.append(comp.allocator, .{
        .declaration = comp.declaration_ids.get(.{ .source = source, .local = local }).?,
        .target = target,
        .named = named,
        .span = module.getDeclaration(local).?.name_span,
    });
}

// Return the semantic owner only after its sources, ASTs and import edges die.
fn lowerCase(allocator: std.mem.Allocator, case: Case) !Sema {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root_text = try std.mem.concat(allocator, u8, &.{ "let lib=@import(\"library.nov\")\nlet api=@import(\"facade.nov\")\n", case.root });
    defer allocator.free(root_text);
    const facade_text = try std.mem.concat(allocator, u8, &.{ "let lib=@import(\"library.nov\")\n@[public]\nlet ns=lib\n", case.facade });
    defer allocator.free(facade_text);
    const root = try comp.addSource("/visibility/main.nov", root_text);
    const library = try comp.addSource("/visibility/library.nov", case.library);
    const facade = try comp.addSource("/visibility/facade.nov", facade_text);
    try addImport(&comp, root, "lib", library, case.named);
    try addImport(&comp, root, "api", facade, case.named);
    try addImport(&comp, facade, "lib", library, false);
    comp.setRoot(root);
    for (comp.modules.items) |module| {
        try std.testing.expectEqual(@as(usize, 0), module.source.ast.errors.len);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
    }
    var result = Lowering.lowerCompilation(&comp) catch |err| {
        if (err == error.OutOfMemory) for (comp.modules.items) |module| {
            try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        };
        return err;
    };
    errdefer result.deinit();
    if (comp.hasErrors() != case.rejected) {
        std.debug.print("visibility case: {s}\n", .{case.root});
        for (comp.modules.items) |module| for (module.diagnostics.items) |d| {
            std.debug.print("{s}: {t}: {s}\n", .{ module.source.path, d.tag, comp.sourceSlice(d.span) orelse "?" });
        };
    }
    try std.testing.expectEqual(case.rejected, comp.hasErrors());
    try std.testing.expectEqual(!case.rejected, result.lowering_complete);
    if (case.rejected) {
        try std.testing.expectEqual(@as(usize, 0), result.ir.functions.items.len);
        var relevant = false;
        for (comp.modules.items) |module| for (module.diagnostics.items) |d| {
            try std.testing.expectEqual(module.source.id, d.span.source);
            try std.testing.expect((comp.sourceSlice(d.span) orelse return error.TestUnexpectedResult).len != 0);
            if (d.related) |span| try std.testing.expect(comp.sourceSlice(span) != null);
            switch (d.tag) {
                .unknown_name,
                .unresolved_value_type,
                .unknown_type,
                .unknown_field,
                .not_a_type,
                .not_a_value,
                .not_callable,
                .not_a_generic,
                .unsupported_body_expression,
                .unsupported_interpolation_type,
                .invalid_to_string,
                .unsupported_dependency_expression,
                .unsupported_name,
                => if (module.source.id == root) {
                    relevant = true;
                },
                else => {},
            }
        };
        if (!relevant) {
            std.debug.print("missing visibility diagnostic: {s}\n", .{case.root});
            for (comp.modules.items) |module| for (module.diagnostics.items) |d| {
                std.debug.print("{t}: {s}\n", .{ d.tag, comp.sourceSlice(d.span) orelse "?" });
            };
        }
        try std.testing.expect(relevant);
    } else {
        try result.ir.verify();
        const main = result.getCompilationEntry(comp.root_main.?).?.function.?;
        try std.testing.expectEqual(root, result.ir.getFunction(main).?.span.source);
        var library_function = false;
        for (result.ir.functions.items) |function| {
            try std.testing.expect(comp.sourceSlice(function.span) != null);
            if (function.span.source == library) library_function = true;
        }
        try std.testing.expect(library_function);
    }
    return result;
}

fn checkCase(allocator: std.mem.Allocator, case: Case) !void {
    var result = try lowerCase(allocator, case);
    defer result.deinit();
    try result.ir.verify();
}

const private_declarations =
    \\let value:int=7
    \\let secret:()->int=value
    \\let Hidden=struct{value:int,}
    \\let Kind=enum{one,two,}
    \\let Choice=union{some:int,none:void,}
    \\let Box:(T:type)->type=struct{value:T,}
    \\let Option:(T:type)->type=union{some:T,none:void,}
    \\@[public]
    \\let exposedValue:()->int=value
    \\@[public]
    \\let exposedFunction:()->int=secret()
    \\@[public]
    \\let Exposed=Hidden
    \\@[public]
    \\let ExposedKind=Kind
    \\@[public]
    \\let ExposedChoice=Choice
    \\@[public]
    \\let ExposedBox=Box
    \\@[public]
    \\let ExposedOption=Option
    \\@[public]
    \\let wrapper:()->int={let h:Hidden=Hidden[.value=secret()]
    \\let b:Box(int)=Box(int)[.value=h.value]
    \\let k:Kind=.one
    \\let u:Choice=.some[b.value]
    \\let o:Option(int)=.some[value]
    \\b.value}
;

const declaration_reexports =
    \\@[public]
    \\let value:()->int=lib.exposedValue()
    \\@[public]
    \\let function:()->int=lib.exposedFunction()
    \\@[public]
    \\let Type=lib.Exposed
    \\@[public]
    \\let Kind=lib.ExposedKind
    \\@[public]
    \\let Choice=lib.ExposedChoice
    \\@[public]
    \\let Box=lib.ExposedBox
    \\@[public]
    \\let Option=lib.ExposedOption
;

test "visibility private top level declarations reject direct and namespace alias access" {
    for ([_]bool{ false, true }) |named| {
        for ([_][]const u8{ "lib", "api.ns" }) |path| {
            for ([_][]const u8{
                "let main:()=@println(PATH.value)",
                "let main:()=@println(PATH.secret())",
                "let f=PATH.secret\nlet main:()=@println(f())",
                "let H=PATH.Hidden\nlet main:()={let h:H=H[.value=1]\n}",
                "let main:()={let h:PATH.Hidden=PATH.Hidden[.value=1]\n}",
                "let K=PATH.Kind\nlet main:()={let k:K=.one\n}",
                "let U=PATH.Choice\nlet main:()={let u:U=.some[1]\n}",
                "let G=PATH.Box\nlet main:()={let g:G(int)=G(int)[.value=1]\n}",
                "let main:()={let g:PATH.Box(int)=PATH.Box(int)[.value=1]\n}",
                "let G=PATH.Option\nlet main:()={let g:G(int)=.some[1]\n}",
            }) |template| {
                const root = try std.mem.replaceOwned(u8, std.testing.allocator, template, "PATH", path);
                defer std.testing.allocator.free(root);
                try checkCase(std.testing.allocator, .{ .library = private_declarations, .root = root, .named = named, .rejected = true });
            }
        }
    }
}

test "visibility unmarked reexports stay private in both import modes" {
    const facade = declaration_reexports ++ "\nlet Local=lib.Exposed\nlet LocalBox=lib.ExposedBox\nlet localRun:()->int=lib.wrapper()";
    for ([_]bool{ false, true }) |named| {
        for ([_][]const u8{
            "let H=api.Local\nlet main:()={let h:H=H[.value=1]\n}",
            "let G=api.LocalBox\nlet main:()={let g:G(int)=G(int)[.value=1]\n}",
            "let main:()=@println(api.localRun())",
            "let main:()=@println(api.lib.wrapper())",
        }) |root| try checkCase(std.testing.allocator, .{
            .library = private_declarations,
            .facade = facade,
            .root = root,
            .named = named,
            .rejected = true,
        });
    }
}

test "visibility public type aliases reexports and wrappers expose private local declarations" {
    for ([_]bool{ false, true }) |named| {
        try checkCase(std.testing.allocator, .{
            .library = private_declarations,
            .facade = declaration_reexports,
            .named = named,
            .root =
            \\let H=api.Type
            \\let G=api.Box
            \\let main:()={let h:H=H[.value=api.value()]
            \\let b:G(int)=G(int)[.value=h.value]
            \\let k:api.Kind=.one
            \\let u:api.Choice=.some[b.value]
            \\let o:api.Option(int)=.some[api.function()]
            \\@println(lib.exposedValue(),lib.exposedFunction(),lib.wrapper(),b.value)}
            ,
        });
    }
}

const Shape = struct { declaration: []const u8, concrete: []const u8, constructor: []const u8 };
const shapes = [_]Shape{
    .{ .declaration = "let Thing=enum{one,two,\n", .concrete = "Thing", .constructor = ".one" },
    .{ .declaration = "let Thing=struct{value:int,\n", .concrete = "Thing", .constructor = "Thing[.value=1]" },
    .{ .declaration = "let Thing=union{some:int,none:void,\n", .concrete = "Thing", .constructor = ".some[1]" },
    .{ .declaration = "let Thing:(T:type)->type=struct{value:T,\n", .concrete = "Thing(int)", .constructor = "Thing(int)[.value=1]" },
    .{ .declaration = "let Thing:(T:type)->type=union{some:T,none:void,\n", .concrete = "Thing(int)", .constructor = ".some[1]" },
};

fn methodLibrary(allocator: std.mem.Allocator, shape: Shape, public_methods: bool) ![]const u8 {
    return std.mem.concat(allocator, u8, &.{
        "let free:int=7\nlet helper:()->int=free\n@[public]\n",
        shape.declaration,
        "let Self=@This()\n",
        if (public_methods) "@[public]\n" else "",
        "let secret:()->int=helper()\n",
        if (public_methods) "@[public]\n" else "",
        "let get:(self:Self)->int=helper()\n",
        if (public_methods) "@[public]\n" else "",
        "let toString:(self:Self)->string=\"private\"\n",
        "@[public]\nlet wrapper:(self:Self)->int=Self.secret()+self.get()\n",
        if (public_methods) "@[public]\n" else "",
        "let unmarked:(self:Self)->int=self.get()\n}\n",
        "@[public]\nlet Alias=Thing\n@[public]\nlet run:()->int={let x:",
        shape.concrete,
        "=",
        shape.constructor,
        "\n@println(x,\"{x}\")\nlet f:()->int=",
        shape.concrete,
        ".secret\nf()+x.get()}\n",
        // File privacy is not container privacy: a sibling container can call it.
        "let Other=struct{\nlet call:()->int={let x:",
        shape.concrete,
        "=",
        shape.constructor,
        "\n",
        shape.concrete,
        ".secret()+x.get()}\n}\n",
    });
}

const method_reexports = "@[public]\nlet Thing=lib.Alias\n@[public]\nlet run:()->int=lib.run()";

test "visibility private methods allow defining file calls and public wrappers" {
    for (shapes) |shape| {
        const library = try methodLibrary(std.testing.allocator, shape, false);
        defer std.testing.allocator.free(library);
        const root = try std.mem.concat(std.testing.allocator, u8, &.{
            "let Thing=api.Thing\nlet main:()={let x:", shape.concrete, "=", shape.constructor,
            "\n@println(x.wrapper(),api.run())}",
        });
        defer std.testing.allocator.free(root);
        for ([_]bool{ false, true }) |named| try checkCase(std.testing.allocator, .{
            .library = library,
            .facade = method_reexports,
            .root = root,
            .named = named,
        });
    }
}

test "visibility explicit public static receiver extracted and toString methods work across files" {
    for (shapes) |shape| {
        const library = try methodLibrary(std.testing.allocator, shape, true);
        defer std.testing.allocator.free(library);
        const root = try std.mem.concat(std.testing.allocator, u8, &.{
            "let Thing=api.Thing\nlet main:()={let x:", shape.concrete, "=",                                                                                    shape.constructor,
            "\nlet f:()->int=",                         shape.concrete, ".secret\nlet get:(",                                                                   shape.concrete,
            ")->int=",                                  shape.concrete, ".get\nlet text:(",                                                                     shape.concrete,
            ")->string=",                               shape.concrete, ".toString\n@println(f(),get(x),text(x),x.get(),x.unmarked(),x.toString(),x,\"{x}\")}",
        });
        defer std.testing.allocator.free(root);
        for ([_]bool{ false, true }) |named| try checkCase(std.testing.allocator, .{
            .library = library,
            .facade = method_reexports,
            .root = root,
            .named = named,
        });
    }
}

test "visibility private static receiver toString and method extraction stay hidden through aliases" {
    for (shapes) |shape| {
        const library = try methodLibrary(std.testing.allocator, shape, false);
        defer std.testing.allocator.free(library);
        for ([_][]const u8{ "lib.Thing", "lib.Alias", "api.Thing", "api.ns.Thing" }) |path| {
            for ([_][]const u8{
                "@println(TYPE.secret())",
                "@println(x.get())",
                "@println(x.unmarked())",
                "let f:(TYPE)->int=TYPE.unmarked\n@println(f(x))",
                "@println(x.toString())",
                "@println(x)",
                "@println(\"{x}\")",
                "let f:()->int=TYPE.secret\n@println(f())",
                "let f:(TYPE)->int=TYPE.get\n@println(f(x))",
                "let f:(TYPE)->string=TYPE.toString\n@println(f(x))",
            }) |expression| {
                const call = try std.mem.replaceOwned(u8, std.testing.allocator, expression, "TYPE", shape.concrete);
                defer std.testing.allocator.free(call);
                const root = try std.mem.concat(std.testing.allocator, u8, &.{
                    "let Thing=", path, "\nlet main:()={let x:", shape.concrete, "=", shape.constructor, "\n", call, "}",
                });
                defer std.testing.allocator.free(root);
                for ([_]bool{ false, true }) |named| try checkCase(std.testing.allocator, .{
                    .library = library,
                    .facade = method_reexports,
                    .root = root,
                    .named = named,
                    .rejected = true,
                });
            }
        }
    }
}

test "visibility returned private nominal values retain method visibility" {
    const library =
        \\let Hidden=struct{value:int,
        \\let get:(self:Hidden)->int=self.value
        \\let toString:(self:Hidden)->string="hidden"
        \\@[public]
        \\let visible:(self:Hidden)->int=self.get()
        \\}
        \\@[public]
        \\let make:()->Hidden=Hidden[.value=7]
        \\@[public]
        \\let wrapper:()->int={let x=make()
        \\let f:(Hidden)->int=Hidden.get
        \\@println(x,"{x}",x.toString())
        \\f(x)+x.get()}
    ;
    for ([_]bool{ false, true }) |named| {
        try checkCase(std.testing.allocator, .{
            .library = library,
            .root = "let main:()={let x=lib.make()\n@println(x.visible(),lib.wrapper())}",
            .named = named,
        });
        for ([_][]const u8{
            "@println(x.get())",
            "let f=x.get\n@println(f(x))",
            "@println(x.toString())",
            "let f=x.toString\n@println(f(x))",
            "@println(x)",
            "@println(\"{x}\")",
        }) |expression| {
            const root = try std.mem.concat(std.testing.allocator, u8, &.{
                "let main:()={let x=lib.make()\n", expression, "}",
            });
            defer std.testing.allocator.free(root);
            try checkCase(std.testing.allocator, .{ .library = library, .root = root, .named = named, .rejected = true });
        }
    }
}

test "visibility namespace aliases retain member privacy despite public and private alias hops" {
    const library =
        \\let hidden:int=7
        \\@[public]
        \\let visible:()->int=hidden
    ;
    const facade =
        \\let local=lib
        \\@[public]
        \\let exported=local
        \\let again=exported
        \\@[public]
        \\let chain=again
    ;
    for ([_][]const u8{ "api.ns", "api.exported", "api.chain" }) |path| {
        for ([_]bool{ false, true }) |rejected| {
            const root = try std.mem.concat(std.testing.allocator, u8, &.{
                "let main:()=@println(", path, if (rejected) ".hidden)" else ".visible())",
            });
            defer std.testing.allocator.free(root);
            for ([_]bool{ false, true }) |named| try checkCase(std.testing.allocator, .{ .library = library, .facade = facade, .root = root, .named = named, .rejected = rejected });
        }
    }
}

fn ownedCase(allocator: std.mem.Allocator, case: Case) !void {
    var result = try lowerCase(allocator, case);
    defer result.deinit();
    try result.ir.verify();
    if (!case.rejected) {
        const c = try CEmitter.emitProgram(allocator, &result.ir, null);
        defer allocator.free(c);
        try std.testing.expect(c.len != 0);
        try std.testing.expect(std.mem.indexOf(u8, c, "main") != null);
    }
}

fn aliasVisibility(allocator: std.mem.Allocator, named: bool) !void {
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    const root = try comp.addSource("/visibility/main.nov",
        \\let lib=@import("library.nov")
        \\let api=@import("facade.nov")
        \\let value=api.value
        \\let function=api.function
        \\let Type=api.Type
        \\let blocked=api.ns.secret
    );
    const library = try comp.addSource("/visibility/library.nov",
        \\let secret:int=7
        \\let hidden:()->int=secret
        \\let Hidden=struct{value:int,}
        \\@[public]
        \\let value=secret
        \\@[public]
        \\let function=hidden
        \\@[public]
        \\let Type=Hidden
    );
    const facade = try comp.addSource("/visibility/facade.nov",
        \\let lib=@import("library.nov")
        \\@[public]
        \\let ns=lib
        \\@[public]
        \\let value=lib.value
        \\@[public]
        \\let function=lib.function
        \\@[public]
        \\let Type=lib.Type
    );
    try addImport(&comp, root, "lib", library, named);
    try addImport(&comp, root, "api", facade, named);
    try addImport(&comp, facade, "lib", library, false);
    comp.setRoot(root);
    const root_module = comp.getModule(root).?;
    const owner = comp.getModule(library).?;
    const api = comp.getModule(facade).?;
    for (comp.modules.items) |module| try std.testing.expect(!module.hasErrors());
    const edge = comp.imports.items[0].declaration;
    for ([_][]const u8{ "secret", "hidden", "Hidden" }) |name| {
        try std.testing.expect(comp.lookupNamespace(root, edge, name) == null);
        const local = comp.lookupNamespace(library, edge, name).?;
        try std.testing.expectEqual(library, comp.getDeclaration(local).?.source);
        try std.testing.expectEqual(owner.lookup(name).?, comp.getDeclaration(local).?.local);
    }
    // Value/function aliases are not lowered by the current bounded pipeline.
    // Visibility itself must still allow them, without chasing their private
    // initializer using the importing file as caller.
    for ([_][]const u8{ "value", "function", "Type" }, [_][]const u8{ "secret", "hidden", "Hidden" }) |name, private_name| {
        const probe = root_module.getDeclaration(root_module.lookup(name).?).?;
        const ref = comp.reference(root_module, root_module.source.ast.decl(probe.node).ast.init_node).?;
        try std.testing.expectEqual(facade, ref.source);
        try std.testing.expectEqual(api.lookup(name).?, ref.local);
        const reexport = api.getDeclaration(ref.local).?;
        const exported = comp.reference(api, api.source.ast.decl(reexport.node).ast.init_node).?;
        try std.testing.expectEqual(library, exported.source);
        const alias = owner.getDeclaration(exported.local).?;
        const private_ref = comp.reference(owner, owner.source.ast.decl(alias.node).ast.init_node).?;
        try std.testing.expectEqual(library, private_ref.source);
        try std.testing.expectEqual(owner.lookup(private_name).?, private_ref.local);
    }
    const blocked = root_module.getDeclaration(root_module.lookup("blocked").?).?;
    try std.testing.expect(comp.reference(root_module, root_module.source.ast.decl(blocked.node).ast.init_node) == null);
}

test "visibility public local value function and type aliases retain defining file access" {
    for ([_]bool{ false, true }) |named| {
        try aliasVisibility(std.testing.allocator, named);
        try TestAllocations.checkAllAllocationFailures(std.testing.allocator, aliasVisibility, .{named});
    }
}

test "visibility semantic ownership diagnostics and imports survive allocation failure sweeps" {
    const library = try methodLibrary(std.testing.allocator, shapes[3], false);
    defer std.testing.allocator.free(library);
    for ([_]Case{
        .{ .library = library, .facade = method_reexports, .named = true, .root = "let main:()=@println(api.run())" },
        .{ .library = library, .facade = method_reexports, .root = "let G=api.Thing\nlet main:()=@println(G(int).secret())", .rejected = true },
        .{ .library = library, .root = "let G=lib.Thing\nlet main:()={let x:G(int)=G(int)[.value=1]\n@println(x)}", .rejected = true },
        .{ .library = private_declarations, .facade = declaration_reexports, .named = true, .root = "let main:()=@println(api.function(),api.value(),lib.wrapper())" },
        .{ .library = private_declarations, .root = "let main:()=@println(api.ns.secret())", .rejected = true },
    }) |case| {
        // Exercise the emitter after teardown as well for the method fixture.
        if (std.mem.eql(u8, case.library, library)) {
            try ownedCase(std.testing.allocator, case);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, ownedCase, .{case});
        } else {
            try checkCase(std.testing.allocator, case);
            try TestAllocations.checkAllAllocationFailures(std.testing.allocator, checkCase, .{case});
        }
    }
}
