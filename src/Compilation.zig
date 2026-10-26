const std = @import("std");
const TestAllocations = @import("TestAllocations.zig");
const Source = @import("Source.zig");
const Module = @import("Module.zig");
const Ir = @import("Ir.zig");
const string_literal = @import("string_literal.zig");
const Literal = @import("Literal.zig");
const Ast = @import("Ast.zig");
const Compilation = @This();

allocator: std.mem.Allocator,
/// Individually allocated owners keep ASTs and defining scopes stable while the
/// graph grows. Namespace edges never copy declarations into an importing scope.
modules: std.ArrayList(*Module) = .empty,
paths: std.StringHashMapUnmanaged(Source.Id) = .empty,
declarations: std.ArrayList(DeclRef) = .empty,
declaration_ids: std.AutoHashMapUnmanaged(DeclRef, DeclId) = .empty,
imports: std.ArrayList(Import) = .empty,
diagnostics: std.ArrayList(Diagnostic) = .empty,
/// One pool for the eventual whole-compilation semantic worklist, not per-file IR.
ir: Ir,
root: ?Source.Id = null,
root_main: ?DeclId = null,

pub const DeclId = enum(u32) { _ };
pub const DeclRef = struct { source: Source.Id, local: Module.DeclId };
pub const Import = struct {
    declaration: DeclId,
    target: Source.Id,
    named: bool,
    span: Source.Span,
};
pub const Diagnostic = struct {
    tag: enum { invalid_import, invalid_path, missing_library, load_failed, import_cycle, import_depth },
    span: Source.Span,
    related: ?Source.Span = null,
    cause: ?anyerror = null,
};
pub const Options = struct {
    /// An explicit library root; callers may derive it from the installed
    /// executable's ../lib/nov. Never inferred from the process working directory.
    lib_dir: ?[]const u8 = null,
    max_source_bytes: usize = 16 * 1024 * 1024,
};
pub const Error = Module.InitError || error{TooManySources};

pub fn init(allocator: std.mem.Allocator) Compilation {
    return .{ .allocator = allocator, .ir = Ir.init(allocator) };
}

pub fn deinit(self: *Compilation) void {
    self.ir.deinit();
    self.paths.deinit(self.allocator);
    self.declaration_ids.deinit(self.allocator);
    self.declarations.deinit(self.allocator);
    self.imports.deinit(self.allocator);
    self.diagnostics.deinit(self.allocator);
    for (self.modules.items) |module| {
        module.deinit();
        self.allocator.destroy(module);
    }
    self.modules.deinit(self.allocator);
    self.* = undefined;
}

pub fn getModule(self: *const Compilation, id: Source.Id) ?*Module {
    const index = @backingInt(id);
    if (index >= self.modules.items.len) return null;
    return self.modules.items[index];
}

pub fn getSource(self: *const Compilation, id: Source.Id) ?*const Source {
    return &(self.getModule(id) orelse return null).source;
}

pub fn sourceSlice(self: *const Compilation, span: Source.Span) ?[]const u8 {
    return (self.getSource(span.source) orelse return null).slice(span);
}

pub fn getDeclaration(self: *const Compilation, id: DeclId) ?DeclRef {
    const index = @backingInt(id);
    if (index >= self.declarations.items.len) return null;
    return self.declarations.items[index];
}

/// Also used for demand-created method instances. IDs are never renumbered when
/// another source or a specialization is appended.
pub fn internDeclaration(self: *Compilation, ref: DeclRef) error{ OutOfMemory, TooManyDeclarations }!DeclId {
    if (self.declaration_ids.get(ref)) |id| return id;
    if (self.declarations.items.len == std.math.maxInt(u32)) return error.TooManyDeclarations;
    const id: DeclId = @fromBackingInt(@intCast(self.declarations.items.len));
    try self.declarations.ensureUnusedCapacity(self.allocator, 1);
    try self.declaration_ids.put(self.allocator, ref, id);
    self.declarations.appendAssumeCapacity(ref);
    return id;
}

/// canonical_path must identify the physical source, not an import spelling.
/// Filesystem loading below obtains it with realPathFileAlloc (including symlinks).
/// This entry also supports in-memory compiler clients without filesystem I/O.
pub fn addSource(self: *Compilation, canonical_path: []const u8, text: []const u8) Error!Source.Id {
    if (self.paths.get(canonical_path)) |id| return id;
    if (self.modules.items.len == std.math.maxInt(u32)) return error.TooManySources;
    const id: Source.Id = @fromBackingInt(@intCast(self.modules.items.len));
    const module = try self.allocator.create(Module);
    errdefer self.allocator.destroy(module);
    module.* = try Module.initSource(self.allocator, id, canonical_path, text);
    errdefer module.deinit();
    try self.modules.ensureUnusedCapacity(self.allocator, 1);
    try self.ir.source_lengths.ensureUnusedCapacity(self.allocator, 1);
    try self.paths.ensureUnusedCapacity(self.allocator, 1);
    const declarations_start = self.declarations.items.len;
    errdefer {
        for (self.declarations.items[declarations_start..]) |ref| _ = self.declaration_ids.remove(ref);
        self.declarations.shrinkRetainingCapacity(declarations_start);
    }
    for (module.declarations.items, 0..) |_, local| {
        _ = try self.internDeclaration(.{ .source = id, .local = @fromBackingInt(@intCast(local)) });
    }
    self.modules.appendAssumeCapacity(module);
    self.ir.source_lengths.appendAssumeCapacity(@intCast(text.len));
    self.paths.putAssumeCapacity(module.source.path, id);
    return id;
}

pub fn setRoot(self: *Compilation, id: Source.Id) void {
    const module = self.getModule(id).?;
    self.root = id;
    self.root_main = if (module.lookup("main")) |local| self.declaration_ids.get(.{ .source = id, .local = local }) else null;
}

pub fn namespace(self: *const Compilation, declaration: DeclId) ?Import {
    for (self.imports.items) |edge| if (edge.declaration == declaration) return edge;
    return null;
}

/// File imports see unmarked declarations; named modules require public metadata.
/// The returned identity retains the defining scope, including for reexports.
pub fn lookupNamespace(self: *const Compilation, declaration: DeclId, name: []const u8) ?DeclId {
    const edge = self.namespace(declaration) orelse return null;
    const module = self.getModule(edge.target).?;
    const local = module.lookup(name) orelse return null;
    if (edge.named and !module.getDeclaration(local).?.public) return null;
    return self.declaration_ids.get(.{ .source = edge.target, .local = local });
}

/// Resolve syntax in its defining scope; namespace members are never injected.
pub fn reference(self: *const Compilation, module: *const Module, node: Ast.Node.Index) ?DeclRef {
    return self.referenceDepth(module, node, 0);
}

fn referenceDepth(self: *const Compilation, module: *const Module, node: Ast.Node.Index, depth: usize) ?DeclRef {
    if (depth >= 128) return null;
    const ast = &module.source.ast;
    return switch (ast.nodes.items(.tag)[node]) {
        .identifier => .{ .source = module.source.id, .local = module.lookup(ast.tokenSlice(ast.firstToken(node))) orelse return null },
        .field_access => blk: {
            const data = ast.nodes.items(.data)[node];
            var base = self.referenceDepth(module, data.lhs, depth + 1) orelse return null;
            for (0..128) |_| {
                const global = self.declaration_ids.get(base) orelse return null;
                if (self.namespace(global) != null) {
                    const member = self.lookupNamespace(global, ast.tokenSlice(data.rhs)) orelse return null;
                    break :blk self.getDeclaration(member);
                }
                const owner = self.getModule(base.source).?;
                const decl = owner.getDeclaration(base.local).?;
                const full = owner.source.ast.decl(decl.node).ast;
                if (full.is_mutable or full.type_node != 0 or full.init_node == 0) return null;
                base = self.referenceDepth(owner, full.init_node, depth + 1) orelse return null;
            }
            break :blk null;
        },
        else => null,
    };
}

pub fn prepareSemantics(self: *Compilation) std.mem.Allocator.Error!void {
    for (self.modules.items) |module| {
        module.compilation = self;
        module.clearSpecializations();
    }
    // Drop only analysis-created suffix identities. Source declarations and
    // namespace edges keep their canonical IDs across repeated analyses.
    for (self.declarations.items, 0..) |ref, i| {
        if (@backingInt(ref.local) < self.getModule(ref.source).?.declarations.items.len) continue;
        for (self.declarations.items[i..]) |stale| _ = self.declaration_ids.remove(stale);
        self.declarations.shrinkRetainingCapacity(i);
        break;
    }
    for (self.imports.items) |edge| {
        const ref = self.getDeclaration(edge.declaration).?;
        self.getModule(ref.source).?.declarations.items[@backingInt(ref.local)].is_namespace = true;
    }
    try self.validateImportGraph();
    // Public reexports and generic aliases can cross several namespace edges.
    for (0..self.declarations.items.len + 1) |_| {
        var changed = false;
        for (self.modules.items) |module| {
            for (module.declarations.items) |*decl| {
                const full = module.source.ast.decl(decl.node).ast;
                if (full.is_mutable or full.type_node != 0 or full.init_node == 0) continue;
                if (module.source.ast.fullCall(full.init_node)) |call| {
                    if (self.reference(module, call.ast.fn_expr)) |callee| {
                        if (self.getModule(callee.source).?.getDeclaration(callee.local).?.is_generic and !decl.is_type_alias) {
                            decl.is_type_alias = true;
                            changed = true;
                        }
                    }
                }
                const ref = self.reference(module, full.init_node) orelse continue;
                const target = self.getModule(ref.source).?.getDeclaration(ref.local).?;
                if (target.is_namespace and !decl.is_namespace) {
                    decl.is_namespace = true;
                    changed = true;
                }
                if (target.is_type_alias and !decl.is_type_alias) {
                    decl.is_type_alias = true;
                    changed = true;
                }
                if (target.is_generic and !decl.is_generic) {
                    decl.is_generic = true;
                    changed = true;
                }
            }
            try module.classifyAliases();
        }
        if (!changed) break;
    }
}

fn validateImportGraph(self: *Compilation) std.mem.Allocator.Error!void {
    const colors = try self.allocator.alloc(enum { unseen, active, done }, self.modules.items.len);
    defer self.allocator.free(colors);
    @memset(colors, .unseen);
    const Frame = struct { source: Source.Id, next: usize = 0 };
    var stack: std.ArrayList(Frame) = .empty;
    defer stack.deinit(self.allocator);
    for (self.modules.items, 0..) |_, start| {
        if (colors[start] != .unseen) continue;
        try stack.append(self.allocator, .{ .source = @fromBackingInt(@intCast(start)) });
        colors[start] = .active;
        while (stack.items.len != 0) {
            const frame = &stack.items[stack.items.len - 1];
            var child: ?Source.Id = null;
            while (frame.next < self.imports.items.len) {
                const edge = self.imports.items[frame.next];
                frame.next += 1;
                const ref = self.getDeclaration(edge.declaration).?;
                if (ref.source != frame.source) continue;
                switch (colors[@backingInt(edge.target)]) {
                    .done => {},
                    .active => {
                        try self.diagnostics.append(self.allocator, .{ .tag = .import_cycle, .span = edge.span });
                        return;
                    },
                    .unseen => {
                        child = edge.target;
                        break;
                    },
                }
            }
            if (child) |source| {
                colors[@backingInt(source)] = .active;
                try stack.append(self.allocator, .{ .source = source });
            } else {
                colors[@backingInt(frame.source)] = .done;
                _ = stack.pop();
            }
        }
    }
}

pub fn hasErrors(self: *const Compilation) bool {
    if (self.diagnostics.items.len != 0) return true;
    for (self.modules.items) |module| if (module.hasErrors()) return true;
    return false;
}

/// Loads a canonical graph, not executable IR. Callers must still run the shared
/// semantic worklist before accepting namespace declarations as executable input.
pub fn load(allocator: std.mem.Allocator, io: std.Io, path: []const u8, options: Options) !Compilation {
    var result = Compilation.init(allocator);
    errdefer result.deinit();
    const root = try result.loadFile(io, path, options.max_source_bytes);
    result.setRoot(root);
    var active: std.ArrayList(Source.Id) = .empty;
    defer active.deinit(allocator);
    var completed: std.AutoHashMapUnmanaged(Source.Id, void) = .empty;
    defer completed.deinit(allocator);
    try result.discover(io, root, options, &active, &completed);
    return result;
}

fn loadFile(self: *Compilation, io: std.Io, path: []const u8, limit: usize) !Source.Id {
    const canonical = try std.Io.Dir.cwd().realPathFileAlloc(io, path, self.allocator);
    defer self.allocator.free(canonical);
    if (self.paths.get(canonical)) |id| return id;
    const text = try std.Io.Dir.cwd().readFileAlloc(io, canonical, self.allocator, .limited(limit));
    defer self.allocator.free(text);
    return self.addSource(canonical, text);
}

fn discover(self: *Compilation, io: std.Io, id: Source.Id, options: Options, active: *std.ArrayList(Source.Id), completed: *std.AutoHashMapUnmanaged(Source.Id, void)) Error!void {
    if (completed.contains(id)) return;
    try active.append(self.allocator, id);
    defer _ = active.pop();
    const module = self.getModule(id).?;
    const ast = &module.source.ast;
    if (ast.errors.len != 0) return;
    // Scan every call, not just declaration initializers: runtime/local/computed
    // imports must not disappear into an ignored body or dormant generic method.
    for (ast.nodes.items(.tag), 0..) |_, index| {
        const node: u32 = @intCast(index);
        const call = ast.fullCall(node) orelse continue;
        if (!std.mem.eql(u8, ast.tokenSlice(ast.firstToken(call.ast.fn_expr)), "@import")) continue;
        const span = module.tokenSpan(ast.firstToken(node));
        var declaration: ?Module.DeclId = null;
        for (module.declarations.items, 0..) |decl, local| {
            const full = ast.decl(decl.node).ast;
            if (decl.container == null and !full.is_mutable and full.type_node == 0 and full.init_node == node) {
                declaration = @fromBackingInt(@intCast(local));
                break;
            }
        }
        if (declaration == null or call.ast.args.len != 1 or ast.nodes.items(.tag)[call.ast.args[0]] != .string_literal) {
            try self.diagnostics.append(self.allocator, .{ .tag = .invalid_import, .span = span });
            continue;
        }
        const literal = ast.tokenSlice(ast.firstToken(call.ast.args[0]));
        if (Literal.hasInterpolation(literal)) {
            try self.diagnostics.append(self.allocator, .{ .tag = .invalid_import, .span = span });
            continue;
        }
        const spelling = string_literal.parseAlloc(self.allocator, literal) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidLiteral => {
                try self.diagnostics.append(self.allocator, .{ .tag = .invalid_path, .span = span });
                continue;
            },
        };
        defer self.allocator.free(spelling);
        const named = std.mem.eql(u8, spelling, "std");
        if (std.mem.indexOfScalar(u8, spelling, 0) != null or (!named and (std.fs.path.isAbsolute(spelling) or !std.mem.endsWith(u8, spelling, ".nov") or spelling.len == 4))) {
            try self.diagnostics.append(self.allocator, .{ .tag = .invalid_path, .span = span });
            continue;
        }
        if (named and options.lib_dir == null) {
            try self.diagnostics.append(self.allocator, .{ .tag = .missing_library, .span = span });
            continue;
        }
        const path = try std.fs.path.join(self.allocator, if (named) &.{ options.lib_dir.?, "std.nov" } else &.{ std.fs.path.dirname(module.source.path) orelse ".", spelling });
        defer self.allocator.free(path);
        const target = self.loadFile(io, path, options.max_source_bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                try self.diagnostics.append(self.allocator, .{ .tag = .load_failed, .span = span, .cause = err });
                continue;
            },
        };
        const global = self.declaration_ids.get(.{ .source = id, .local = declaration.? }).?;
        var cycle = false;
        for (active.items) |ancestor| if (ancestor == target) {
            cycle = true;
            break;
        };
        if (cycle) {
            var related: ?Source.Span = null;
            for (self.imports.items) |edge| if (edge.target == target or edge.span.source == target) {
                related = edge.span;
                break;
            };
            try self.diagnostics.append(self.allocator, .{ .tag = .import_cycle, .span = span, .related = related });
            continue;
        }
        if (active.items.len >= 256) {
            try self.diagnostics.append(self.allocator, .{ .tag = .import_depth, .span = span });
            continue;
        }
        try self.imports.append(self.allocator, .{ .declaration = global, .target = target, .named = named, .span = span });
        try self.discover(io, target, options, active, completed);
    }
    try completed.put(self.allocator, id, {});
}

test "named std imports require explicit library roots and public metadata" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data = "let standard = @import(\"std\")\nlet file = @import(\"std.nov\")" });
    try tmp.dir.writeFile(io, .{ .sub_path = "std.nov", .data = "let hidden = true\n@[public]\nlet visible = true\n@[public]\nlet helper: () -> bool = true\n@[public]\nlet Result = result.Result\nlet result = @import(\"result.nov\")" });
    try tmp.dir.writeFile(io, .{ .sub_path = "result.nov", .data = "@[public]\nlet Result:(T:type,E:type)->type=union{ok:T,err:E,}" });
    const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
    defer allocator.free(path);
    const lib = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(lib);
    var missing = try Compilation.load(allocator, io, path, .{});
    defer missing.deinit();
    try std.testing.expectEqual(.missing_library, missing.diagnostics.items[0].tag);
    var compilation = try Compilation.load(allocator, io, path, .{ .lib_dir = lib });
    defer compilation.deinit();
    try std.testing.expect(!compilation.hasErrors());
    const named = compilation.imports.items[0];
    const file = compilation.imports.items[compilation.imports.items.len - 1];
    try std.testing.expectEqual(named.target, file.target);
    try std.testing.expect(compilation.lookupNamespace(named.declaration, "hidden") == null);
    try std.testing.expect(compilation.lookupNamespace(file.declaration, "hidden") != null);
    for ([_][]const u8{ "visible", "helper", "Result" }) |name| {
        try std.testing.expectEqual(compilation.lookupNamespace(named.declaration, name).?, compilation.lookupNamespace(file.declaration, name).?);
    }
}

fn loaderAllocationTest(allocator: std.mem.Allocator, path: []const u8) !void {
    var compilation = try Compilation.load(allocator, std.testing.io, path, .{});
    defer compilation.deinit();
    try std.testing.expect(!compilation.hasErrors());
    try std.testing.expectEqual(@as(usize, 2), compilation.modules.items.len);
}

test "canonical loader owns graph storage across allocation failures" {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data = "let one = @import(\"leaf.nov\")\nlet two = @import(\"./leaf.nov\")" });
    try tmp.dir.writeFile(io, .{ .sub_path = "leaf.nov", .data = "let value = true" });
    const path = try tmp.dir.realPathFileAlloc(io, "main.nov", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, loaderAllocationTest, .{path});
}

test "compilation owners and global declaration identities survive graph growth" {
    var compilation = Compilation.init(std.testing.allocator);
    defer compilation.deinit();
    const root = try compilation.addSource("/app/main.nov", "let main: () -> void = {}\nlet Helper = struct { value: bool, }");
    compilation.setRoot(root);
    const owner = compilation.getModule(root).?;
    const main = compilation.root_main.?;
    const other = try compilation.addSource("/app/helper.nov", "let main: () -> void = {}\nlet Helper = struct { value: bool, }");
    try std.testing.expect(root != other);
    try std.testing.expectEqual(owner, compilation.getModule(root).?);
    try std.testing.expectEqual(main, compilation.root_main.?);
    try std.testing.expectEqual(root, compilation.getDeclaration(main).?.source);
    try std.testing.expectEqual(other, try compilation.addSource("/app/helper.nov", "ignored"));
    const first = owner.getDeclaration(owner.lookup("Helper").?).?.name_span;
    const second = compilation.getModule(other).?.getDeclaration(compilation.getModule(other).?.lookup("Helper").?).?.name_span;
    try std.testing.expect(first.source != second.source);
    try std.testing.expect(owner.sourceSlice(second) == null);
    try std.testing.expectEqualStrings("Helper", compilation.sourceSlice(second).?);
}

fn ownershipAllocationTest(allocator: std.mem.Allocator) !void {
    var compilation = Compilation.init(allocator);
    defer compilation.deinit();
    const root = try compilation.addSource("/main.nov", "let main: () -> void = {}");
    compilation.setRoot(root);
    _ = try compilation.addSource("/other.nov", "let Value = struct { value: bool, }\nlet helper: () -> bool = true");
    try std.testing.expectEqual(@as(usize, 2), compilation.modules.items.len);
}

test "compilation publication rolls back under allocation failures" {
    try TestAllocations.checkAllAllocationFailures(std.testing.allocator, ownershipAllocationTest, .{});
}

test "canonical diamond imports preserve namespaces and defining scopes" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data = "let left = @import(\"left.nov\")\nlet right = @import(\"right.nov\")\nlet main: () -> void = {}" });
    try tmp.dir.writeFile(io, .{ .sub_path = "left.nov", .data = "let shared = @import(\"shared.nov\")\nlet helper: () -> bool = true" });
    try tmp.dir.writeFile(io, .{ .sub_path = "right.nov", .data = "let shared = @import(\"./shared.nov\")\nlet helper: () -> bool = false" });
    try tmp.dir.writeFile(io, .{ .sub_path = "shared.nov", .data = "let Value = struct { value: bool, }" });
    const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
    defer allocator.free(path);
    var compilation = try Compilation.load(allocator, io, path, .{});
    defer compilation.deinit();
    try std.testing.expect(!compilation.hasErrors());
    try std.testing.expectEqual(@as(usize, 4), compilation.modules.items.len);
    try std.testing.expectEqual(@as(usize, 4), compilation.imports.items.len);
    try std.testing.expectEqual(compilation.imports.items[1].target, compilation.imports.items[3].target);
    const root = compilation.getModule(compilation.root.?).?;
    try std.testing.expect(root.lookup("Value") == null);
    const edge = compilation.imports.items[0];
    const helper = compilation.lookupNamespace(edge.declaration, "helper").?;
    try std.testing.expectEqual(edge.target, compilation.getDeclaration(helper).?.source);
}

test "loader rejects cycles missing files mutable computed and local imports" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.nov", .data = "let next = @import(\"next.nov\")\nlet missing = @import(\"missing.nov\")\nlet mut bad = @import(\"next.nov\")\nlet computed = @import(path)\nlet f: () -> void = {\nlet local = @import(\"next.nov\")\n}" });
    try tmp.dir.writeFile(io, .{ .sub_path = "next.nov", .data = "let root = @import(\"main.nov\")" });
    const path = try tmp.dir.realPathFileAlloc(io, "main.nov", allocator);
    defer allocator.free(path);
    var compilation = try Compilation.load(allocator, io, path, .{});
    defer compilation.deinit();
    try std.testing.expect(compilation.hasErrors());
    var cycles: usize = 0;
    var missing: usize = 0;
    var invalid: usize = 0;
    for (compilation.diagnostics.items) |diagnostic| switch (diagnostic.tag) {
        .import_cycle => {
            cycles += 1;
            try std.testing.expect(diagnostic.related.?.source != diagnostic.span.source);
        },
        .load_failed => missing += 1,
        .invalid_import => invalid += 1,
        else => return error.UnexpectedDiagnostic,
    };
    try std.testing.expectEqual(@as(usize, 1), cycles);
    try std.testing.expectEqual(@as(usize, 1), missing);
    try std.testing.expectEqual(@as(usize, 3), invalid);
}
