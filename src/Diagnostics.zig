const std = @import("std");
const CEmitter = @import("CEmitter.zig");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const Module = @import("Module.zig");
const Source = @import("Source.zig");
const Compilation = @import("Compilation.zig");

const Location = struct {
    start: usize,
    line: usize = 1,
    line_start: usize = 0,
    line_end: usize,

    fn init(source: []const u8, start: usize) Location {
        var location: Location = .{
            .start = start,
            .line_end = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse source.len,
        };
        for (source[0..start], 0..) |byte, i| {
            if (byte == '\n') {
                location.line += 1;
                location.line_start = i + 1;
            }
        }
        return location;
    }

    fn renderPrefix(location: Location, path: []const u8, kind: []const u8, writer: *std.Io.Writer) !void {
        try writer.print("{s}:{d}:{d}: {s}: ", .{ path, location.line, location.start - location.line_start + 1, kind });
    }

    fn renderExcerpt(location: Location, source: []const u8, length: usize, writer: *std.Io.Writer) !void {
        try writer.print("\n{s}\n", .{std.mem.trimEnd(u8, source[location.line_start..location.line_end], "\r")});
        for (source[location.line_start..location.start]) |byte| {
            try writer.writeByte(if (byte == '\t') '\t' else ' ');
        }
        const width = @max(1, @min(length, location.line_end - location.start));
        try writer.writeByte('^');
        try writer.splatByteAll('~', width - 1);
        try writer.writeByte('\n');
    }
};

test "compilation diagnostics render related locations and backend leaves from their owners" {
    var compilation = Compilation.init(std.testing.allocator);
    defer compilation.deinit();
    const root = try compilation.addSource("root.nov", "let value = true");
    const library = try compilation.addSource("library.nov", "let other = false");
    const root_module = compilation.getModule(root).?;
    const library_module = compilation.getModule(library).?;
    const primary = root_module.getDeclaration(root_module.lookup("value").?).?.name_span;
    const related = library_module.getDeclaration(library_module.lookup("other").?).?.name_span;
    try root_module.diagnostics.append(std.testing.allocator, .{ .tag = .expression_type_mismatch, .span = primary, .related = related });
    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try renderCompilation(&compilation, &out.writer);
    try std.testing.expect(std.mem.indexOf(u8, out.written(), "root.nov:1:5: error:") != null);
    try std.testing.expect(std.mem.indexOf(u8, out.written(), "library.nov:1:5: note:") != null);
    try renderCompilationBackend(&compilation, .{ .reason = .integer_width_unsupported, .function = null, .span = related }, &out.writer);
    try std.testing.expect(std.mem.indexOf(u8, out.written(), "library.nov:1:5: error: C backend") != null);
}

pub fn renderModule(module: *const Module, writer: *std.Io.Writer) !void {
    try renderModuleWithSources(module, null, writer);
}

fn spanSource(module: *const Module, compilation: ?*const Compilation, span: Source.Span) !*const Source {
    const source = if (compilation) |owner| owner.getSource(span.source) orelse return error.InvalidSpan else &module.source;
    if (source.slice(span) == null) return error.InvalidSpan;
    return source;
}

fn renderModuleWithSources(module: *const Module, compilation: ?*const Compilation, writer: *std.Io.Writer) !void {
    try render(module.source.ast, module.source.path, writer);
    for (module.diagnostics.items) |diagnostic| {
        const source = try spanSource(module, compilation, diagnostic.span);
        const location = Location.init(source.ast.source, diagnostic.span.start);
        try location.renderPrefix(source.path, "error", writer);
        switch (diagnostic.tag) {
            .borrow_conflict => try writer.writeAll("owner access conflicts with a live borrow"),
            .borrow_escape => try writer.writeAll("references cannot escape as capabilities; read the borrowed value instead"),
            .unsupported_borrow => try writer.writeAll("borrow requires a fixed unannotated let binding to &local or &localArray[index]; void has no borrowable storage; nonlocal owners, reborrows and explicit reference types are unsupported"),
            .invalid_generic_declaration => try writer.writeAll("generic container declarations require named type parameters: (T: type, ...) -> type = struct { ... } or union { ... }"),
            .generic_needs_arguments => try writer.writeAll("generic container requires explicit type arguments"),
            .not_a_generic => try writer.writeAll("type application requires an ordinary named generic container"),
            .generic_argument_count => try writer.writeAll("type argument count does not match the generic container declaration"),
            .generic_cycle => try writer.writeAll("cyclic generic container instantiation"),
            .generic_limit => try writer.writeAll("generic container expansion exceeds the current limit of 64"),
            .unsupported_generic_method => try writer.writeAll("generic container members require an ordinary function signature or an immutable @This() alias"),
            .invalid_propagation_operand => try writer.writeAll(".! requires a union with exactly ok and err variants, or exactly some and none variants with a void none payload"),
            .invalid_propagation_return => try writer.writeAll(".! requires an enclosing function returning the same union family: exactly ok and err, or exactly some and none with a void none payload"),
            .propagation_error_mismatch => try writer.writeAll(".! error payload must exactly match the enclosing union's err type"),
            .empty_union => try writer.writeAll("union requires at least one variant"),
            .empty_enum => try writer.writeAll("enum requires at least one tag"),
            .enum_needs_type => try writer.writeAll("contextual enum tag needs an expected enum type"),
            .unsupported_enum_value => try writer.writeAll("explicit enum values and payload types are unsupported; use unnumbered tags"),
            .match_covered_enum_prong => try writer.writeAll("enum match prong is completely covered by earlier prongs"),
            .unsupported_union_default => try writer.writeAll("union payload defaults are not supported; construct a variant explicitly"),
            .invalid_union_constructor => try writer.writeAll("union construction requires U.tag[payload] or .tag[payload], with an exact payload type and [] for void"),
            .union_needs_type => try writer.writeAll("contextual union constructor needs an expected union type"),
            .match_missing_variants => try writer.writeAll("enum or union match must cover every variant or end with a wildcard (_)"),
            .match_covered_union_prong => try writer.writeAll("union match prong is already covered by earlier prongs"),
            .invalid_match_capture => try writer.writeAll("payload capture requires a single union variant pattern and an immutable named binding"),
            .unsupported_attribute => try writer.writeAll("unsupported, misplaced or duplicate attribute; @[public] requires an immutable declaration, @[noreturn] requires a function"),
            .invalid_test_declaration => try writer.writeAll("@[test] requires an ordinary immutable top-level function with a body, no arguments to the attribute, and no export, extern or noreturn attribute"),
            .invalid_test_signature => try writer.writeAll("test function must have type () -> void"),
            .test_not_callable => try writer.writeAll("test functions cannot be referenced or called; only the test runner may invoke them"),
            .invalid_export_declaration => try writer.writeAll("@[export] requires an ordinary immutable top-level function with a body"),
            .invalid_export_attribute => try writer.writeAll("@[export] accepts no argument or one ordinary non-interpolated string literal, cannot be repeated, and cannot be combined with @[extern]"),
            .invalid_export_name => try writer.writeAll("export name must be a nonreserved ASCII C11 identifier outside the backend namespaces"),
            .invalid_export_signature => try writer.writeAll("export ABI supports only bool and 8/16/32/64-bit integers, with void allowed only as the result"),
            .duplicate_export_name => try writer.writeAll("duplicate exported symbol"),
            .invalid_extern_declaration => try writer.writeAll("@[extern] requires an ordinary immutable top-level function prototype without a body"),
            .invalid_extern_attribute => try writer.writeAll("@[extern] accepts no argument or one ordinary non-interpolated string literal, cannot be repeated, and cannot be combined with @[export]"),
            .invalid_extern_name => try writer.writeAll("extern name must be a nonreserved ASCII C11 identifier outside the backend namespaces"),
            .invalid_extern_signature => try writer.writeAll("extern ABI supports only bool and 8/16/32/64-bit integers, with void allowed only as the result"),
            .conflicting_foreign_signature => try writer.writeAll("foreign symbol declarations must have the same canonical signature and noreturn annotation"),
            .noreturn_result => try writer.writeAll("@[noreturn] function must return void"),
            .noreturn_return => try writer.writeAll("return is forbidden in an @[noreturn] function"),
            .noreturn_completion => try writer.writeAll("@[noreturn] function may complete normally"),
            .duplicate_field => try writer.writeAll("duplicate container field"),
            .unknown_field => try writer.writeAll("unknown container field, method, enum tag or union variant"),
            .invalid_method_receiver => try writer.writeAll("instance method requires a first parameter of the exact receiver container type"),
            .missing_field => if (diagnostic.related) |field| try writer.print("missing required struct field '{s}'", .{(try spanSource(module, compilation, field)).slice(field).?}) else try writer.writeAll("missing required struct field"),
            .invalid_field_default => try writer.writeAll("struct field default must be a scalar literal of the exact field type; nested structs require explicit values"),
            .record_layout_cycle => try writer.writeAll("cyclic by-value struct layout"),
            .record_layout_limit => try writer.writeAll("by-value aggregate nesting exceeds the current limit of 128"),
            .invalid_to_string => try writer.writeAll("printing a struct, enum or union requires its method toString: (MyType) -> string"),
            .duplicate_name => try writer.print("duplicate declaration '{s}'", .{source.slice(diagnostic.span).?}),
            .unknown_name => try writer.print("unknown name '{s}'", .{source.slice(diagnostic.span).?}),
            .unsupported_declaration => try writer.writeAll("unsupported declaration; containers currently allow only explicitly typed methods"),
            .unsupported_name => try writer.writeAll("quoted identifiers are not supported in semantic analysis yet"),
            .type_alias_cycle => try writer.print("cyclic type alias dependency on '{s}'", .{source.slice(diagnostic.span).?}),
            .type_alias_limit => try writer.writeAll("type alias expansion exceeds the current analysis limit of 128"),
            .not_a_value => try writer.writeAll("expected a runtime value, found a type alias"),
            .value_cycle => try writer.print("cyclic value dependency on '{s}'", .{source.slice(diagnostic.span).?}),
            .unsupported_dependency_expression => try writer.writeAll("dependency analysis for this expression is not implemented yet"),
            .missing_initializer => try writer.writeAll("dependency analysis requires an initializer"),
            .unknown_type => try writer.print("unknown type '{s}'", .{source.slice(diagnostic.span).?}),
            .unsupported_type => try writer.writeAll("this type is not supported in semantic analysis yet"),
            .not_a_type => try writer.writeAll("expected a type, found a value"),
            .duplicate_parameter => try writer.print("duplicate parameter '{s}'", .{source.slice(diagnostic.span).?}),
            .invalid_parameter_type => try writer.writeAll("void is not a parameter type"),
            .type_nesting_limit => try writer.writeAll("type annotation nesting exceeds the current analysis limit of 128"),
            .invalid_literal => try writer.writeAll("invalid literal; rune literals require exactly one Unicode scalar"),
            .unsupported_literal => try writer.writeAll("this literal is not supported in semantic analysis yet"),
            .invalid_interpolation => try writer.writeAll("expected {expression}; blocks, control flow, quotes and format options are unsupported; escape literal braces as \\{ and \\}"),
            .unsupported_interpolation_type => try writer.writeAll("interpolation requires a bool, integer, rune, string, or struct/enum/union with toString: (MyType) -> string"),
            .unsupported_global_interpolation => try writer.writeAll("global strings require literal initializers; move interpolation into a function"),
            .literal_type_mismatch => try writer.writeAll("literal does not match the annotated type"),
            .literal_out_of_range => try writer.writeAll("integer literal does not fit the annotated type"),
            .unsupported_match_pattern => try writer.writeAll("match patterns support scalar literals, literal alternatives (|), union variants and wildcards (_) only"),
            .unsupported_match_subject => try writer.writeAll("match subjects must be bool, integer, rune or string values, or one enum or union value"),
            .match_missing_catchall => try writer.writeAll("match requires a final wildcard catchall (bounded exhaustiveness)"),
            .match_missing_boolean_values => try writer.writeAll("boolean match does not cover true or false; add both values or a wildcard (_)"),
            .match_missing_true => try writer.writeAll("boolean match does not cover true; add true or a wildcard (_)"),
            .match_missing_false => try writer.writeAll("boolean match does not cover false; add false or a wildcard (_)"),
            .match_covered_boolean_prong => try writer.writeAll("boolean match prong is completely covered by earlier prongs"),
            .match_catchall_not_last => try writer.writeAll("match wildcard catchall must be the final prong"),
            .match_pattern_count => try writer.writeAll("match prong needs one positional pattern per subject (or a single _ catchall)"),
            .unsupported_body_expression => try writer.writeAll("body checking for this expression is not implemented yet"),
            .inclusive_string_slice => try writer.writeAll("string slicing requires half-open '..' bounds; '..=' is not supported"),
            .string_slice_requires_string => try writer.writeAll("slicing requires a string value"),
            .unsupported_local_declaration => try writer.writeAll("local declarations require a supported value type (void needs an explicit immutable annotation; local function definitions and discards are not supported yet)"),
            .immutable_assignment => try writer.writeAll("assignment requires a mutable local"),
            .unsupported_assignment_target => try writer.writeAll("assignment target must be a mutable local, its struct field path, or its scalar-array element"),
            .unsupported_array_length_write => try writer.writeAll("array .len is read-only in the current subset; length writes and growth initialization are deferred"),
            .array_literal_needs_type => try writer.writeAll("array literal needs an expected []T type"),
            .unreachable_code => try writer.writeAll("unreachable statement after terminal control flow"),
            .loop_control_outside_loop => try writer.writeAll("break and continue require an enclosing loop"),
            .unsupported_loop_control => try writer.writeAll("labeled loop control is not supported"),
            .conditional_value_break => try writer.writeAll("conditional loops cannot break with a value without an else branch"),
            .loop_missing_value_break => try writer.writeAll("nonvoid loop result requires a reachable value-carrying break"),
            .unused_block_value => try writer.writeAll("non-final block expression must return void; discarding values is not supported yet"),
            .unsupported_shadowing => try writer.writeAll("shadowing is not supported in body analysis yet"),
            .expression_type_mismatch => try writer.writeAll("expression does not match the expected type"),
            .operator_not_supported => try writer.writeAll("operator is not supported for this operand type"),
            .literal_needs_type => try writer.writeAll("integer literal needs an expected type (no default chosen)"),
            .not_callable => try writer.writeAll("expression is not callable"),
            .argument_count_mismatch => try writer.writeAll("argument count does not match the function signature"),
            .unresolved_value_type => try writer.writeAll("declaration needs a resolved type before use here"),
            .body_nesting_limit => try writer.writeAll("recursive body checking exceeds the current analysis limit of 128"),
            .unsupported_global_initializer => try writer.writeAll("inferred globals require immutable bool/rune/string literals or @as(T, scalar literal); general global initialization is unsupported"),
            .unsupported_lowering => try writer.writeAll("lowering this source form is not implemented yet"),
        }
        try location.renderExcerpt(source.ast.source, diagnostic.span.end - diagnostic.span.start, writer);
        const note: ?[]const u8 = switch (diagnostic.tag) {
            .borrow_conflict, .borrow_escape => "borrow originates here",
            .duplicate_name => "first declaration here",
            .duplicate_export_name => "first export of this symbol here",
            .conflicting_foreign_signature => "first declaration of this foreign symbol here",
            .value_cycle, .type_alias_cycle => "declaration in this cycle",
            .type_alias_limit, .not_a_value => "type alias declared here",
            .duplicate_parameter => "first parameter here",
            .missing_field => "required field declared here",
            .record_layout_cycle => "by-value layout dependency returns here",
            .record_layout_limit => "nested layout declared here",
            .match_covered_boolean_prong => "earlier pattern covers this boolean value",
            .match_covered_union_prong, .match_covered_enum_prong => "earlier pattern covers this variant",
            .literal_type_mismatch, .literal_out_of_range => "type annotation here",
            .expression_type_mismatch, .literal_needs_type => "expected type required here",
            .immutable_assignment => "immutable declaration here",
            .unsupported_shadowing => "conflicting declaration here",
            .operator_not_supported => "operand type specified here",
            .argument_count_mismatch, .not_callable => "type specified here",
            .unresolved_value_type => "declaration here",
            else => null,
        };
        if (note) |message| {
            if (diagnostic.related) |related| {
                const related_source = try spanSource(module, compilation, related);
                const original = Location.init(related_source.ast.source, related.start);
                try original.renderPrefix(related_source.path, "note", writer);
                try writer.writeAll(message);
                try original.renderExcerpt(related_source.ast.source, related.end - related.start, writer);
            }
        }
    }
}

pub fn renderBackend(module: *const Module, diagnostic: CEmitter.Diagnostic, writer: *std.Io.Writer) !void {
    try renderBackendSource(try spanSource(module, null, diagnostic.span), diagnostic, writer);
}

fn renderBackendSource(source: *const Source, diagnostic: CEmitter.Diagnostic, writer: *std.Io.Writer) !void {
    if (source.slice(diagnostic.span) == null) return error.InvalidSpan;
    const location = Location.init(source.ast.source, diagnostic.span.start);
    try location.renderPrefix(source.path, "error", writer);
    try writer.writeAll(diagnostic.reason.message());
    try location.renderExcerpt(source.ast.source, diagnostic.span.end - diagnostic.span.start, writer);
}

pub fn renderCompilationBackend(compilation: *const Compilation, diagnostic: CEmitter.Diagnostic, writer: *std.Io.Writer) !void {
    const source = compilation.getSource(diagnostic.span.source) orelse return error.InvalidSpan;
    try renderBackendSource(source, diagnostic, writer);
}

pub fn renderCompilation(compilation: *const Compilation, writer: *std.Io.Writer) !void {
    for (compilation.modules.items) |module| try renderModuleWithSources(module, compilation, writer);
    for (compilation.diagnostics.items) |diagnostic| {
        const source = compilation.getSource(diagnostic.span.source) orelse return error.InvalidSpan;
        if (source.slice(diagnostic.span) == null) return error.InvalidSpan;
        const location = Location.init(source.ast.source, diagnostic.span.start);
        try location.renderPrefix(source.path, "error", writer);
        try writer.writeAll(switch (diagnostic.tag) {
            .invalid_import => "@import requires one literal string argument and must be the initializer or the base of a qualified member chain in an immutable, unannotated top-level declaration",
            .invalid_path => "import requires a relative literal .nov path or the named module 'std'",
            .missing_library => "named import 'std' requires a configured library directory",
            .load_failed => "cannot load imported source",
            .import_cycle => "cyclic source import",
            .import_depth => "import nesting exceeds the current limit of 256",
        });
        if (diagnostic.cause) |cause| try writer.print(": {s}", .{@errorName(cause)});
        try location.renderExcerpt(source.ast.source, diagnostic.span.end - diagnostic.span.start, writer);
        if (diagnostic.related) |related| {
            const original_source = compilation.getSource(related.source) orelse return error.InvalidSpan;
            if (original_source.slice(related) == null) return error.InvalidSpan;
            const original = Location.init(original_source.ast.source, related.start);
            try original.renderPrefix(original_source.path, "note", writer);
            try writer.writeAll("earlier import in this cycle");
            try original.renderExcerpt(original_source.ast.source, related.end - related.start, writer);
        }
    }
}

pub fn render(ast: Ast, path: []const u8, writer: *std.Io.Writer) !void {
    for (ast.errors) |parse_error| {
        const offset = ast.errorOffset(parse_error);
        const start = ast.tokens.items(.start)[parse_error.token] + offset;
        const location = Location.init(ast.source, start);
        try location.renderPrefix(path, "error", writer);
        try ast.renderError(parse_error, writer);
        try location.renderExcerpt(ast.source, if (offset != 0) 1 else ast.tokenSlice(parse_error.token).len, writer);
    }
}

test "module duplicate diagnostics include the first binding with tabs and CRLF" {
    var module = try Module.init(std.testing.allocator, "duplicate.nov", "let text = \"é\"\r\n\tlet name = 1\r\n\tlet name = 2");
    defer module.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "duplicate.nov:3:6: error: duplicate declaration 'name'\n\tlet name = 2\n\t    ^~~~\n" ++
            "duplicate.nov:2:6: note: first declaration here\n\tlet name = 1\n\t    ^~~~\n",
        output.written(),
    );
}

test "module value cycle diagnostics include the target declaration" {
    var module = try Module.init(std.testing.allocator, "cycle.nov", "let first = second\nlet second = first");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .value_cycle,
        .span = .{ .start = 12, .end = 18 },
        .related = module.getDeclaration(module.lookup("second").?).?.name_span,
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "cycle.nov:1:13: error: cyclic value dependency on 'second'\nlet first = second\n            ^~~~~~\n" ++
            "cycle.nov:2:5: note: declaration in this cycle\nlet second = first\n    ^~~~~~\n",
        output.written(),
    );
}

test "module duplicate parameter diagnostics include the first parameter" {
    var module = try Module.init(std.testing.allocator, "parameters.nov", "let value: (x: i32, x: bool)");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .duplicate_parameter,
        .span = .{ .start = 20, .end = 21 },
        .related = .{ .start = 12, .end = 13 },
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "parameters.nov:1:21: error: duplicate parameter 'x'\nlet value: (x: i32, x: bool)\n                    ^\n" ++
            "parameters.nov:1:13: note: first parameter here\nlet value: (x: i32, x: bool)\n            ^\n",
        output.written(),
    );
}

test "module literal diagnostics include the type annotation when present" {
    const Case = struct { tag: Module.Diagnostic.Tag, message: []const u8 };
    for ([_]Case{
        .{ .tag = .literal_out_of_range, .message = "integer literal does not fit the annotated type" },
        .{ .tag = .literal_type_mismatch, .message = "literal does not match the annotated type" },
    }) |case| {
        for ([_]bool{ true, false }) |with_related| {
            var module = try Module.init(std.testing.allocator, "literal.nov", "let value: u8 = 256");
            defer module.deinit();
            try std.testing.expect(!module.hasErrors());
            try module.diagnostics.append(module.allocator, .{
                .tag = case.tag,
                .span = .{ .start = 16, .end = 19 },
                .related = if (with_related) .{ .start = 11, .end = 13 } else null,
            });
            var output = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer output.deinit();
            try renderModule(&module, &output.writer);
            const expected = try std.fmt.allocPrint(
                std.testing.allocator,
                "literal.nov:1:17: error: {s}\nlet value: u8 = 256\n                ^~~\n{s}",
                .{ case.message, if (with_related) "literal.nov:1:12: note: type annotation here\nlet value: u8 = 256\n           ^~\n" else "" },
            );
            defer std.testing.allocator.free(expected);
            try std.testing.expectEqualStrings(expected, output.written());
        }
    }
}

test "module type and literal diagnostics render messages and source excerpts" {
    const Case = struct {
        tag: Module.Diagnostic.Tag,
        token: []const u8,
        underline: []const u8,
        message: []const u8,
    };
    for ([_]Case{
        .{ .tag = .unknown_type, .token = "Missing", .underline = "^~~~~~~", .message = "unknown type 'Missing'" },
        .{ .tag = .unsupported_type, .token = "i32", .underline = "^~~", .message = "this type is not supported in semantic analysis yet" },
        .{ .tag = .not_a_type, .token = "value", .underline = "^~~~~", .message = "expected a type, found a value" },
        .{ .tag = .invalid_parameter_type, .token = "void", .underline = "^~~~", .message = "void is not a parameter type" },
        .{ .tag = .type_nesting_limit, .token = "i32", .underline = "^~~", .message = "type annotation nesting exceeds the current analysis limit of 128" },
        .{ .tag = .invalid_literal, .token = "123", .underline = "^~~", .message = "invalid literal; rune literals require exactly one Unicode scalar" },
        .{ .tag = .invalid_literal, .token = "'ab'", .underline = "^~~~", .message = "invalid literal; rune literals require exactly one Unicode scalar" },
        .{ .tag = .unsupported_literal, .token = "1.5", .underline = "^~~", .message = "this literal is not supported in semantic analysis yet" },
    }) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator, "let value = {s}", .{case.token});
        defer std.testing.allocator.free(source);
        var module = try Module.init(std.testing.allocator, "semantic.nov", source);
        defer module.deinit();
        try std.testing.expect(!module.hasErrors());
        try module.diagnostics.append(module.allocator, .{
            .tag = case.tag,
            .span = .{ .start = 12, .end = @intCast(source.len) },
        });
        var output = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output.deinit();
        try renderModule(&module, &output.writer);
        const expected = try std.fmt.allocPrint(
            std.testing.allocator,
            "semantic.nov:1:13: error: {s}\n{s}\n            {s}\n",
            .{ case.message, source, case.underline },
        );
        defer std.testing.allocator.free(expected);
        try std.testing.expectEqualStrings(expected, output.written());
    }
}

test "module body diagnostics render messages and optional related notes" {
    const Case = struct {
        tag: Module.Diagnostic.Tag,
        message: []const u8,
        note: ?[]const u8 = null,
    };
    for ([_]Case{
        .{ .tag = .expression_type_mismatch, .message = "expression does not match the expected type", .note = "expected type required here" },
        .{ .tag = .argument_count_mismatch, .message = "argument count does not match the function signature", .note = "type specified here" },
        .{ .tag = .unsupported_body_expression, .message = "body checking for this expression is not implemented yet" },
        .{ .tag = .inclusive_string_slice, .message = "string slicing requires half-open '..' bounds; '..=' is not supported" },
        .{ .tag = .string_slice_requires_string, .message = "slicing requires a string value" },
        .{ .tag = .match_missing_boolean_values, .message = "boolean match does not cover true or false; add both values or a wildcard (_)" },
        .{ .tag = .match_missing_true, .message = "boolean match does not cover true; add true or a wildcard (_)" },
        .{ .tag = .match_missing_false, .message = "boolean match does not cover false; add false or a wildcard (_)" },
        .{ .tag = .match_covered_boolean_prong, .message = "boolean match prong is completely covered by earlier prongs", .note = "earlier pattern covers this boolean value" },
        .{ .tag = .unsupported_local_declaration, .message = "local declarations require a supported value type (void needs an explicit immutable annotation; local function definitions and discards are not supported yet)" },
        .{ .tag = .immutable_assignment, .message = "assignment requires a mutable local", .note = "immutable declaration here" },
        .{ .tag = .unsupported_assignment_target, .message = "assignment target must be a mutable local, its struct field path, or its scalar-array element" },
        .{ .tag = .unsupported_array_length_write, .message = "array .len is read-only in the current subset; length writes and growth initialization are deferred" },
        .{ .tag = .array_literal_needs_type, .message = "array literal needs an expected []T type" },
        .{ .tag = .unused_block_value, .message = "non-final block expression must return void; discarding values is not supported yet" },
        .{ .tag = .unsupported_shadowing, .message = "shadowing is not supported in body analysis yet", .note = "conflicting declaration here" },
        .{ .tag = .operator_not_supported, .message = "operator is not supported for this operand type", .note = "operand type specified here" },
        .{ .tag = .literal_needs_type, .message = "integer literal needs an expected type (no default chosen)", .note = "expected type required here" },
        .{ .tag = .not_callable, .message = "expression is not callable", .note = "type specified here" },
        .{ .tag = .unresolved_value_type, .message = "declaration needs a resolved type before use here", .note = "declaration here" },
        .{ .tag = .body_nesting_limit, .message = "recursive body checking exceeds the current analysis limit of 128" },
        .{ .tag = .unsupported_global_initializer, .message = "inferred globals require immutable bool/rune/string literals or @as(T, scalar literal); general global initialization is unsupported" },
        .{ .tag = .unsupported_lowering, .message = "lowering this source form is not implemented yet" },
    }) |case| {
        for ([_]bool{ true, false }) |with_related| {
            var module = try Module.init(std.testing.allocator, "body.nov", "let value: i32 = 123");
            defer module.deinit();
            try std.testing.expect(!module.hasErrors());
            try module.diagnostics.append(module.allocator, .{
                .tag = case.tag,
                .span = .{ .start = 17, .end = 20 },
                .related = if (with_related) .{ .start = 11, .end = 14 } else null,
            });
            var output = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer output.deinit();
            try renderModule(&module, &output.writer);
            var expected = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer expected.deinit();
            try expected.writer.print(
                "body.nov:1:18: error: {s}\nlet value: i32 = 123\n                 ^~~\n",
                .{case.message},
            );
            if (with_related) {
                if (case.note) |note| {
                    try expected.writer.print(
                        "body.nov:1:12: note: {s}\nlet value: i32 = 123\n           ^~~\n",
                        .{note},
                    );
                }
            }
            try std.testing.expectEqualStrings(expected.written(), output.written());
        }
    }
}

test "module unsupported dependency expressions render diagnostics" {
    var module = try Module.init(std.testing.allocator, "expression.nov", "let value = value()");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .unsupported_dependency_expression,
        .span = .{ .start = 12, .end = 19 },
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "expression.nov:1:13: error: dependency analysis for this expression is not implemented yet\nlet value = value()\n            ^~~~~~~\n",
        output.written(),
    );
}

test "module missing initializers render diagnostics" {
    var module = try Module.init(std.testing.allocator, "initializer.nov", "let value: i32");
    defer module.deinit();
    try std.testing.expect(!module.hasErrors());
    try module.diagnostics.append(module.allocator, .{
        .tag = .missing_initializer,
        .span = module.getDeclaration(module.lookup("value").?).?.name_span,
    });
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "initializer.nov:1:5: error: dependency analysis requires an initializer\nlet value: i32\n    ^~~~~\n",
        output.written(),
    );
}

test "module unknown names render only after explicit resolution" {
    var module = try Module.init(std.testing.allocator, "unknown.nov", "let value = absent");
    defer module.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings("", output.written());
    try std.testing.expectEqual(@as(?Module.DeclId, null), try module.resolveName(.{ .start = 12, .end = 18 }));
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "unknown.nov:1:13: error: unknown name 'absent'\nlet value = absent\n            ^~~~~~\n",
        output.written(),
    );
}

test "module unsupported attributes and quoted names render semantic diagnostics" {
    var module = try Module.init(std.testing.allocator, "unsupported.nov", "@[export]\nlet decorated = 1\nlet @\"quoted\" = 2");
    defer module.deinit();
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try renderModule(&module, &output.writer);
    try std.testing.expectEqualStrings(
        "unsupported.nov:1:1: error: @[export] requires an ordinary immutable top-level function with a body\n@[export]\n^~\n" ++
            "unsupported.nov:3:5: error: quoted identifiers are not supported in semantic analysis yet\nlet @\"quoted\" = 2\n    ^~~~~~~~~\n",
        output.written(),
    );
}

test "syntax-only module rendering matches parser rendering" {
    for ([_][]const u8{
        "let broken =",
        "let f: () = {\r\n\touter: loop {}\r\n}\r\n",
        "\tlet x = 1 let y = 2\n",
    }) |source| {
        var module = try Module.init(std.testing.allocator, "syntax.nov", source);
        defer module.deinit();
        try std.testing.expect(module.source.ast.errors.len > 0);
        try std.testing.expectEqual(@as(usize, 0), module.diagnostics.items.len);
        var expected = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer expected.deinit();
        try render(module.source.ast, module.source.path, &expected.writer);
        var output = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output.deinit();
        try renderModule(&module, &output.writer);
        try std.testing.expectEqualStrings(expected.written(), output.written());
    }
}

test "unsupported loop diagnostics identify the keyword with tabs and CRLF" {
    var ast = try Parser.parse(std.testing.allocator, "let f: () = {\r\n\touter: loop {}\r\n}\r\n");
    defer ast.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 1), ast.errors.len);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try render(ast, "loop.nov", &output.writer);
    try std.testing.expectEqualStrings(
        "loop.nov:2:9: error: unsupported loop form; expected 'loop condition { ... }'\n\touter: loop {}\n\t       ^~~~\n",
        output.written(),
    );
}

test "diagnostics include filename source and caret at EOF" {
    var ast = try Parser.parse(std.testing.allocator, "let broken =");
    defer ast.deinit(std.testing.allocator);
    try std.testing.expect(ast.errors.len > 0);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try render(ast, "broken.nov", &output.writer);
    try std.testing.expect(std.mem.startsWith(u8, output.written(), "broken.nov:1:13: error: "));
    try std.testing.expect(std.mem.endsWith(u8, output.written(), "\nlet broken =\n            ^\n"));
}

test "diagnostics point after previous token and preserve tabs" {
    var ast = try Parser.parse(std.testing.allocator, "\tlet x = 1 let y = 2\n");
    defer ast.deinit(std.testing.allocator);
    try std.testing.expect(ast.errors.len > 0);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();
    try render(ast, "tabs.nov", &output.writer);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "\n\t") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "^") != null);
}
