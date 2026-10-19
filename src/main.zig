const std = @import("std");
const build_options = @import("build_options");
const clap = @import("clap");
const Parser = @import("Parser.zig");
const Diagnostics = @import("Diagnostics.zig");
const Formatter = @import("Formatter.zig");
const Module = @import("Module.zig");
const Dependencies = @import("Dependencies.zig");
const Lowering = @import("Lowering.zig");
const CEmitter = @import("CEmitter.zig");
const Driver = @import("Driver.zig");

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-v, --version          Display the version and exit.
    \\--check                Check formatting without changing files (fmt only).
    \\--semantic             Check the bounded semantic/lowering subset (check only).
    \\--debug                Compile with -O0 -g (build/run only).
    \\--release              Compile with -O2 (default; build/run only).
    \\--cc <str>             C compiler executable (build/run only).
    \\-o, --output <str>      Output executable path (build only).
    \\<arg>...
    \\
);

fn usage(writer: *std.Io.Writer) !void {
    try writer.writeAll(
        \\Usage: nov check [--semantic] <file>...
        \\       nov fmt [--check] <file>...
        \\       nov emit-c <file>
        \\       nov [build] <file> [-o|--output <path>] [--cc <executable>] [--debug|--release]
        \\       nov run <file> [--cc <executable>] [--debug|--release]
        \\
        \\Check syntax, or the bounded semantic subset with --semantic.
        \\Emit program C11 with emit-c, compile with build, or compile and run with run.
        \\
        \\Options:
        \\
    );
    try clap.help(writer, clap.Help, &params, .{
        .description_on_new_line = false,
        .description_indent = 2,
        .spacing_between_parameters = 0,
        .indent = 2,
    });
}

pub fn main(init: std.process.Init) !u8 {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_file = std.Io.File.stderr().writer(init.io, &stderr_buffer);
    const stderr = &stderr_file.interface;
    defer stderr.flush() catch {};

    var diag: clap.Diagnostic = .{};
    var res = clap.parse(clap.Help, &params, .{ .arg = clap.parsers.string, .str = clap.parsers.string }, init.minimal.args, .{
        .allocator = init.gpa,
        .diagnostic = &diag,
    }) catch |err| {
        try diag.report(stderr, err);
        try usage(stderr);
        return 1;
    };
    defer res.deinit();

    if (res.args.version != 0) {
        var buffer: [128]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        try stdout.interface.print("nov {f}\n", .{build_options.version});
        try stdout.interface.flush();
        return 0;
    }
    if (res.args.help != 0) {
        try usage(stderr);
        return 0;
    }

    const args = res.positionals[0];
    const is_emit = args.len != 0 and std.mem.eql(u8, args[0], "emit-c");
    const is_fmt = args.len != 0 and std.mem.eql(u8, args[0], "fmt");
    const implicit_build = args.len != 0 and (std.mem.endsWith(u8, args[0], ".nov") or
        std.mem.indexOfAny(u8, args[0], "/\\") != null);
    const is_build = implicit_build or (args.len != 0 and std.mem.eql(u8, args[0], "build"));
    const is_run = args.len != 0 and std.mem.eql(u8, args[0], "run");
    if (res.args.debug != 0 and res.args.release != 0) {
        try stderr.writeAll("error: --debug and --release are mutually exclusive\n");
        return 1;
    }
    if ((res.args.debug != 0 or res.args.release != 0) and !is_build and !is_run) {
        try stderr.writeAll("error: --debug and --release require the build or run command\n");
        return 1;
    }
    const mode: Driver.Mode = if (res.args.debug != 0) .debug else .release;
    if (res.args.cc != null and !is_build and !is_run) {
        try stderr.writeAll("error: --cc requires the build or run command\n");
        return 1;
    }
    if (res.args.output != null and !is_build) {
        try stderr.writeAll("error: --output requires the build command\n");
        return 1;
    }
    if (res.args.cc) |cc| {
        if (cc.len == 0) {
            try stderr.writeAll("error: --cc requires a nonempty executable name or path\n");
            return 1;
        }
    }
    if (res.args.output) |output| {
        if (output.len == 0) {
            try stderr.writeAll("error: --output requires a nonempty path\n");
            return 1;
        }
    }
    if (res.args.semantic != 0 and (args.len == 0 or !std.mem.eql(u8, args[0], "check"))) {
        try stderr.writeAll("error: --semantic requires the check command\n");
        return 1;
    }
    if (res.args.check != 0 and !is_fmt) {
        try stderr.writeAll("error: --check requires the fmt command\n");
        return 1;
    }
    if (is_build or is_run) {
        const files = if (implicit_build) args else args[1..];
        if (files.len != 1) {
            try stderr.print("error: {s} requires exactly one file; program arguments are not supported yet\n", .{if (is_run) "run" else "build"});
            return 1;
        }
        return buildFile(init.io, init.gpa, init.environ_map, files[0], res.args.output, res.args.cc, mode, is_run, stderr) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ files[0], @errorName(err) });
            return 1;
        };
    }
    if (is_emit) {
        if (args.len != 2) {
            try stderr.writeAll("error: emit-c requires exactly one file\n");
            return 1;
        }
        var buffer: [4096]u8 = undefined;
        var stdout = std.Io.File.stdout().writer(init.io, &buffer);
        const valid = emitFile(init.io, init.gpa, args[1], stderr, &stdout.interface) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ args[1], @errorName(err) });
            return 1;
        };
        try stdout.interface.flush();
        return if (valid) 0 else 1;
    }
    if (args.len < 2 or (!is_fmt and !std.mem.eql(u8, args[0], "check"))) {
        if (args.len != 0) try stderr.print("error: unknown command or missing file for '{s}'; expected 'check', 'fmt', 'emit-c', 'build', 'run', or a .nov file/path\n", .{args[0]});
        try usage(stderr);
        return 1;
    }

    var failed = false;
    for (args[1..]) |path| {
        const valid = processFile(init.io, init.gpa, path, stderr, is_fmt, res.args.check != 0, res.args.semantic != 0) catch |err| {
            try stderr.print("{s}: error: {s}\n", .{ path, @errorName(err) });
            failed = true;
            continue;
        };
        failed = failed or !valid;
    }
    return if (failed) 1 else 0;
}

fn processFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, stderr: *std.Io.Writer, is_fmt: bool, check_only: bool, semantic: bool) !bool {
    const cwd = std.Io.Dir.cwd();
    // Resolve symlinks so atomic replacement updates the target, not the link.
    const resolved = try cwd.realPathFileAlloc(io, path, allocator);
    defer allocator.free(resolved);
    const source = try cwd.readFileAllocOptions(io, resolved, allocator, .limited(1024 * 1024), .of(u8), 0);
    defer allocator.free(source);
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;
    if (semantic) return checkSemantic(allocator, path, source, stderr);
    var ast = try Parser.parse(allocator, source);
    defer ast.deinit(allocator);
    try Diagnostics.render(ast, path, stderr);
    if (ast.errors.len != 0) return false;
    if (!is_fmt) return true;

    const formatted = Formatter.format(allocator, source) catch |err| switch (err) {
        error.UnsupportedSyntax => {
            try stderr.print("{s}: error: formatting this syntax is not supported yet; file unchanged\n", .{path});
            return false;
        },
        else => return err,
    };
    defer allocator.free(formatted);
    if (std.mem.eql(u8, source, formatted)) return true;
    if (check_only) {
        try stderr.print("{s}: error: file is not formatted\n", .{path});
        return false;
    }

    try replaceFormattedFile(io, allocator, resolved, source, formatted);
    return true;
}

fn buildFile(io: std.Io, allocator: std.mem.Allocator, env: *const std.process.Environ.Map, path: []const u8, output_option: ?[]const u8, cc: ?[]const u8, mode: Driver.Mode, execute: bool, stderr: *std.Io.Writer) !u8 {
    var source = std.Io.Writer.Allocating.init(allocator);
    defer source.deinit();
    if (!try emitFile(io, allocator, path, stderr, &source.writer)) return 1;

    const temp_root = env.get("TMPDIR") orelse env.get("TEMP") orelse
        (if (@import("builtin").os.tag == .windows) "." else "/tmp");
    const workspace = try Driver.Workspace.init(io, allocator, temp_root);
    defer workspace.deinit(io);
    const c_path = try workspace.file("program.c");
    defer allocator.free(c_path);
    const exe_path = try workspace.file(if (@import("builtin").os.tag == .windows) "program.exe" else "program");
    defer allocator.free(exe_path);
    const cwd = std.Io.Dir.cwd();
    try cwd.writeFile(io, .{ .sub_path = c_path, .data = source.written() });
    if (!try Driver.compile(io, .{ .cc = cc, .nov_cc = env.get("NOV_CC"), .env_cc = env.get("CC"), .mode = mode }, c_path, exe_path, stderr)) return 1;
    if (execute) return Driver.run(io, exe_path, stderr);

    const default_output = if (output_option == null) try Driver.defaultOutput(allocator, path) else null;
    defer if (default_output) |output| allocator.free(output);
    const output = output_option orelse default_output.?;
    const source_path = try cwd.realPathFileAlloc(io, path, allocator);
    defer allocator.free(source_path);
    const existing_output = cwd.realPathFileAlloc(io, output, allocator) catch |err| switch (err) {
        error.FileNotFound => null,
        else => return err,
    };
    defer if (existing_output) |resolved| allocator.free(resolved);
    if (existing_output) |resolved| {
        if (std.mem.eql(u8, source_path, resolved)) {
            try stderr.writeAll("error: output path refers to the input source file\n");
            return 1;
        }
    }
    // Publish only a successful build, retaining executable permissions.
    try cwd.copyFile(exe_path, cwd, output, io, .{});
    return 0;
}

fn emitFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    const source = try std.Io.Dir.cwd().readFileAllocOptions(io, path, allocator, .limited(1024 * 1024), .of(u8), 0);
    defer allocator.free(source);
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidSource;
    return emitSource(allocator, path, source, stderr, stdout);
}

fn emitSource(allocator: std.mem.Allocator, path: []const u8, source: [:0]const u8, stderr: *std.Io.Writer, stdout: *std.Io.Writer) !bool {
    var module = try Module.init(allocator, path, source);
    defer module.deinit();
    if (!module.hasErrors()) {
        var dependencies = try Dependencies.analyze(&module);
        defer dependencies.deinit();
        if (!module.hasErrors()) {
            var result = try Lowering.lower(&module);
            defer result.deinit();
            if (!module.hasErrors()) {
                var diagnostic: CEmitter.Diagnostic = undefined;
                const c_source = CEmitter.emitProgram(allocator, &result.ir, &diagnostic) catch |err| switch (err) {
                    error.UnsupportedIntegerWidth, error.MissingEntrypoint, error.InvalidEntrypoint => {
                        if (err == error.MissingEntrypoint) {
                            if (module.lookup("main")) |id| {
                                const declaration = module.getDeclaration(id).?;
                                diagnostic.reason = .entrypoint_signature;
                                diagnostic.span = .{ .start = declaration.name_span.start, .end = declaration.name_span.end };
                            }
                        }
                        try Diagnostics.renderBackend(&module, diagnostic, stderr);
                        return false;
                    },
                    else => return err,
                };
                defer allocator.free(c_source);
                try stdout.writeAll(c_source);
                return true;
            }
        }
    }
    try Diagnostics.renderModule(&module, stderr);
    return false;
}

fn checkSemantic(allocator: std.mem.Allocator, path: []const u8, source: []const u8, stderr: *std.Io.Writer) !bool {
    var module = try Module.init(allocator, path, source);
    defer module.deinit();
    if (!module.hasErrors()) {
        var dependencies = try Dependencies.analyze(&module);
        defer dependencies.deinit();
        if (!module.hasErrors()) {
            // Partial semantic passes leave deferred entries; lowering rejects
            // those rather than reporting unchecked source as valid.
            var result = try Lowering.lower(&module);
            defer result.deinit();
        }
    }
    try Diagnostics.renderModule(&module, stderr);
    return !module.hasErrors();
}

fn replaceFormattedFile(io: std.Io, allocator: std.mem.Allocator, path: []const u8, original: []const u8, formatted: []const u8) !void {
    const cwd = std.Io.Dir.cwd();
    const file = try cwd.openFile(io, path, .{});
    const stat = file.stat(io) catch |err| {
        file.close(io);
        return err;
    };
    file.close(io);
    var atomic = try cwd.createFileAtomic(io, path, .{ .replace = true, .permissions = stat.permissions });
    defer atomic.deinit(io);
    try atomic.file.setPermissions(io, stat.permissions);
    var buffer: [4096]u8 = undefined;
    var writer = atomic.file.writer(io, &buffer);
    try writer.interface.writeAll(formatted);
    try writer.interface.flush();
    // Detect edits made while parsing/rendering or preparing the replacement.
    // There is no portable compare-and-swap rename; a final rename race remains.
    const current = try cwd.readFileAlloc(io, path, allocator, .limited(1024 * 1024));
    defer allocator.free(current);
    if (!std.mem.eql(u8, original, current)) return error.SourceChanged;
    try atomic.replace(io);
}

test "emit-c buffers source and renders frontend entrypoint and backend failures" {
    const allocator = std.testing.allocator;
    const cases = [_]struct { source: [:0]const u8, message: ?[]const u8 = null }{
        .{ .source = "let main: () -> void = {}" },
        .{ .source = "", .message = "requires a root function 'main'" },
        .{ .source = "let main: i32 = 1", .message = "must have type () -> void" },
        .{ .source = "let main: () -> bool = true", .message = "must have type () -> void" },
        .{ .source = "let main: (x: bool) -> void = {}", .message = "must have type () -> void" },
        .{ .source = "let broken =", .message = "expected" },
        .{ .source = "let main: () -> void = absent()", .message = "unknown name" },
        .{ .source = "let main: () -> void = {}\r\n\tlet wide: (x: i128) -> i128 = x", .message = "C backend only supports" },
        .{ .source = "let wide: () -> u128 = 1\nlet main: () -> void = {}", .message = "C backend only supports" },
    };
    for (cases) |case| {
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        var output = std.Io.Writer.Allocating.init(allocator);
        defer output.deinit();
        try std.testing.expectEqual(case.message == null, try emitSource(allocator, "input.nov", case.source, &diagnostics.writer, &output.writer));
        if (case.message) |message| {
            try std.testing.expectEqualStrings("", output.written());
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), message) != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "input.nov:") != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "^") != null);
            if (std.mem.indexOf(u8, case.source, "\r\n") != null) {
                try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "input.nov:2:32: error:") != null);
                try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "\n\t" ++ " " ** 30 ++ "^\n") != null);
            }
        } else {
            try std.testing.expectEqualStrings("", diagnostics.written());
            try std.testing.expect(std.mem.indexOf(u8, output.written(), "int main(void)") != null);
        }
    }
}

test "formatting refuses to overwrite a changed source" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const updated = "; edited concurrently\nlet x = 2\n";
    try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = updated });
    const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
    defer allocator.free(path);
    try std.testing.expectError(error.SourceChanged, replaceFormattedFile(io, allocator, path, "let x=1\n", "let x = 1\n"));
    const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(updated, actual);
}

test "format files atomically and leave rejected input unchanged" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cases = [_]struct { source: []const u8, check_only: bool = false, valid: bool, expected: []const u8 }{
        .{ .source = "let x=1+2\n", .valid = true, .expected = "let x = 1 + 2\n" },
        .{ .source = "let x=1+2\n", .check_only = true, .valid = false, .expected = "let x=1+2\n" },
        .{ .source = "let x =\n", .valid = false, .expected = "let x =\n" },
        .{ .source = "let x = foo(1)\n", .valid = false, .expected = "let x = foo(1)\n" },
    };
    for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
        defer allocator.free(path);
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(case.valid, try processFile(io, allocator, path, &diagnostics.writer, true, case.check_only, false));
        const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
        defer allocator.free(actual);
        try std.testing.expectEqualStrings(case.expected, actual);
    }
}

test "semantic file checks reject errors and deferred forms without writing" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cases = [_]struct { source: []const u8, message: ?[]const u8 = null }{
        .{ .source = "let base: i32 = 42\nlet first: (x: i32) -> i32 = second(x)\nlet second: (x: i32) -> i32 = first(x)\nlet empty: () -> void = {}" },
        .{ .source = "let x: i128 = 1" }, // No C backend width restriction.
        .{ .source = "let broken =", .message = "expected" },
        .{ .source = "let x = 1\nlet x = 2", .message = "first declaration here" },
        .{ .source = "let x = absent", .message = "unknown name 'absent'" },
        .{ .source = "let x = y\nlet y = x", .message = "cyclic value dependency" },
        .{ .source = "let x: i8 = 128", .message = "type annotation here" },
        .{ .source = "let f: () -> i32 = true", .message = "does not match" },
        .{ .source = "let f: (x: i32) -> i32 = x\nlet g: () -> i32 = f()", .message = "argument count" },
        .{ .source = "let x = 1", .message = "lowering this source form" },
        .{ .source = "let mut x: i32 = 1", .message = "lowering this source form" },
        .{ .source = "let f: (x: bool) -> bool = x and true", .message = "lowering this source form" },
    };
    for (cases) |case| {
        try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = case.source });
        const path = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
        defer allocator.free(path);
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        try std.testing.expectEqual(case.message == null, try processFile(io, allocator, path, &diagnostics.writer, false, false, true));
        if (case.message) |message| {
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), message) != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "input.nov:") != null);
            try std.testing.expect(std.mem.indexOf(u8, diagnostics.written(), "^") != null);
        } else try std.testing.expectEqualStrings("", diagnostics.written());
        const actual = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(4096));
        defer allocator.free(actual);
        try std.testing.expectEqualStrings(case.source, actual);
    }
}

test "build and run integrate discovery, preserve outputs, and clean temporary files" {
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{ .iterate = true });
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    try tmp.dir.writeFile(io, .{ .sub_path = "input.nov", .data = "let main: () -> void = {}\n" });
    const input = try tmp.dir.realPathFileAlloc(io, "input.nov", allocator);
    defer allocator.free(input);
    var env = std.process.Environ.Map.init(allocator);
    defer env.deinit();
    try env.put("TMPDIR", root);
    var compilers: [3][:0]const u8 = undefined;
    var count: usize = 0;
    defer for (compilers[0..count]) |path| allocator.free(path);
    for (0..3) |index| {
        const name = try std.fmt.allocPrint(allocator, "compiler {d};literal", .{index});
        defer allocator.free(name);
        const script = try std.fmt.allocPrint(
            allocator,
            "#!/bin/sh\n[ \"$1\" = -std=c11 ] && [ \"$2\" = -O2 ] && [ -f \"$3\" ] && [ \"$4\" = -o ] || exit 90\nprintf '#!/bin/sh\\nexit {d}\\n' > \"$5\"\n/bin/chmod 700 \"$5\"\n",
            .{37 + index},
        );
        defer allocator.free(script);
        try tmp.dir.writeFile(io, .{ .sub_path = name, .data = script });
        try tmp.dir.setFilePermissions(io, name, .fromMode(0o700), .{});
        compilers[index] = try tmp.dir.realPathFileAlloc(io, name, allocator);
        count += 1;
    }
    try env.put("NOV_CC", compilers[1]);
    try env.put("CC", compilers[2]);
    var diagnostics = std.Io.Writer.Allocating.init(allocator);
    defer diagnostics.deinit();
    try std.testing.expectEqual(@as(u8, 37), try buildFile(io, allocator, &env, input, null, compilers[0], .release, true, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 38), try buildFile(io, allocator, &env, input, null, "/nov-missing/explicit", .release, true, &diagnostics.writer));
    try env.put("NOV_CC", "");
    try std.testing.expectEqual(@as(u8, 39), try buildFile(io, allocator, &env, input, null, null, .release, true, &diagnostics.writer));
    const output = try std.fs.path.join(allocator, &.{ root, "output with spaces;literal" });
    defer allocator.free(output);
    try std.testing.expectEqual(@as(u8, 0), try buildFile(io, allocator, &env, input, output, compilers[0], .release, false, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 37), try Driver.run(io, output, &diagnostics.writer));
    try tmp.dir.writeFile(io, .{ .sub_path = "failure", .data = "#!/bin/sh\nexit 51\n" });
    try tmp.dir.setFilePermissions(io, "failure", .fromMode(0o700), .{});
    const failure = try tmp.dir.realPathFileAlloc(io, "failure", allocator);
    defer allocator.free(failure);
    try std.testing.expectEqual(@as(u8, 1), try buildFile(io, allocator, &env, input, output, failure, .release, false, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 37), try Driver.run(io, output, &diagnostics.writer));
    try std.testing.expectEqual(@as(u8, 1), try buildFile(io, allocator, &env, input, input, compilers[0], .release, false, &diagnostics.writer));
    const source = try tmp.dir.readFileAlloc(io, "input.nov", allocator, .limited(1024));
    defer allocator.free(source);
    try std.testing.expectEqualStrings("let main: () -> void = {}\n", source);
    var iterator = tmp.dir.iterate();
    while (try iterator.next(io)) |entry| {
        try std.testing.expect(!std.mem.startsWith(u8, entry.name, "nov-"));
    }
}

test {
    _ = @import("Ast.zig");
    _ = @import("Ir.zig");
    _ = @import("Module.zig");
    _ = @import("Dependencies.zig");
    _ = @import("TypeResolver.zig");
    _ = @import("Sema.zig");
    _ = @import("Literal.zig");
    _ = @import("BodyChecker.zig");
    _ = @import("Lowering.zig");
    _ = @import("CEmitter.zig");
    _ = @import("Tokenizer.zig");
    _ = @import("string_literal.zig");
    _ = Driver;
    _ = Diagnostics;
    _ = Formatter;
}
