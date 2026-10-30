# Nov

Nov is a multi-paradigm ~functional~ programming language.

Nov [typing discipline](https://en.wikipedia.org/wiki/Type_system) is
[static](https://en.wikipedia.org/wiki/Static_typing),
[strong](https://en.wikipedia.org/wiki/Strong_and_weak_typing),
sound and
[manifest](https://en.wikipedia.org/wiki/Manifest_typing) with bidirectional inference.

Nov has automatic memory management via a minimal tracing GC.
<!-- https://github.com/ivmai/bdwgc -->
<!-- autofree? https://docs.vlang.io/memory-management.html -->

## Building

Requires Zig 0.17.0.

Growable arrays use an internal libc-only nonmoving mark/sweep GC. The
runtime is emitted for managed values and closures; programs using only scalars
and ordinary functions retain scalar-only emission.
See the [runtime API and root discipline](src/runtime/README.md) for layout, C
boundary, OOM, testing, and unsupported-reference contracts. Array slicing
and general aliases remain deferred; bounded local array-element borrowing is
supported below. Immutable UTF-8 strings are supported below.

```sh
zig build
zig build test
zig fmt --check src build.zig
```

Check syntax in one or more files with `zig build run -- check file.nov`.
Diagnostics include the filename, line, column, source line, and a caret. Invalid
input or unreadable files produce a nonzero exit status. Single-file programs can
be compiled and executed through the C11 backend. The new typed IR provides
owned, canonical type/constant storage and verification. Internal module passes
collect names and diagnose duplicate/unknown names and initializer dependency
cycles for a bounded expression subset (type aliases are not runtime initializers).
Internal annotation analysis resolves
explicit `bool`, `void`, fixed-width integer, `f32`/`f64`, native `int`/`uint`/`float` and function
types, and checks numeric and boolean literals against annotations. Bounded function-body checking binds
parameters and checks integer arithmetic, boolean expressions, comparisons and
calls against collected signatures, including recursive calls. Immutable and mutable
locals (explicitly typed or inferred from their initializer) and blocks
with final-expression results are supported in function bodies. Immutable module-level
[type aliases](#type-aliases) name existing canonical types. Unconstrained integer
literals default to signed pointer-sized `int` after contextual inference;
shadowing and global mutation remain deferred or unsupported. Direct range
literal bounds retain their dedicated native `uint`/`int` rule (see [Range](#range)).
Lowering produces typed operations with source spans, ordered operands/call
arguments, explicit returns, and structured regions for boolean `and`/`or`,
`if`/`else`, scalar `match`, and condition-based or unconditional loops with optional continuation
clauses, value-carrying breaks, and conditional/range-loop exhaustion (`else`) results.
It requires immutable literal globals; unit/global block initializers are
not lowered yet. An internal C11 backend emits verified IR, including fixed-width
integer arithmetic and program entrypoint emission through `emit-c`.
`@print`/`@println` support booleans, runes, strings, inferred or explicitly typed integers
(including `int`/`uint`), `f32`/`f64`, and default structural formatting for printable arrays,
structs, unions and enums. Nominal values, including nested values, can override
the default with an exact container method marked `@[stringify]`. Print contexts
also accept compile-time type names as static strings, not runtime type values.
Plain `check` remains syntax-only.

Use `zig build run -- check --semantic file.nov` for the current bounded
semantic/lowering subset. It runs declaration collection, initializer dependency
analysis, annotation/literal and function-body checking, then typed lowering and
IR verification. Source errors and unsupported/deferred forms fail with source
diagnostics (including related notes where available); recovered syntax errors
stop semantic analysis. Runtime globals must be immutable scalar constants: explicitly
typed integer/float/boolean/rune/string literals, inferred integer/float (including literal
negation), boolean, rune or plain UTF-8 string literals, or scalar literals
explicitly contextualized with `@as(T, literal)`.
Immutable module-level type aliases are also supported. Functions need explicit supported signatures and bounded
expression bodies (including forward/recursive calls, local declarations and
assignments, nested blocks, boolean short-circuiting, `if`/`else`, explicit returns,
bounded integer range loops, condition-based or unconditional loops with continuation clauses and unlabeled
`break`/`continue`, value-producing unconditional, conditional-with-`else`, or range-with-`else` loops,
and empty void bodies).
Bare integer globals default to `int` when no explicit type context is supplied.
Mutable globals, general nonliteral global initializers and global unit
values are currently rejected, along with the other
unsupported forms listed above. This is not a backend compatibility check: it
does not emit C, validate C integer widths, require `main`, build, or execute a
program. Each input file is checked independently; any failed/unreadable input
makes the command fail, without modifying files.

Emit a single-file program as C11 with `zig build run -- emit-c file.nov`
(or `nov emit-c file.nov` after installation). C source goes to stdout; diagnostics
go to stderr with a nonzero exit status and no partial source on analysis or
emission failure. Exactly one input file is required; `--semantic` and `--check`
are not accepted. This command does not modify files, invoke a C compiler, or run
the program. Redirect stdout to a `.c` file if desired.

The current target is native: `int`/`uint` resolve using the pointer width of the
running Nov compiler (64 bits on a 64-bit target, 32 on a 32-bit target).
There is no Nov cross-compilation option. All emitted C, including library output,
contains a C11 `_Static_assert` that `sizeof(void *) * CHAR_BIT` matches that width.
Compile emitted C for the same pointer width; a mismatched C compiler target is
rejected rather than silently changing alias semantics. This guard applies even
to programs without aliases, keeping one native-target contract without carrying
source spelling through canonical IR.

Build or run a single-file program (after `zig build`, use `zig-out/bin/nov`):

```sh
nov file.nov                         # same as nov build file.nov
nov build file.nov                    # ./file (file.exe on Windows)
nov file.nov -o path/to/program       # --output is equivalent
nov run file.nov --debug              # -O0 -g
nov build file.nov --release          # -O3 (also the default)
nov file.nov --cc /path/to/cc

nov check --semantic examples/fibo.nov examples/fizzbuzz.nov
nov fmt --check examples/fibo.nov examples/fizzbuzz.nov
nov run examples/fibo.nov            # 55
nov run examples/fizzbuzz.nov        # fifteen lines, 1 through FizzBuzz at 15
nov run examples/range-search.nov    # exclusive/inclusive searches and MAX exhaustion
nov run examples/type.nov            # x: 5, y: hello. z: true
nov run examples/results.nov         # success and propagated error handling
```

Both examples also work with `--debug` and `--release`, and can be built with
`nov build examples/fibo.nov` / `nov build examples/fizzbuzz.nov`.

`build` defaults to the input basename without its extension in the current
working directory. It atomically replaces the output only after successful
compilation and refuses an output path resolving to the input source. `run`
compiles a temporary executable, inherits stdin/stdout/stderr and the current
working directory, then returns the program's exit code. On POSIX, signal
termination is diagnosed and mapped to `128 + signal` (capped at 255). Program
arguments are not supported yet; extra positional arguments are rejected.
`--output` is build-only; `--cc`, `--debug`, and `--release` are build/run-only.
The two mode flags are mutually exclusive. Neither command accepts `--semantic`
or `--check`. Omitting the command accepts all build options: the first positional
argument must end in `.nov` or contain a path separator (`/` or `\`); bare unknown
words remain command errors. Use an explicit path (such as `./source`) or `build`
for a source filename without the `.nov` extension.

Compiler discovery tries `--cc`, nonempty `NOV_CC`, nonempty `CC`, `cc`, then
`zig cc` if `zig` is on PATH. Each configured value is **one executable name or
path**, including paths containing spaces, not shell fragments or flag lists:
`CC='cc -g'` and `--cc 'zig cc'` do not mean multiple arguments. Use an executable
wrapper for extra compiler options, or put `zig` on PATH for the `zig cc` fallback.
Missing executables advance to the next candidate, including missing explicit or
environment selections. An executable that cannot start for another reason, or
starts and fails, stops discovery and fails the command. Empty environment values
are ignored; an empty `--cc` is an error. Every invocation uses `-std=c11`;
default/release builds add `-O3`, while debug builds add `-O0 -g`. These select C
optimization/debug information only, not different Nov semantics: wrapping
arithmetic, panic checks, output, and exit behavior stay identical in both modes.
No shell is involved. Compiler diagnostics (including compiler stdout) go to
stderr; compiler failure exits nonzero without running the program. If nothing
is found, the diagnostic lists candidates tried and suggests `nov emit-c`.

Generated C and executables are kept in a private temporary directory under
`TMPDIR`, then `TEMP`, then `/tmp` (current directory on Windows without either
variable), and removed after success, compiler failure, or normal child exit.
As with most temporary files, force-killing Nov or losing power may leave them
behind. `run` does not leave a build output in the current directory.

Programs require a root function named `main` with exactly type `() -> void`:

```nov
let helper: () -> void = {}
let main: () -> void = helper()
```

The C wrapper exits 0 when Nov `main` returns and the final stdout flush succeeds;
arithmetic and stdout failures panic with exit status 1. A panic first attempts to
flush stdout, then writes `panic: <message>\n` to stderr (no backtrace yet).
`nov emit-c --library file.nov` emits library C11 without a process `main` and does
not require a Nov entrypoint. Plain `emit-c`, build and run still require the root
entrypoint above. The library mode accepts `--lib-dir` but is not a shared-library
builder; compile/link the emitted C with your chosen C toolchain. Internally,
`CEmitter.emit` emits the library and `CEmitter.emitProgram` adds the wrapper.
Backend integer widths are restricted to 8, 16, 32 and 64 bits, including nested
function signatures, with source diagnostics for unsupported widths.

`@print` and `@println` return `void` and write arguments to buffered stdout,
separated by one space. Booleans use `true`/`false`; integers use decimal notation;
runes encode their scalar as UTF-8, including NUL.
`@println` appends a newline, including when called without arguments; `@print()`
outputs nothing. Arguments are evaluated left to right before the builtin writes
any output. Each stdout write and the program's final flush are checked; failure
panics with `panic: stdout write failed`. Runtime helpers are libc-only, `static`,
and emitted only when used. Library emission has no exit wrapper: the embedding
caller is responsible for its final stdout flush.

```nov
let number: i64 = -42
let answer: () -> i64 = number
let main: () -> void = @println(true, answer())
```

This outputs `true -42` followed by a newline. Bare integer literals such as
`@println(42)` default to signed pointer-sized `int`. Explicit annotations, `@as`,
call signatures and typed sibling expressions take precedence over that fallback.
Interpolated strings can be printed like any string. Functions/closures, ranges
and void values have no default representation; aggregates containing them also
reject unless a valid enclosing hook overrides that value and stops structural
recursion. A void union alternative is payloadless and printable. Unspecialized generic templates and namespaces are not
runtime printable. Float leaves use the [float formatting contract](#floats). The C
backend continues to reject integer widths other than 8/16/32/64.

Default formatting recurses through nested aggregates:

| Value | Representation |
| --- | --- |
| Array | `[a, b]` (empty: `[]`) |
| Struct | `Entry[ name: Ada, scores: [1, 2, 3], color: Color.blue ]` |
| Empty struct | `Empty[]` |
| Union with record payload | `Message.entry[ name: Ada, scores: [1, 2, 3], color: Color.blue ]` |
| Union with scalar payload | `Message.number[7]` |
| Void union alternative | `Message.none` |
| Enum | `Color.red` |

Strings and runes are raw UTF-8, without quotes or escaping, even when nested.
This is debugging output, not an unambiguous source-roundtrip format. Every nominal
value, including nested fields, elements and union payloads, prefers its explicit
`@[stringify]` converter with exact signature `(SelfType) -> string`. Each
encountered hook runs exactly once in traversal order: array index order, struct
declaration order, and the active union payload. A valid
hook replaces that value's entire representation and stops structural recursion,
so unsupported fields behind it do not prevent printing. Malformed or inaccessible
explicit hooks reject even when nested; only the absence of a hook enables the
default. A default record payload is flattened into the union's brackets, omitting
its own type name. If that payload has a hook, its output replaces the fields:
`Message.entry[ <custom output> ]` (with spaces around the custom payload).

After ordinary argument evaluation and value copies, direct default aggregate
printing streams without building a formatting string. Nested hooks can still
allocate, perform value copies and call user code with side effects during that
traversal. Interpolation converts aggregates in one pass with rooted string
accumulation and concatenations, not a measurement pass or one-allocation scheme;
hooks are not repeated to calculate output size.
See [`examples/printing.nov`](examples/printing.nov), whose marked converter is named
`format`.

### Explicit stringify converters

Place argument-free `@[stringify]` on an ordinary immutable container method with
a body. At most one method per struct, union or enum may be marked, and the
annotation cannot be repeated on that method. Its name is arbitrary; its exact
signature must be `(SelfType) -> string`, with one by-value parameter of the
containing nominal type and an immutable string result. Parameter names are
unrestricted, and aliases of the exact nominal type and `string` are accepted.
An ordinary method in a generic container is allowed: its signature is checked
against the concrete container instance. The converter cannot have its own generic
function parameters or combine `@[stringify]` with `@[noreturn]`, `@[test]`,
`@[export]` or `@[extern]`. Top-level/local functions, mutable declarations,
bodyless functions, fields and type aliases cannot be marked.

```nov
let Entry = struct {
    name: string,

    @[stringify]
    let format: (entry: Entry) -> string = "entry:{entry.name}"

    let toString: (entry: Entry) -> string = "ordinary:{entry.name}"
}

let main: () = {
    let entry = Entry[.name = "Ada"]
    @println(entry, [entry], "{entry}")
    @println(entry.toString())
}
```

The first line is `entry:Ada [entry:Ada] entry:Ada`; the second is `ordinary:Ada`.
There is no magic method name: an unmarked `toString` is an ordinary callable
method and has no effect on printing or interpolation, even if its signature
matches. Without a marked converter, recursive default formatting applies.
Marked methods also remain ordinary callable methods (`entry.format()` or
`Entry.format(entry)`). Cross-file implicit conversion requires `@[public]` on
the marked method itself; a public container alone does not expose it. Private or
malformed marked converters diagnose rather than enabling structural fallback,
including when nested. Diagnostics refer to `@[stringify]`: invalid placement,
arguments, repetition or incompatible annotations reject at declaration collection;
a second marked method diagnoses with a note to the first; inaccessible or
non-exact converters reject when conversion is requested.

Print contexts also recognize compile-time type syntax:

```nov
let MyType = struct { ready: bool, }
let Ints = []int
let main: () = {
    @println(int, []int, MyType, (int)->bool)
    @println("{int} {MyType} {Ints}")
}
```

These emit static strings, not runtime type values. Aliases display canonical,
unqualified names; native `int`/`uint` display `iN`/`uN` at the native width
(for example `i64`/`u64` on a 64-bit target), function types display `(iN) -> bool`,
and concrete generic instances display names such as `Box(bool)`. Inline `[]int`
in interpolation is not accepted by the existing parser grammar; an alias such as
`Ints` works. This adds no builtin or general runtime type-value support.

`@as(T, value)` supplies an explicit expected type inside a function body:

```nov
let main: () -> void = {
    @println("{@as(int, 1)}", @as(uint, 1))
    let numbers = @as([]int, [1, 2])
    @println(numbers[0])
}
```

The first argument uses annotation type syntax, including supported array and
function types; it is not evaluated as a value. The second argument is evaluated
exactly once and checked against that canonical type. Boolean, string, function
and void values are supported as well as floats and contextual numeric/array literals.
The enclosing context must still match exactly: `@as(int, @as(uint, 1))` is an
error, not a signedness conversion. There is no widening, narrowing, numeric cast,
type-alias evaluation or general type-value inference. Ascription adds no array
copy boundary; existing local, argument and return copy rules still apply.
Interpolation accepts ascription within its existing bounded expression grammar
(unnamed function parameter types work; quoted strings and named parameter colons
inside interpolation remain unsupported).

A runnable arithmetic example is [`examples/checkout.nov`](examples/checkout.nov):

```sh
nov run examples/checkout.nov
# 3 6750
```

It computes the discounted total in cents for three items priced at 2500 each,
using typed globals, locals, parameters, block results, function calls and printing.
Quantity is printed
as `uint`; arithmetic uses `int` consistently. No implicit signed/unsigned
conversion is introduced; the count argument has its own `int` context.

Function blocks use their final expression as the result, including with a newline
before `}`. Empty blocks and blocks ending in a declaration produce `void`.
Statements and initializers execute left to right; a local becomes visible only
**after** its initializer, so self/forward local references are errors. Nested
blocks have lexical scopes; names do not escape them, and separate sibling blocks
may reuse a name. Shadowing any visible local, parameter, global or primitive
name remains unsupported. Non-final expressions must return `void`; explicit
value-discard syntax is not implemented yet.

```nov
let twice: (value: int) -> int = {
    let result: int = value + value
    @println(result)
    result
}
let main: () -> void = {
    let answer: int = twice(21)
    @println(answer)
}
```

This prints `42` on two lines. Locals require an initializer. The annotation may
be omitted in `let name = expression` and `let mut name = expression` when the
initializer already determines a supported `bool`, integer (fixed-width or native
`int`/`uint`), float, rune, string, enum, record/union, range, function-value or array type. Examples include
`let ready = true`, `let result = value + 1`, `let callback = twice`, and
`let mut copy = values` for an already typed array. Calls, indexing, `.len`, and
block/conditional results use the same bidirectional rules as other expressions.
No type is inferred from later uses or from the enclosing function result for a
local initializer. `let n = 1`, `let n = -1` and `let n = 1 + 2` infer `int`
when no stronger initializer context exists; there are no widening conversions. Array literals infer
`[]T` from any already known storable element type, including nested arrays and ranges.
A later element can supply context to earlier literals: `let values = [0, typed_value]`.
Known element types must match exactly; no signedness or width widening occurs.
An expected `[]T` takes precedence and checks every element against `T`, including
literal bounds. Without stronger context, `[1, 2]` infers `[]int` and `[[1], [2]]`
infers `[][]int`. Empty `[]` (including unresolved nested empty arrays) still needs
an element type.
Inferred arrays keep the same copy and GC-root semantics
as annotated arrays. See [examples/inferred-locals.nov](examples/inferred-locals.nov).

Explicit annotations remain supported, including immutable `void` values and
function values with unnamed parameter types (e.g. `let callback: (int) -> int = twice`).
Function expressions and nonrecursive local function definitions support automatic
captures, as described under [Functions](#functions). Inferred `void` locals,
and labels remain unsupported. Mutable locals use `let mut` and support
existing storable runtime values (not `void`). Direct
identifier assignment (`n = value`), numeric `+=`, `-=`, `*=`, `/=`, and integer-only `%=`
return `void`; a block ending in assignment therefore also returns `void`.
Assignments require a mutable local, not a parameter or global. RHS types must
match exactly, without implicit numeric conversions. Compound assignments read
the old local value before evaluating the RHS, then apply the existing
integer wrapping/division or IEEE float rules and store once. RHS effects can update the same local;
the compound calculation still uses the captured old value. A returning RHS exits
without performing the pending store. Mutable initializers execute exactly once,
before the local is visible; each later read captures its current value. Updates
to outer locals persist across selected conditional branches and short-circuit
RHS regions; skipped regions do not read or write their locals. Slots remain
lexically scoped and separate sibling blocks may reuse names.
Global block initializers remain unsupported by lowering.

Boolean `and`/`or` evaluate the left operand exactly once, then evaluate the right
operand only when needed (`and`: left is true; `or`: left is false). Both operands
must typecheck as `bool`, even when the RHS will be skipped at runtime. RHS blocks,
local initializers, calls, printing and arithmetic panics are all conditional;
other expressions and call/print arguments retain left-to-right evaluation.
Nested operators merge only their boolean result, not RHS-local values. The IR
stores nestable RHS instruction ranges in the owned function body; verification
rejects crossing ranges, forward references and references escaping a region.
The C backend emits conditional blocks, never eager RHS temporaries. This does
not add new global initializer forms.

`if` conditions must be `bool` and execute exactly once. Only the selected branch
executes, including its local initializers, calls, printing and arithmetic panics;
both branches are typechecked. Braces are mandatory, with `else if` chains allowed.
An explicit `else` requires matching result types on paths that continue (including
`void` and function values); returning paths do not contribute a merge value. A surrounding result/argument/annotation type or either branch
can supply the type context before fallback; two otherwise unconstrained integer
branches infer `int`. There are no implicit numeric conversions. Without `else`, the expression
is `void`, and its then-branch must either produce `void` or return from the function. Each branch has its own lexical scope;
branch locals cannot escape or become visible in the other branch. Conditional
regions nest with `and`/`or`, and IR verification rejects cross-branch references
and escaping private values.

```nov
let magnitude: (n: int) -> int = if n < 0 {-n} else {n}
let main: () -> void = {
    let answer: int = magnitude(-42)
    if answer == 42 {@println(answer)}
}
```

Explicit `return expression` exits the enclosing function, even inside nested
blocks, local initializers, call arguments, conditional branches or short-circuit
RHS blocks. The expression is checked against the **function's result type**, not
the surrounding block/argument type. It is evaluated once before returning;
later effects in that execution path do not run. A bare `return` requires a
`void` function; `return` with a void expression is also valid. Existing parser
rules apply, including one-newline expression continuation; use `{return}` for a
bare-return expression body before another declaration.

```nov
let magnitude: (n: int) -> int = {
    if n < 0 {return -n}
    n
}
```

A branch that returns does not need to supply the conditional's value. An `if`
without `else` can return early and still leave a reachable fallback, as above.
If both branches return, the conditional does not continue. An `and`/`or` RHS may
return, but the skipped-RHS path still continues with the boolean result. These
rules do not fold constant conditions: both conditional paths are checked even
when a condition is a literal. Statements after an unconditional return (including
a nested block or `if` whose branches both return) are diagnosed as unreachable.
Non-void functions still require a correctly typed final value on every path
that reaches the end. IR verification checks return operands, signatures,
region-private references and terminal tails; C emission merges only continuing
paths and never reads a value from a returning path.

Condition-based `loop condition { ... }` expressions without `else` return `void`.
The condition must be `bool` and is evaluated before **every** iteration, including the first,
using the current local state. False skips the body and exits. With no continuation,
normal body completion and unlabeled `continue` reevaluate the condition. With a
continuation, `loop condition : update { ... }`, normal body completion and a
body-side `continue` execute `update` before reevaluating the condition. A `continue`
inside the condition or continuation goes directly to the condition; it does not
execute or restart the continuation. `break` and `return` skip the continuation.
Unlabeled `break` exits the innermost loop. Both controls also work in nested blocks, initializers,
branches, arguments and short-circuit RHS regions, without evaluating the rest of
that path or performing a pending assignment. Within a nested loop's condition,
they target that nested loop. `return` still exits the enclosing function.

The body must produce `void` on paths that continue; a terminal `return`, `break`
or `continue` needs no result. Its lexical scope is fresh each iteration: local
initializers execute again, body-local slots do not escape, and updates to outer
mutable locals persist. Condition, body, and continuation blocks have separate
lexical scopes; the continuation cannot access body locals. All regions are checked
even when the condition is literally false. Without `else`, conditional loops remain
`void` and reject value breaks.
Statements after an unconditional loop control
within a block are unreachable. Existing one-newline expression continuation
also applies to `break`: use `{break}` before a following expression to keep the
break bare.

```nov
let main: () -> void = {
    let mut n: int = 0
    loop n < 5 {
        n += 1
        if n == 2 {continue}
        if n == 4 {break}
        @println(n)
    }
    @println(n)
}
```

This prints `1`, `3`, and `4` on separate lines. Unconditional loops can produce a
value with `break expression`; the loop's expected type constrains the expression,
or a known break-value type supplies inference. Break values must have exactly
compatible types; bare `break` produces `void` and cannot mix with nonvoid breaks.
Context and typed sibling break values win before fallback; `loop {break 1}`
otherwise infers `int`. As with returns and
assignments, array break values are independent copies; immutable strings share
storage. Inner loops have independent break-value scopes.

```nov
let answer: () -> int = loop {
    let val: int = 42
    break val
}
```

Unconditional `loop : update { ... }` also supports continuation clauses and value
breaks. Continuing body/continuation paths must still produce `void`. A nonvoid
loop requires a value-carrying break; loops with only `return`/`continue` do not
acquire a special never type. Loops are otherwise conservatively considered able
to finish. See `examples/loop-values.nov` for a search loop returning its result.

Conditional loops with `else` yield either a break value or the exhaustion result:

```nov
let contains: (begin: int, end: int, needle: int) -> bool = {
    let mut n = begin
    loop n < end : n += 1 {
        if n == needle {break true}
    } else {
        false
    }
}
```

`else` executes exactly when the condition becomes false, including before the
first iteration. Breaks from the condition, body, or continuation skip it; returns
exit the function without running it. Continuing `else` results and break values
must have exactly compatible types. Expected context, a known break type, or the
`else` result can supply inference in either direction before integer fallback,
without widening. Array results copy at both break and exhaustion value boundaries; strings
share immutable storage. A terminal `else` (`return`, outer `break`, or outer
`continue`) contributes no merge value. If it is terminal and no path breaks from
the loop, the whole expression is terminal; this does not introduce a never type
for unconditional loops.

The exhaustion branch has its own lexical scope **outside the completed loop's
control scope**: its unlabeled `break`/`continue` target an enclosing outer loop,
including the outer continuation routing, or diagnose if there is none. It cannot
restart the completed loop. Nested loop conditions/bodies retain independent result
contexts. `loop { ... } else { ... }` remains rejected: unconditional loops have no
condition-false exhaustion path. Newline handling matches `if`/`else`: keep `else`
on the same line as the closing body brace; a newline after `else` before its block
is not accepted.

Loop IR owns separate condition, body, optional continuation, and optional exhaustion
ranges, with checked break operands/result types and explicit terminal merges.
Verification checks nesting, terminal paths, and region-private references. Only outer slots carry mutable state across
backedges. C emission evaluates ordered instructions in the correct region, rather
than hoisting snapshots before the loop. Managed loop results stay rooted across
allocations and function returns. Bounded ascending exclusive/inclusive integer
range loops reuse this IR with endpoint snapshots and a private counter. Inclusive
continuations exit at the endpoint before adding one, so even MAX cannot overflow.
General iterators and labels remain unsupported and diagnosed.

Format files in place with `zig build run -- fmt file.nov`, or use
`zig build run -- fmt --check file.nov` to check formatting without writing.
Both commands accept multiple files and directories:

```sh
nov fmt src examples --exclude src/generated --exclude examples/broken.nov
nov fmt --check . --exclude vendor
nov fmt - < input.nov > formatted.nov
nov fmt --check - < input.nov
```

Directories are scanned recursively for case-sensitive `.nov` filenames only;
explicit files retain the previous behavior and need not have that extension.
Hidden directories are included: there are no implicit ignore rules. Repeatable
`--exclude <path>` options exclude literal files or whole directory subtrees,
including explicit inputs, with path-component boundaries (`vendor` does not
exclude `vendor-old`). Paths are resolved lexically against the working directory;
`..` and trailing separators are normalized. Exclusions are not globs, basename
patterns, or gitignore rules. Existing exclusion targets are also resolved so file
symlink aliases cannot reinclude an excluded target. Missing exclusions are valid.

Directory symlink entries are skipped, even as explicit inputs; scans never recurse
through them. Explicit paths may still contain symlink ancestors resolved by the OS.
File symlinks are supported: replacement updates the resolved target and preserves
the link. Broken/cyclic selected file links are errors. Inputs are sorted by normalized
absolute path (bytewise, case-sensitive), and repeated/overlapping inputs and file
symlink aliases are processed once per resolved path; the first sorted alias is used
for reporting. Hard-link identity is not deduplicated. Successful writes report one
absolute path per changed file to stderr in sorted order; unchanged files are silent.
Traversal errors are reported in sorted traversal order before file processing.
Unreadable inputs, traversal errors, invalid or unsupported sources fail the command
but do not stop other selected files. An empty or fully excluded selection succeeds.
`--check` never writes or reports successful changes: unformatted inputs receive
stderr diagnostics and cause exit status 1 (as do other input errors).

`-` must be the sole input and cannot be combined with `--exclude`; build/run flags
and `--semantic` remain invalid for `fmt`. Stdin formatting writes only validated
formatted source to stdout, with diagnostics labeled `<stdin>` on stderr and no
changed-file report. `--check -` emits no source, succeeds for already formatted
input, and otherwise diagnoses the input and exits 1. Empty stdin succeeds. Both
file and stdin sources have a 1 MiB limit; NUL bytes are rejected. Input validation
failure emits no partial formatted source (an output I/O failure can still leave a
partial stream).

File replacement is atomic and retains permissions. A source-content recheck refuses
to overwrite concurrent edits detected before rename. There is still a final rename
race, and traversal/path resolution is not a security boundary against concurrent
symlink changes. Ownership, hard-link relationships and other metadata are not
promised to survive replacement.

Formatting supports scalar `let`/`let mut`
declarations, literals and operators, function signatures and bodies, calls (including
`@print`/`@println`), local blocks, `if`/`else if`/`else`, condition and unconditional
loops with optional continuation clauses, conditional-loop `else` and value breaks,
returns, unlabeled `break`/`continue`, and direct identifier assignments (`=`, `+=`, `-=`,
`*=`, `/=`, `%=`). Supported array annotations (including nested `[]T`),
literals (including empty `[]` and nested literals), indexing (including negative
indices), `.len` reads, and plain `name[index] = value` stores are also supported.
Nonempty blocks use four-space indentation. Function parentheses,
list commas (including trailing commas), precedence, comments, and multiline
continuations are preserved; no grouping parentheses are invented.
This is syntax formatting, not semantic validation: existing literal/bitwise/shift
formatting does not make those forms executable. Array formatting does not check
expected literal types, scope, mutability, or whether a base is an array; it does
not enable array globals or new language semantics. Other members, `.len` writes,
indexed compound stores and stores through computed bases remain unsupported.
Array slicing, pointers, other unsupported memory/container forms, labels,
iterator/other unsupported loop forms, attributes, quoted identifiers,
BOMs and shebangs remain unsupported or invalid. Unsupported syntax is reported
without modifying the file. Formatted output is reparsed and checked for structural,
token, and comment preservation before atomic replacement; any failed preservation
check also leaves the file unchanged.
Declarations are separated by newlines; EOF also terminates a complete declaration,
so the final newline is optional. Formatting adds a final newline.

## Testing Nov code

`nov test file.nov` runs top-level `@[test]` functions in that file, in source order.
Tests must be ordinary immutable `() -> void` functions with bodies. No Nov `main`
is required, and an ordinary `main` is not executed. Imported files provide helpers
and types, but their tests are not automatically selected.

```nov
let add: (a: i32, b: i32) -> i32 = a + b

@[test]
let addition: () = {
    @assert(add(19, 23) == 42)
    @assert(add(-7, 7) == 0, "addition handles negative operands")
}
```

`@assert(condition)` and `@assert(condition, message)` require a bool condition and
an optional string message, and return void. Both arguments follow ordinary eager,
left-to-right evaluation, exactly once—even when the assertion succeeds. A failure
prints `path:line:column: assertion failed`, optionally followed by `: message`,
and terminates the process with exit status 1. Assertions work in ordinary functions
too and remain enabled in every debug/release mode.

Test declarations are excluded from ordinary semantic checking and emission:
their signatures, bodies, imports and generic demands are ignored by normal builds.
Source syntax is still parsed and attribute placement is validated. Neither ordinary
code nor other tests may call or extract a test function, even if it is public.
Tests cannot also be extern, export, noreturn, methods or generic containers.

The command compiles one executable, then runs each test in a fresh subprocess.
Panic, nonzero exit or a crash fails that test without stopping the remaining tests;
there is no panic recovery or stack unwinding. Child output is inherited, each test
gets a `PASS`/`FAIL` result, and a final summary reports passed, failed and total counts.
The command exits 0 only if compilation and every test process succeed. Empty suites
still compile and report `0 passed; 0 failed; 0 total`.

`--cc`, `--debug`, `--release` and `--lib-dir` use the same compiler and installed-
stdlib rules as build/run; default C optimization is `-O3`. Processes run sequentially,
inherit the environment and share the filesystem; this is memory isolation, not a
sandbox. There is no default timeout or test-filter option in this initial runner.
Temporary generated files/executables are removed on normal completion and errors.

[`examples/tests.nov`](examples/tests.nov) covers arithmetic, independent array
copies and standard-library Option, with commands in its opening comments.

# Concepts

## Comments
Regular comments start with `;` and end on a newline.

Doc comments start with exactly three semicolon (i.e. `;;;` but not `;;;;`).
Doc comments are used to automatically generate documentation.

## Functions
Nov functions can be understood as lazy or postponed expressions that takes parameters.

Parameters are immutable by default.

### Function expressions and closures

A function expression places its block directly after the signature, without `=`:

```nov
let double = (value: int) -> int { value * 2 }

let makeAdder = (base: int) -> (int) -> int {
    (value: int) -> int { base + value }
}

let addFive = makeAdder(5)
@println(addFive(3)) ; 8
```

The binding infers the function type. Parameters still need names and explicit
types; omitting the return annotation means `void`, not inferred return typing.
`let printHello = () { @println("hello") }` is therefore a void function.
Existing `let double: (value: int) -> int = value * 2` declarations remain valid.
The new form also works for module functions, methods and `@[test]` functions.

Referenced outer bindings are captured automatically when the function expression
is evaluated:

- Immutable bindings and parameters capture readonly snapshots. Arrays and arrays
  inside nested records/union record payloads receive recursively independent backing
  storage; immutable strings share.
- Mutable bindings capture shared GC-managed cells. The outer binding and every
  capturing closure observe the same updates, even after the defining function
  returns. Ordinary reads still make normal recursive value copies; sharing the
  captured cell does not turn array or record reads/assignments into aliases.
- Copying or passing a function value shares its environment and captured cells;
  it does not repeat capture evaluation or deep-copy the environment.
- Captures propagate through nested closures. GC retains environments, cells and
  their managed contents; no stack reference escapes.

```nov
let mut count: uint = 0
let increment = () { count += 1 }
increment()
@println(count) ; 1
count = 10
increment()
@println(count) ; 11

let saved = count
let snapshot = () -> uint { saved } ; explicitly take a readonly snapshot
```

Captured mutable cells are shared state, **not** exclusive scoped `&local` loans.
Borrowed bindings cannot be captured. Scoped borrows of captured values remain
possible inside a closure, but calls while a loan of shared captured storage is
live are conservatively rejected: the callee could access the same cell. Calls
are also rejected during ordinary iteration over a mutable array captured by a
closure. This covers methods, pipes and implicit conversions, and uses finalized
capture information: even a capture discovered later can make an earlier loop call
unsafe on a backedge. These conservative restrictions await effect analysis.

Functions may be returned, passed, assigned, and selected by control flow using
exact existing function types. Recursive local definitions, escaping reference
capabilities, function-valued fields, and function-valued C ABI
boundaries remain unsupported. Shared captures are single-threaded; no concurrency
semantics are introduced. See [`examples/closures.nov`](examples/closures.nov).

### Inferred generic functions (bounded executable subset)

Named generic functions and local generic closures support direct calls and
contextual specialization as function values, using backtick type variables in
their signatures. A backtick marks an inferred type, not a runtime or explicit
compile-time argument:

```nov
let first: (items: []`T) -> `T = items[0]
let add: (x: `a, y: `b) -> `c = x + y

let items: []uint = [1, 2]
let head = first(items)       ; uint from a typed runtime argument
let sum: uint = add(1, 2)    ; expected result and body constrain all three types
let text = add("hel", "lo") ; string
```

Same-named variables agree; distinct names are related by exact body constraints.
Inference combines typed runtime arguments, the expected call result and supported
body constraints. Existing integer/string `+` requires one exact supported type
for both operands and result. Exact argument, expected-result and body constraints
win before integer fallback: `let sum = add(1, 2)` infers `int`. There are no
implicit conversions. An identity call `id(1)` likewise infers `int` without other
context; an expected `u8` result or a correlated typed `u8` sibling can select `u8`
before fallback.

Signatures may contain nested arrays, function types and nominal generic-container
templates such as ``Box(`T)``. Concrete function values can be passed to generic
functions with function-typed parameters. Top-level functions, imported/public
aliases, namespace static functions and generic-container static/sibling methods
support direct calls; aliases share canonical specializations. Generic instance
methods also infer method-local backtick variables:

```nov
let Box: (T: type) -> type = struct {
    value: T,
    let map: (self: Box(T), convert: (T) -> `U) -> Box(`U) =
        Box(`U)[ .value = convert(self.value) ]
}
let identity: (x: `V) -> `V = x
let toBool: (x: uint) -> bool = x == 7
let number = Box(uint)[ .value = 7 ]
let mapped = number.map(toBool)             ; Box(bool)
let same: Box(uint) = number.map(identity)  ; contextual generic callback
```

The concrete receiver container binds `T`; explicit arguments, callback signatures,
the expected result and supported body constraints infer method-local `U`. Static
`Box(uint).map(number, toBool)` and instance `number.map(toBool)` calls share the
same canonical specialization, including through imported/public aliases. Existing
public/private access and field-before-method lookup rules still apply. The exact
nominal receiver is passed by value: it evaluates and snapshots once **before**
explicit arguments, which evaluate once left-to-right. No mutable receiver or
implicit borrow is introduced. Bound method values (`number.map`) and callable
fields are not supported.

A named generic function also specializes as a value when an exact concrete
function type is expected, using existing syntax:

```nov
let identity: (x: `T) -> `T = x
let apply: (f: (`T) -> `U, x: `T) -> `U = f(x)

let callback: (uint) -> uint = identity
let cast = @as((uint) -> uint, identity)
let answer = apply(identity, @as(uint, 42)) ; uint from the other argument
let contextual: uint = apply(identity, 42) ; expected result constrains the callback
```

Callback inference is bidirectional: it combines the callback signature, other
arguments, expected result and supported body constraints until the signature is
concrete. Specialization produces an ordinary function reference to the demanded
canonical specialization, not a runtime-polymorphic or unspecialized function
value. Inference does not duplicate runtime evaluation; callees and arguments
retain their existing once-only evaluation order. Typed assignments, function
results, branches and arrays can also supply an exact function-type context.
Aliases, imports/public reexports, namespace static functions and generic-container
static methods follow the same canonicalization and existing visibility rules;
contextual specialization does not expose private members.

Without enough context, `let callback = identity` remains ambiguous, while
`apply(identity, 42)` resolves to `int` through integer fallback after exact
constraints. Conflicting signatures are errors; no implicit conversion is introduced. No new parser or explicit specialization syntax is needed.

Body inference is deliberately bounded. It gathers constraints from parameter/local
reads and explicitly typed globals, string/rune/bool literals, boolean operations,
exact operand relationships for arithmetic/bitwise/comparison operations, array
indexing/literals, string indexing, known record fields and array/string `.len`,
block bindings, `if/else`, conditional/unconditional loops, scoped returns and
breaks, `@as`, supported direct calls and concrete function-value calls. Integer
literals supply deferred `int` fallback candidates only after exact constraints;
index operands do not supply inference constraints. These relationships do not expand operator support:
ordinary checking of the specialized body still validates every operation. Forms
outside this solver subset are not general inference rules; if resolving a type
requires an unsupported constraint, compilation gives an explicit unsupported-
inference diagnostic. If types are otherwise resolved, ordinary body checking
still applies. Terminating paths (including direct `noreturn` calls and `@panic`)
do not contribute false `void` constraints; bare returns do constrain the enclosing
result to `void`. This is internal control-flow tracking, not a new source type.

Local function expressions can introduce their own inferred type variables, as
can typed local function declarations:

```nov
let apply: (callback: (`T) -> `T, value: `T) -> `T = callback(value)
let main: () = {
    let identity = (value: `T) -> `T { value }
    let local: (value: `U) -> `U = { value }
    let direct = identity(true)
    let callback: (uint) -> uint = local
    let contextual: uint = apply(identity, 42)
    @println(direct, callback(7), contextual)
}
```

Direct calls, exact expected function types and bidirectional callback inference
use the same bounded constraints and ambiguity/conflict rules described above.
Generic closures may also appear inline as callees or contextual callbacks.
Inside a generic body, enclosing type variables retain their substituted types;
a local signature may introduce additional inferred variables.

Captures are evaluated **once at template creation**, not at specialization or
call time, even if the template is never specialized. Immutable bindings and
parameters snapshot recursively independent array backing; immutable strings
share storage. Mutable bindings share the same GC-managed cells across every
specialization and copied specialized function value. Specializing or copying
does not recapture or clone the environment. Each runtime factory call or loop
evaluation creates a distinct environment, even when specialization code is
reused. Escaping specialized values keep their environments and captured contents
reachable through GC, including transitive captures through ordinary closures.
The existing borrowing and captured-shared-storage call restrictions still apply.

A local template binding is immutable and is not itself a first-class runtime
function value: an exact concrete context must select a specialization before it
can be copied, passed or returned. For example, `let copy = identity` is ambiguous;
`let copy: (bool) -> bool = identity` is supported. Module-scope inferred generic
function expressions remain unsupported; use named generic declarations there.
Local recursion, runtime-polymorphic unspecialized generic values and explicit
function specialization syntax remain unsupported. See
[`examples/generic-closures.nov`](examples/generic-closures.nov) for local generic
calls, callbacks, shared captures and escaping specialized values.

Specialization is demand-driven and canonical, bounded to 64 expansion levels and
1024 specializations per defining module; inference is bounded to 8192 terms and
128 levels. Unused generic bodies are not type-checked. Failed/OOM lowering rolls
back partial specializations and demands; speculative result probes do not publish
methods, specializations, declaration IDs or body/worklist demands. Fresh nominal
result layouts can supply inference constraints without depending on earlier
instantiation order. See
[`examples/generic-functions.nov`](examples/generic-functions.nov).

`#` is separate: it denotes **proposed explicit compile-time arguments**, not
inferred type variables. Compile-time argument handling is not implemented.

### Other function syntax

The examples below mix implemented forms with older design sketches. Pointer
parameters, tuples and explicit `#` compile-time arguments are proposals only.

Parenthesis are specific to functions, whenever there is a parenthesis there is a function involved.
```nov
; signature: () -> void
let doNothing: () = {}

; here x is passed as a reference and its value is mutable
; signature: (*mut int) -> void
let retNothing: (x: *mut int) = {
    x += 1
}

; functions body doesn't need to be a block
; signature: () -> int
let ret2: () -> int = 2

; signature: (int, int) -> int
let add: (a: int, b: int) -> int = a + b

; return a tuple
; see #Struct for more information about tuples and structs
; signature: (int, int) -> struct{ int, int }
let div: (a: int, b: int) -> struct{ int, int } = {
    [a / b, a % b]
}

; PROPOSAL ONLY: explicit compile-time type arguments, not implemented.
; # marks an explicit compile-time argument, not an inferred type variable.
; See Comptime; inferred generics instead use backtick type variables.
; proposed signature: (#type, any, any) -> bool
let eql: (T: #type, x: T, y: T) -> bool = x == y
eql(int, 1, 2) ; returns false
eql(int, true, 0) ; compile error

let add: (T: #type, x: T, y: T) -> T = x + y
; it's easy to create a custom function from a generic one
let addInt: (x: int, y: int) -> int = add(int, x, y)
addInt(1, 2) ; returns 3
add(string, "he", "llo") ; returns "hello"
add(bool, true, false) ; compile error: bool doesn't support `+` operator
```

## Block & Grouping
Blocks are expression, they are also used for grouping.
```nov
let x = {1} ; this is the same as `let x = 1`

let x = {1 + 1} * 3 ; x = 6

; we can do more complex things with blocks
; here x will be equal to "hahaha"
let x = {
    let max = 1 + 2
    let mut str = ""
    loop _ in 0..max {
        str += "ha"
    }
    str
}
```

## Containers
Containers are types that can have fields and declarations. Available
containers are [Enum](#Enum), [Struct](#Struct) and [Union](#Union).

[Arrays](#Arrays) are builtin containers.

Similarly to functions, brackets are specific to containers.

### Enum

#### Executable nominal enums

Unnumbered module declarations such as `let Kind = enum { not_found, permission_denied, io, }`
introduce nominal enum types. Different declarations are distinct even when their
names and tags match; module aliases preserve identity.

- `Kind.not_found` determines its own type. Contextual `.not_found` requires an
  expected enum type; bare tags do not infer a type. Tags use no brackets.
- Equality and inequality require exactly the same enum type. Enums are not
  integers: numeric conversions, arithmetic, ordering and integer-backed syntax
  are unsupported. Internal `uint32_t` ordinals are private implementation details,
  not an exposed numeric or FFI contract.
- Single-enum matches accept contextual or qualified tag patterns, `|` alternatives,
  and `_`. All tags must be covered or a final catchall supplied. Unknown tags and
  wholly covered prongs diagnose with related earlier-pattern notes; partially
  overlapping alternatives retain first-match behavior. Enum patterns do not bind
  payload captures.
- Enum locals, parameters, returns, control-flow results, literal globals, flat
  record fields (including tag defaults), union payloads and generic type arguments
  work with exact nominal typing. `Result(uint, Kind)` supports ordinary `.!`
  propagation with exact error identity. Enum leaves need no GC roots.
- Static container methods and `@[public]` metadata follow the existing bounded
  container rules. Default printing/interpolation uses `Kind.tag`; an exact
  container method marked `@[stringify]` with signature `(Kind) -> string`
  overrides it, including for nested enum values, and runs once per encountered value.
- Enum instance calls such as `kind.eql(other)` use the value-receiver rules below.
  Enum arrays support exact-type element equality and membership. Explicit numbered
  values/backing types remain unsupported.

[examples/structured-errors.nov](examples/structured-errors.nov) uses an enum error
kind in a record payload, exhaustive error-message selection and special handling
for `.not_found`.

The numbered-enum example below remains design-only; its instance-call syntax is
supported for unnumbered enums.

Nov enums are just like C enums except that they can have methods and their fields are not global.

Overall enums are a nice way to represent tags or constant numbers.
```nov
let Season = enum {
    spring,     ; 0
    summer = 5, ; 5
    autumn,     ; 6
    winter,     ; 7

    let eql: (self: Season, other: Season) -> bool = {
        self == other
    }
}

let s1 = Season.spring ; type of s1 is inferred from the initializer
let s2: Season = .spring ; type of the initializer is inferred from the type of s2
s1.eql(s2) |> @println ; prints true

match s1 {
    .spring => ... ; do something
    _ => {} ; do nothing
}
```

### Struct

#### Executable nominal value structs

[examples/type.nov](examples/type.nov) runs unchanged through semantic checking,
format checking, C emission, and default/debug/release build and execution.
The delivered subset is deliberately bounded:

- An immutable module declaration `let MyType = struct { ... }` introduces a
  **nominal** type. Separately declared structs are distinct even with identical
  fields. `let Alias = MyType` preserves the original identity, fields and methods;
  aliases work in annotations, method signatures, constructors and static calls.
  Forward module type names and method signatures are supported.
- Named fields require explicit `bool`, `rune`, integer (including `i128`/`u128` and native
  `int`/`uint`), `f32`/`f64`, immutable `string`, nominal enum, acyclic nominal struct types,
  or arrays of supported element types (including aliases). Semantic checking
  uses the same integer-width and literal-bound rules as scalar values. The C
  backend still supports only 8/16/32/64-bit integers: a reachable record layout
  containing a 128-bit field fails C emission with a backend width diagnostic;
  an unused record layout adds no backend requirement or emitted definition.
  Array element restrictions are unchanged. Fields end with a comma. A field may have a literal default of its exact type, with ordinary
  literal-bound checks. Defaults cannot call functions, compute values, reference
  globals or interpolate strings. Aggregate defaults are not supported: nested
  struct and array values must be supplied explicitly. Recursive layouts (including
  cycles through array element types) and layouts deeper than 128 aggregate levels
  diagnose, including aliases/imports and late generic instantiations.
  `f32`/`f64` fields are supported. Direct union/function/void fields remain unsupported; array fields may
  contain any supported storable element type, including nested arrays. Arrays of
  supported records are described below.
- Construct explicitly with `MyType[ .x = 5, .z = true ]` (or `Alias[...]`).
  Empty `MyType[]` works only when every field has a default. Unknown, duplicate,
  missing required and mistyped fields diagnose. Supplied expressions evaluate
  **once, left to right in source order**, independent of field declaration order.
  Omitted literal defaults are then inserted in declaration order and have no
  effects. Same-name shorthand `Type[.width, .height]` means
  `Type[.width = width, .height = height]`: each name is an ordinary scoped read
  checked against the field's exact expected type, with no implicit conversion.
  Shorthand and explicit fields may mix in source order; defaults follow all
  supplied fields. Imported types and generic aliases support the same syntax.
  Unknown/out-of-scope names diagnose even if the field has a default; existing
  unknown/duplicate/missing-field diagnostics remain. Array reads/deep copies,
  borrowed-value read copies and closure capture rules are unchanged.
  Union/enum payload grammar is unchanged (for example, `.item[.ready]` still
  supplies an enum literal). No new `use`/`pub` syntax is introduced.
  See [examples/constructor-shorthand.nov](examples/constructor-shorthand.nov).
  Contextual `[ .x = ... ]`/`[.width]`, anonymous records and tuples are not part
  of this slice.
- Read fields with `item.x`, including inside interpolation (`"{self.x}"`). Field
  lookup is type-directed: a record field named `len` is an ordinary field and
  does not change array/string `.len`. Assignment, locals, parameters, returns,
  branches and loop results copy record field values. Array backing is copied
  recursively through nested structs, record-array elements and union record
  payloads; immutable strings share their backing storage. Constructors, field/index
  reads, method receivers and ordinary reads through loans obey the same independence
  rule. Replacing or mutating a record binding does not change earlier copies.
- Fields of a direct mutable local support `item.field = value` with exact bool,
  integer, float, rune, string, enum, struct or array type. Nested paths such as `item.inner.count += 1`
  and exact-type subrecord replacements are supported. Integer fields support `+= -= *= /= %=`, with the
  same wrapping/division/panic rules as scalar integers; float fields support
  `+= -= *= /=` with IEEE semantics; string fields support `+=`.
  These assignments yield void. A real struct field named `len` is writable;
  array/string lengths remain read-only.
- A compound reads the old field **before** evaluating the RHS once. After the RHS,
  the final store updates only that field of the **current local slot**. Changes
  made by the RHS to other fields, or to the whole record, are preserved except
  for the targeted field. Plain assignment does not read its old field. A terminal
  RHS skips the pending operation/store without undoing earlier effects. This uses
  stable local field storage, not a whole-record snapshot replacement.
- Array-valued fields support reads, exact-type replacement, `item.inner.values += rhs`,
  indexed replacement and integer indexed compounds on nested mutable-local paths.
  Append/index updates snapshot the target array before index/RHS effects and store
  the updated snapshot into **only that field of the current record slot**. RHS
  changes to siblings survive; RHS changes to the target array are overwritten.
  Index evaluation, negative indices, bounds-check timing and terminal-control
  behavior follow the ordinary array rules below. Direct `items[index].field`
  mutation is still unsupported. Existing whole-record/record-element loans permit
  array-field replacement and append, but not indexed updates through such paths.
  See [`examples/array-fields.nov`](examples/array-fields.nov).
- Field mutation does not alter earlier value copies or immutable string backing.
  Immutable bindings/ordinary parameters (including `self`), union payloads/tags,
  temporary values and general pointer-based targets remain unsupported or
  diagnose immutable assignment. Bounded local element borrows support field updates
  as described under arrays. No implicit mutable receiver or borrowing is added.
  See [examples/struct-mutation.nov](examples/struct-mutation.nov) and
  [examples/nested-structs.nov](examples/nested-structs.nov). Nested record copies
  have independent value storage and recursively independent array backing at every
  level; immutable string backing shares.
  C declarations follow dependency order and GC roots flatten all nested owners.
- Container declarations are ordinary explicitly typed functions, not runtime
  initializers. Methods have their own container scope; they can call sibling or
  recursive methods by name. Outside that scope use `MyType.method(args...)` (an
  alias works too). Parameters, including a parameter named `self`, are ordinary
  by-value parameters. Instance syntax `item.method(args...)` inserts `item` as the
  explicit first argument; that parameter must have the exact nominal container
  type (its name need not be `self`). The receiver evaluates once before explicit
  arguments, which evaluate left-to-right. Value copies and string sharing remain
  unchanged; there is no implicit mutation or borrowing. Instance calls also work
  for non-generic unions and enums. Direct `@[noreturn]` method calls terminate
  control flow, including through instance syntax.
  Existing fields take precedence over method lookup; array/string `.len` is not
  a method. A bare `item.method` does not create a bound function or closure;
  explicit `Type.method` remains an ordinary unbound function value. Concrete
  generic union instances support these same method calls. Methods with their own
  backtick variables use the bounded instance inference described above; callable
  fields remain unsupported.
  Container constants, nested type declarations, general type values,
  struct global instances and C FFI are unsupported.
- Default printing/interpolation uses `MyType[ field: value, ... ]` (`MyType[]`
  when empty), recursively applying converters to nested fields/elements.
  An explicit `@[stringify]` container method `(MyType) -> string` overrides the whole
  value at any depth and stops recursion, allowing otherwise unprintable fields
  behind that hook. The parameter name is unrestricted, and aliases of the exact
  nominal type and `string` are accepted. Wrong signatures or inaccessible explicit
  hooks diagnose even when nested; fallback applies only when no hook exists.
  Each encountered hook runs once in traversal order. No global overload lookup is provided.
  The method name is arbitrary; unmarked `toString` methods have no formatting effect.
  Marked methods remain callable through ordinary static and instance syntax.
- Each print argument is evaluated, then (if it has an explicit hook) converted
  by **one** call to its marked converter, before moving to the next argument.
  Only after all arguments and top-level conversions finish does the final print operation
  write their values, separated by spaces, with the existing checked-write/flush
  behavior. Top-level hook effects can precede later argument effects; nested
  hooks instead run once during structural traversal and can interleave their
  effects with streamed output. Allocating methods return normal immutable strings.
- Declarations and methods are file-private by default. One argument-free
  `@[public]` exposes a supported immutable declaration across files, including
  structs, methods and module re-exports. Both file and named imports require
  public exports; cross-file printing requires `@[public]` on the `@[stringify]`
  method if an explicit hook exists. A private hook rejects rather than exposing
  structural fields.
  `@[private]` is unsupported. Duplicate, unknown or invalidly placed attributes
  diagnose. Visibility does not change C export policy.

C emission uses stack value structs with index-mangled fields and injectively
mangled method symbols. Reachable layouts precede function-pointer typedefs and
prototypes. Record temporaries live at function scope, including skipped-branch
slots; actual string `void *owner` and array `void *data` fields are rooted across
allocations, recursively through inline records.
String byte-data pointers (including static literals) are never traced. The
current uniform record path installs a GC frame even for scalar-only records;
this is implementation overhead, not heap allocation of the records themselves.

#### Broader struct syntax (design, not fully executable)

The following older examples describe future syntax outside the bounded subset
above (including container constants, contextual and anonymous values).
Struct is Nov's equivalent of [Product type](https://en.wikipedia.org/wiki/Product_type).
```nov
let MyStruct = struct {
    name: string,
    x: float = 1.0,
    y: float = 1.0,

    let max = 100.0

    let init: (name: string, x: float, y: float) -> MyStruct = {
        if x > max or y > max {
            @panic()
        }
        return [ .name = name, .x = x, .y = y ]
    }
}

; create a new MyStruct using its init method
let a = MyStruct.init("", 0, 0)

; create a new MyStruct with the container syntax
; here x and y use the default value specified i.e. 1.0
let b = MyStruct[ .name = "" ]

; same as above except that the type of the initializer is inferred from the type of c
let c: MyStruct = [ .name = "", .x = 0, .y = 0 ]

;;; Tuples
; tuples are simply anonymous structs

; is a tuple of type struct{ int, string }
let x = [ 0, "test" ]

; this is also a tuple
let y = [ .name = "", .x = 0, .y = 0 ]

; this is not a tuple because all values are of the same type, see #Arrays for more information
let z = [ 10, 20, 30 ]
```

### Union

#### Executable tagged unions

Ordinary module declarations such as `let Value = union { number: int, text: string, empty, }`
introduce nominal tagged value types. Separate declarations remain distinct; aliases
preserve identity. The current executable subset supports bool, integer, float, rune, string,
void, enum, and existing acyclic nominal struct payloads, with no payload defaults. As with structs, integer fields are
checked semantically through 128 bits, while reachable C layouts require
8/16/32/64-bit widths.

- Construct with `Value.number[7]`, or `.number[7]` under an expected union type.
  A payloadless variant uses `Value.empty[]` or contextual `.empty[]`. Payloads
  have exact types and evaluate once. Bare payloads never implicitly become unions.
- Match one union subject with `.number => |n| ...`, `.text => |s| ...`, and
  `.empty => ...`. Captures are immutable and private to their prong; omit the
  capture when unused. Every variant must be covered, or use a final `_`.
  Unknown variants, repeated/covered prongs and missing variants diagnose.
- Assignment, arguments, returns and control-flow results copy the tag and active
  payload, recursively detaching array backing in record payloads;
  strings share immutable backing storage. C emission uses a tag and separate
  zero-initialized payload fields, not an overlapping C union. String owner fields
  are rooted, including across allocations and early returns. Payload extraction
  checks the active tag in all build modes.
- Non-generic union methods and `@[public]` use the same bounded container rules
  as structs, including default `Union.tag[payload]`/`Union.tag` printing and exact
  `@[stringify]` overrides with exact signature `(SelfType) -> string` at any depth.
  Default record payloads flatten their fields into `Union.tag[ field: value, ... ]`;
  a record payload hook instead produces
  `Union.tag[ <custom output> ]`. Visibility is metadata only.
- Record payloads, including nested structs, retain exact nominal identity and value-copy semantics;
  their string fields share immutable storage. Constructors, captures and `.!`
  work with records and aliases. C emits payload record layouts before containing
  unions and roots each nested string owner, with inactive payload fields zeroed.
- Explicit tag enums, nested union/array/function payloads, variant-pattern
  alternatives, nested destructuring, direct payload field access, union equality
  and union globals remain unsupported.

The examples below describe broader design, not additional implemented features.

Union is Nov's equivalent of [Sum type](https://en.wikipedia.org/wiki/Tagged_union)
since Nov's executable unions are always tagged. Untagged C unions remain a
proposal; `@[extern]` currently applies only to function declarations.
Thus we can match on an union to find its active field.

See [Result and Option unions](#result-and-option-unions) for an example of generic unions.

```nov
let NodeKind = enum { empty, node }
let Tree = union(NodeKind) {
    empty,
    node: struct {
        value: int,
        left: *mut Tree,
        right: *mut Tree,
    },

    let sum: (self: *mut Tree) -> int = match self {
        .empty => 0
        .node => |n| n.value + n.left.sum() + n.right.sum()
    }
}
let mut leaf = Tree.empty[]
let a = Tree.node[ .value = 0, .left = &leaf, .right = &leaf ]
let b: Tree = .node[ .value = 0, .left = &leaf, .right = &leaf ]

; we can also create an union without explicit enum
let Number = union {
    raw: int = 0, ; provide a default value for Number.raw
    text: string,

    ;;; Converts a Number to an int
    let toRaw: (self: Number) -> int = {
        match self {
            .raw => |raw| return raw
            .text => ... ; TODO: parseInt
        }
    }

    @[public]
    @[operator(.@"==")]
    let eql: (a: Number, b: Number) -> bool = {
        a.toRaw() == b.toRaw()
    }
}

let x = Number.raw[3]
x.text ; runtime error

; `==` can be used to check the tag of an union and to compare two unions
; overloading `==` in an union only overload the comparison between two unions,
; not between the tag
@println(x == .raw) ; prints true
@println(x == .text) ; prints false

@println(x == .raw[4]) ; prints false
@println(x == .text["3"]) ; prints true, would have been false if we hadn't overloaded the `==` operator
```

### Generics

#### Executable type-parameterized containers

Named module declarations with explicit `type` parameters and `-> type` can
produce ordinary unions or structs:

```nov
let Result: (T: type, E: type) -> type = union {
    ok: T,
    err: E,
}
let CountResult = Result(uint, string)
```

`Result` is not a reserved builtin: its name and variant declarations are ordinary
Nov source. Calls in type positions instantiate templates. Aliases of templates
and instantiated types work; the same template with identical canonical type
arguments produces the same nominal type. Distinct templates or different type
arguments remain distinct, even with identical layouts. Expansion is bounded at
64 levels, with alias traversal bounded at 128. Only plain named `type` parameters
are supported in this slice, not value parameters, `#type`, general compile-time
execution. Named runtime generic functions use the separate bounded backtick
inference subset described above. Generic container methods
specialize by canonical concrete container type; aliases share specializations and
different type arguments remain isolated. `@This()` and immutable
`let Self = @This()` container aliases refer to that concrete type. Static and
instance calls, callback function values, and recursive/forward/sibling calls use
the ordinary exact typing, evaluation-order, copy and rooting contracts.

Signatures and bodies are checked on demand when a concrete method is referenced,
called, or required for `@[stringify]` conversion. Unused methods do not invalidate an
otherwise valid type instance; requested methods still undergo full checking.
This preserves void-payload Result construction/propagation without permitting
void parameters. Method discovery and specialization are bounded; failed checking
never exposes partial executable IR. Generic struct fields are resolved for each
concrete instance, retaining exact nominal identity. Scalar literal defaults are
checked against concrete field types; aggregate defaults remain unsupported.

```nov
let Box: (T: type) -> type = struct {
    value: T,
    let get: (self: @This()) -> T = self.value
}
let main: () = {
    let boxed = Box(uint)[ .value = 42 ]
    @println(boxed.get())
}
```

Generic structs support direct instantiated constructors, aliases/imports, finite
nested instances, field mutation, array elements and Option/Result payloads using
the existing copy/rooting rules. See [examples/generic-structs.nov](examples/generic-structs.nov).
Array-valued fields work in plain and generic acyclic structs, including `value: T`
with an array type argument or `items: []T` with a supported element type. Copies
recursively detach array backing; this copy-depth contract is settled. Recursive
layouts (also through arrays), contextual constructors, general container constants,
value parameters and general compile-time execution remain unsupported.

The following broader stack example remains design-only: `#type` proposes an
explicit compile-time type argument (not implemented), distinct from backtick
inferred type variables. Aggregate defaults and pointer-based mutable receivers
are not delivered. Array fields themselves work.

We can generate a type with a function thus creating a generic type.
```nov
let Stack: (T: #type) -> type = struct {
    list: []T = [],

    let Self = @This()

    @[public]
    let push: (self: *mut Self, value: T) = {
        self.list += [value]
    }

    @[public]
    let pop: (self: *mut Self) -> Option(T) = {
        if self.list.len == 0 {
            return .none
        }
        let value = self.list[self.list.len - 1]
        self.list.len -= 1
        return .some[value]
    }

    @[public]
    let isEmpty: (self: Self) -> bool = self.list.len == 0
}
```

### Result and Option unions

#### Executable Result propagation

[examples/results.nov](examples/results.nov) imports the standard library's ordinary
generic `Result`, handles both variants, and propagates errors with `.!`. It prints:

```text
Doubled: 42
Error: value exceeds 100
```

For a union with exactly `ok` and `err` variants, `value.!` evaluates `value` once.
It yields the `ok` payload on success. On error it constructs the enclosing
function's `err` variant and returns immediately, skipping subsequent effects.
The enclosing result must also have exactly `ok` and `err` variants, and its error
payload type must match exactly; success types may differ. This is a structural
operator rule, not special handling of a type named `Result`. Void payloads work.
Outside propagation, construct returns explicitly with `.ok[value]` or
`.err[error]`; bare payload returns remain type errors.

Option-style `some`/`none` propagation is also supported as described below.
Ordinary generic union methods in the definitions below, including
`unwrapOr`/`unwrapOrElse` and `@This()`, are supported.

#### Executable Option

[`lib/option.nov`](lib/option.nov) defines the ordinary generic `Option(T)` union,
publicly available as `std.Option`. It has `some: T` and payloadless `none` variants.
Use `.some[value]` / `.none[]` under an expected Option type, or qualified
constructors such as `Option(uint).some[21]`. Matches must cover both variants
or include a catchall; `some` can capture its exact payload type.

`isSome()` / `isNone()` test the active variant and return bool without unwrapping
or inspecting the payload, including for `Option(void)` and `some[false]`.

`unwrapOr(fallback)` returns the payload or an eagerly evaluated fallback argument.
`unwrapOrElse(callback)` returns the payload or calls a zero-argument `() -> T`
callback only for `none`. These are ordinary generic methods, not compiler
special cases. Aliases, value-copy semantics, supported scalar/record payloads,
and void construction/matching follow the existing generic union rules.
`value.!` evaluates an Option-shaped union once, yields its `some` payload, or
constructs and returns `.none[]` from the enclosing Option-shaped result. Both
unions must have exactly `some` and `none` variants, with a void `none` payload;
variant order and nominal name do not matter. The enclosing success type may
differ, including void. Cross-family Option/Result propagation is rejected; no
implicit conversion or ordinary return wrapping is introduced. Later operand,
argument and statement effects are skipped when `none` propagates.

[examples/options.nov](examples/options.nov) uses safe division, propagates from
`Option(uint)` into `Option(string)` with `.!`, and shows that its lazy fallback
runs only for a missing quotient:

```text
Quotient: 21
Using fallback
Missing: 0
```

#### Standard-library foundation

[`lib/result.nov`](lib/result.nov) provides the ordinary generic `Result` definition
with public `isOk`, `isErr`, `unwrapOr` and `unwrapOrElse` methods. The predicates
return bool based only on the active tag, including with void payloads.
`unwrapOr` evaluates its fallback
as an ordinary argument even on success. `unwrapOrElse` evaluates the callback
value normally but calls its body only on error. Neither requires a Result builtin.

[`lib/std.nov`](lib/std.nov) publicly re-exports Result from `result.nov`, Option
from `option.nov`, and the `array` namespace from `array.nov`. Examples use real
imports rather than copied definitions:

```nov
let std = @import("std")
let Result = std.Result
let Option = std.Option
```

The build installs `lib/` at `<prefix>/lib/nov`. The compiler finds `std.nov` relative
to its executable at `../lib/nov`; semantic check, emit-c, build and run accept
`--lib-dir <path>` to override the library root. No implicit working-directory
search or developer-checkout path is used. Tests exercise real library imports,
callbacks and void payloads, plus a relocated executable/library installation.
See [`lib/README.md`](lib/README.md).

#### Array helpers

[`lib/array.nov`](lib/array.nov) provides public ordinary generic functions through
`std.array`:

```nov
let std = @import("std")
let array = std.array

let main: () = {
    let numbers: []int = [1, 2, 3, 4]
    let doubled = array.map(numbers, (value: int) -> int { value * 2 })
    let selected = array.filter(doubled, (value: int) -> bool { value > 4 })
    let total = array.fold(
        selected,
        @as(int, 0),
        (sum: int, value: int) -> int { sum + value }
    )
    @println("Mapped:", doubled[0], doubled[1], doubled[2], doubled[3])
    @println("Filtered:", selected[0], selected[1])
    @println("Folded:", total)
}
```

Output:

```text
Mapped: 2 4 6 8
Filtered: 6 8
Folded: 14
```

Signatures below use `T`/`U` for inferred backtick type variables:

- `map(items: []T, convert: (T) -> U) -> []U` converts each element.
- `filter(items: []T, predicate: (T) -> bool) -> []T` retains matching elements
  in their original order.
- `fold(items: []T, initial: U, combine: (U, T) -> U) -> U` folds left from the
  seed: `combine(combine(initial, items[0]), items[1])`, and so on.

Callbacks run once per element, in source order, and never run for empty input.
Empty map/filter results are empty arrays; empty fold returns the seed. Callee
and argument expressions still evaluate eagerly in normal left-to-right call
order, including the callback expression and fold seed on empty input.

Ordinary functions, capturing closures and inferred generic closures/local generic
functions are supported within the bounded inference rules. Typed inputs and seeds
(as above, or with `@as`) supply exact constraints before integer
fallback; there are no implicit conversions, and remaining ambiguity is an error.

Normal by-value copies preserve recursively independent array backing through
nested arrays, structs and active union payloads; immutable strings share storage.
Function/closure copies share their capture environments and mutable cells, not
recaptured values. These helpers add no aliasing pointer mutation. Callback
failures and panics propagate normally; the helpers do not catch errors or panics.

Map/filter currently build results by repeated append, which can be quadratic;
normal by-value copying adds overhead. Further optimization remains **PAUSED**.
[`examples/array-helpers.nov`](examples/array-helpers.nov) contains the runnable
example and six tests covering callbacks, ordering, empty inputs, folds, recursive
copies, shared closure environments and argument evaluation.

#### Executable imports

- Immutable, unannotated top-level imports support namespace bindings such as
  `let ns = @import("literal")` and direct member aliases such as
  `let Result = @import("result.nov").Result`, including qualified member chains.
  `"std"` names the installed entry module; explicit relative paths must end in
  `.nov` and resolve against the importing source directory. Computed, interpolated,
  local/runtime, mutable, annotated, absolute-path and other named imports diagnose.
- Imports do not inject bare names. Namespace aliases, qualified declarations and
  explicit re-exports work. Supported ordinary functions, constants, nominal types,
  aliases and generic union templates/methods retain their defining scope.
- Canonical paths cache each source once, including repeated/diamond imports.
  Independently declared types in different files remain nominally distinct;
  aliases and generic arguments preserve canonical identity across import paths.
  Import cycles and excessive import depth diagnose rather than partially compile.
- Each file retains its own AST and source-qualified locations. One semantic
  worklist and shared IR pools cover the compilation; diagnostics and related
  notes can refer to different files. IR owns names/layouts/body storage and source
  bounds independently of source ASTs. Imported declarations are not simulated by
  concatenation, source-name rewriting or special Result handling.
- Only the root source's `main` is an entrypoint. Imported same-named functions
  cannot collide or become the entrypoint. Build output cannot overwrite any loaded
  source, including imported/library files or their resolved symlink targets.
- Declarations and methods are file-private by default. Both named and explicit
  file imports expose only `@[public]` declarations. Aliases and re-exports cannot
  bypass access checks. A public local alias may intentionally expose a private
  type, but its unmarked methods remain file-private. Public exports do not define
  a C ABI.
- Plain `check` remains syntax-only for its input files; `check --semantic`, emit-c,
  build and run load the import graph. Formatting preserves import literals and
  does not load imports. General runtime module initializers, wildcard injection,
  arbitrary package search and broader visibility/resource policies remain deferred.

`examples/error.nov` now uses `.!` and explicit Result returns, with its alternative
implementations commented out. It is still a filesystem design example, not an
executable program: the filesystem library and its resource semantics remain future
work. Imports are implemented. Instance calls support explicit by-value receivers. Nominal enum error kinds and exhaustive enum matches are implemented. Record payloads and record interpolation are implemented;
terminal `@panic` follows the `@[noreturn]` void-function contract described below.

[examples/structured-errors.nov](examples/structured-errors.nov) is a runnable
in-memory example with records on both sides of Result, `.!` propagation into a
different success type, and captured error interpolation. It prints:

```text
readme: 42 bytes
Error: missing: entry not found
```

```nov
@[public]
let Result: (T: type, E: type) -> type = union {
    ok: T,
    err: E,

    let Self = @This() ; see #Builtins

    @[public]
    let unwrapOr: (self: Self, fallback: T) -> T = match self {
        .ok => |value| value ; catch the value and return it
        .err => fallback
    }

    ; here unwrapOrElse takes a function and not a value so the fallback is
    ; only evaluated when needed
    @[public]
    let unwrapOrElse: (self: Self, fallback: (E) -> T) -> T = match self {
        .ok => |value| value
        .err => |err| fallback(err)
    }
}

let MyResult = Result(int, string)
let my_value = MyResult.ok[0]
let my_err: MyResult = .err["my error string"]

; error handling
let file = match File.open("file.txt") {
    .ok => |f| f
    .err => |err| match err.kind {
        .not_found => {
            ; do something
        }
        _ => return err ; return early with the error
    }
}
; .! unwrap and returns the err if there is any
let file = File.open("file.txt").!
```

```nov
let Option: (T: type) -> type = union {
    some: T,
    none,

    @[public]
    let unwrapOrElse: (self: @This(), fallback: () -> T) -> T = {
        match self {
            .some => |value| value
            .none => fallback()
        }
    }
}

let x = 5
let y = Option(int).some[5]
let sum = x + y.unwrapOrElse(0)

let MyOption = Option(float)
let a = MyOption.some[1.0]
let b = MyOption.none[]
let prod = a.! * b.! ; will return .none to the calling function since b is .none
```

## Arrays

### Executable array subset

Growable `[]T` arrays support every existing storable runtime type: `bool`,
`rune`, integers, immutable `string`, nominal enums, finite acyclic records and
existing tagged unions, function values (including closures), nested arrays, and
inferred integer range values. `void` and compile-time `type` are not storable;
`f32`/`f64` are storable. Semantic integer types extend through 128 bits, but
reachable C layouts, including nested elements and function signatures, still
require 8/16/32/64-bit widths (native `int/uint` retain their pointer-width rules).
Existing annotation syntax applies in bodies and signatures; ranges have no
source-level annotation syntax and enter arrays through inference. Literals use an
expected array type when available; otherwise, a known supported element type
determines the exact element type for the whole literal, even when it
appears after untyped integer literals. For example, `[true, false]` infers
`[]bool`, and `[0, typed_integer]` infers that integer's array type before fallback.
Otherwise `[1, 2]` infers `[]int`, recursively including nested arrays such as
`[[1], [2]]` as `[][]int`. Empty `[]` still needs an element type. Type inference does not evaluate elements: evaluation remains once,
left-to-right. Array-valued parameters, locals, calls and other already typed
expressions can initialize inferred locals without changing copy semantics. See [examples/arrays.nov](examples/arrays.nov):

```nov
let main: () -> void = {
    let mut values: []int = [1, 2, 3]
    let original: []int = values
    values[-1] = 30
    values += values
    @println(original[-1], values[-1], values.len) ; 3 30 6
    values = []
}
```

- Array assignment, local initialization, by-value arguments and returns have
  **copy semantics**: recursively independent array backing, not aliases, including
  nested arrays and arrays inside active union record payloads. Immutable strings
  share backing. Function elements copy their function value and share the existing
  closure environment/captured cells, not a deep-cloned closure; immutable capture
  snapshots and mutable shared-cell capture rules are unchanged. Range elements
  copy bounds and inclusivity by value. Union arrays do not expand the supported
  payload types: direct array/union/function payloads remain unsupported.
  Immutable arrays
  cannot have their elements changed. Parameters are immutable; copy into a
  mutable local to edit them.
- `.len` has pointer-sized `uint` type. Internal capacity is also pointer-sized
  `uint`, but is not a source-level field. Indices have native `int` type, with
  `-1` denoting the last element and `-len` the first. Every out-of-bounds access
  (including empty arrays and minimum native `int`) panics with
  `panic: array index out of bounds`, exit 1, identically in debug and release.
- Plain `values[index] = element`, integer element `+=`, `-=`, `*=`, `/=`, `%=`
  and `values += other_array` require a mutable local base (a direct array local or
  a supported nested struct field path).
  Whole nested-array elements may be replaced, but direct chained writes such as
  `rows[i][j] = value`, `items[i].field` writes and arbitrary computed-base writes
  remain unsupported. Copy an element into a mutable local and assign it back,
  or use an existing scoped element loan.
  Element compound assignments use the existing wrapping arithmetic and guarded
  division/remainder rules; bool compounds are unsupported. Append accepts the
  same array type and self-append is safe. All assignments return `void`.
- Evaluation is once, left-to-right: callee before arguments, literal elements in
  order, indexing base before index. Array reads capture values; read-modify-write
  updates (append and indexed assignments) capture the left array before index/RHS
  effects, then store the updated snapshot after the RHS. Indexed compounds evaluate
  the index exactly once and read/bounds-check the old element from that same snapshot
  **before** the RHS. Plain replacement needs no old element and checks bounds only
  at its final update. A `return`, `break` or `continue` during index/RHS evaluation
  skips the pending update; effects already performed remain visible.
  The final store replaces the **whole local array** with the updated snapshot,
  discarding changes to that array made by the index or RHS, including changes to
  other elements or length. For an array-valued struct field, it replaces only the
  target field in the current record, preserving sibling fields. It does not reload
  the current target array. For example:

  ```nov
  let mut a: []int = [10, 20]
  a[-1] += {a = [99]
      3}
  @println(a[0], a[-1], a.len) ; 10 23 2
  ```
- **IMPORTANT tracked cost — optimization PAUSED.** All further optimizations
  are deferred; continue language features instead. The current
  implementation eagerly copies storage reads when independent values are needed,
  including every reachable array backing in record elements. Already-fresh
  temporaries transfer through binding, argument and return boundaries without
  being cloned again. Readonly field and simple-index projections avoid copying
  the enclosing aggregate; effectful indices retain pre-effect snapshots.
  Recursive copying remains a major performance concern: cost follows the copied
  contents, not just outer array length. Insertion and replacement now transfer
  proven-detached, single-use temporaries; append keeps its fresh outer buffer
  and recursively copies only unproven input portions. Its final RHS read uses a
  readonly projection rather than another outer-buffer clone, while LHS pre-RHS
  snapshots and internal snapshots of effectful RHS bases remain. Plain assignment
  still copies normally. Indexed compounds can reuse
  a detached snapshot after earlier scalar/length observations in the same execution
  region, avoiding a second backing allocation. Multiple consuming uses, escaping
  projections, later observations and region crossings retain safe copies. Calls,
  merges and storage reads are not backend ownership evidence. Broader elision, moves/consuming transfers and
  copy-on-write remain deferred candidates, **not approved designs**;
  any optimization must preserve value independence, static borrowing and GC safety.
  Append does not yet promise amortized constant time or preserve spare capacity
  across value copies.
  Compiler roots conservatively retain temporary backing until its instruction is
  reused or the function returns, rather than doing last-use liveness analysis.
- String/record arrays have independent element storage at ordinary copy boundaries.
  Record fields copy recursively by value, including all nested array backing;
  immutable string backing shares. Indexing
  returns a value, not an element reference. To edit a record element, copy it to a
  mutable local and assign it back, or use the checked local borrowing form below.
  Direct `items[index].field` mutation remains unsupported.
- String membership compares exact UTF-8 bytes/length, including embedded NUL, not
  owner identity. Enum elements support exact nominal equality and membership.
  Storage support adds no other equality or membership: functions, unions, records,
  arrays and ranges reject comparisons and membership as element values. Float
  elements support IEEE equality/membership and indexed `+= -= *= /=`;
  `%=` remains integer-only and string indexed `+=` is not added.
- Managed backing allocations own repeated element descriptors and expanded pointer
  maps, tracing actual string-owner and array-backing fields across the entire
  zero-initialized capacity. Record layout dependencies precede array descriptors. Map storage costs
  capacity times managed owner fields; GC never traces static byte pointers.
  See [examples/managed-arrays.nov](examples/managed-arrays.nov).
- `.len` writes are explicitly rejected: shrinking and especially initialization
  on length growth remain undesigned. No array equality, concatenation
  with `+`, slices, pointer elements, general aliases,
  array globals, or capacity API is delivered. Bounded value/reference iteration is
  supported (see [Loop](#loop)). The formatter supports
  the array syntax described above without extending these semantics.
  Arrays print/interpolate as `[a, b]` when their elements are recursively
  printable, honoring exact nominal `@[stringify]` converters once in index/traversal order.
  Functions/closures and ranges have no default representation; aggregates
  containing them need a valid enclosing hook that stops recursion before them.
  Malformed/inaccessible nested hooks reject rather than falling back.
  See [`examples/storable-arrays.nov`](examples/storable-arrays.nov) for the
  expanded element matrix.

### Local static borrowing

Scoped borrows support every currently implemented non-void runtime value type:
bool, integer, float, rune, string, enum, struct, union, array, range and function values. Borrow
a local with `let view = &value`, or a supported array element with
`let view = &items[index]`. Readonly locals and parameters can be borrowed for
reading; writes require a mutable owner.

```nov
let mut x: uint = 1
let y = &x
y += 1
@println(x) ; 2; access resumes after y's last use
```

Assignment through a borrowed name replaces the **pointee**, not the fixed reference
binding: `view = value` writes to its source. Supported operations are the ordinary
ones for that type: integer arithmetic compounds, string `+=`, array append/index
updates, record field updates, range replacement and function-value calls/replacement.
Exact typing and arithmetic rules do not change. Compounds read the old value before
RHS evaluation; a terminal RHS skips the pending operation/store.

Reading `view` yields an ordinary value. `let copy = view`, function arguments,
returns and method receivers copy values according to existing rules: arrays have
independent element storage, records copy by value and immutable strings share.
These operations do **not** copy or escape a reference capability. In particular,
passing a borrowed record to a by-value method does not give the method mutable
access to the original. Reborrowing with `&view` remains unsupported.

Borrow creation itself does not clone or allocate its target. Ordinary value reads
and operations may allocate as usual. No runtime reference-validity check is added.
See [examples/local-borrows.nov](examples/local-borrows.nov) for the type matrix.

A direct local binding `let changed = &people[index]` borrows an element of a local
array without copying it. All currently supported element types are borrowable.
Nested record field reads/writes, subrecord replacement, integer compounds, string
`+=` and whole-element replacement use the actual element. A mutable owner permits
writes; an immutable owner permits reads only. The
reference binding itself is fixed—`let changed` does not make a mutable owner’s
pointee immutable. No explicit pointer annotation or `let mut` reference is supported.

```nov
let changed = &people[0]
changed.name.text += " Lovelace"
changed.score += 1
; last use of changed ends the borrow
@println(people[0].name.text, people[0].score)
```

- Validity is checked **statically** with control-flow liveness, including branches,
  loop backedges, break/continue, returns and short-circuit paths. While a borrow
  is live, its owner is unavailable for conflicting access. For an array element,
  the entire owner array is unavailable for direct reads, copies, `.len`,
  writes, append, replacement or other borrows—even of a different index. Access
  resumes after the last possible reference use on the current path. This first
  implementation is conservatively exclusive even for readonly owners; it does
  not implement shared-borrow splitting.
- A compound keeps the loan live through RHS evaluation and final store.
  The compiler rejects owner replacement inside that RHS. It also reserves the
  owner during borrow-index evaluation: accessing the owner from the index
  expression is conservatively rejected, including read-only access.
- For element borrows, the index evaluates once with native-int negative-index
  rules and ordinary runtime bounds checking. The compiler retains a stable owner
  slot and optional normalized index, rooting managed owners and deriving addresses
  only when used. There are no runtime generation
  or reference-invalidation checks, and no interior pointer is traced by GC.
- The reference capability cannot escape or be rebound; ordinary copied pointee
  values may be returned, passed or stored. Void has no borrowable storage. Nonlocal
  sources, explicit pointer annotations, reborrows, captures of borrowed bindings, temporary
  owners and general pointer APIs remain unsupported. Array-valued struct fields
  add no new borrowing forms: `&record.field`, `&record.field[index]` and indexed
  array-field updates through a whole-record/record-element loan remain unsupported.
  Those existing loans still support array-field replacement/append and ordinary
  recursively copied reads. Nested-array elements can use existing scoped element
  loans for replacement/append and supported indexed updates. Stable owner slots
  preserve scoped nested-array replacement; this adds no chained borrow syntax,
  reborrows or direct/computed indexed-write forms.
- Normal array snapshots/copies remain independent, including snapshots evaluated
  before a later borrow inside an index or RHS. In-place borrowed writes do not
  alter earlier copies. See the unchanged borrowed-element form in
  [examples/managed-arrays.nov](examples/managed-arrays.nov).

The broader examples below describe future syntax as well as implemented syntax;
bare integer elements now default to `int`, and bounded [`@TypeOf`](#type-queries) is
implemented, but writable `.len` and the broader pointer/functional forms shown
there are **not** executable features of this subset. Array printing is implemented for recursively printable elements.

Array literals are comma-separated, may span multiple lines, and may have a
trailing comma.

```nov
let mut my_array = [1, 2, 3]
@TypeOf(my_array) ; returns []int
; array .len is read-only in the executable subset
my_array.len == 3 ; true
my_array[0] == 1 ; true
my_array[-1] == 3 ; true
; append an element
my_array += [5]
my_array |> @println ; prints [1, 2, 3, 5]
; append multiple elements
my_array += [1, 1, 7]
my_array |> @println ; prints [1, 2, 3, 5, 1, 1, 7]
; remove the last element
my_array.len -= 1
my_array |> @println ; prints [1, 2, 3, 5, 1, 1]
my_array = []
@TypeOf(my_array) ; still return []int
; check if an element is in the array
6 in my_array ; false

let my_array_of_array = [["Hello", "World!"], ["Bonjour", "Monde!"]]
@TypeOf(my_array_of_array) ; returns [][]string

; TODO:
; proposal about copy/ref of variables
; side note, it kinda sucks to hide through type if something is copied or not
; we're trying to solve that btw https://jvns.ca/blog/2024/08/06/go-structs-copied-on-assignment/
; ---
; alias because it's long to type. we specify the type so it's passed by
; reference and create an actual alias instead of copying the data
let arr_arr: *[][]string = my_array_of_array
arr_arr.len == 2 ; true

; this works like a pointer in C because it's mutable, note that the compiler
; will emit an error if it is never mutated like here
let mut ref_arr: *[][]string = my_array_of_array

; this is a copy
let arr_copy = my_array_of_array

; error type mismatch my_array_of_array is immutable
let arr_error: *mut [][]string = my_array_of_array

; imperative way of printing an array
loop arr in arr_arr {
    loop w in arr {
        @print(w + " ")
    }
    @println()
}

; functional way, I think
arr_arr.map(|arr| {
    arr.map(|word| word + " " |> @print)
    @println
}

; with monad bind operator
; type annotation is optional
arr_arr >>= |arr: []string| {
    arr >>= |word| word + " " |> @print
    @println()
}
```

## Slice

Strings support [immutable scalar-offset views](#strings). Array slicing remains unsupported.

## Range

Bounded integer ranges are reusable values describing ascending iteration:

```nov
let accept: (n: uint) -> void = @println(n)
let main: () = {
    let range = 0..10
    loop i in range {accept(i)}
    loop i in range {accept(i)} ; starts again from 0
    loop i in 0..=10 {accept(i)} ; includes 10
    loop _ in -2..2 {} ; discard the index
}
```

- `start..end` / `start..=end` evaluate `start`, then `end`, exactly once when
  constructing the range. Stored bounds do not change when their source bindings
  change later. Ranges copy by value, require no allocation, and are not consumable
  iterators: each loop starts again from the stored start.
- Each loop evaluates its source range expression once and snapshots both bounds
  and inclusivity. Reassigning a mutable range binding in the body cannot change
  an active loop. Inline bounds use the same construction and snapshot rules.
  The index is not in scope while constructing or selecting the source range.
- Range type identity depends on the exact integer endpoint type; inclusivity is
  part of the value. Inferred locals, assignment/copies, and branch/block/loop
  results can carry ranges, including mixed inclusive/exclusive branches. No new
  source-level range annotation syntax is provided yet, so range parameters/return
  signatures, fields, union payloads and globals remain unsupported. Arrays can
  infer range elements, for example `[0..3, 1..=2]`; their bounds and inclusivity
  copy by value with exact endpoint typing and the bound inference rules below.
  Range field access, equality, membership and direct printing are not introduced.
- Ordinary range operators bind above assignment and below pipes/other binary
  operators; chaining `a..b..c` is rejected. Existing slice/match syntax is preserved.
- `..` excludes the upper endpoint. `start >= end` is empty; otherwise increment
  by one. `..=` includes the upper endpoint; `start > end` is empty. Both
  forms are finite even at a maximum-integer endpoint: inclusive iteration exits
  after that endpoint without incrementing it, including on body-side `continue`.
- If either bound has a known integer type, use that **exact** type for both
  bounds and the index; no implicit widening or signedness conversion occurs.
  A dynamic signed bound stays signed, even when its runtime value is positive.
- Only when **both** bounds are untyped integer literals (including literal
  brace grouping/negation), use native `uint` if both values are nonnegative,
  including zero; use native `int` if either is negative. Literal decoding checks
  that both fit the selected native type, including negative minima. This is a
  **range-specific default**, distinct from ordinary `int` fallback. Otherwise
  unconstrained numeric-expression bounds use `int`: `1 + 1..3` has `int`
  endpoints. Typed bounds always win first; neither rule involves constant folding.
- The index is immutable and fresh in the body's lexical scope each iteration.
  `_` discards it and creates no binding. Existing no-shadowing rules apply.
- Without `else`, range loops yield `void` and reject value breaks. Continuing
  bodies always yield `void`. Body-side `continue` advances (or exhausts at an
  inclusive endpoint); `break` and `return` skip the increment.
- With `else`, normal exhaustion runs the fallback exactly once, including empty
  and reversed ranges. Source `break` (including at MAX) and `return` skip it;
  inclusive endpoint completion, including `continue` at MAX, does not skip it.
  `break value` and continuing fallback results use the same exact contextual or
  bidirectional inference as conditional-loop results, including range values,
  with integer fallback only after those constraints and no widening.
  Arrays copy independently at break/fallback and result boundaries; immutable
  strings share rooted storage. Terminal fallback paths contribute no merge value.
- Bounds use the enclosing control scope before the new loop starts. The fallback
  is also outside the completed range's control scope: its `break`/`continue`
  targets an outer loop or diagnoses if none exists. The index is visible only in
  the body, never in either bound or the fallback; body locals do not escape.

```nov
let contains: (start: uint, end: uint, target: uint) -> bool = loop i in start..end {
    if i == target {break true}
} else {false}
```

See [`examples/range-search.nov`](examples/range-search.nov) for executable
exclusive/inclusive searches and overflow-free MAX exhaustion.

See [`examples/range-values.nov`](examples/range-values.nov) for stored bounds
iterated twice, first to print 0 through 9 and then to compute a sum of 45.

Ranges are not membership containers or implementations of a general iterator
protocol. Missing bounds, steps/descending iteration, general iterators, multiple
bindings, reference iteration over ranges, labels, user continuation clauses, and
value breaks without `else` are explicitly unsupported. Array/string iteration is
supported separately (see [Loop](#loop)). There is no `for` keyword alias. Header expressions
retain the existing single-newline continuation rules, including after `in` and
`..`/`..=`; the body follows the completed upper-bound expression without a newline.

## Match

The executable subset supports one or more `bool`, integer, `rune`, or string subjects,
scalar literal patterns, `|` alternatives within a positional pattern, and `_`
wildcards. Subjects are evaluated **once, left to right, before selection**.
Prongs are tested in source order and only the first matching body executes.
With multiple subjects, commas separate positional patterns; a single `_`
matches all subjects. A single canonical `bool` subject (including type aliases)
is exhaustive when its patterns cover both `true` and `false`, even without a
final catchall. Alternatives such as `true | false` cover both values; `_` inside
an alternative is universal. Completely covered boolean prongs are rejected,
but partial overlaps retain first-match behavior. Coverage depends on the subject
type, not its runtime or literal value. Integer, rune, string and multi-subject matches
still require a final catchall (`_` or all positional `_`s); general finite-domain
exhaustiveness is not implemented.

```nov
let classify: (n: uint) -> string = match n % 3, n % 5 {
    0, 0 => "FizzBuzz"
    0, _ => "Fizz"
    _, 0 => "Buzz"
    _ => "number"
}
```

Literal patterns use the exact subject type, not an independent default or conversion.
String patterns compare exact UTF-8 bytes, including NUL. Each prong has private
local scope; continuing results have one exact, bidirectionally inferred or
contextual type. Return/break/continue paths do not contribute merge values.
Array/function subjects, enums, captures, range/destructuring/computed patterns,
and general exhaustiveness remain unsupported. Identifier interpolation works in
branch results, including the FizzBuzz fallback `"{n}"`.
The following broader syntax remains a design example, **not executable in full**:

```nov
match 5 {
    0 | 1 | 2 => ... ; use `|` to specify multiple cases
    {0b100 | 0b001} => ... ; wrap your expr in a {} to use `|` bitwise operator
    10..20 => ... ; use `x..y` to match over x to y excluded, [x;y[
    20..=30 => ... ; use `x..=` to match over x to y included, [x;y]
    31 => {} ; {} does nothing, it's a empty block
    _ => {} ; _ corresponds to every other possible values
}
```

## If/Else
```nov
let a = 10
let b = 20
; braces are mandatory, else is optional
if a < b {
    @println("{a} < {b}")
} else if a > b {
    @println("{a} > {b}")
} else {
    @println("{a} == {b}")
}

; all ifs are expressions which means that they all return a value
; the previous if returns `void`
; this one returns a bool
let is_even = if 69 % 2 == 0 {true} else {false}
; another example which returns an Option(int)
let x: Option(int) = if is_even {
    @println("even")
    .some[42]
} else {
    @println("not even")
    .none[]
}
```

## Loop
Condition-based `loop condition { ... }` and unconditional `loop { ... }` support
optional `: continuation` clauses in the parser, semantic checker, C backend and
formatter. Continuing body/continuation paths must yield `void`; unlabeled
`break`/`continue` target the innermost active loop. Conditional loops without `else`
yield `void`; with `else`, condition-false exhaustion (including zero iterations)
yields its result, and `break value` skips `else`. Unconditional loops also support
value breaks, but reject `else`. Results use exact contextual/bidirectional inference.
Unconditional loops without value breaks do not acquire a never type. See the
executable subset above for continuation routing, scopes, copying and result rules.

Two adjacent blocks, `loop { booleanExpression } { body }`, remain a block
condition followed by a body. A newline before the second `{` ends the first
loop rather than continuing it, just as in the existing condition-loop grammar.

```nov
let main: () -> void = {
    let mut remaining: int = 3
    loop {
        if remaining == 0 {break}
        @println(remaining)
        remaining -= 1
    }
}
```

### Array and string iteration

`loop item in items { ... }` evaluates the iterable expression once. For arrays,
`item` is an immutable value copied with the normal element-copy rules; it is not
an alias into the source. Mutation of the source array and new conflicting loans
are statically rejected during iteration. A direct unborrowed local-array source
retains a rooted readonly descriptor instead of deep-copying the whole backing;
yielded values still copy normally when passed, stored or captured. Computed and
borrowed sources keep their existing snapshot evaluation. For strings, `item` is a Unicode scalar
`rune`, not a byte or grapheme. A forward UTF-8 cursor makes the whole traversal
O(n) in the byte length, rather than repeatedly scanning from the start. The
immutable string value is captured once, so rebinding its source local is safe
and does not change the active traversal.

`loop &item in items { ... }` is supported only for an unborrowed local array
owner. The item is a scoped, nonescaping reference represented by a stable owner
and index, not a general pointer. A mutable local array permits element writes;
an immutable local array permits only reads. The whole owner is exclusively
borrowed and unavailable in the body, even for a readonly item. Reference
iteration over strings, ranges, temporaries or references (reborrowing) is not
supported. Ordinary reads of a reference still produce copied values.

Both forms preserve the existing loop control/result rules: normal exhaustion,
including an empty source, runs `else`; `break`/`return` skip it, and `continue`
advances to the next element. With `else`, value breaks and exhaustion results
must match exactly; without `else`, these loops yield `void` and reject value
breaks. The item is scoped to the body, and `else` is outside the completed
loop's control scope. See [`examples/iteration.nov`](examples/iteration.nov).
Multiple/zip bindings, general iterator protocols, array slices and labels remain
unsupported; this does not enable arbitrary library-container iteration.

The condition/unconditional forms, continuation clauses, conditional `else`,
array/string iteration and bounded integer range examples below are executable
within these limits (see also [Range](#range)). The multiple-binding example is
future syntax only.

```nov
; items can be a supported array or a string; type is inferred
; slices and general iterators are not supported
loop item in items {}

; n takes values [0;10[
; note than the boundary doesn't need to be literal
; loop n in x..y {} uses the bounds' exact integer type
loop n in 0..10 {}

; n takes values [0;10]
loop n in 0..=10 {}

; future proposal only: multiple/zip iteration is not implemented
loop item, i in items, 0.. {}

; scoped reference iteration: items must be an unborrowed local array owner
loop &item in items {}

; use underscore to ignore a value
loop _ in 0..10 {}

; we can also loop on a condition
loop x > 10 {}

; infinite loop
loop {}

; continuation loop (executable with explicit integer context)
let mut i: int = 0
loop i < 100 : i += 2 {}

; else branch
; the else branch is evaluated when the loop is not exited with a break
let rangeHasNumber: (begin: uint, end: uint, number: uint) -> bool = {
    let mut i = begin
    loop i < end : i += 1 {
        if i == number {
            break true
        }
    } else {
        false
    }
}
```

## Break & Continue
Unlabeled `break` and `continue` target the innermost loop, including from nested
blocks, branches and short-circuit expressions. Unconditional loops and conditional
loops with `else`, including bounded ranges and array/string iteration, support
`break value`; conditional, range and array/string loops without `else` support
only bare `break`. A loop's `else` is outside that loop's control scope. With a
continuation clause, body-side `continue` runs the continuation; condition-side
or continuation-side `continue` reevaluates the condition directly. `break` and
`return` skip the continuation. Labels remain unsupported.

## Defer
Not implemented; explicitly postponed separately from the optimization pause.
The approved rules in the current local handoff `PLAN2.md` specify
reverse-order block-exit cleanup on normal completion, return, break, continue
and `.!` propagation (not panic), evaluated against current locals at exit.
Results are saved before cleanup; deferred code must yield `void` and cannot
return, break, continue or propagate with `.!`. GC roots and static borrowing
must be preserved. These are future rules, not executable support.

## In
`value in array` returns `bool`: true if any element equals the value, false
otherwise (including an empty array).
```nov
let nums: []int = [1, 2, 3]
@println(1 in nums) ; true
@println(5 in nums) ; false
```

The executable subset supports membership in scalar `[]T` arrays: `bool`, `rune`,
`string`, nominal enums, `f32`/`f64`, fixed 8/16/32/64-bit signed/unsigned integers,
and native `int`/`uint`. Value and
element types must match exactly; there is no widening or signedness conversion.
A known array element type constrains the left literal; a known typed left value
constrains an array literal, including `[]`. Boolean literals provide a known
`bool` type (`true in [false, true]` works). With no stronger operand context,
`1 in [1, 2]` and `1 in []` use `int` elements. The expected `bool` result does
not itself choose an element type.

Operands evaluate once, left then right. Membership projects its RHS for a readonly
scan rather than cloning existing array storage, including array-valued fields and
permitted readonly borrows. Required snapshots inside effectful computed bases are
preserved; constructing the RHS can still allocate. The scan itself is an
allocation-free linear traversal using scalar equality, stopping at the first match. It neither
mutates nor appends. String elements compare exact bytes, including NUL. Strings
as containers are unsupported. Functions, unions, records, arrays and ranges have
no element equality or membership, even though they are storable array elements;
pointers and iterator protocols remain unsupported.

## Assignment
```nov
let mut a = 3 ; @TypeOf(a) == int
let b = 4 ; @TypeOf(b) == int

let x = y = 3 ; parse error
let x = a += b ; parse error
let x = {y = 3} ; should be fine, @TypeOf(x) == void
let z += 3 ; compile error, expected `=`
```

## Scalar Constant Globals

Immutable module bindings infer `bool` from `true`/`false`, `rune` from single-quoted
scalar literals and `string` from plain UTF-8 string literals. Integer literals
and literal negation default to `int`; float literals default to native `float`.
Annotations or `@as` supply context before either fallback:

```nov
let enabled = true
let label = "Count:"
let count = @as(Count, @as(uint, 8))
let Count = uint
let main: () -> void = @println("{label} {count}", enabled)
```

These are owned constants, inlined at use sites, not runtime initialization code.
Forward type aliases work in `@as`; nested ascriptions must have exactly the same
canonical type, including an explicit declaration annotation when present:
`let count: Count = @as(uint, 8)` works, while `let n: uint = @as(int, 1)`
and `@as(int, @as(uint, 1))` are errors, not conversions. Literal
bounds remain exact, including native `int`/`uint`; explicit 128-bit constants
can type-check but retain the C backend's width rejection.

`let count = 8` and `let negative = -8` infer `int`; negation also accepts literal
brace grouping, as in `-{{8}}`. This is still literal-only inference, not compile-time
evaluation: identifier value chains, arithmetic, standalone blocks such as `{8}`,
calls, function values, arrays and void globals remain unsupported, even under `@as`. Existing explicitly annotated literal globals are unchanged.
Interpolated strings still require a function; plain literals support UTF-8,
escaped braces and embedded NUL. Mutable globals remain unsupported. Type
ascriptions inside function bodies retain their existing expression semantics.

See [examples/type-aliases.nov](examples/type-aliases.nov) for globals together
with aliases, array locals and an exhaustive boolean match.

## Type Aliases

Immutable module declarations can name supported types:

```nov
let Count = uint
let Counts = []Count
let Callback = (Count) -> Count
let NextCount = Count

let increment: (value: NextCount) -> Count = value + 1
let main: () -> void = {
    let values: Counts = [7]
    let callback = @as(Callback, increment)
    @println(callback(values[0]), "{@as(Count, 8)}")
}
```

Aliases are canonical synonyms, **not distinct newtypes**: `Count` and `uint`
match exactly, with the same literal bounds, operations and backend restrictions.
Supported RHS forms are `bool`, `void`, fixed-width integers, `f32`/`f64`, native `int`/`uint`/`float`,
`rune`, `string`, nominal container types, supported array types and function types, including aliases in nested
parameter/result/array-element positions. Bounded [`@TypeOf`](#type-queries)
queries are also supported, including `let Count = @TypeOf(global)`.
Function-type parameters may be unnamed.
`let Callback = (int) -> int` declares a type alias, not a function body.
Function definitions still use explicit signature syntax, such as
`let increment: (value: Count) -> Count = value + 1`; a function-type alias can
annotate a function-valued local or parameter, but does not define parameter names
or introduce a new function-definition shorthand.

Forward references and alias chains are supported. Cycles, including cycles
through function or array components, diagnose with source notes independently
of runtime initializer cycles. Expansion uses an iterative work stack bounded to
128 active alias expansions; explicit type-syntax nesting has its own limit of
128. Invalid aliases fail analysis even when unused; an unused valid alias creates
no runtime global, function or initialization code. As with direct type spellings,
128-bit integer types can resolve but executable C emission rejects reachable
128-bit values.

Only plain immutable module declarations `let Name = typeExpression` define
aliases. They share the existing module namespace: duplicates and implicit
shadowing are rejected. An identifier RHS names a type only when its target is a
type (or a type position demands resolution and reports a type error); aliases
of runtime values do not become types. Scalar literal globals infer independently;
identifier-chain global initialization remains unsupported. Unknown identifier RHSs without type context are diagnosed as
unknown names, not assumed to be function types. Identifier-only cycles with no
type context remain value-dependency errors; a type annotation or `@as` demand
classifies the same candidates as attempted aliases and reports type-alias cycles.

Aliases work in annotations and `@as`, and print contexts render their canonical
type names as static strings (`@println(Count)` or `"{Count}"`). They are not
general runtime values; `Count(1)` remains an error. An alias of a nominal struct
supports that struct's explicit `Alias[ .field = value ]` constructor and static methods;
this does not add numeric alias constructors, mutable/local aliases, general runtime
type values or comptime functions. Bounded `@TypeOf` queries and generic container
arguments are supported as described below. `@as` remains exact expected-type ascription, not a checked
numeric cast. See [examples/type-aliases.nov](examples/type-aliases.nov).

## Type Queries

`@TypeOf(x)` returns the existing canonical type of a runtime value binding at
compile time. It takes exactly one operand: a binding identifier or an imported
namespace value reference such as `lib.value`, subject to ordinary visibility
checks. Globals, visible locals, parameters, mutable and borrowed bindings, and
bindings visible from an enclosing closure scope can be inspected. Scalar,
array, nominal container and concrete function/closure types are supported.
A concrete function variable specialized from a generic function is queryable;
the unspecialized template itself is not, even after another call specializes it.

This is a bounded type query, not general expression reflection. Literals,
arithmetic, calls, indexing and runtime field access (`@TypeOf(record.field)`)
are rejected, as are types/type aliases, generic templates and namespaces as
operands. In particular, `@TypeOf(f())` is rejected, **not executed**. Looking up
a binding's type does not evaluate its initializer or read its runtime value,
mutate it, add a runtime initializer dependency, or introduce closure captures
or mutable-capture boxing. Querying a borrowed binding inspects its value type
without a runtime read. Existing scope, borrowing and global-initializer
restrictions remain unchanged.

The result is a compile-time type, not a first-class runtime value. Use it in
annotations (including `[]@TypeOf(x)`), exact ascription
(`@as(@TypeOf(x), value)`), generic type arguments (`Box(@TypeOf(x))`), or an
immutable module-level type alias (`let Count = @TypeOf(global)`). Local type
aliases remain unsupported: `let T = @TypeOf(x)` inside a function is an error.
Direct print/interpolation contexts are special: `@println(@TypeOf(x))` and
`"{@TypeOf(x)}"` emit the canonical type name as a static string, without printing
the value or invoking its `@[stringify]` converter.

```nov
let global = 42
let Count = @TypeOf(global)
let Box: (T: type) -> type = struct { value: T }
let identity: (x: `T) -> @TypeOf(x) = x
let same: (x: u8, y: @TypeOf(x)) -> @TypeOf(y) = y

let main: () = {
    let x: u8 = 7
    let values: []@TypeOf(x) = [x]
    let boxed: Box(@TypeOf(x)) = Box(@TypeOf(x))[.value = x]
    let count: Count = 1
    let copy = @as(@TypeOf(x), 8)
    @println(@TypeOf(x), "{@TypeOf(values)}", identity(count), same(x, copy), boxed)
}
```

Later function parameters and the return type may query earlier named parameters,
including in function types. In inferred generic signatures such as
``(x: `T) -> @TypeOf(x)``, the query shares the parameter's inferred type
constraints; contextual types still take precedence over integer fallback.
Broader reflection (`@typeInfo`, member enumeration and arbitrary expression
queries) remains a proposal. See [examples/type-of.nov](examples/type-of.nov).

## Primitive Types
TODO
- bool
- string
- rune (distinct Unicode scalar type; see [Runes](#runes))

See also:
- [Integers](#Integers)
- [Floats](#Floats)
- [Arrays](#Arrays)

## Runes

`rune` is a distinct Unicode scalar type: `0..=0x10FFFF`, excluding surrogate
values `0xD800..=0xDFFF`. It is neither an integer nor a grapheme (visible character).
Single quotes contain exactly one scalar or escape: `'a'`, `'é'`, `'😀'`, `'\n'`,
`'\0'`, `'\''`, `'\\'`, `'\xE9'` and `'\u{1F600}'`. Rune `\xNN` denotes
that scalar value (`'\xE9' == 'é'`), not a standalone UTF-8 byte. Empty/multiple-scalar
literals, malformed UTF-8, invalid escapes and surrogate/out-of-range scalars reject.

Runes support inference, exact `@as(rune, value)` ascription, aliases, literal
globals, parameters/returns, `==`/`!=`, arrays, fields, union payloads, literal
matches (with a catchall) and scoped whole-value/array-element borrows.
`@print`/`@println` and interpolation encode runes as UTF-8, including NUL:

```nov
let main: () = {
    let accent = 'é'
    let emoji = '\u{1F600}'
    let letters: []rune = [accent, emoji]
    @println(letters[0], "Runes: {accent}{emoji}")
}
```

Bind rune literals to variables before interpolating: quoted literals inside
placeholders remain unsupported. No numeric conversions, arithmetic, ordering,
rune ranges or foreign ABI support are provided. See
[`examples/runes.nov`](examples/runes.nov).

## Integers

The C backend supports signed and unsigned 8/16/32/64-bit integers. Addition,
subtraction, multiplication and signed negation wrap in two's complement, equally
in debug and release. Division truncates toward zero; remainder has the dividend's
sign. `MIN / -1` wraps to `MIN`, and `MIN % -1` is zero. Division or remainder by
zero flushes stdout, writes `panic: division by zero` or `panic: remainder by zero`
(with a newline) to stderr, and exits with status 1.

Integer bitwise `&`, `|`, `^` and unary `~` are executable for signed/unsigned
8/16/32/64-bit integers and native `int`/`uint`. Binary operands must have exactly
the same integer type; the result keeps that type and width, using two's-complement
bit patterns without overflow panics. A known operand or expected result type can
constrain literals, including through block, match and loop results. Literal bounds
are checked; unconstrained integer operands fall back to `int` only after context
and sibling inference. There is no widening or implicit signedness conversion.
Booleans are not integers: use `and`/`or` for boolean operations. Binary operands
execute once, left to right; terminal paths skip remaining operands. Match-pattern
`0 | 1` remains a literal alternative, not a computed bitwise pattern. Computed
patterns, shifts and bitwise compound assignments (`&=`, `|=`, `^=`) remain
unsupported. Precedence is unchanged: `~` binds tighter than the shared,
left-associative `& ^ |` level, which binds tighter than comparisons.

C arithmetic and binary bitwise operations use `uint32_t`/`uint64_t` working values to avoid signed overflow
and narrow integer promotions. Conversion back to signed integers relies on the
modular unsigned-to-signed conversion provided by GCC, Clang and MSVC. Shifts,
128-bit and odd-width backend integers remain unsupported. `int` and `uint` are
signed and unsigned aliases of the native target's pointer-sized fixed-width
integers: `i64`/`u64` on 64-bit targets and `i32`/`u32` on 32-bit targets. They have
identical type identity, literal bounds, wrapping arithmetic and printing behavior
to those fixed-width types, including in nested function signatures. They are not
C `int`/`unsigned int`.

If no type is inferred for an integer literal, it defaults to `int` only after
annotations, call signatures, typed sibling operands/array elements/control
branches, and generic expected-result/body/argument constraints have been tried.
This covers bare locals/globals, negation, arithmetic, arrays, printing and
interpolation: `let n = 1`, `let a = [1, 2]` and `@println(1, "{2}")` work.
Empty arrays still need an element type. There is no implicit numeric widening,
signedness conversion or size-based fallback: a literal above native `int`'s
maximum fails out of range unless given a fitting explicit type such as `u64` or
`i128` (128-bit types remain semantic-only for the C backend). The dedicated direct
range-literal rule is described in [Range](#range). Integer literals may instead
be checked directly in a float context; typed integer values are never implicitly
converted to floats. Float literals use their own late `float` fallback below.

C-specific integer aliases remain deferred.

## Floats

`f32` (IEEE binary32) and `f64` (IEEE binary64) are executable types. `float`
is a canonical alias of `f32` on 32-bit targets and `f64` on 64-bit targets, not
C `float` on every target. These are accepted, implemented rules, not a proposal.

Float literals default to native `float` only after annotations, signatures,
typed siblings/array elements/control branches and generic constraints have been
tried. Integer literals are also allowed directly in float contexts. Already typed
integer, `f32` and `f64` values require exact type matches: no implicit conversions,
widening or narrowing. `@as(f32, 1)` supplies literal context;
`@as(f32, typed_f64)` is not a conversion. No numeric cast feature exists yet.

Decimal and hexadecimal float literals (for example `1.5`, `1e-3`, `0x1.8p1`)
round directly to the selected target width. Finite literal overflow diagnoses;
subnormals and underflow, including signed zero, are supported. Arrays, struct
fields, union payloads, function parameters/results, closures, generic
specializations and bounded `@TypeOf` queries all support `f32`/`f64`. Type queries
report canonical fixed-width names, including for the native alias.

`+ - * /`, unary negation, `+= -= *= /=`, `== !=` and ordering use IEEE semantics.
Compounds work on mutable locals, supported fields and array elements. Division
by zero produces infinity or NaN rather than an integer-style panic. NaN compares
unequal to every value, including itself, and all ordered comparisons with NaN are
false. Positive and negative zero compare equal. Array membership uses the same
equality, so NaN is never found and either zero matches the other. `%`/`%=`, bitwise
operations, ranges and indices remain integer-only; float match subjects are deferred.

Printing, interpolation and nested default formatting use round-trip-safe `%.9g`
for `f32` and `%.17g` for `f64`, with normalized `nan`, `inf`, `-inf` and `-0`.
This is **not shortest-decimal formatting**. Existing `@[stringify]` hooks and
recursive copy rules are unchanged; float leaves add no GC roots.

The C backend maps `f32` to C `float` and `f64` to C `double`. Generated C guards
IEEE widths, binary radix, subnormal support and `FLT_EVAL_METHOD == 0`, rejects
fast-math, and disables FP contraction. Embedding code must preserve the C numeric
locale and normal round-to-nearest rounding environment. The scalar `@[extern]` /
`@[export]` ABI includes both widths. See `examples/floats.nov`.

`f16`, `f80`, `f128` and `c_longdouble` remain deferred.

## Function Pipes

`value |> function` is exactly unary application `function(value)`, including
**callee-first** evaluation: evaluate the RHS callee before the LHS argument,
not a new left-to-right pipeline effect order. Chaining is left-associative.
Named functions and function values require exactly one parameter; its exact type
constrains literals (`10 |> fibo`). `value |> @print` and `value |> @println`
accept printable values (including floats and aggregates) and return void,
including unconstrained numeric expressions via the normal `int`/`float` fallbacks. No partial application or implicit conversions occur.

The unchanged [`examples/fibo.nov`](examples/fibo.nov) now runs:

```sh
nov run examples/fibo.nov          # 55
nov run examples/fibo.nov --debug  # 55
nov run examples/fibo.nov --release # 55
```

## Operators
<!-- Yes this is not really lisible as raw text -->
| Name                  | Syntax             | Types                                        | Remarks                                                             |
|-----------------------|--------------------|----------------------------------------------|---------------------------------------------------------------------|
| Assignment            | a = b              | All types                                    | `a` is an identifier and `b` is an expression.                      |
| Addition              | a + b <br> a += b  | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Concatenation         | a + b <br> a += b  | string <br> [Arrays](#Arrays)                | Strings: `+`/`+=`; arrays: `+=` only                          |
| Substraction          | a - b <br> a -= b  | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Negation              | -a                 | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Multiplication        | a * b <br> a \*= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Division              | a / b <br> a /= b  | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Remainder Division    | a % b <br> a %= b  | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Left        | a << b             | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Right       | a >> b             | [Integers](#Integers)                        | TODO                                                                |
| Bitwise And           | a & b              | [Integers](#Integers)                        | Executable; exact matching integer types                                                                |
| Bitwise Or            | a \| b             | [Integers](#Integers)                        | Executable; exact matching integer types                                                                |
| Bitwise Xor           | a ^ b              | [Integers](#Integers)                        | Executable; exact matching integer types                                                                |
| Bitwise Not           | ~a                 | [Integers](#Integers)                        | Executable; same-width integer complement                                                                |
| Logical And           | a **and** b        | bool                                         | TODO                                                                |
| Logical Or            | a **or** b         | bool                                         | TODO                                                                |
| Boolean Not           | !a                 | bool                                         | TODO                                                                |
| Equality              | a == b             | All types                                    | TODO                                                                |
| Inequality            | a != b             | All types                                    | TODO                                                                |
| Greater Than          | a > b              | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Greater or Equal      | a >= b             | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Less Than             | a < b              | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Less or Equal         | a <= b             | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Bind                  | a >>= \|b\| ...    | Monads                                       | TODO                                                                |
| Function Pipe         | a \|> f            | [Functions](#Functions)                      | TODO                                                                |
| Member Search         | a **in** b         | [Arrays](#Arrays)                            | TODO                                                                |
| Access                | a\[b]              | [Arrays](#Arrays)                            | Array elements and immutable string scalars supported          |
| Field / Method Access | a.b                | All types                                    | TODO                                                                |
| Reference Type        | \*T <br> \*mut T   | All types                                    | Create a reference type from `T`. Unless `mut` is specified the wrapped value is constant |
| Reference Of          | &a                 | All types                                    | Returns a reference to `a`.                                         |
| Dereference           | a.*                | Reference                                    | Unwrap a reference type, this is done automatically when using `.` or `[]`. |
| Unwrap or Rethrow     | a.!                | Result <br> Option                           | Unwrap a value or return if it's wrong (`.err`, `.none`).           |

## Precedence
```
x() x[] x.y x.! x.*
!x -x ~x &x *T
* / %
+ -
<< >>
& ^ | in
== != < > <= >=
and
or
|> >>=
= *= /= %= += -=
```

## Operator Overloading

This is a historical proposal, not implemented operator-overloading behavior.
The proposed operators and automatic derivations below are not delivery claims;
the executable equality rules are described in their type-specific sections.
Printing customization is implemented separately through `@[stringify]`, not a
magic method name. In this proposal, overloading would cover these operators:
- `+`: (T, T) -> T
- `-`: (T, T) -> T
- `*`: (T, T) -> T
- `/`: (T, T) -> T
- `%`: (T, T) -> T
- `<`: (T, T) -> bool
- `==`: (T, T) -> bool

Note:
- `==` is automatically generated for all types by the compiler but can be overridden.
- `!=`, `>`, `<=`, `>=` are automatically generated when `==` and `<` are defined.
- `+=`, `-=`, `*=`, `/=`, `%=` are automatically generated when the corresponding operator is defined.

```nov
let Complex = struct {
    re: float
    im: float

    @[operator(.@"+")]
    let add: (self: Complex, other: Complex) -> Complex = [
        .re = self.re + other.re,
        .im = self.im + other.im,
    ]

    ; explicit print converter; the name is arbitrary
    ; exact signature must be (T) -> string where T is the container type
    @[stringify]
    let format: (self: Complex) -> string = "{self.re} + i{self.im}"
}

let x: Complex = [ .re = 5, .im = 3 ]
let y: Complex = [ .re = 2, .im = 7 ]
x + y ; returns Complex[ .re = 7, .im = 10 ]
```

## Strings

`string` is immutable, valid UTF-8 on every platform, including Windows. Assignment,
initialization, parameters and returns share immutable storage; `let mut` allows
replacing the binding, never changing bytes. `+` concatenates strings, evaluating
operands once, left to right. For a mutable local, `s += rhs` captures the old string
before evaluating `rhs`, concatenates, then replaces `s`. RHS effects may replace
`s`; a return, break or continue skips the final concatenation and store without
undoing those effects. Shared originals remain unchanged, including for `s += s`.
Concatenation checks length overflow and allocates GC-managed backing; allocation
failure panics.

`==` and `!=` compare exact UTF-8 bytes and byte length, including embedded NUL,
not storage addresses. Both operands must be strings; the result is `bool`.
Operands evaluate once, left to right, retaining the left value even if the RHS
replaces its binding or allocates. Comparison itself does not allocate. Equality
is case-sensitive and performs no Unicode normalization: `"é" == "e\u{301}"`
is false, while `"é" == "\u{e9}"` is true. A return, break or continue in an
operand skips the remaining comparison without undoing earlier effects.

Ordering (`<`, `<=`, `>`, `>=`), other operators (including `-=`, `*=`, `/=` and
`%=`) and grapheme operations remain deferred. Scalar string iteration is
supported (see [Loop](#loop)).
`[]string` and supported record arrays work with independent element storage and
shared immutable string backing; nested arrays recursively copy their array backing.

`text[index]` decodes one Unicode scalar as **exactly `rune`**, using a native signed
`int` index. Indices count scalars, not bytes or graphemes; `-1` is the last scalar. Out-of-bounds access,
including indexing an empty string, panics with `string index out of bounds` in
all build modes. No implicit integer widening or index signedness conversion occurs.
The string and index evaluate once, left to right; the string value is captured
before index effects can replace its binding or trigger GC. Indexing scans UTF-8
in O(n) time and allocates nothing; internal storage remains UTF-8 bytes.

```nov
let main: () = {
    let text = "é😀\0"
    let first: rune = text[0] ; 'é'
    let last: rune = text[-1] ; '\0'
    @assert(text[1] == '😀')
    @assert(text.len == 3)
    @println("First: {first}", last)
}
```

Embedded NUL is an ordinary scalar; continuation bytes are not exposed by indexing.
Indexed assignment, compound assignment and string-element
borrows are unsupported, even for `let mut` bindings. Whole-string aliases retain
the existing borrowing rules. Chained reads such as `strings[0][1]` work for arrays
of strings; nested arrays separately support chained value reads, not direct
chained indexed writes. See
[`examples/string-bytes.nov`](examples/string-bytes.nov), which keeps its historical
filename but now demonstrates Unicode scalar indexing and length.

String slicing supports `text[start..end]`, `text[..end]`, `text[start..]` and
`text[..]`, using either half-open `..` or inclusive `..=`. Bounds are native `int`
Unicode scalar offsets; negatives count from the scalar end. An explicit inclusive
end must name an existing scalar, and is converted to an exclusive boundary by
adding one **after** normalization. For example, `"é😀x"[0..=1] == "é😀"` and
`text[..=-1]` includes the last scalar. An explicit inclusive end equal to `.len`
is out of bounds, even for an empty slice.

Omitted bounds default to 0 and the scalar length for either operator:
`text[1..=] == text[1..]`, `text[..=] == text[..]`. After conversion,
`0 <= start <= exclusive_end <= len` must hold; invalid bounds panic in every build
mode without clamping. Adjacent reversed inclusive bounds can describe an empty
slice: `"abc"[2..=1] == ""`; further-reversed bounds panic. `""[..=]` is valid,
but `""[..=0]` panics because no scalar exists at index 0.
The base, start and end evaluate once, left to right. Slicing returns an immutable,
allocation-free UTF-8 view sharing the original GC owner, without copying bytes;
even a small view retains the full allocation. `.len` still counts scalars.
For example, `"é😀x"[1..-1] == "😀"`. Array slicing remains unsupported. See
[`examples/string-slices.nov`](examples/string-slices.nov).

For editing, keep the distinction between a mutable binding and mutable storage:
`s += "more"` produces a new immutable string and rebinds `s`, preserving older
copies. Agreed future APIs will provide independent string-to-`[]u8` copies and
validated, recoverable-error byte-buffer-to-string conversion. A separate stdlib
builder may preserve UTF-8 while appending/editing and produce independent immutable
snapshots. These conversions and builder are **not implemented**; there is no second
built-in mutable-string type. Rune decoding through indexing is implemented;
forward scalar iteration is also implemented (see [Loop](#loop)); grapheme
operations remain deferred.

`.len` is the Unicode scalar count as native `uint`, **not byte length or a grapheme
count**. It scans UTF-8 in O(n) time without allocations. `"é".len` and `"😀".len`
are both 1; `"e\u{301}".len` is 2 and `"".len` is 0. Literals use the existing escapes (`\n`, `\r`,
`\t`, `\\`, `\"`, `\'`, `\0`, `\xNN`, `\u{...}`, `\{`, `\}`). Decoded bytes
must be valid UTF-8; NUL is valid, including through `\x00` or `\u{0}`. Invalid
escapes/UTF-8 report source locations. Unescaped `{expression}` interpolates a
scalar value, including function calls; use `\{` and `\}` for literal braces.

`@print`/`@println` write exact UTF-8 bytes with explicit lengths, including embedded
NUL, with the same spaces, buffering, checked writes/final flush and panic behavior
as scalars. Windows stdout is switched to binary mode on the first print to avoid
newline translation; this is UTF-8 byte output, not UTF-16 console conversion.
Literal-only immutable globals with explicit `string` annotations are supported
without eager initialization calls; inferred plain string globals also work. Mutable
globals and global concatenation or calls remain unsupported. Global interpolation is also rejected as nonliteral;
move it into a function.

```nov
let prefix: string = "Hello"
let identity: (text: string) -> string = text
let main: () -> void = {
    let mut text = prefix + ", 世界!"
    let shared = identity(text)
    text += " Again!"
    @println(shared, shared.len, text)
}
```

See [`examples/strings.nov`](examples/strings.nov), which also demonstrates escaped
braces, embedded-NUL output and byte-exact equality across independent allocations.
String formatting preserves literal tokens and escapes.
Literals use static read-only bytes with a null GC owner; concatenation results
have an exact scalar GC backing descriptor. Only managed owner fields are traced,
never static byte pointers. Temporaries use the same precise root frames as arrays.

## String Interpolation

`"{expression}"` evaluates an expression in the current lexical scope and produces
an immutable `string`. Placeholders support `bool` (`true`/`false`), signed and
unsigned fixed-width integers (`i8`/`u8` through `i64`/`u64`), native `int`/`uint`
(exact decimal, including minimum/maximum values), runes (UTF-8 encoded scalars),
and strings (unchanged bytes).
Arrays, nominal enums, structs and concrete generic or non-generic unions use the
same recursive default representations as print: `[a, b]`, `Entry[ field: value, ... ]`
(or `Empty[]`), `Message.tag[payload]` or payloadless `Message.tag`, and enum
`Color.tag`. Default record union payloads flatten to `Message.entry[ field: value, ... ]`.
Strings/runes remain raw, not quoted or escaped. Nominal values at every depth
prefer an explicit container method marked `@[stringify]` with exact signature
`(SelfType) -> string`, invoked once in traversal order. A hook overrides the value
and stops recursion; a record
payload hook produces `Message.entry[ <custom output> ]` rather than flattened
fields. Malformed/inaccessible hooks reject even when nested, and only a missing
hook permits structural fallback. Functions/closures, ranges and void values
(other than payloadless union alternatives) have no default representation;
aggregates containing them require a valid enclosing hook that stops recursion.
Each placeholder expression evaluates once, then converts once immediately before
proceeding to the next chunk or placeholder. Aliases preserve exact hook signatures;
allocating converters and managed payloads stay rooted. No global overload lookup
is introduced. Terminal expressions skip conversion and later placeholder effects.
Ordinary integer fallback applies after contextual inference; no widening is introduced.
Compile-time types such as `{int}`
and `{MyType}` emit canonical, unqualified static name strings, not runtime type
values. Inline `[]int` is outside the existing interpolation parser grammar; use a
type alias instead.

```nov
let describe: (n: int, ready: bool) -> string = "n={n}, ready={ready}"
```

Placeholders use the normal expression parser and checker: identifiers, calls
(including nested calls), scalar operators, array access and `.len` work with the
usual type contexts. For example, `"{describe(1, 5, 5)}\n{describe(5, 1, 3)}"`
uses the callee's parameter types for the numeric arguments. Existing lexical
scope, no-shadowing and declaration-before-use rules apply; spaces are allowed.
Bare integer placeholders use `int` fallback (`"{1}"` works); float placeholders
use late native `float` fallback and the [float formatting contract](#floats).

This bounded syntax excludes blocks/braces, control flow, comments, quotes and
backslashes inside placeholders. String/character literals and quoted names
inside calls are therefore unsupported, even with source escapes: bind them to
locals first. Captures and format options such as `"{n:02}"` remain unsupported.
The entire placeholder must parse as one expression; trailing tokens and
malformed/unescaped braces diagnose rather than silently printing text. Escaped
braces and braces produced by `\xNN` or `\u{...}` remain literal, and inserted
strings are never reinterpreted.

Chunks and placeholders evaluate once, left to right. UTF-8 bytes and embedded NUL
are preserved, with no Unicode normalization. String placeholders share storage;
integer conversions and concatenations allocate rooted GC backing. Aggregate
conversion is one-pass rooted string accumulation, invoking nested hooks exactly
once rather than measuring and replaying traversal. It can allocate multiple
strings; outer chunk concatenations retain their existing costs. Direct default
aggregate printing streams after ordinary evaluation/copies without building a
formatting string, but nested hooks can allocate and have user-code effects.
Concatenation checks length overflow; allocation failure panics. This minimal implementation uses
successive concatenations (potentially quadratic copied bytes for many chunks),
not an optimized string builder. Interpolation works independently of printing,
including equality, `.len`, branch results and returns; globals still require
literal initializers, not interpolation or eager evaluation.

The formatter preserves the single literal token, including its chunks, escapes
and placeholders. See [`examples/interpolation.nov`](examples/interpolation.nov)
for a runnable FizzBuzz match with a conditional loop, and
[`examples/fizzbuzz.nov`](examples/fizzbuzz.nov) for the bounded integer range-loop
version. General iterator syntax remains deferred.

## Concurrency
TODO

## Builtins
### Functions
- `@import(path: string)`: Import a nov file. See [Visibility](#Visibility) for which declarations gets imported.
- `@TypeOf(x)`: Implemented bounded compile-time query of a runtime binding's canonical type; see [Type Queries](#type-queries). Not arbitrary expressions or runtime type values.
- `@typeInfo(...)`: Proposed type-information reflection; not implemented.
- `@This()`: Returns the type of the current container
- `@print(...)`: Output args separated with a space to stdout; returns void. Supports bool, fixed/native integer, f32/f64, raw rune/string and recursive default array/struct/union/enum formatting. Nominal values at any depth prefer an explicit `@[stringify]` method with exact signature `(SelfType) -> string`, called once per encountered value in traversal order and stopping structural recursion. Top-level hooks run immediately after argument evaluation; nested hooks run during traversal. Malformed/inaccessible hooks reject even when nested. Compile-time types emit canonical static name strings. Interpolated strings work like other strings. See the printing contract above.
- `@println(...)`: Same as print with a newline at the end, including with no args.
- `@fprint(file: File, ...)`: Same as print but output to a specific file. Returns a Result<>.
<!-- - `@eprint(...)`: Same as print but output to stderr. -->
<!-- - `@printf(fmt: #string, args: ...)` works like std.fmt.format and print to stdout -->
- `@panic(s: string)`: A terminal, void-returning builtin following the `@[noreturn]` contract. Evaluate exactly one string argument once, attempt to flush stdout, write `panic: <message>\n` to stderr, then exit with code 1. Messages are length-based UTF-8 bytes, including embedded NUL. No backtrace is produced; debug and release behave identically.
- `@max(a: T, b: T, ...)`: Returns the maximum value between all the supplied arguments
- `@min(a: T, b: T, ...)`: Returns the minimum value between all the supplied arguments
- `@dump(expr)`: TODO: https://docs.vlang.io/builtin-functions.html#dumping-expressions-at-runtime
- `@embedFile()`: TODO, also allow for compressing the file
- `@sizeOf()`: TODO
- `@bitSizeOf()`: TODO
<!-- - `@as(T: type, arg: any)`: Cast arg to T if possible. -->
<!-- - `@bitCast()`: Same as zig -->
- `@swap(a: any, b: @Typeof(a))`
- `@unreachable()`: TODO
- atomic stuff
- `@memcpy()` or `@dupe()`/`@clone()`?
- math func? prob no, put them in std.math
<!-- - `@fieldParentRef()` similar to `@fieldParentPtr()` from zig, useful for C-style OOP -->
- `@call()`: TODO
- `@volatile_load()`: TODO
- `@volatile_store()`: TODO

### Variables
- os type
- architecture
- support i128...
- optimization level
- safety check enabled
- current file name
- current func name
- `@line`: current line

## Attributes
- `@[deprecated]` - `@[deprecated("message...")]`
- `@[operator(op)]`: Note that op is an enum for easier parsing.
- `@[pure]`: Mainly for extern functions, a [pure function](https://en.wikipedia.org/wiki/Functional_programming#Pure_functions)
             can only call pure functions but impure functions can call a pure function without issue.
- `@[extern]` / `@[extern("symbol_name")]`: declare a bodyless external C function.
- `@[packed]`
- `@[export]` / `@[export("symbol_name")]`: expose an ordinary function through the C ABI.
- `@[inline]`
- `@[noinline]`
- `@[cold]`
- `@[noreturn]`: Specify that a function can't return.
- `@[test]`: test-only top-level `() -> void` function, selected by `nov test`.
<!-- - `@[callconv]` -->
- `@[public]`

- `@[memoize]`: (?, related to pure...)
- `@[require(condition..., "optional error message")`: Ensure that the specified conditions are verified for the arguments given to a function or to a container initializer.
<!-- - `@[ensure(condition..., "optional error message")`: Ensure that the specified conditions are verified for the values returned by a function. -->

The executable subset accepts one argument-free `@[public]` on supported immutable
declarations, including types, functions, constants and re-exports. Declarations and
methods are file-private by default, with identical rules for file and named imports.
`@[private]` is unsupported; omit the attribute instead. Argument-free `@[noreturn]`
on ordinary functions and methods may be combined with `@[public]` where supported.
Duplicate attributes, arguments, invalid placements and unknown/unsupported
attributes diagnose. The `extern` and `export` forms described below are supported
and do not imply `@[public]`. `@[test]` is supported under the separate
[test-only declaration rules](#testing-nov-code). Other entries in this list remain
design proposals.

### C function exports

```nov
@[export]
let add: (a: i32, b: i32) -> i32 = a + b

@[export("api_invert")]
let invert: (value: bool) -> bool = !value
```

The C symbols are exactly `add` and `api_invert`, with no Nov prefix, suffix or
module mangling. The optional argument must be one ordinary string literal;
escapes are decoded, but interpolation and computed names are unsupported. Export
names must be nonreserved ASCII C11 identifiers. C keywords, C11 library names
(including functions such as `sin` even when their header is not included),
`main`, leading underscores, backend namespaces and generated local names (`pN`,
`vN`, `nov_frame`, `nov_roots`, `nov_target`) are rejected. Duplicate symbols across
loaded files diagnose with a note to the first declaration. Exact collisions with
other generated function names are rejected before any C is returned.

Exports are limited to immutable, ordinary top-level function declarations with
bodies. Parameters/results may be `bool`, `f32`, `f64` or 8/16/32/64-bit signed/unsigned integers;
results may also be `void`. Canonical aliases are accepted, including pointer-sized
`int`/`uint`. The C types are `bool` (include `<stdbool.h>`) and the corresponding
`intN_t`/`uintN_t` (include `<stdint.h>`); native integers match `intptr_t`/`uintptr_t`
on the native target. `f32` and `f64` map to C `float` and `double` respectively;
`float` follows its target-width alias. Calls use the C compiler's ordinary C calling convention.
Runes, strings, arrays, ranges, enums, records/unions, function values, wider integers,
methods and generic exports are not supported as foreign ABI declarations.

`@[public]` controls **Nov import visibility**; export controls the **foreign symbol**.
Neither implies the other. Use both when both APIs are intended. `@[noreturn]` may
combine with export for a void-returning function. Unreferenced exports are retained
in emitted C, and internal calls/function values refer to the exact exported symbol.
Other generated mangled symbols are implementation details, not a stable foreign API.

[`examples/exports.nov`](examples/exports.nov) and its
[C caller](examples/exports.c) demonstrate scalar exports and a private Nov struct
behind scalar wrappers. Build/run commands are in the Nov file's opening comments.
The separate-translation-unit example is tested at `-O0` and `-O3`.

Foreign calls use the `extern` annotation below. Pointers, callbacks, generated
headers, shared-library packaging and DLL export-list support remain deferred. Managed values cannot cross the ABI;
the existing runtime remains single-threaded. Automatic library teardown and
concurrent foreign entry are not provided. Panics remain process-terminating;
exports do not turn them into recoverable C errors.

### Calling external C functions

```nov
@[extern]
let abs: (value: i32) -> i32

@[extern("c_max_i32")]
let max: (left: i32, right: i32) -> i32

let main: () = @println(abs(-42), max(7, 42))
```

`@[extern]` uses the declaration's exact name; its optional single ordinary string
literal selects an explicit C name. Escapes are decoded; computed/interpolated
names are unsupported. Extern declarations must be immutable, ordinary top-level
function prototypes **without a body**. They cannot also carry `@[export]`.

The initial ABI is the same scalar subset as exports: bool and 8/16/32/64-bit
signed/unsigned integers, pointer-sized int/uint, `f32`/`f64` (C `float`/`double`),
and void results. The native `float` alias follows the target width. No variadics,
C-specific integer aliases, strings, pointers, aggregates or function-valued ABI
parameters/results are supported. Extern functions can still be called indirectly
or passed as ordinary function values inside Nov. Standard C function names such
as `abs` and `putchar` are accepted; C keywords, emitted-header macros/types and
backend-reserved names are rejected.

**These declarations are trusted FFI contracts.** Match the real C signature,
calling convention and target exactly: Nov `int` is pointer-sized, not C `int`.
On the supported native platforms the example's `i32` matches C `int`. Nov checks
its own declarations, not arbitrary linked C implementations. C undefined behavior
and external side effects are not made safe by Nov's arithmetic or memory rules.
A matching `@[noreturn]` extern declaration is allowed for a void result, but its
C implementation must actually never return.

Repeated declarations of one C symbol must have the same canonical signature and
`noreturn` annotation, including across imports and when matching a Nov export.
Extern does not grant Nov import access: add `@[public]` when another Nov file
should call it. An extern prototype cannot serve as the program's root `main`.

[`examples/extern.nov`](examples/extern.nov) calls libc plus the custom provider
in [`examples/extern.c`](examples/extern.c); compile/run instructions are comments
in the Nov file. Emit program C and link the provider with a C compiler. Missing
referenced symbols fail at C link time; an unused prototype needs no provider.
`nov build`/`nov run` can call functions already provided by the normal C link, but
do not yet accept extra C objects or library flags. Library mode also accepts
extern-only modules and emits declarations without a process main.

### Non-returning functions

`@[noreturn]` is a function annotation, **not a type**. Annotated functions must
have return type `void`, either explicit or omitted. They cannot contain an
explicit `return` or complete normally. Panic, direct annotated calls, and
unconditional loops without an owned exit can satisfy this guarantee.

```nov
@[noreturn]
let fail: (message: string) -> void = {
    @panic(message)
}

let require: (valid: bool) -> uint = {
    if valid { @as(uint, 1) } else { fail("invalid input") }
}
```

A direct call to an annotated function is terminal: it contributes no continuing
value to an `if`/`match` merge, skips later operand/argument effects, and makes
following statements unreachable. Terminal loop conditions are tracked too;
loop-local `break` and `continue` retain their ordinary control targets.

The guarantee belongs to the declaration, not the function type. Ordinary function
values keep their `(...) -> void` signature and erase the annotation; indirect
calls are conservatively treated as possibly completing. Annotated C functions
use `_Noreturn`; function-pointer typedefs remain ordinary void-returning types.

Attributes can be set only on top level / container declarations

## Visibility
Declarations and methods are **file-private by default**:

- No attribute: accessible only within the defining file, not merely its container.
- `@[public]`: accessible from other files that can access the declaration or type.

These rules are identical for file imports (`@import("result.nov")`) and named
imports (`@import("std")`). There is no `@[private]` attribute.

Aliases and re-exports do not bypass access checks. Public wrappers can call private
helpers in their defining file, and a public type alias can intentionally expose a
private local type without exposing its unmarked methods. Returning or passing a
value does not grant access to its private methods. Checks cover static and receiver
calls, function-value extraction, printing and interpolation (an explicit
`@[stringify]` converter must also be `@[public]` for cross-file use; a private hook
prevents structural fallback). Fields and union variants do not support visibility
attributes.

Migration: add `@[public]` to declarations and methods used across files; remove any
`@[private]` attributes. A public type does not automatically make its methods public.

## Comptime

**Proposal only—not implemented.** `#` denotes explicit compile-time arguments;
it is not the notation for inferred generic type variables (which use backticks).
The exact argument spelling and compile-time evaluation rules remain unsettled.
The broader Zig-inspired expression/block/variable/assertion forms below are also
historical proposals, not executable features or newly approved syntax.

```nov
; PROPOSAL ONLY: compile-time expression
let x = #fibonacci(10)

; comptime block
let x = #{}

; comptime variable
let mut x: #int = 0

; comptime parameter
let intList: (len: #uint) -> []int = ...

; comptime assertion
let assert_name = #{
    let cond = 1 == 1
    if !cond {
        @unreachable()
    }
}
```

# Inspirations
[Zig](https://ziglang.org/),
[OCaml](https://ocaml.org/),
[Rust](https://www.rust-lang.org/),
C,
[V](https://vlang.io/)
