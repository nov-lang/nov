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
runtime is emitted only for managed values; scalar-only programs stay unchanged.
See the [runtime API and root discipline](src/runtime/README.md) for layout, C
boundary, OOM, testing, and unsupported-reference contracts. Slices, nested arrays,
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
explicit `bool`, `void`, fixed-width integer, native `int`/`uint` and function
types, and checks integer and boolean literals against annotations. Bounded function-body checking binds
parameters and checks integer arithmetic, boolean expressions, comparisons and
calls against collected signatures, including recursive calls. Immutable and mutable
locals (explicitly typed or inferred from an already typed initializer) and blocks
with final-expression results are supported in function bodies. Immutable module-level
[type aliases](#type-aliases) name existing canonical types. General literal defaults,
shadowing and global mutation remain deferred or unsupported. Bounded range-loop
literal bounds have a specific native `uint`/`int` default (see [Range](#range));
this is not a general integer-literal default.
Lowering produces typed operations with source spans, ordered operands/call
arguments, explicit returns, and structured regions for boolean `and`/`or`,
`if`/`else`, scalar `match`, and condition-based or unconditional loops with optional continuation
clauses, value-carrying breaks, and conditional/range-loop exhaustion (`else`) results.
It requires immutable literal globals; unit/global block initializers are
not lowered yet. An internal C11 backend emits verified IR, including fixed-width
integer arithmetic and program entrypoint emission through `emit-c`.
`@print`/`@println` support booleans, strings and explicitly typed integers,
including `int`/`uint`, and [nominal value structs](#executable-nominal-value-structs)
with an exact container `toString` method. Plain `check` remains syntax-only.

Use `zig build run -- check --semantic file.nov` for the current bounded
semantic/lowering subset. It runs declaration collection, initializer dependency
analysis, annotation/literal and function-body checking, then typed lowering and
IR verification. Source errors and unsupported/deferred forms fail with source
diagnostics (including related notes where available); recovered syntax errors
stop semantic analysis. Runtime globals must be immutable scalar constants: explicitly
typed integer/boolean/string literals, inferred boolean or plain UTF-8 string
literals, or scalar literals explicitly contextualized with `@as(T, literal)`.
Immutable module-level type aliases are also supported. Functions need explicit supported signatures and bounded
expression bodies (including forward/recursive calls, local declarations and
assignments, nested blocks, boolean short-circuiting, `if`/`else`, explicit returns,
bounded integer range loops, condition-based or unconditional loops with continuation clauses and unlabeled
`break`/`continue`, value-producing unconditional, conditional-with-`else`, or range-with-`else` loops,
and empty void bodies).
Bare integer globals still need type context; no numeric default is chosen.
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
separated by one space. Booleans use `true`/`false`; integers use decimal notation.
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
`@println(42)` still require a type context: use `@as(int, 42)`, a typed global,
local, parameter, or function result. Native aliases do not introduce bare-literal inference defaults.
Interpolated strings can be printed like any string; function values and void
arguments remain unsupported for printing. The C backend
continues to reject widths other than 8/16/32/64.

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
and void values are supported as well as contextual integer and array literals.
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
`int`/`uint`), string, existing function-value or scalar-array type. Examples include
`let ready = true`, `let result = value + 1`, `let callback = twice`, and
`let mut copy = values` for an already typed array. Calls, indexing, `.len`, and
block/conditional results use the same bidirectional rules as other expressions.
No type is inferred from later uses or from the enclosing function result for a
local initializer. `let n = 1` and `let n = 1 + 2` still need annotations; there
are no integer defaults or widening conversions. Scalar array literals infer
`[]bool` from boolean elements or `[]T` from an already typed integer element.
A later element can supply context to earlier literals: `let values = [0, typed_value]`.
Known element types must match exactly; no signedness or width widening occurs.
An expected `[]T` takes precedence and checks every element against `T`, including
literal bounds. `let values = []` and `let values = [1, 2]` still need context.
Inferred arrays keep the same copy and GC-root semantics
as annotated arrays. See [examples/inferred-locals.nov](examples/inferred-locals.nov).

Explicit annotations remain supported, including immutable `void` values and
function values with unnamed parameter types (e.g. `let callback: (int) -> int = twice`).
Inferred `void` locals, named local function definitions/closures, labels and
other managed types remain unsupported. Mutable locals use `let mut` and support
`bool`, integer, string, existing function and scalar-array values (not `void`). Direct
identifier assignment (`n = value`) and integer-only `+=`, `-=`, `*=`, `/=`, `%=`
return `void`; a block ending in assignment therefore also returns `void`.
Assignments require a mutable local, not a parameter or global. RHS types must
match exactly, without implicit numeric conversions. Compound assignments read
the old local value before evaluating the RHS, then apply the existing wrapping
arithmetic/division rules and store once. RHS effects can update the same local;
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
can supply the type context; two bare integer branches still need an explicit
context. There are no implicit numeric conversions. Without `else`, the expression
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
There are no implicit numeric defaults: `loop {break 1}` needs integer context,
while breaking with an explicitly typed `int` local can infer `int`. As with returns and
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
`else` result can supply inference in either direction, without widening or integer
defaults. Array results copy at both break and exhaustion value boundaries; strings
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
`*=`, `/=`, `%=`). Scalar array annotations (`[]bool`, fixed-width integer types,
`[]int`/`[]uint`), literals (including empty `[]`), indexing (including negative
indices), `.len` reads, and plain `name[index] = value` stores are also supported.
Nonempty blocks use four-space indentation. Function parentheses,
list commas (including trailing commas), precedence, comments, and multiline
continuations are preserved; no grouping parentheses are invented.
This is syntax formatting, not semantic validation: existing literal/bitwise/shift
formatting does not make those forms executable. Array formatting does not check
expected literal types, scope, mutability, or whether a base is an array; it does
not enable array globals or new language semantics. Other members, `.len` writes,
indexed compound stores and stores through computed bases remain unsupported.
Nested array annotations, directly nested literals/indexing, slices, pointers,
other memory/container forms, labels,
iterator/other unsupported loop forms, attributes, quoted identifiers,
BOMs and shebangs remain unsupported or invalid. Unsupported syntax is reported
without modifying the file. Formatted output is reparsed and checked for structural,
token, and comment preservation before atomic replacement; any failed preservation
check also leaves the file unchanged.
Declarations are separated by newlines; EOF also terminates a complete declaration,
so the final newline is optional. Formatting adds a final newline.

# Concepts

## Comments
Regular comments start with `;` and end on a newline.

Doc comments start with exactly three semicolon (i.e. `;;;` but not `;;;;`).
Doc comments are used to automatically generate documentation.

## Functions
Nov functions can be understood as lazy or postponed expressions that takes parameters.

Parameters are immutable by default.

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

; generic functions
; see #Comptime for more information about that #
; signature: (#type, any, any) -> bool
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
  container rules. Printing/interpolation require the exact container method
  `toString: (Kind) -> string`; numeric or tag-name formatting is not derived.
- Enum instance calls such as `kind.eql(other)` use the value-receiver rules below.
  Explicit numbered values/backing types and enum arrays remain unsupported.

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
- Named fields require explicit `bool`, integer (including `i128`/`u128` and native
  `int`/`uint`), immutable `string`, nominal enum, or acyclic nominal struct types
  (including aliases). Semantic checking
  uses the same integer-width and literal-bound rules as scalar values. The C
  backend still supports only 8/16/32/64-bit integers: a reachable record layout
  containing a 128-bit field fails C emission with a backend width diagnostic;
  an unused record layout adds no backend requirement or emitted definition.
  Array element restrictions are unchanged. Fields end with a comma. A field may have a literal default of its exact type, with ordinary
  literal-bound checks. Defaults cannot call functions, compute values, reference
  globals or interpolate strings. Aggregate defaults are not supported: nested
  struct values must be supplied explicitly. Recursive by-value layouts and layouts
  deeper than 128 aggregate levels diagnose, including aliases/imports and late
  generic instantiations. Array/union/function/void/float fields remain unsupported;
  arrays of supported records are described below.
- Construct explicitly with `MyType[ .x = 5, .z = true ]` (or `Alias[...]`).
  Empty `MyType[]` works only when every field has a default. Unknown, duplicate,
  missing required and mistyped fields diagnose. Supplied expressions evaluate
  **once, left to right in source order**, independent of field declaration order.
  Omitted literal defaults are then inserted in declaration order and have no
  effects. Contextual `[ .x = ... ]`, anonymous records and tuples are not part
  of this slice.
- Read fields with `item.x`, including inside interpolation (`"{self.x}"`). Field
  lookup is type-directed: a record field named `len` is an ordinary field and
  does not change array/string `.len`. Assignment, locals, parameters, returns,
  branches and loop results copy record field values; immutable strings share
  their backing storage. Replacing a mutable record binding does not change copies.
- Fields of a direct mutable local support `item.field = value` with exact bool,
  integer, string, enum or struct type. Nested paths such as `item.inner.count += 1`
  and exact-type subrecord replacements are supported. Integer fields support `+= -= *= /= %=`, with the
  same wrapping/division/panic rules as scalar integers; string fields support `+=`.
  These assignments yield void. A real struct field named `len` is writable;
  array/string lengths remain read-only.
- A compound reads the old field **before** evaluating the RHS once. After the RHS,
  the final store updates only that field of the **current local slot**. Changes
  made by the RHS to other fields, or to the whole record, are preserved except
  for the targeted field. Plain assignment does not read its old field. A terminal
  RHS skips the pending operation/store without undoing earlier effects. This uses
  stable local field storage, not the whole-array snapshot update rule.
- Field mutation does not alter earlier value copies or immutable string backing.
  Immutable bindings/ordinary parameters (including `self`), union payloads/tags,
  temporary values and general pointer-based targets remain unsupported or
  diagnose immutable assignment. Bounded local element borrows support field updates
  as described under arrays. No implicit mutable receiver or borrowing is added.
  See [examples/struct-mutation.nov](examples/struct-mutation.nov) and
  [examples/nested-structs.nov](examples/nested-structs.nov). Nested record copies
  have independent value storage at every level; immutable string backing shares.
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
  generic union instances support these same method calls.
  Container constants, nested type declarations, general type values,
  struct global instances and C FFI are unsupported.
- Printing requires exactly the container method `toString: (MyType) -> string`.
  The parameter name is unrestricted, and aliases of the exact nominal type and
  `string` are accepted. Missing/wrong signatures diagnose when the record is
  printed. No global overload lookup or automatically derived representation is
  provided. Interpolation reuses this exact converter. Explicit `MyType.toString(item)`
  is also an ordinary call.
- Each print argument is evaluated, then (if a record) converted by **one** call
  to its `toString`, before moving to the next argument. Only after all arguments
  and conversions finish does the final print operation write their values,
  separated by spaces, with the existing checked-write/flush behavior. Thus a
  method's own effects can precede later argument effects; conversions are not
  repeated by the final write. Allocating methods return normal immutable strings.
- Declarations and methods are file-private by default. One argument-free
  `@[public]` exposes a supported immutable declaration across files, including
  structs, methods and module re-exports. Both file and named imports require
  public exports; cross-file printing also requires a public `toString` method.
  `@[private]` is unsupported. Duplicate, unknown or invalidly placed attributes
  diagnose. Visibility does not change C export policy.

C emission uses stack value structs with index-mangled fields and injectively
mangled method symbols. Reachable layouts precede function-pointer typedefs and
prototypes. Record temporaries live at function scope, including skipped-branch
slots; every string field's actual `void *owner` is rooted across allocations.
String byte-data pointers (including static literals) are never traced. The
current uniform record path installs a GC frame even for scalar-only records;
this is implementation overhead, not heap allocation of the records themselves.

#### Broader struct syntax (design, not fully executable)

The following older examples describe future syntax outside the bounded subset
above (including floats, container constants, contextual and anonymous values).
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
preserve identity. The current executable subset supports bool, integer, string,
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
- Assignment, arguments, returns and control-flow results copy the tag and fields;
  strings share immutable backing storage. C emission uses a tag and separate
  zero-initialized payload fields, not an overlapping C union. String owner fields
  are rooted, including across allocations and early returns. Payload extraction
  checks the active tag in all build modes.
- Non-generic union methods and `@[public]` use the same bounded container rules
  as structs, including exact `toString` printing. Visibility is metadata only.
- Record payloads, including nested structs, retain exact nominal identity and value-copy semantics;
  their string fields share immutable storage. Constructors, captures and `.!`
  work with records and aliases. C emits payload record layouts before containing
  unions and roots each nested string owner, with inactive payload fields zeroed.
- Explicit tag enums, nested union/array/function payloads, variant-pattern
  alternatives, nested destructuring, direct payload field access, union equality
  and union globals remain unsupported.

The examples below describe broader design, not additional implemented features.

Union is Nov's equivalent of [Sum type](https://en.wikipedia.org/wiki/Tagged_union)
since Nov's unions are always tagged unless annotated with `@[extern]`.
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
execution or generic functions returning runtime values. Generic container methods
specialize by canonical concrete container type; aliases share specializations and
different type arguments remain isolated. `@This()` and immutable
`let Self = @This()` container aliases refer to that concrete type. Static and
instance calls, callback function values, and recursive/forward/sibling calls use
the ordinary exact typing, evaluation-order, copy and rooting contracts.

Signatures and bodies are checked on demand when a concrete method is referenced,
called, or required for `toString` conversion. Unused methods do not invalidate an
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
Array-valued fields, recursive layouts, contextual constructors, general container
constants, value parameters and general compile-time execution remain unsupported.

The following broader stack example remains design-only: array fields, `#type`
and pointer-based mutable receivers are not delivered.

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

[`lib/std.nov`](lib/std.nov) publicly re-exports Result from `result.nov` and Option
from `option.nov`. Examples use real imports rather than copied definitions:

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

#### Executable imports

- Only immutable top-level `let ns = @import("literal")` namespace bindings are
  supported. `"std"` names the installed entry module; explicit relative paths
  must end in `.nov` and resolve against the importing source directory. Computed,
  interpolated, local/runtime, absolute-path and other named imports diagnose.
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

Growable `[]T` arrays are supported in function bodies and signatures for `bool`,
`i8/i16/i32/i64`, `u8/u16/u32/u64`, native `int/uint`, immutable `string`, and
finite acyclic nominal structs (including nested structs). Literals use an
expected array type when available; otherwise, a known supported element type
determines the exact element type for the whole literal, even when it
appears after untyped integer literals. For example, `[true, false]` infers
`[]bool`, and `[0, typed_integer]` infers that integer's array type. Empty `[]`
and all-untyped `[1, 2]` still need an expected `[]T`; numeric defaults remain
deferred. Type inference does not evaluate elements: evaluation remains once,
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
  **copy semantics**: independent element storage, not aliases. Immutable arrays
  cannot have their elements changed. Parameters are immutable; copy into a
  mutable local to edit them.
- `.len` has pointer-sized `uint` type. Internal capacity is also pointer-sized
  `uint`, but is not a source-level field. Indices have native `int` type, with
  `-1` denoting the last element and `-len` the first. Every out-of-bounds access
  (including empty arrays and minimum native `int`) panics with
  `panic: array index out of bounds`, exit 1, identically in debug and release.
- Plain `values[index] = element`, integer element `+=`, `-=`, `*=`, `/=`, `%=`
  and `values += other_array` require a mutable direct local identifier base.
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
  other elements or length. It does not reload the current array. For example:

  ```nov
  let mut a: []int = [10, 20]
  a[-1] += {a = [99]
      3}
  @println(a[0], a[-1], a.len) ; 10 23 2
  ```
- This first implementation deliberately copies eagerly, including temporary
  snapshots. Reads and updates may allocate/copy O(n) elements; append does not yet
  promise amortized constant time or preserve spare capacity across value copies.
  Compiler roots conservatively retain temporary backing until its instruction is
  reused or the function returns, rather than doing last-use liveness analysis.
- String/record arrays have independent element storage at ordinary copy boundaries.
  Record fields copy recursively by value; immutable string backing shares. Indexing
  returns a value, not an element reference. To edit a record element, copy it to a
  mutable local and assign it back, or use the checked local borrowing form below.
  Direct `items[index].field` mutation remains unsupported.
- String membership compares exact UTF-8 bytes/length, including embedded NUL, not
  owner identity. Record membership remains unsupported until record equality is
  defined. Indexed compounds remain integer-only; string indexed `+=` is not added.
- Managed backing allocations own repeated element descriptors and expanded pointer
  maps, tracing actual string-owner fields across the entire zero-initialized
  capacity. Record layout dependencies precede array descriptors. Map storage costs
  capacity times managed owner fields; GC never traces static byte pointers.
  See [examples/managed-arrays.nov](examples/managed-arrays.nov).
- `.len` writes are explicitly rejected: shrinking and especially initialization
  on length growth remain undesigned. No array printing, equality, concatenation
  with `+`, slices, nested arrays, enum/union/pointer/function elements, general aliases,
  array globals, capacity API, or iteration is delivered. The formatter supports
  the array syntax described above without extending these semantics.
  Indexed elements use normal scalar/record printing rules; array values themselves
  cannot be printed.

### Local static borrowing

Scoped borrows support every currently implemented non-void runtime value type:
bool, integer, string, enum, struct, union, array, range and function values. Borrow
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
  sources, explicit pointer annotations, reborrows, closure captures, temporary
  owners and general pointer APIs remain unsupported. Array-valued struct fields,
  nested arrays and other unsupported value types are not enabled by borrowing.
- Normal array snapshots/copies remain independent, including snapshots evaluated
  before a later borrow inside an index or RHS. In-place borrowed writes do not
  alter earlier copies. See the unchanged borrowed-element form in
  [examples/managed-arrays.nov](examples/managed-arrays.nov).

The broader examples below describe future syntax as well as implemented syntax;
printing arrays, defaulting bare integer elements and writable `.len` in them
are **not** executable features of this subset.

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
TODO

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
  signatures, fields, arrays, union payloads and globals remain unsupported.
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
  **range-specific default**, not a default for ordinary literals, array elements,
  or untyped arithmetic expressions. It does not involve constant folding.
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
  with no widening or general literal defaults.
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

Ranges are not general iterable or membership containers. Missing bounds,
steps/descending iteration, array/string/general iterators, multiple bindings,
references, labels, user continuation clauses, and value breaks without `else`
are explicitly unsupported. There is no `for` keyword alias. Header expressions
retain the existing single-newline continuation rules, including after `in` and
`..`/`..=`; the body follows the completed upper-bound expression without a newline.

## Match

The executable subset supports one or more `bool`, integer, or string subjects,
scalar literal patterns, `|` alternatives within a positional pattern, and `_`
wildcards. Subjects are evaluated **once, left to right, before selection**.
Prongs are tested in source order and only the first matching body executes.
With multiple subjects, commas separate positional patterns; a single `_`
matches all subjects. A single canonical `bool` subject (including type aliases)
is exhaustive when its patterns cover both `true` and `false`, even without a
final catchall. Alternatives such as `true | false` cover both values; `_` inside
an alternative is universal. Completely covered boolean prongs are rejected,
but partial overlaps retain first-match behavior. Coverage depends on the subject
type, not its runtime or literal value. Integer, string and multi-subject matches
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

Literal patterns use the exact subject type (no integer defaults or conversions).
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

The condition/unconditional forms and continuation clauses below are executable.
The conditional `else` and bounded exclusive/inclusive integer range examples are
also executable (see [Range](#range)). General iteration, references, and labels
below still describe intended syntax.

```nov
; items can be an array, a slice, a string or an iterator
; iterator semantic are not defined yet...
; type is always inferred
loop item in items {}

; n takes values [0;10[
; note than the boundary doesn't need to be literal
; loop n in x..y {} uses the bounds' exact integer type
loop n in 0..10 {}

; n takes values [0;10]
loop n in 0..=10 {}

; it's possible to loop on multiple values at the same time as long as they
; have the same length
loop item, i in items, 0.. {}

; item is a copy by default, add & to get a ref instead
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
loops with `else`, including bounded ranges, support `break value`; conditional
and range loops without `else` support
only bare `break`. A loop's `else` is outside that loop's control scope. With a
continuation clause, body-side `continue` runs the continuation; condition-side
or continuation-side `continue` reevaluates the condition directly. `break` and
`return` skip the continuation. Labels remain unsupported.

## Defer
TODO: same as zig

## In
`value in array` returns `bool`: true if any element equals the value, false
otherwise (including an empty array).
```nov
let nums: []int = [1, 2, 3]
@println(1 in nums) ; true
@println(5 in nums) ; false
```

The executable subset supports the existing scalar `[]T` arrays only: `bool`,
fixed 8/16/32/64-bit signed/unsigned integers, and native `int`/`uint`. Value and
element types must match exactly; there is no widening or signedness conversion.
A known array element type constrains the left literal; a known typed left value
constrains an array literal, including `[]`. Boolean literals provide a known
`bool` type (`true in [false, true]` works). Integer literals have no default:
`1 in [1, 2]` is rejected even with an expected `bool` result. The result type
does not choose an element type.

Operands evaluate once, left then right, following the existing array read/copy
rules. Membership itself is an allocation-free linear scan of the array's
length, using scalar value equality and stopping at the first match. It neither
mutates nor appends. Strings, nested arrays, pointers, ranges and iterators are
not supported membership containers or element types.

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

Immutable module bindings infer `bool` from `true`/`false` and `string` from plain
UTF-8 string literals. Numeric inference requires explicit type context:

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

There are no implicit integer defaults: `let count = 8` needs an annotation or
`@as`. Inference does not evaluate expressions: identifier chains, arithmetic,
blocks, calls, function values, arrays and void globals remain unsupported, even
under `@as`. Existing explicitly annotated literal globals are unchanged.
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
Supported RHS forms are `bool`, `void`, fixed-width integers, native `int`/`uint`,
`string`, nominal struct types, scalar array types and function types, including aliases in nested
parameter/result/array-element positions. Function-type parameters may be unnamed.
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

Aliases work in annotations and `@as`, not runtime expressions:
`@println(Count)` and `Count(1)` are errors. An alias of a nominal struct supports
that struct's explicit `Alias[ .field = value ]` constructor and static methods;
this does not add numeric alias constructors, mutable/local aliases, general runtime type values, `@TypeOf`, generics or
comptime functions. `@as` remains exact expected-type ascription, not a checked
numeric cast. See [examples/type-aliases.nov](examples/type-aliases.nov).

## Primitive Types
TODO
- bool
- string
- rune

See also:
- [Integers](#Integers)
- [Floats](#Floats)
- [Arrays](#Arrays)

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
are checked; there are no defaults, widening or implicit signedness conversions.
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
C `int`/`unsigned int`. Bare literals still require type context; native aliases do
not select an inference default or add implicit integer conversions. The wider
type list below is a proposal.

TODO
- c\_int, ...
- u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f16, f32, f64, f80, f128
  - float = f32 or f64 based on architecture

## Floats
- `float` - f32 or f64 depending on architecture
- `f16` - IEEE-754-2008 binary16
- `f32` - IEEE-754-2008 binary32
- `f64` - IEEE-754-2008 binary64
- `f80` - IEEE-754-2008 80-bit extended precision
- `f128` - IEEE-754-2008 binary128
- `c_longdouble` - matches long double for the target C ABI

## Function Pipes

`value |> function` is exactly unary application `function(value)`, including
**callee-first** evaluation: evaluate the RHS callee before the LHS argument,
not a new left-to-right pipeline effect order. Chaining is left-associative.
Named functions and function values require exactly one parameter; its exact type
constrains literals (`10 |> fibo`). `value |> @print` and `value |> @println`
accept an already typed bool/integer/string result and return void. They do not
supply an integer default. No partial application or implicit conversions occur.

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
| Concatenation         | a + b <br> a += b  | string <br> [Arrays](#Arrays)                | Strings: `+` only; scalar arrays: `+=` only                          |
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
| Access                | a\[b]              | [Arrays](#Arrays)                            | Scalar array indexing supported; string indexing deferred          |
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
Operator overloading is possible on the following operators:
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

    ; used by print
    ; signature must be `toString: (T) -> string` where T is the container type
    let toString: (self: Complex) -> string = "{self.re} + i{self.im}"
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
`%=`), indexing, slicing and Unicode operations remain deferred.
`[]string` and supported record arrays work with independent element storage and
shared immutable string backing; nested arrays remain unsupported.

`.len` is the byte length as native `uint`, not a character or grapheme count.
`"é".len` is 2 and `"😀".len` is 4. Literals use the existing escapes (`\n`, `\r`,
`\t`, `\\`, `\"`, `\'`, `\0`, `\xNN`, `\u{...}`, `\{`, `\}`). Decoded bytes
must be valid UTF-8; NUL is valid, including through `\x00` or `\u{0}`. Invalid
escapes/UTF-8 report source locations. Unescaped `{expression}` interpolates a
scalar value, including function calls; use `\{` and `\}` for literal braces.

`@print`/`@println` write exact UTF-8 bytes with explicit lengths, including embedded
NUL, with the same spaces, buffering, checked writes/final flush and panic behavior
as scalars. Windows stdout is switched to binary mode on the first print to avoid
newline translation; this is UTF-8 byte output, not UTF-16 console conversion.
Literal-only immutable globals with explicit `string` annotations are supported
without eager initialization calls; inferred/mutable globals and global concatenation
or calls remain unsupported. Global interpolation is also rejected as nonliteral;
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
(exact decimal, including minimum/maximum values), and strings (unchanged bytes).
Nominal enums, structs and concrete generic or non-generic unions also work when they define the exact
container `toString: (SelfType) -> string` method already required by print.
Each placeholder expression evaluates once, then converts once immediately before
proceeding to the next chunk or placeholder. Aliases preserve the exact signature;
allocating converters and managed payloads stay rooted. Missing/wrong converters
diagnose; no derived formatting or global overload lookup is introduced. Terminal expressions skip conversion and later placeholder effects.
No numeric defaults, widening or implicit fallback formatting are introduced.

```nov
let describe: (n: int, ready: bool) -> string = "n={n}, ready={ready}"
```

Placeholders use the normal expression parser and checker: identifiers, calls
(including nested calls), scalar operators, array access and `.len` work with the
usual type contexts. For example, `"{describe(1, 5, 5)}\n{describe(5, 1, 3)}"`
uses the callee's parameter types for the numeric arguments. Existing lexical
scope, no-shadowing and declaration-before-use rules apply; spaces are allowed.
Bare numbers still need a type context (`"{1}"` is rejected).

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
integer conversions and concatenations allocate rooted GC backing. Concatenation
checks length overflow; allocation failure panics. This minimal implementation uses
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
- `@TypeOf(...)`: Returns the type of a value.
- `@typeInfo(...)`: Returns type information about a value.
- `@This()`: Returns the type of the current container
- `@print(...)`: Output bool, fixed/native integer and string args separated with a space to stdout; returns void. Nominal structs require their exact `toString: (MyType) -> string` method, called once immediately after evaluating each record argument. Interpolated strings work like other strings.
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
- `@[extern]`
- `@[packed]`
- `@[export]` / `@[export("symbol_name")]`: expose an ordinary function through the C ABI.
- `@[inline]`
- `@[noinline]`
- `@[cold]`
- `@[noreturn]`: Specify that a function can't return.
- `@[test]`: Decl with it are ignored unless run with `nov test` where it behave like `zig test`.
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
attributes diagnose. `@[export]` and `@[export("symbol_name")]` are supported as
described below and do not imply `@[public]`. Other entries in this list remain
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
bodies. Parameters/results may be `bool` or 8/16/32/64-bit signed/unsigned integers;
results may also be `void`. Canonical aliases are accepted, including pointer-sized
`int`/`uint`. The C types are `bool` (include `<stdbool.h>`) and the corresponding
`intN_t`/`uintN_t` (include `<stdint.h>`); native integers match `intptr_t`/`uintptr_t`
on the native target. Calls use the C compiler's ordinary C calling convention.
Strings, arrays, ranges, enums, records/unions, function values, wider integers,
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

This slice adds no foreign calls, pointers, callbacks, generated headers, shared-
library packaging or DLL export-list support. Managed values cannot cross the ABI;
the existing runtime remains single-threaded. Automatic library teardown and
concurrent foreign entry are not provided. Panics remain process-terminating;
exports do not turn them into recoverable C errors.

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
calls, function-value extraction, printing and interpolation (`toString` must be
public for cross-file use). Fields and union variants do not support visibility
attributes.

Migration: add `@[public]` to declarations and methods used across files; remove any
`@[private]` attributes. A public type does not automatically make its methods public.

## Comptime
Works almost like zig except that we use `#` instead of `comptime`
```nov
; comptime expr
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
