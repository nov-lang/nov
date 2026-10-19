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

Requires Zig 0.16.0.

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
cycles for a bounded expression subset. Internal annotation analysis resolves
explicit `bool`, `void`, fixed-width integer, native `int`/`uint` and function
types, and checks integer and boolean literals against annotations. Bounded function-body checking binds
parameters and checks integer arithmetic, boolean expressions, comparisons and
calls against collected signatures, including recursive calls. Explicitly typed
immutable and mutable scalar locals and blocks with final-expression results
are supported in function bodies. Literal defaults, user-defined type aliases,
shadowing, global mutation and loops remain deferred or unsupported.
Lowering produces typed operations with source spans, ordered operands/call
arguments, explicit returns, and structured conditional regions for boolean `and`/`or` and `if`/`else`.
It requires immutable literal globals; unit/global block initializers are
not lowered yet. An internal C11 backend emits verified IR, including fixed-width
integer arithmetic and program entrypoint emission through `emit-c`.
`@print`/`@println` support booleans and explicitly typed integers, including
`int`/`uint`. Plain `check` remains syntax-only.

Use `zig build run -- check --semantic file.nov` for the current bounded
semantic/lowering subset. It runs declaration collection, initializer dependency
analysis, annotation/literal and function-body checking, then typed lowering and
IR verification. Source errors and unsupported/deferred forms fail with source
diagnostics (including related notes where available); recovered syntax errors
stop semantic analysis. Globals must be immutable, explicitly typed integer or
boolean literals. Functions need explicit supported signatures and bounded
expression bodies (including forward/recursive calls, local declarations and
assignments, nested blocks, boolean short-circuiting, `if`/`else`, explicit returns
and empty void bodies).
Inferred globals, mutable globals, nonliteral global initializers, global unit
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
nov build file.nov --release          # -O2 (also the default)
nov file.nov --cc /path/to/cc
```

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
default/release builds add `-O2`, while debug builds add `-O0 -g`. These select C
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
Library `CEmitter.emit` still emits no C `main`; only
`CEmitter.emitProgram` adds the wrapper. Backend integer widths are restricted to
8, 16, 32 and 64 bits, including nested function signatures, with source diagnostics
for unsupported widths.

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
`@println(42)` still require a type context: use a typed global, local, parameter, or
function result. Native aliases do not introduce bare-literal inference defaults.
Strings/interpolation, function values and void arguments remain unsupported for
printing; the C backend
continues to reject widths other than 8/16/32/64.

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

This prints `42` on two lines. Locals require an explicit supported type and an
initializer. Immutable scalar/unit values and existing function values (with
unnamed parameter types, e.g. `let callback: (int) -> int = twice`) work; named
local function definitions/closures, inferred locals, labels, loops and
memory-bearing values are rejected. Mutable locals use `let mut`
and support `bool`, integer and existing function values (not `void`). Direct
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

Format files in place with `zig build run -- fmt file.nov`, or use
`zig build run -- fmt --check file.nov` to check formatting without writing.
Both commands accept multiple file paths. Formatting currently supports simple
`let` declarations, literals, operators, and comments, but not function signatures,
calls or blocks (including the executable examples above); unsupported syntax is
reported without modifying the file. Formatted output is reparsed and checked
for structural, token, and comment preservation before atomic replacement.
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
`.!` is syntax sugar for Result/Option which is equivalent to unwrapping the
value or returning it if it's `.err`/`.none`.

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
Array literals are comma-separated, may span multiple lines, and may have a
trailing comma.

```nov
let mut my_array = [1, 2, 3]
@TypeOf(my_array) ; returns []int
; arrays have a special field for its len, it is mutable if the string is mutable
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
Ranges are values too!
```nov
5 in 0..10 ; true
```
<!--
note for myself: implem
```
struct {
    start: int,
    end: int,
    step: int, ; always 1?
    ; `.next()` for iter and `in` op are ez to implem
}
```
for step maybe ++/-- or +=/-=

loop _ in 10..=0-=1 {
}
-->

## Match
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
Loop expressions are not implemented yet; `check` and `fmt` report a diagnostic.
The examples below describe intended syntax, not currently supported behavior.

```nov
; items can be an array, a slice, a string or an iterator
; iterator semantic are not defined yet...
; type is always inferred
loop item in items {}

; n takes values [0;10[
; note than the boundary doesn't need to be literal
; loop n in x..y {} is fine as long as x and y are unsigned? integers
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

; "zig while" loop
let mut i = 0
loop i < 100 : i += 2 {}

; else branch
; the else branch is evaluated when the loop is not exited with a break
let rangeHasNumber: (begin: uint, end: uint, number: uint) -> bool = {
    var i = begin;
    return loop i < end : i += 1 {
        if i == number {
            break true;
        }
    } else {
        false;
    }
}
```

## Break & Continue
TODO: same as zig

## Defer
TODO: same as zig

## In
Check if an element is in an array
```nov
let nums = [1, 2, 3]
@println(1 in nums) ; true
@println(5 in nums) ; false
```

## Assignment
```nov
let mut a = 3 ; @TypeOf(a) == int
let b = 4 ; @TypeOf(b) == int

let x = y = 3 ; parse error
let x = a += b ; parse error
let x = {y = 3} ; should be fine, @TypeOf(x) == void
let z += 3 ; compile error, expected `=`
```

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

C arithmetic uses `uint32_t`/`uint64_t` working values to avoid signed overflow
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

## Operators
<!-- Yes this is not really lisible as raw text -->
| Name                  | Syntax             | Types                                        | Remarks                                                             |
|-----------------------|--------------------|----------------------------------------------|---------------------------------------------------------------------|
| Assignment            | a = b              | All types                                    | `a` is an identifier and `b` is an expression.                      |
| Addition              | a + b <br> a += b  | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Concatenation         | a + b <br> a += b  | string <br> [Arrays](#Arrays)                | TODO                                                                |
| Substraction          | a - b <br> a -= b  | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Negation              | -a                 | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Multiplication        | a * b <br> a \*= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Division              | a / b <br> a /= b  | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Remainder Division    | a % b <br> a %= b  | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Left        | a << b             | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Right       | a >> b             | [Integers](#Integers)                        | TODO                                                                |
| Bitwise And           | a & b              | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Or            | a \| b             | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Xor           | a ^ b              | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Not           | ~a                 | [Integers](#Integers)                        | TODO                                                                |
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
| Access                | a\[b]              | [Arrays](#Arrays) <br> string                | TODO: b is an Integer                                               |
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

## String Interpolation
`"{varname:[fill][alignment][width][.precision]}"`

Escape `{` and `}` with `\`.

## Concurrency
TODO

## Builtins
### Functions
- `@import(path: string)`: Import a nov file. See [Visibility](#Visibility) for which declarations gets imported.
- `@TypeOf(...)`: Returns the type of a value.
- `@typeInfo(...)`: Returns type information about a value.
- `@This()`: Returns the type of the current container
- `@print(...)`: Output bool/fixed-width integer args separated with a space to stdout; returns void. Strings/interpolation are not implemented yet.
- `@println(...)`: Same as print with a newline at the end, including with no args.
- `@fprint(file: File, ...)`: Same as print but output to a specific file. Returns a Result<>.
<!-- - `@eprint(...)`: Same as print but output to stderr. -->
<!-- - `@printf(fmt: #string, args: ...)` works like std.fmt.format and print to stdout -->
- `@panic(s: string)`: Output `s` and backtrace to stderr, then terminate the program with error code 1.
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
- `@[export]`
- `@[inline]`
- `@[noinline]`
- `@[cold]`
- `@[noreturn]`: Specify that a function can't return.
- `@[test]`: Decl with it are ignored unless run with `nov test` where it behave like `zig test`.
<!-- - `@[callconv]` -->
- `@[public]`
- `@[private]`
- `@[memoize]`: (?, related to pure...)
- `@[require(condition..., "optional error message")`: Ensure that the specified conditions are verified for the arguments given to a function or to a container initializer.
<!-- - `@[ensure(condition..., "optional error message")`: Ensure that the specified conditions are verified for the values returned by a function. -->

Attributes can be set only on top level / container declarations

## Visibility
Visibility is modified via [attributes](#Attributes).

- public: visible everywhere
- private: visible only in current file
- no modifier: visible when importing as a file (`@import("std.nov")`) but not
               when importing as a module (`@import("std")`)

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
