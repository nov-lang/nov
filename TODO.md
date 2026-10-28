# TODO

## HIGH PRIORITY: optimize expensive recursive value copies

Array-valued struct fields are implemented.
The copy-depth question is settled: ordinary value boundaries eagerly create
recursively independent array backing through nested structs, record elements and
union record payloads; immutable strings share. These recursive copies are a major
performance concern, especially repeated snapshots and updates. Targeted copy
elision now transfers fresh temporaries rather than recloning at binding/call/return
boundaries; readonly field/simple-index projections avoid enclosing aggregate copies.
Effectful indices retain snapshots. Readonly membership also projects the RHS
without cloning existing arrays/fields/borrowed values; contextual literals still
work, and effectful computed bases retain required internal snapshots. Exact
allocation-count regressions cover these paths.
Insertion/replacement now adopt proven-detached single-use temporaries. Append
keeps its fresh outer buffer and recursively copies only unproven input portions.
All IR references are counted. Indexed compounds may transfer a detached snapshot
with one consuming update after earlier nonescaping scalar/length observations in
the same conditional/loop region. Later reads, aggregate projections, repeated
consumption and region crossings retain copies. Calls/merges/storage reads are not
ownership evidence.
Snapshot copies, conservative fallbacks and more general update paths still need
further optimization.
Broader elision, moves/consuming transfers and copy-on-write remain candidates,
**not approved designs**. Preserve observable value independence, static borrowing and GC safety.
This targeted high-priority follow-up is not blanket approval for other optimizations.
`defer` implementation remains explicitly postponed until the user asks to resume.

Plain/generic acyclic struct array fields support reads, replacement, append and
indexed replacement/integer compounds on nested mutable-local paths. Updates snapshot
the array before index/RHS effects, then replace only the target field in the current
record; sibling changes survive. Existing whole-record/record-element stable loans
support array-field replacement/append and ordinary recursively copied reads, but
not indexed field updates. No field-specific loans, implicit mutable receivers,
nested arrays or recursive layouts through arrays are added. Immutable closure
captures snapshot recursively; mutable captures share a cell, not ordinary read
results. See `examples/array-fields.nov`.

## Current delivered status

Runes and Unicode scalar string inspection are implemented. `rune` is distinct from integers: `0..=0x10FFFF` excluding `0xD800..=0xDFFF`.
Single quotes hold exactly one scalar or escape (`'é'`, `'😀'`, `'\n'`, `'\xE9'`,
`'\u{1F600}'`); empty/multiple-scalar or invalid literals reject. Runes support
UTF-8 printing/interpolation via variables, exact equality, arrays, fields, literal
matches with catchalls, union payloads and scoped borrows. No numeric conversions,
arithmetic, ordering, rune ranges or foreign ABI support.

String indexing decodes `rune` with native `int` scalar offsets; negatives count
scalars from the end. `.len` is native `uint` scalar count, not byte/grapheme count:
`"é😀\0".len == 3`, `"é😀"[-1] == '😀'`, `"e\u{301}".len == 2`.
Both indexing and length scan UTF-8 in O(n), allocating nothing; storage remains
UTF-8 bytes. See examples/runes.nov and examples/string-bytes.nov (historical name,
now a Unicode scalar example). String slicing is implemented: `text[start..end]`,
`text[..end]`, `text[start..]` and `text[..]`, with half-open `..` or inclusive `..=`.
Bounds are native `int` scalar offsets; negatives count from the scalar end.
An explicit inclusive end must name an existing scalar, then advances one boundary;
omitted ends mean through the string end for either operator. After conversion,
`0 <= start <= exclusive_end <= len` is required; invalid bounds panic in all modes.
Adjacent reversed inclusive bounds form an empty slice; an explicit end at `.len`
or any explicit inclusive end on an empty string rejects. Base/start/end evaluate once,
left to right. Views are immutable, allocation-free and share the original GC owner
without copying UTF-8 bytes; small views retain the full allocation. `.len` counts
scalars. Array slicing remains unsupported. Array/string iteration is supported
as described below; see examples/string-slices.nov for slicing.

Language tests are executable: `@[test]` marks top-level `() -> void` functions,
excluded from normal semantic analysis/emission and not callable as ordinary values.
`nov test file.nov` selects only root-file tests in source order, compiles once and
runs each in a fresh subprocess, continuing after failures/crashes and reporting a
summary/nonzero failure status. No main required; supports --cc, --debug/--release
and --lib-dir. `@assert(bool[, string])` evaluates arguments eagerly once in order,
reports source locations, and remains active in all modes. No unwinding/recovery,
parallel execution, test filters, timeout flags or automatic dependency-test discovery.
See examples/tests.nov and README.

Scoped borrowing now covers every supported non-void runtime value type: bool,
integers, runes, strings, enums, structs/unions, arrays, ranges and functions. let ref=&local
also supports readonly locals/parameters; mutability follows owner. Reads yield
ordinary copied values, so copying/arguments/returns/methods do not escape references.
Assignment writes the pointee, compounds/array updates use ordinary exact-type rules.
No runtime validity checks. General pointer annotations, nonlocal/void sources,
reborrows and escaping capabilities remain unsupported. See examples/local-borrows.nov.

Local static array-element borrowing is executable: let ref=&localArray[index]
for all supported array elements, with whole-element and nested record updates.
Mutable owners permit writes; immutable owners permit reads. CFG fixed-point
liveness ends the exclusive whole-array loan after its last use, tracking branches,
loops/backedges, terminal controls and RHS stores. Conflicting owner access and
reference-capability escape/rebind/reborrow use diagnose; value copies are allowed. Owner access
during index evaluation is conservatively rejected too. References are bounded
local capabilities, not general first-class pointers; readonly loans also remain
exclusive. Index normalization/bounds happens once, owner base is rooted, and no
runtime reference-validity checks or interior GC roots are introduced. Normal array
copies and pre-borrow snapshots remain independent. User's managed-arrays.nov now
works unchanged with &people[0]; explicit annotations/escaping refs remain deferred.

Reusable range values are executable: start..end / start..=end construct integer
bounds once left-to-right, and loop i in range snapshots the source once per loop.
Reuse restarts at the beginning; source-binding replacement cannot affect an active
loop. Locals/copies/assignment and inferred branch/block/loop results work. Inclusive
state is per value, endpoint type exact; existing range-only int/uint defaults,
empty reversed ranges and inclusive-MAX exhaustion remain unchanged. No allocation.
No new range annotation syntax, signatures, aggregate fields/payloads, arrays,
globals, field attributes, printing/equality/membership or iterator protocol yet.
See examples/range-values.nov. Chained ranges diagnose; slice/match syntax preserved.

Ordinary generic structs are executable alongside generic unions: named type
parameters, canonical declaration+argument identity through aliases/imports,
concrete fields/defaults, finite nested instances, direct Box(T)[...] constructors,
@This/Self and demand-specialized methods. They work in arrays and Option/Result
payloads with existing value-copy/GC rules. See examples/generic-structs.nov.
Array-valued fields now use recursive independent copies, including concrete array
type arguments; contextual constructors, value generics and general comptime remain deferred.

Growable []string and acyclic nested-record arrays are executable with independent
element storage and shared immutable string backing. Index values and replacements,
append, literal inference, copies/returns and managed owner tracing work. String
membership compares bytes, including UTF-8/NUL; record membership/equality and
indexed string compounds remain unsupported. Owned expanded GC pointer maps cover
all managed capacity slots, with checked alignment/size/OOM and zeroed owners.
No nested/enum/union/function arrays or escaping element aliases. Record elements
may themselves contain supported array fields with recursively independent backing.
See examples/managed-arrays.nov. Generic structs now work with supported field types.

Acyclic nested nominal struct fields and mutable-local nested field paths now work,
including aliases/imports/forward layouts and nested structs as union payloads.
Value copies have independent struct storage; immutable strings share backing.
Recursive layouts or depth over 128 aggregate levels diagnose; nested values must
be supplied explicitly (scalar literal defaults unchanged). C topological layouts
and recursively flattened string-owner roots preserve GC safety. Validation includes
late generic layouts, forged cycles, deep graphs and zero-root diamond performance.
See examples/nested-structs.nov. Array fields now work; union/function fields remain unsupported.

Direct mutable-local struct fields support exact bool/integer/rune/string/enum assignment,
integer +=/-=/*=//=/%= and string +=. Compounds snapshot the old field before RHS;
final stores change only that field in the current local, preserving other-field
and whole-record RHS changes. Terminal RHS skips the pending operation/store.
Copies remain independent, strings share immutable storage, and string owners stay
rooted. Bounded local element borrows also mutate fields; general temporary/union/
parameter/pointer mutation and implicit borrowing remain unsupported.
See examples/struct-mutation.nov; generic structs now use the same mutation rules;
prioritized roadmap is in local PLAN.md.

Option.isSome/isNone and Result.isOk/isErr are ordinary public bool-returning
methods that test only the active variant, without unwrapping or inspecting payloads.
They work with scalar, record and void payloads. Real-import CLI tests cover both
tags, static/instance calls, method references, aliases and receiver effects.

lib/option.nov provides ordinary public Option(T), re-exported as std.Option, with
some payload/none variants, unwrapOr (eager fallback) and unwrapOrElse (zero-argument
callback invoked only for none). Real-import tests cover identity, exact types,
scalars/records/void, exhaustive matching, fallback effects and debug/release runs.
examples/options.nov demonstrates safe division and .! propagation from Option(uint)
to Option(string). Structural .! accepts exactly some/none with void none, yields
some or immediately returns enclosing none. Result behavior is unchanged; crossing
Option/Result families is rejected. Evaluation is once-only and later effects skip
on none; no builtin Option or implicit payload wrapping was introduced.

Ordinary generic union methods now specialize on demand by canonical concrete
container type, preserving aliases and independent type arguments. @This and
let Self=@This aliases work in signatures/bodies, as do static/instance calls,
callbacks, sibling/forward/recursive calls and exact concretized toString.
Unrequested signatures/bodies remain dormant; requested methods are fully checked.
This keeps void Result payloads usable without changing void-parameter restrictions.

lib/std.nov now publicly re-exports ordinary Result from lib/result.nov. Examples
use `let std=@import("std")` and `let Result=std.Result`; real-import CLI tests cover
its unwrapOr/unwrapOrElse methods and void payloads. Library lookup uses executable-
relative ../lib/nov or explicit --lib-dir, including relocated installs. Explicit
relative .nov imports resolve from the importer, cache canonical paths and reject
cycles. Namespaces do not inject bare names; aliases/re-exports preserve identities.

Separate AST owners, source-qualified spans/diagnostics, canonical declaration
identities, shared semantic/IR pools and defining-scope generic method resolution
now support multi-source compilation. Only root main is an entrypoint. Build output
cannot overwrite any loaded source, including imported library/symlink targets.
No source concatenation/name rewriting or builtin Result is used. Declarations and
nominal methods are file-private by default; both file and named imports require
explicit @[public] for cross-file access. Checks cover aliases/reexports, generic
instances and implicit toString calls. @[private] is unsupported. Runtime module
initialization, arbitrary package search and value generics/comptime remain deferred.

Instance calls value.method(args) work on nominal structs, non-generic unions and
enums as sugar for Type.method(value,args). The explicit first parameter must match
the exact nominal receiver type. Receiver evaluates/snapshots once before explicit
arguments left-to-right; direct noreturn methods remain terminal. Fields retain
lookup precedence. No implicit mutation/borrowing, bound method references or
general generic functions were added. structured-errors.nov uses Entry.describe().

Nominal unnumbered enums are executable: qualified/contextual tags, aliases, exact
==/!=, exhaustive single-enum matches with qualified patterns/alternatives/catchall,
covered-pattern notes, literal globals, record fields and tag defaults, union
payloads, generic arguments and .! error propagation. Static methods/public and
exact toString printing/interpolation reuse container rules. Enum leaves add no GC
roots; internal uint32 ordinals expose no numeric/FFI policy. Numbered values,
backing types, numeric conversions/arithmetic/ordering and arrays remain unsupported. structured-errors.nov now handles an enum .not_found kind.

Acyclic nominal records now work as ordinary/generic union payloads, retaining exact
identity through aliases, constructors, captures, copies and .! propagation. C
layouts are dependency-ordered and every nested string owner is rooted; inactive
payload fields are zeroed. Nested union/array/function payloads and recursive
layouts remain unsupported. Reachable wide scalar leaves
still produce backend width diagnostics at the leaf field.

Record interpolation now uses the same exact container toString method as print,
once after each placeholder expression and before later chunks/placeholders.
Non-generic unions with an exact converter also work. No derived formatting or
global overload lookup was added. See examples/structured-errors.nov for record
success/error payloads, propagation and captured error interpolation.

Terminal `@panic(message)` and function annotation `@[noreturn]` are executable.
`noreturn` is not a type: annotated functions return void, reject explicit returns
and possible normal completion. Direct annotated calls are terminal; ordinary
function values erase the guarantee and indirect calls conservatively complete.
Panic takes one exact string, evaluates once, attempts stdout flush, writes
length-based UTF-8 `panic: <bytes>\n` to stderr (including NUL), and exits 1 without
a backtrace. Branch/match merges and loop conditions track terminal paths; C emits
_Noreturn only on annotated functions, not function-pointer types.

Ordinary nominal tagged unions, exhaustive payload-capturing matches and bounded
ordinary generic union declarations are executable. `Result` is defined in Nov,
not a compiler builtin; see `examples/results.nov`. Explicit `U.ok[value]` and
contextual `.ok[value]` constructors preserve exact types. Payloadless variants
use `.none[]`. Copies preserve value semantics and share immutable string storage;
GC roots track zero-initialized payload owner fields. Single-union matches require
complete variant coverage or a catchall, with immutable private captures.

Plain named `type` parameters instantiate union templates, with canonical identity
by declaration and exact arguments. Template aliases and instantiated aliases work.
Expansion is bounded at 64 levels. Structural `.!` requires exactly `ok`/`err`
variants, evaluates once, yields success or constructs/returns the enclosing error
variant with identical error type. Ordinary returns never implicitly wrap payloads.
General comptime/value parameters, nested union/
array/function payloads and explicit tag enums remain deferred.
The filesystem error example remains design-only: filesystem APIs and file-resource
semantics are missing and intentionally deprioritized; imports are implemented.

Bounded nominal value structs are executable; `examples/type.nov` is unchanged
and runs/checks/formats successfully. Separate declarations have distinct identities;
module aliases preserve identity. Flat bool/integer/rune/string/enum fields have exact types,
literal-only defaults and required-field diagnostics. Explicit `Type[.field=value]`
constructors evaluate supplied fields once left-to-right, then omitted literal defaults
in declaration order. Record value boundaries copy fields; strings share immutable
storage. Field reads work in interpolation. Container methods support scoped and
static `Type.method(...)` calls; print calls exact `toString: (MyType) -> string` once
immediately after each argument is evaluated, before the next argument. Only
argument-free `@[public]` on immutable supported declarations is accepted;
declarations and methods are otherwise file-private under both import forms.
`@[private]` is unsupported. Explicit C export annotations are separate from Nov
visibility. C value layouts/root frames retain every string owner in
parameters, temporaries, local slots and control-flow merges. Contextual/anonymous
constructors, union/function fields, record globals, container constants/types,
derived printing and FFI remain deferred. Nested structs, array fields and record
arrays are delivered under the recursive value-copy rules above. See README's executable struct
contract; the older broader syntax examples below are not delivery claims.

Executable scalar status: integer binary `&`, `|`, `^` and unary `~` support
exact matching fixed-width 8/16/32/64 and native `int`/`uint` types, with same-width
two's-complement results, contextual literals, no widening/defaults and no overflow
panic. Booleans use `and`/`or`; match-pattern `|` remains alternatives. Shifts,
bitwise compound assignments and computed match patterns remain unsupported.
See README's integer section for the executable contract; proposals below are
not an implementation checklist.

Bounded immutable global inference: bool, rune and plain UTF-8 string literals infer
canonical types; numeric literals require explicit `@as(T, literal)` context.
Nested scalar ascriptions require exact canonical types (including forward type
aliases and any explicit declaration annotation), with checked literal bounds and
no runtime conversion. Constants own
string bytes and inline at use sites. Bare integer globals have no default;
identifier chains, computation, blocks, eager calls, arrays, void/function values,
mutable globals and global interpolation remain unsupported. Existing annotated
globals and body/control-flow ascriptions are unchanged.

Explicit expression type context: `@as(T, value)` is implemented in function
bodies, including interpolation, and for the bounded scalar globals above. It checks exact canonical types and contextual
literal bounds without a runtime conversion or additional array-copy boundary.
Numeric conversions, widening, signedness changes and general type values still
need language-design decisions; `@as` does not choose those semantics.

Canonical type aliases: plain immutable module `let Name = typeExpression` is
implemented for existing supported scalar, scalar-array and function types,
including nominal structs, forward chains and aliases in nested type components.
Aliases work in annotations and `@as`, never as runtime type values. Nominal struct
aliases additionally preserve explicit `Alias[...]` constructors and static methods;
this does not introduce numeric constructors. Cycles have
source-related diagnostics; alias expansion is iterative and bounded to 128 active
expansions. Local/mutable aliases, newtypes, general type values, `@TypeOf`,
general generics and comptime functions remain unsupported (bounded generic unions
are delivered above). Numeric conversion design
remains deferred; this does not add casts or general literal defaults.

Next step: add new IR that
- forward declaration
- resolve literals
- desugar operator overloading?
- have more usage safety than Ast
- start storing values in a constant pool

AstGen/Sema from zig looks to complex if we just want to generate some C code.
Move to a custom IR?

Figure out what (first) backend to chose
- **A language, probably C** ~but can be Zig or something else (JS)~
- A VM, a custom one (support JIT) or the JVM (or WASM)
- Native, either unoptimized for fun or dig into LLVM

Keep to the main features aka:
- functional: functions are first class citizen (?)
- async: good and lightweight async support is mandatory (no semantic yet)
- GC: nov should handle everything memory related
- interoperability with C unless it harms the features above
Keep Nov away from:
- Really low level stuff aka assembly, kernel or drivers. Use zig instead.

# Proposals and stuff to add

## Explicit member bindings and fallible main (not implemented)

Leading `use` syntax candidate:
```nov
let std = @import("std")
use std.{Option, Result}
```
This is explicit binding shorthand, not a new import mechanism: equivalent to
`let Option = std.Option` and `let Result = std.Result`. Preserve visibility and
no-shadowing rules; do not make bindings public implicitly. Initially limit it to
top-level namespace member bindings supported by ordinary declarations, not runtime
destructuring. No wildcard imports. Braces allow multiline lists; optional renaming
such as `Result as Outcome` is a syntax candidate, not a settled grammar. Public
re-exports remain a separate concern.

Separate entrypoint proposal:
```nov
let main: () -> Result(void, string) = {
    .err["error from main"]
}
```
Keep ordinary function-body `=` syntax. Returning `.ok[]` would exit 0; returning
`.err[message]` would print the message to stderr and exit 1. Recognition of Result
and exact error-output/flush details still need specification. Current executable
main remains `() -> void`; neither proposal changes the implementation roadmap yet.

## Same-name constructor fields (not implemented)

```nov
Rectangle[.width, .height]
Rectangle[.width, .height = 10]
```
Shorthand `.width` means `.width = width`, resolved in the current scope with
ordinary type and visibility checks. Allow mixing shorthand and explicit fields;
preserve left-to-right evaluation, existing copy semantics, and duplicate/missing-
field diagnostics. An unresolved shorthand name is an unknown-name error, even
when that field has a default. This is not positional initialization or destructuring
and does not change the implementation roadmap yet.

## Agreed text-storage design (future APIs)

Keep one immutable, valid-UTF-8 `string` type. `let mut` permits rebinding and `+=`
creates a new string; it does not permit in-place byte changes. Use mutable `[]u8`
for unrestricted bytes. Future conversions must copy string bytes into independent
array storage, and validate UTF-8 when turning arbitrary bytes into a string, with
a recoverable error on invalid input. These conversions are not implemented.

A separate stdlib builder (tentatively `StringBuilder`) should preserve UTF-8 for
string/rune appends and require valid UTF-8 boundaries for text edits. Producing a
string initially makes an independent snapshot so later builder edits cannot change
it. Ownership-transfer optimizations, exact APIs and array-field/mutable-receiver
contracts remain deferred. Do not add a second built-in mutable-string type.

`rune`, scalar string indexing and half-open/inclusive scalar slicing are implemented (see
above). String views share the original GC owner without copying UTF-8 bytes;
small views retain the full allocation. Forward scalar string iteration is
implemented; grapheme operations remain deferred.

## Other proposals
- transform this file into GitHub issues
- commands:
  - no cmd: same as build
  - build: output a compiled file, 2 modes debug & release (ReleaseSafe)
  - run: execute a nov file
  - ~repl: run the repl~
  - test: run unit tests
  - fmt: format nov code
    - [x] in-place formatting and read-only --check for multiple files
    - [x] reject unsupported syntax without changing files; validate output preservation
    - expand beyond simple declarations, literals, operators, and comments
    - recursive directory traversal and stdin support
- strings:
  - [x] immutable UTF-8 literals, sharing, concatenation, `+=`, scalar length and output
  - [x] allocation-free, byte-exact `==`/`!=` (including NUL; no normalization)
  - [x] `{expression}` interpolation (including nested calls) for bool/fixed/native integers/rune variables/strings;
    decimal boundaries, UTF-8/NUL, rooted temporaries and formatter preservation
  - blocks/control flow/quoted literals/formats/captures and global interpolation remain unsupported
  - [x] allocation-free O(n) scalar indexing returns exact rune, with int/negative indices and all-mode bounds panics
  - [x] allocation-free O(n) `.len` counts scalars, not UTF-8 bytes or graphemes
  - [x] half-open/inclusive scalar slicing with native int/negative bounds, all-mode panics and allocation-free shared-owner views
  - [x] forward O(n) scalar string iteration (see Loop)
  - ordering, byte-buffer conversions and the UTF-8 builder remain deferred
- add tests, mainly for Parser, IRs, Codegen and Runtime
  - [x] initial parser, formatter, file-safety, and CLI regression coverage
- expand Render.zig to cover the full AST (simple declarations and expressions work)
  - based on std.zig.render from https://github.com/ziglang/zig/pull/21727
- Error type for each step (~Tokenizer, Parser~, IRs, Codegen, Runtime)
- handle SIG.INT correctly, need to write an alternative to isocline in zig
  - [zigline](https://github.com/alimpfard/zigline)?
- [x] render (parser) error with caret under the error + full line info
- add Timer for parsing_time, cod, 2 modes debug & release (ReleaseSafe)egen_time, runnning_time (or use tracy)
- implement correct leaking allocation to have fast exit time?
- add `let _ =` or `_ =` to discard the return value of an expression, it's mandatory to not ignore the returned value if it's != void
- add operator overloading:
  - compilation error when trying to use an operator for values of different types
  - compilation error when trying to use an operator that is not defined for the said value
  - `+` binary op is syntaxic sugar for Object.add, or iadd/fadd for int/float
  - How to handle string * int?
    - use String.repeat() (how to make it comptime?)
  - How to handle int + float? or int + uint? or BigInt + int?
    - have to cast variable e.g. `1.0 + @as(float, 3)`
    - have seemless transition between numbers types, at least for int and uint
    - for BigInt use addScalar
- optimize tail call recursion
- tree shaking
- constant folding
- write nov website with nov as backend? idk
- add cache either in cwd or in $XDG_CACHE_HOME/nov/... (ofc finding the dir is
  handled by known-folders) like \_\_pycache__
- add a way to have better OOP, interface, traits etc idk I hate OOP but it
  sometimes stuff sucks without correct OOP
- add interfaces? (no)
- autodoc with doc comments `;;;`
- package system
- [compile time pseudo variables](https://docs.vlang.io/conditional-compilation.html#compile-time-pseudo-variables)?
  - like builtin module in zig
- formal grammar definition
  - https://craftinginterpreters.com/appendix-i.html
  - https://github.com/ziglang/zig-spec/blob/master/grammar/grammar.peg
  - https://doc.rust-lang.org/stable/reference/introduction.html
- IO API:
  - like zig: thin wrapper around read()/write() with optional buffering
  - same as C
  - buffering and async by default? (maybe customizable with compiler option)
- How to handle arithmetic overflow? current behaviour is to wrap,
  we don't want crash on arithmetic overflow
- add regex pattern as builtin type?
- [Switch Prongs Defined as Comptime-Known Arrays](https://github.com/ziglang/zig/issues/21507)
- dependant types?
- In std provide a simple way to create Struct of Arrays
  - `soa[.field][i]` and `soa[i]` with combining results
  - `&soa[i]` not possible
- Add in scope closure so `xxx.orelse(return 0)` could smh work?
- Look at [gleam](https://gleam.run) parser because they have no ; and cool expr
- Look at https://tour.gleam.run/data-types/bit-arrays/ idk if it's useful in Nov
- Proposal: `pub let` as shorter syntax for `@[public]` on declarations/methods.
  Keep private-by-default and identical file/named import visibility; this is syntax
  only, not implemented or approved as a replacement. Foreign symbol export remains
  a separate `@[export("symbol_name")]` annotation, not Nov import visibility.
- change attributes as a special anonymous struct?
  - from
```nov
@[public]
@[packed]
let myStruct = struct {}
```
  - to
```nov
@[ .visibility = .public, .layout = .packed ]
let myStruct = struct {}
```
- Support [APE](https://justine.lol/ape.html) & [Cosmopolitan](https://github.com/jart/cosmopolitan)? / Use it by default?
- Support casting? (check [this](https://c3-lang.org/language-rules/conversion/))
- type as values
- add `any` similar to zig's `anytype`
- assign destructure like zig
- make slice immutable or find a way to differentiate ref from copy...
- embed tcc into the compilator? (revive the repl proposal)
- arbitrary sized integers?
- support relative import e.g. `@import("github.com/nov-lang/idk_lib")`
- support circular imports
- variable number of parameters in a function?
  - implem `any` instead which works like zig's `anytype`
- support C variadic functions?
- add a way to convert a int to enum and enum to int.
- add a way to create an enum from a string.
- `opaque` container...
- add a [cons](https://en.wikipedia.org/wiki/Cons) operator instead of doing
  `+ [x]` to append a single element to an array?
- I think that we can actually allow parenthesis for grouping since function is
  now a type so it shouldn't make any issue in the parser
  - test this after implementing basic IR/CodeGen
  - note that this conflicts with one of the closure proposal
- Move `@print` etc... to stdlib? (either need to wrap args in an anonymous
  struct or allow to pass a variadic amount of parameters to a function).
- Make `*` always `*mut`? What are the positives and negatives of this change?
- Future format-option proposal (expression interpolation is implemented):
  `"{varname:[fill][alignment][width][.precision]:[custom options]}"`
  and use format like zig instead of toString?
- **Rework Parser to remove newline if they are unnecessary**
  - idk about render but this will simplify other steps a lot
- provide a way to duplicate a function based on parameters?
  - useful when an argument can be comptime or runtime
- Add `unroll` keyword to `match` and `for`, works like `inline` in zig
- Eliminate hidden pass-by-reference from the codebase, i.e. use \*const more ofth for params
- Make type optional (default to `any`) for function param?
- Remove c_int, c_long, ...  from language and add them to std.c smh instead.
- Cast with int(), u8(), etc...?
  - overloadable functions: `str()` and `len()`, also `next()`...

# proposal: add Traits or something similar
Currently many proposals are about using arbitrary declaration for overloading
behavior like `.toString()` overwrite the default format, `.len` override idk
what, `.next()` implem an iterator, etc...

I think it's a better idea to uniformize all of that with specific description
like traits which should make the language easier to understand and read.

Add a way to tell that a container implement a certain behavior, maybe
`@[impl(Iterator)]`, need a way to describe behavior.

https://github.com/ziglang/zig/issues/1268

# proposal: Annotations for container fields aka Tags
add [tags](https://github.com/Hejsil/zig-clap/issues/8#issuecomment-381637825)
to container fields, this is a must have (e.g. for arg parsing or json parsing)

this can allow to specify if a field is public/private or if it's constant/mutable
```nov
let User = struct {
    @[key]
    @[auto]
    id: Id,

    name: string,

    @[unique]
    email: string,

    todos: []Todo,

    moto: Option(string),
}

let Todo = struct {
    @[key]
    @[auto]
    id: Id,

    @[index]
    user_id: Id(User),

    @[relation(key = user_id, references = id)]
    user: User,

    title: string,
}
```

# Closures, anonymous functions & bind

Implemented: `(value: int) -> int { ... }` and `() { ... }` function expressions,
inferred function bindings, nonrecursive local definitions, higher-order calls and
escaping automatic captures. Immutable bindings/parameters snapshot by value
(independent arrays, shared immutable strings); mutable bindings share GC-managed
cells. Function copies/arguments share environments, without recopying captures.
Transitive captures, generic methods, source-free IR and forced-GC/OOM paths are
covered. See README and examples/closures.nov.

Borrowed binding captures, recursive local definitions, inferred parameter/result
types, function fields/array elements and C callback ABI remain deferred. Calls
while a shared captured cell is borrowed are conservatively rejected, as are
calls during iteration over an already captured mutable array; precise effect
analysis and concurrent shared-state rules remain future work.

Historical alternatives below are not executable contracts. Explicit C++-style
`[]` capture syntax was rejected: brackets remain construction syntax. Automatic
capture by binding mutability is the approved rule; `|x|` shorthand and bind
operators below remain unapproved proposals.

```nov
let forEach: (a: *mut []int, f: (int) -> int) = {
    for i in 0..a.len {
        a[i] = f(a[i])
    }
}
let a = [1, 2, 3]
; we need to specify the name of the argument but that's it (?)
; no need to specify the return type since it's inferred, no need for ->
; no need to specify the type of params sicne it's inferred
; body can be an expr since || delimit it
;
; so we have 2 types of functions, classic functions and instant function that
; needs to name its parameter but everything else is inferred from type,
forEach(a, |x| x + 1)

; similar to rust
let add = |a: int, b: int| -> int { a + b }
let add = |a, b| a + b

let x = [1, 2, 3].map(|x| x + 1).filter(|x| x % 2).sum()

; similar to c++
; []: capture list
; (): arguments
; {}: body
let add = [](a: int, b: int) { a + b }

; proposal: allow for below syntax so there is no difference between a function and a closure
; body block is necessary if we specify type
; pretty hard to parse if we accept grouped expr!
let add = (a: int, b: int) -> int { a + b }

; closure / anon/instant function
; proposal
let plusF: (x: *mut int, f: () -> int) = {
    x.* += f()
}
let n = 3
plusF(&n, () 3)
plusF(&n, () {
    let x = 120
    let y = 300
    return {x * x} / y - 3
})
; if f actually took an argument, f: (int) -> int
plusF(&n, (n) n)
plusF(&n, (n) {
    let x = 120
    let y = 300
    return {x * x} / y - n
})
; we can specify all infos like a function type
; body block is necessary if we specify type
plusF(&n, (n: int) -> void {n}

; or closure like
(arg) -> body

; bind
let x = Option.none[]
    >>= (arg) -> {body}
```

# Union
Problem: invalidating an interior pointer
```nov
@[public]
let main: () -> !void = {
    let Value = union {
        s: string,
        n: uint,
    }
    let mut value = Value[ .n = 42 ]
    let number = &value.n
    value = Value[ .s = "hello world" ]
    number.* -= 42
    ; outputs memory garbage
    @println(value.s)
    @println(number.*)
}
```
Solutions?:
- Disallow reassignment on union
  - In what situation is this an issue?
  - This means that we only need to alloc for the tag + size of the field we
    use instead of tag + max size of fields thus saving memory

# If
Add catch syntax for unions?
```nov
if expr |val| {
    ...
} else |err| {
    ...
}
```

# Arrays

Approved and delivered first slice: growable scalar `[]T` value arrays with
pointer-sized uint len/cap, native int negative indexing, independent copies at
value boundaries, literals (including context-typed empty arrays), read-only
`.len`, mutable element replacement and self-safe `+=` append. Formatting now
covers scalar array annotations, literals, indexing, `.len` reads and direct
indexed replacement and integer element `+=`, `-=`, `*=`, `/=`, `%=`, with
token/comment/AST preservation and first-pass idempotence. Integer element compounds
now execute using one pre-index array snapshot and one index evaluation, reading
the old element before the RHS and replacing the local with the updated snapshot
afterwards. Plain replacement retains its final-update bounds check; compounds
check on the pre-RHS read. No new memory forms are introduced. See README's
executable subset for precise limits and evaluation order. The proposals below
remain proposals; writable length, slices, nested/pointer elements, aliases,
capacity APIs and efficient copy/append optimizations are still deferred.

Scalar `value in array` membership is delivered for bool and supported fixed/native
integers with exact element types, bidirectional literal context and no numeric
defaults. It returns bool, evaluates left then right once, preserves array copy
rules, and scans length with typed equality and early exit without allocating.
Formatter support is syntactic only; ranges remain rejected. String/nested/pointer
membership, conversions and iterators remain outside this slice.

Side note about mutability. A constant array cannot be modified in any way, its
values are constant too. A mutable array can be reassigned/extanded and its
values can be modified. TODO: talk about * and *mut array.

Arrays don't have methods? (only a .len field)

TODO:
- keep arrays like that aka growable size list etc... or move to static array like zig?
  - probably continue like that because it's garbage collected and easier for programmer ig
- add a way to initialize the capacity of an array without adding any element
  - `let x = []int.initCapacity(50)` x is an array with len = 0 but with capacity = 50
  - use a builtin?
  - `let x: [50]int`
  - `let x = []int{0}.repeat(50)`
- add a way to repeat an array like a string, allow to use that as an initializer
  - `let x = [1].repeat(50)` x is an array of 50 int with value 1
  - `let x = [@as(uint, 1)].repeat(50)` x is an array of 50 uint with value 1
  - `let x: []uint = [1].repeat(50)` x is an array of 50 uint with value 1

Approved executable storage contracts supersede the earlier shared-layout proposal:
arrays have independent scalar backing with native `uint` len/cap; strings share
immutable UTF-8 byte backing and no exposed capacity; public `.len` scans for the
native `uint` Unicode scalar count, not the internal byte length.
See README and src/runtime/README.md for the implemented root/ownership model.

See [1](https://docs.vlang.io/v-types.html#array-methods)
[2](https://docs.vlang.io/v-types.html#array-method-chaining)
[3](https://tour.gleam.run/standard-library/list-module/)

```
; experimentation
; [1,2,3].map(a => a + 1).filter(b => b != 3)
[1,2,3].map(|a| -> a + 1).filter(|b| -> b != 3)

; [1,2,3] >>= \a -> [a+1] >>= \b -> if b == 3 then [] else [b]
;
; do
;   a <- [1,2,3]
;   b <- [a+1]
;   if b == 3 then [] else [b]
[1,2,3] >>= |a| -> [a + 1] >>= |b| -> if b == 3 { [] } else { [b] }
```

# Higher-order functions
Highly experimental syntax
```nov
; Traditional imperative loop
let num_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let mut result = 0
for i in 0..num_list.len {
    if num_list[i] % 2 == 0 {
        result += num_list[i] * 10
    }
}

; With Higher-order functions
let result = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    .filter(|n| n % 2 == 0)
    .map(|a| a * 10)
    .reduce(|a, b| a + b)
```

# Slice

String slicing is implemented as immutable shared-owner views (see above).
Array slicing remains deferred: https://docs.vlang.io/v-types.html#array-slices

Array proposal: slicing creates a ref by default unless type attribute states otherwise?

# Map
TODO: https://docs.vlang.io/v-types.html#maps

This will probably be implemented in stdlib instead.

# Match
Delivered bounded executable slice: bool/integer/rune/string literal alternatives,
positional multi-subject patterns, `_` wildcards and first-match selection. A single
canonical bool subject (including aliases) needs coverage of true and false, not
necessarily a final catchall; `|` combines coverage and `_` is universal even in
alternatives. Completely covered boolean prongs diagnose; partial overlaps remain
allowed. Integer/rune/string/multi-subject matches still require a final catchall,
including full integer-domain enumerations. General finite-domain analysis remains
deferred. Unary function/print pipes are also executable; fibo.nov
runs unchanged. Match subjects evaluate once left-to-right; pipes evaluate as
callee-first unary calls. Exact branch types, private scopes and terminal paths
reuse typed branch IR. Captures/enums/complex patterns/full exhaustiveness,
range patterns remain deferred. Bounded range and array/string loops are
implemented separately (see Loop). Identifier interpolation works in branch results.
Proposals below are not a claim of executable support.

TODO: check https://docs.vlang.io/statements-&-expressions.html#match

TODO: check https://tour.gleam.run/everything/#flow-control-case-expressions
- rework how match work and remove catch syntax
  - add [Flow-sensitive typing](https://en.wikipedia.org/wiki/Flow-sensitive_typing)?
    that could solve the issue with `if`

Delivered syntax: match over multiple scalar values with identifier interpolation
(discard assignment in this historical example remains unsupported)
```nov
_ = match x % 3, x % 5 {
    0, 0 => "FizzBuzz"
    0, _ => "Fizz"
    _, 0 => "Buzz"
    _, _ => "{x}" ; use `_` instead of `_, _`?
}
```

proposal: match over array
proposal2: revert to using `,` to separate values and allow match over
arrays/tuples instead of match over multiple values (no, allow both)
```nov
_ = match [1, 2, 3] {
    [] => "Empty array"
    [1] => "Array of just 1"
    [4, ..] => "Array starting with 4"
    [_, _] => "Array of 2 elements"
    _ => "Some other array"
}
```

proposal: match over start/end of string
```nov
let getName: (s: string) -> string = {
    match s {
        ; "Foo".. => TODO idk
        ; .."Bar" => TODO idk
        "Hello, ".. => |name| name ; no this is the full string? how to catch just the end?
        _ => "Unknown"
    }
}
```

# Loop
Approved and implemented: condition/unconditional loops support optional
`: continuation` clauses. Body-side `continue` runs the continuation; `continue`
in the condition or continuation reevaluates the condition directly. `break` and
`return` skip the continuation. Unconditional loops can yield `break value` with
an exact inferred/contextual result type. Conditional loops with `else` now yield
an exactly compatible break or exhaustion result with bidirectional inference.
`else` runs on condition false, including zero iterations, and is skipped by break
or return. It is outside the completed loop's control scope: its break/continue
target an outer loop or diagnose. Terminal exhaustion paths do not contribute a
merge value. Arrays copy at value boundaries; strings share. Conditional loops
without `else` remain void and reject value breaks; unconditional `else` remains
rejected. Bounded ascending `loop i in start..end { ... }` and
`loop i in start..=end { ... }` are implemented with
once-only left-to-right endpoint snapshots, an immutable per-iteration index (or
`_` discard), exact known integer bound types and overflow-free iteration.
Inclusive iteration visits the endpoint once and exits before incrementing it,
including on `continue` at the maximum integer. Only two untyped literal bounds
default: native `uint` when both are nonnegative (including zero), otherwise native
`int`, with checked native fits.
This is not a general literal default or runtime signedness selection. Range loops
without `else` yield void and reject value breaks. Both range forms now support
`else` and exact contextual/bidirectional break/fallback results using the approved
generic loop semantics. Normal exhaustion (including empty/reversed ranges and
inclusive MAX completion/continue) runs fallback once; source break/return skip it.
The fallback is outside the completed loop's control scope and cannot see its
index; bounds retain the parent scope. Arrays copy, strings share rooted storage,
and terminal fallback paths contribute no merge value. See
`examples/range-search.nov`. Explicit range continuations,
unbounded/stepped/descending ranges, general iterators, reference iteration over
ranges, multiple/zip bindings and labels remain rejected.
Reusable bounded range values are implemented (see above); ranges are not
membership containers. See README for syntax.

Array/string iteration is implemented:
`loop item in items` evaluates the iterable once. Array items are immutable
copies using normal element-copy rules; source array mutation and new conflicting
loans are statically rejected. Strings yield scalar `rune` values using a forward
UTF-8 cursor, O(n) for the full traversal; rebinding the immutable string source
is safe and cannot change the captured traversal.

`loop &item in items` accepts only unborrowed local array owners. Its scoped,
nonescaping reference uses a stable owner/index; mutable locals permit element
writes and immutable locals permit only reads. The exclusive whole-array loan
makes the owner unavailable in the body, including readonly iteration. No
reference iteration over strings, ranges, temporaries or references/reborrows.
Normal reference reads still copy values. Both forms preserve exhaustion `else`
(including empty sources), exact valued-break/fallback typing and `continue`
advancement. Break/return skip `else`; without `else`, value breaks reject.
See examples/iteration.nov. Multiple/zip iteration and general
iterator protocols/library-container iteration remain deferred.
`examples/fibo.nov` (unchanged) and `examples/fizzbuzz.nov` now pass semantic and
format checks and execute in default/debug/release modes.

- loop over multiple values with iterators, how? they need to have the same length
- allow backward iteration, this means that indexes are int instead of uint?
  - what about backward iteration on arrays, etc...?
```nov
let arr = [0, 4, 5, 1]
for i in 3..=0 {
    @println(arr[i])
}
```
- allow type annotation? (use that instead of taking *mut/& of item?)
```nov
let arr = [1, 2, 3, 4]
for x: int in arr {
    @println(x)
}
```

## Iterators
Future proposals only: the bounded array/string loops above do not implement a
general iterator protocol or multiple/zip iteration.

- an iterator is any object with a public .next() method that returns an
  Option(T) e.g. `next: (*T) -> ?U` (yes)
  - need a `(*T) -> ?*mut U` (no, \*mut is included in U if needed)
  - It's fine to iterate on an array and an iterator at the same time. It will
    panic if the iterator is null when array still has elelemnts and when
    iterator is not null when array as no more elements.
- use &[], [] and .len operator overloading instead? (no, too complicated and doesn't work for e.g. tokenizer)
  - []: `(*T, int) -> U`
  - &[]: `(*T, int) -> *mut U`
  - this is better than .next() because we know the len and [] op overloading
    is a cool side effect but it's a pain to have separate [] and .len
    overloading doesn't make much sense
- use async with yield for iterators

## Use after realloc / iterator invalidation
Current array iteration prevents source mutation/new conflicting loans statically;
reference iteration exclusively borrows its local array owner. It does not promise
that mutation-during-iteration examples below work. General iterator invalidation
and growable aliases remain future design work.

Historical proposal:
- make it work by definition for all primitives (i.e. everything but iterators)
- for iterator do it like rust aka force immutability and stuff to prevent this from happening
```zig
const std = @import("std");

pub fn main() !void {
    const init_queue = [5]usize{ 0, 1, 2, 3, 4 };
    var queue = try std.ArrayList(usize).initCapacity(std.heap.page_allocator, init_queue.len);
    defer queue.deinit();
    try queue.appendSlice(&init_queue);
    // var i: usize = 0;

    // works fine...
    // const len = queue.items.len;
    // while (i < len) : (i += 1) {
    //     const item = queue.items[i];
    //     try queue.append(item);
    // }

    // infinite loop (expected behavior)
    // while (i < queue.items.len) : (i += 1) {
    //     const item = queue.items[i];
    //     try queue.append(item);
    // }

    // segmentation fault
    // for (queue.items) |*item| {
    //     item.* += 1;
    //     try queue.append(item.*);
    // }
    std.debug.print("{any}\n", .{queue.items});
}
```

# Operator Overloading
TODO:
- `>>=`: `(*T, (*T) -> U) -> U`
- `[]`: `(*T, int) -> U`
  - probably too complex since we should be able to overload it twice with `U` and `*mut U`
- `.?`: unwrap...
- for unary negation `-`, autogen it from `-` operator and `zero` decl?

# C FFI

Implemented: `@[export]` (exact declaration name) and `@[export("symbol_name")]`
(exact explicit C symbol) on ordinary immutable top-level functions. Export is
independent of Nov `@[public]`. The initial ABI admits bool, 8/16/32/64-bit integers,
native int/uint, and void results; unsupported names/signatures and duplicate symbols
diagnose. `nov emit-c --library file.nov` emits a translation unit without a process
main. Separate-TU C callers are tested with -O0/-O3 and strict warnings/UBSan. See
README and examples/exports.nov for the supported contract.

Implemented: bodyless `@[extern]` / `@[extern("symbol_name")]` declarations call C
functions under the same scalar ABI. Extern does not imply public; compatible
aliases share a symbol and incompatible signatures/noreturn contracts diagnose.
Direct/indirect/libc/custom-provider calls and source-free IR/OOM are tested.
examples/extern.nov contains compile commands for its examples/extern.c provider.

Deferred: C-specific integer aliases, variadics, pointers, managed-value boundaries,
foreign callbacks, extra object/library driver flags, generated headers and library
packaging.
The broader examples below remain proposals:
```nov
@[extern("malloc")]
let c_malloc: (uint) -> voidptr

@[extern("malloc")]
let c_malloc: (uint) -> voidptr = {} ; error extern fn can't have a body

@[export("nov_add")]
let add: (a: c_int, b: c_int) -> c_int = a + b
```

# Concurrency
TODO: add async/await/yield

What we want:
- Green threads i.e. lightweight user space threads that can run on multiple Kernel threads
  - this is what go does

Notes from the ignorant:
- Fibers and Async/Await tend to always have function coloring but are lightweight.
- "Goroutines" do not have color but are harder to implement? and heavier because relying "mostly" on OS threads.
- Thread Pool are easy to implement but only use kernel thread and are not incorporated into the language.

## [Fibers](https://en.m.wikipedia.org/wiki/Fiber_(computer_science))
- wren [fibers](https://wren.io/concurrency.html)
- buzz [fibers](https://buzz-lang.dev/guide/fibers.html)
- https://medium.com/the-tech-collective/the-unbelievable-simplicity-of-fibers-3339097948c4
  - https://github.com/roscopeco/fiber-blog-example/tree/main
- zig async...
- https://ayazhafiz.com/articles/23/a-lambda-calculus-with-coroutines-and-heapless-closures

## "Goroutines"
- https://docs.vlang.io/concurrency.html
- go goroutines

## Thread Pool
- zig std.Thread.Pool

## Previous Syntax Proposal
```nov
; async func, can be run normally or asynchronous
; signature: `(int) -^ int -> void
let range: `(n: int) -^ int = {
    loop i in 0..n {
        yield i
    }
}
```

# Attributes
TODO add a good way to make code usable on multiple systems through attributes.

TODO: add an attribute for signal handler and add a way to pass parameters to it
via SA\_SIGINFO (see std.debug.attachSegfaultHandler) `@[signal_handler(signal, args)]`

# Comptime
```nov
; comptime variable?
let mut x = #0 ; no
let mut x: #int = 0 ; probably the best, #int and #uint can be bigger than their runtime equivalent
let mut #x = 0
let #mut x = 0

; change the last two change param to this to match with comptime var
let ArrayList: (#T: type) -> type = { ... }
; instead of
let ArrayList: (T: #type) -> type = { ... }
```

# Notes
- shifting warning: `x >> y` (same for `<<`)
  - error if y is signed
  - warn if y >= @bitSizeOf(x): `x >> ${y} is the same as x >> ${y % @bitSizeOf(x)}`
- Remember to check [value.zig](https://github.com/nov-lang/nov/blob/d8cc0edc95c43461e37b48a72d1e02b2307d278c/src/vm/value.zig) for cool stuff
- Declarations in top level can't be `mut`
- Approved strings (implemented; supersedes earlier platform/termination/mutation proposals):
  - Immutable valid UTF-8 on every platform, including Windows; shared storage at
    assignment and call/return boundaries. `let mut` replaces the binding only.
  - Internal lengths measure UTF-8 bytes; embedded NUL is valid. Public `.len` is
    native `uint` Unicode scalar count, scanned in O(n) without allocation.
  - Literals, `+`, printing, locals, parameters/returns, conditionals and explicitly
    typed immutable literal globals are supported. Concatenation uses rooted GC
    backing; literals use static read-only bytes with a null traced owner.
  - Expression interpolation supports bool, fixed/native decimal integers, rune
    variables and shared strings, preserving UTF-8/NUL with no normalization. Placeholders are
    evaluated left-to-right once, including nested calls and scalar expressions.
        Blocks/control flow/quoted literals/formats/captures remain unsupported.
    Global interpolation is nonliteral and rejected, not eagerly evaluated.
  - Scalar indexing returns rune without allocation; negative indices count scalars
    from the end. Half-open/inclusive scalar slicing returns allocation-free views with shared
    GC ownership and no byte copy; small views retain the full allocation. String
    arrays work with shared immutable backing. Iteration, grapheme operations and
    C-string conversion remain deferred. No terminator or compile-time concatenation promise.
- `void` is Nov's unit type (`()` in Rust and OCaml, `Nil` in Gleam)
  - `void` only value is `[]` (`{}` in zig)
- about variable initialization:
  - initialize to zero by default if there is no initializer
  - no initialization if there is no initializer
  - require an initializer (need `undefined` keyword?)
- since Nov has first-class functions, how can we return a function?
  - do just like zig aka comptime function generation is possible e.g. with a
    generic or by making a function with comptime parameters and returning it
  - put it on the stack/heap? need executable memory which is bad, also idk how gc will handle that
  - look at how ocaml or rust does it
- comptime assertion like `let _ = #{}`?
- move generic to attributes? `@[generic(T)]`

# Stuff to check & links
- check https://docs.vlang.io/functions-2.html#anonymous-&-higher-order-functions + closures
- Check std.zig.AstGen, std.zig.Zir and zig/src/Sema.zig for IR
- Check zig/src/InternPool.zig for storing variables
- Check std.zig.start.callMainWithArgs
- Defunctionalization
  - 3591260.pdf
  - https://blog.sigplan.org/2019/12/30/defunctionalization-everybody-does-it-nobody-talks-about-it/
- about compiler codegen optimization https://github.com/vnmakarov/mir/blob/master/mir-gen.c#L26-L47
- following to @dupe/@clone https://jvns.ca/blog/2024/08/06/go-structs-copied-on-assignment/
- https://go.dev/blog/slices-intro
- Go design
  - [What are the NO's in GO design?](https://kuree.gitbooks.io/the-go-programming-language-report/content/32/text.html)
  - https://www.bluxte.net/musings/2018/04/10/go-good-bad-ugly/
- Check [SSA](https://en.wikipedia.org/wiki/Static_single-assignment_form) and
        [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style)
- Build system / project configuration: https://c3-lang.org/build-your-project/project-config/
- [roc-lang](https://www.roc-lang.org/examples/FizzBuzz/README.html) function pipes usage (instead of monads)
- [ML functions](https://en.wikipedia.org/wiki/Standard_ML#Language)
- 14-Semantic-Analysis.pdf
- https://stackoverflow.com/questions/44965/what-is-a-monad
- https://en.wikipedia.org/wiki/Algebraic_data_type
- https://en.wikipedia.org/wiki/Coroutine
- https://doc.rust-lang.org/book/ch13-01-closures.html
- Result
  - https://docs.vlang.io/type-declarations.html#custom-error-types
  - https://docs.vlang.io/type-declarations.html#optionresult-types-and-error-handling
  - https://doc.rust-lang.org/std/result/index.html
  - https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- Change Parser.parseExprPrecedence? https://www.scattered-thoughts.net/writing/better-operator-precedence/
- https://www.scattered-thoughts.net/writing/notes-on-compiler-irs/
- Builtins
  - https://docs.vlang.io/conditional-compilation.html
  - https://docs.python.org/3.12/library/functions.html
- https://en.wikipedia.org/wiki/Clean_(programming_language)#Examples
