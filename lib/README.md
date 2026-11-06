# Nov standard library foundation

Import the standard namespace and alias its ordinary union templates and helper
namespaces:

```nov
let std = @import("std")
let Result = std.Result
let Option = std.Option
let array = std.array
let text = std.string
```

`std.nov` is the entry module. It publicly re-exports `Result` from `result.nov`,
`Option` from `option.nov`, the `array` namespace from `array.nov`, and the
`string` namespace from `string.nov`. These are
ordinary library declarations, not compiler builtins or automatically injected
bare names.

## Result

`Result(T, E)` has variants `ok: T` and `err: E`.

- `value.isOk()` / `value.isErr()` report the active variant as a bool, without
  inspecting or unwrapping its payload. They also work with void payloads.

- `value.map(convert)` takes `(T) -> U` and returns `Result(U, E)`, invoking the
  converter exactly once on success and preserving errors unchanged, including
  void errors. `value.mapErr(convert)` takes `(E) -> F` and returns `Result(T, F)`,
  converting only errors and preserving successes.
- `value.unwrapOr(fallback)` returns the success payload or the supplied fallback.
  The fallback expression is an ordinary argument and always evaluates.
- `value.unwrapOrElse(callback)` returns the success payload or calls the callback
  with the error payload. The callback expression evaluates normally, but its body
  runs only for an error. Both ordinary functions and capturing closures are
  supported.
- `value.!` yields success or propagates an exactly typed error to the enclosing
  Result. This operator recognizes the `ok`/`err` shape, not the library name.

Methods specialize on demand. Void success/error payloads remain valid; a method
whose concrete signature would contain a void parameter cannot be used, following
Nov's existing parameter rules. For example, `Result(void, string)` can be
constructed and propagated without requesting `unwrapOr`.

## Option

`Option(T)` has variants `some: T` and payloadless `none`. Construct it explicitly
with `Option(uint).some[21]`, or use `.some[21]` / `.none[]` under an expected
Option type. Exhaustive matches can capture the `some` payload or handle `none`.

- `value.isSome()` / `value.isNone()` report the active variant as a bool, without
  inspecting or unwrapping its payload. `some[false]` is still present; these methods
  work with void and record payloads too.

- `value.map(convert)` takes `(T) -> U` and returns `Option(U)`: some invokes the
  converter exactly once, while none stays none without invoking it.
- `value.unwrapOr(fallback)` returns the payload or the supplied fallback. As an
  ordinary argument, the fallback expression always evaluates.
- `value.unwrapOrElse(callback)` returns the payload or invokes a zero-argument
  `() -> T` callback for `none` only. Evaluating the callback value is not deferred.
- `Option(void)` can be constructed and matched. `unwrapOr` cannot be requested
  for it because void parameters are disallowed; `unwrapOrElse: (() -> void)` works.
- `value.!` evaluates once, yields the `some` payload, or immediately returns
  `.none[]` from the enclosing Option-returning function. The enclosing success
  payload type may differ. The operator structurally requires exactly `some` and
  `none` variants with a void `none` payload; it does not depend on the Option name.
  Result-to-Option and Option-to-Result propagation are rejected.

See `examples/options.nov`: division by zero yields `none` rather than panicking,
`.!` propagates it from `Option(uint)` into `Option(string)`, and the fallback runs
only for the absent quotient.

## Array payloads

`Option([]T)` and `Result([]T, E)` are supported, as are array-valued error
payloads. Nested arrays and arrays of any existing storable element type are
valid within finite acyclic layouts. Constructors supply an ordinary array value:

```nov
let values: std.Option([]int) = .some[[1, 2]]
let doubled = values.map((items) { std.array.map(items, (x) { x * 2 }) })
let found: std.Option([]int) = std.array.find([[1], [2]], (items) { items[0] == 2 })
```

Normal assignments, calls, returns, match captures and `.!` preserve recursively
independent array backing. Only the active payload is cloned; inactive payload
fields remain zeroed. Immutable strings share backing, and closure elements share
their capture environments. Arrays do not grant aliases to the union's storage.
Direct nested-union, function, range and reference payloads remain unsupported;
recursive layouts, including cycles through arrays, reject. See
`examples/union-arrays.nov` for a runnable example and six tests.

## Container mapping

```nov
let number: std.Option(int) = .some[21]
let doubled = number.map((value) { value * 2 })
let result: std.Result(int, string) = .ok[21]
let text = result.map((value) { "value:{value}" })
let error = result.mapErr((message) { "error:{message}" })
```

These are ordinary generic methods, also callable statically as
`Option(int).map(number, callback)` and `Result(int, string).map(result, callback)`.
The receiver is evaluated/snapshotted before the callback expression; callback
construction is eager even when its body will be skipped. Inferred closures,
ordinary functions, exact expected results and supported generic callbacks use
the existing bounded solver. No implicit conversion, reference escape or catching
of callback panics/errors is introduced. Normal recursive copies preserve array
independence through record payloads; copied closures share their captured cells.

### Chaining fallible operations

`Option(T).andThen(next: (T) -> Option(U)) -> Option(U)` calls `next` only for
some, otherwise returning none. `Result(T, E).andThen(next: (T) -> Result(U, E))`
returns `Result(U, E)`, forwarding an existing error unchanged or returning the
callback's success/error. This flattens a container-returning callback rather
than wrapping it again. The Result error type must be identical, without implicit
conversion. Callbacks may return void success payloads, including `Option(void)`
and `Result(void, E)`; a void Result error is also preserved. Input `T` cannot be
void under the existing function-parameter rules. Eager callback construction,
receiver snapshot order, independent copies and uncaught panics match `map`.

Mapped types must be supported union payloads. Currently the mapping methods require
nonvoid callback parameters and nonvoid mapped results; use a match when the
mapped result is void. `Result.mapErr` also requires nonvoid success payloads,
because its ordinary reconstruction uses a payload expression while void variants
require `[]`. `Result.map` can preserve a void error using existing propagation.
Unused methods remain demand-specialized, so other methods on containers with
void payloads still work as documented above. See `examples/container-map.nov`
for a runnable example and six tests, including chaining and void outputs.

## Array helpers

`array.nov` implements eight public generic functions.
Signatures here use `T`/`U` for inferred backtick type variables:

- `array.map(items: []T, convert: (T) -> U) -> []U` converts every element.
- `array.filter(items: []T, predicate: (T) -> bool) -> []T` keeps matching elements
  in stable source order.
- `array.fold(items: []T, initial: U, combine: (U, T) -> U) -> U` is a left fold
  starting with `initial`, passing the accumulator before the element.

- `array.any(items: []T, predicate: (T) -> bool) -> bool` stops at the first true
  predicate; `array.all` has the same signature and stops at the first false one.
- `array.find(items: []T, predicate: (T) -> bool) -> Option(T)` returns the first
  matching element by value.
- `array.findIndex(items: []T, predicate: (T) -> bool) -> Option(uint)` returns its
  zero-based, pointer-sized unsigned index.
- `array.each(items: []T, action: (T) -> void) -> void` visits all elements without
  constructing an output array. Callbacks must return void.

Callbacks run once per visited element in source order. Empty input invokes no
callback: map/filter return empty arrays, fold returns `initial`, any returns
false, all returns true, and both searches return `.none[]`. Searches also return
none if every predicate is false. Short-circuiting skips later callback bodies,
not argument expressions: callee then arguments evaluate eagerly in ordinary
left-to-right order, including callback construction and the seed.

`find` supports the existing `Option` payload types: bool, integers, floats,
runes, strings, enums, arrays (including nested arrays), and supported acyclic
records, including array fields. Direct unions, functions and ranges cannot be
union payloads yet; use
`findIndex` to search those arrays. Other helpers accept all supported element
types. The returned `Option` is the same canonical type as `std.Option`.

```nov
let found = array.find([1, 2, 3], (x) { x > 1 })
let hasEven = array.any([1, 2, 3], (x) { x % 2 == 0 })
array.each([1, 2, 3], (x) { @println(x) })
```

See `examples/array-search.nov` for a runnable example and seven tests.
Ordinary functions and capturing closures are supported, as are inferred generic
closures/local generic functions within the bounded solver. Typed inputs, seeds,
callback signatures, expected results and supported body constraints take precedence
over numeric literal fallback. Otherwise unconstrained integer literals default to
signed pointer-sized `int` and float literals to native `float` (`f32` on 32-bit,
`f64` on 64-bit targets), including in nonempty and nested array arguments; empty
arrays still need an element type. `f32`/`f64` inputs, seeds, callback signatures
and inferred generic specializations are supported. Integer literals can be checked
directly in float contexts, but already typed integers and floats require exact
matches: no implicit widening, signedness/float-width conversion or numeric casts.
`@as` supplies context, not conversion. Remaining ambiguity is an error. See the
root README's [float contract](../README.md#floats) and `examples/floats.nov` for
IEEE arithmetic and round-trip-safe (not shortest) formatting.

```nov
let std = @import("std")
let array = std.array

let main: () = {
    let items: []int = [1, 2, 3, 4]
    let mapped = array.map(items, (value: int) -> int { value * 2 })
    let filtered = array.filter(mapped, (value: int) -> bool { value > 4 })
    let folded = array.fold(
        filtered,
        @as(int, 0),
        (sum: int, value: int) -> int { sum + value }
    )
    @println("Mapped:", mapped[0], mapped[1], mapped[2], mapped[3])
    @println("Filtered:", filtered[0], filtered[1])
    @println("Folded:", folded)
}
```

```text
Mapped: 2 4 6 8
Filtered: 6 8
Folded: 14
```

Normal by-value semantics apply: nested arrays, structs and active union payloads
have recursively independent array backing; immutable strings share. Copies of
function/closure values share their existing environments and mutable capture
cells. No aliasing pointer mutation is introduced. Callback failures and panics
propagate normally; these helpers do not catch errors or panics.

Map/filter use repeated append and can be quadratic; by-value copies can add
substantial overhead. Further optimization remains **PAUSED**. See
`examples/array-helpers.nov` for the runnable example and six tests covering typed
and generic callbacks, stable order, empty inputs, left folds, recursive copies,
shared closure environments and eager argument order.

## String search

- `std.string.startsWith(text: string, prefix: string) -> bool`
- `std.string.endsWith(text: string, suffix: string) -> bool`
- `std.string.find(text: string, pattern: string) -> Option(uint)`
- `std.string.contains(text: string, pattern: string) -> bool`

Matching is exact and case-sensitive, with no Unicode normalization or grapheme
matching. `find` returns the first match's zero-based **Unicode scalar index**,
not a byte offset; absence returns none. Empty patterns match at index zero,
including an empty input, and empty prefixes/suffixes always match. Oversized
patterns do not match. Embedded NUL is an ordinary scalar. Canonically equivalent
spellings such as precomposed `é` and `e` followed by a combining accent remain
distinct. Pattern and input expressions evaluate once in normal left-to-right
argument order; immutable views retain their existing GC owners.

```nov
let index = std.string.find("é😀z", "z")
@assert(index.unwrapOr(99) == 2)
@assert(std.string.startsWith("é😀z", "é"))
@assert(std.string.contains("a\0b", "\0"))
```

These are ordinary Nov functions implemented using existing scalar indexing,
slices and zip loops. They may repeatedly scan UTF-8; no efficient-search
complexity is promised yet, and optimization remains paused. They do not add
mutable strings, byte conversions, normalization or case folding. See
`examples/string-search.nov` for a runnable example and five tests.

## References and library boundaries

Explicit references are local and call-scoped capabilities, not ordinary
first-class values. Runtime handles lower to typed C pointers; unit-typed IR
`reference_parameter` / `reference_argument` capabilities carry their descriptors.
Internal Nov C prototypes use typed pointers, not a source-level foreign ABI:
`@[extern]` / `@[export]` reference parameters and results remain forbidden.
`let p = &a` infers `&mut T` iff owner `a` is mutable;
otherwise it infers readonly `&T` (not `&const T`). An explicit `let p: &T = &a`
can downgrade a mutable owner's borrow. Binding `mut` is independent: `let p`
cannot rebind but permits `p.*` writes for `&mut T`; `let mut p` allows `p = &b`
with the same exact reference type, retaining any explicit readonly annotation.
Rebinding works across `if`, `match`, short-circuit expressions, nested blocks
and loops, including conditions, continuation and exhaustion paths. CFG proofs
merge possible targets with slot kills and NLL snapshot liveness; conflicting
owner reads, writes or invalidation are blocked for any possible live alias.
Readonly loans remain exclusive. Newly borrowed owner storage must be in the
same or an outer lexical scope relative to the reference binding: rebinding an
outer `p` to an inner owner rejects even when `p` is unused afterward.

A load captures the current runtime target before dereference RHS effects:
rebinding `p` to `b` inside the RHS of `p.* += ...` still updates the old pointee.
The compound keeps that snapshot live; another element of the same array still
conflicts with its whole-owner loan (no element disjointness). Ordinary rebinding
after the last use works. Conservative owner-set checks may overreject
target-sensitive iteration.

There is no autodereference: use `p.*`, `p.*[i]`, `p.*.field`, or `p.*()` to
access the pointee. Library calls such as `array.map(p.*, convert)` pass ordinary
array values under the existing copy rules, not references. Functions, callbacks,
closures, inferred generic functions, methods and pipes may accept `&T` / `&mut T`.
Pass `f(p)`, `f(&local)` or `f(&array[index])` to a compatible reference parameter;
`f(p.*)` passes a value instead. Mutable arguments may weaken to readonly for the
same child type, never the reverse; function/callback types remain invariant.
Formal reference bindings cannot rebind, but mutable-pointee `.*` writes and
forwarding to other compatible reference parameters work. Existing method
receivers remain by value; the array helper signatures above are unchanged.

Each reference argument snapshots its target before later argument effects:
`call(p, later)` passes the old target if `later` rebinds `p` to `&b`.
Caller loans and managed-owner roots
remain live throughout the remaining argument evaluation and call, including
forwarding. Possibly aliased reference arguments reject even when readonly:
`f(p, p)` / `f(&a, &a)` are invalid, and different array indices do not establish
disjointness. Unsafe owner changes in later arguments reject. Conservative
boxed/captured-owner call protection may reject safe calls; this is not
interprocedural effect analysis.

`let q = p`, returning, storing, capturing and reborrowing references remain rejected;
`Result(&T, E)`, `Option(&T)` and arrays/fields of references are not supported.
`loop &item in items` yields a local reference accessed with `item.*`.
Existing bounds checks and rooted stable owners remain in force. Owner roots
retain managed backing for runtime handles and snapshots; interior pointers are
not GC roots. No runtime reference-validity checks are added.

**Accepted roadmap:** passing references is delivered under the call-scoped rules
above. Returning and storing references, ordinary copying, capture and reborrowing
remain **not implemented**. Escape will require lifetime/referent tracking,
GC retention of managed referents and static prevention or safe invalidation of references when storage changes. No
dangling references may be exposed. This does not change today's by-value library
signatures or introduce manual memory management, pointer arithmetic, null or
unsafe dereference. See the root README's [reference contract](../README.md#local-static-borrowing)
and `examples/references.nov`, `examples/reference-rebinding.nov` /
`examples/reference-parameters.nov`; broader pointer
proposals remain separate.

## Loading and installation

`zig build` and `zig build release` install this directory at `<prefix>/lib/nov`;
Zig package sources include it as well. The installed compiler resolves `"std"`
from executable-relative `../lib/nov/std.nov`. Semantic check, emit-c, build, and
run accept `--lib-dir <path>` to override that library root. There is no implicit
working-directory library search or embedded developer-checkout path.

Explicit relative `.nov` imports resolve from the importing file, not the process
working directory. For example, `@import("result.nov")` in `std.nov` refers to this
directory's file. A re-export may select a member directly:
`@[public] let Result = @import("result.nov").Result` (place the attribute on its
own line). This has the same identity and visibility as a separate namespace binding.
Imports yield compile-time namespaces, never runtime values. Bounded
`@TypeOf(lib.value)` can query an imported runtime binding's canonical type while
preserving visibility checks; `@TypeOf(lib)` and queries of imported types or
unspecialized generic templates are rejected. Expressions are supported, including
literals, calls, arithmetic, fields, indexing, string slices, blocks and inferred
generic calls. The operand is typechecked without execution: calls, mutations,
bounds checks, converter invocation and arithmetic panics do not occur. Invalid
operands still diagnose, and unsupported language features remain unsupported.
For a local reference `p`, `@TypeOf(p)` reports its descriptor (`&u8` or `&mut u8`),
while `@TypeOf(p.*)` reports its child type without dereferencing. Queries inspect
lexical metadata without creating loans or introducing runtime dependencies,
captures, boxing, specializations or callee-body demands. Results are compile-time types usable in
annotations, `@as`, generic type arguments and module-level aliases, not first-class
runtime values or local type aliases. Direct print/interpolation renders their
canonical names without calling value stringify hooks. See the root README's
[type-query contract](../README.md#type-queries), `examples/type-of.nov` and
`examples/type-of-expressions.nov`.
Canonical paths share one source and nominal identity across repeated/diamond
imports, including cyclic module imports; actual declaration/type dependency
cycles still diagnose. Declarations and methods are file-private by default;
both named and file imports expose only `@[public]` declarations. Public types do
not automatically expose their methods. Aliases/re-exports and implicit
`@[stringify]` conversion preserve access checks: a marked converter must itself
be `@[public]` for cross-file printing/interpolation, including nested values.
Private or malformed marked converters reject rather than falling back to
structural formatting. The converter may have any name and must be an ordinary
immutable container method with a body and exact `(SelfType) -> string` signature.
At most one argument-free `@[stringify]` is allowed per struct/union/enum; ordinary
methods in generic containers are allowed, but the converter's own generic
function parameters and `@[noreturn]`/`@[test]`/`@[export]`/`@[extern]` are not.
Unmarked `toString` methods are ordinary callable methods with no print effect.
See the root README's [converter contract](../README.md#explicit-stringify-converters).
There is no `@[private]` attribute.

The actual library is tested through real imports in semantic, emit-C, build/run,
formatter, and relocated-install checks. `examples/results.nov` and
`examples/structured-errors.nov` use `std.Result` rather than copied definitions.

Filesystem and other OS-resource APIs remain outside this first library slice.
