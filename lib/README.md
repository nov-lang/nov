# Nov standard library foundation

Import the standard namespace and alias its ordinary union templates and helper
namespaces:

```nov
let std = @import("std")
let Result = std.Result
let Option = std.Option
let array = std.array
let range = std.range
let string = std.string
let Operator = std.builtin.Operator
let mem = std.mem
let fn = std.fn
let math = std.math
```

`std.nov` is the entry module. It publicly re-exports `Result` from `result.nov`,
`Option` from `option.nov`, the `array` namespace from `array.nov`, the
`range` namespace from `range.nov`, the `string` namespace from `string.nov`,
the `mem` namespace from `mem.nov`, the `fn` namespace from `fn.nov`,
the `math` namespace from `math.nov`,
and the `builtin` namespace from
`builtin.nov` (including its public `Operator` enum). These are
ordinary library declarations, not compiler builtins or automatically injected
bare names. String values use the `str` type; the module remains `std.string`.

`std.mem` provides `swap(&mut T, &mut T)`, `replace(&mut T, T) -> T`, and
`take(&mut T) -> T`. `std.fn` provides `identity(T) -> T`,
`const(T) -> () -> T`, `noop()`, `compose((B) -> C, (A) -> B) -> (A) -> C`,
`flip((A, B) -> C) -> (B, A) -> C`, and `neg((T) -> bool) -> (T) -> bool`.
Types are inferred at each call. Ordinary value-copy rules apply; `const`
captures its argument by value, while copied closures share their captured cells.
`compose(f, g)(x)` calls `g(x)` then `f` on its result, once each.
`neg(predicate)(x)` calls the predicate once and negates its boolean result.
Neither helper invokes its callbacks during construction.
`replace(&value, replacement)` installs the replacement and returns the old value.
The destination must be mutable and the replacement must match its type exactly.
Normal recursive value-copy rules apply to both the returned and stored values;
function copies retain shared captured cells. Arguments evaluate once, destination
before replacement; an unevaluated `@TypeOf` query neither borrows nor mutates.
See [`examples/mem.nov`](../examples/mem.nov) (eight tests) and
[`examples/fn.nov`](../examples/fn.nov) (seven tests) for usage.
`fn` is an ordinary identifier, not a reserved keyword.
Run `zig build test-cli-mem-fn` for the migration CLI coverage.

`take(&value)` returns the old value and installs its inferred type's default.
Its source is the ordinary wrapper ``replace(dst, @default(`T))``: the destination
evaluates once, then the default source evaluates before `replace` reads the old
value or stores the replacement. Factory effects happen before replacement, and
a factory panic never reaches the store. The destination must be mutable and its
type must support `@default`. This is not a move: recursive array/record copies
have independent backing; closure copies still share captured cells.

`@default(T)` supplies `false`, numeric zero, NUL rune, empty string, exact empty
arrays (even if their elements have no default), and empty exclusive integer
ranges `0..0`. Struct fallback uses existing declared defaults for every field;
it does not zero or recursively default missing fields. Nominal structs/enums/
unions may select one argument-free `@[default]` ordinary immutable non-generic
method with a body and exact `() -> Owner` signature. Generic containers may use
concrete owner aliases/`@This()`; the method itself must not be generic. The factory
overrides struct defaults and supplies enum/union defaults. Cross-module calls
require `@[public]`; inaccessible factories do not fall back. Duplicate factories,
invalid signatures/placements, annotation arguments and conflicts with test,
noreturn, extern, export, stringify or operator annotations reject even when
unused. Factories may allocate, mutate or panic like ordinary calls.

Readonly `@TypeOf(@default(T))` validates without demanding the factory body,
running effects or publishing specializations. Global initializers support only
compile-time-known builtin/declared struct data defaults; any reached custom
factory call (including Option's) must instead run in `main` or a function called
from it. `void`, functions, references and metatypes have no default; `std.Result`
has no automatic default. See [Default values](../README.md#default-values) and
[`examples/defaults.nov`](../examples/defaults.nov), which contains seven tests.

Future direction only: compiler-recognized protocols may be replaced/generalized
by a Rust-like trait system, including user-defined annotations. No trait syntax
or implementation is approved; user-defined annotations are not supported now.

## Math helpers

``std.math.abs(value: `T) -> `T`` is an ordinary generic Nov function accepting
only native `int`, `i8`/`i16`/`i32`/`i64`, and `f32`/`f64`, preserving the exact
input type. Unsigned integers reject through the existing unary-negation rule;
bools, runes, strings and nominal types reject through numeric-zero context.
Zero returns positive zero (including `-0.0`); negative values are negated,
and a still-negative result panics with `absolute value overflow`. Thus every
signed integer minimum panics under the existing wrapping negation contract.
Negative infinity becomes positive infinity, positive infinity is unchanged,
and NaN propagates. The argument evaluates eagerly exactly once; `@TypeOf`
executes neither argument effects nor overflow panics. Generic aliases and typed
callbacks use ordinary function semantics. No builtin or reflection is added,
and numeric execution needs no managed allocation. See
[`examples/math-abs.nov`](../examples/math-abs.nov) (four tests),
`zig build test-cli-math-abs` (all modes, relocated imports, check/emit-c rejection
fixtures and all signed-minimum panics), and the source-free O0/O3 backend test
with forced allocation failure.

``std.math.clamp(value: `T, lower: `T, upper: `T) -> `T`` is an ordinary Nov
function requiring one exact ordered type (builtin integers/floats or accessible
nominal less-than hooks). It first checks `lower <= upper`, otherwise panicking
with `invalid clamp bounds`. It then returns `lower` if `value < lower`, `upper`
if `upper < value`, and otherwise the original `value`. Bounds are inclusive;
equal bounds and infinities are supported. Builtin float `<=` is IEEE, so either
NaN bound panics, while a NaN input is retained with valid bounds. In-range signed
zero retains the input's sign, including equal zero bounds. Nominal `<=` derives
`!(upper < lower)` and requires only ordering, not an equality hook.

Arguments evaluate once, eagerly left-to-right, before bounds validation or value
comparisons. Ordinary argument snapshots and recursive independent managed-result
storage apply; copied callbacks still share captured cells. `@TypeOf` queries
execute neither arguments, comparisons nor panics. No new builtin or ordering rule
is introduced. See
[`examples/math-clamp.nov`](../examples/math-clamp.nov) (six tests) and
`zig build test-cli-math-clamp` for all three modes, relocated imports, rejection
fixtures and invalid-bound panics.

## Equality and operator metadata

Arrays compare lengths and then elements recursively. Records compare fields,
tagged unions compare active tags and payloads, and enums compare tags, with
exact nominal identity for named types (including generic arguments). Anonymous
records require the same exact structural type. `!=` negates the same equality
operation; `value in array` uses it for each candidate, stopping at the first
match. Strings compare bytes/length, including NUL; floats use IEEE equality
(NaN unequal even to itself, signed zeroes equal), also when nested.
Functions/closures, references and ranges have no default equality. Direct
`==` / `!=` also support exact mixed-type hooks; membership still requires the
needle's exact element type. Same-type structural equality remains unless an
exact `(Self, Self)` hook replaces it; unrelated mixed equality hooks do not
block that fallback.

A nominal struct, union or enum can declare an ordinary immutable non-generic
method with a body marked `@[operator(.equal)]`, typed exactly `(A, B) -> bool`.
At least one parameter must be the exact owner type, including concrete generic
arguments; generic containers may use `@This()`. An exact self/self hook applies
recursively to nested values and membership, and can stop traversal before
otherwise noncomparable contents. Imported matching hooks need `@[public]`:
a matching private hook diagnoses rather than being ignored. Invalid concrete
signatures and inaccessible matching nested hooks diagnose rather than falling
back. Within an owner, one hook per operator and ordered parameter signature is
allowed; return types cannot distinguish duplicates. Method names are arbitrary
but must be distinct because ordinary function names are not overloaded. The
attribute takes one literal tag, cannot repeat, and excludes test/noreturn/extern/export.
Enum match patterns always use raw tag identity, not the hook.

`@[operator(std.builtin.Operator.equal)]` is the qualified form. It must resolve
to the canonical enum exposed by `std.builtin`; another enum with an `equal` tag
is not interchangeable. `Operator` declares exactly `equal`, `less_than`, `add`,
`sub`, `mul`, `div`, `rem`, `try`, `deref`, `deref_mut`, `index`, `index_mut`, `neg`,
`bit_and`, `bit_or`, `bit_xor`, `shl`, `shr`.

Mixed operand/result hooks are supported: `equal`/`less_than` take `(A, B)` and
return `bool`; `add`/`sub`/`mul`/`div`/`rem` and `bit_and`/`bit_or`/`bit_xor` take
`(A, B)` and return any ordinary non-reference type, including `void`. Unary
`neg(Self)` allows the same return types. These use the same method, canonical-tag
and visibility rules as equality, with at least one exact owner parameter.
Reference operands and returns are unsupported in this batch; lifetime support
is deferred. Invalid concrete signatures reject even when unused.

Binary lookup searches both nominal operand owners for exact ordered parameter
matches, selecting a unique hook or diagnosing ambiguity without an owner
preference. No implicit conversions are used. Untyped literals receive unique
hook parameter context where possible; expected return types never dispatch.

Only `less_than` is needed: `a > b` uses `less_than(b, a)`, `a <= b` uses
`!less_than(b, a)`, and `a >= b` uses `!less_than(a, b)`, with no independent tags.
Mixed `>` and `<=` therefore look up the reversed `(B, A)` signature; `>=` uses
the normal `(A, B)` signature, negated. Source operands still evaluate and are
snapshotted once left to right before reversal.
This is boolean-negation derivation, not floating-point partial-order semantics;
hook authors define the order, and there is no default structural ordering.
`+=`, `-=`, `*=`, `/=` and `%=` derive from binary hooks whose result must exactly
match the LHS type: `Vector *= float` may use `mul(Vector, float) -> Vector`, but
`mul(Vec, Vec) -> float` cannot implement `Vec *= Vec`. Compounds preserve pre-RHS
LHS/target snapshots like builtins; their final store may overwrite RHS
reassignment. Operands evaluate once, left to right. Hooks are ordinary calls
that may allocate, mutate or panic; readonly `@TypeOf` validates without running
hooks or publishing specializations. Direct mixed equality and exact-element-type
membership follow the rules above.

`shl`, `shr`, `try`, `deref`, `deref_mut`, `index` and `index_mut` remain reserved
and diagnose unsupported overloads; no shifts are implemented. There is no
separate inequality hook. See [`examples/operators.nov`](../examples/operators.nov)
for seven tests covering a scalar `Number`, generics, compound targets,
evaluation/snapshots, and enum/union hooks. See
[`examples/mixed-operators.nov`](../examples/mixed-operators.nov) for mixed operators.
See [Value equality](../README.md#value-equality) and
[Operator Overloading](../README.md#operator-overloading) for the full contract.

## Storable struct fields

Struct constructors use `Processor.[run = callback, outcome = .ok[[1]], interval = 0..10]`
or contextual `.[field = value]` under an expected struct type. Nominal shorthand
`Type.[field]` and contextual `let p: Type = .[field]` read same-named scoped
values without dots; old dotted shorthand `Type.[.field]` is rejected.
Inferred all-bare `.[x, y]` is positional, while an explicit assignment plus bare
names is named. Contextual named entries map to the expected layout independent
of source order. Old `Type[.field = value]` / `Type[]` source
constructors are rejected. Anonymous named `.[a = 1]` and positional `.[1, "a"]`
values work with inferred generic helpers such as `std.array.map`. Their structural
identity uses ordered names and exact canonical types, including public module
aliases, and is distinct from nominal structs. `.[1, 2]` is not an array; `[1, 2]`
is. Positional fields use `.0`, `.1`, etc., with compile-time position checks and
exact-type writes through mutable owners. Nested paths, callable fields and
explicit `reference.*.0` follow the ordinary field rules. Bracket/dynamic tuple
indexing, destructuring and mixed named/positional construction remain unsupported.
Scalar and array literal defaults, recursive array snapshots and closure-field
sharing follow the ordinary value rules. Anonymous unions/enums remain deferred; `.some[value]`, `.none[]`,
array indexing and slicing keep their existing syntax. See
`examples/anonymous-structs.nov` and `zig build test-cli-anonymous-structs`.

Struct fields may hold every ordinary storable runtime value, including the
standard library's unions, callbacks and ranges:

```nov
let Processor = struct {
    run: (int) -> int,
    outcome: std.Result([]int, str),
    interval: @TypeOf(0..10),
}

let main: () = {
    let processor = Processor.[
        run = (value) { value + 1 },
        outcome = .ok[[1, 2]],
        interval = 0..10,
    ]
    @println(processor.run(4), processor.outcome)
}
```

Ordinary record copies recursively detach arrays, including those in active union
fields. Function fields share their existing closure environments/captured cells;
range fields copy typed bounds and inclusivity. Exact-type field replacement and
existing mutable nested paths apply to these fields too. A field call selects its
callee once before argument expressions and does not insert a receiver parameter;
fields take lookup precedence over methods. Reference-parameter callbacks retain
ordinary explicit borrowing/forwarding rules, without permitting reference fields.

Union- and range-valued fields print recursively with existing converters.
Function fields require an enclosing valid stringify hook to print the whole record.
Defaults support scalar literals and nested array literals, including `[]` for
any storable element type. Nonempty arrays require supported literal leaves;
computed expressions, callbacks and other aggregate constructors are not enabled.
Each construction gets independent recursive backing, and explicit overrides skip
unused defaults. Generic `values: []T = []` works. See `examples/array-defaults.nov`
and `zig build test-cli-array-defaults`. No deferred `#` compile-time arguments
are enabled. Void/type/
namespace/reference fields and recursive type/signature graphs remain unsupported.
See `examples/storable-fields.nov` for a runnable example and six tests.

## Result

`Result(T, E)` has variants `ok: T` and `err: E`. Match payloads with
`.ok[value] => ...` and `.err[error] => ...`; bindings are immutable and local
to their prong. Omit brackets when ignoring a payload. The former pipe-delimited
capture syntax is no longer accepted.

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
Nov's existing parameter rules. For example, `Result(void, str)` can be
constructed and propagated without requesting `unwrapOr`.

## Option

`Option(T)` has variants `some: T` and payloadless `none`. Construct it explicitly
with `Option(uint).some[21]`, or use `.some[21]` / `.none[]` under an expected
Option type. Exhaustive matches use `.some[value] => ...` to bind the payload
and `.none => ...` for absence.

- `Option(T).empty()` is public, marked `@[default]`, and has exact signature
  `() -> Self`, returning `.none[]`. `@default(std.Option(T))` therefore returns
  `none` without requiring a default for `T`, including void/function payloads.
  This is an explicit ordinary library factory, not automatic union defaulting;
  `std.Result` has no automatic default.
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
`.!` propagates it from `Option(uint)` into `Option(str)`, and the fallback runs
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
Function, range and nested union payloads are supported under the same acyclic
rules; references cannot be payloads. Recursive layouts,
including cycles through arrays, reject. See
`examples/union-arrays.nov` for a runnable example and six tests.

## Nested union payloads

Union payloads may themselves be unions, retaining each container's nominal type:

```nov
let wrapped: std.Option(std.Result([]int, str)) = .some[.ok[[1, 2]]]
let nested: std.Result(std.Option(int), str) = .ok[.some[7]]
```

Match captures yield the inner union as an ordinary value. `.!` processes one
layer only, preserving its existing exact error/Option propagation rules. It does
not flatten nested containers or bridge Option and Result automatically. Use
`map` to wrap a container-returning callback in another container, or `andThen`
when the callback already returns the same container family with the required
error type. `std.array.find` now also supports union elements via `Option(Union)`.

Only active tag chains are inspected, copied and formatted; arrays inside them
retain recursively independent backing. Inactive by-value fields remain zeroed.
Malformed active tags panic before allocating copies or invoking formatters.
Default output retains nested union tags and uses exact nested stringify hooks.
Struct fields may hold these unions directly; recursive type/layout graphs and
reference payloads remain unsupported. See `examples/nested-unions.nov` for a runnable example and six tests.

## Function and range payloads

Containers may hold callable values and ranges:

```nov
let Callback = (int) -> int
let Interval = ..uint
let function: std.Option(Callback) = .some[(value: int) { value + 1 }]
let range: std.Option(Interval) = .some[0..=3]
```

Extract a payload with a match or `.!`, then call the function or iterate the
range. Function copies share the existing closure environment and captured cells;
immutable capture snapshots are not recreated. Ranges copy exact typed bounds and
inclusivity by value without allocation. Both work as container `map`/`andThen`
results and `std.array.find` payloads. References are still call-scoped capabilities,
not storable payloads, and closures cannot capture them.

Use the builtin `@len(range)` to count a finite bounded range, including an
extracted payload, without iterating or allocating. It returns native `uint`:
reversed ranges count 0, exclusive ranges count `end - start`, inclusive ranges
add 1. Signed cross-zero and full narrow-domain counts are supported; full
inclusive `u8` counts 256, not 0. Counts exceeding native `uint` panic
`range length overflow` in every mode; known overflowing global initializers
are compile errors. Endpoints evaluate once left-to-right, and stored, passed
or returned ranges preserve their bounds. A generic helper
``let length: (x: `T) -> uint = @len(x)`` works after instantiation for arrays,
strings and finite ranges; no library overload or length protocol is needed.
`@TypeOf(@len(...))` remains unevaluated and yields `uint`. Unbounded ranges,
synthetic `.len` properties and reachable 128-bit backend endpoints are not
supported. See `examples/range-length.nov`.

Ranges print/interpolate as `start..end` or `start..=end` with exact signed/unsigned
numeric bounds, including extrema. Array/record/union printing recurses and honors
nested `@[stringify]` hooks. Functions remain unprintable by default, including
inside a union or array. An enclosing valid `@[stringify]` hook can provide a representation and stop
structural recursion. Even an empty Option of a callable type needs such a hook
for printing. Struct fields may also hold these values directly; recursive structural
signature graphs remain unsupported. See `examples/value-payloads.nov` for a
runnable example and six tests.

## Range helpers

The source type `..T` requires integer endpoints: `@TypeOf(@as(u8, 0)..1)` prints
`..u8`, replacing `range(u8)`. Aliases, ``..`T`` generic signatures, `[]..T`,
references and fields follow existing storage rules. Only finite ranges are
values; open loop followers are unchanged. Range equality/membership remain
unsupported, and no range fields are added.

``std.range.toArray(range: ..`T) -> []`T`` in `range.nov` loops over the range and
appends ordinary value copies:

```nov
let values: []u8 = std.range.toArray(@as(u8, 1)..=3) ; [1, 2, 3]
```

Empty and descending ranges produce empty arrays. Inclusive extrema are safe
under existing iteration rules; this helper adds no new iteration semantics.
See [`examples/range-types.nov`](../examples/range-types.nov) for usage and five tests.

## Container mapping

```nov
let number: std.Option(int) = .some[21]
let doubled = number.map((value) { value * 2 })
let result: std.Result(int, str) = .ok[21]
let text = result.map((value) { "value:{value}" })
let error = result.mapErr((message) { "error:{message}" })
```

These are ordinary generic methods, also callable statically as
`Option(int).map(number, callback)` and `Result(int, str).map(result, callback)`.
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

`array.nov` implements public generic collection helpers.
Run `zig build test-cli-collection-helpers` for join/selection examples, imported
aliases, invalid arguments and relocated-install checks.
Signatures here use `T`/`U` for inferred backtick type variables:

- `array.map(items: []T, convert: (T) -> U) -> []U` converts every element.
- `array.flatMap(items: []T, convert: (T) -> []U) -> []U` concatenates callback
  arrays in source order, flattening exactly one layer. Each input invokes the
  callback once; empty input invokes none, and empty callback arrays add nothing.
  Arguments remain eager, returned storage is independent recursively, and closure
  environments remain shared. Existing repeated-append costs still apply. See
  `examples/array-flat-map.nov` and `zig build test-cli-array-flat-map`.
- `array.reverse(items: []T) -> []T` returns an independent reversed value,
  including recursive array storage. Empty input stays empty; closures retain shared
  environments. It uses ordinary Nov indexing/append, with the existing copy costs
  and no in-place capacity reuse. See `examples/array-reverse.nov` and
  `zig build test-cli-array-reverse`.
- `array.min(items: []T) -> Option(T)` and `array.max(items: []T) -> Option(T)`
  return `.none[]` for empty input, otherwise `.some[@min(items)]` or
  `.some[@max(items)]`. These are ordinary Nov functions, not new builtins.
  `T` must support builtin integer/float ordering or an accessible exact
  `(T, T) -> bool` `@[operator(.less_than)]` hook, even for empty/singleton
  calls. Mixed-only and private imported hooks do not qualify.
  Exact numeric widths/signedness and nominal types are preserved, without widening.
  Comparisons form a stable left fold: first ties and signed zero survive, a leading
  NaN survives, and later NaNs are ignored. Minimum compares `(candidate, best)`;
  maximum compares `(best, candidate)`, once per later element. Empty/singleton
  inputs invoke no hook. Arguments evaluate eagerly once; the by-value input
  snapshot precedes comparisons, so hooks may mutate a captured source owner safely.
  Returned payloads have recursively independent array backing; copied callbacks
  retain shared capture cells. Hook failures propagate normally; `@TypeOf` does not
  evaluate arguments or invoke hooks. See `examples/array-extrema.nov` and
  `zig build test-cli-array-extrema` for executable coverage, including inferred
  callbacks, concrete function aliases and negative semantic/emit-C fixtures.
- `array.filter(items: []T, predicate: (T) -> bool) -> []T` keeps matching elements
  in stable source order.
- `array.fold(items: []T, initial: U, combine: (U, T) -> U) -> U` is a left fold
  starting with `initial`, passing the accumulator before the element.

- `array.any(items: []T, predicate: (T) -> bool) -> bool` stops at the first true
  predicate; `array.all` has the same signature and stops at the first false one.
- `array.find(items: []T, predicate: (T) -> bool) -> Option(T)` returns the first
  matching element by value.
- `array.findLast(items: []T, predicate: (T) -> bool) -> Option(T)` visits elements
  from the end and stops at the first match, returning the last matching value.
- `array.take(items: []T, count: uint) -> []T` returns the first `min(count, @len(items))`
  elements in source order.
- `array.drop(items: []T, count: uint) -> []T` discards the first `min(count, @len(items))`
  elements and returns the remainder in source order.
- `array.findIndex(items: []T, predicate: (T) -> bool) -> Option(uint)` returns its
  zero-based, pointer-sized unsigned index.
- `array.each(items: []T, action: (T) -> void) -> void` visits all elements without
  constructing an output array. Callbacks must return void.

`array.sort(items: []T, less: (T, T) -> bool) -> []T` returns an independent,
stably sorted copy without mutating the source. `less(a, b)` means that `a`
belongs before `b`: use `a < b` for ascending numeric order or `a > b` for
descending order. The comparator is required; there is no default ordering.
Equivalent elements (false in both directions) retain their original order;
provide a consistent strict ordering. All ordinary storable element types work,
including records, nested arrays, unions, functions and ranges, using suitable
comparisons of fields, lengths, computed keys or callback results. Rune ordering
can be supplied with equality-based keys; rune `<` is not currently supported.

Sort is ordinary Nov insertion sort, with O(n²) worst-case comparisons/shifts
and O(n) comparisons on already sorted input; ordinary recursive copies can add
substantial overhead. It visits input left-to-right, comparing the current item
against preceding items from right-to-left until the first false result. Empty
and singleton inputs invoke no comparator. Arguments evaluate once, left-to-right;
the input snapshot is taken before comparator construction or invocation can
mutate captured source storage. Recursive backing is independent, while copied
closure environments remain shared. Comparator failures/panics propagate normally.
No builtin, optimization or mutable receiver is introduced. See
`examples/array-sort.nov` for a runnable example and nine tests. Run:

```sh
nov check --semantic examples/array-sort.nov --lib-dir lib
nov run examples/array-sort.nov --debug --lib-dir lib
nov run examples/array-sort.nov --release --lib-dir lib
nov test examples/array-sort.nov --debug --lib-dir lib
nov test examples/array-sort.nov --release --lib-dir lib
```

For the other helpers, callbacks run once per visited element in source order, except `findLast`, whose
predicate runs in reverse source order and stops immediately on a match.
Empty input invokes no callback: map/filter/take/drop return empty arrays, fold
returns `initial`, any returns false, all returns true, and all searches return
`.none[]`. Searches also return
none if every predicate is false. Short-circuiting skips later callback bodies,
not argument expressions: callee then arguments evaluate eagerly in ordinary
left-to-right order, including callback construction and the seed.

`find` and `findLast` support the existing `Option` payload types: bool, integers, floats,
runes, strings, enums, arrays (including nested arrays), and supported acyclic
records, including array fields, as well as nested unions, functions/closures
and ranges. Except for `min`/`max`, which require ordered elements as described
above, these helpers accept every currently supported array element type. The
returned `Option` is the same canonical type as `std.Option`.

```nov
let found = array.find([1, 2, 3], (x) { x > 1 })
let hasEven = array.any([1, 2, 3], (x) { x % 2 == 0 })
array.each([1, 2, 3], (x) { @println(x) })
```

Zero count makes `take` empty and `drop` a full independent copy. Counts at or
above the input length make `take` a full independent copy and `drop` empty;
even maximum `uint` is accepted without signed conversion or count arithmetic.
Neither helper mutates the source. Returned arrays use ordinary recursive copy
semantics, not shared mutable views; closure environments remain shared.
See `examples/array-selection.nov` for a runnable example and eight tests covering
selection, reverse visitation/short-circuiting, typed empties, maximum counts,
generic storable families, recursive copies and shared captures.

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

Map/filter/take/drop use repeated append and can be quadratic; by-value copies can add
substantial overhead. Further optimization remains **PAUSED**. See
`examples/array-helpers.nov` for the runnable example and six tests covering typed
and generic callbacks, stable order, empty inputs, left folds, recursive copies,
shared closure environments and eager argument order.

## String conversions

- `std.string.Utf8Error = enum { invalidUtf8, }`
- `std.string.toBytes(text: str) -> []u8`
- `std.string.fromBytes(bytes: []u8) -> std.Result(str, std.string.Utf8Error)`

```nov
let bytes = std.string.toBytes("é😀\0")
@assert(@len(bytes) == 7)
let text = std.string.fromBytes(bytes).unwrapOr("")
@assert(text == "é😀\0" and @len(text) == 3)
@assert(std.string.fromBytes([0xED, 0xA0, 0x80]).isErr())
```

`toBytes` copies the exact UTF-8 encoding into independent mutable array storage.
`fromBytes` first validates the entire input using a private ordinary Nov byte loop,
then copies valid input into independently owned immutable UTF-8 string storage.
Empty input, ASCII including NUL, and all Unicode scalars through U+10FFFF are
accepted. Stray continuations, C0/C1 and F5..FF leads, overlong encodings,
surrogates, out-of-range scalars, bad continuations and truncation return
`.err[.invalidUtf8]` without calling the panicking primitive. Valid input returns
`.ok[text]`. Allocation failure can still panic normally.

Neither direction mutates its input or shares mutable byte storage with it.
Later array writes cannot change a string, and returned bytes cannot change their
source string. Results remain valid after source scope exit, rebinding and GC
churn, including conversion of string slices. NUL is ordinary data; no terminator,
normalization, case folding or grapheme processing is added. `@len(bytes)` counts
byte elements for a `[]u8` array; other arrays count their elements, not bytes.
`@len(text)` counts Unicode scalars for a `str`, not bytes or grapheme clusters.
Both return native `uint`. A real struct field named `len` remains an ordinary
field; `@len` does not accept records or use user-defined length protocols.

Use `.!` in a function returning a compatible Result to propagate `Utf8Error`, or
match `.ok`/`.err` and the `.invalidUtf8` enum case. The wrappers are ordinary
first-class functions usable in function variables and `@TypeOf` queries.

Low-level `@stringToBytes(str) -> []u8` and `@stringFromBytes([]u8) -> str`
are copying runtime intrinsics. Direct `@stringFromBytes` **panics on invalid
UTF-8 in all build modes**; arbitrary input should use the recoverable public
wrapper instead. Allocation failures retain normal panic semantics. These narrow
conversions do not enable general casts or arbitrary compile-time execution;
`@as` is still only expected-type ascription. A UTF-8 editing builder and
performance optimizations remain deferred. See `examples/string-conversions.nov`
for a runnable example with exact stdout and nine tests.

## Integer parsing

- `std.string.parseInt(str) -> Result(int, std.string.ParseIntError)`
- `std.string.parseUint(str) -> Result(uint, std.string.ParseIntError)`

Decimal ASCII only, with optional `+` for either function and `-` only for signed
parsing. Leading zeros are valid. Reject whitespace (use `trim` explicitly), empty
or sign-only input, separators/prefixes, non-ASCII digits and trailing characters.
Errors are `.invalidInput` and `.overflow`. Support the full pointer-sized range,
including minimum signed `int`, without silent wrapping or allocation. No radix,
fixed-width destination or float parsing API is introduced. See
`examples/integer-parsing.nov` and `zig build test-cli-integer-parsing`.

## String search

- `std.string.startsWith(text: str, prefix: str) -> bool`
- `std.string.endsWith(text: str, suffix: str) -> bool`
- `std.string.find(text: str, pattern: str) -> Option(uint)`
- `std.string.contains(text: str, pattern: str) -> bool`

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
mutable strings, normalization or case folding; byte conversions are separate APIs above. See
`examples/string-search.nov` for a runnable example and five tests.

## String splitting

`std.string.split(text: str, separator: str) -> []str` splits at exact,
non-overlapping occurrences of a nonempty string separator, from left to right.
Matching is case-sensitive and Unicode-scalar aware, without normalization or
case folding; embedded NUL and multi-scalar separators are supported.

```nov
let parts = std.string.split("one::three", ":")
@assert(@len(parts) == 3)
@assert(parts[0] == "one" and parts[1] == "" and parts[2] == "three")
```

Leading, trailing and adjacent separators preserve empty fields. Empty input with
a nonempty separator returns `[""]`; no match or an oversized separator returns
one field containing the whole input. Matches do not overlap: splitting `"aaaaa"`
by `"aa"` yields `["", "", "a"]`.

An empty separator returns `[text]`, including `[""]` for empty input. Argument
expressions still evaluate once in normal left-to-right order before the function
checks the separator.

Fields are immutable, zero-copy string views retaining their original GC owner;
they remain valid after rebinding or dropping the input binding. The result array
allocates storage and follows ordinary independent array-copy semantics, while
its string elements continue sharing immutable content. Allocation failure panics
normally. This initial implementation uses existing scalar indexing/slicing and
repeated append; efficient-search/growth optimization remains deferred. See
`examples/string-split.nov` for a runnable example and five tests.

## String joining

`std.string.join(parts: []str, separator: str) -> str` inserts the
separator between parts, never before the first or after the last part. Empty
input returns `""`; a singleton returns its content unchanged. An empty separator
concatenates all parts. Empty parts are preserved, so joining `["", "a", ""]` with
`"|"` produces `"|a|"`.

```nov
@assert(std.string.join(["red", "green", "blue"], ", ") == "red, green, blue")
@assert(std.string.join(["e", "\u{301}"], "") == "e\u{301}")
```

Existing string `+` preserves the exact UTF-8 bytes of parts and separator,
including embedded NUL; no normalization, case folding or grapheme processing is
performed. Parts and separator evaluate once in normal left-to-right argument
order. Inputs remain unchanged and the immutable result remains valid after input
bindings are replaced or leave scope. Allocation failures panic normally.

This is an ordinary Nov loop using existing string `+`, not a compiler builtin
or optimized builder. Repeated concatenation can be quadratic; optimization stays
paused. See `examples/string-join.nov` for a runnable example and five tests.

## String transforms

- `std.string.trim(text: str) -> str`
- `std.string.replace(text: str, pattern: str, replacement: str) -> str`
- `std.string.repeat(text: str, count: uint) -> str`

`trim` removes only leading/trailing Unicode `White_Space`: U+0009–000D,
U+0020, U+0085, U+00A0, U+1680, U+2000–200A, U+2028, U+2029, U+202F,
U+205F and U+3000. Empty/all-whitespace input returns an empty view. Interior
whitespace, BOM (U+FEFF), zero-width characters, NUL and combining marks are not
removed. The result is a zero-copy immutable slice retaining the source owner;
scalar-aware native `int` bounds are computed without casts.

`replace` substitutes exact, case-sensitive, non-overlapping occurrences from left
to right, preserving UTF-8 bytes including NUL without normalization or grapheme
processing. Replacing `"aaaaa"` / `"aa"` with `"X"` yields `"XXa"`; newly inserted
text is not searched again. An empty replacement deletes matches. No match or an
oversized pattern preserves the input content; empty text yields `""` with a
nonempty pattern. An empty pattern preserves the input content, including empty
text, consistently with `split`'s empty-separator policy. Arguments remain eager.

`repeat` concatenates exactly `count` copies, preserving bytes without
normalization. Count zero or empty text returns `""`; empty text returns before
entering any loop, including for maximum `uint`.

```nov
@assert(std.string.trim("\u{3000} é😀\t") == "é😀")
@assert(std.string.replace("banana", "ana", "X") == "bXna")
@assert(std.string.repeat("é", 3) == "ééé")
```

All arguments evaluate eagerly, once, left to right, including empty/zero cases
and before pattern validation; there are no callbacks. Inputs remain unchanged,
and results stay valid after source rebinding or scope exit. Allocation failure
panics normally. These are ordinary Nov helpers: `replace` reuses `split`/`join`,
and `repeat` loops with existing string `+`. Repeated concatenation can be
quadratic under current copying; no efficient builder/search or other optimization
is added. Optimization remains paused. See `examples/string-transform.nov` for
exact main stdout and seven tests covering property boundaries, exact matching,
count boundaries, effects and scope/GC retention.

## References and library boundaries

Last-use copy elimination may transfer backing from a proven fresh value or final
private-local read instead of recursively copying it. Observable value independence
and argument evaluation order are unchanged. Borrowed/global storage, mutable
captures, unknown caller ownership and ambiguous execution regions remain
conservative. This is not copy-on-write; COW is a deferred, unaccepted proposal,
and explicit `@move` is deferred. See `examples/last-use.nov`.

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

Ordinary pointee access requires `p.*`, `p.*[i]`, `p.*.field`, or `p.*()`.
Method lookup is the exception: `p.method()` also works. Library calls such as `array.map(p.*, convert)` pass ordinary
array values under the existing copy rules, not references. Functions, callbacks,
closures, inferred generic functions, methods and pipes may accept `&T` / `&mut T`.
Pass `f(p)`, `f(&local)` or `f(&array[index])` to a compatible reference parameter;
`f(p.*)` passes a value instead. Mutable arguments may weaken to readonly for the
same child type, never the reverse; function/callback types remain invariant.
Formal reference bindings cannot rebind, but mutable-pointee `.*` writes and
forwarding to other compatible reference parameters work. Methods can declare
exact `&Self` / `&mut Self` receivers; the array helper signatures above remain
by value and unchanged.

Instance calls such as `counter.add(2)` supply a call-scoped reference when the
method declares `self: &mut Counter`; its body mutates through `self.*`.
Supported receivers are existing local owners, array elements and named
references (including reference parameters). Method lookup automatically
dereferences references: `p.add(2)` and `p.*.add(2)` are equivalent. This works
for reference and by-value receivers; by-value methods snapshot the pointee.
Mutable receivers require mutable storage; readonly `&Self` receivers also
accept immutable local owners. Fields and callable fields still need explicit
`p.*` access. Nested field receivers,
global/temporary borrowing and readonly mutation remain rejected. This is not
general reborrowing or reference escape support. Static `Counter.add(&counter, 2)`
and compatible reference forwarding still work.

Receiver targets and indices evaluate once before explicit arguments, left to
right. Loans stay live throughout arguments and calls; conflicting owner access
or invalidation rejects (including `counter.add(counter.value)`). Later reference
rebinding cannot redirect a pending call. By-value methods retain their independent
snapshots. Callable fields keep lookup precedence and ordinary calls, without
implicit receiver insertion; bound method values remain unsupported.
Generic containers may declare `let Self = @This()` and use `&mut Self`; inferred
method type parameters retain exact constraints. Imported calls require public
types/aliases and public methods independently. `@TypeOf` validates receiver places
and mutability without runtime borrows, calls, effects or captures. See
[`examples/mutable-methods.nov`](../examples/mutable-methods.nov) for seven tests:

```sh
nov check --semantic examples/mutable-methods.nov
nov fmt --check examples/mutable-methods.nov
nov run examples/mutable-methods.nov --debug
nov test examples/mutable-methods.nov --debug
nov run examples/mutable-methods.nov --release
nov test examples/mutable-methods.nov --release
```

The example prints:

```text
Counter: 9
Through reference: 10
Array elements: 10 25
Generic: after 7
```

Method autodereferencing adds no builder API or array clear/truncate; deferred
builder and optimization work remains paused.

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
immutable container method with a body and exact `(SelfType) -> str` signature.
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
