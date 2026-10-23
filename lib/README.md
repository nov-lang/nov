# Nov standard library foundation

Import the standard namespace and alias its ordinary `Result` template:

```nov
let std = @import("std")
let Result = std.Result
```

`std.nov` is the entry module. It publicly re-exports `Result` from `result.nov`;
there is no compiler builtin or automatic bare-name injection for Result.
Its variants are `ok: T` and `err: E`.

- `value.unwrapOr(fallback)` returns the success payload or the supplied fallback.
  The fallback expression is an ordinary argument and always evaluates.
- `value.unwrapOrElse(callback)` returns the success payload or calls the callback
  with the error payload. The callback expression evaluates normally, but its body
  runs only for an error. Callbacks are ordinary function values, not closures.
- `value.!` yields success or propagates an exactly typed error to the enclosing
  Result. This operator recognizes the `ok`/`err` shape, not the library name.

Methods specialize on demand. Void success/error payloads remain valid; a method
whose concrete signature would contain a void parameter cannot be used, following
Nov's existing parameter rules. For example, `Result(void, string)` can be
constructed and propagated without requesting `unwrapOr`.

## Loading and installation

`zig build` and `zig build release` install this directory at `<prefix>/lib/nov`;
Zig package sources include it as well. The installed compiler resolves `"std"`
from executable-relative `../lib/nov/std.nov`. Semantic check, emit-c, build, and
run accept `--lib-dir <path>` to override that library root. There is no implicit
working-directory library search or embedded developer-checkout path.

Explicit relative `.nov` imports resolve from the importing file, not the process
working directory. For example, `@import("result.nov")` in `std.nov` refers to this
directory's file. Imports yield compile-time namespaces, never runtime values.
Canonical paths share one source and nominal identity across repeated/diamond
imports; cycles diagnose. Named namespaces expose only public top-level exports;
file namespaces also expose unmarked declarations. Member-level privacy and the
`@[private]` attribute are not implemented yet.

The actual library is tested through real imports in semantic, emit-C, build/run,
formatter, and relocated-install checks. `examples/results.nov` and
`examples/structured-errors.nov` use `std.Result` rather than copied definitions.

Filesystem and other OS-resource APIs remain outside this first library slice.
