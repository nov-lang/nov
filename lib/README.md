# Nov standard library foundation

`result.nov` defines `Result(T, E)` as an ordinary Nov generic union, not a
compiler builtin. Its variants are `ok: T` and `err: E`.

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

## Current integration boundary

The library source is formatted and tested by `zig build test`. CLI integration
fixtures combine the actual library definition and a test body into one module;
this is a test harness, **not an import implementation**.

`zig build` and `zig build release` install this directory at `<prefix>/lib/nov`;
Zig package sources include it as well. The compiler does not yet load it through
`@import`, inject `Result` automatically, or search a library path. Existing examples
still define their own Result until source-qualified multi-file compilation lands.

Module import spelling, standard-library root lookup, and re-export rules need to
be settled before the examples can consume this module directly. Filesystem and
other OS-resource APIs are intentionally outside this first library slice.
