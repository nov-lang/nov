# nov

# TODO
Rewrite Ast and Parser based on the previous versions (1 and 2)

Keep it small at first with only a few expressions and very simple declaration (there is no statement in nov)

Implement basic IR then codegen to try it before adding more features

It's fine to keep arena fucked up allocation fest until GC implementation

## Crafting Interpreters
- https://craftinginterpreters.com/local-variables.html#challenges
- https://craftinginterpreters.com/global-variables.html#challenges
- match, break, continue: https://craftinginterpreters.com/jumping-back-and-forth.html#challenges
- next step: https://craftinginterpreters.com/calls-and-functions.html

## Grammar
- https://craftinginterpreters.com/appendix-i.html
- https://github.com/ziglang/zig-spec/blob/master/grammar/grammar.peg
- https://doc.rust-lang.org/stable/reference/introduction.html
- https://ocaml.org/docs/values-and-functions#the-pipe-operator
- https://ocaml.org/docs/basic-data-types#options--results

## Proposals and stuff to add
- opt to output bytecode and to run bytecode instead of interpreting
- tests mainly for Parser, IR and VM
- Error type for each step (~Tokenizer~, Parser, IR, Compiler, Runtime)
- forward reference
- string interpolation
- separate int, uint, float and string operations. e.g. + for int/uint, +. for float, ++ for string
- replace , with | in match prong?
- add ** for exponential?
- handle SIG.INT
- in repl mode output statement result by default
- separate codegen, VM and CLI to allow embedding nov into another project and to
  make it easier to implement JIT or native compilation
  - see [this](https://wren.io/embedding/) for embedding
- move to zig master
  - replace `while (true) switch` with [labeled switch/continue](https://github.com/ziglang/zig/pull/21257)
    see https://github.com/ziglang/zig/pull/21367 for tokenizer
  - for vm implem debugLoop() (original with debug infos) and releaseLoop() (with labeled switch/continue)
  - add min_version for build.zig
  - update ci
- for Value: replace pointer with raw type and use a MultiArrayList
  - need to improve globals in VM
  - idk how to handle it in codegen yet
- builtin functions
  - see https://docs.python.org/3.12/library/functions.html
- add a try catch system or something to deal with errors
- render (parser) error with caret under the error + full line info
- add Timer for parsing_time, codegen_time, runnning_time
- implement correct leaking allocation to have fast exit time
- add import
- add async/await/yield
  - see go coroutines and other languages way of doing it
  - see wren's [fibers](https://wren.io/concurrency.html)
- add throw/try/catch
  - try and catch like in zig
  - throw used to return an error (or just return a error.XXX like zig?)
- add generics
- add `type` keyword to define a new type or `struct`/`enum` idk
  - also tagged union
- add typeof keyword/builtin/idk
- rework examples
- remove `loop` loops?
- remove `while` loops?
- add `do while` loops or `repeat until` loops?
- monads, there already here but with specific `>>=` bind function
- add an FFI with zig and C
- add `_ =` or `() =` to discard the return of a function and make it mandatory
  to not ignore the return value from an expression? (no, these are called
  expression statement and it's fine to ignore their result just to trigger the
  side effects of evaluating the expression)
- add operator overloading?
- concat string with `++` and mult with `**` instead of same as number
- replace float operators with `+.`, `-.`, `*.`, `/.`, `%.`
- compilation error when trying to add/compare value of different type? (at least for non number)
- remove uint?
- optimize tail call recursion
- tree shaking

## Notes
- Check previous step of crafting interpreters and implement them with
  AST/Parser/IR/Codegen instead of just compiler
- Check std.zig.Ast.parse and std.zig.Ast.tokenLocation for Ast and Parser
- Check std.zig.AstGen, std.zig.Zir and zig/src/Sema.zig for IR
- Check zig/src/InternPool.zig for storing variables
- Check std.zig.start.callMainWithArgs
- Old errors:
  - `print i` throw a expected new line after statement which is not a very good error message
  - giving "}}" to Parser result in an infinite loop
- No need for parenthesis everywhere (look at rust, go and caml)
- See [Option](https://doc.rust-lang.org/std/option) and [Result](https://doc.rust-lang.org/std/result) for nil and error
- See [What are the NO's in GO design?](https://kuree.gitbooks.io/the-go-programming-language-report/content/32/text.html)
- A file is a module, import module as value like zig?
  - `import "std"` or `let std = import "std"`
  - When importing other files only declarations gets imported?
