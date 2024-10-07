# nov

# TODO
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
- modes / arg options:
  - build: output a file with nov bytecode
  - run: run in a VM either a .nov file or a .novc file
  - repl: run the repl
  - fmt: format nov code
- target JVM?
- output native code?
- add tests, mainly for Parser, IR and VM
- Error type for each step (~Tokenizer, Parser~, IR, Codegen, Runtime)
- forward reference
- string interpolation like "${variable name}"
- separate int, uint, float and string operations. e.g. + for int/uint, +. for float, ++ for string
- replace , with | in match prong?
- handle SIG.INT correctly, need to write an alternative to isocline in zig
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
- remove `loop` loops?
- remove `while` loops?
- add `do while` loops or `repeat until` loops?
- monads, there already here but with specific `>>=` bind function
- add an FFI with zig and C
- add `_ =` or `() =` to discard the return of a function and make it mandatory
  to not ignore the return value from an expression? (no, these are called
  expression statement and it's fine to ignore their result just to trigger the
  side effects of evaluating the expression)
- compilation error when trying to add/compare value of different type if there
  is no method for the operator and underlying type. e.g. it is possible to do
  "a" * 3 if String.mul(String, int) or Integer.mul(int, String) is implemented.
- add operator overloading:
  - note for bytecode, replace add, etc... bytecode with function call,
    try to find a way to make function calls or at least these kind of function call extra lightweight
  - implem `>=`, `==`, `<`, etc... with Object.order() wich should return a std.math.Order
  - `+` binary op is syntaxic sugar for Object.add
  - `!` postfix unary op is syntaxic sugar for Object.unwrap (idk about name,
    should only be available for Result and Option)
  - How to handle string * int?
    - handle Object.add(a: Object, b: OtherObject), make it clear in the
      doc/language that this is possible, look at rust traits
    - just disallow and compile error (no)
  - How to handle int + float? or big_int + int?
    - have to cast variable e.g. `3.to(BigInt)`
    - have seemless transition between numbers types, at least for int and big int
- automatically convert int to big int on arithmetic overflow instead of crashing,
  big int has the same type as int (in user program) but not the same representation
- remove uint? (yes, remove uint value and use raw u64 for both, the representation
  should be handled with the variable inner type (nan boxing is useless))
- optimize tail call recursion
- tree shaking
- The polymorphism in [Functions](#Functions) can be too weird and complex
  - maybe just do like zig and accept a type as parameter instead
  - just have generics and disallow polymorphism (no)
- write nov website with nov as backend? idk

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
- Import file as value like zig
  - `import "std"` or `let std = import "std"`
  - When importing other files only declarations gets imported?
- Check [roc-lang](https://www.roc-lang.org/examples/FizzBuzz/README.html) function pipes usage (instead of monads)

## Concepts

### Builtins
- `@This()`: Same as zig, return the type of the current container.
- `@import()`: Import a nov file.
- `@TypeOf(val: any)`: Returns the type of a value.
- `@print()`: TODO
- `@panic()`: TODO
- `@range()`: TODO
- `@max()`: TODO
- `@min()`: TODO

### Functions
```nov
let doNothing = () -> () {}
@TypeOf(doNothing) ; returns `() -> ()`

let retNothing = (x: int) -> () {
    x += 1
}
@TypeOf(retNothing) ; returns `(int) -> ()`

let ret2 = () -> int {
    2
}
@TypeOf(ret2) ; returns `() -> int`

;;; Polymorphism
; This is still conceptual
let eql = (x, y) -> bool {
    x == y
}
@TypeOf(eql) ; returns `(:a, :a) -> bool`

; `:*` is a special type that corresponds to any type
let poly = (x, y) -> :* {
    ; ...
}
@TypeOf(poly) ; returns `(:a, :b) -> :c`

; here `:a` can be int, float, string or whatever type that has a definition
; for the `+` operator
let add = (x: :a, y: :a) -> :a {
    x + y
}
@TypeOf(add) ; returns `(:a, :a) -> :a`
```

### Enum
Nov enums are just like C enums except that they can have methods and are not
global.
```nov
let MyEnum = enum {
    x ; default to 0
    y = 5
    z ; default to y + 1 = 6

    let eql = (self: MyEnum, other: MyEnum) -> bool {
        self == other
    }
}

let x: MyEnum = MyEnum.x
let y = MyEnum.y ; type is inferred
x.eql(y) |> println ; print false
```

### Struct
TODO: are tuples like that?
```nov
let MyStruct = struct {
    name: string
    x: float = 1.
    y: float = 1.
    z: float = 1.

    let max = 100.

    let init = (name: string) -> MyStruct {
        MyStruct{ .name = name }
    }
}

let x = MyStruct.init("aaa") ; same as let x = MyStruct{ .name = name }
x |> println ; idk what this prints

; tuples are anonymous structs
let tuple = .{ 0, 1, 2 }
@TypeOf(tuple) |> println ; idk what this prints
```

### Result and Option unions
The Result and Option unions are created with functions since types are values.
In nov there is no untagged union thus  we can match on any union to find the
active field. Note that it isn't represented here but an union field can have a
default value just like a struct field.
```nov
let Result = (T: type, E: type) -> type {
    union {
        ok: T
        err: E
    }
}

let Option = (T: type) -> type {
    union {
        some: T
        none

        ; unions can also have methods
        let forceUnwrap = (self: @This()) -> T {
            match self {
                .some => |val| val ; catch the value and return it
                .none => @panic("...")
            }
        }
    }
}

let x = 5
let y: Option(int) = .some{5}
; `??` is a special operator for Result and Option to provide a fallback value
; if the variable is .err or .none
let sum = x + y ?? 0
```
