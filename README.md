# nov

nov is a functional programming language.

nov type system is strong and sound. idk about static or dynamic yet. also most types should be inferrable.

nov has uncolored async! (well not yet tho)

nov code is compiled to bytecode then run in the NVM (Nov Virtual Machine). (This name sucks)

nov main inspirations are Zig and OCaml.

# TODO
Implement basic IR then codegen to try it before adding more features

It's fine to keep arena fucked up allocation fest until GC implementation

Fix this README, add links where there should be e.g. for colored async above.

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
- make all variable constant, all functions pure and remove mut keyword (obviously)
  - which specific case would be lost by not having mutable values?
  - the compiler (or vm?) can perform mutation where it's guarented to have no side effect
- modes / arg options:
  - build: output a file with nov bytecode
  - run: run in a VM either a .nov file or a .novc file
  - repl: run the repl
  - fmt: format nov code
- target JVM?
- output native code? (make a tracing GC or see [V's autofree](https://docs.vlang.io/memory-management.html))
- output C? (kinda weird why not just target LLVM which will probably result in faster compilation)
  - actually it's not a bad idea
- support JIT? (JIT in another thread btw)
- add tests, mainly for Parser, IR and VM
- Error type for each step (~Tokenizer, Parser~, IR, Codegen, Runtime)
- forward reference
- string interpolation like `"${varname:[fill][alignment][width][.precision][type]}"`
- handle SIG.INT correctly, need to write an alternative to isocline in zig
- in repl mode output statement result by default
  - add a print instruction right before a statement if it returns another type than ()
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
- render (parser) error with caret under the error + full line info
- add Timer for parsing_time, codegen_time, runnning_time
- implement correct leaking allocation to have fast exit time
- add async/await/yield
  - see go coroutines and other languages way of doing it
  - see wren's [fibers](https://wren.io/concurrency.html)
  - see https://docs.vlang.io/concurrency.html
- add an FFI with C and bindings for zig
- add `_ =` or `() =` or `let () =` to discard the return of a function and make
  it mandatory to not ignore the return value from an expression? (no, these are
  called expression statement and it's fine to ignore their result just to
  trigger the side effects of evaluating the expression) (actually yes, we
  shouldn't ignore a non () return)
- add operator overloading:
  - compilation error when trying to use an operator for values of different types
  - compilation error when trying to use an operator that is not defined for the said value
  - try to find a way to make function calls or at least these kind of function call extra lightweight
  - implem `>=`, `==`, `<`, etc... with Object.order() wich should return a std.math.Order
  - `+` binary op is syntaxic sugar for Object.add, or iadd/fadd for int/float
  - `!` postfix unary op is syntaxic sugar for Object.unwrap (idk about name,
    should only be available for Result and Option)
  - How to handle string * int?
    - handle Object.mul(a: Object, b: OtherObject), make it clear in the
      doc/language that this is possible, look at rust traits (no)
    - just disallow and compile error (no)
    - just add a method (yes, String.repeat())
  - How to handle int + float? or big_int + int?
    - have to cast variable e.g. `3.to(float)` or builtin
    - have seemless transition between numbers types, at least for int and uint
    - for big_int use addScalar
- disallow operation between int and floats? what about between int and uint?
- automatically convert int to big int on arithmetic overflow instead of crashing,
  big int has the same type as int (in user program) but not the same representation
  (no this is cool imo but too much overhead)
- optimize tail call recursion
- tree shaking
- constant folding
- The polymorphism in [Functions](#Functions) can be too weird and complex
  - maybe just do like zig and accept a type as parameter instead
  - just have generics and disallow polymorphism (no)
- write nov website with nov as backend? idk
- add a way to have better OOP, interface, traits etc idk I hate OOP but it
  sometimes stuff sucks without correct OOP
- make nov embeddable, just make the compiler and VM like a library and provide bindings to them
- add cache either in cwd or in $XDG_CACHE_HOME/nov/... (ofc finding the dir is
  handled by known-folders) like \_\_pycache__
- add the `rune` primitive which represents a unicode code point
  - handle char_literal in tokenizer/parser and remove ' notation for strings
- add interfaces? (no)
- add generics instead of function that accepts a type?
  - probably make compilation easier
  - syntax: `add<T>(a: T, b: T) T` instead of `add(T: type, a: T, b: T) T`
  - infer type from givern arguments -> works almost just like ML like polymorphism
  - need @typeInfo() builtin?
  - what about ML like polymorphism?
- autodoc with doc comments `;;;`
- package system
- [compile time pseudo variables](https://docs.vlang.io/conditional-compilation.html#compile-time-pseudo-variables)?
- add defer?

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
- Check [roc-lang](https://www.roc-lang.org/examples/FizzBuzz/README.html) function pipes usage (instead of monads)
- Check [OCaml Loops](https://ocaml.org/docs/loops-recursion) for nov loops

## Concepts

### Builtins
- `@This()`: Same as zig, returns the type of the current container.
- `@import(path: string)`: Import a nov file.
  - When importing other files only declarations gets imported which means that
    if there is a print in global scope in a library it will be ignored (or
    error?)
- `@TypeOf(...)`: Returns the type of a value.
- `@print(...)`: Output all args separated with a space to stdout. (supports string interpolation)
                 TODO: what about printf? what about print to another file?
- `@println(...)`: Same as print with a newline at the end.
- `@fprint(file: File, ...)`: Same as print but output to a specific file.
<!-- - `@eprint(s: string)`: Output `s` to stderr. -->
- `@panic(s: string)`: Output `s` and backtrace to stderr, then terminate the program with error code 1.
- `@max(a: T, b: T, ...)`: Returns the maximum value between all the supplied arguments
- `@min(a: T, b: T, ...)`: Returns the minimum value between all the supplied arguments
- `@dump()`: TODO: https://docs.vlang.io/builtin-functions.html#dumping-expressions-at-runtime
- `@embedFile()`: TODO, also allow for compressing the file
- see https://docs.vlang.io/conditional-compilation.html
- see https://docs.python.org/3.12/library/functions.html

### Functions
Arguments are immutable by default unless mut is specified.
TODO: variable number of arguments? (just use an array)
```nov
let doNothing = () -> () {}
@TypeOf(doNothing) ; returns `() -> ()`

; note that even if x is mutable, the variable given as argument will not
; change because int are passed as value
let retNothing = (mut x: int) -> () {
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

This is without parenthesis for args and return but idk if there should
actually be parenthesis. Adding parenthesis would kinda make sense with e.g.
`f()` is call f with 0 arg which is (), or return () is return nothing which
add sense to the () type/value but `(int)` as return type is ugly.
- Maybe make parenthesis mandatory only for args
- or make parenthesis mandatory only if there is multiple args (yes, ez way to
  distinguish simple_fn_proto with fn_proto)
```nov
let add = a: int, b: int -> int {
    a + b
}

; return multiple values
let div = a: int, b: int -> int, int {
    a / b, a % b
}

; async func, can be run normally or async
let range = n: int -^ int -> () {
    for i in 0..n {
        yield i
    }
}
```

### Enum
Nov enums are just like C enums except that they can have methods and are not
global.

TODO:
- is it possible to do `let x: MyEnum = .xxx` also for match do we specify the `.`?
- add a way to convert a int to enum and enum to int.
- add a way to create an enum from a string.
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

match x {
    x => ... ; do something
    y, z => {} ; do nothing
}
```

### Struct
TODO:
- pub/priv keyword, also mut keyword?
- add a way to init a struct without its type like in zig or c99?
- support anonymous structs, useful for json, I think it's easy since struct is
  like a function that returns a type but syntax is ugly outside of a struct
  which is fine and intended
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

    ; TODO: should we omit the type on self?
    let eql = (self: MyStruct, other: MyStruct) -> bool {
        ; side note, parenthesis allows to bypass newline checks
        return (
            self.name == other.name and
            self.x == other.x and
            self.y == other.y and
            self.z == other.z
        )
    }
}

let a = MyStruct{} ; error: missing struct field: name

let x = MyStruct.init("aaa")
let y = MyStruct{ .name = "aaa" }
x.eql(y) ; true
x |> println ; idk what this prints
```

### Union
See [Result and Option unions](#result-and-option-unions) for another example.
```nov
let MyUnion = union {
    a: int
    b: float
    c: int
    d: string
}

let x = MyUnion{ .b = 3 };
x.a ; error

let Tree = union {
    empty
    node: struct {
        value: int
        left: Tree
        right: Tree
    }

    let sum = (self: Tree) -> int {
        match self {
            empty => 0
            node => |n| n.value + n.left.sum() + n.right.sum()
        }

        ; we could also use an if here
        ; TODO: will this cause problem with operator overloading?
        ;       maybe replace with `is` keyword and add a | | for union?
        if self == empty {
            0
        } else {
            n = n.node
            n.value + n.left.sum() + n.right.sum()
        }
    }
}
```

### Result and Option unions
The Result and Option unions are created with functions since types are values.
In nov there is no untagged union thus  we can match on any union to find the
active field. Note that it isn't represented here but an union field can have a
default value just like a struct field.

Sugar for Result and Option:
- !T is Result(T, string)
- E!T is Result(T, E)
- ?T is Option(T)
- expr! is unwrap or propagate for result
- expr? is unwrap or propagate for option

TODO: propagation in top level
- not possible
- panic if it's an error or a none
- replace top level with main

TODO: how to return an error?
- err(...)
- error(...)
- ???

See https://docs.vlang.io/type-declarations.html#optionresult-types-and-error-handling
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
let y = Option(int){ .some = 5 }
; `??` is a special operator for Result and Option to provide a fallback value
; if the variable is .err or .none
let sum = x + y ?? 0

let MyOption = Option(float)
let a = MyOption{ .some = 1.0 }
let b = MyOption{ .none }
let prod = x! * y! ; not sure about syntax (this returns an error btw)
```

### Array
Side note about mutability. A constant array cannot be modified in any way, its
values are constant too. A mutable array can be reassigned/extanded and its
values can be modified.

TODO:
- add a way to initialize the capacity of an array without adding any element
  - `let x = []int.initCapacity(50)` x is an array with len = 0 but with capacity = 50
  - use a builtin?
- add a way to repeat an array like a string, allow to use that as an initializer
  - `let x = [1].repeat(50)` x is an array of 50 int with value 1
  - `let x = [@as(uint, 1)].repeat(50)` x is an array of 50 uint with value 1
  - `let x: []uint = [1].repeat(50)` x is an array of 50 uint with value 1

See [1](https://docs.vlang.io/v-types.html#array-methods) [2](https://docs.vlang.io/v-types.html#array-method-chaining)
```nov
let mut my_array = [1, 2, 3]
@TypeOf(my_array) ; returns []int
my_array.len == 3 ; true, should `len` be a function?
my_array[0] == 1 ; true
my_array[-1] == 3 ; true
my_array << 5 ; TODO: push operator for arrays
my_array |> println ; prints [1, 2, 2, 5]
my_array += [1, 1, 7 ]
my_array |> println ; prints [1, 2, 3, 5, 1, 1, 7]
6 in my_array ; false
my_array = []
@TypeOf(my_array) ; still return []int

let my_array_of_array = [["Hello", "World!"], ["Bonjour", "Monde!"]]
@TypeOf(my_array_of_array) ; returns [][]string
let arr_arr = my_array_of_array ; alias because it's long to type
arr_arr.len == 2 ; true

; python like way of printing the array
for arr in arr_arr {
    for w in arr {
        print(w + " ")
    }
    println()
}

; functional way, I think, add another way with .map()
arr_arr >>= |arr| {
    arr >>= |word| word + " " |> print
    println()
}
```

### Slice
TODO: https://docs.vlang.io/v-types.html#array-slices

### Map
TODO: https://docs.vlang.io/v-types.html#maps
Our implem is different, check src/value.zig.

### Match
TODO: check https://docs.vlang.io/statements-&-expressions.html#match

Allow to match strings? (also allow to match like startsWith, endsWith?)

TODO: proposal to match over weird stuff like `x when x % 2`
```nov
let x = 3
let idk = match x {
    0 => 0
    x when x % 2 == 0 => 1
    x => 2 ; same as _ => 2 (no need for _ then?)
}
```

### If/Else
TODO: add if unwrapping sugar for Result and Option?
```nov
let a = 10
let b = 20
; braces are mandatory, else is optional
if a < b {
    @println("${a} < ${n}")
} else if a > b {
    @println("${a} > ${n}")
} else {
    @println("${a} == ${n}")
}

; all If are expression which means that they all return a value
; the previous if returns `()`
; this one returns a bool
let is_even = if 69 % 2 == 0 { true } else { false }
; another example which returns an Option(int)
let x: ?int = if is_even {
    @println("even")
    42
} else {
    @println("not even")
    .none
}
```

### For loop
```nov
; names can be an array, a slice, a string or an iterator
; an iterator is any object with a public .next() method that returns an Option(T)
; type is always inferred
for name in names {
}

; n takes values [0;10[
; note than the boundary doesn't need to be literal
; for n in x..y {} is fine as long as x and y are unsigned integers
for n in 0..10 {
}

; it's possible to loop on multiple values at the same time as long as they
; have the same length
; TODO: what about iterators?
for name, i in names, 0.. {
}

; loop over a map (need parenthesis or give a kv "object" (in compiler it will
; be replaces by 2 values but in code it will look like an object) instead?)
for k, v in m {
}

; values are const by default, add mut to make them mutable,
; a range cannot be mutable
for mut name in names {
}

; use underscore to ignore a value
for _ in 0..10 {
}

; we can also loop on a condition
for x > 10 {
}

; infinite loop
for {
}

; proposal: add "zig while" or C like loop
;     let mut i = 0
;     for i < 100 : i += 2 {}
;
;     for i = 0; i < 100; i += 2 {}
```

### Break & Continue
TODO: same as zig

### Defer
TODO: same as zig, still a proposal

### In
Check if an element is in an array or if it's a key in a map.
This should be faster than using a for loop because it should be vectorized.
(with std.mem.indexOfScalar)
```
let nums = [1, 2, 3]
@println(1 in nums) ; true
@println(2 !in nums) ; false
```

### Operator
TODO: add precedence

TODO: it's not very clear if T is a type or a variable here.

Prefix
- `!`: `logical not` !bool
- `-`: `negation` -int/uint/float
- `~`: `bitwise not` ~int/uint
- `?`: `optionify` ?T, equivalent to Option(T)
- `!`: `resultify` !T, equivalent to Result(T)
Infix
- `+`: `add` int, uint, float, string, []T
- `-`: `sub` int, uint, float
- `*`: `mul` int, uint, float
- `/`: `div` int, uint, float
- `%`: `rem` int, uint
- `&`: `bitwise and` int, uint
- `|`: `bitwise or` int, uint
- `^`: `bitwise xor` int, uint
- `<<`: `left shift` int/uint << uint
- `>>`: `right shift` int/uint >> uint
- `or`: `logical or` bool
- `and`: `logical and` bool
- `==`: `equal` T == T
- `!=`: `not equal` T != T
- `<`: `less than` int, uint, float
- `>`: `greater than` int, uint, float
- `<=`: `less equal` int, uint, float
- `>=`: `greater equal` int, uint, float
- `<<`: `push` []T << T
- `>>=`: `bind` T >>= |T2| ...
- `|>`: `pipe` T |> func
- `??`: `optional fallback` Option(T) ?? T
- `in`: `in` T in []T / T in \[K]V
- `!in`: `not in` T !in []T / T !in \[K]V
Postfix
- `!`: `unwrapOrReturn` Result(T)!
- `?`: `unwrapOrReturn` Option(T)?
- `[]`: `access` string, array
Assignment
- `=`: `assign` let x, y, ... = T1, T2, ...
- `+=`: `assign add` let x += T
- `-=`: `assign sub` let x += T
- `*=`: `assign mul` let x += T
- `/=`: `assign div` let x += T
- `%=`: `assign rem` let x += T

TODO: what about logical and shift assign?

### Operator overloading
Operator overloading is possible on the following operators:
- `+`: (T, T) -> T
- `-`: (T, T) -> T
- `-`: (T) -> T    unary negation (is it possible to autogen it from the binary op?)
- `*`: (T, T) -> T
- `/`: (T, T) -> T
- `%`: (T, T) -> T
- `<`: (T, T) -> bool
- `==`: (T, T) -> bool
- ~`>>=`: (T, (T) -> T2) -> T2~
- ~`[]`: (T, int) -> T2~

Note:
- `==` is automatically generated for all types by the compiler but can be overridden.
- `!=`, `>`, `<=`, `>=` are automatically generated when `==` and `<` are defined.
- `+=`, `-=`... are automatically generated when the corresponding operator is defined.
```nov
let Complex = struct {
    re: float
    im: float

    ; TODO: replace `@"+"` with `@overload(+)` or `@operator(+)`
    let @"+" = (self: Complex, other: Complex) -> Complex {
        Complex{
            .re = self.re + other.re,
            .im = self.im + other.im,
        }
    }

    ; used by print
    ; signature must be `toString: (T) -> string` where T is the container type
    let toString = (self: Complex) -> string {
        "${self.re} + i${self.im}"
    }
}

let x = Complex{ .re = 5, .im = 3 }
let y = Complex{ .re = 2, .im = 7 }
x + y ; returns Complex{ .re = 7, .im = 10 }
```

### C FFI
```nov
let malloc = extern malloc: (uint) -> (voidptr)
```

### Multiline string literal
```nov
; Multiline string literal used to be like zig e.g.
let s =
    \\this my string
    \\this is on a new line
    \\yes

; but now it was replaced with grouped expression & string `+` operator
let s = (
    "this is my string\n" +
    "this is on a new line\n" +
    "yes"
)
