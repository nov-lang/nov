# Nov

Nov is a functional programming language.

Nov type system is static, strong and sound. Also most types should be inferrable.

Nov has uncolored async! (well not yet tho)

Nov has automatic memory management via a minimal tracing GC.
<!-- TODO: https://github.com/ivmai/bdwgc -->
<!-- TODO: autofree https://docs.vlang.io/memory-management.html -->

# TODO
Next step: add new IR that
- forward declaration
- resolve literals
- replace operator overloading?
- have more usage safety than Ast
- start storing values in a constant pool

AstGen/Sema from zig looks to complex if we just want to generate some C code.
Move to a custom IR?

Need to work on Parser/Ast for more complex stuff (e.g. functions, for loops,
...) after basic IR and codegen is implemented.

Separate frontend (Tokenizer, Parser, IRs) from backend (Codegen, ...).
Also separate CLI, REPL and VM from all the that.
And cleanup the repo

Figure out what (first) backend to chose
- **A language, probably C** but can be Zig or something else (JS)
- A VM, a custom one or the JVM (or WASM)
  - a custom one allow support for JIT
- Native, either unoptimized for fun or dig into LLVM

Fix this README, add links where there should be e.g. for colored async above.

Keep to the main features aka:
- functional: functions are first class citizen
- async: good and lightweight async support is mandatory
- GC: nov should handle everything memory related
- interoperability with C unless it harms the features above
Keep Nov away from:
- Really low level stuff aka assembly, kernel or drivers. Use zig instead.

## Proposals and stuff to add
- make all variable constant, all functions pure and remove mut keyword (obviously)
  - which specific case would be lost by not having mutable values?
  - the compiler (or vm?) can perform mutation where it's guarented to have no side effect
- modes / arg options:
  - no arg: same as build
  - build: output a compiled file
  - run: execute a nov file
  - repl: run the repl
  - test: run unit tests
  - fmt: format nov code
    - works like zig fmt but for nov
    - rename variable, functions and types to snake_case, camelCase and PascalCase?
- add tests, mainly for Parser, IRs, Codegen and Runtime
- Error type for each step (~Tokenizer, Parser~, IRs, Codegen, Runtime)
- handle SIG.INT correctly, need to write an alternative to isocline in zig
- in repl mode output statement result by default
  - add a print instruction right before a statement if it returns another type than `void`
- render (parser) error with caret under the error + full line info
- add Timer for parsing_time, codegen_time, runnning_time (or use tracy)
- implement correct leaking allocation to have fast exit time?
- add `_ =` to discard the return of a function and make it mandatory to not
  ignore the return value from an expression
- add operator overloading:
  - compilation error when trying to use an operator for values of different types
  - compilation error when trying to use an operator that is not defined for the said value
  - `+` binary op is syntaxic sugar for Object.add, or iadd/fadd for int/float
  - How to handle string * int?
    - handle Object.mul(a: Object, b: OtherObject), make it clear in the
      doc/language that this is possible, look at rust traits (no)
    - just disallow and compile error (no)
    - just add a method (yes, String.repeat())
  - How to handle int + float? or int + uint? or big_int + int?
    - have to cast variable e.g. `1.0 + @as(float, 3)`
    - have seemless transition between numbers types, at least for int and uint
    - for big_int use addScalar
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
- IO API like zig, no buffering by default? or just like C?
- How to handle arithmetic overflow? current behaviour is to wrap,
  we don't want crash on arithmetic overflow
- Replace `void` with `()`?
- allow for default argument in function? (probably not)
- add regex pattern as builtin type?
- [Switch Prongs Defined as Comptime-Known Arrays](https://github.com/ziglang/zig/issues/21507)
- rename `for` to `loop`?
- make all arrays/string 0 terminated
- dependant types?
- Replace ` for functions with \ or #?
- Add `<=>` operator?
- Remove `<<=` operator?
- add `pop` keyword which works like return but for the current scope, same as a `break :blk`?

## Notes
- Check std.zig.AstGen, std.zig.Zir and zig/src/Sema.zig for IR
- Check zig/src/InternPool.zig for storing variables
- Check std.zig.start.callMainWithArgs
- No need for parenthesis everywhere (look at rust, go and caml)
- See [Option](https://doc.rust-lang.org/std/option) and [Result](https://doc.rust-lang.org/std/result) for nil and error
- See [What are the NO's in GO design?](https://kuree.gitbooks.io/the-go-programming-language-report/content/32/text.html)
- Check [roc-lang](https://www.roc-lang.org/examples/FizzBuzz/README.html) function pipes usage (instead of monads)
- it's a compile error to modify a string with [] or to take a mut of a string in a for loop
- shifting warning: `x >> y` (same for `<<`)
  - error if y is signed
  - warn if y >= @bitSizeOf(x): `x >> ${y} is the same as x >> ${y % @bitSizeOf(x)}`
- Check [SSA](https://en.wikipedia.org/wiki/Static_single-assignment_form) and [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style)
- Remember to check src/vm/value.zig for cool stuff

# Concepts

## Builtins
- `@This()`: Same as zig, returns the type of the current container.
- `@import(path: string)`: Import a nov file.
  - When importing other files only declarations gets imported which means that
    if there is a print in global scope in a library it will be ignored (or
    error?)
  - support relative import e.g. `@import("github.com/nov-lang/idk_lib")`
  - support circular imports
- `@TypeOf(...)`: Returns the type of a value.
- `@typeInfo(...)`: Returns type information about a value.
<!-- - `@format(...)`: Format all args to a string. (support string interpolation) -->
- `@print(...)`: Output all args separated with a space to stdout. (supports string interpolation)
                 TODO: what about printf? what about print to another file?
- `@println(...)`: Same as print with a newline at the end.
- `@fprint(file: File, ...)`: Same as print but output to a specific file.
<!-- - `@eprint(...)`: Same as print but output to stderr. -->
- `@panic(s: string)`: Output `s` and backtrace to stderr, then terminate the program with error code 1.
- `@max(a: T, b: T, ...)`: Returns the maximum value between all the supplied arguments
- `@min(a: T, b: T, ...)`: Returns the minimum value between all the supplied arguments
- `@dump()`: TODO: https://docs.vlang.io/builtin-functions.html#dumping-expressions-at-runtime
- `@embedFile()`: TODO, also allow for compressing the file
- `@sizeOf()`: TODO
- `@bitSizeOf()`: TODO
- `@as(T: type, arg: any)`: Cast arg to T if possible.
- see https://docs.vlang.io/conditional-compilation.html
- see https://docs.python.org/3.12/library/functions.html

## Functions
Parameters are immutable by default.

TODO: variable number of parameters? (just use an array)
```nov
; signature: `() -> void
let doNothing: `() = {}

; here x is passed by reference
; signature: `(&int) -> void
let retNothing: `(x: &int) = {
    x += 1
}

; functions body doesn't need to be a block
; signature: `() -> int
let ret2: `() -> int = 2

; signature: `(int, int) -> int
let add: `(a: int, b: int) -> int = a + b

; TODO: return multiple values or return a tuple?
; signature: `(int, int) -> (int, int)
let div: `(a: int, b: int) -> (int, int) = {
    a / b, a % b
}

; TODO: write a whole section about async
; async func, can be run normally or asynchronous
; signature: `(int) -^ int -> void
let range: `(n: int) -^ int = {
    for i in 0..n {
        yield i
    }
}

;;; Polymorphism
; signature?

; int signature: `<int>(int, int) -> bool
let eql: `<T>(x: T, y: T) -> bool = x == y
eql(1, 2) ; returns false
eql(true, 0) ; compile error

let add: `<T>(x: T, y: T) -> T = x + y
add(1, 2) ; returns 3
add("he", "llo") ; returns "hello"
add(true, false) ; compile error
```

## Enum
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

    let eql: `(self: MyEnum, other: MyEnum) -> bool = {
        self == other
    }
}

let x: MyEnum = MyEnum.x
let y = MyEnum.y ; type is inferred
x.eql(y) |> @println ; print false

match x {
    x => ... ; do something
    y, z => {} ; do nothing
}
```

## Struct
TODO:
- add a way to init a struct without its type like in zig or c99?
- support anonymous structs, useful for json, I think it's easy since struct is
  like a function that returns a type but syntax is ugly outside of a struct
  which is fine and intended
- make fields immutable by default? -> add mut keyword to fields
- pub/priv keyword on struct fields?
- do like C++/V for telling what is pub/mut?
```nov
let MyStruct = struct {
    name: string
    x: float = 1.
    y: float = 1.
    z: float = 1.

    let max = 100.

    let init: `(name: string) -> MyStruct = MyStruct{ .name = name }

    ; TODO: should we omit the type on self?
    let eql: `(self: MyStruct, other: MyStruct) -> bool = {
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
x |> @println ; idk what this prints
```

## Union
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

    let sum: `(self: Tree) -> int = {
        ; TODO: match with .empty?
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

## Generics
TODO

See [Result and Option unions](#result-and-option-unions) for example.

## Result and Option unions
The Result and Option unions are created with functions since types are values.
In nov there is no untagged union thus  we can match on any union to find the
active field. Note that it isn't represented here but an union field can have a
default value just like a struct field.

Sugar for Result and Option:
- !T is Result(T, string) or Result(T, any)?
- E!T is Result(T, E)
- ?T is Option(T)
- expr.! is unwrap or propagate for result
- expr.? is unwrap or propagate for option

TODO:
- allow for this syntax? `let a: MyUnion = .field{value}`, replace `{` with `(`?
  - current syntax is `MyUnion{ .field = value }` or `.{ .field = value }` with inference
- add a way to merge multiple error type into one?
- find a way to reduce nested Result
```
`(Result<T>, `(T) -> U) -> Result<U> ; replace the value type but keep the same error
`(Result<T>, `(T) -> Result<U>) -> Result<U> ; bind, see https://doc.rust-lang.org/std/result/enum.Result.html#method.and_then
```
- remove type sugar and only keep `.!` and `.?`?
- Check https://docs.vlang.io/type-declarations.html#custom-error-types
- Check https://docs.vlang.io/type-declarations.html#optionresult-types-and-error-handling
- Check https://doc.rust-lang.org/std/result/index.html

```nov
let Result = union<T, E> {
    ok: T
    err: E

    let newError: `(err: E) -> @This() = .{ .err = err }
    ; same as
    let newError: `(err: E) -> Result<T, E> = Result<T, E>{ .err = err }
}

let MyResult = Result<int, string>
let my_err = MyResult.newError("my error string")

; error handling
let file = match File.open("file.txt") {
    .ok => |f| f
    .err => |err| match err.kind {
        not_found => {
            ; do something
        }
        _ => return err ; return early with the error
    }
}
; .! unwrap and returns the err if there is any
let file = File.open("file.txt").!

let Option = union<T> {
    some: T
    none

    ; unions can also have methods
    @[public]
    @[inline]
    let orelse: `(self: @This(), fallback_value: T) -> T = {
        match self {
            .some => |val| val ; catch the value and return it
            .none => fallback_Value
        }
    }
}

let x = 5
let y = Option<int>{ .some = 5 }
let sum = x + y.orelse(0)

let MyOption = Option<float>
let a = MyOption{ .some = 1.0 }
let b = MyOption{ .none }
let prod = x.? * y.?
```

## Arrays
Side note about mutability. A constant array cannot be modified in any way, its
values are constant too. A mutable array can be reassigned/extanded and its
values can be modified.

TODO:
- keep arrays like that aka growable size list etc... or move to static array like zig?
  - probably continue like that because it's garbage collected and easier for programmer ig
- add a way to initialize the capacity of an array without adding any element
  - `let x = []int.initCapacity(50)` x is an array with len = 0 but with capacity = 50
  - use a builtin?
  - `let x: [50]int`
- add a way to repeat an array like a string, allow to use that as an initializer
  - `let x = [1].repeat(50)` x is an array of 50 int with value 1
  - `let x = [@as(uint, 1)].repeat(50)` x is an array of 50 uint with value 1
  - `let x: []uint = [1].repeat(50)` x is an array of 50 uint with value 1
- use zig/go synthax?

See [1](https://docs.vlang.io/v-types.html#array-methods) [2](https://docs.vlang.io/v-types.html#array-method-chaining)
```nov
let mut my_array = [1, 2, 3]
@TypeOf(my_array) ; returns []int
my_array.len == 3 ; true, should `len` be a function?
my_array[0] == 1 ; true
my_array[-1] == 3 ; true
my_array <<= 5 ; push operator for arrays
my_array |> @println ; prints [1, 2, 3, 5]
my_array += [1, 1, 7 ]
my_array |> @println ; prints [1, 2, 3, 5, 1, 1, 7]
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
        @print(w + " ")
    }
    @println()
}

; functional way, I think, add another way with .map()
; type annotation is optional
arr_arr >>= |arr: []string| {
    arr >>= |word| word + " " |> @print
    @println()
}
```

## Slice
TODO: https://docs.vlang.io/v-types.html#array-slices

## Map
TODO: https://docs.vlang.io/v-types.html#maps
Our implem is different, check src/value.zig.

## Match
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

## If/Else
TODO: add if unwrapping sugar for Result and Option?
```nov
let a = 10
let b = 20
; braces are mandatory, else is optional
if a < b {
    @println("${a} < ${b}")
} else if a > b {
    @println("${a} > ${b}")
} else {
    @println("${a} == ${b}")
}

; all If are expression which means that they all return a value
; the previous if returns `void`
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

## For loop
TODO: add else statement?
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

; "zig while" for loop
let mut i = 0
for i < 100 : i += 2 {
}

; else branch
; the else branch is evaluated when the loop is not exited with a break
let rangeHasNumber: `(begin: uint, end: uint, number: uint) -> bool = {
    var i = begin;
    return for i < end : i += 1 {
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
TODO: same as ~zig~ go

## In
Check if an element is in an array or if it's a key in a map.
This should be faster than using a for loop because it should be vectorized.
(with std.mem.indexOfScalar)
```
let nums = [1, 2, 3]
@println(1 in nums) ; true
@println(5 in nums) ; false
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
- Maps

## Integers
TODO
- c_int, ...
- u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f16, f32, f64, f80, f128
  - int = i32 or i64 based on architecture
  - uint = u32 or u64 based on architecture
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
| Name                  | Syntax            | Types                                        | Remarks                                                             |
|-----------------------|-------------------|----------------------------------------------|---------------------------------------------------------------------|
| Assignment            | a = b             | All types                                    | `a` is an identifier and `b` is any type.                           |
| Addition              | a + b <br> a += b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Concatenation         | a + b <br> a += b | string <br> [Arrays](#Arrays)                | TODO                                                                |
| Substraction          | a - b <br> a -= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Negation              | -a                | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Multiplication        | a * b <br> a *= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Division              | a / b <br> a /= b | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Remainder Division    | a % b <br> a %= b | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Left        | a << b            | [Integers](#Integers)                        | TODO                                                                |
| Bit Shift Right       | a >> b            | [Integers](#Integers)                        | TODO                                                                |
| Bitwise And           | a & b             | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Or            | a \| b            | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Xor           | a ^ b             | [Integers](#Integers)                        | TODO                                                                |
| Bitwise Not           | ~a                | [Integers](#Integers)                        | TODO                                                                |
| Optionify             | ?T                | All types                                    | Equivalent to Option(T)                                             |
| Optional Unwrap       | a.?               | Option                                       | Unwrap an Option type or return it's error.                         |
| Resultify             | E!T <br> !T       | All types                                    | Equivalent to Result(T, E) <br> Equivalent to Result(T, string).    |
| Result Unwrap         | a.!               | Result                                       | Unwrap a Result type or return it's error. (same as `try` in zig)   |
| Logical And           | a **and** b       | bool                                         | TODO                                                                |
| Logical Or            | a **or** b        | bool                                         | TODO                                                                |
| Boolean Not           | !a                | bool                                         | TODO                                                                |
| Equality              | a == b            | All types                                    | TODO                                                                |
| Inequality            | a != b            | All types                                    | TODO                                                                |
| Greater Than          | a > b             | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Greater or Equal      | a >= b            | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Less Than             | a < b             | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Less or Equal         | a <= b            | [Integers](#Integers) <br> [Floats](#Floats) | TODO                                                                |
| Push                  | a <<= b           | [Arrays](#Arrays)                            | TODO []T <<= T                                                      |
| Bind                  | a >>= \|b\| ...   | Monads                                       | TODO                                                                |
| Function Pipe         | a \|> f           | [Functions](#Functions)                      | TODO                                                                |
| Member Search         | a **in** b        | [Arrays](#Arrays) <br> Maps                  | TODO                                                                |
| Access                | a\[b]             | [Arrays](#Arrays) <br> Maps <br> string      | TODO: for Arrays and string b is an Integer, for Maps it's a key    |
| Field / Method Access | a.b               | All types                                    | TODO                                                                |
| Reference Type        | *T                | All types                                    | Create a reference type from `T`.                                   |
| Reference Of          | &a                | All types                                    | Returns a reference to `a`.                                         |
| Dereference           | a.*               | Reference                                    | Unwrap a reference type, this is done automatically when using `.`. |

## Precedence
```
x() x[] x.y x.? x.! x.*
E!T
x{}
!x -x ~x &x *T ?T
* / %
+ -
<< >>
& ^ | in
== != < > <= >=
and
or
|> >>=
= *= /= %= += -= <<=
```

## Operator Overloading
Operator overloading is possible on the following operators:
- `+`: `(T, T) -> T
- `-`: `(T, T) -> T
- `-`: `(T) -> T    unary negation (is it possible to autogen it from the binary op?)
- `*`: `(T, T) -> T
- `/`: `(T, T) -> T
- `%`: `(T, T) -> T
- `<`: `(T, T) -> bool
- `==`: `(T, T) -> bool
- TODO: `>>=`: \`(T, `(T) -> U) -> U
- TODO: `[]`: `(T\<U>, int) -> U

Note:
- `==` is automatically generated for all types by the compiler but can be overridden.
- `!=`, `>`, `<=`, `>=` are automatically generated when `==` and `<` are defined.
- `+=`, `-=`, `*=`, `/=`, `%=` are automatically generated when the corresponding operator is defined.
```nov
let Complex = struct {
    re: float
    im: float

    ; TODO: make the arg an enum so parsing is handled like an expr and it
    ; solve the binary/unary issue with `-`
    @[operator(+)]
    let add: `(self: Complex, other: Complex) -> Complex = {
        Complex{
            .re = self.re + other.re,
            .im = self.im + other.im,
        }
    }

    ; used by print
    ; signature must be `toString: (T) -> string` where T is the container type
    let toString: `(self: Complex) -> string = {
        "${self.re} + i${self.im}"
    }
}

let x = Complex{ .re = 5, .im = 3 }
let y = Complex{ .re = 2, .im = 7 }
x + y ; returns Complex{ .re = 7, .im = 10 }
```

## C FFI
TODO...

also add binding generator for zig?
```nov
@[extern("malloc")]
let c_malloc: `(uint) -> voidptr

@[extern("malloc")]
let c_malloc: `(uint) -> voidptr = {} ; error extern fn can't have body

@[export("nov_add")]
let add: `(a: c_int, b: c_int) -> c_int = a + b
```

## String Interpolation
`"{varname:[fill][alignment][width][.precision][type]}"`

Escape `{` and `}` with `\`.

## Async
TODO: add async/await/yield
- see go coroutines and other languages way of doing it
- see wren's [fibers](https://wren.io/concurrency.html)
- see https://docs.vlang.io/concurrency.html
- see https://buzz-lang.dev/guide/fibers.html

## Attributes
- `@[deprecated]` - `@[deprecated("message...")]`
- `@[warnif(cond, message)]` idk
- `@[operator(op)]`
- `@[pure]` useless? a function is pure if its args are immutable and if it doesn't return an error union? (no)
- `@[extern]`
- `@[packed]`
- `@[export]`
- `@[inline]`
- `@[noinline]`
- `@[cold]`
- `@[noreturn]` or have it as a return type?
- `@[test]` decl with it are ignored unless run with `nov test` where it behave like `zig test`
- `@[comptime]` run the decl function at comptime, TODO: or `@[assert]` idk it's for comptime assertion
- `@[public]`
- `@[private]`
- `@[entry]` idk, use it for main or just default to public main method?

Attributes can be set only on top level / container declarations

Side note on visibility:
- public: visible everywhere
- private: visible only in current file
- no attribute: visible when importing as a file (`@import("std.nov")`) but not
  when importing as a module (`@import("std")`)

TODO add a good way to make code usable on multiple systems through attributes.

# Inspirations
Zig, OCaml, Rust, C, V
