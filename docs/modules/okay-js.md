# okay-js

> JavaScript as a value: a typed tree, a printer, a macro that emits
> the text as a compile-time constant, and a `js { }` block that
> builds the tree from a deliberately small subset of Scala. Not a
> compiler — that is Scala.js, and this module says so when you leave
> the subset.

Depends on: nothing. Pure Scala — cross-built for JVM, JS and Native;
the suite runs on all three.

## Guide

**JavaScript is a tree, and printing is its interpreter.** That is
the move the rest of this codebase makes: `Ui` is a tree that `Html`,
`React` and `Frame` interpret, `Schema` is a tree the codecs
interpret, `Json` is a tree. So:

```scala
import okay.js.*

val hello = Js.Call(Js.Field(Js.Name("console"), "log"), Vector(Js.Str("hi")))
Js.print(hello)                  // console.log("hi")
```

Expressions (`Js`) and statements (`Stmt`) are different types,
because JavaScript separates them and a printer that pretends
otherwise emits `var x = if (…)`.

### Two roads out of one tree

```scala
Js.print(tree)                       // runtime: built when called
inline val src = Emit.emit(tree)     // compile time: a constant in the jar
```

`Emit` is `transparent inline`, so the result carries the singleton
type of the text: an `inline val` that compiles is proof the work
happened while the compiler ran. A tree the compiler cannot read is a
compile error naming what it could not read, never a silent fall back
to runtime.

### `js { }` — the tree from plain Scala

```scala
val program = Direct.js {
  val n = 0
  while (n < 10) {
    global.console.log(n)
  }
}
```

**The subset is closed and small, and everything outside it is a
compile error that points at Scala.js.** That is the whole design: an
approximation of Scala's semantics in a browser fails by being
*wrong*, not by failing to build, so nothing is approximated. What is
in it:

| Scala | JavaScript |
| --- | --- |
| `val x = e`, `var x = e` | `var x = e;` |
| `x = e` | `x = e;` |
| `if (c) a else b` | statement, or `c ? a : b` in expression position |
| `while (c) { … }` | `while (c) { … }` |
| `+ - * / %`, `< > <= >=`, `&& \|\| !` | the same operators |
| `==` and `!=` on a primitive | `===` and `!==` |
| `d.f`, `d.f(a)` on a `Dyn` | `d.f`, `d.f(a)` |
| `(a, b) => …` | `function (a, b) { … }` |
| a value of type `Js` | spliced in place |

`global` is a `Dyn`: a `scala.Dynamic` whose only job is to let
`global.console.log(x)` typecheck so the macro can read it.

**`==` becomes `===`, and only where that is honest.** Scala's `==`
means equality; JavaScript's `==` coerces and `===` does not. The
macro maps to `===` when the operand type is `Int`, `Long`, `Double`,
`String`, `Boolean` or `Dyn`, and refuses otherwise rather than
guessing at what equality means for some other type.

### The escape hatch is named

`Js.Raw` and `Stmt.Raw` carry source the tree cannot express — a
regex literal, a `try/catch`. They exist so that one awkward line does
not push the whole file back to string concatenation, and
`Js.raws(program)` counts them, so a test can hold a program to zero.

### Escaping is a security property

`Js.quote` escapes `<` so that `</script` cannot end the document the
script is written into, and U+2028/U+2029 because they are line
terminators in JavaScript and are not in JSON. A generator whose
output can be ended by its own data is a cross-site scripting hole,
and putting data inside scripts is what this module is for.

## Spec

`specs/js.md`.
