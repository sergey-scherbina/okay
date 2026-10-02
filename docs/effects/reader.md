# Reader

A value the whole program can read and nobody can change: a
configuration, a request, a clock, a set of dependencies. `R ! Reader % E`
reads "computes R, with an E to read".

## Operations and handlers

| | |
|---|---|
| `Reader.ask[E]` | the environment |
| `Reader.read[E, T]` | one part of it, found by type (`Has[E, T]`: a tuple member or a case-class field) |
| `p.handle(Reader(e))` | handle it: give the program its environment |
| `Reader.run(e)(p)` | handle it: give the program its environment |
| `Reader.local(f)(p)` | run `p` under a changed environment, the outer one untouched |
| `Reader.lift(cf)` / `unlift(p)` | to and from a context function `E ?=> A` |

`Reader.run` is a relay: every `ask` is answered in place and the handler
never captures a continuation, so it costs about as much as passing a
parameter.

## Example

```scala
val greet: String ! Reader % String =
  Reader.ask[String].map(name => s"hello, $name")

val hello = greet.handle(Reader("ada")).run   // "hello, ada"

val shout = Reader.local[String, String, Pure](_.toUpperCase)(greet).handle(Reader("ada")).run   // "hello, ADA"
```

## When to use it

Reach for Reader when many layers need the same value and threading it
through every signature would be noise. When the value is a service
rather than data, context functions are often simpler
([capabilities](../capabilities.md)); `Reader.lift` and `unlift` move
between the two.

See also: `src/main/scala/Reader.scala`, [the guide](../guide.md),
[State](state.md) for a value the program may change.
