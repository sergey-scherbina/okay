# Maybe

A value that may not be there, as an effect: `Some(x).maybe` answers `x`,
`None.maybe` stops the program, and `Maybe.run` answers `None` for it. The
`Option` moves out of every signature along the way and into the handler
at the end.

## Operations and handlers

| | |
|---|---|
| `option.maybe` | the value, or stop |
| `Maybe.none[A]` | stop: nothing is there |
| `Maybe.run(p)` | handle it: `Some(answer)` or `None` |

## Example

```scala
val ages = Map("ada" -> 36)
def age(name: String): Int ! Maybe = ages.get(name).maybe

val found   = !.run(Maybe.run(age("ada")))   // Some(36)
val missing = !.run(Maybe.run(age("bob")))   // None
```

## Why not Abort

`Abort` is `Throws % Unit`, and a row holds one `Throws`. So a program
that can both find nothing and fail with a reason cannot say
`Abort + Throws % E`. `Maybe` is its own effect, so `Maybe + Throws % E`
is a good row, and "was it not there, or did it break?" has two answers
from two handlers.

See also: `src/main/scala/Maybe.scala`, [Throws](throws.md).
