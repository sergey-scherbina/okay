# Chronicle

Errors that accumulate in a program that goes on. `Throws` stops at the
first error; `Validated` collects every error but only across checks that
do not depend on each other. `Chronicle` sits between them: a program
RECORDS an error and continues, and stops only where it cannot go on.

## Operations and handlers

| | |
|---|---|
| `Chronicle.dictate(e)` | record `e` and continue |
| `Chronicle.halt` | stop here |
| `Chronicle.confess(e)` | record `e` and stop |
| `Chronicle.all(xs)(f)` | run `f` over every element, keeping every error |
| `Chronicle.run(p)` | handle it: a `Verdict` |

The `Verdict` is `Clean(answer)` when nothing was recorded,
`Warned(answer, errors)` when errors were recorded and the program still
finished, and `Failed(errors)` when it halted.

## Example

```scala
def host(s: String): String ! Chronicle % String =
  if s.contains("_") then Chronicle.dictate(s"'$s' has an underscore").map(_ => s)
  else pure(s)

val clean  = !.run(Chronicle.run(host("db")))      // Clean(db)
val warned = !.run(Chronicle.run(host("my_db")))   // Warned(my_db, Vector('my_db' has an underscore))

val failed = !.run(Chronicle.run(Chronicle.confess[String, String]("no host")))   // Failed(Vector(no host))
```

The name and the operations come from Haskell's `these` package. The
same idea is cats' `Ior` and arrow-kt's `Raise.accumulate`.

See also: `src/main/scala/Chronicle.scala`, [Throws](throws.md).
