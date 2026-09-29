# Throws and Abort

Typed failure: the program stops at the first error, and the error's
type is in the signature. `A ! Throws % E` reads "computes A, or fails
with an E". `Abort` is `Throws % Unit`, a failure with nothing to say.

## Operations and handlers

| | |
|---|---|
| `raise(e)` | fail with `e` |
| `abort` | fail with nothing to say |
| `either.orRaise` | an `Either` as a program |
| `p.recover(h)` / `p.orElse(q)` | handle an error inside the program |
| `runEither(p)` | handle it: `Right(answer)` or `Left(error)` |
| `runOption(p)` | handle `Abort`: `Some(answer)` or `None` |
| `catching(a)` | a Scala 3 `throws` call as a program |

## Example

```scala
def parse(s: String): Int ! Throws % String =
  s.toIntOption match
    case Some(n) => pure(n)
    case None    => raise(s"not a number: $s")

val good = !.run(runEither(parse("42")))   // Right(42)
val bad  = !.run(runEither(parse("x")))    // Left(not a number: x)

val nothing = !.run(runOption(abort[Int]))   // None
```

## Which error effect

- **Throws** stops at the first error. Use it when later steps need the
  earlier answers.
- **[Maybe](maybe.md)** is "not there" without a reason, and can sit in
  one row with `Throws`, which `Abort` cannot.
- **[Chronicle](chronicle.md)** records errors and goes on, stopping only
  where it must.
- **`Validated`** is not an effect but an applicative: independent checks
  report every error at once, as a form or a configuration should.

A row holds one `Throws`: its operations are told apart by class.

See also: `src/main/scala/Throws.scala`, `src/main/scala/Validated.scala`.
