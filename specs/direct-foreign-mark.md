# A foreign effect marked inside a direct block

Status: done, 2026-09-29. Owner lane: `direct-foreign-mark`.
Follows `specs/zio-direct-cancel.md` (ZIO inside `direct[Task]`) and
`specs/zio-typed-row.md` (the whole `ZIO[R, E, A]` as a row).

## Goal

Inside a `direct` block over an okay PROGRAM, a value of a foreign effect
(`Future`, ZIO, cats `IO`) is bound with the same marks as an okay
operation:

```scala
val p: Int ! Async = direct {
  val a = Future(20).?
  val b = ZIO.attempt(22).?
  a + b
}
```

Today the macro accepts the block's own `F[T]`, an operation of its row,
a program of a narrower row, or a value with a `Layered` layer, and
refuses anything else.

## Interface

```scala
package okay   // okay-async: no dependency on any foreign library

trait ForeignEffect[M[_]]:
  type G[+X]
  def lift[A](m: M[A]): A ! G

object ForeignEffect:
  given future: ForeignEffect[Future]              // G = Async, an Await on completion

package okay.zio     // import okay.zio.given
  ZIO[Any, E <: Throwable, _]  -> Async                       (fromZIO)
  ZIO[Any, E, _]               -> Throws % E + Async
  ZIO[R, E, _]                 -> ZioRow[R, E]                (fromZIOTyped)

package okay.cats    // import okay.cats.given, an IORuntime in scope
  IO -> Async                                                  (fromIO)
```

The macro's new case: the marked value is not a program, has no
`Layered` layer, and a `ForeignEffect[M]` is found for its type
constructor (for a type with several arguments, all but the last fixed:
`ZIO[R, E, _]`). The mark binds `lift(m)`, which is then widened into the
block's row by the same subtyping proof a narrower program gets
(`narrowRow`): `G` must be a member of the block's row, or the block is
refused naming both.

## Behaviour

- [x] `Future(20).?` inside `direct` over `Async` binds 20; nothing runs
      before the program does (the Future's own eagerness aside).
- [x] A failed Future fails the program with the same throwable.
- [x] A ZIO `Task` is marked with `.?` and `.reflect` inside a block over
      `Async`; a ZIO with a typed error inside a block over
      `Throws % E + Async` raises `E`; a ZIO with an environment inside a
      block over `ZioRow[R, E]` reads it from the Reader.
- [x] A cats `IO` is marked with `.?`, `.reflect` and `!io`.
- [x] A foreign value whose `G` is not in the block's row is refused at
      compile time, naming the row.
- [x] Without the given (no `import okay.zio.given`), the old refusal
      stands.

## Decisions

- Prefix `!z` stays refuted for ZIO (specs/zio-direct-cancel.md): ZIO's
  own `unary_!` wins over any extension. Cats `IO` and `Future` have none,
  so `!io` works.
- The typeclass lives in okay-async, beside `Async`, so okay-direct needs
  no dependency on ZIO or cats; each library's instance lives in its own
  interop module, behind that module's `given` import (AGENTS.md: every
  dependency behind an abstraction).
- ZIO has three instances, most specific first: without an environment
  and with a Throwable error it is a plain `Async` operation; without an
  environment it adds `Throws % E`; otherwise the whole `ZioRow[R, E]`.
- `G` is a type MEMBER, not a parameter. The first cut had
  `ForeignEffect[M[_], G[+_]]` and the macro searched
  `ForeignEffect[M, ?]` with an empty-bounds wildcard for `G`: the search
  found nothing, even for `Future` with its given in the companion (the
  three Future tests stayed red with the old refusal). With `G` a member
  the search is `ForeignEffect[M]`, and `lift`'s `A ! found.G` dealiases
  through the given's own `type G[+X] = Async[X]`.
- A ZIO's error type must MATCH the row's `Throws`: `Throws` is invariant
  in `E`, so a `ZIO[R, Nothing, A]` (as `ZIO.serviceWith` infers) lands on
  `Throws % Nothing` and does not fit a row at `Throws % String`. Ascribe
  the ZIO (`ZIO[Greeting, String, String]`), as the test does.

## Results

- okay-direct TestDirectForeign (Future): 5 passed, watched red first
  (the old refusal on all three marked Futures).
- okay-zio TestZioForeign: 4 passed; okay-cats TestCatsForeign: 2 passed.
