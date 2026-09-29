# The whole ZIO[R, E, A] as an okay row

Status: in progress, 2026-09-29. Owner lane: `zio-typed-row`.
Follows `specs/zio-direct-cancel.md`, which crosses `Task[A]` only.

## Goal

A ZIO's three channels land on okay's three effects, and back:

| ZIO | okay |
|---|---|
| environment `R` | `Reader % ZEnvironment[R]` |
| typed failure `E` | `Throws % E` |
| running, interruption, defects | `Async` |

```scala
type ZioRow[R, E] = Reader % ZEnvironment[R] + Throws % E + Async
```

## Interface

```scala
object ZioInterop:
  def fromZIOTyped[R, E, A](z: ZIO[R, E, A], runtime: Runtime[Any] = Runtime.default): A ! ZioRow[R, E]
  def toZIOTyped[R, E, A](p: => A ! ZioRow[R, E]): ZIO[R, E, A]
```

## Behaviour

- [ ] `fromZIOTyped` reads the environment from the Reader: the same ZIO
      run under two `Reader.run` values sees each.
- [ ] A typed ZIO failure `e` is `raise(e)` on the okay side:
      `runEither` answers `Left(e)`.
- [ ] A ZIO defect (`die`) is not a typed failure: it fails the `Async`
      run with the same throwable.
- [ ] `toZIOTyped` takes its Reader from ZIO's environment
      (`provideEnvironment`), and an okay `raise(e)` is ZIO's `fail(e)`:
      `.either` answers `Left(e)`.
- [ ] A throwable escaping the okay program is a ZIO defect, not a typed
      failure.
- [ ] Round trip: `toZIOTyped(fromZIOTyped(z))` answers as `z` for a
      success, a typed failure and a read environment.
- [ ] Cancellation crosses as in `fromZIO`: cancelling the okay side
      interrupts the ZIO fiber.

## Decisions

- `ZEnvironment[R]`, not `R`: a ZIO environment is an intersection of
  services (`Db & Log`), and `ZEnvironment` is the value that carries one.
  One `Reader` per row holds it all (a row may hold ONE Reader — Reader.scala).
- Defects stay defects: ZIO separates a typed failure from a defect, and
  so does this bridge — `Throws % E` for the first, the `Async` failure for
  the second. Folding a defect into `E` would need `E >: Throwable`.
- `toZIOTyped` runs the okay side through `toZIO` (blocking pool): it must
  be right for any program, including a blocking `Async.Run`.
