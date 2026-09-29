# The whole ZIO[R, E, A] as an okay row

Status: done, 2026-09-29. Owner lane: `zio-typed-row`.
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

- [x] `fromZIOTyped` reads the environment from the Reader: the same ZIO
      run under two `Reader.run` values sees each.
- [x] A typed ZIO failure `e` is `raise(e)` on the okay side:
      `runEither` answers `Left(e)`.
- [x] A ZIO defect (`die`) is not a typed failure: it fails the `Async`
      run with the same throwable.
- [x] `toZIOTyped` takes its Reader from ZIO's environment
      (`provideEnvironment`), and an okay `raise(e)` is ZIO's `fail(e)`:
      `.either` answers `Left(e)`.
- [x] A throwable escaping the okay program is a ZIO defect, not a typed
      failure.
- [x] Round trip: `toZIOTyped(fromZIOTyped(z))` answers as `z` for a
      success, a typed failure and a read environment.
- [x] Cancellation crosses as in `fromZIO`: cancelling the okay side
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
- Rows are widened by NAME inside the bridge (`!.widen`), not with `.at`:
  `.at[ZioRow[R, E]]` found no `Row.In` at abstract `R`, `E` (the rule in
  AGENTS.md: an obligation over a row is carried, never searched for).
- Observed: `.at[ZioRow[Greeting, String]]` fails even for CONCRETE
  arguments, while the same row spelled out
  (`Reader % ZEnvironment[Greeting] + Throws % String + Async`) resolves.
  The implicit search does not see through the parameterised alias. The
  docs say to spell the row out for `.at`; `ZioRow` stays for signatures.

## Results

- `scripts/gate.sh "okayZio/testOnly okay.zio.TestZioTypedRow"`: 7 passed.
- Mutant: a typed failure turned into a defect in `toZIOTyped` failed two
  tests ("raise is fail", "round trip"); restored.
