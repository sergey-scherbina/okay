# ZIO in direct style, and a cancellable way back

Status: done, 2026-09-29. Owner lane: `zio-direct-cancel`.
Follows `specs/zio-async-bridge.md` (okay → ZIO without a parked thread).
Next lanes: `direct-foreign-mark` (`!z` inside a block over an okay
program), `zio-typed-row` (the whole `ZIO[R, E, A]`).

## Goal

A ZIO value is bound in a `direct` block with okay's own marks, and the
two sides cross in both directions with cancellation carried across.

```scala
import okay.Direct.*
import okay.zio.given

val t: Task[Int] = direct[Task] {
  val a = ZIO.attempt(20).?
  val b = ZIO.attempt(22).reflect
  a + b
}
```

## Interface

```scala
package okay.zio

given zioMonad[R, E]: okay.Monad[[A] =>> ZIO[R, E, A]]

object ZioInterop:
  def fromZIO[A](z: Task[A], runtime: Runtime[Any] = Runtime.default): A ! Async

extension [A](p: => A ! Async) def asZIO: Task[A]        // = toZIO(p)
extension [A](z: Task[A]) def asOkay: A ! Async          // = fromZIO(z)
```

## Behaviour

- [x] `direct[Task]` binds ZIO values with `.?` and `.reflect`; the
      block is a `Task` and nothing runs until ZIO runs it.
- [x] A ZIO failure inside the block fails the whole `Task` and skips the
      rest of the block.
- [x] The instance is ZIO's own `flatMap`: a block binding in a loop of
      100 000 iterations does not overflow the stack.
- [x] `fromZIO` is an `Async.await`: under `Async.runAsyncCancellable` no
      thread is parked while the ZIO runs.
- [x] Cancelling the okay drive interrupts the ZIO fiber (its finalizer
      runs), and a late ZIO result does not resume the program.
- [x] A ZIO failure crosses as the same throwable; `runWith` still works.
- [x] `p.asZIO` and `z.asOkay` are `toZIO` and `fromZIO`.

## Decisions

- REFUTED: prefix `!z` on a ZIO value. ZIO declares its own
  `unary_!(implicit ev: A <:< Boolean)`, and a member beats an extension,
  so `!ZIO.attempt(2)` is ZIO's negation and fails with "Cannot prove that
  Int <:< Boolean" (observed, the first compile of TestZioDirect). No
  import can change that: on ZIO values the marks are `.?` and `.reflect`.
  The same holds for `direct-foreign-mark`.

- `fromZIO` CHANGES rather than gaining an async twin. It used to block the
  okay thread in `runtime.unsafe.run`; there is no case where the blocking
  form is better (under `runWith` an `Await` parks exactly as before), and
  the old one could not be cancelled. `toZIO` keeps its blocking twin for
  the opposite reason: an okay `Async.Run` may block, and only
  `attemptBlocking` is safe for it.
- `p.asZIO` is `toZIO`, not `toZIOAsync`: it must be right for every
  program, including ones with blocking `Async.Run`. The non-blocking road
  stays a named choice.
- The Monad is ZIO's `flatMap`/`succeed`, `fmap` is ZIO's `map`: stack
  safety is ZIO's trampoline, so no recursion of ours to bound.

## Results

- Watched red first: with the Monad and the extensions in place and the
  OLD `fromZIO`, the "parks no thread" test hung at TestZioDirect.scala:60
  (the gate's stall watchdog killed it at 480 s): `async(unsafe.run(z))`
  runs in place under the callback runner and blocked on a ZIO that only
  the test thread, now stuck, could complete. With `fromZIO` as an
  `Async.await` on a forked fiber the same test passes.
- `scripts/gate.sh "okayZio/testOnly okay.zio.TestZioDirect
  okay.zio.TestZioInterop"`: 14 passed, 0 failed, no warnings.
