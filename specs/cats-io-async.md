# cats IO across without a parked thread

Status: done, 2026-09-29. Owner lane: `cats-io-async`.
The cats twin of `specs/zio-async-bridge.md` and `specs/zio-direct-cancel.md`.

## Goal

`CatsInterop.fromIO` blocked a virtual thread in `unsafeRunSync`, and no
cancellation crossed. Both directions now wait by callback, and
cancellation crosses both ways.

## Interface

```scala
object CatsInterop:
  def fromIO[A](io: IO[A])(using IORuntime): A ! Async      // now an Await
  def toIOAsync[A](p: => A ! Async): IO[A]                  // new
  def toIO[A](p: => A ! Async): IO[A]                       // unchanged: IO.blocking
```

## Behaviour

- [x] `fromIO` is an `Async.await` on `unsafeToFutureCancelable`: under
      `Async.runAsyncCancellable` no thread is parked while the IO runs.
- [x] Cancelling the okay drive cancels the IO (its `onCancel` finalizer
      runs), and a late result resumes nothing.
- [x] An IO failure crosses as the same throwable; `runWith` still works.
- [x] `toIOAsync` runs a callback-driven okay program as an `IO` without
      the blocking runner; a failure fails the IO.
- [x] Cancelling the IO fiber cancels the okay drive: the pending Await's
      canceller runs once and a later callback resumes nothing.
- [x] `toIO` keeps `IO.blocking` (an okay `Async.Run` may block).

## Decisions

- `fromIO` CHANGES, as `fromZIO` did (specs/zio-direct-cancel.md): the
  blocking form is never better, and it could not be cancelled.
- `toIOAsync` is a second door, as `toZIOAsync` is: only a program whose
  waits are `Await`s is safe off the blocking pool.

## Results

- `scripts/gate.sh "okayCats/testOnly okay.cats.TestCatsIOAsync
  okay.cats.TestCatsInterop okay.cats.TestCatsForeign"`: 13 passed.
- Mutant: `fromIO`'s canceller made a no-op — "cancelling the okay side
  cancels the IO" failed ("the IO finalizer ran", 5 s); restored.
- The old `fromIO` was not re-run against the new tests: it is the
  `async(unsafeRunSync)` shape whose twin, the old `fromZIO`, hung the
  same "parks no thread" test until the stall watchdog killed it
  (specs/zio-direct-cancel.md, Results).
