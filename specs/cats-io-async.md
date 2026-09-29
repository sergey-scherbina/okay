# cats IO across without a parked thread

Status: in progress, 2026-09-29. Owner lane: `cats-io-async`.
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

- [ ] `fromIO` is an `Async.await` on `unsafeToFutureCancelable`: under
      `Async.runAsyncCancellable` no thread is parked while the IO runs.
- [ ] Cancelling the okay drive cancels the IO (its `onCancel` finalizer
      runs), and a late result resumes nothing.
- [ ] An IO failure crosses as the same throwable; `runWith` still works.
- [ ] `toIOAsync` runs a callback-driven okay program as an `IO` without
      the blocking runner; a failure fails the IO.
- [ ] Cancelling the IO fiber cancels the okay drive: the pending Await's
      canceller runs once and a later callback resumes nothing.
- [ ] `toIO` keeps `IO.blocking` (an okay `Async.Run` may block).

## Decisions

- `fromIO` CHANGES, as `fromZIO` did (specs/zio-direct-cancel.md): the
  blocking form is never better, and it could not be cancelled.
- `toIOAsync` is a second door, as `toZIOAsync` is: only a program whose
  waits are `Await`s is safe off the blocking pool.
