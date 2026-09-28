# Non-blocking cancellable ZIO bridge

## Overview

`okay-zio` currently turns an `A ! Async` program into `Task[A]` by
calling its blocking `runWith` interpreter inside `ZIO.attemptBlocking`.
Add a second bridge for callback-driven `Async` programs: ZIO must not park a
thread while an `Await` is pending, and interrupting the ZIO fiber must cancel
the active Okay drive and its registered callback.

## Interface

```scala
object Async:
  final class Running[+A]:
    def future: scala.concurrent.Future[A]
    def cancel(): Unit

  def runAsyncCancellable[A](prog: A ! Async): Running[A]

object ZioInterop:
  def toZIOAsync[A](p: => A ! Async): Task[A]
```

`Running.cancel()` is idempotent. It unregisters a pending `Await`, releases
open cancel scopes, and completes `future` with `CancellationException` when
the program has not already completed.

`toZIOAsync` is a non-blocking wait bridge. It starts the callback drive and
settles the ZIO callback with the same success or failure. ZIO interruption
cancels the `Running` drive. The existing `toZIO` remains the blocking bridge
for programs containing potentially blocking `Async.Run` work.

## Behavior

- [x] A callback-only `Async.await` program becomes a successful `Task` without
      using the blocking `runWith` path.
- [x] A failure supplied to an `Async.await` callback fails the `Task` with the
      same throwable.
- [x] Interrupting the ZIO fiber invokes the active `Await` canceller exactly
      once and prevents a later callback from resuming the program.
- [x] Cancelling `Async.runAsyncCancellable` unregisters its pending callback
      and fails its future with `CancellationException`.
- [x] `toZIO` retains its `attemptBlocking` implementation and contract.

## Out of scope

- Making arbitrary `Async.Run` computation non-blocking. `Async.Run` may block
  by definition; use `toZIO` for that case.
- Translating ZIO interruption into `Thread.interrupt` for existing `toZIO`.
- Replacing ZIO's scheduler or changing `fromZIO`.

## Design

`Async.PromiseDrive` already owns exactly the cancellation behaviour needed by
the bridge, but `runAsync` exposes only its `Future`. `Running` exposes the
same drive's future and `cancel` method without exposing `Drive` itself.

`toZIOAsync` uses `ZIO.asyncInterrupt`: register the completion callback on
the `Running.future` with `ExecutionContext.parasitic`, then return a ZIO
canceller that invokes `Running.cancel`. This preserves callback semantics and
does not occupy ZIO's blocking executor while an `Await` is pending.

## Decisions

- **Add `toZIOAsync`; retain `toZIO`.** A silent replacement would run
  `Async.Run` on the ZIO caller thread because a callback drive executes `Run`
  eagerly. Keeping the names explicit makes the blocking boundary visible.
- **Expose `Running`, not `Drive`.** Consumers need a future and cancellation,
  not access to the interpreter's state machine.
- **Use `ZIO.asyncInterrupt`, not `ZIO.fromFuture`.** `fromFuture` observes a
  future but has no way to unregister the active Okay `Await` on interruption.

## Results

`Async.runAsyncCancellable` exposes the callback driver's cancellation door;
`ZioInterop.toZIOAsync` maps it into `ZIO.asyncInterrupt`. Focused gates
passed: `okayPlatformJVM/testOnly okay.TestAsyncCross` (20 results) and
`okayZio/testOnly okay.zio.TestZioInterop` (7 results). The affected gate's
only initial failure was the newly added board item's malformed heading;
`scripts/board.sh --check` and `okayDeploy/testOnly okay.deploy.TestBoardEntries`
(7 results) pass after its correction.
