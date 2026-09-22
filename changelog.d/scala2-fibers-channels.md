## scala2-fibers-channels - fibers and channels for Scala 2.13, closing the operator's list

This is the last queued item of the Scala 2.13 arc. Effects,
continuations, a user's own effects, streams, fibers and channels are
all in `okay.scala2` now. Direct style is not planned: it is Scala 3
macros, and the operator agreed it is not needed for now.

- `Async.fork` (the core's `Async.spawn`), `par`, `race`, `sleep` and
  `timeout`, over the platform's own Scheduler and Timer.
- `Fiber[A]`: `join`, `joinEither`, `cancel`, each an `Eff[Async, _]`.
- `Channel[A](capacity)`: `send` and `receive` are programs that wait
  (while full and while empty). Also `offer`, `close`, `isClosed`,
  and `source`, which reads the channel as a `Source` until it closes.
- The 2.13 probe has 34 tests, all green on their first run under
  `-Xlint -Werror`, including a producer and consumer moving 1 000
  elements through a channel of capacity 4, and cancelling a sleeping
  fiber.
- Docs: a Fibers and channels section in docs/modules/okay-scala2.md
  (the example is copied from the probe), a "Not planned" section, and
  a guide sentence. Spec stage 5 has its boxes checked, and no later
  stages remain queued.
