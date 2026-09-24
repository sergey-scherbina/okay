## okay2-interop-async - the Async bridge and scoped stream sources in okay2's cats/fs2/zio

okay2's interop was written before okay2 had `Async` and `Resource`;
now it uses both, as the Scala 3 core's interop does and in two places
further. `CatsInterop.toIOBlocking` / `ZioInterop.toZIO` run an `Async`
program on the other side's blocking pool; `fromIO` / `fromZIO` are
one of theirs as an `Async` operation, answered by callback and
CANCELLED when the waiting side gives up (the Scala 3 core parks a
virtual thread in `unsafeRunSync`); `scheduler` is okay2's `Scheduler`
on their runtime. `fromFs2` and `fromZStream` are scoped pulls under
`Resource`, one chunk per operation: fs2 runs into a bounded queue (its
backpressure), zio opens its scope and pull once; the release cancels
the fs2 fiber / closes the zio scope, so a consumer that stops early
runs the stream's finalizers then (the Scala 3 core waits for the end).
The first `fromFs2` compiled each step and closed the stream's scope
under its own tail; the first `fromZStream` collected the whole stream.
Core: `Failing.both[F, G]` (explicit, as `Handler.union`) and
`Writer.failing` (a tell cannot fail), so a scope over `Writer + Io`
runs. Mutants of both releases turn the early-stop tests red. Backlog
item closed; docs §9.
