- [ ] zio-direct-cancel — ZIO in direct style and a cancellable way back
      (specs/zio-direct-cancel.md). (1) `given Monad[ZIO[R, E, _]]` in
      okay-zio, so `direct[Task] { val x = z.?; ... }` binds ZIO values
      with okay's own marks; (2) `fromZIO` through `Async.await`: no parked
      thread under the callback runner, and cancelling the okay side
      interrupts the ZIO fiber; (3) `p.zio` / `z.okay` extensions. Done
      when TestZioDirect passes and TestZioInterop stays green.
