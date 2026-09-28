## native-pool-stale-interrupt - a cancel of a finished Native pool fiber no longer interrupts the worker's next task

- `Schedulers.pool`'s `Task` (okay-platform, scala-native) set `runner`
  when its body started and never cleared it. Cancelling a fiber that had
  already finished therefore interrupted whatever its worker ran next,
  which broke the promise in its own comment ("never a later, unrelated
  one"). An interrupt that stayed set after the body could also have
  thrown the worker out of `TaskQueue.take`.
- `runner` is now held only while the body runs, under a monitor that
  `cancel` also takes. The `finally` clears it and consumes that task's
  own interrupt. This is the same shape as the JVM `forkJoin` fix in
  supervised-waits-on-failure.
- TestNativeScheduler pins it. The test failed first (the stale cancel
  turned the next task's `Right(2)` into a `Left`) and passes now, together
  with TestAsyncCross on Native. specs/cross-platform-async.md, Decisions.
