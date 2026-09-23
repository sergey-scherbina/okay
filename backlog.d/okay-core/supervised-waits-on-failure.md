- [ ] **supervised-waits-on-failure — the scope's failure answer does not
      wait for its cancelled children** (found by supervision-shapes-race,
      2026-09-23). `Async.Nursery`'s header promises "the scope does not
      finish while a child is still running", and on SUCCESS it keeps it
      (`whenIdle`). On the FIRST FAILURE, however, `supervised` runs
      `cancelAll()` and answers `Left(e)` at once. A child that the cancel
      reached between its `await`'s registration and the drive storing the
      canceler gets that canceler called by its own drive a moment LATER
      (`Drive.op`: `if stopped then cancelReg()`), so the scope's answer can
      come before the child has stopped. Measured: 6 of 9 cancellations had
      arrived when `supervised` answered; all 9 arrived within milliseconds.
      The fix the header implies is to answer `Left(e)` from `whenIdle`,
      which is safe on the JVM, where `DriveTask.cancel` answers the fiber at
      once. It is NOT obviously safe on JS: `PromiseDrive` has no cancel
      that answers the promise, so a cancelled child parked in an `await`
      might never settle, and the scope would hang instead of failing.
      Decide per platform, with a test on each, before changing it.
      TestSupervisionShapes now waits (bounded) for cancellations to ARRIVE
      rather than asserting them at the instant of the answer.
