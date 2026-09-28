## drive-interrupts-blocking-run - a cancel interrupts a drive's blocking Run, as Loom's does; a fiber resumed inline is not hit by another's cancel

- Found by the default flip (scheduler-default-flip): TestAsync's "a bracket
  cancelled by timeout releases its resource" went red the moment
  `adaptive` was the given. Loom's cancel interrupts the fiber's thread, so
  a use blocked in a `Run` (`Thread.sleep`, a JDBC call, a `CanBlock` park)
  throws and its bracket releases at the cancel. A drive (`own`,
  `adaptive`, `drive`) only stopped between operations: the use kept its
  resource until the blocking call returned by itself — five seconds in
  the law, for ever for an `accept()`.
- `Async.Drive` has slice hooks around each run of `apply` on a thread
  (no-ops on JS). `Schedulers.DriveTask` uses them: `runner` names the
  thread running the drive's code, `cancel` interrupts it under the task's
  monitor, and the slice takes its own cancel's interrupt back before it
  leaves — a pooled worker must never keep one, or its `park` becomes a
  spin. A slice of ANOTHER drive nested on the same thread (a wake resumed
  inline, the common case on a callback drive) suspends the outer one, so
  an outer cancel is delivered when the inner slice is over, not into the
  inner fiber's code.
- Laws (TestAsync): the bracket law now runs on loom, adaptive and own,
  each followed by a fiber on the same single worker that would meet a
  leftover interrupt; and "a cancel interrupts only its own drive's code"
  — fiber A wakes B inline, A is cancelled while B sleeps there, B answers
  42. A mutant without the suspension fails that law; okayPlatformJVM
  178/178.
