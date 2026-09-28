## drive-resume-throw-lost - a throw in a drive's late continuation fails the fiber instead of vanishing

- A drive (`own`, `adaptive`, `drive`, JS's `PromiseDrive`) resumed by a
  callback ran `apply(k(x))`: the continuation `k(x)`, the fiber's own
  code, was evaluated as the ARGUMENT, before `apply`'s try. A throw there
  went to whoever called the callback — a child fiber finishing, a timer,
  a producer — and was lost with it. The fiber never answered: nothing
  parked, no thread ran it, `onComplete` never fired.
- Found by the default flip: okay-pool's TestPoolElastic hung 3 of 3 on
  own, drive and adaptive and passed 3 of 3 on Loom (whose continuation
  runs inside the blocking handler's loop). `Cluster.Rescale`, thrown in
  such a continuation, never reached `Pool.nudge`'s `onComplete`, so the
  run's attempt stayed "running" and every later nudge was refused. A
  temporary probe named it: the attempt's trace ended in the Rescale, the
  fiber's `onComplete` never ran, and no drive was parked.
- The callback now hands `apply` the continuation to run
  (`Free.Return(x).flatMap(k)`, rotated by `resume` like any other bind),
  so it runs inside the try and inside the slice. RED FIRST, TestAsync "a
  continuation that throws after an Await answered LATER fails its own
  fiber, on every drive": the throw reached the answering thread before
  the fix. After: okayPlatformJVM 179/179, TestPoolElastic 0.3 s on
  `adaptive`, twice.
