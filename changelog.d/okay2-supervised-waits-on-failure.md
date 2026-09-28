## okay2-supervised-waits-on-failure - okay2 twin: a failing scope answers after its children; cancel answers the fiber

- Ports the Scala 3 core's supervised-waits-on-failure and
  native-pool-stale-interrupt to okay2 (Scala 2.13).
- `Async.supervised` answers a failure from `whenIdle`, and the first
  error is kept.
- `PromiseDrive` settles on cancel.
- `Schedulers.forkJoin` answers a cancel that arrives while the task is
  queued, and interrupts its running task itself: `Future.cancel(true)`
  never interrupted a `ForkJoinTask`.
- Native's `pool` answers a skipped task, and holds its runner only
  while the body runs.
- Tests: TestAsyncCross covers a cancelled parked fiber that still
  completes, and a failing scope that answers after its children with
  the first failure winning. TestAsync covers the forkJoin queued cancel.
  That test failed first on the tests-only commit 09752fc60.
- Result: 69 tests green on JVM, JS and Native. The whole okay2 build
  is the gate.
