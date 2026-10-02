## fiber-is-done - Fiber.isDone: whether a fiber has its answer, without waiting

- `f.isDone` on any `Fiber`: true once the fiber has a value, a failure or
  the cancellation it answers with; a snapshot that never turns back. For a
  watchdog, a test or a progress line — a wait is still `onComplete` /
  `joinAsync` / `join`.
- The trait member is `answered` and `isDone` is the extension in
  `object Fiber` (found without an import): the JVM's pooled fiber IS a
  `ForkJoinTask`, whose final `isDone` means the pool task returned — which
  a parked fiber's has, long before its answer.
- Implemented on every scheduler: Loom/threads/`CompletableFuture`, the
  pooled `DriveTask` (its answer cell), Native's `FiberCell`, JS's
  `Promise`, okay-cats' `Future`, okay-zio's `fiber.unsafe.poll`.
- Tests: `TestFiberIsDone` (loom, own, default, threads, drive: running,
  then a value, a failure, a cancellation). docs/guide.md, the Fiber
  paragraph. Additive for users; every implementer in the repository updated,
  `affected master Test/compile` across all platforms.
- Commits: af9e3011b.
