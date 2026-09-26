## cancel-releases-resource - a Resource scope cancelled by timeout or race releases, as ZIO's interruption does

- A `bracket`/`Resource.run` scope whose `use` was parked on an `Await`
  leaked its resource when `Async.timeout` cancelled it or when it lost
  `Async.race`. Two tests were watched red. The Async `Failing` guard now
  wraps the Await's canceller, and cancelling an OPEN wait releases the
  scope once. After the wait has answered, a stale cancel does not
  release, because the scope is still running.
- A `use` blocked in a `Run` was already released by the fiber's
  interrupt. It is pinned now.
- Still open, and written down in specs/core-gaps.md stage 7: the
  callback drive stops between two non-blocking operations without
  discontinuing the residual.
- Found by this lane's gate, not caused by it: okay-foreign-cluster's
  `FakeReducer` checked and cleared a `@volatile` flag in two steps, so
  two workers could both die (`died == 2`). It is atomic now
  (`AtomicBoolean.compareAndSet`). lake-hudi fixed the same race in
  `TestForeignStage`'s `Fake` the same day, and its fix is the one kept.
