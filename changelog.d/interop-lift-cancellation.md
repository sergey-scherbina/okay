## interop-lift-cancellation - cancelling a fiber now stops a lifted Frege or Clojure step

A backlog question, answered by measurement. A cancel stopped a lifted
Frege `liftIO` under Loom, but NOT under a pool-threaded scheduler
(`Schedulers.drive`). There the fiber was reported finished while the
lifted IO ran on in the background. The first cut of the test was fooled
by exactly that. The instrument now watches the WORK (a mark the action
leaves after its sleep), with a control run proving that it sees the
sleep.

The fix: `Foreign.View.liftAsOperation`. Where the row has `Async`, the
lifted action runs as `okay.Interruptible.await`, on its own thread,
interrupted by the cancel, under any scheduler. Clojure gains
`(ok/lift f)` for blocking code. Tests: TestFregeCancel and
TestClojureCancel, 3 each. Docs: docs/jvm-languages.md,
docs/modules/okay-frege.md, docs/modules/okay-clojure.md; spec:
specs/interop-shared.md.
