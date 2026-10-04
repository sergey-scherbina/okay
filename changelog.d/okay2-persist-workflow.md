## okay2-persist-workflow - the durable workflow over the log in okay2-persist

Lane 4, the last of okay2-persist-rest (operator: "do everything needed
for okay2, all at once"; specs/okay2.md stage 56, docs/okay2.md section
37), on JVM, Scala.js and Scala Native. okay2-persist now depends on
okay2-workflow.

- `Dialogue`: a paused program whose journal is a topic. Each record
  carries the program that wrote it and the position its writer
  expected. An answer is journalled after the program accepts it.
  There is a warm path (`step`) and a cold one (`answer`), chapters,
  `continueAs`, and a `diagnosis` that names the reader's own line.
  `Dialogue.workflow` uses `Wf.replay`, and `Dialogue.answerSchema` is
  the schema of a workflow's `Either` answers.
- `Worker` drives many runs of one program: sleeps, signals, children,
  cancellation, continuations, leases, the status index, isolation in
  `tick`, and `Incompatible` for a program that cannot read its own
  history. The oracle runs in its own wider row and receives the
  `Attempt`.
- The side tables `Timers`, `Signals`, `Cancels`, `Children`, `Leases`,
  `Statuses`, `Queues` and `Resume`, plus `Retire` and `Saga`.
- 135 tests over 25 suites, ported from okay-persist's: 58 shared, and 77
  on the JVM for the worker and saga, which run an `Async` activity row
  to a value. `TestDocExamplesDurableProgram` pins a Scala 3 page and is
  not ported.
- Still not ported: `Repair` (needs `Condition`) and `TestWireTls` (needs
  TLS on okay2); okay2/backlog.d/modules/okay2-persist-rest.md.
