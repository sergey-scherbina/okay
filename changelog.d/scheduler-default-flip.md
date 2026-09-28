## scheduler-default-flip - the default scheduler is adaptive where the JVM has Loom

- `Schedulers.auto` is ONE shared `adaptive` on JDK 21+ (it was `loom`),
  and `platform` on 17-20 as before; `given Scheduler` follows it, and
  `-Dokay.scheduler=loom` or `given Scheduler = Schedulers.loom` brings
  Loom back. The re-run table (scheduler-default-rerun) already allowed
  it on speed; what held it was correctness, and this lane is that gate.
- The gate was the whole JVM family under the new default, twice on a
  4-core box. It found two defects of the callback drives, fixed and
  landed ahead of this (drive-interrupts-blocking-run,
  drive-resume-throw-lost), one law that read "never started" as "never
  cancelled" (TestSupervisionShapes, now accepting both), and one test
  that hung a fork under load (TestAdaptiveFifo's producers had no
  deadline). The second run left no red that reproduces alone under
  `adaptive` and not under Loom; specs/schedulers.md, "The flip", has
  the table.
- Laws that meant LOOM now say so (TestAsync's virtual-thread law,
  TestReadyMerge's "loom" arms); TestAdaptiveScheduler asserts the
  shared adaptive pick. docs/schedulers.md and docs/guide.md say what
  the default is and why; the page's `given Scheduler = Schedulers.loom`
  example line is gone and its debt entry with it.
- Not re-measured: the slice hooks' cost on the fork/join rows. The
  spec names the lanes to re-run on the operator's machine.
