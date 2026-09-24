## hedge-start-hang-under-load - the hang was the test's, and it is gone

`okay.resilience.TestHedgeStart` hung to its 30 s timeout in wire-tls's
full gate (load 28), and was filed rather than tagged away.

- Reproduced: 1 run in 40, then 1 in about 76, under 20-24 CPU burners
  (load 50-61).
- Diagnosed from a thread dump taken inside the hung wait
  (`HotSpotDiagnosticMXBean.dumpThreads`, virtual threads included):
  the first attempt was parked in an `await` that never answers, and the
  thread that should have answered it did not exist. The program chose
  its branch by reading the shared FORK COUNTER when it ran. Under load
  the first attempt started only after the timer had fired and the
  second fork had counted itself, so it read 2 and took the second
  attempt's never-answering branch.
- Proved, not inferred: starting the first attempt 200 ms late made the
  old test hang every time on an idle box, and the fixed one pass.
- Fixed: the attempt's number is set when it is FORKED (a `ThreadLocal`
  set in `Watched.fork` on the fiber's thread). Then 80 of 80 runs passed
  under 24 burners at load 50.
- Refuted, and worth knowing: this was NOT a lost wakeup in
  `CanBlock.block`. Its waiter and completer are ordered correctly, and
  the dump showed a wait for a callback that nothing held.
