## okay2-cross-stm - okay2-stm on Scala.js and Scala Native

okay2-stm is a crossProject, with no source change. The async and
simulator suites (TestStmCross, TestStmOrElse and TestStmSim) run on the
JVM, Scala.js and Scala Native. The thread-based battery
(TestStmThreads) is JVM-only. The full okay2 gate: 1714 results
(specs/okay2.md stage 34). okay2-stream waits for adaptive-seal-race.
