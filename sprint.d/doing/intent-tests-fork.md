- [ ] intent-tests-fork — okay-intent's JVM tests run INSIDE sbt's own
      JVM (no `Test / fork`, unlike 29 other modules), and its suites
      refit models (TestOfflineGate 8 splits x 5 gates, MeasureAutonomy,
      TestSlavicRows). In the whole build they share sbt's 6g heap with
      every other in-process module running beside them: 7 of 13 of the
      runner's builds on 2026-09-30 logged GC pressure (up to 55% of
      time in GC, 0.24g free), 5 threw OutOfMemoryError in okay-intent's
      suites, and in-process neighbours timed out as victims
      (TestCoreAsync 4x, TestProgram 2x, TestMergeScopeReachable 2x,
      TestUiDepth, TestClojureCallbacks) — flakes recorded against
      suites that were never at fault. THE FIX: fork okay-intent's JVM
      tests with a heap of their own, sized by measurement; then check
      the remaining in-process modules for the same shape. (2026-09-30)
