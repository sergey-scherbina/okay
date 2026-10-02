## intent-tests-fork — okay-intent's tests fork, off sbt's own heap

okay-intent's JVM suites refit models (TestOfflineGate, MeasureAutonomy,
TestSlavicRows) and ran INSIDE sbt's JVM, sharing its 6g with every
in-process module running beside them. In the runner's whole builds on
2026-09-30, 7 of 13 logged GC pressure (up to 55% of the time in GC,
0.24g free) and 5 threw OutOfMemoryError in these suites, while
in-process neighbours timed out as victims and were recorded as flakes:
TestCoreAsync 4x, TestProgram 2x, TestMergeScopeReachable 2x,
TestUiDepth, TestClojureCallbacks. The module now forks its tests with
-Xmx1g and the repository root as the working directory (the suites read
`okay-agent/src/test/resources/...` relative to it): okayIntentJVM/test
249/249, the three heavy suites run, none skipped.
