## okay2-cross - okay2 on Scala.js and Scala Native, stage A

The core, okay2-data, okay2-optics and okay2-workflow are now
crossProjects in okay2's own build. They target Scala.js 1.22.0 and
Scala Native 0.5.12, and every suite runs on all three platforms. The
JVM projects keep their ids, so `okay2/testOnly …` still works. The full
okay2 gate from clean: 1639 test results.

Found on the way (specs/okay2.md stage 31):
- JS test linking is compliant, so a failed cast throws;
- the live runtime's UUID no longer uses `SecureRandom`, which JS and
  Native lack;
- each macro module has its own `Provided` scala-reflect;
- `.jvmopts`, and forked JVM suites;
- the root build's `parallelExecution := false`, after the first cross
  run stalled on idle `node` and Native runners.

Thread tests are JVM-only (`src/test/scala-jvm`). Stage B, the platform
modules, stays in the backlog.

Docs: docs/okay2.md section 1.
