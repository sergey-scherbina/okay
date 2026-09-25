## testsignals-live — okay-ops TestSignals leaves the default gate

The runner's first whole-build gate over a landing that touched only
build.sbt and okay-platform (mrjar-jdk25-ci-gap, 7 421 results, load
21) went red on `okay.ops.TestSignals`: "the drain did not see the
request finish" — a releaser thread sleeping 60 ms, a drain giving
up after a 2 s grace, on a box that was scheduling 14 cores' worth of
forked JVMs. Being the only landing in the range, that lane was
reverted without a bisect (ci-revert-mrjar-jdk25-ci-gap), as stage C
prescribes. The suite's own doc already records the same failure on
untouched master at load ~20 on 2026-09-10; the latch it got then
fixed a false start, not the timing.

- `TestSignals` is tagged `Live` (whole-suite `munitTests` override,
  the form every other timing- or service-bound suite uses): out of
  `sbt test`, in `sbt integrationTest`. No assertion widened, no retry.
- mrjar-jdk25-ci-gap re-lands after this, unchanged.
