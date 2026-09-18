## gate-jvm-first - the gate runs JVM first, and a JVM red stops there

`affected` takes a platform now (`jvm`, `js`, `native`, `rest`, `all`),
sharing one definition of "what counts as JVM" with `family` rather
than keeping a second copy. `gate.sh` expands the bare
`affected <ref>` form - the one AGENTS.md tells every lane to run -
into TWO commands inside ONE sbt: `affected <ref> test jvm` then
`affected <ref> test rest`. sbt runs them in sequence and stops at the
first failure, so a red JVM never pays for the other two platforms.
One sbt, one JVM start, one log; any other argument passes through
untouched.

MEASURED ON ONE FULL GATE, and the numbers correct the claim that
opened this lane:

| phase | projects | time |
|---|---|---|
| JVM | 59 | 123 s |
| JS + Native | 43 | 74 s |

So a lane that fails on the JVM now learns it in 123 s instead of
197 s - 37% off the failing case, which is the common one.

WHAT I HAD WRONG, and it is worth more than the saving. The
measurement that opened this said "node is 684 of 944 samples, 72% of
a matrix" and I let that stand for WEIGHT. It is 72% of the PROCESS
COUNT. By time the split is the other way round: the JVM phase is the
longer half. Scala.js runners are numerous and cheap, not few and
expensive, and counting processes answered a different question from
the one being asked. The bottleneck by time is the JVM arm, and
whoever wants the next win should look there rather than at `node`.

Also fixes a defect this lane put into `gate-selftest.sh` an hour
earlier: the busy case asserted the fake burned 3 CPU-seconds in a 6
second window, which is an assertion about the SCHEDULER and failed
the first time the suite ran beside another gate - the build was
real but starved. The threshold is 0 now: the distinction the
watchdog actually draws is stalled=0 against working>0, and that is
what the test checks.
