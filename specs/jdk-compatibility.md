# JDK compatibility — the floor, the ceiling, and why

Bookkeeping, not a feature: this project has no single stated JDK
floor or ceiling, and pretending otherwise almost drove a wrong
decision (2026-09-19, jdk-compatibility-matrix) — the two constraints
below pull in opposite directions, and nothing before this doc wrote
either one down. `.sdkmanrc` pins the one JDK version known to satisfy
both; this doc is why.

## The floor: JDK 21 — narrower than it looked, and getting narrower

`src/main/scala-jvm/Platform.scala`'s **default `Scheduler` was Loom
unconditionally** — `Thread.startVirtualThread` one call away from
every fiber this library forked, no fallback. Virtual threads
(`Thread.ofVirtual`, `Thread.startVirtualThread`,
`Executors.newVirtualThreadPerTaskExecutor`) became a normal,
non-preview, GA API in **JDK 21** (JEP 444) — nothing here uses
`--enable-preview`, and nothing needs to.

**Correction (jdk-adaptive-scheduler, 2026-09-19): "breaks at compile
time" below was wrong, and it mattered — it nearly drove an
MRJar-based rewrite of `Schedulers` for a problem that doesn't exist.**
Checked directly rather than assumed: a class compiled by dotc
(hosted on JDK 21, our only real toolchain) referencing
`Thread.startVirtualThread` in one method loads and runs FINE on JDK
17, calling any OTHER method in that same class — JVM constant-pool
resolution is lazy, per call site, not per class. The class file
itself was always JDK17-loadable (dotc's default target is class file
major version 61 regardless of host JDK). The real failure mode is
narrower and later: the first TIME one of these call sites actually
RUNS on a JDK that lacks the API, not when the class loads and not
when it's built.

`core`'s row is now fixed (`Schedulers.hasVirtualThreads` /
`Schedulers.auto`, see specs/jdk-adaptive-scheduler.md) — `okayJVM`
and anything depending only on it is now genuinely correct on JDK 17,
proven with a standalone probe there, not assumed. The other rows are
UNCHANGED and still call these APIs unconditionally, so for them the
practical effect is the same as "breaks immediately" even though the
mechanism is "throws on first use, not on load" — a server's first
request or connection is not far from immediate:

| module | file | what it does with the thread | JDK <21 |
|---|---|---|---|
| core | `Platform.scala` | the default `Async` scheduler, the delay timer's fire callback | **adaptive** (jdk-adaptive-scheduler) |
| okay-http | `Server.scala` (the JDK backend) | `Executors.newVirtualThreadPerTaskExecutor()` — one thread per request | throws on the first request |
| okay-jetty | `Jetty.scala` | `VirtualThreadPool` (jetty-virtual-threads) on the main connector, plus streams a response body off the event thread | throws on the first request |
| okay-netty | `Netty.scala` | server-sent-events push that must not block the event loop | throws on the first such push |
| okay-cluster | `Served.scala` | one thread per accepted connection | throws on the first connection |
| okay-script | `Sessions.scala` | the live-reload tail loop | throws when live-reload is used |
| okay-persist | `Wire.scala`, `RaftWire.scala` | one thread per connection, the accept loop, Raft's tick loop | throws on the first connection |

Each of the un-adapted rows *could* get the same treatment
`Schedulers` got — none of them are compile-time blockers either, now
that the actual mechanism is understood — but none has, and this doc
does not claim otherwise.

## The ceiling: okay-spark wants JDK ≤ 23

Spark 4.0.0 supports Java 17 and 21. Hadoop's
`UserGroupInformation.getCurrentUser`, which Spark calls on every job,
reaches `Subject.getSubject(AccessControlContext)` — part of the
**Security Manager**, deprecated for removal since JDK 17 (JEP 411)
and **removed outright in JDK 24** (JEP 486):

```
UnsupportedOperationException: getSubject is not supported
```

`-Djava.security.manager=allow` is the documented workaround for
18–23 (it re-enables the deprecated, not-yet-removed API); it does
nothing on 24+, because the API is gone, not merely disabled — and a
JDK 26 refuses to even **start** with that flag
(`Enabling a Security Manager is not supported`). There is no flag
that reaches past JEP 486. `TestSparkInterop` (`okay-spark/src/test`)
already carries this as an `override def munitIgnore: Boolean =
Runtime.version().feature() >= 24`, so a run on 24+ skips rather than
fails opaquely — the ceiling is enforced there, not merely documented.

**Update (spark-4-2-0-jdk25, 2026-09-19): the ceiling moves with the
Spark version, and 4.2.0 raises it past 24.** Checked upstream, not
assumed — Apache JIRA SPARK-51167 ("Build and Run Spark on Java 25")
is Resolved/Fixed in 4.2.0 (created 2025-02-11, resolved 2026-05-11);
Spark's own 4.2.0 release notes say it runs on Java 17/21/25. Bumped
`build.sbt`'s `spark-sql` dependency to 4.2.0 and re-paired the
`legacyStdlib` fix above (its `scala-library` pin has to track
`scala-reflect`'s — both went to 2.13.18): `TestSparkInterop` passes
all 4 tests on this box's JDK 21 floor, confirming 4.2.0 is a safe
upgrade for the range we actually run.

**Update (spark-jdk25-guard-fix, 2026-09-19): the "not yet verified"
above was a false alarm, and closed.** Forking `okaySpark/Test`
through `~/.sdkman/candidates/java/25.0.4.1-tem` did report `Total 0`
with no exception — but that was never an sbt/JDK-25 fork wrinkle.
`TestSparkInterop.munitIgnore` read `javaFeature >= 24`, a guard
written for Spark 4.0.0's ceiling and never updated when 4.2.0 landed
— it was silently skipping every test on JDK 25 right along with the
genuinely broken 24, and a skip is exactly what an sbt fork reports
as `Total 0, no exception`. Narrowed the guard to `== 24` (24 has no
workaround, JEP 486; 22/23 still have a deprecated Security Manager
and are simply untested here, not assumed broken either way), then
verified 25 for real: ran `TestSparkInterop`'s own
SparkSession-creation-and-aggregation path as a standalone `java -cp
<test classpath>` process (bypassing sbt's fork entirely) under
25.0.4.1 — `local=8333333.25 onSpark=8333333.25 diff=0.0`, a genuine
distributed job (8 partitions, DAGScheduler, real task log), not a
process-exit-code check. Spark 4.2.0 on JDK 25 is now directly
confirmed here, not just upstream-claimed.

## What this means for "can we bump the JDK"

- **Lowering the floor for `okayJVM` core alone is done** — its
  scheduler and timer now adapt (jdk-adaptive-scheduler). Lowering it
  for the WHOLE project is not: every other row in the table above
  still calls a JDK21-only API unconditionally, and moving the floor
  would mean the same fix, call site by call site, for each — not
  attempted here, and each one is its own decision (a server's request
  path is a hotter, more load-bearing place to add a branch than a
  core scheduler default).
- **Raising the floor past 23 no longer needs okay-spark's own JDK 24
  ceiling to move with it** — Spark 4.2.0 (landed spark-4-2-0-jdk25 +
  spark-jdk25-guard-fix, 2026-09-19) runs on 17/21/25, confirmed here
  directly on both the 21 and 25 ends (see above). The ceiling this
  section opens with is Spark 4.0.0's; 4.2.0 is what is actually in
  `build.sbt` now. 22 and 23 remain genuinely untested — not claimed
  either way — and 24 stays a real ceiling with no known fix.
- **JDK 25** is not a floor-or-ceiling question at all: it is an
  *additive* per-feature answer. `okay.Scoped` (core,
  `specs/script-scoped-state-mrjar.md`) ships as a Multi-Release JAR —
  a JDK 21 `ThreadLocal` backend at the jar root, a JDK 25+
  `java.lang.ScopedValue` one under `META-INF/versions/25/`, picked by
  the running JVM per JEP 238 with no runtime branch in this
  library's own code. Nothing else here does this yet, and nothing
  requires it to.
- **`.sdkmanrc` is the AMBIENT/compile pin, not the whole story any
  more (jdk26-default-runtime, below).** It was `21.0.7-tem`; since
  java-gatherers (2026-09-23) it is `25.0.4.1-tem` — see "Compiling
  on 25" below for what that moved and what it did not.

## Runtime defaults to JDK 26, floor stays 21 (jdk26-default-runtime, 2026-09-19)

Compiling and running are different JVMs now, on purpose.
`ThisBuild / Test / javaHome` and `ThisBuild / run / javaHome` default
to the newest GA JDK this project has verified end to end — 26,
installed at `~/.sdkman/candidates/java/26.0.2.1-tem` (27 is not GA at
this date; Adoptium's `available_releases` tops out at 26, 27/28 are
tip/EA only — not repeating the preview-API mistake
script-scoped-state already made once, for a whole JDK this time).
This only takes effect where `Test / fork` / `run / fork` is already
`true`, the existing convention across most test-bearing modules —
checked directly (`show okayJVM/Test/javaHome`), not assumed.
Compiling (sbt's own JVM, in-process dotc) was untouched, still 21 —
until java-gatherers moved it to 25 (below).

**First full-matrix run on 26 found exactly one new problem**, not the
handful feared: `okay-delta`'s `TestDelta`, 4 failures, all the same
`KernelEngineException: ... getSubject is not supported` — Delta
Kernel resolves paths through Hadoop's `Configuration`/
`UserGroupInformation`, same as Spark, so it hits the identical JEP
486 wall (Security Manager gone, JDK 24+). A different library, the
same root cause `.sdkmanrc` already named for Spark. Unlike Spark
4.2.0, no upstream JDK25+ fix was found for delta-kernel 4.4.0, so
`okayDelta` shadows the build-wide default back to `None` (the
ambient/compile JDK, 21) rather than assuming a newer version also
works. `okaySpark` already had its own shadow, to 25
(spark-jdk25-guard-fix) — both now co-exist as the two modules that
opt OUT of the build-wide 26 default; everything else opts in
untested-before-today and passed (5664 tests, 181 modules, cold, no
warnings).

**Everywhere else checked directly, not just "the gate didn't fail
somewhere else":** `okayJVM` (746/746, unchanged count from before
this session's other JDK work), `okayScript` (208/208 — its own
in-process dotc now itself running hosted on JDK 26), `okayJetty`'s
real integration suite (19/19, `Live`-tagged, actual sockets,
WebSocket sessions, the `VirtualThreadPool` from jetty-virtual-threads
actually serving under 26), `okaySpark` (4/4, confirmed still pinned
and unaffected).

## Building for JDK 17 is not the question — running on it is (jdk17-compat-check, 2026-09-19)

**"Can we optionally build the library for JVM 17?" has a one-line answer: it already
does, for every module, with no separate build step.** dotc's classfile target is major
version 61 (JDK 17) by default regardless of the host JDK compiling it — the whole
`.jvmSettings` tree here is compiled once, on the ambient JDK (21 then, 25 since java-gatherers — the target is still 61, `-java-output-version 17`), and every `.jar` it
produces already loads on a JDK 17 JVM. There is nothing to opt into at build time. The
real question this session measured is the one the by-inspection table above could only
guess at: **which modules, once loaded, actually run correctly on JDK 17**, given that
six of them call a JDK 21+ `Thread`/`Executors` API unconditionally. `sbt verifyJdk17`
(build.sbt, forks `Test`/`run` onto `~/.sdkman/candidates/java/17.0.19-tem`) exists now
so this is checkable again, not just claimed once.

**A trap the first pass fell into, worth naming because it produces a false PASS, not a
crash: `Test / javaHome` is silently a no-op unless `Test / fork := true` is ALSO set for
that same project.** `okay-http`, `okay-persist`, and `okay-netty` have no
`Test / fork := true` of their own — running `set every Test / javaHome := Some(jdk17)`
and then `okayHttpJVM/test` looked like a clean 109/109 pass, but the sbt banner still
read "Eclipse Adoptium Java 21.0.12": the tests ran in-process, on the JVM that launched
sbt, not on the requested one. Only forcing `set every Test / fork := true` in the same
command revealed the real (broken) behavior underneath. Any future JDK-version check
here must force both settings `every`, together, and confirm from the sbt banner or a
genuine JDK-version-gated failure that the fork actually happened — a clean pass alone is
not evidence.

Measured (fork + javaHome + `--include-tags=Live` where a module's real integration
suite is Live-tagged, since that is where the unconditional call sites actually run):

| module | JDK 17, for real | evidence |
|---|---|---|
| `okayJVM` (core) | **yes** (jdk17-core-loom-tests, 2026-09-20) | 754/762, 0 failed, 8 skipped — every skip is a test that IS about Loom (`SchedulerFamily`'s `loom` member, `TestAsync`'s "spawn runs on a virtual thread"), an `assume` on `hasVirtualThreads`; the tests that merely USED a JDK 21 `Thread` API to carry fibers (`TestDirectParallel`'s counting scheduler, `TestTDict`'s racers, the deque law's thieves) carry them on `Schedulers.threads` / `okay.Threads.spawnThread` / `new Thread` now, and `TestAdaptiveScheduler` asserts the branch the JVM it runs on takes. Was 736/746 with all 10 failures naming `Schedulers.loom` or a JDK21+ `Thread` API by identifier |
| `okaySpark` | **yes** | 4/4, unaffected — ceiling is JDK 24+, 17 is far under it |
| `okayDelta` | **yes** | 4/4, same ceiling story |
| `okayScript` | **yes** (jdk17-adaptive-runtime) | 208/208 — `Sessions.scala:89` now `okay.Threads.spawn` |
| `okayClusterJVM` | **yes** (jdk17-adaptive-runtime) | 136/136 — `Served.scala:50` now `okay.Threads.spawn` |
| `okayPersistJVM` | **yes** (jdk17-adaptive-runtime) | 242/242 default + 13/13 `Live` — five call sites across `RaftWire.scala`/`Wire.scala` now `okay.Threads.spawn` |
| `okayNetty` | **yes** (jdk17-adaptive-runtime) | 16/16 `Live` — was only ever broken by `okayJetty`'s stale classfile, transitively (below) |
| `okayHttpJVM` | **yes** (own-lost-wakeup, 2026-09-20) | 38/38 with `Live`. `Server.scala:34`'s crash was fixed by jdk17-adaptive-runtime (`Executors.newCachedThreadPool()` fallback); `TestNio` then HUNG on real 17, first read as `Schedulers.own`'s pool starving under blocking accept/read fibers — the thread dump said one worker blocked and thirteen PARKED, a lost wakeup, and `auto` picks a watched `own` (`Schedulers.platform`) there now. BUGS.md `own-lost-wakeup` |
| `okayJetty` | **yes** (own-lost-wakeup, 2026-09-20) | 19/19 with `Live`. `VirtualThreadPool.<init>`'s crash fixed (conditional `QueuedThreadPool` fallback) and `Listen.java`'s classfile 65 fixed (`--release 17`) by jdk17-adaptive-runtime; `TestResumable`'s hang was the same lost wakeup as `okayHttpJVM`'s and went with it |

**`okayJVM`'s own caveat, found by measurement, not assumed away:** two of the ten
"expected" failures are not API-absence at all. `TestPar`'s fail-fast timing assertion
and `TestDirectParallel`'s rendezvous test both use the ambient default scheduler (no
explicit `Schedulers.loom`), and both fail differently — not `NoSuchMethodError`, but
wrong answers / blown timing budgets — because `Schedulers.auto` correctly falls back to
`own.build` (a bounded platform-thread pool) on JDK 17, and `own.build`'s workers do not
tolerate a fiber body making a *real* OS-level blocking call (`Thread.sleep`,
`CountDownLatch.await`) the way a virtual thread can. A fiber that blocks its own
platform-thread worker can starve a sibling fiber waiting on the same bounded pool — the
exact hazard the source article this session started from was about. This is a genuine,
expected trade-off of the adaptive fallback, not a bug in it: code written against
`Async`'s own primitives (not raw `java.util.concurrent` blocking calls) does not hit it.
(own-lost-wakeup, 2026-09-20: the trade-off was worse than timing — a fiber blocked for
good hid every later fork, a hang — and `auto` picks `Schedulers.platform`, a watched
`own`, on such a JVM now; BUGS.md has the dump. And with it the caveat is gone: `TestPar`
and `TestDirectParallel` pass on real 17 — the stuck-check is what "tolerates" a raw
blocking call, at a tick of latency.)

**What this measurement does NOT do: fix the six broken modules.** Each would need the
same treatment `Schedulers`/`Timer` already got (`hasVirtualThreads`-gated branch, a
portable fallback) at its own unconditional call site — real, separate work, one module
at a time, not attempted here.

**Update (jdk17-adaptive-runtime, 2026-09-20): four of the six now genuinely done.**
`okay-script`, `okay-cluster`, `okay-persist`, `okay-netty` are green on real JDK 17,
verified the same way this table was built. `okay-http` and `okay-jetty` are NOT —
fixing their unconditional API calls uncovered the SAME `Schedulers.own`/blocking-call
trade-off named above, except there it causes a genuine HANG (not a wrong answer) in
`okay-http`'s `Nio.scala` and, on the same evidence pattern, `okay-jetty`'s
`TestResumable`. Full writeup: specs/jdk17-adaptive-runtime.md.

## Compiling on 25, the floor as a flag (java-gatherers, 2026-09-23)

okay-java bridges `java.util.stream.Gatherer`, final in JDK 24, and
dotc sees only the class library of the JVM it runs in: on 21 the
import is E008, `-release 25` is "not a valid choice", and Scala 3 has
no `-system`. So sbt runs on 25 (`.sdkmanrc`). What that must NOT
move, and did not (measured with `javap` on the built classes):

- **bytecode**: dotc's default target is 61 on 25 exactly as on 21, and
  `-java-output-version 17` — now on every Scala 3 module — pins it.
- **the API floor**: compiling on 21 refused any 22+ API by accident;
  on 25 nothing would, and tests run on 26. `-java-output-version N`
  restores the refusal, but it sets the API check and the bytecode
  TOGETHER (the `-Xunchecked-` variant is overridden by it), so N is
  per module, via `jdkFloor(n)` in build.sbt:
  - **17**, the default: the API AND the bytecode every module had.
  - **21**: `compare` only — the benchmark harness, JDK 21 API
    unconditionally, never run below 21.
  - **0**, no flag: modules that reach PAST their floor on purpose,
    behind a guard of their own, and need bytecode 61 so the class
    loads on 17 at all — `okay-platform` and `okay-http` (Loom behind
    `Schedulers.hasVirtualThreads`), `okay-java` (Gatherer, linked
    lazily: a JDK 17/21 user of okay-java loads it fine and fails only
    by CALLING `Gather`).
  The compiler made that list, not a survey: a full `Test/compile`
  with 17 everywhere named exactly those three main-code modules.
- **tests carry no floor** (project/JdkFloor.scala): forty-odd test
  files call JDK 21 API unconditionally, and floor 21 would make every
  test class of their modules major 65, so `verifyJdk17` could not
  load the suites that pass on 17 today. Test bytecode stays 61.

## Related

`backlog.d/okay-script/mrjar-jdk25-ci-gap.md` — nothing automatically
re-verifies the JDK 25 (`ScopedValue`) side of the one Multi-Release
JAR this project ships; that is the per-feature gap this doc's
JDK 25 paragraph names, not a floor-or-ceiling question this doc
answers by itself.
