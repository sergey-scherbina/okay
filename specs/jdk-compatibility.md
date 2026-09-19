# JDK compatibility — the floor, the ceiling, and why

Bookkeeping, not a feature: this project has no single stated JDK
floor or ceiling, and pretending otherwise almost drove a wrong
decision (2026-09-19, jdk-compatibility-matrix) — the two constraints
below pull in opposite directions, and nothing before this doc wrote
either one down. `.sdkmanrc` pins the one JDK version known to satisfy
both; this doc is why.

## The floor: JDK 21

`src/main/scala-jvm/Platform.scala`'s **default `Scheduler` is Loom
itself** — `val loom: Scheduler = ...`, `Thread.startVirtualThread`
one call away from every fiber this library forks — so the floor is
not a feature some modules opt into, it is the JVM runtime's own
default execution model. Virtual threads (`Thread.ofVirtual`,
`Thread.startVirtualThread`, `Executors.newVirtualThreadPerTaskExecutor`)
became a normal, non-preview, GA API in **JDK 21** (JEP 444) — nothing
here uses `--enable-preview`, and nothing needs to.

Every one of these calls it directly, all JVM-only source:

| module | file | what it does with the thread |
|---|---|---|
| core | `Platform.scala` | the default `Async` scheduler (`loom`), and the delay timer's fire callback |
| okay-http | `Server.scala` (the JDK backend) | `Executors.newVirtualThreadPerTaskExecutor()` — one thread per request |
| okay-jetty | `Jetty.scala` | streams a response body off the event thread |
| okay-netty | `Netty.scala` | same, for a server-sent-events push that must not block the event loop |
| okay-cluster | `Served.scala` | one thread per accepted connection |
| okay-script | `Sessions.scala` | the live-reload tail loop |
| okay-persist | `Wire.scala`, `RaftWire.scala` | one thread per connection, the accept loop, Raft's tick loop |

A build floor below 21 (17, 11, 8) breaks all of these at compile
time (the APIs do not exist) or, for the ones reflection could paper
over, at the JVM's own default scheduler — there is no fallback path
that runs this library without Loom.

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

**What is NOT yet verified: an actual JDK 25 run of okay-spark's
tests on THIS machine.** Forking `okaySpark/Test` through
`~/.sdkman/candidates/java/25.0.4.1-tem` (via `Test / javaHome`)
discovered zero tests — no exception, no Spark startup log, sbt just
reported `Total 0` — which reads as an sbt-test-fork/JDK-25
compatibility wrinkle in THIS setup rather than a Spark defect (Spark
4.2.0's own CI already ran 66,716 tests green on Java 25, per the
same JIRA). Not chased further this session; the `munitIgnore` guard
at `>= 24` is consequently still accurate to LEAVE IN — it now
under-states what Spark itself supports, but removing it would need
the local JDK25 run actually working first, not just the upstream
claim.

## What this means for "can we bump the JDK"

- **Lowering the floor** (17, 11, 8) is not on the table while
  `Platform.scala`'s default scheduler is Loom: every JVM consumer of
  this library, not just the ones in the table above, inherits the
  requirement transitively.
- **Raising the floor past 23 no longer needs okay-spark's own JDK 24
  ceiling to move with it** — Spark 4.2.0 (landed spark-4-2-0-jdk25,
  2026-09-19) claims 17/21/25 upstream, verified here on the 21 end.
  The ceiling this section opens with is Spark 4.0.0's; 4.2.0 is what
  is actually in `build.sbt` now. The local JDK 25 run is still
  unverified (see above), so treat "past 23 is safe for Spark" as
  upstream-claimed-and-partially-confirmed, not fully closed.
- **JDK 25** is not a floor-or-ceiling question at all: it is an
  *additive* per-feature answer. `okay.Scoped` (core,
  `specs/script-scoped-state-mrjar.md`) ships as a Multi-Release JAR —
  a JDK 21 `ThreadLocal` backend at the jar root, a JDK 25+
  `java.lang.ScopedValue` one under `META-INF/versions/25/`, picked by
  the running JVM per JEP 238 with no runtime branch in this
  library's own code. Nothing else here does this yet, and nothing
  requires it to.
- **The working range today is 21–23.** `.sdkmanrc` pins
  `21.0.7-tem` — the floor exactly, and the version every contributor
  and CI runner should default to — because it is inside okay-spark's
  supported range (17, 21) and needs no `-Djava.security.manager`
  flag at all.

## Related

`backlog.d/okay-script/mrjar-jdk25-ci-gap.md` — nothing automatically
re-verifies the JDK 25 (`ScopedValue`) side of the one Multi-Release
JAR this project ships; that is the per-feature gap this doc's
JDK 25 paragraph names, not a floor-or-ceiling question this doc
answers by itself.
