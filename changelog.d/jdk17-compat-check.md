## jdk17-compat-check — measured, not inspected, JDK 17 runtime compatibility per module

`sbt verifyJdk17` forks `Test`/`run` onto `~/.sdkman/candidates/java/17.0.19-tem` and
runs the affected suite for real. First real run, recorded in
specs/jdk-compatibility.md: `okayJVM` core (with the adaptive scheduler from
jdk-adaptive-scheduler), `okaySpark`, and `okayDelta` genuinely run on JDK 17 today.
`okayScript`, `okayHttpJVM`, `okayClusterJVM`, `okayPersistJVM`, `okayJetty`, and
`okayNetty` do not — each calls a JDK 21+ `Thread`/`Executors` API unconditionally, at a
named call site, confirmed by a real `NoSuchMethodError`/`IllegalStateException`, not by
inspection.

Also found: `Test / javaHome` is a silent no-op unless `Test / fork := true` is set for
that same project — three modules (`okay-http`, `okay-persist`, `okay-netty`) have no
`Test / fork := true` of their own, so an unforced check gives a false PASS (tests ran
in-process on the launching JDK 21, not the requested 17). Documented as a trap for any
future JDK-version check.

`okayJVM`'s own two non-API-absence failures (`TestPar`, `TestDirectParallel`) are the
adaptive fallback's real trade-off: `Schedulers.own` (bounded platform threads) does not
tolerate a raw OS-blocking call inside a fiber body the way a virtual thread does — code
against `Async`'s own primitives is unaffected.

This task measures; it does not fix the six broken modules — that is separate,
per-module follow-up work, not started here.
