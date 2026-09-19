- [ ] **jdk17-adaptive-runtime** — adapt the six modules
      `jdk17-compat-check` measured as genuinely broken on JDK 17
      (`okay-script`, `okay-http`, `okay-jetty`, `okay-netty`,
      `okay-cluster`, `okay-persist`) with the same treatment
      `Schedulers`/`Timer` already got in core (jdk-adaptive-scheduler):
      gate each unconditional JDK21+ `Thread`/`Executors` call site on
      `Schedulers.hasVirtualThreads`, fall back to a platform-thread
      equivalent. Full design, exact call sites (file:line, all seven —
      jetty has two), a proposed shared `okay.Threads.spawn` helper for
      the five fire-and-forget sites, per-module regression suites, and
      a suggested landing order: specs/jdk17-adaptive-runtime.md — read
      it whole before picking any module.

      WHY: `jdk17-compat-check` (2026-09-19) measured, not guessed, that
      these six break on first use, not at compile or load time — this
      is the fix that measurement was scoped to name but not do.

      HOW: read the spec's "Order and independence" — `okay-script` is
      the smallest, single-site, and is where the shared
      `okay.Threads.spawn` helper should land first (core), since the
      other five modules' fixes depend on it existing. Each of the six
      is independently landable/gateable after that, except
      `okay-netty`'s fix is not provable on JDK17 until `okay-http`'s
      also lands (netty's own tests route through http's Server.scala
      first).

      DONE WHEN (per module): `sbt verifyJdk17` scoped to that module,
      `Test/fork := true` and (where the real suite is `Live`-tagged)
      `--include-tags=Live` both forced explicitly — the measurement
      trap `jdk17-compat-check` documented, where a missing `fork`
      silently gives a false PASS — green on 17, still green on the
      ambient default (21). Update the module's row in
      specs/jdk-compatibility.md's measured table from "no" to "yes"
      with the landing commit.

      NOT IN SCOPE: `okayJVM` core's own two non-crash JDK17 findings
      (`TestPar`, `TestDirectParallel` — the bounded-platform-thread-pool
      trade-off in `Schedulers.own`) are a property of the fallback
      scheduler, not a missing branch — no fix proposed. Nor is actually
      lowering the project's floor below 21 anywhere — this is the
      precondition for that decision, not the decision.
