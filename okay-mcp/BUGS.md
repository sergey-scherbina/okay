# Bugs — `okay-mcp`

The ledger for defects whose fix lands in `okay-mcp`'s own sources.

Newest first. Status lives in the machine-readable header, never in the prose.

## test-fork-leaks-processes — okay-mcp's default (non-Live) test fork accumulates idle child processes
<!-- status: open
     lane: okay-mcp
     area: test
     found-by: jdk17-compat-check, running the WHOLE `test` aggregate
       (not affected-scoped, not this module specifically) under a
       JDK17 Test/javaHome override
     gate: none
-->

Running `sbt verifyJdk17` (a full, unscoped `test` across every
project — not how this project is normally exercised; `gate.sh`
always scopes to `affected master`) reached `okayMcp`'s test fork and
it sat idle for 27+ minutes on a suite that normally takes seconds.
`ps` showed **63 child `node` processes**, all at 0.0% CPU, ages 27–29
minutes, still accumulating when the run was killed (by PID, own
process, confirmed via the fork's own classpath before killing).

**Not yet localized to a specific test.** `TestLive.scala` was the
first suspect (it does `ProcessBuilder(...).start()` and spawns a real
`npx`-run MCP server) but its tests are tagged `Live`
(`ThisBuild / Test / testOptions` excludes that tag by default,
confirmed in build.sbt) — a bare `test` run should never reach
`assume(available, ...)`, so the accumulation is not obviously that
suite. `okayMcp/src/main/scala-jvm/okay/mcp/Stdio.scala` is the other
place `ProcessBuilder` appears in this module and was not read far
enough to rule in or out before this session's time ran out.

**Not established whether this is JDK17-specific or a pre-existing
leak this project's normal (scoped, gate.sh-driven) usage never
happens to trigger.** No control run was done — killing the stuck
fork and re-running everything on JDK21/26 for comparison is the
first thing whoever picks this up should do, not assumed either way.
