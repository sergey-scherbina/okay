- [ ] schedulers-own-hangs-under-blocking-nio — `okay-http`'s `Nio.scala`
      (raw blocking NIO, `Async.spawn`'d) genuinely DEADLOCKS on JDK 17
      (`Schedulers.auto` picks `own`, a bounded platform-thread pool,
      since there's no Loom there) — confirmed by isolating
      `okayHttpJVM/testOnly okay.http.TestNio` alone, twice,
      `timeout 30`/`40`, exit 124 both times, not even the suite header
      printed. `okay-jetty`'s `TestResumable` (SSE resumable stream)
      shows the same symptom on the same evidence pattern (a live
      server, four live `HttpClient`s, Jetty's own pool all present and
      not deadlocked against EACH OTHER in a `kill -QUIT` dump, and a
      control run forcing the same `QueuedThreadPool` on the ambient
      JDK 21 passed in 3s — ruling out the pool type itself), but not
      traced call-by-call the way `TestNio`'s was.

      WHY: jdk17-adaptive-runtime (2026-09-20) set out to fix six
      modules' unconditional JDK21+ API calls and did — four are fully
      green on real JDK 17 now (script, cluster, persist, netty). Fixing
      `okay-http`/`okay-jetty`'s crash-level bugs uncovered this SECOND,
      deeper problem underneath: `Schedulers.own`'s bounded pool was
      already known (jdk17-compat-check) to cause wrong-timing failures
      under raw blocking calls (`TestPar`/`TestDirectParallel` in
      `okayJVM` itself) — this is the same mechanism causing a genuine
      HANG instead, once enough fibers are concurrently parked in real
      blocking accept/read calls (one fiber held per open connection,
      for its whole life). `okay.Threads.spawn` (a dedicated platform
      `Thread` per call, not a shared bounded pool) does NOT have this
      problem — it's why `okay-cluster`/`okay-persist` converted to it
      cleanly while `Nio.scala` (which goes through `Async.spawn`
      instead) did not.

      HOW (not decided, three real options, from
      specs/jdk17-adaptive-runtime.md's "What this surfaced"):
        (a) make `Schedulers.own` detect/tolerate blocking calls — grow
            the pool, or a dedicated blocking-pool tier
        (b) convert `Nio.scala` to `okay.Threads.spawn`-per-connection
            the way cluster/persist already are — loses the
            fiber-cooperative structure `Async.spawn` gives the rest of
            that file
        (c) accept `Nio`-based transports and Jetty's SSE resumable
            stream as JDK17-unsafe and say so plainly wherever JDK 17 is
            offered as an option, rather than trying to make everything
            uniformly portable

      Read specs/jdk17-adaptive-runtime.md's "What this surfaced that
      the spec didn't plan for" section in full before picking this up
      — it has the exact repro commands and the reasoning for ruling out
      the alternatives already tried (QueuedThreadPool itself, the
      classfile-version bug, `TestHttp`/`TestMcpHttp`/`TestMcpPushServer`
      all confirmed NOT the cause).

      NOT a regression on the shipping path: the project's default JDK
      (26, via `Test/javaHome`) has virtual threads, so `Schedulers.auto`
      picks `loom` there and none of this triggers — full `sbt
      integrationTest` (ambient JDK) is green, `okayJetty` included,
      confirmed 2026-09-20. This only matters if/when JDK 17 becomes a
      real runtime target, not today.
