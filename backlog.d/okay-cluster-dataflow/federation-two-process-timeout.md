- [ ] **federation-two-process-timeout — TestFederation's two-process
      test times out under load** (found by ts-facade's full gate,
      2026-09-23). "TWO REAL PROCESSES, each its own party: the union's
      answer, the bytes, and B killed" failed with
      `TimeoutException: test timed out after 30 seconds` (45 s wall) in
      a full matrix at load average ~48 on 14 cores. The same suite
      alone, a minute later at load 18, passed 10/10. It spawns real
      JVMs, so its result depends on process start-up time, which
      `sbt test` does not control. By the policy in AGENTS.md ("no flaky
      tests in the default gate"), it belongs under the `Live` tag, or
      needs a start-up wait that does not count against the test's
      30 s. The owner of the federation suite should decide which; this
      entry does not retag it.
