- [~] supervision-shapes-timing-flake — RECURRENCE LEDGER.
      `compare/TestSupervisionShapes` "par supervises its ONE sibling:
      cancelled, not waited for" asserts on elapsed time and failed once
      in a full matrix with "took 422ms — the healthy sibling was waited
      for" (compare/src/test/scala/okay/TestSupervisionShapes.scala:42),
      2026-09-23 10:20, while the box's load average was 48–72 (several
      agents' gates at once). The same tree, the suite alone a few
      minutes later (load still around 36–80): 4/4 green. The suite is
      not Live-tagged, but its verdict depends on scheduling, which
      `sbt test` does not control. That is the integration-test-gate
      policy's case: understand the assertion first (is 422 ms a wait or
      a descheduled thread?), then either widen the observable (assert
      that the sibling was CANCELLED, not how long it took) or tag it.
      Seen by the scala2-ws lane, which does not touch `compare`.
      MOVED to integrationTest 2026-09-23 (flaky-to-integration, operator:
      "вынеси нестабильные тесты в интеграционные"): the test is `Live`-tagged,
      so the default gate no longer depends on its timing. What stays OPEN is
      the fix above — an assertion that does not need the clock — after which
      the tag comes off.
