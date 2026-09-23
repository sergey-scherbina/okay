- [ ] async-cross-sleep-timer-flake — RECURRENCE LEDGER. `TestAsyncCross`
      "sleep then answer completes via runAsync without blocking the
      loop" asserts a 10 ms `Timer.after` fired before a 50 ms
      `Async.sleep` answered. On `okayPlatformNative` under the full
      `affected master test rest` phase (57 JS+Native projects in
      parallel) it failed once with the whole test at 0.081 s — the
      sleep answered, the timer had not fired — and passed alone the
      same minute (27/27). The neighbouring `race` test in the same
      file already records this class ("two sleeps 90ms apart did
      invert once, under a full parallel run") and was rewritten to
      need no clock; this one still needs two clocks to ORDER. Fix
      when it recurs: assert the timer fired by the time the RESULT is
      read after a join, or make the sleep long against the timer by
      a factor the box cannot invert (10 vs 500), or drop the ordering
      claim — "sleep does not block the loop" is provable without a
      second timer beating it.
      1. 2026-09-22, fused-out-of-core gate, Native, alone green.
      MOVED to integrationTest 2026-09-23 (flaky-to-integration, operator:
      "вынеси нестабильные тесты в интеграционные"): the test is `Live`-tagged,
      so the default gate no longer depends on its timing. What stays OPEN is
      the fix above — an assertion that does not need the clock — after which
      the tag comes off.
