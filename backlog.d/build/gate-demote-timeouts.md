- [ ] gate-demote-timeouts — PRIORITY: HIGH (false reds in every busy
      gate). Found 2026-09-27 by one-bind-hot-steps' whole-build gate.
      Since bench-window-demote-measure (bdb1e561a), a gate that meets a
      queued or running benchmark runs on the efficiency cores
      (`taskpolicy -b`) instead of waiting. Heavy tests then miss their
      munit timeouts. The same tree ran okay-platform's TestGenerate
      "stack safety: 1M produced values" in 6.3 s on the performance
      cores and 74.6 s / 254 s (TIMEOUT at 120) demoted. That gate went
      red on nine timeouts: TestGenerate, TestCodeDepth ×2, TestUiDepth,
      PriceInterop 1e6, TestCoreAsync, TestChildren, TestScopeExtrusion,
      TestOwnMonitor. A demoted run's timeout is not a verdict on the
      tree. Options: `gate.sh` reads a munit TimeoutException in a
      DEMOTED run as no-verdict (like KILLED, so `gate-retry.sh` retries);
      demote only commands that are not a test run; or raise the
      timeouts by the demotion factor. `OKAY_BENCH_DEMOTE=off` is the
      workaround a lane can use today.
