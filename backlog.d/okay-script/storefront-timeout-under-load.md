- [ ] storefront-timeout-under-load — `okay.script.TestStorefront` "the library answers no URL, and the site warms with every page compiling" hit munit's 30 s timeout in a full matrix (2026-09-23, okay-scalus-chain's gate, load average 88 from sibling matrices) and passed 6/6 run alone on the same tree minutes later. A wall-clock budget on a test that COMPILES pages is load-dependent — policy says a default-gate suite must not depend on timing: either the warm-up is made cheaper/deterministic, the budget is stated per page rather than for the site, or the test is `Live`-tagged with the reason. Read the test before choosing; do not just raise the timeout.
      MOVED to integrationTest 2026-09-23 (flaky-to-integration, operator:
      "вынеси нестабильные тесты в интеграционные"): the test is `Live`-tagged,
      so the default gate no longer depends on its timing. What stays OPEN is
      the fix above — an assertion that does not need the clock — after which
      the tag comes off.
