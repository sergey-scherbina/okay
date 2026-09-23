- [~] reactive-tck-gc-under-load — `okay.reactive.PublisherTckTest` "required_spec313_cancelMustMakeThePublisherEventuallyDropAllReferencesToTheSubscriber" failed in a full affected run (2026-09-23, cardano-tables' interaction gate after `Delim.collectUntil` landed, load average ~80) and passed 39/39 alone on the same tree minutes later. TCK §3.13 is judged by a WeakReference being cleared after `System.gc()` within `publisherReferenceGCTimeoutMillis` — a GC-and-wall-clock check, load-sensitive by construction. Policy: a default-gate suite must not depend on timing — read how the TCK environment is configured here (the GC timeout), then either raise it with the measured reason or tag this one TCK test `Live`; do not retry-loop it.
      MOVED to integrationTest 2026-09-23 (flaky-to-integration, operator:
      "вынеси нестабильные тесты в интеграционные"): the test is `Live`-tagged,
      so the default gate no longer depends on its timing. What stays OPEN is
      the fix above — an assertion that does not need the clock — after which
      the tag comes off.
