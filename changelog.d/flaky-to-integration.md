## flaky-to-integration: four timing-dependent tests leave the default gate

Operator: "вынеси нестабильные тесты в интеграционные". Each test below
reached a verdict through the machine's timing — a wall clock, the
garbage collector, two timers racing — failed once under a loaded box
(several agents' matrices at once) and passed alone on the same tree.
Per the integration-test-gate policy they are now `Live`-tagged: out of
`sbt test`, in `sbt integrationTest`. Only the flaky CASE is tagged, never
its suite:

- okay-script `TestStorefront` "the site warms with every page compiling"
  (a 30 s budget on compiling every page; timed out at load ~88);
- okay-reactive `PublisherTckTest` TCK §3.13 only (a WeakReference must
  clear after `System.gc()` within a timeout; failed at load ~80) — the
  other 38 TCK cases stay in the gate, checked: 38 by default, exactly
  this one under `--include-tags=Live`;
- compare `TestSupervisionShapes` "par supervises its ONE sibling"
  (an elapsed-time verdict, 422 ms at load 48–72);
- okay-platform `TestAsyncCross` "sleep then answer … without blocking
  the loop" (orders a 10 ms timer before a 50 ms sleep; inverted once on
  Native under a full parallel run).

Tagging is not the fix: each backlog entry (storefront-timeout-under-load,
reactive-tck-gc-under-load, supervision-shapes-timing-flake,
async-cross-sleep-timer-flake) keeps the clock-free rewrite open and says
the tag comes off after it.
