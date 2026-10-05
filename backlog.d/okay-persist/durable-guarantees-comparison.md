- [ ] durable-guarantees-comparison — P3 / evaluation: compare durable
      execution systems only at explicitly matched guarantees.
      DEPENDS: durable-recovery-contract-tests; do not optimize or quote
      comparative performance before correctness is demonstrated.
      CONTEXT: https://foojay.io/today/durable-execution-is-a-property-not-a-product/
      and https://github.com/iNicholasBE/temporal-vs-jobrunr-benchmark .
      The reviewed benchmark used JobRunr OSS 8.7.1; its step completion
      metadata persistence differs from the immediate Pro path. Re-check
      pinned implementations and docs when picked:
      https://www.jobrunr.io/en/guides/advanced/durable-executions/ .
      HOW: first write a capability/guarantee table for okay, JobRunr
      OSS/Pro and Temporal: step durability barrier, retries, crash-window
      behavior, replay/drift, versioning, signals/timers, concurrent
      execution and external idempotency assumptions. Mark unavailable
      editions or unmatched guarantees as such; never invent equivalence.
      Then select ONE representative workload from the demo, same step
      granularity, provider behavior, database/storage durability settings,
      concurrency and retained history. Report ACK/fsync/transaction
      boundaries, total writes, throughput/latency and recovery behavior.
      Use the performance skill, quiet serialized runs and alternating
      repeated comparisons; record versions, configs and raw results.
      DONE: reproducible narrow harness and honest report; no ranking
      based on fewer writes achieved by delaying durability, and no
      extrapolation from a warm microbenchmark to production throughput.
      This is optional evaluation, not a prerequisite for the first demo.
