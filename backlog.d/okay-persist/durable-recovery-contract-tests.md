- [ ] durable-recovery-contract-tests — P1 / evidence: prove crash and
      concurrency guarantees through the actual durable storage path.
      READY: first-attempt WithKey and run-scoped-key fixes are implemented
      in okay-durable; the workflow branch can also be audited independently. Context: docs/durable-workflows.md,
      okay-durable Durable / okay-durable-persist TopicJournal, okay-persist Dialogue/Worker,
      Leases and JVM FileStore. Source review is not a crash-test result.
      FIRST inventory existing TestDurable, TestFileStore, TestWorker,
      TestContinueAsWorker, TestDialogueHardening and TestProcCut cases;
      add only missing coverage, preserving their existing guarantees.
      MATRIX: crash before intent ACK; after intent ACK before request;
      after provider success before answer ACK; after answer ACK; then
      reopen the store in a new runtime and resume the same run. Include
      a small owned-process abrupt-exit test to distinguish reopen from
      in-memory exception recovery; kill only an identified owned PID.
      A process-crash test does not establish power-loss durability.
      Test WithKey dedup, completed replay without remote calls, drift,
      and explicit unresolved Reconcile/Escalate/Fail outcomes.
      CONCURRENCY: barrier-controlled two-worker execution plus a stale
      worker resuming after lease expiry. Dialogue invokes the oracle
      before journal expect accepts one answer; expect fences JOURNAL
      progress, not the external action. Use Attempt(id,index) at a
      deduplicating provider and demonstrate the boundary with a
      non-deduplicating control. Leases are advisory. Specify whether
      Durable/TopicJournal requires a single writer; do not promise
      distributed safety without a tested ownership/fencing contract.
      DONE: deterministic diagnosed tests, explicit guarantee table
      (storage ACK, restart, concurrency, provider assumptions), scoped
      gates for the suites touched. No generic exactly-once claim; a
      shared-DB transaction and a remote API are different boundaries.
      Extraction: TestDurableAnyOp now lives in okay-durable; adapter
      wire tests live in okay-durable-persist. Agent tests cover legacy
      Tool APIs. Fixed-wire coverage is not process-crash coverage.
