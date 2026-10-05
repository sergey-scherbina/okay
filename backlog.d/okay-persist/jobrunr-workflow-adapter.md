- [ ] jobrunr-workflow-adapter — P3 / conditional: use JobRunr as an
      optional JVM scheduling/queue adapter around okay durable workflows.
      TRIGGER: a concrete adopter already uses JobRunr and needs its
      dispatch/operations integration. Until then retain the self-contained
      Worker path; do not copy JobRunr's scheduler/dashboard or add a core
      dependency merely because the article exists.
      FIRST verify the required pinned edition/API/license supports the
      customer's scenario. Specify the facade before any dependency:
      core depends only on our dispatch interface; JobRunr adapter behind
      an import and optional jar, absence refused by name per AGENTS.md.
      OWNERSHIP: okay's journal owns workflow step progress/recovery;
      JobRunr dispatches a stable run ID and wakes the worker. Define
      scheduling, retry/backoff, cancellation and status projection at
      this seam so two engines do not independently retry business steps
      or maintain competing authoritative progress records. Duplicate
      dispatch is expected; reuse the tested idempotency/ownership
      contract, not an assumption that queue delivery is exactly once.
      DEPENDS: durable-recovery-contract-tests; demo is the acceptance
      fixture. DONE: the same process works with our dispatcher and the
      optional adapter; duplicate dispatch and restart preserve behavior;
      no JobRunr type leaks into core APIs; scope integration tests to the
      adapter and document what remains authoritative in okay. If the
      trigger never fires, this stays deferred.
