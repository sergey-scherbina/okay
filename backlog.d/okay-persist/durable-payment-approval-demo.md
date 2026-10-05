- [ ] durable-payment-approval-demo — P2 / adopter proof: one runnable
      business process that survives restart without duplicate payment.
      DEPENDS: durable-recovery-contract-tests; agent key fixes if that
      handler is used. Reuse okay-workflow Wf and okay-persist Worker,
      Dialogue, FileStore, Signals/statuses; do not build another engine.
      FLOW: prepare a synthetic payment request -> wait for human
      approval -> restart while waiting -> deliver approval -> execute
      payment with a stable attempt key -> complete. Show another restart after provider success
      before journal answer and deduplicate at a persistent fake provider;
      approval event delivery and consumption must survive restart too.
      The fake provider's record is independent of the workflow journal,
      so the crash window is real rather than hidden in one transaction.
      HOW: a small CLI/demo with explicit run ID, local store, inspectable
      status/history, approval input and documented recovery commands.
      Add a controlled unknown-outcome path showing reconciliation or
      escalation instead of an unsafe blind retry. Demonstrate drift and
      the existing version/patch policy without reimplementing them.
      DONE: a newcomer follows the README end to end across separate
      process launches; one provider business action, preserved approval
      wait, recorded answers reused, inspectable final outcome. Automated
      fixture covers the same path. Document limits: not a compliance
      certification or proof of production-scale operations. Scheduler
      scaling stays in existing workflow-timers-index, not this task.
