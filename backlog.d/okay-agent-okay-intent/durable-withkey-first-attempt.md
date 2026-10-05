- [ ] durable-withkey-first-attempt — P1 / correctness: send the persisted
      idempotency key on the FIRST attempt as well as every retry.
      Operator request 2026-10-05, after the durable-execution review.
      BASELINE (source review, not a failing test run):
      `okay-agent/src/main/scala/okay/agent/Durable.scala`, `over.handle`:
      the fresh `case None` calls `execute(..., op)`, while incomplete
      `OnRepeat.WithKey` calls `execute(..., J.withKey(op, entry.key))`.
      The remote service therefore need not see the journal key on the
      first request. A crash after remote success and before `complete`
      can cause another action on retry. `TestDurable`'s existing WithKey
      test seeds an incomplete entry; it does not exercise that sequence.
      HOW: specify the WithKey transport contract, then apply `withKey`
      on fresh execution too; intent must be durable before the request.
      Preserve the ORIGINAL operation fingerprint for drift detection.
      Define precedence if an operation already carries a supplied key;
      keep generic `Journalled[Op]` and the Tool adapter consistent.
      DONE: a fresh request carries `Entry.key`; a deduplicating fake
      provider succeeds, journal completion fails, and recovery sends the
      SAME key and causes exactly one business action; completed replay
      makes no remote call. Also exercise the generic Journalled seam.
      Adopt Diagnosed in edited suites; gate TestDurable and affected
      behavior consumers, per AGENTS.md. No real financial API required.
      Related: durable-run-scoped-keys; durable-recovery-contract-tests.
