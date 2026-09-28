- [ ] dlm-laya-smoke — first prove the local deterministic DLM, then compare
      it with Laya in a later lane. Plan: `specs/dlm-laya-smoke.md` first;
      add a fixed Russian double-charge → scoped-confirmation transcript to
      `okay-dlm` with action/record replay assertions and an explicit
      caller-side payment-ledger guard: a text claim opens a review, never a
      refund, and a nonexistent payment makes no refund call. No network,
      Python, model download, key or Laya dependency belongs in this slice.
      `dlm-laya-comparison` remains deferred until this DLM-only fixture is
      useful. Done when the deterministic suite is self-contained and the
      documentation renders the state → rule → action → ledger-evidence
      trace honestly.
