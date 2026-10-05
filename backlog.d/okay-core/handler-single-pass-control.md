- [ ] handler-single-pass-control — PRIORITY: MEDIUM (2026-10-05, stage 3
      of specs/handler-single-pass.md). A handler that needs `k` (Throws
      with its catch, Choose's multi-shot, `Handler.control`, Maybe)
      is not `Stepped`. In a chain of `handle`s it is a run of its own
      that the stack's walk forces: correct (TestHandledStack's Throws and
      Choose laws), but the chain is cut into two stacks around it, each
      with its own walk. The design: a non-stepped handler SPLITS the
      stack in place. What is inside it stays one fused walk, it runs as
      today (its own loop or capture), and what is outside fuses again,
      all inside ONE `Handled` node, so a chain is one node whatever its
      handlers are. Laws: the oracle, the stack against the nested runs,
      with a control handler inside, outside and in the middle. Measure
      FusionBenchmark.handledTSW (Throws outside a stack) before against
      after.
