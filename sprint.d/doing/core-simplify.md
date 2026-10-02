- [ ] core-simplify — the core review's small, behavior-free items
      (2026-10-02, operator "Да"): (1) Free.scala's comments — `resume`
      called "THE rotation, and the only one" beside `resumeRun`, the
      reflection-without-remorse paragraph written twice, `Free.defer`
      repeating `Freer.defer`'s body; (3) Delimited.scala's capture
      stanza (the `identical` claim, two `liftCo`s, `k` and `Next`)
      written twice, in `cut` and in `nearest` — one inline helper,
      bytecode-equal by construction, checked by one JMH lane. Filed
      to the backlog instead: (2) Cont.scala's four list combinators
      as one deferred walk (after cont-stack-layer1-c, which adds more
      beside them), (4) `Cont0.Handling.ever` as per-machine state,
      (5) the 2- and 3-handler `handle` overloads.
