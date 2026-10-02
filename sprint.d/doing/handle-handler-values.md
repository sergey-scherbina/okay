- [ ] handle-handler-values — level 1 step 3 (operator, 2026-10-02: "Да"
      to `p.handle(State(5))` over `State.run(5)(p)`): each ready effect
      gives a handler VALUE and `handle` applies any of them —
      `p.handle(State(5)).handle(Throws.either).run`, `reset` one of them.
      The old runners stay as aliases for a transition. (2026-10-02)
