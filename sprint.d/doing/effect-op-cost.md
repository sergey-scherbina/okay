- [ ] effect-op-cost — the price of ONE effect operation, okay's hottest
      path. ProbeRowCost (exact): a recursion level performing one
      `State.get` costs 80 B against 40 B for a bare `!.tailcall` level;
      `State.get` expands to `effect(Get())`, a fresh `Get` and a fresh
      `Inject` per call although neither carries data. First: measure
      what a shared node would save (a probe variant), then decide how
      to share it without breaking the `Direct.staged` macro, which
      recognises operations by their constructor term. Same for
      `Reader.ask`. (2026-09-26, operator: "the goal is to make okay
      faster")
