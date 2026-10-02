- [ ] cont-list-combinators-one-walk — PRIORITY: LOW, a simplification
      found by the core review (core-simplify, 2026-10-02). Cont.scala's
      `traverse`, `foldIn`, `existsIn` and `findIn` (the macro's lowering
      of `xs.map`/`foldLeft`/`exists`/`forall`/`find` over the lazy `k`)
      each carry a private recursive helper of the same shape —
      `Bind(step(x), b => next(rest))` with an early exit or not. One
      deferred list walk with an early exit can hold all four; the
      public names, which ContMacro calls, become one-liners (~50 lines
      to ~15). WAIT FOR cont-stack-layer1-c: that lane (claimed
      2026-10-02) adds Either/Try/Set/Map lowerings beside these, and
      the unification should take them in too rather than race them.
      Pinned already by cont-stack-layer1-c's tests (a million on
      128 KB, the order of `k`'s resumptions); no new behaviour.
