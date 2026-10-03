- [ ] cont-list-combinators-one-walk — PRIORITY: LOW, a simplification
      found by the core review (core-simplify, 2026-10-02). Cont.scala's
      `traverse`, `foldIn`, `existsIn` and `findIn` (the macro's lowering
      of `xs.map`/`foldLeft`/`exists`/`forall`/`find` over the lazy `k`)
      each carry a private recursive helper of the same shape —
      `Bind(step(x), b => next(rest))` with an early exit or not. One
      deferred list walk with an early exit can hold all four; the
      public names, which ContMacro calls, become one-liners (~50 lines
      to ~15). cont-stack-layer1-c's rest (32d30c2ad) landed first and
      made one difference load-bearing: `existsIn`/`findIn` walk a
      MEMOISED LazyList (an infinite receiver answers at the first hit),
      `traverse`/`foldIn` a strict List. One walk over the LazyList keeps
      both, `traverse`/`fold` simply never stopping early. Its Set/Map
      traversals reach these four (Either/Try are rewritten to a match,
      not walked). Pinned by
      that lane's tests (a million each on 128 KB, the infinite
      receiver, the order of `k`'s resumptions); no new behaviour.
