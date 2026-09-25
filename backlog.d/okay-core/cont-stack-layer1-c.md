- [ ] cont-stack-layer1-c — the rest of Layer 1 B, after
      cont-stack-layer1-b landed the answer-using bodies (`k(1) + k(10)`,
      `a :: k(x)`, interpolation, a block with `val x = k(1)`, a tail
      `if`/`match` with `k` in its branches, PState's `s => k(s)(s2)` as
      a `Fun` answer) on the explicit pending stack. What the transform
      still leaves opaque, each a lane with a red-first shape test and
      the 1M-on-128-KB zero-switch assertion:
      (1) conditionals NOT in tail position (`1 + (if c then k(1) else
      2)`, a `match` feeding an expression): needs a join point — a
      `Body` bind (`Then(body, x => rest)`) the walker rotates as
      `Free` does, or a `Fun` for the rest applied in both branches;
      (2) the KNOWN higher-order functions — `k` passed to `map`,
      `foreach`, `flatMap`, `fold` on the standard collections, `Option`,
      `Either` (`List(1, 2).map(k).sum`) — substituted with trampolined
      traversals the walker knows;
      (3) VISIBLE user functions along the path `k` flows (an `inline
      def`, a same-compilation `def` through `Symbol.tree`, TASTy with
      `-Yretain-trees`), rewritten and cached;
      (4) `direct { !k(…) }` inside a body, through okay-direct's
      machinery;
      (5) `try` around a call (the `finally` would have to run after
      the rest, which the pending stack can hold as a part), `while`
      with a call in the body, a lambda that is not the whole answer.
      NOT (6), unless for Scala.js alone: a FUNCTION answer (PState's
      `s => k(s)(s2)`) walked as a `Fun` with an `Ap` node was built in
      cont-stack-layer1-b and measured 2.8x the direct road on
      statePara (89 vs 32 µs), so it was taken out; on the JVM and
      Native Layer 3 runs it switch-free anyway. JS has no switch, so
      there — and only there — the walked function answer would lift
      the engine-stack bound on deep state-passing programs; a
      platform-conditional expansion is the shape if anyone needs it.
      Literature: Rompf, Maier & Odersky, ICFP 2009 (the selective CPS
      transform and its wall at code it cannot read).
