- [ ] handler-single-pass — PRIORITY: MEDIUM (design + measure first).
      Found by the core review 2026-09-26 (specs/core-gaps.md). Every
      `Effects[Free].handle` is ONE FULL WALK of the tree, and an operation
      the handler does not claim is rebuilt on the way out
      (`Free.Inject(e).flatMap(x => again(k(x)))`, Effects.scala): a `Bind`
      plus a closure per forwarded op per layer. A row of n effects run by
      n handlers costs n walks. `Handler.union`/`<|>` fold them into one
      pass, but only for comonadic (tail-resumptive) handlers. The micro-
      optimisations of the loop are already measured and refuted
      (runfree-inlined-rotation, runfree-inlined-small-step,
      handler-fusion-step), so the ceiling is the architecture. Road to
      price: ONE runner holding a stack of handlers, where an operation
      goes straight to its handler and the continuation is not
      re-materialised per layer. That is evidence passing (Xie & Leijen,
      "Generalized Evidence Passing for Effect Handlers", ICFP 2021),
      and it is how Koka and kyo run. The bar is task-state
      handler-fusion.floor (FusionBenchmark.fusedSWr 13.7 us / 122641
      B/op per 1000 ops): a stack of 3-4 handlers over a mixed program,
      measured against today's nested `handle`s. The no-tree roads of
      handler-fusion stage B (Eff/Func/Cont 0.58-0.86x) are REFUTED, so
      this must keep the Free tree and change only who walks it.
