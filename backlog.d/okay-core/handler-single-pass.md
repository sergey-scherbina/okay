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
      handler-fusion.floor (FusionBenchmark.fusedSWr, 13.7 us / 122641
      B/op per 1000 ops when set; re-measured 12.4 us / 122 624 B,
      specs/handler-fusion.md's 2026-09-27 re-measure): a stack of 3-4 handlers over a mixed program,
      measured against today's nested `handle`s. The no-tree roads of
      handler-fusion stage B (Eff/Func/Cont 0.58-0.86x) are REFUTED, so
      this must keep the Free tree and change only who walks it.
      RE-MEASURED 2026-09-27 (specs/handler-fusion.md, the last Results
      section): fused vs nested 1.36x / 1.31x on foldLeft-built programs
      and 1.05x right-nested. The bar is cleared only for the foldLeft
      shape. (The "60-70% is rotation" reading was corrected the same
      day: specs/left-nested-build-cost.md.) Parked behind
      `map-flatmap-pair-cost`, which measures what the nestedSW/nestedSWr
      gap actually is. ANSWERED by map-cost-residual (2026-09-27,
      specs/map-fusion.md, last section): the gap is allocation at ~2 µs
      an object per 1000 steps — the map node the user's `.map` builds,
      the boxed accumulator, the builder's closures — not the rotation
      and not `Bind(Return, g)`; the shape itself is ≤1.1x
      (left-nested-build-cost). The bar above stands as written.
      DESIGNED 2026-10-04 (operator): specs/handler-single-pass.md. `handle`
      REGISTERS a handler on a stack and `run` walks ONCE, with one match over
      the whole stack. The abstraction between the handlers and the machine is
      the step (`init`/`step`/`ret`), which every state-threading built-in has
      had since handler-one-step. Handlers that need `k` split the stack.
      Next: stage 0 there (re-measure, a two-state prototype).
      STAGE 0 DONE 2026-10-04 (spec, Results): one pass against nested is
      1.19x / 1.17x on fold-built programs and NO win (1.01x) on a
      recursion's shape, so the case is architectural. The class table
      beats the chain from 4 handlers (0.78x, 0.66x at 8) and ties at 2.
      Stage 1 (`Stepped`) is next if the operator takes the design on that
      evidence.

