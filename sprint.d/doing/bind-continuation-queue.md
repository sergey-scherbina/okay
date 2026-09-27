- [ ] bind-continuation-queue — ORDER 3 of the map-cost plan, a PROTOTYPE
      first (operator, 2026-09-27). This is the general fix for a map
      followed by a bind, and for left-nested binds in general. `Bind`
      holds a QUEUE of continuations (van der Ploeg and Kiselyov,
      "Reflection without remorse", Haskell 2014, the type-aligned
      sequence). `flatMap` on a Bind APPENDS in O(1), so no Bind is
      nested left and nothing is rotated. `map` appends a MARKED pure
      function, which the runner applies to the value itself with no
      `Return` and no `Bind(Return, g)`. Every continuation is still
      called by the interpreter's loop, so it is stack-safe, which is
      the property the map-fusion attempt lost.
      THE COST, and the reason it is a prototype: `Bind` is the node
      every interpreter matches (about 100 `Bind(Inject(e), k)` sites),
      `Free.resume` is 323 of 325 bytes (`TestInlineBudget`), and the
      2026-09 note that the queue is "not needed" measured stepping, not
      this. Prototype on a branch where `resume` folds the queue into
      one continuation when it hands out the head, so the handlers stay
      unchanged. A/B on BuildShapeBenchmark rowFoldM/stateFoldM/
      rowOneBind, FusionBenchmark nestedSW/fusedSWr and HandlerBenchmark
      relayPrebuilt/handlePrebuilt. Go on only if the map-heavy lanes
      approach rowOneBind (≈2x) with the map-free lanes unchanged.
      Related, not overlapping: relay-forward-same-inject (the forwarding
      arms' Inject), which is a different allocation.
