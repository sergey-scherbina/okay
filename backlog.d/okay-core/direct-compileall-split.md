- [ ] direct-compileall-split — `Direct.compileAll` (Direct.scala
      line 641 to the end of the file, ~1460 lines) is ONE method of
      nested defs: marks and colouring, statement/bind lowering, the
      `foreach`/`map` loops, the `lazyOnce` lowering, `substUses`,
      `colourlessVal`, `deferSelfCalls`. Nothing is broken and nothing
      here is measured slow; the cost is that no phase can be unit-
      tested on its own and every edit re-reads the whole macro. Split
      by phase into private objects/methods taking `(using Quotes)`
      — the file's own `applicativeOnly` (line 334) already shows the
      shape. NOT a lane of its own: do it the next time the macro is
      touched for a real reason, as the first commit of that lane, so
      the behaviour change and the move are separable in the diff.
      Found in the 2026-09-20 review.
