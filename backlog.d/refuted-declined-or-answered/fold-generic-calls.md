- fold-generic-calls — ANSWERED 2026-09-28 (fold-each-residual-split,
  specs/map-fusion.md): the `Function1`/`Function2` calls a `foldEach`
  step makes to the user's `f` and `combine` were named as half of the
  1.8x over the hand loop; measured (rung D1 → D2), they are 0.7 µs per
  1000 steps inside a ±0.9 error, and the 16 B a step they seemed to
  carry is the closure that captures them. The half was the per-step
  `Vector.apply`, removed. Do not chase the calls.
