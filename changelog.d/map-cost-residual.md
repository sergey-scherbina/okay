## map-cost-residual - the map+flatMap gap named: allocation, not the two binds; foldM one closure a step (1.11-1.14x)

- specs/map-fusion.md read the 2.27x of `op.map(f)`-then-bind over one
  flatMap as "the step's two binds". A ladder of control lanes in
  BuildShapeBenchmark (`rowUnwrap`, `rowOneBindAcc`, `rowFoldMAcc`, each
  its own `jmh-lane.sh` run) says the two binds were a fifth of it. The
  rest is allocation at ~2 µs an object per 1000 steps: the `Bind` +
  `Mapped` that `.map` builds and the builder throws away (6.3 µs, 48 B a
  step, half the residual on its own), the boxed erased accumulator
  (~2 µs), and the builder's second closure (~4). No rotation, no
  `Bind(Return, g)`: `rowUnwrap` has neither and carries half the gap.
- `!.foldM` builds its continuation in place (the `step` helper inlined
  into `go`, one closure a step): rowFoldM 23.45 → 21.12 µs (1.11x,
  −40 B a step), stateFoldM 21.03 → 18.43 (1.14x), arms alternated in one
  session. Every `foldM`/`each` caller gains with no edit.
- Also read from the diff: the refuted "safe form" of map fusion (form 2)
  made `inline def flatMap` a call with two type tests on every bind in
  the library; its 21% priced those tests. Its ceiling is two objects a
  step at that price, so it is priced and not retested. specs/map-fusion.md.
