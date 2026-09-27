## one-bind-hot-steps - `!.foldM`/`!.each` fold a step's trailing map into their next step: 1.18-1.36x

- `map` leaves a `Free.Mapped` continuation. It is the same node and runs
  the same way as `a => Return(f(a))`, but a builder can read the function
  back. `!.foldM`/`!.each` meet a step written `op.map(g)` and build
  `Bind(op, y => next(g(y)))`: one bind a step instead of two nested left,
  which were rotated on every step. It is stack-safe because `next` only
  builds the rest and calls no continuation. `Free.flatMap` itself still
  cannot do this (map-fusion, refuted: Delim's composed continuations).
- A/B against master: `stateFoldM` 26.3 → 19.3 µs (1.36x, −31% B/op),
  `rowFoldM` 28.5 → 24.3 µs (1.18x, −21%). relayPrebuilt and fusedSWr are
  unchanged, with identical bytes. Every library fold converted to
  `foldM` (Choice, Prob, Maybe, Chronicle, Retrieve, Repair and the rest)
  gains with no edit of its own. The ceiling, a hand-written one-flatMap
  step (`stateOneBind`, the new lane), reads 10.1 µs.
- A `foldEach` combinator was drafted and dropped, because fixing
  `foldM` made it redundant. docs/guide.md: "Folding with effects".
  Inventory row for `step`, which stores `next` and never calls it.
