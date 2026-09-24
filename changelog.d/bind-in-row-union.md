## bind-in-row-union - a bind across rows, the other row inferred

The operator's ask (2026-09-23), after a core test spelled one bind
with two `!.widen`s and six type arguments. `Free` stays invariant in
its row — a measured decision — so `flatMap` refuses a continuation
in another row; `at`/`plus` moved programs, not binds.

- `Row`: `p.bindIn(f: A => B ! G): B ! (F + G)` and `p.thenIn(q)`
  — two `plus` coercions and one `flatMap`, no walk, no witness: the
  gain is INFERENCE, `G` read off the continuation and `F` off the
  receiver.
- The ergonomics questions the operator asked, answered in the spec:
  a spelling, kept for its inference; NOT `flatMap` itself (the
  hottest path, and `for` binds by name — mixed rows in a `for` stay
  `.at[R]` per generator or a `direct` block); when to reach for
  `plus`/`at`/`bindIn`/`direct`, one line in the guide.
- `TestBindIn` (core, 4): the two-`plus` equivalence, three rows in
  two binds, the tree is one `Bind` on the Ask (by stepping and by
  counting under a relay), `thenIn`.

Gate `affected master` green, no warnings.
