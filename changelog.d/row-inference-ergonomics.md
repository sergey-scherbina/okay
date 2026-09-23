## row-inference-ergonomics - what a row's type actually needs, verified — and one backlog claim refuted

The backlog entry that filed this lane wrote its friction from
memory; tested before writing anything else, one of its own specific
claims ("`Once + Async + Pure` written out because `X + Pure` and
`X` do not unify without help") does not appear anywhere in the tree
and is FALSE once probed — a bare `X` satisfies an `X + Pure` slot
and the reverse, both by plain ascription.

- `ProbeRowInference.scala` (9 tests, kept permanently): the real,
  verified traps — `.at`/`.plus` need an explicit import even inside
  package `okay` (hit five times today, the most common one);
  `flatMap` between two different effects needs BOTH sides widened,
  not just one; a method expecting `R ! (F + G)` does not recover
  that shape from an argument already typed as the expanded union
  without explicit type args at the call site (`Delim.Stacked`'s own
  `push[R, F]`/`run[R, F]` needed exactly this) — and the REFUTED
  ones: `X + Pure` vs `X` unify fine either direction; a union's ACI
  is FULL (associativity AND commutativity), not just
  re-parenthesization as a first draft of this lane's own probe
  wrongly assumed.
- docs/typepedia.md, "Rows: what infers and what you spell": a new
  section beside "Recurring gotchas," stating only what testing
  confirmed.
- The spike this lane's item (3) asked for (a normalizer for
  `X + Pure`) is answered as unnecessary — the friction it would fix
  does not exist. The one-line-overload item (2) is answered by
  citing `SharedOnce.run`/`runIn`, which already is that pattern.
