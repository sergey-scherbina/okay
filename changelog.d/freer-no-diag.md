## freer-no-diag — `Diag` out of `Freer`; one system of combinators for two arities

Operator, 2026-10-04: solve the unary-member-in-an-indexed-row problem without
`Diag`, with one simple, coherent system of `+` combinators.

- `Freer` has one case fewer: `Return`, `Inject`, `Bind`, `Delay`.
- A unary member's operation is proven on the diagonal by its typed door
  (`Unary[G]` reduces only at `S = R`); a handler asks `Indexed.onDiagonal`
  for the equality — one isolated claim, no allocation. `State.handleIndexed`
  and `Tx.interpret` read their operations under `Inject` alone.
- The system (specs/indexed-effects.md): `G[_, _, +_]` the one kind in the
  tree, `+~` its sum, `Unary` the one bridge, `+` agreeing
  (`Unary[F + G] = Unary[F] +~ Unary[G]`, pinned at compile time).
