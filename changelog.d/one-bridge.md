## one-bridge — `Pure[+A] = Nothing`, `Lift` gone: one bridge for both arities

Operator, 2026-10-04: "В scala 3 используй Pure[+A] = Nothing и убери Lift …
найди решение для 2.13".

- `type Pure[+A] = Nothing`; the empty row is written `Pure` everywhere (87
  files: `Resource.run[A, Pure]`, `Async.run[A, okay.Pure]`, …).
- `Free[F, A] = Freer[Unary[F], Unit, Unit, A]`; `Unary[F] = Diagonal[F]#L`
  (the projection, kept for inference, now around the match type).
  `Freer.Lift`/`Lifted` deleted. One system: `G[_, _, +_]` in the tree, `+~`
  its sum, `Unary` its only bridge, `+` agreeing.
- Scala 2.13: its TASTy reader refuses a match type, and reaches `Unary`
  through a program in a constructor it loads. Fixed where the probe met it —
  `okay.ui.Nav.Run` holds `Nav.Launch` (built by `Nav.launch(prog, s)`), the
  facade's `UiHost` holds `UiHostBody` — and the rule is written down
  (specs/scala2-facade.md).
- A direct GADT match on an operation under `Free`'s nodes no longer
  refines: through `split` (TestSqlPure). `SparkFrames` names `via`'s row.
