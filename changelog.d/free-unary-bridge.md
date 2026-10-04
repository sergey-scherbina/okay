## free-unary-bridge — PROBE, refuted: `Free` keeps `Lift` as its bridge

After freer-no-diag (operator: one system of combinators for two arities): could
`Free[F, A]` be `Freer[Unary[F], Unit, Unit, A]`, one bridge for both arities?
Core compiles, but the empty row breaks inference — `Pure = Nothing`, and
`Unary[Nothing]` reduces to `Nothing`, losing the row, so `runWith`, `!.run`,
`peek` on `A ! Pure` find no `Answers[Pure]`. The code was not landed; the law
that joins the two bridges where `Free` lives is in specs/indexed-effects.md.
The operator's `type Pure[+A] = Nothing` fixed that for Scala 3 (with the empty
row spelled `Pure` in 87 files) but broke Scala 2.13: the okay-scala2 facade
reads `Free` through TASTy, whose Scala 2 reader rejects a match type there.

