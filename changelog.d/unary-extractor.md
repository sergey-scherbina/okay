## unary-extractor — the extractor that reads a unary operation at `F[A]` is the bridge's own

Operator, 2026-10-04: "сделай такой экстрактор в Unary".

- `object Unary` beside the type: `case Unary(op)` on a diagonal node
  (`Freer.Inject[Unary[F], R, R, A]`, any `R`) answers it viewed at
  `Unary.Op[F]`, whose field is `F[A]` — the one claim, the bridge's own
  definition read back, nothing allocated. `Free.Inject.unapply` is it at
  `Unit, Unit` (`Free.Op` moved to `Unary.Op`). TestFreerPara pins a match at a
  non-`Unit` diagonal.
