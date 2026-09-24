- [ ] okay2-intersection-row — a DESIGN QUESTION for the operator, with a
      measurement behind it (2026-09-24, scala-cli, 2.13.18 -Xlint
      -Werror). okay2's row is `sealed trait +[F, G] extends Row`, which
      neither commutes nor associates, so it carries `Member`, `Sub`,
      `NotPure`, `Remove` (+ `Aux`), `.at`/`.plus`/`.bind` and an
      `…At` twin of every handler. The other Scala 2 road — the one the
      facade `okay-scala2` already takes — is a CONTRAVARIANT phantom
      row that is an INTERSECTION: `Free[-R, A]`, `R = State[Int] with
      Writer[String]`. Measured on a 150-line model: all SIX handler
      orders over a three-effect program infer the residual with no
      annotation; widening is subtyping (no cast, no witness);
      `flatMap[R1 <: R]` finds the union of two programs' rows;
      `A with B` and `B with A` are mutual subtypes; a helper
      polymorphic in the rest (`Int ! State[Int] with R`) works; 1M
      left-nested binds run. The ONE trap: a handler's PARAMETER must be
      spelled with the class (`p: Free[State[S] with R, A]`), not
      through the arg-swapping alias `A ! (...)` — through it 4 of 6
      orders fail (same finding as the facade's stage 19).
      COST: `Inject` holds its operation as `Any`; a handler recovers
      `Op[X]` by the class test, the same one cast `Split.split` makes
      now, so the cast count need not grow if the kernel owns it.
      Distinct is unchanged (class test either way). WHAT IT WOULD
      TOUCH: every module of okay2 (the row is in every signature) —
      a rewrite of the row layer, not a patch. Decide before okay2
      grows further: each stage built on `+`/`Remove` raises the price.
