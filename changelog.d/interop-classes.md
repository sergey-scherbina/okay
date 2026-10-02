## interop-classes — okay's class ladder across cats, ZIO and kyo

Operator ask, 2026-10-02. okay's `Functor`/`Applicative`/`Selective`/
`Monad` now answer for their types (`IO`, `Eval`, cats' `Validated` with a
real `select`, `ZStream`, kyo's `A < S`), and cats' classes for ours
(`okay.Validated` accumulates under cats' `traverse`, `Static` stays
static, `A ! Async`/`Par` is cats' `Parallel`, `A ! Choose` a `MonoidK`).
Generic bridges `okay.cats.FromCats` / `okay.cats.ToCats`, one import
each. Parallel applicatives for ZIO and kyo are explicit
(`ZioClasses.parApplicative`, `KyoClasses.parApplicative`). ZIO core and
kyo have no type classes, so outward stops at cats. Spec
specs/interop-classes.md, guide docs/interop-classes.md.
