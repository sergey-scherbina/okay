## okay2-validated-static - every error, and the free selective

On the stage-13 typeclasses (specs/okay2.md stage 16): `Semigroup`,
`Monoid`, `Group`; `Validated`, whose `Selective` combines errors so a
`traverse` reports all of them, with no Monad on purpose and `andThen`
for the short-circuit; `Static`, the free selective — `leaves` before
running, `toFree` running at most one branch of each select, `foldMap`
into any Selective (fifty keys in one round trip, 50 000 leaves without
stack). The Scala 3 suites, 13 tests.

Docs: docs/okay2.md section 20.
