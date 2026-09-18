## optics-arrow-instances - the "one Profunctor" sentence becomes a fact

`Optic.Arrow` had ONE instance: `Mealy` in okay-lex. The comment beside
the class says arrows and optics are written on one `Profunctor` here,
which with one instance was a remark about the literature. There are
three now, and the shared `ArrowLawsSuite` a sibling landed this
morning holds each to the same fifteen laws — three lines per carrier,
which is exactly what that lane was written to make possible.

THE HAZARD THIS LANE HAD TO AVOID, and it would have broken the
library rather than the lane. `Arrow` extends `Strong`, so a second
given carrying `Arrow[Function1]` makes `Strong[Function1]` ambiguous
at EVERY optic call site — every `set`, every `modify`, everywhere.
The existing `opticFunction1` is widened instead: one given, both
roles, no possible collision. A test asserts a lens still resolves at
`Function1`, because that is the thing that would have broken.

THE KLEISLI IS A `def`, NOT A GIVEN, for the same reason one level
along: `opticStarTraversing` already provides `Strong[Star[F]]` for
every `Applicative[F]`, and a second given would make every optic at
`Star` ambiguous the moment somebody imported a `Monad`. So
`Optic.kleisliArrow[F]` is asked for by name. The asymmetry is the
honest one - a program written against `traverseOf` should not stop
compiling because a monad came into scope.

WHAT IT DOES NOT BUY, written in the code beside the instance: with a
monad in hand, `fanout` is a for-comprehension with extra syntax. This
exists so the laws can be stated at the carrier and so theory ch. 10's
table has its second row, not as a better way to write effects. What
it does buy on plain functions is `split` and `fanout`, and those have
call-site tests rather than only laws.

34 tests: fifteen laws at each new carrier, plus four on use.
