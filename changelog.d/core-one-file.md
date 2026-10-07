## the core's interface in one file

Lane core-one-file (the operator): `Answers.scala` and `Control.scala` are
part of `Effects.scala` — the effect interface and its facade, the control
interface of delimited continuations, and `TypeableK`/`Effect`/`Answers`,
one file in the core beside `Monad.scala`.
