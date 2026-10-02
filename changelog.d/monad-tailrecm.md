## monad-tailrecm — `tailRecM` for every okay Monad, and cats' Monad from ours

Operator ask, 2026-10-02. `TailRecM[F]` (Monad.scala: the class and a
one-line companion given) is answered for every okay `Monad` by the
extension `M.tailRecM(a)(f)` in Effects.scala, beside `!.loop` and
`foldMap`: each iteration a `Cont.shift` whose body hands the
continuation to the monad's `flatMap`, so it is stack-safe on an EAGER
carrier too (a million through `Option`; the naive `flatMap` recursion,
as a mutant, overflows). With it `ToCats.monad`: cats' `Monad` from any
okay `Monad`, cats-laws `MonadTests` green on it. docs/interop-classes.md
no longer says ToCats has no Monad; typepedia names the class.
CORRECTED by eager-carrier-depth (same day): the eager-carrier stack
claim held only on an 8 MB stack; a 128 KB thread overflowed at 1 000,
Scala.js at 300-1 000. `TailRecM` is now the carrier's own loop and
`foldMap` is the target's `tailRecM`.
