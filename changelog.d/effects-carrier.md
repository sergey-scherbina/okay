## Effects names its continuation carrier; the machine folds natively

Feature, lane effects-carrier (specs/freer-min.md, stage 32). `Effects[M]`
gets `type C[_, _, _]`, the carrier `foldCont` folds into, and its
`Control`; `foldCont` takes `Interpr[F, C, S]` and answers `C[A, S, S]`;
`runWith`, `handle` and `convert` speak to the carrier through
`control.shift` and `control./`, never `Cont`. The instances declare their
carrier in their type, `Effects.Aux[M, C]`: `Free` and `Eager` at `Cont`,
so a handler `F !> S` is what it was, `Effects[Free].handle(…)(h)`
included (`Effects.apply` summons with the carrier bound). `Effects[Prog]`
(okay-cont) folds at the machine's own carrier, `Carrier[A, S, R]` with
`Cont.control`: `foldCont` is the program run with the handler as its
dispatch, `/` the delimiter — `handle`, `convert` and `reify` run on the
machine. Chosen, as before, by the given at compile time.
