## foldcont-doc — `foldCont` is `foldMap` into `Cont`, and what its result is for

Operator ask, 2026-10-02. The doc comment on `Effects.foldCont` says what
it is (`foldMap` into `Cont`, beside `Static.foldMap`) and what to do with
the `A /> S` it returns (close it with `/`). TestFoldCont interprets one
program three ways by the choice of `S` (the answer, every answer, a
function of the state) and pins `runWith = foldCont / identity`;
docs/contract.md gains the section "`foldCont`, and what to do with its
result", with `handle` as the fourth choice (`S` another program).
