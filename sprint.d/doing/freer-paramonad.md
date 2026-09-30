- [ ] freer-paramonad — `Freer[G, S, R, A]` (Free.scala) IS Atkey's
      parameterised monad structurally (`Bind` composes the indexes,
      `Return` sits on the diagonal) but has no `ParaMonad` instance:
      only `Control[Cont]` has one, at the Shift signature. Operator ask
      (2026-09-30): make Freer the instance, for every signature G —
      `given ParaMonad[Freer.Para[G]]` with `Para[G] = [A, S, R] =>>
      Freer[G, S, R, A]` (the trait's order is value-first). Beside it,
      the operator's question answered by compiling, not by argument
      (ProbeFreerPara): an EFFECT whose signature carries the indexes —
      `enum PSt[S, R, X]`, typestate as data — under which reading of
      `S`/`R` a handler for it types on THIS base (`+R`): the
      answer-type reading (PState's, handler = `G ~> Shift`) and the
      before/after-state reading (McBride's `IxFree`, handler threads
      the state). Findings go to specs/freer-base.md. Gate: additive —
      `okayJVM/testOnly okay.TestFreerPara` + `affected master
      Test/compile`.
