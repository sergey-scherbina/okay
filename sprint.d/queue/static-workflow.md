- static-workflow — the durable spine without `ArrowApply`
  (specs/static-workflow.md, stage 0 the spec LANDED 2026-09-18). The
  operator's question from Appendix A's last sentence: the opacity is
  `flatMap`'s closure, not the continuation's frames — so build a
  workflow from everything EXCEPT the monad. WHAT: `Proc[Q, A, X, Y]`,
  a free arrow with `Choice` and an `Iter` node (Elgot iteration — the
  one thing the operator's list lacked, and the reason Appendix A
  refused the static route) whose leaves are the questions `Wf`
  already journals. `toProgram` runs it through `Dialogue.workflow`
  UNCHANGED; `walk` folds the position with no runtime. WHY: the
  deploy check becomes structural and pre-deploy, `Replayable` has
  nothing left to police, fault injection over the leaves is
  exhaustive, and two derivations of the position must agree. HOW:
  Proc.scala beside Wf.scala; instances of `Optic.Arrow`/`Optic.Choice`
  so every optic applies; stages 1-4 in the spec, stage 5 gated with
  triggers. DONE-WHEN stage 1: the book's booking as a `Proc` runs on
  the landed engine, a monadic journal is accepted by the static
  booking, and `walk` agrees with `Wf.replay` by property.
