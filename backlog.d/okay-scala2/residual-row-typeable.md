- [ ] residual-row-typeable — INVENT a `TypeableK` for the facade's
      residual row, so that okay's FAIR search (`Logic.interleave`,
      `fairBind`, `>>-`, `observe`) can be wrapped for Scala 2.13.
      Operator, 2026-09-23: "выдумай. Запиши это в беклог - выдумать".
      The problem: those combinators need `TypeableK[F]` for the rest of
      the row, F. On the Scala 2 side the rest is only a phantom
      intersection, and every program is stored at `Rows.Top`
      (`[X] =>> Any`), so there is no F to test.
      The candidate to prove or refute: a test built as the COMPLEMENT
      of the known side. For a program at `Choose + Top`, "an operation
      of F" = "not a `Choose` operation", i.e.
      `x => !summon[TypeableK[Choose]].test(x)`. The core already says
      that a row split "tests one side and takes the other by
      exclusion" (Handler.scala, the no-generic-instance note), so a
      complement is that exclusion, packaged as an instance.
      What has to be shown before it is trusted, each as a test:
      (1) with another effect really present in the rest (State, a user
      `Effect`), interleave forwards its operations, and the other
      effect's handler still sees them in order;
      (2) nested splits: interleave inside a handler that itself splits
      the row, where F may stand for a different set, so that a
      complement taken at the wrong level cannot misroute;
      (3) multi-shot: a branch resumed twice sees its own state (the
      Memory property);
      (4) an infinite left branch cannot starve the right one: the
      fairness itself, from Scala 2.
      If (2) fails, the fallback is a narrower door: a fair search only
      over `Eff[Choose, A]` (nothing else in the row), where the
      complement is trivially empty.
      Needed by: `scala2-choose-search` (queued), which ships without
      fairness until this lands.
