- [ ] okay2-dollar — the Scala 2 twin fell four lanes behind on
      2026-09-25 (operator approved the plan). okay2/Delim.scala has
      `shift0` and a `Stacked` object but no `dollar`, no stacked
      `shift0`/`dollar`, no Layered, no Lexical. THIS LANE: `dollar` in
      the okay2 machine as in Scala 3's delim-dollar — a `Ret` frame
      (body `R0` to prompt `R`), the cut as `Plain | AtRet | NotFound`
      with `P0` an existential the okay2 way (Split.at, memory
      okay2-split-at-pattern; one isolated cast at most, said why),
      `push` keeps its plain mark, control-captures to a dollar refused
      by name; TestDollar as a twin (the two rules, re-installation,
      "reset0 = pure $" on six bodies, R0 ≠ R, ICFP 2011 with rets,
      100 000 nested dollars in constant stack — specs/stack-safety-okay2.tsv
      row if a loop is added). Stacked `shift0`/`dollar` only if okay2's
      value-stack `Has` can carry `Below`; if not, write why in
      specs/scala2-twin.md and file it. Layered and Lexical for okay2 are
      SEPARATE items in okay2/backlog.d, filed by this lane, not built.
      Gate: `cd okay2 && ../scripts/gate.sh test`. DONE WHEN: the twin
      suite is green and the okay2 gate passes; stage row in
      specs/shift0-dollar.md Results.
