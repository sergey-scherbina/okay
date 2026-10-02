- [ ] refine-cases-macro — operator 2026-10-01 ("пока без макроса. Макрос в
      беклог"). `Refine.cases[A, B] { case p(x) if g => e; … }`: a Scala 3
      macro turning each CASE of a match into a named refine step (name =
      the case's source), combined with `orElse` (first case wins, as in a
      match) — `Refine.anyCase` with `or`, so overlapping cases read
      `Unclear` in a test. Per-case refusals in the verdict ("guard … was
      false", or the extractor's OWN refusal by running it), write through
      the extractor when the right side is the bound variable (`Left`
      otherwise — RefineLaws shows it), and the plain match also expanded
      at the call site so a sealed type keeps its exhaustiveness warning.
      okay2: old scala.reflect macros, heavier — likely `unapply` only.
      Builds on refine-match (`Refine.unapply`, landed).
