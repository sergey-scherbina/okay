- [ ] logic-cut-releases — PRIORITY: LOW (trigger). Found by the
      resource-release work of 2026-09-26 (specs/core-gaps.md stages
      5b-8). `Logic.cut`/`once` (Logic.scala:65) keep the first answer of
      `msplit` and DROP the rest of the search, `(a, _)`. A Resource scope
      INSIDE a branch that is suspended on a NON-EMPTY `choose` when the
      rest is dropped never releases. It is not `Final` (a non-empty
      choice resumes), and nothing tells the scope its continuation is
      gone. ZIO has no such case (no multi-shot continuations). OCaml 5's
      answer is the handler calling `discontinue` on what it drops. Here
      that would be `cut` discontinuing the dropped remainder: descend it
      to its next operation, as `Drive.discontinue` does, and release a
      `Discontinue` there. That needs `Resource.run` to forward `Choose`
      operations wrapped as a `Discontinue` too, which today only the
      Async guard does. TRIGGER: the first consumer that acquires inside
      a search it cuts, or a review that finds one. Until then: acquire
      OUTSIDE the search, or use `bracketNow` inside a branch.
