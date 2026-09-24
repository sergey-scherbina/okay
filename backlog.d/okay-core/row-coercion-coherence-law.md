- [ ] row-coercion-coherence-law — PRIORITY: MEDIUM (a law test).
      Since widen-is-a-coercion (2026-09-23) a program reaches a wider
      row in two ways: `!.widen`, which is `Row.into` (a coercion), and
      `!.normalize`, which walks the tree. The meaning must not depend
      on which of the two was used. That is COHERENCE of effect
      subtyping: Biernacki & Polesiuk, "Logical relations for
      coherence of effect subtyping" (TLCA 2015; LMCS 2018). okay2 has
      already produced one real incoherence: an intersection row's
      `#Op` is the LAST parent's, and the result is a
      ClassCastException (memory okay2-intersection-row). THE LANE: a
      property law over every core signature and a few mixed rows:
      `run(p.widen) == run(p.normalize) == run(p)` for handlers of the
      wider row, including rows with a `Tag` and a `Writer.byValue`
      pair. Add an okay2 twin if the intersection encoding can express
      it. DONE WHEN: the law is in the gate and a mutant `into` that
      reorders the row is caught. Source: biernacki-literature,
      2026-09-24.
      TOOL (handler-equivalence-oracle, 2026-09-24): `Bisim.check`
      (docs/equivalence.md) compares the two sides as trees over
      sampled answers, and gives a path when they differ. Write the law
      with it rather than comparing run results.
