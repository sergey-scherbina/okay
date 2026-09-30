- [ ] freer-mcbride-probe — the operator's question after freer-paramonad
      (2026-09-30): WHY McBride's reading (the index a state the handler
      CONSUMES) is refused on `Freer[G, S, +R, +A]`, what is lost without
      it, what it would give. Answered by compiling: the same `St`
      signature and threading loop against ProbeFreerStep's INVARIANT
      enum — does it type with no continuation object at all, and is the
      loop `@tailrec` with the type arguments changing per call (Scala 2
      refused that). Findings to specs/freer-base.md. Gate: additive —
      `okayJVM/testOnly okay.TestFreerPara` + `affected master
      Test/compile`.
