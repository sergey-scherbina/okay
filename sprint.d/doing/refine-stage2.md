- [ ] refine-stage2 — okay-refine's DOCUMENT level and open registry
      (specs/refine.md §3 stage 2), after stage 1 landed the vocabulary
      and the format level. THE ASK: (1) `Refine.schema[A](using
      Schema[A]): Refine[Json, A]` — a derived Schema is a pattern,
      declining with the codec's own decode message; (2) a registry:
      `Refine[A, B]` values registered by name under a level, `Or`-ed in
      registration order, so a new format or product is one registration
      and no edit; (3) the `Judge` seam — an ORDERING of alternatives
      (ours: registration order; DLM-shaped: a judge's ranking, specs/dlm.md
      `Judge`) that never adds or removes a taker, only says what `cut`
      tries first; (4) `Logic` integration: a `Refine` as a program under
      `Choose` so a caller writes `for case Took(swap, _, _) <- …` and
      `ifte`; (5) the public FpML prover: two ISDA public examples (a
      plain-vanilla IRS, an FX forward) read `xml → fpml → swap|forward`
      end to end — which NEEDS [[xml-processing-instruction]] first (every
      FpML file begins with the declaration). The full domain (all FpML
      products, CDM) is the private repository's, not this one's. Spec:
      specs/refine.md. (2026-09-29, from the okay-refine landing)
