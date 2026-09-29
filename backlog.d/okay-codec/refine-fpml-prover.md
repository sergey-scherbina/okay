- [ ] refine-fpml-prover — okay-refine's PUBLIC prover on real material
      (specs/refine.md §4 "The domain is not here"): two documents from
      ISDA's public FpML examples — a plain-vanilla interest-rate swap
      and an FX forward (FpML 5.x confirmation view) — read end to end,
      `xml → fpml → swap | forward`, with the verdict naming the path
      and every refusal, and written back. NEEDS an XML value bridge
      first: `Format.value` declines XML today ("no value projection for
      xml"), so this lane writes `Refine[Doc.Xml, …]` steps over
      `Xml.elements`/the CST directly, or an XML→`Json`-shaped projection
      (attributes and text as fields — decide in the spec, record the
      alternative). The two FpML patterns are the only domain code okay
      carries; every further product, version and CDM is the private
      repository's. This is also the first `cut`-ing consumer and so the
      trigger for the deferred `Judge` seam (specs/refine.md stage 2) —
      only if ambiguity is actually met on the two documents. Fixtures
      under okay-refine/src/test/resources/fpml/, licence noted beside
      them. (2026-09-29, from refine-stage2)
