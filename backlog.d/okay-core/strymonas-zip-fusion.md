- [ ] strymonas-zip-fusion — PRIORITY: MEDIUM. Kiselyov, Biboudis, Palladinos &
      Smaragdakis, "Stream fusion, to completeness" (POPL 2017; the
      JFP 2022 strymonas paper extends it): a staged stream library
      whose guarantee is ONE loop and no intermediate structure for
      EVERY pipeline, including the two hard cases — `zip` of streams
      that were `flatMap`ped (nested loops on both sides), and
      `flatMap` whose inner stream is itself staged. Our staged
      pipelines (specs/staged-pipelines.md, gen-chain-fusion) fuse
      linear chains and `Gen.flatMap`; `Gen` has `zipWithIndex` and
      zip exists only at the aggregator level. THE LANE: `Gen.zip` as
      a fused operator, then their case table — zip after flatMap on
      one side, on both, take/filter between — each measured against
      the hand-written loop (the `direct-staged`/`Handler.flat`
      precedent: a hand ceiling first, and B/op is the tell). What
      cannot be fused is refused by name, as strymonas does with its
      "linearity" restriction, and the spec says which shapes.
      (2026-09-23)
