- [ ] stacked-shift0 — PRIORITY: LOW (trigger). `Delim.Stacked` (freer-base-stage2) stacks
      `shift`, `control` and `abort`, not `shift0`/`control0`: their
      body runs with the delimiter CONSUMED, so its stack is the part
      of `st.S` BELOW `p` — a match type `Below[S, P]` the identity
      probe never exercised. The unstacked doors remain. THE LANE:
      `Below[S <: Tuple, P]` (drop through `P`), `shift0`'s signature
      `f: (A => Under[F, R, st.S]) => Under[F, R, Below[st.S, p.type]]`,
      the same for `control0`, and a test that a shift to the consumed
      prompt inside the body is refused while a shift to an outer one
      resolves. TRIGGER: the first stacked program that wants
      `shift0`; the unstacked door serves until then. (2026-09-23)
      LITERATURE (biernacki-literature, 2026-09-24): the type system
      for `Below` already exists. Materzok & Biernacki, "Subtyping
      delimited continuations" (ICFP 2011): shift0/reset0 typed with a
      STACK of answer types and subtyping between them, which is our
      `st.S` with `Below[S, P]`. Materzok & Biernacki, "A dynamic
      interpretation of the CPS hierarchy" (APLAS 2012) relates it to
      the hierarchy, and Biernacki, Pyzik & Sieczkowski, "Reflecting
      stacked continuations in a fine-grained direct-style reduction
      theory" (PPDP 2021) gives a direct-style reduction theory for it.
      Take the lane's signatures from ICFP 2011 and check the
      refusal/resolution test against the paper's typing rules
      instead of deriving them anew.
