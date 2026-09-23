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
