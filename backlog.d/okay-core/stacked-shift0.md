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
      LITERATURE (biernacki-literature, 2026-09-24; read against the
      ICFP text by biernacki-literature-fix): Materzok & Biernacki,
      "Subtyping delimited continuations" (ICFP 2011) types shift0/reset0
      with an effect annotation that IS a stack of contexts:
      `sigma ::= eps | [tau sigma] tau sigma`, each entry the type of
      one delimited context, answer-type change included. The rule for
      shift0 is the rule this lane wants for `Below`:
      `G, f: t1 -s1-> t2 |- e : t3 s2` gives `S0 f.e : t1 [t2 s1] t3 s2`,
      so the body `e` is typed under the stack with the top context
      removed. Two further points apply here. (1) Subtyping `eps <=
      [tau sigma] tau sigma`: a pure term fits under any stack whose
      contexts compose, which `Prog.diag` already does. (2) A function
      that needs the top k contexts runs under more: the tail of the
      stack stays polymorphic, which is what keeping `S` abstract in
      `Under[F, A, S]` gives. WHAT DOES NOT TRANSFER DIRECTLY: their
      stack is POSITIONAL (shift0 takes the nearest context) and ours
      is NAMED (a multi-prompt `Has[S, P]`), so `Below[S, P]` drops
      THROUGH `P`, not one entry. The paper fixes the rule's shape,
      and the naming is ours. The paper also allows an answer-type
      change per context, while our `Prompt[R]` fixes `R`. Materzok &
      Biernacki, "A dynamic interpretation of the CPS hierarchy" (APLAS
      2012) shows shift0/reset0 express Danvy-Filinski's
      shift_i/reset_i hierarchy (the "layered effects" tool), with
      proofs in Twelf, and introduces `e1 $ e2` (push a function as a
      context), close to our `push`.
