- [ ] handler-equivalence-oracle — PRIORITY: MEDIUM (a test tool; no
      trigger needed). The fusion and staging lanes (handler-fusion,
      gen-chain-fusion, optics-fuse, Direct.staged, the stagers) prove
      a rewrite correct by EXAMPLES: this input, this answer. The
      semantics literature has a stronger check that is also cheap to
      run. Normal-form bisimilarity (Biernacki, Lenglet & Polesiuk: for
      algebraic effects and handlers, FSCD 2020; for state, FoSSaCS
      2019; for delimited control, LMCS 2019) says two programs are
      equivalent when they reach the same operation with equivalent
      continuations. A free tree already stops at every operation, so
      the check is executable. THE LANE: a symbolic handler that
      records each operation and answers with a fresh symbol, plus
      `equivalentUpTo(depth)(p, q)`, which walks both trees in
      lockstep and compares their operation traces. Then a fused or
      staged program is checked against its unfused source,
      structurally, over generated inputs. DONE WHEN: the oracle runs
      under at least one landed fusion (gen-chain-fusion is the
      smallest), and a deliberately wrong rewrite (a mutant) is
      caught, watched failing first. Source: biernacki-literature,
      2026-09-24.
