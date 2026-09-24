- [ ] monadic-reflection-stacked — PRIORITY: LOW (design; trigger).
      specs/monadic-reflection.md is Filinski's construction for ONE
      monad ("Representing monads", POPL 1994): `reflect` over `shift`,
      `reify` over `/`. Its extension to SEVERAL monads layered on each
      other is Filinski, "Representing layered monads" (POPL 1999):
      each layer has its own reflect/reify, and a layer can reflect a
      computation of the layer below. Danvy & Filinski's CPS hierarchy
      (shift_i/reset_i) was built for the same layering, and Materzok &
      Biernacki (APLAS 2012) show that shift0/reset0 express that
      hierarchy. So the road here is: layer i is a prompt on the
      `Delim.Stacked` stack, and `reflect` into it is a `shift0` to that
      prompt. The result is a monad STACK in direct style with no
      transformers. Depends on `stacked-shift0`. CORRECTED 2026-09-24
      (biernacki-literature-fix): this entry first cited Biernacki,
      Pyzik & Sieczkowski, "A reflection on continuation-composing
      style" (FSCD 2020) as its source, from the title alone. Its
      reflection is a Galois reflection between the lambda calculus with
      shift/reset and its CPS image (a direct-style transform inverse to
      CPS), not monadic reflection. It does not apply here. TRIGGER: a
      consumer that wants two foreign monads in one direct block.
      Source: biernacki-literature, 2026-09-24.
      PROMOTED 2026-09-24 (operator): specs/layered-reflection.md is
      the plan. Its stage 0 runs on TODAY's unstacked `Delim` (native
      multi-prompt is enough, per Brachthäuser, Boruch-Gruszecki &
      Odersky 2020), so it does NOT wait for stacked-shift0. Stages 1-2
      do. The trigger no longer applies.
      STAGE 0 LANDED 2026-09-24 (monadic-reflection-stacked):
      `okay.Layered` on today's Delim, TestLayered 7. Back in the queue
      for stages 1-2, which wait for delim-dollar (reify as `dollar`,
      priced) and stacked-shift0 (the capability's escape refused at
      compile time).
