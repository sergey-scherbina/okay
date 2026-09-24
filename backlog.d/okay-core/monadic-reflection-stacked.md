- [ ] monadic-reflection-stacked — PRIORITY: LOW (design; trigger).
      specs/monadic-reflection.md is Filinski's construction for ONE
      monad: `reflect` over `shift`, `reify` over `/`. Biernacki, Pyzik
      & Sieczkowski, "A reflection on continuation-composing style"
      (FSCD 2020) does reflection over `shift0`, whose stack of
      delimiters is what lets it reflect into the n-th of several
      monads: a monad STACK in direct style with no transformers.
      Here that becomes `reflect` onto a named layer of
      `Delim.Stacked`. Depends on `stacked-shift0`. Read the paper
      first to confirm its construction is the one described here;
      this summary comes from the abstract, not the body. TRIGGER: a
      consumer that wants two foreign monads in one direct block.
      Source: biernacki-literature, 2026-09-24.
