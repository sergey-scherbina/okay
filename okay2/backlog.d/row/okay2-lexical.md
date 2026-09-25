- [ ] okay2-lexical — the Scala 3 core's `Lexical` (specs/
      lexical-instances.md: handler instances as installations the body
      names, `Lexical.deep` as a `dollar` with the instance's return,
      `Lexical.tail`'s re-entry guard through `dollarResumed`) has no
      okay2 twin. okay2 has `Delim.dollar` since okay2-dollar
      (2026-09-25) but NOT `dollarResumed` (the `Shots` count), which
      the tail guard needs: port that first, then Lexical and a
      TestLexical twin. Filed by okay2-dollar, not built. (2026-09-25)
