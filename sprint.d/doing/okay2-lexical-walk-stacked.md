- [ ] okay2-lexical-walk-stacked — what okay2-lexical (2026-09-25,
      specs/okay2.md stage 46) did not port of the Scala 3 core's
      `Lexical`: (1) `walk`, the optional strategy over `Instances`
      (okay2 has `Instances` and `Distinct`; the walk is a `Split.at`
      loop answering its own handle's operations and forwarding the
      rest, with `Instances.exhausted` at the top); (2) `Lexical.Stacked`,
      instances as subclasses of `Delim.Stacked.In` whose `perform` asks
      for `Has.Below` (okay2 has both since okay2-dollar), so an instance
      used outside its installation is a compile error. Both need the
      Scala 2 twin of TestLexicalWalk/TestLexicalStacked; (3) `Layered.Stacked`
      (okay2-layered, stage 47, left it out for the same reason: a layer as
      the stacked dollar's `In`, `reflect` asking `Has`; TestLayeredStacked's
      three tests). DONE WHEN:
      both strategies exist by name with their suites green in the okay2
      gate, and stage 46's "not ported" line is amended.
