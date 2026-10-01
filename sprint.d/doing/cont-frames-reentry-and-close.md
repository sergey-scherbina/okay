- [ ] cont-frames-reentry-and-close — two changes to the segmented
      frame machine (Cont.scala), measured one by one:
      (1) RE-ENTRY WITHOUT `Bind(Return(a), k)`: `Frames.Resume.apply`
      (a foreign handler's `k(x)`, forced) builds two nodes the loop's
      first step takes apart; an entry `Frames.runFrom(a, k)` goes to the
      registers through `pushed`. Target writerTellUnderDelim (1.155x the
      single list), every outer effect under a delimiter.
      (2) THE DEEP CUT CLOSES `k` AS `Kept`: `cut` links its reversed
      prefix onto `Done`, leaving an emptied `Reset` at the bottom — which
      IS `Kept(End, …)`; closing the bottom as `Kept` always deletes
      `relink`, `emptied` and their two casts, `Rev.onto` keeping one typed
      case. Then (3) if time: `Delim.run` starting with the boundary as
      the initial stack instead of a `Reset0` operation.
      Gate: the machine's 24 suites, then affected; A/B DelimBenchmark
      writerTellUnderDelim, delimDollarResume, layeredViaDollar,
      stateLexDeep against the single-list machine (../okay-wt-cont-ref).
