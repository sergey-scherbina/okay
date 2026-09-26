- [ ] okay2-writer-told-then — the Scala 3 core's Writer stream views
      lost a told value when the continuation after it threw
      (source-merge-via-ready, 2026-09-26: `Writer.uncons` applied
      `k(())` as it handed the value over, and a throw took the value
      along). okay2's `Writer.scala` has the same shape at both
      `uncons` (`Right((w, k(())))`, lines 130 and 145 on 2026-09-26).
      Port `Writer.toldThen` (apply `k` now, a throw becomes a `Delay`
      that throws at the next step) and core's `TestWriterToldBeforeThrow`,
      watched failing first; gate with `cd okay2 && ../scripts/gate.sh`.
      Do NOT make `k` lazy: that moved pull timing and broke two
      FoldUntil laws in the Scala 3 core. TRIGGER: the next okay2 lane
      in Writer, or a source in okay2 whose step throws. (2026-09-26)
