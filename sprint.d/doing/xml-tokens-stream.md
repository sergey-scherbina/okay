- [ ] xml-tokens-stream — found measuring okay-watch's start (its
      perf-start-peak, 2026-09-25): 80% of what is allocated while three
      sanctions lists (55 MB of XML) are read is `Xml.scan.stepInto` — per
      character an `S.copy`, a `P`, and `buf + c`, which copies the whole
      lexeme per character; and in Text mode `buf.forall(_.isWhitespace)`
      rescans the buffer per character (quadratic in a text run).
      `Xml.tokens(reader)(emit)`: the same tokens from a Reader on a
      mutable buffer, for a document read once (property-tested equal to
      `scan`, chunk boundaries included); `scan`'s Text rescan becomes the
      first character's class (a text run is homogeneous by construction).
      Measured before and after on the real lists (history.d).
