- [ ] cont-frames-relink-two-nodes — PRIORITY: LOW. A usual `k` (a
      segment and the delimiter it was cut at) is resumed by
      `Rev.relink`: a copy of the `Reset` carrying the live registers
      and a `Run` over it — two nodes per resumption.
      `delimDollarResume` reads 1.11x the single-list machine
      (404 vs 540 KB/op: fewer bytes, more time). Ask whether the
      `Run` can be folded (the `Reset`'s own segment field already holds
      a segment) and what the resume lanes give for it. history.d
      2026-10-01T053245Z.
