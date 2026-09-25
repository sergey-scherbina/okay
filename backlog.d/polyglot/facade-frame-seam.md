- [ ] facade-frame-seam — tier 2 through the facade is 37% slower than the
      language's own road (MeasureFacade, 2026-09-25, load 4.9: 100 000
      rows, python3, columnar JSON — 188 ms against 137 ms). The 51 ms is
      the seam: `Rows.table` (rows → Table) then `ArrowFrames.frame`
      (Table → PyFrame) in, `ArrowFrames.table` then `Rows.rows` out —
      two conversions each way where the own road does one. Two fixes,
      by the road: where Arrow is spoken, the Table should go to the wire
      AS ITSELF (`OkayArrow.write` of the Table, no PyFrame in between —
      `ForeignWorker.sendArrow` already takes a Table); where it is not,
      `Road.rows` should build the columnar JSON frame from rows in one
      pass (rows → PyFrame), and `Frames.frame` keep the Table road for a
      caller that has a Table. Measure both against 137 ms; the spec's
      rule is that a cell worse than the own road is a defect. (2026-09-25)
