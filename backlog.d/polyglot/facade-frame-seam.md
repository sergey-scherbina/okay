- [ ] facade-frame-seam — the R half. Python's is done (facade-frame-seam,
      2026-09-25): `Frames.rows` takes the language's own road (rows to a
      frame in one pass) and `ForeignWorker.frameTable` sends a Table as
      itself where Arrow is spoken; the first "37%" was a comparison
      against a pre-built frame no caller has, and rows through the facade
      now read within 3% of the own road (specs/foreign-facade.md,
      Results). LEFT: the same two things for R — `Frames.r.rows` over
      `RFrame.of`/`.rows`, and an `RSubprocess.frameTable` twin — and the
      Arrow cells of the table, which need a pyarrow/arrow interpreter this
      box lacks (`OKAY_PYARROW_PYTHON`, as MeasurePyArrow uses). (2026-09-25)
