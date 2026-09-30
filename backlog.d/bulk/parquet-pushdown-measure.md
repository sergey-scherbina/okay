- [ ] parquet-pushdown-measure — the measurement that decides whether
      streams-seam lane 5 (the structural sub-language + `Bulk[DataFrame]`)
      is built. On CSV the gap is 1.32x (MeasureGtfsFrames, 2026-09-30:
      our seam on Spark 708 ms vs a hand DataFrame 536 ms over the GTFS
      three joins), which does not earn a second plan language. Where a
      closure is structurally worse is PARQUET with a selective
      predicate: Catalyst pushes `where(col op literal)` into the reader
      and skips row groups by their statistics, and prunes columns, while
      `select(f)`/`where(p)` read every row group. Measure that first: the
      GTFS stop_times written as Parquet sorted by trip_id (a few dozen
      row groups), a predicate selecting ~1% of trips, three arms — our
      seam (`read(path, ParquetFile.rows[A])` + `where(p)`), a hand
      DataFrame with the same filter, and our reader with row-group
      skipping done by hand from the footer's min/max (what a structural
      `where` would buy us on EVERY backend, local included). If the
      second and third beat the first by a wide margin, build lane 5 with
      the pushdown as its first rewrite; if not, close lane 5 as refuted
      in specs/streams-seam.md. (2026-09-30, filed by tables-structural)
