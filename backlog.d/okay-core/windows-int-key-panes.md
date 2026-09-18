- [ ] windows-int-key-panes — the OTHER third of the same gap
      (docs/benchmarks.md §20, "Why one core loses"): `okay.Windows`
      keys panes by `HashMap[K, LongMap[Acc]]`, which boxes an `Int`
      key and hashes twice where the packed benchmark form does one
      `LongMap` lookup — 8.5-11% measured. A `PaneKey[K]` seam with a
      packed store for `Int` keys and today's store for everything
      else would take it, IF the window index and the key both fit in
      a `Long` (they do for any `Int` key and a slide over ~2 ms, and
      the fallback must be chosen at construction rather than
      mid-run). Measure before believing: the same 2x2 is in
      `OkayLane` and prices it on every run.
