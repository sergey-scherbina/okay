- [ ] vector-search-dominates — `searchVectors` 379 us dominates §11's
      per-query table, where everything else is under 20. Not a
      defect (240 segments x 1536 dims is real work), filed because it
      is where retrieval's time actually goes.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
