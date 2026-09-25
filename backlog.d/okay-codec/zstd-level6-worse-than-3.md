- [ ] zstd-level6-worse-than-3 — ZstdEncoder at level 6 (the hash chain,
      16 candidates, lazy matching, no acceleration) compresses WORSE
      than level 3 did with the chain at depth 4: `big` 437 853 vs
      424 526 bytes, `numbers` 47 927 vs 42 639 (measured 2026-09-25,
      okay-compress-zstd-speed-2's baseline). A deeper search that
      loses bytes points at the choice, not the search: the longest
      match by length alone, with no price for the offset's code, or
      lazy matching that skips the repeat offset. Compare level 6's
      sequences with level 3's on `numbers`, and price a match by its
      bits (offset code + extra bits) before its length. (2026-09-25)
