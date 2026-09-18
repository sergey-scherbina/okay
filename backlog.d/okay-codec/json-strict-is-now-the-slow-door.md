- [ ] json-strict-is-now-the-slow-door — `Json.readStrict` reads 1104
      ns against `Json.read`'s 1004. The strict door was built to
      avoid the lossless road's cost, and 131cedc2 + b4172242 removed
      that cost. Either make the strict walk cheaper than the CST road
      it was meant to replace, or leave it and keep it for its
      REFUSAL — docs/benchmarks.md §10 already says the latter.
      DISQUALIFYING: if the strict walk's extra 100 ns is the field
      map and `make` (the breakdown says it is ~3.3x the bare parse),
      there is no cheap win and this closes as wontfix.
      (was filed under "bench-known-prices" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
