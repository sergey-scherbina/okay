- [~] merge-chunk-size-curve-inverted — CAUSE ISOLATED, A QUARTER OF
      IT FIXED (2026-09-10). okay's chunked merge gets slower as the
      chunk grows where every competitor's gets faster. Measured, one
      stage at a time, quiet box:
        chunk building alone   154 / 197 / 167 us  — FLAT (and its
          allocation flat to +-0.1 B/op, so the first suspect,
          `Stage.chunked`'s per-chunk buffer, is refuted)
        merge alone            245 / 237 / 280 us  — nearly flat
        merge + unchunk        232 / 262 / 430 us  — the rise
      So the k-dependence lives in `unchunked`. It was
      `through(s)(Stage.unchunk)`: a Take/Writer COROUTINE PAIRING
      with every element crossing the handshake. `Writer.expand`
      replaces it — one walk, elements re-told into a plain Free
      chain — and buys 7.3% of the lane's allocation (5 014 435 ->
      4 643 659 B/op at k=16, 4 827 290 -> 4 474 639 at k=1024, both
      exact) and 8% of its time at k=1024 (430.3 +-18 -> 396.4 +-7).
      The premium `unchunked` charges over a bare merge fell from
      ~150 us to ~111 at k=1024 — a quarter of it.
      WHAT REMAINS: the other three quarters, and the curve still
      rises. Refuted along the way, so nobody re-takes them: the
      chunk buffer (flat), the channel's element BUDGET (a lane with
      capacity scaled to hold 1 024 elements at every k rises
      identically: 216 / 297 / 424), and allocation growth (flat at
      every k, before and after).
