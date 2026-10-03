- [ ] cont-fun-answer — the one shape of the old cont-stack-layer1-c list
      left open (its (6); the rest closed 2026-10-02, changelog.d
      cont-macro-inline-helpers and cont-macro-collections): a FUNCTION
      answer, PState's `s => k(s)(s2)`, okay-optics' `Zoom.scala` (4 sites)
      and okay-ui's `PWizard.scala` (3) — state passing, where every level
      still takes the strict `k` and nests. cont-stack-layer1-b built it as
      a walked `Fun` with an `Ap` node and measured 2.8x the direct road on
      statePara (89 vs 32 µs), so it was taken out — but that was the OLD
      runner's price. To do: re-measure it ON THE FRAME MACHINE
      (cont-strict-k's lead (2)); land it everywhere if it pays, or as a
      Scala.js-only expansion if not (JS has no stack switch, so there it
      lifts the engine-stack bound on deep state-passing programs —
      specs/cont-js-depth.md stage 3). Acceptance as the others: a red
      shape test, a million on 128 KB with zero switches, statePara alternated
      ref/mine through jmh-lane.sh.
