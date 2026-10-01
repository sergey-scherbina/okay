- [ ] cont-frames-register-pressure — PRIORITY: LOW, a diagnosis kept
      so nobody re-derives it. Bare delimiter install/pop
      (`delimPushOnly`, `delimDollarOnly`, KontBenchmark
      `kontResetOnly`) reads ~1.36x the single-list machine on the
      segmented stack, and it is NOT steps, inlining, GC or a hot spot
      (all identical, specs/freer-kont.md Results): it is C2's register
      allocation. Three loop registers (`focus`, `fs`, `st`) live across
      every allocation's slow-path call, and C2 keeps all three in
      stack slots on the fast path — 74 loads from `sp` in `loop$1`
      against the single list's 34 (hsdis; the dylib is built from
      OpenJDK's src/utils/hsdis over brew's capstone and sits in the
      JDK 26 lib/server). REFUTED already: moving the cold arms out
      (74 -> 60 loads, no time). Open: a layout that carries two
      registers without giving up shared segments.
