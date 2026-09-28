- [ ] fold-each-residual-split — after fold-each (2026-09-27):
      `stateFoldEach` 18.1 µs against the hand-written `stateOneBind`
      10.0, and specs/map-fusion.md names the residual only as "the
      boxed accumulator and the generic calls", unmeasured. THE LADDER,
      top-down, each rung removing ONE thing the library's `foldEach`
      does that `stateOne` does not, in BuildShapeBenchmark, each lane
      its own `jmh-lane.sh` run (`-f 2 -wi 3 -w 1 -i 5 -r 1 -prof gc`),
      the ends re-read in the same series:
      D0 stateFoldEach (library: erased B, f/combine Function values,
      `v(i)`); D1 stateFoldEachInt — foldEach's `go` verbatim with B
      and A fixed to Int, X still generic (Δ = the boxed accumulator);
      D2 stateOneVec — the hand loop over `items(i)` with the step and
      the combine written inline (Δ = the two generic calls); D3
      stateOneBind (Δ = the Vector read). Result: three numbers in
      specs/map-fusion.md, history.d, changelog.d; no library change
      unless a rung says one is worth it. NOT statePara: it has no hand
      loop to ladder against, and its ~1 µs is
      cont-stack-statepara-time-residual.
