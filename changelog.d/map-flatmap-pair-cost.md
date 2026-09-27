## map-flatmap-pair-cost - the map + flatMap step measured (2.27x), map fusion in Free refuted

- Each step written as `op.map(f)` then a flatMap costs 28.6 µs / 306 KB
  per 1000 operations under two handlers. The same step as ONE flatMap
  costs 12.6 µs / 138 KB (BuildShapeBenchmark rowFoldM / the new
  rowOneBind). That gap is what FusionBenchmark's nestedSW/nestedSWr
  showed.
- Fusing `map` into the following bind in `Free` was tried two ways and
  NOT landed. The direct form (`y => g(f(y))`) gave 1.17-1.23x but
  overflowed the stack on Delim's composed continuations
  (TestStackSafetyCore). The stack-safe form (`Bind(Return(f(y)), g)`)
  made nestedSW 21% slower. specs/map-fusion.md has both A/B runs, and so
  does history.d.
- For a hot loop the lever is in the user's hands: write the step as one
  flatMap.
