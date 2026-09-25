- [ ] okay2-split-at-rest — the handler loops okay2-handler-allocs did
      not reach still split through `Split.split` (two closures, a
      `Tuple2` and an `Either` per handled operation): the `Delim`
      machine, `Stream`, `Generate`, `Optic`, `SharedOnce`. Move each
      onto `Split.at`, measured one A/B lane per loop against master on
      the okay2-bench protocol. Found by the okay-vs-okay2 audit.
      (2026-09-25)
