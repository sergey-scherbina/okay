- [ ] okay2-distinct — two `State[_]` of different parameters in one row
      are two TYPES to the row and one CLASS to the split, so
      `State[Int] + State[String]` compiles and misroutes at the first
      wrong answer (a ClassCastException, loud). The Scala 3 core refuses
      the row with the `Distinct` macro, which reads the erasure. The
      lane: a Scala 2 blackbox macro walking the row's intersection
      parents and comparing their classes, required by `Handler.union`
      and the handlers as the Scala 3 one is. Since stage 8 (2026-09-24)
      the shape to copy is already in the core: `ReplayableMacro`
      flattens a row's parents the same way, and scala-reflect is
      already a dependency. Watched to misroute first (TestRowIdentity in
      the Scala 3 core is the shape). (2026-09-24)
