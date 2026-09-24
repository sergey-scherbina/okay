- [ ] okay2-distinct — two `State[_]` of different parameters in one row
      are two TYPES to `Remove`/`Member` and one CLASS to the split, so
      `State[Int] + State[String]` compiles and misroutes at the first
      wrong answer (a ClassCastException, loud). The Scala 3 core refuses
      the row with the `Distinct` macro, which reads the erasure. The
      lane: a Scala 2 blackbox macro (`scala.reflect.macros`) walking
      the `+` tree and comparing `Op`'s erasures, required by
      `Handler.union` as the Scala 3 one is; or, cheaper, a `Remove`
      instance for `F` that FAILS when the row holds another signature
      of the same erasure — which needs the erasure at the type level
      and so needs the macro anyway. Watched to misroute first
      (TestRowIdentity in the Scala 3 core is the shape). (2026-09-24)
