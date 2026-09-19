- [ ] **writer-typeablek-by-class** — Writer's default `TypeableK` tests
      the class of `Say` alone (total: Say is Writer's only constructor,
      class-distinct from every other signature's operations), so no
      `Typeable[W]` is summoned and the E092 "cannot be checked at
      runtime" warning stops firing at every `Writer.fold/collect/run/
      uncons` call site with a parameterised W — 23 `@nowarn`s across
      ten modules, each repeating the same caveat. The finer test (the
      told value's class too, `TypeableK.ByValue`, what routes
      `Writer % String + Writer % Int` in ONE row) becomes the opt-in
      `Writer.byValue.writerK`; `Distinct` refuses a two-Writer row
      without the import, which Distinct.scala names as the safe
      direction. No module held two Writers in one row (grep, 2026-09-19).
      Proposed in the producer-to-writer-carrier wrap-up; the operator
      said "запиши и сделай". DONE-WHEN: `grep nowarn("msg=cannot be
      checked at runtime")` finds only the `Ask[Nothing]`/`Claim +
      Produce` sites (TestHandleForward, HandlerBenchmark — a different
      cause); TestRowIdentity/TestDistinct green with the import;
      docs/many-instances.md says which is which; gate GREEN, 0 warnings.
