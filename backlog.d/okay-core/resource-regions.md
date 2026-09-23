- [ ] resource-regions — PRIORITY: LOW (trigger). `Resource` is region-style (Resource.scala says
      so) but nothing stops a handle from ESCAPING its scope: a file
      handle stored in a `var` inside `Resource.scoped` and read after
      the region released it is a run-time error at best. Kiselyov &
      Shan, "Lightweight monadic regions" (Haskell 2008): a phantom
      scope on the handle and rank-2 scoping on the region make the
      escape a compile error. This is exactly the trick `Delim.Stacked`
      landed today for prompts (a lexical given as evidence the scope
      is open), so the lane is small: `Resource.open` hands the body an
      `Open[H]` evidence that `use(h)` requires; after the region
      returns no evidence is in scope. Nested regions, and a handle
      passed to an inner region, are the cases to pin (their `liftRegion`).
      TRIGGER: the first consumer that holds a handle in a val past the
      region, or a review that finds one — okay-jdbc's `Db` and
      okay-persist's stores are the places to look. (2026-09-23)
