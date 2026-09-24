- staging-scope-extrusion — DONE 2026-09-24, ANSWERED BY THE COMPILER:
  the test the entry asked for was written first and read. Scala 3's
  staging refuses an extruded `Expr` BY NAME at construction ("a
  reference to parameter x was used outside the scope where it was
  defined"), for a host `var` cell and for an okay `State` cell leaked
  through its answer alike — so the "refusal at splice time" road
  exists and is not ours to build, and the typed let-insertion
  discipline has nothing left to prevent. okay's generator
  (`RuntimeStaged`) keeps no `Expr` in a cell and falls back to the
  interpreter on any failure; inline stagers have no code value to
  leak. `TestScopeExtrusion` pins the refusal and a control; the
  entry's premise that okay-staging runs effectful user generators
  was inaccurate — it stages schemas. Reopen if a public API ever
  hands users an `Expr`-producing effect.
