## staging-scope-extrusion - answered: Scala 3's staging refuses extruded code by name

The backlog asked for the hygiene failure an effectful generator can
produce, scope extrusion (Kameyama, Kiselyov and Shan), to be tried
first and then refused, either by a check at splice time or by the
paper's typed let-insertion.

- `TestScopeExtrusion` (okay-staging, 3) tries it: a generator keeps
  `'x` in a cell inside `(x: Int) => …` and splices it after the
  lambda. Two cells were tried, a host `var` and an okay `State` whose
  answer carries the code out. Both are refused BY NAME before
  anything compiles: "a reference to parameter x was used outside the
  scope where it was defined". The third test is the control: the
  same cell, spliced inside its binder, generates and runs (4 -> 40).
- So the cheaper road already exists, and it is the compiler's. The
  entry's premise was also inaccurate. okay-staging stages SCHEMAS
  with a pure generator whose caches hold compiled codecs, never
  `Expr`s, and it falls back to the interpreter (recording
  `lastFailure`) on any failure. The inline-stager road
  (`Direct.staged`) has no code value a cell could hold.
- docs/modules/okay-staging.md gains "Hygiene: generated code cannot
  outlive its binder", with the two papers. The entry moves to
  refuted-declined-or-answered.
