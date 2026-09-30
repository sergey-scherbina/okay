## refine-algebra - Refine's glyphs, the classes it is an instance of, and its roads into effects and streams

- `>>>` (= `andThen`) and `or` (= `<|>`); `orElse` as a real fallback —
  the second pattern only when the first declines, never `Unclear` from
  it (the operator's choice over aliasing `<|>`: Scala's `orElse` is
  first-wins everywhere).
- `Refine.id` (no name in the path) and `given Refine.category`
  (okay-optics' `Optic.Category`); `Refine.empty`, the unit of `or` and
  `orElse`; `***` on pairs, `+++` on Either, `and` for records with
  `Refine.Merge` (given for `Json`).
- `orRaise` (the verdict through `Throws`), `verdicts` and `taken`
  (`Stage`s; `taken` answers `Missed(declined, unclear)`).
- Laws in `TestRefineAlgebra` (11 tests): category, both monoids,
  products, the prism law through `and`. docs/modules/okay-refine.md
  "The algebra" records what holds and what cannot (Functor, Profunctor,
  Arrow — each the way back refusing).
