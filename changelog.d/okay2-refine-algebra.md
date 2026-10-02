## okay2-refine-algebra - refine-algebra on the Scala 2 core

okay2-refine gains what refine-algebra gave okay-refine: `>>>`, `or`,
`orElse` (first-wins fallback), `Refine.id` with an implicit
`Optic.Category[Refine]`, `Refine.empty`, `***` / `+++` / `and` with
`Refine.Merge` (given for `Json`), `orRaise` through `Throws`, and the
`verdicts` / `taken` Stages. The same law suite (`TestRefineAlgebra`,
10 tests) holds on JVM, JS and Native; okay2-refine depends on
okay2-stream for the Stages. docs/okay2.md §31 extended.
