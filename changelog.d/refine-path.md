## refine-path - the fold of >>> as a combinator: Refine.path, Refine.json.at

`Refine.path(a, b, c)` is `a >>> b >>> c` for steps that keep one type,
`Refine.path()` is `id` (no name in the verdict); `Refine.json.at("a",
"b", "c")` is a descent through fields, each a step of the path, written
back as a skeleton. In okay and okay2, with law tests in both
TestRefineAlgebra suites.
