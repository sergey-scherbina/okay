## fold-erased-accumulator — the inline fold road measured: 1.24x on foldEach, the map-node scalar replacement refuted

- Two rungs in BuildShapeBenchmark (`stateFoldEachInline`,
  `stateFoldMInline`): the library's `foldEach`/`foldM` verbatim under
  `inline def` with `inline` step functions, so the local `go` is
  expanded at the call site with `B = Int` and the step lambda
  beta-reduced into it. Each lane its own `jmh-lane.sh` run on a quiet
  box (history.d `…-fold-erased-accumulator.tsv`).
- foldEach: 15.16 → 12.27 µs (1.24x, −24 B a step) — the erased
  accumulator's ceiling (D1) reached on the library's own road; 1.27x
  still over the hand loop.
- foldM: 16.81 → 15.15 µs (1.11x) at the SAME −24 B a step: the `Bind`
  + `Mapped` a `.map` step builds are not scalar-replaced even inside
  one compiled `go` — the escape-analysis hypothesis is refuted by
  bytes. The map node's answer stays `foldEach`.
- Not landed in `Effects` (a copy of `go` per call site, inline budget
  on every caller, for 1.24x on a primitive fold); re-filed under
  refuted-declined-or-answered with the numbers and the original
  trigger. Spec, changelog, boards.
