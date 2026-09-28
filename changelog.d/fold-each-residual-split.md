## fold-each-residual-split - foldEach's 1.8x over the hand loop named and halved: the Vector read per step, not the generic calls

fold-each left `stateFoldEach` at 18.1 µs against the hand-written
one-bind loop's 10.0 and called the residual "the boxed accumulator and
the generic calls". A ladder top-down from the library to the hand loop
(BuildShapeBenchmark, N = 1000, one `jmh-lane.sh` run per lane) named
it as two things of ~3.3 µs each — and the calls were not one of them:

| rung | lane | µs/op | B/op | removes |
|---|---|---:|---:|---|
| D0 | stateFoldEach | 17.35 | 143 712 | — |
| D1 | stateFoldEachInt | 14.04 | 127 864 | the erased accumulator: −3.3 µs, −16 B a step |
| D2 | stateOneVecBind | 13.31 / 13.35 | 111 816 | the two generic calls: −0.7 µs, inside a ±0.9 error |
| D3 | stateOneBind | 10.02 | 111 816 | `Vector.apply` per step: −3.3 µs, 0 B |

- `foldM`/`foldEach` (and `each` through `foldM`) now index a flat
  `ArraySeq.untagged.from(xs)` instead of `xs.toIndexedSeq`: a bounds
  check and a load per step instead of a Vector's radix walk. One copy
  per program (an `ArraySeq` input passes through). **stateFoldEach
  17.35 → 14.85 µs (1.17x), stateFoldM 18.22 → 16.76 (1.09x)**, each
  +4 112 B/op for 1000 elements. The rung that priced the road first
  (`stateFoldEachArr`, foldEach verbatim with the one line changed) read
  14.40.
- What is left, and whose it is (specs/map-fusion.md, "What is left in
  the fold path"): the erased accumulator is the language's — filed as
  `fold-erased-accumulator` with its measured ceiling (an inline
  `foldEach` specialising `go` at the call site: 1.24x); the map node a
  `.map`-written `foldM` step builds and discards is the syntax's,
  `Free.map` carries no defect, and the `Map` node of `Free` stays
  declined (`free-map-node`); the generic calls are answered
  (`fold-generic-calls`). 1.8x is now 1.48x (14.85 / 10.02).
- Records: history.d `2026-09-28T012003Z-fold-each-residual-split.tsv`
  (9 rows); spec section and Decisions entry of 2026-09-28. Not
  statePara: it has no hand loop to ladder against, and its ~1 µs is
  `cont-stack-statepara-time-residual`.
