- free-map-node — PRICED AND DECLINED 2026-09-27, restated 2026-09-28
  (specs/map-fusion.md, "What is left in the fold path"). A `Map(a, f)`
  case of `Free` so that `op.map(f)` builds ONE object instead of
  `Bind` + `Mapped`: −16 of the 48 B a `.map`-written `foldM` step builds
  and discards (map-cost-residual, rung 1: 6.3 µs per 1000 steps). It is
  an enum change against `resume`'s 325-byte inline budget
  (`TestInlineBudget`) for a third of the node's bytes; `Free.map` is
  not at fault — a map must build something for `foldM` to read `f`
  out of later. THE ANSWER THAT EXISTS: `!.foldEach`, whose step is the
  element's program plus a pure combine and builds no map at all. Retake
  only if `resume`'s budget is re-cut for another reason.
