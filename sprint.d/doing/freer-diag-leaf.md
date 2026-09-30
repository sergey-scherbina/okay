- [ ] freer-diag-leaf — the operator's "Да" (2026-09-30) to the first
      item of the from-scratch list in freer-consumed-index's answer: a
      DIAGONAL LEAF as a case of the node. `Freer.Diag[G, R, A](a: G[R,
      R, A]) extends Freer[G, R, R, A]` says on the node what `Lift`'s
      phantom index cannot and a handler's loop over a mixed row needs —
      the operation moves nothing, so the continuation of a matched
      `Bind(Diag(e), k)` is at `R`, not at an existential `T`. A unary
      effect then enters an indexed row BARE (`[S, R, X] =>> PSt[S, R, X]
      | State[Int, X]`) through `Freer.diag`, with no `At` wrapper and no
      extractor cast; TestFreerPara's row probe is rewritten so. `Free`'s
      doors at `Unit` keep building `Inject` (the 112 match sites do not
      move); Cont never builds `Diag` and its `step` says so with
      `@unchecked` rather than a dead arm in the hottest loop. Changes the
      base: the gate is the full `affected master staged`; not measured
      (the runner is gating beside), a lane to re-read if any core number
      moves.
