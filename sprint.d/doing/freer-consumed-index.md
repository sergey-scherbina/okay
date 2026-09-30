- [ ] freer-consumed-index — McBride's reading of the indexes (the state
      BEFORE in `R`, consumed by the handler; the state AFTER in `S`) on
      the library's own base, so a type-changing state runs through
      `State.handle`'s tail-recursive loop instead of `PState`'s CPS
      (1.29x, a frame and a `Reentry` per operation, the room/switch
      bookkeeping). ProbeMcBride (freer-mcbride-probe, 2026-09-30) types
      it on the INVARIANT copy of the enum, `@tailrec` and with no
      continuation object; specs/freer-base.md "McBride's reading is
      refused by the variance ALONE" prices the move: `Freer[G, S, R,
      +A]` with `S`, `R` invariant and `G[_, _, +_]`; `Cont.tailShift`/
      `tailPure` place their `Return` at `(S, R)` by one cast justified by
      the `S <:< R` the macro already summons (or a `Return` carrying the
      evidence, +8 B a `pure`); TestFreerPara's reading-2 pin flips to
      green and moves to the library's `Freer`. IN TENSION with
      `cont-variance` (more covariance on the facade): choose one.
      TRIGGER: the first consumer that needs a typed protocol at
      `State.handle`'s cost — okay-sql's `Tx` handled by threading, a
      session channel typed by its state. Changes the base's variance:
      the gate is the full `affected master staged`.
