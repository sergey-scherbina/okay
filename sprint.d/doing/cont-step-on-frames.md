- [ ] cont-step-on-frames — the last stage of freer-kont-migrate
      (specs/freer-kont.md stage 2, last box), a decision by
      measurement: `Cont.step`, the strict runner (`Cont = Freer[Shift]`,
      `k` synchronous, `StackSwitch` rooms for a deep `k`), is the one
      continuation machine left beside `Frames.run`. `Shift[S, R, X] =
      (X => S) => R` is `Shift0` with a strict `k` (the delimiter's `Y`
      is Cont's `S`; below the run nothing is lazy), so `Cont` could run
      on `Frames.run` as `Delim.Stacked.contShift` already does — one
      loop, no rooms — IF the Cont lanes (Cont's own benchmarks, the
      `direct` staged lanes that lower to Cont) hold. Cont keeps its
      `(S, R)` reading either way: with a strict `k` the escape type is
      real (specs/freer-kont.md, Results). Also to re-read against the
      machine: docs/continuations-in-practice.md "The second rule: one
      machine" — the reason is stack depth, not expressiveness
      (Delim.scala's header).
