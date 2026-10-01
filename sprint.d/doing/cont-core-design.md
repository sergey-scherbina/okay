- [ ] cont-core-design — PRIORITY: HIGH, operator ask 2026-10-01 ("очистить
      дизайн продолжений от всего лишнего"): the continuation core reduced
      to what the theory needs — `$` and `shift0` (`bare` for control0)
      over `Frames(End|Frame)` / `Stack(Done|Run|Reset)`, one cut, one
      entry — and everything else (shift/reset/control/abort, Cont's
      strict `k`, Delim's libraries) derived ON TOP of it, no flags in
      the machine. Correctness first, speed measured later. Cont.shift
      keeps `(A => S) => R` (operator): `k` is an object whose `apply`
      trampolines over its own stack; nested opaque bodies bounded by a
      level counter and `StackSwitch.fresh`. `dollarResumed` leaves the
      core (its one user, Lexical's tail guard, rewritten without it).
      Spec: specs/cont-core.md.
