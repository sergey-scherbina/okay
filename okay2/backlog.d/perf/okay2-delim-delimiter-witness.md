- [ ] okay2-delim-delimiter-witness — the twin of delim-machine-allocs
      (2), the Scala 3 core's prize (2026-09-27: delimPushOnly 342 ->
      278 B/op and 0.79x, delimGenerator 854 -> 726 B/op and 0.83x).
      okay2's `Delim` machine still puts an identity
      `Segs.K((a: r) => pure(a), kont)` under every `Mark`/`Ret`
      (okay2/src/main/scala/okay2/Delim.scala:793, :842) only to
      retype the prompt's answer to the operation's. THE LANE: carry
      that on the delimiter frame as a witness (`r <:< X`,
      `<:<.refl`, `liftCo` over the covariant program type; the split
      threads it through its cut) and drop the frame — one fewer per
      delimiter, per capture copy and per return. Scala 2.13 has the
      same `<:<` API; check GADT knowledge of `r <: X` there first
      (the core's compiler had it at the match). Measure okay2's delim
      lanes, both arm orders. The core's (3) narrow form (a foreign
      resume continuing at the head bind's `f(x)`, -40 B per foreign
      op) is the smaller second half. (2026-09-27)
