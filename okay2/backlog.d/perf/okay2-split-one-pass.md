- [ ] okay2-split-one-pass — the twin of delim-split-wrap-free (Scala 3
      core, 2026-09-26: delimGenerator 942 328 -> 902 328 B/op, 90.0 ->
      88.1 us; delimDollarResume -16 B and 0.98x). okay2's
      `Delim.split` still walks with a type-aligned `Wrap` of frames
      and unwinds it, and its dollar cut is two chains (`AtRet`). THE
      LANE: the same one-pass copy into a separate `Frames` type with
      once-set holes (read only by `reify`), a dollar closing the copy
      with a copy of its own frame; the machine's own `Segs` stay
      immutable (a `var` on them cost 3-4% on lanes that never
      capture). Measure okay2's delim lanes, both arm orders.
      (2026-09-26)
