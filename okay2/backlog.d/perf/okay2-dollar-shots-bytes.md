- [ ] okay2-dollar-shots-bytes — the twin of delim-dollar-shots-bytes
      (Scala 3 core, 2026-09-26: delimDollarOnly 388 008 -> 372 008 B/op).
      okay2/src/main/scala/okay2/Delim.scala still gives the plain
      `Dollar` and `Segs.Ret` a `shots: Shots` field that is null unless
      the dollar came from `dollarResumed`. THE LANE: a watched dollar as
      its own op and frame (`Watched`, `Segs.Watch`), matched last, as
      the Scala 3 core now does; measure okay2's delimDollarOnly lane
      (if it has none, add it beside DelimBenchmark's). (2026-09-26)
