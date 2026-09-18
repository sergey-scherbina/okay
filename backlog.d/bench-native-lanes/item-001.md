- [x] **The five in-process lanes: DONE** (2026-09-10). The plain JVM,
      `java.util.stream`, fs2, zio-streams and kyo all fold
      `Native.Fold` — panes as keys in a map, nothing evicted, a
      mutable cell, a sort for the top-5 — with no okay type in the
      lane, each carried by the library's own combinators and each at
      1/2/4/8 cores. §20 has the numbers and the reversal they show.
