## cont-frames-relink-two-nodes - the usual continuation is one node: `Stack.Kept`

- A capture to the delimiter at the head of the stack (`nearest`: every
  `shift`, `emit`, a generator's step) builds ONE node, `Stack.Kept` (a
  segment and the delimiter it was cut at, nothing below), where it
  built a `Run` and a copy of the `Reset`. A resumption relinks it over
  the live registers typed (`Rev.onto`), without `relink`'s claim, which
  now serves only a `k` built by the deep walk. 12174e541.
- Against the single-list machine: delimGenerator 0.68x, layeredViaDollar
  0.82x, stateLexDeep 0.89-0.91x, stateDeep 0.95x, delimDollarResume
  1.07-1.08x — every lane better than the segmented stack's landing, and
  the deep-handler lanes now ahead of the single list too; bytes down on
  each (history.d, specs/freer-kont.md Results).
