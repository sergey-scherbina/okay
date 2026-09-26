## delim-split-wrap-free - a capture copies its prefix in one pass, and costs less than before the stack-safety loop

- `Delim.split` is one `@tailrec` loop that copies the captured prefix
  front to back while it walks. The copies are their own type,
  `Frames`, whose `rest` is set exactly once while the copy is still
  unpublished, and only `reify` reads them. The `Wrap` stack of frame
  closures is gone. The machine's own frames are unchanged.
- A dollar closes the copy with a copy of its own frame. `reify` is a
  left fold, so the old pair of chains linked by an existential type
  member (`AtRet`) became one chain (`AtDollar`), with no cast.
- Measured against master, both arm orders, two forks:
  delimGenerator 942 328 -> 902 328 B/op, 8 B per capture under the
  recursive original, and 90.0 -> 88.1 us. delimDollarResume
  740 016 -> 724 016 B/op and 50.0 -> 48.9 us. delimDollarOnly is at
  parity.
- Three variants were refuted on the way and recorded in history.d:
  a find-then-copy walk, a two-pass dollar path, and a mutable `rest`
  on the machine's own frames, which made lanes that never capture
  3-4% slower.
- The okay2 twin still uses the `Wrap` stack; filed as okay2 perf
  `okay2-split-one-pass`.
