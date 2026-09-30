- delim-prompt-as-nested-handler-loops — REFUTED 2026-09-30 before any
  code, by reading the tests: replacing the Delim machine with one
  handler LOOP per prompt (`reset` as a handler application whose loop
  forwards other prompts' captures, wrapping the continuation) is
  expressive enough (Forster, Kammar, Lindley & Pretnar, ICFP 2017;
  `runNested` already forwards this way) but NOT stack-safe: a loop must
  call a nested handler's loop to step it, so n dynamically nested
  prompts are n JVM frames, and `TestDollar` pins 100 000 nested
  `dollar`s in constant stack (the operator's no-unbounded-recursion
  rule). The machine's `Segs` is that handler stack held as data. The
  stack-safe form of the same design is freer-kont-frames-probe (every
  handler a mark in one loop's continuation); the half that keeps the
  machine is delim-forwarding-default.
