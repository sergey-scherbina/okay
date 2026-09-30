## freer-kont-frames-probe - the machine's continuation stack as a Freer: `Frames`, `Reset`, `Cont0 = Shift0 | Reset0`

- A probe, additive (package `okay.kont`, `Freer` untouched): the
  machine's stack is `enum Frames` — a type-aligned list of frames
  that IS a continuation (`A => Freer`), its indexes joined as `Bind`
  joins them; the delimiter `$` is its `Reset` case; `Cont0` has two
  operations, `Reset0` (asks for the frame) and `Shift0` (cuts at it,
  `k` carries `ret`). One `@tailrec` loop, `Frames.run`, typed by GADT
  refinement, one class test on a `Bind`'s continuation and one on a
  `Delay`'s thunk. `Frames.apply` answers `Delay(Resume(a, fs))`: forced
  by an outer `Freer.resume` loop it runs the machine on itself, met by
  the machine it is spliced — the continuation carries its own
  interpreter, and `Freer` stays self-sufficient (specs/freer-kont.md,
  commits 4fdc4d496..7edd3c485).
- TestKont, 16 oracles: the `($v)`/`($/S0)` rules on TestDollar's
  bodies, nested prompts, 100 000 nested `$`, a 100 000-yield generator,
  100 000 left-nested binds without rotation, a 100 000-frame `k`
  resumed twice, the head form fed twice, a handler loop over the real
  `Freer.resume` outside the machine.
- Measured (history.d `…-freer-kont-frames-probe.tsv`): delimiter
  install at parity with the Delim machine, capture+resume 0.89-0.90x,
  left-nested binds 0.57x the rotation, right-nested at parity after
  the `Bind(Return(x), f)` arm.
- Found: in the lazy machine Cont's `(S, R)` pair carries no escape
  type (a body stands in the delimiter's place); the index that fits
  `shift0/$` is Materzok–Biernacki's stack of answer types —
  `Delim.Stacked`'s `p.type *: b`. Decision: migrate
  (`freer-kont-migrate`, sprint).
