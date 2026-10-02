## cont-step-on-frames - the frame machine's stack SEGMENTED at its delimiters (decided: the machine), Cont.step kept, the segmented machine made faster

- The stack of `Frames.run` is two type-aligned lists (Dybvig, Peyton
  Jones & Sabry 2007): `Frames = End | Frame` is one segment, `Stack =
  Done | Run | Reset` the segments and the delimiters between them, a
  `Reset` carrying the segment that waits for its answer; three
  registers. A capture moves NODES and shares frames, a resumption
  pushes `k`'s nodes, so the single list's two O(n²) — 20 000 captures
  under a deep stack, 100 000 nested resumptions — are linear (TestKont
  pins both). Step 1 is 6d63508bc, its path work 862b40451..9471e2b2f.
  DECIDED by the operator: this is the machine; what follows is speed.
- `Cont.step` STAYS: Cont's runner on the frame machine read 4.3–6.3x on
  fib/statePara/contAnswer (d894c6a42, reverted in 8c960950b; history.d
  2026-09-30T221639Z).
- Simpler and faster: one operation arm for `Inject`/`Diag`, `relink`
  typed, `Delim.samePrompt` named (3e37e9bc8); `Frames` is not a
  function, Delim's stale doc comments gone and its inline doors through
  `Cont0.shift`/`shift0` (1bf072481); `plain` is `ret eq
  Cont0.identity`, no field, and `shift`/`control` install their
  delimiter in the machine (`Shift0.under`) instead of wrapping the body
  in a `reset` operation (b0bd665ed, f40c8d46e); `capture` and `pushed`
  out of the loop (90b7fedd9); `Rev.onto` tests an empty machine first
  (acd9941a9).
- Against the single-list machine at the end: delimGenerator **0.72x**,
  layeredViaDollar **0.86x**, stateLexDeep 1.00x, pure binds 1.00–1.04x,
  layeredViaPush 1.03x, stateDeep 1.03–1.05x, delimDollarResume 1.11x,
  writerTellUnderDelim 1.155x, bare install/pop 1.36–1.39x; 20–45% fewer
  bytes on every resuming lane (history.d 2026-10-01T050417Z,
  2026-10-01T053245Z). Refuted and reverted: the plain-pop branch (−3 ns
  per `$`), a hot core with a driver (85d96fed3, f972a14df).
- The install/pop gap, read off C2's code with hsdis: register
  allocation — three loop registers kept in stack slots on the fast path
  (74 loads from `sp` against the single list's 34); specs/freer-kont.md
  Results, backlog cont-frames-register-pressure. Also filed:
  cont-frames-head-form-run, cont-frames-relink-two-nodes.
- On the way: appendix A of docs/continuations quotes the segmented
  types (a0e150e31); OneJob's unused import from master dropped
  (7291d46aa).
