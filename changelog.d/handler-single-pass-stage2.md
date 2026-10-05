## handler-single-pass-stage2: `handle` registers, one walk answers the stack (the operator's design)

- `p.handle(h)` with a stepped `h` registers it. One handler is its own `run`, marked. The second `handle`
  makes the pair a `HandleFrames.Handled` stack, and every later one pushes onto it. One walk answers the
  whole stack: dispatch by a class table checked by the handler's own test, `Stepped.stepAt` (state into
  the slot, no pair), `Halt` dropping the handlers inside, forwarding once with the states kept per
  resumption. Handlers that are not stepped stay runs of their own. The frame face is the runs nested.
- `TestHandledStack`: the laws against the handlers' own runs nested (three handlers, a map between, halts
  inside and outside, Choose outside, Throws inside, a `ret` performing outward, 100 000 operations on
  256 KB).
- Cost: fold-built programs 0.91–0.95x, 37 KB less; the right-nested shape of a recursion 1.17x slower
  (backlog handler-single-pass-staged takes it back with a macro-staged walk); a single small `handle`
  1.10x; one `handle` over many operations unchanged (history.d handler-single-pass-stage2).
