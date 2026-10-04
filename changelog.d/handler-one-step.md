## handler-one-step (stage 1) — a tail-resumptive handler written once: its fold and frame from one step

Operator, 2026-10-04 ("делай как предлагаешь", of what is still too complex).

- `HandleFrames.stateRun(t, ret)(step)(s0, x)`: `step(s, op) => (s2, v)`;
  inline, so at each site the step expands into the fold's own loop (the
  hand-written folds' shape, `val (s2, v) = f(s, op); loop(s2)(k(v))`), and
  the frame is `stateful`'s from the same step.
- On it: State (`Handler.stateOf`), Writer (`loopWith`), Supply, Refs, Once —
  their hand-written loops gone (-109 +47 lines).
- Measured against the hand loops (history.d handler-one-step): stateSmall
  1.02x, stateHandle 0.99x, writerTell 0.97x, mixedList within noise. The
  first cut was 1.36x on stateSmall: the lone last operation rewritten as a
  bind (+56 B a run); answered in place now.
- Refuted on the way: the resuming step `(s, op, resume)` as the fold's — the
  `resume` is a closure and its loop call no tail call (`@tailrec`).
- Left (sprint item): the handlers that stop or capture.
