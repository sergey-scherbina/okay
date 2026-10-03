## delimited-simplify — one loop, marks as boundaries, a prompt as one boundary, a throw into a continuation (with shift-prompt-one-boundary)

Operator, 2026-10-03, after cont-atm: "есть что нибудь в новой машине и
эффектах что можно упростить или сделать лучше?" — "Сделай сразу все".
specs/cont-atm.md §7.

- `Delimited.Run` is the ONE loop for every effect; `Outer` says what leaves.
  `Machine`, `Under`, `Over` were three copies of it.
- `Frames.Marked` deleted: a mark is a value boundary; `find` is `holds`;
  `closed` walks value boundaries, a `Kont` is a piece — Cont's ATM through marks.
- A Shift prompt is ONE boundary: `push` marked `whole`, `dollar` by the prompt
  with `ret` the first frame above it; `close` gone. For `push` alone
  (shift-prompt-one-boundary, history.d): delimPushOnly 0.54x, delimDollarResume
  0.73x, statePara 1.00x; delimGenerator 1.07-1.22x with 18% fewer bytes — open.
- `Shift.Resumption.raise`, `Shift.raise`, `Paused.fail`: a failure thrown INTO a
  paused run, caught by its own `try`s.
