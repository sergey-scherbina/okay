## handler-one-step (stage 2) — a fold that stops, from one step too

- `HandleFrames.stateRunUntil(t, done, end)(step)`: `stateRun` that answers
  `end(s)` the moment the state is `done` — at the start or after a step —
  calling no continuation, so nothing past that operation is built; its frame
  is a clause that does not resume when done.
- On the engines: `Generate.foldUntil`, `Writer.foldUntil` (stopping),
  `Generate.fold`, `Generate.each` (tail) — their hand loops gone (-84 +36).
- Measured against the hand loops (compare module, history.d
  handler-one-step-2): writerFoldUntilOfLong 1.01x, writerUnfoldFoldUntil
  1.00x, streamSpecialized 1.00x.
