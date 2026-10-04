## handler-one-step (stage 3) — a step that may stop with an answer; every state fold on the engines

- `HandleFrames.stateRunOr(t, ret)(step)`: the step answers `(s2, v)` to
  resume or `Stop(program)` to answer the whole run (allocated only then).
- Chronicle (a dictate records and resumes, a halt stops with the record) on
  it; Lexical.walk on `stateRun` (stateLexWalk 1.00x, history.d
  handler-one-step-3). -53 +38 lines.
- Every state-threading fold is now written once, as its step. What is left
  is of another nature (re-telling, finalizing, searching, capturing): the
  sprint item says which and where it belongs.
