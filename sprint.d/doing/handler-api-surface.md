- [ ] handler-api-surface — PRIORITY: MEDIUM (2026-10-04). Too many ways to
      write a handler, overlapping: `Effects.handle`, `relay`, `translate`,
      `interpret`, `Handler`, `Handler.Full`, `Handler.stateOf`, `Answers`,
      `HandleFrames.*`, `Lexical`. The differences are cost and what the
      handler may do (abort, perform G). Reduce to two or three with one
      table of "when which" (docs), after handler-one-step.
      FROM handler-one-step (closed 2026-10-04, stages 1-3: every
      state-threading fold is one step on `HandleFrames.stateRun` /
      `stateRunUntil` / `stateRunOr`): what stays hand-written is of another
      nature and belongs here — `State.zoomWith` and `Maybe.prune` re-tell one
      effect as another (translate-shaped, kept off `Effects.translate` by
      `Distinct`), Resource (finalizers, a catch frame), Logic.msplit (a
      search), Effects.handle/relay/translate (the clause gets `k`).

