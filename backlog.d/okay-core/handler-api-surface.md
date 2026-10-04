- [ ] handler-api-surface — PRIORITY: MEDIUM (2026-10-04). Too many ways to
      write a handler, overlapping: `Effects.handle`, `relay`, `translate`,
      `interpret`, `Handler`, `Handler.Full`, `Handler.stateOf`, `Answers`,
      `HandleFrames.*`, `Lexical`. The differences are cost and what the
      handler may do (abort, perform G). Reduce to two or three with one
      table of "when which" (docs), after handler-one-step.
