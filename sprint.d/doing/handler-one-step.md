- [ ] handler-one-step — PRIORITY: HIGH, STAGE 1 LANDED 2026-10-04 (operator, 2026-10-04: "что еще …
      слишком сложно"). Every handler (~20: State, Writer, Supply, Refs,
      Once, Chronicle, Generate, Lexical, Resource, Maybe, Logic,
      Effects.handle/relay/translate, Handler.stateOf …) is written TWICE:
      its fold (a loop over `resumeRun`, `split`, `forwarded`, `shallow`,
      ~30 lines each) and its frame (`HandleFrames.stateful`/`control`). The
      frames cannot replace the folds (handlers-as-frames, REFUTED: 1.5-4.3x),
      but most handlers state ONE step `(s, op, resume)`; one generic INLINE
      fold engine could build both faces from it, each handler written once.
      Probe first: the hand loops exist for speed (TestInlineBudget,
      FreqInlineSize 325) — stateSmall, stateHandle, writerTell, mixedList,
      handlePrebuilt, relayPrebuilt against master.
      STAGE 1 (2026-10-04): `HandleFrames.stateRun` — a TAIL-RESUMPTIVE state
      handler written once as `step(s, op) => (s2, v)`, its fold and its frame
      both built from it (inline: the step expands into the fold's own loop).
      State (`Handler.stateOf`), Writer (`loopWith`), Supply, Refs, Once on it:
      -109 +47 lines; stateSmall 1.02x, stateHandle 0.99x, writerTell 0.97x,
      mixedList within noise (history.d handler-one-step). The resuming form
      `(s, op, resume)` cannot build a fold: the `resume` it is handed is a
      closure, and a loop call in a closure is no tail call (`@tailrec`
      refused it). LEFT: the handlers that do not always resume at once —
      Chronicle, Generate (stopping), Writer.foldUntil, Lexical.walk, Maybe,
      Logic.msplit, Resource, Effects.handle/relay/translate — need a form of
      their own (a step answering "resume with (s2, v)" OR "stop with r").
      STAGE 3 (2026-10-04): `HandleFrames.stateRunOr` — a step answering
      `(s2, v)` or `Stop(program)` (allocated only on the stop): Chronicle
      (dictate / halt) on it; Lexical.walk on `stateRun` (stateLexWalk 1.00x).
      The state-threading folds are all on the three engines now. LEFT, and
      of another nature: `State.zoomWith` and `Maybe.prune` RE-TELL one effect
      as another (translate-shaped; `Distinct` keeps them off
      `Effects.translate`, Maybe's own comment); Resource (finalizers, a catch
      frame); Logic.msplit (a search); Effects.handle/relay/translate (the
      clause gets `k`). They are not one-step state folds — handler-api-surface
      is where they belong.

