- [ ] handler-one-step — PRIORITY: HIGH (operator, 2026-10-04: "что еще …
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
