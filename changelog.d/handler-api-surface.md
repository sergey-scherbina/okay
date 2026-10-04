## handler-api-surface: one table of every handler form; Memory and PyStream on the state engine

- docs/your-own-effect.md, "Which one: the whole list": every way to give an effect a meaning, keyed by what
  the handler must do. It covers the four `Handler` forms, `Answers[F]`, `!.interpret`/`!.tracing` and
  `Lexical`. The primitives the forms stand on (`Effects.handle`, `!.relay`, `!.translate`) and the
  level-3 engines (`HandleFrames.stateRun`/`stateRunUntil`/`stateRunOr`) are named as such.
- Nothing removed: each entry answers a question none of the others does, and each has callers
  (specs/handler-forms.md, Decisions).
- okay-agent's `Memory.handle` and okay-py's `PyStream.holding` were the last hand-written state folds
  outside the core. Each is now one step on `HandleFrames.stateRun` (-45 lines).
