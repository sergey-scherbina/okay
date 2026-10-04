## handler-single-pass-stage1: `Handler.Stepped`, the step a one-pass walk needs, exposed by the built-ins

- `Handler.Stepped[E, S, O]`: `takes`, `init`, `step(s, op): (S, Any) | Halt[S]`, `ret`, `halted`. These are
  the only things the coming walk over a stack of handlers will know of a handler (specs/handler-single-pass.md).
- Implemented by `Handler.stateOf` (so `State(s)` and `Handler.state`), `Handler.answerOf` (`Reader(r)` and
  `Handler.answer`), `Writer.log`, `Once.memo`, `Fresh.counter`, `Supply.from` and `Chronicle.verdict`,
  whose halt is `Halt` and `halted`. Every `run` is unchanged, so a handler used alone costs what it did.
- `TestStepped`: each, walked through its step alone, answers what `p.handle(h)` answers.
