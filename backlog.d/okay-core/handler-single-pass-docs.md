- [ ] handler-single-pass-docs — PRIORITY: LOW (2026-10-05, stage 5 of
      specs/handler-single-pass.md). The user's pages say nothing yet of
      `handle` registering. docs/your-own-effect.md "Which one" should say
      which handlers FUSE into one walk: the forms `Handler.answer` and
      `Handler.state`, and every state-threading built-in (State, Reader,
      Writer.log, Once.memo, Fresh.counter, Supply.from, Chronicle.verdict),
      which are `Handler.Stepped`. `Handler.into` and `Handler.control`,
      Throws, Choose and Maybe stay runs of their own. It should also say
      how an author makes a handler of their own fuse (implement `Stepped`:
      `takes`, `init`, `step`, `ret`, `halted`, `stepAt`), and what a
      stack costs: fold-built programs 0.91–0.95x, a recursion's shape
      1.17x until handler-single-pass-staged. Every example line pinned by
      a TestDocExamples suite.
