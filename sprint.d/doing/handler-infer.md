- [ ] handler-infer — the operator, 2026-10-02: "можно ли писать так
      p.handle(Handler.answer { case Find(id) => … })". The effect is
      read off the cases: each pattern names a constructor, and its parent
      is the effect. `Handler.answer { case … }` and `Handler.state(s0) {
      case … }` with no `[F]`, a transparent macro typing the result.
      An effect with parameters (`Reader % Int`) cannot be read off
      `Ask()`, and keeps its `[F]`; the macro says so. specs/handler-forms.md.
      (2026-10-02)
