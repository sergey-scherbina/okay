- [ ] okay2-handler-case-form — the Scala 3 core's `Handler[F] { case … }`,
      each case's answer checked against its constructor's declared type
      by a macro (specs/handler-forms.md). okay2-level1-api (2026-10-02)
      gave okay2 the four forms with clauses as anonymous classes, and a
      clause asserts its answer per case (`answer[X](…)`), because scalac 2
      does not refine `X` from `Find <: Op[Option[String]]` in a match. A
      blackbox macro over the cases could check each one and insert the
      assertion itself; worth it if okay2 users write handlers by hand.
      (2026-10-02)
