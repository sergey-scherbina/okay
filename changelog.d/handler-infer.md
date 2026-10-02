## handler-infer - p.handle(Handler.answer { case Find(id) => … }), no [F]

The operator's ask, 2026-10-02 (specs/handler-forms.md).

- `Handler.answer { case … }` reads the effect off its cases: each
  pattern names a constructor, and the sealed parent they share is the
  effect. A transparent macro types the result `Handler[Accounts, …]`, so
  `p.handle(Handler.answer { … })` needs no type argument. The checks are
  the same as with `[F]`: each case's answer and exhaustiveness.
- An effect named explicitly is now `Handler[F].answer { … }`,
  `Handler[F].state(s0) { … }` or `Handler[F].into[G] { … }`, each with
  `.poly`. One name could not hold both forms (Scala calls it an
  ambiguous overload). An effect with parameters besides its answer
  (`Reader % Int`) cannot be read off `Ask()`, and the macro says to name
  it. `state` keeps its effect named: read off the cases, its pair would
  end in an `Any`, and the compiler's exhaustiveness check warns.
- TestHandlerInfer has 5 tests. TestHandlerForms, the docs and the
  benchmark are moved to the new spelling.
