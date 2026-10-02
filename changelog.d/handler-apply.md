## handler-apply - Handler[Accounts] { case … }, one implementation per form

The operator, 2026-10-02 (specs/handler-forms.md).

- Each form has one implementation: `Handler.answer[F]`, `Handler.state[F,
  S](s0)`, `Handler.into[F, G]` and `Handler.control[F, O](ret)`.
- `Handler[F]` is a helper for type inference only, and names the effect
  once. `Handler[Accounts] { case … }` is form 1, the default.
  `Handler[Accounts].state(s0)` reads `S` off `s0`, and `.into[G]` and
  `.control[O](ret)` fill the effect in.
- Reading the effect off the cases (`Handler.answer { … }`, from
  handler-infer) is gone. It cannot share the name with
  `Handler.answer[F]`, and the effect named is the operator's choice.
- TestHandlerInfer is now TestHandlerFor (4 tests). The docs, the tests and
  the benchmark lead with `Handler[F] { … }`.
