## okay2-level1-api - level 1 in the Scala 2.13 twin: Shift[R], handler values, p.handle(h), Effects in okay's shape

okay2 gets the Scala 3 core's level-1 API (specs/okay2.md stage 49):

- `Shift[R]` over Delim, with top-level `shift`/`shift0`/`reset`, a
  compile-time `Shift.Key`, `Shift.Nesting`, nested resets past the stack,
  and `Shift.exit`/`collect`/`emit`/`cont`/`embed`. Cont's own capture is
  `Cont.shift`/`Cont.reset`.
- `Handler[F]` renamed `Answers[F]`. `Handler[E, O]`/`Handler.Full` are
  the handler value, and `p.handle(h)` (one to three handlers at a call)
  is a whitebox macro that computes the rest of the row. There are four
  forms (`answer`, `state`, `into`, `control`, with `Handler[F]` as the
  helper), ready values for State, Reader, Throws, Choose, Writer, Once,
  Resource and Reset, and `p.run`.
- State is `Get` + `Update`.
- `Effects`: `handle(m)(ret)(h)` and level 1 in the trait, plus
  `Effects.convert`/`reify`/`reflect`.

Commits on feature/okay2-level1-api: `Cont.shift` rename, `Shift[R]`,
handler values and forms, Effects. Docs: docs/okay2.md §4, §6, §8, §23.
Tests: TestShift, TestHandler, TestReflect.
