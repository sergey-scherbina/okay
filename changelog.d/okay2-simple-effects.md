## okay2-simple-effects - the facade's effect form in okay2, handlers as okay's

- `okay2.simple` (optional): `Op`, `Effect[F[_]]` with `send`/`handle`/`run`,
  and `Handler[F[_], R, B]` — the facade's way to declare an effect and
  handle it with one shape. `import okay2.simple.{Effect, Handler, Op}`
  beside `import okay2._`; the facade's own-effect suite runs on okay2
  with only its package and imports changed.
- okay2's handlers are spelled as okay's: `new Handler[F] { def
  handle[A](a: F.Op[A]): A }` (`Handler.Of` stays), and
  `!.handle[F, G](m)(ret)(h)` in okay's order with the program and
  answer inferred (a value class for the split, no allocation).
- `Free[-R, +A]` and `!` no longer bound the row by `Row`, so a handler
  over "any rest", `def console[R, B]`, needs no bound.
- The facade's own-effect test names the empty row `Pure`.

336 tests in the okay2 gate. Docs: docs/okay2.md section 3 ("The simple
form") and section 7 ("The four handler shapes"); specs/okay2.md stage 11.
