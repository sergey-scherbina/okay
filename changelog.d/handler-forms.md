## handler-forms - one door for the effect author: four forms of handler

Level 2, step 2 (operator, 2026-10-02; specs/handler-forms.md,
docs/your-own-effect.md "A handler as a value").

- `Handler.answer[F]`, `Handler.state[F, S](s0)`, `Handler.into[F, G]` and
  `Handler.control[F, O](ret)` each give a level-1 value, applied as
  `p.handle(h)`. Under them are `relay`, State's loop, `translate` and
  `Effects.handle`. `Handler.from(answers)` turns an `Answers[F]` into one.
- Two roads under one name. `{ case Find(id) => … }` is checked by a macro:
  each case's answer, a missing operation (a compile error), and a `case _`
  that may only throw. `.poly { [X] => … }` is checked by the compiler.
- The cases see an operation at an abstract `Answer`, not at `Any`. An
  operation whose caller chooses its answer (`Asks`, `Update`) can only be
  answered by its own data: `g(1)` passes, `"oops"` is refused. An
  operation whose answer is its own field's type (`State`'s `Set(s: S)`)
  is beyond any monomorphic case, and the macro refuses it by name,
  pointing at `.poly`.
- `control` answers a tail `resume` with no capture (2.17x -> 1.26x of
  `Maybe.option`). `state` is inline (1.60x -> 1.04x of `State(s)`).
  `answer` is at parity with `Reader(r)`, and the case form costs what the
  polymorphic one does.
- TestHandlerForms has 16 tests. HandlerFormsBenchmark is new.
