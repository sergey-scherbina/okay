## okay2-handler-case-form - okay2's Handler[F] { case … } checked by a macro; modify measured as one operation

okay2's handler forms take `{ case … }` (specs/okay2.md stage 51,
specs/handler-forms.md "In okay2"):

- **The forms:** `Handler[Accounts] { case Find(id) => db.get(id); … }`,
  `.state(s0) { case (s, op) => … }` and `.into[G] { … }`.
- **The check:** a blackbox macro checks every case against its
  constructor's declared answer:
  - a wrong answer is a compile error naming the constructor and both
    types;
  - an answer chosen by the caller (`Update[S, B]`) or tied to the
    operation's own field is refused by name, pointing at `.poly`;
  - `case _` may only throw;
  - a missing constructor is scalac's exhaustiveness error.
- **`.poly`** takes the clause traits, as the Scala 3 core's does.

`modify`, one `Update` since okay2-level1-api, is measured: 112 B a level
against 240 for a get and a set, the same as `set` (ProbeRowCost twin,
history.d `okay2-state-modify-op`).

Tests: TestHandler (case forms, four refusals), ProbeRowCost. Docs:
docs/okay2.md §4.
