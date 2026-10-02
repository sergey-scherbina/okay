## handler-rename - Handler is the handler; an answer per operation is Answers

Level 2, step 1 (operator, 2026-10-02: "Окей бери"; specs/api-levels.md).

- `Handler[F]` answered each operation with a value: `F ~> Id`, no
  continuation, and it did not take the effect off the row. It is now
  `Answers[F]`: `Answers.union`, `Answers.flat`, `ComonadAnswers`, and
  `Answers.scala` for the file. 760 sites in 220 files, code and docs, every
  code site confirmed by the compiler. The scala2 facade's own
  `Handler[F, R, B]` and Jetty's `Handler` are untouched.
- The name `Handler` goes to what the literature calls a handler (Plotkin
  and Pretnar): the value `p.handle(h)` applies, until now `Handling`. The
  usual case is `Handler[E, O]` (`State(s): Handler[State % S, [A] =>> (S,
  A)]`) and the full form is `Handler.Full[E, I, O, Needs]` (`Reset[R]`).
- TestHandling is now TestHandler. TestDocSnippets is green.
