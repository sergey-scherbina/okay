- [ ] handler-rename — level 2, step 1 (operator, 2026-10-02: "Окей бери").
      The name `Handler` goes to what the literature calls a handler: the
      value that takes an effect off the row, today's `Handling`, becomes
      `Handler[E, O]` (`Handler.Full[E, I, O, N]` in full). Today's
      `Handler[F]`, an answer per operation (`F ~> Id`, no continuation),
      becomes `Answers[F]`. Two commits, in that order: `Answers` first,
      every site found by the compiler (518 in 158 files, docs 162), then
      the new `Handler`. okay2 is its own build and keeps its names. Gate:
      `affected master staged`. specs/api-levels.md. (2026-10-02)
