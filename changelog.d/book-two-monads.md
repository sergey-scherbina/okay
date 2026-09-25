## book-two-monads - the book's chapter 16b: two monads at once, one program three ways

docs/continuations/16b-two-monads-at-once.md, between chapters 16 and 17
(numbered 16b so no later chapter or cross-link moves).

- The problem: each monad alone is easy, two need hand plumbing, and the
  reason — composing `M[N[_]]` needs a "swap" (Jones & Duponcheel's
  distributive law) that no monad supplies. Toy `OptionT` and `ListT`
  show what a transformer's `flatMap` actually does.
- One real program (a basket over two shops: List of choices, Either
  errors, a Writer log) written three ways and asserted equal: cats
  `EitherT[WriterT[List, …], …]` with its lifts; layered reflection
  (`Layered`, reify = η $ e, reflect = shift0, a `Logged` case class of
  the user's as a layer); algebraic effects (`Choose + Writer % String +
  Throws % String` with `runEither`, `Writer.collect`, `runChoice`). The
  other order is a new `App` for cats, swapped `reify` blocks for layers,
  swapped handlers for effects — the last two shown and tested.
- Filinski 1994 (reflection into the transformer stack, which stays) and
  1999 (the numbered CPS hierarchy) between them; the FSCD 2019
  correspondence (a deep handler is `$` + `shift0`) ties the last two.
- Compiled: TestBookTwoMonads (okay-direct, 8) and TestBookTwoMonadsCats
  (okay-cats, 4); every example line pinned (a mutant unpinned line was
  watched turning TestDocSnippets red).
- Linked from the book index, chapters 16 and 17, docs/direct-style.md,
  docs/theory/08-direct-style.md and docs/okay2.md.
