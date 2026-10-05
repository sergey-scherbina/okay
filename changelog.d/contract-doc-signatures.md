## contract-doc-signatures - docs/contract.md's code says the signatures it means

The operator, reading the page: "по этой ссылке неправильное определение".
Every code block on docs/contract.md had been assembled from single source
lines with the indentation and the `extension` lines dropped, so the page
stated other functions than the library has:

- `Applicative.app` read `F[A] => F[B]` and `Selective.select` lost its
  `F[Either[A, B]]`. Both blocks now quote Monad.scala with their
  `extension` lines, and a sentence spells the full signatures (`app`:
  `F[A => B] => F[A] => F[B]`; `select`: `F[Either[A, B]] => F[A => B] =>
  F[B]`, its handler by name).
- `Effects[M]`'s `flatMap` and `foldCont` sat beside `pure` with `F` and `A`
  unbound; they are under `extension [F[+_], A](m: M[F, A])` now, the gaps
  marked `...`, and a sentence says the program is their first argument.
- The `foldCont` examples show their effect (`Op`), each handler as a whole
  `val` and its closing line apart from it; the state example shows its
  program; `foldMap`'s signature and its IO test are two blocks, not a `def`
  whose body looked like a test assertion; every `for` is indented.

Every line still occurs in a compiled source (TestDocSnippets), which is
how the broken blocks passed before: the check is per line. Filed as
backlog `doc-snippet-structure`. Commits: `git log --grep contract-doc-signatures`.
