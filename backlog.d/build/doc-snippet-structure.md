- [ ] doc-snippet-structure — PRIORITY: MEDIUM. `TestDocSnippets` pins a
      doc's Scala block LINE BY LINE: each trimmed line must occur somewhere
      in a compiled source. Order, indentation and the lines left out are
      not checked, so a block assembled from scattered real lines passes
      while it is no longer code. Found 2026-10-02 in docs/contract.md (the
      operator: "по этой ссылке неправильное определение"): `Applicative`'s
      `def app(a: F[A]): F[B]` and `Selective`'s `def select(f: => F[A =>
      B]): F[B]` had lost their `extension` lines, so the page stated
      `app: F[A] => F[B]` and dropped `select`'s `F[Either[A, B]]`;
      `Effects[M]`'s `flatMap`/`foldCont` the same; a `for` with its
      generators unindented; a handler's `case` glued to a test's
      `assertEquals`; `foldMap`'s `def … =` followed by an unrelated test
      line as if it were the body. Every line was pinned and green. Fixed
      on that page by contract-doc-signatures. THE CHECK THAT WOULD HAVE
      CAUGHT IT: a block is an EXCERPT of one source, in order — its
      non-prose lines a subsequence of one file's lines (`...` marking a
      gap), indentation relative to the block's first line preserved; a
      block quoting two files splits into two blocks. Measure first how
      many pinned blocks across docs/ the stricter rule turns red, and
      ratchet them the way snippet-debt does.
