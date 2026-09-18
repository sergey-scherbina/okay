- [x] deep-recursive-direct — LANDED 2026-09-15: inside `direct` at the
      program type a self-call is deferred wherever it is marked or
      auto-coloured, so `def fib(n: Int): Long ! Pure = direct: ... fib(n - 1)
      + fib(n - 2)` needs no annotation (with `Direct.given` imported). A
      separate `deepRecursive` was built first and removed at the operator's
      ask. TestDirectDeep; specs/direct-macro.md "Deep recursion". The
      `.?` finding filed beside it was withdrawn: `.?` is retired as a mark
      on purpose (Direct.scala's own comment), `.reflect`/`.!?`/`!p` are the
      spellings. The entry as written: the article's `deepRecursive` (Halotu Kozak,
      "Deep recursion in Scala 3", a macro that rewrites a self-recursive
      body into TailRec's tailcall/flatMap/done) as a `direct` rule. It
      already works BY HAND on master, probed 2026-09-15 with scala-cli
      against the built classes, native stack 512 KB: `def fib(n) = direct:
      if n < 2 then n else !.tailcall(fib(n - 1)).reflect + !.tailcall(fib(n
      - 2)).reflect` (75025 at 25), `sum(1_000_000)` non-tail in 65 ms,
      `isEven(1_000_001)` through `isOdd` — mutual recursion, which the
      article's macro refuses. Her TailRec IS our Free: tailcall = `Delay`,
      flatMap = `Bind(Delay, k)`, done = `Pure`, `.result` = `!.run`. What is
      missing is the zero-annotation form: a rule in Direct.scala that
      colours a call to the ENCLOSING def as `!.tailcall(call).reflect` —
      `asMark`/`opColor` colour by type today, this one colours by the
      enclosing method's symbol. ~30 lines plus a test on fib/sum/isEven;
      the lowering of if/match/blocks/`a + b` is Direct's already.
