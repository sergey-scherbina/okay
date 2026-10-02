## cont-macro-collections - the Cont macro reads Set/Map traversals, Either.map/flatMap and Try.getOrElse; cont-stack-layer1-c closed

The rest of cont-stack-layer1-c (operator: "Реплей нет. Остальное да":
no replay mode; the rest, yes).

**Read now**, each a program over the lazy `k`, a million on a 128 KB
thread with zero switches (red first: StackOverflowError):
- the known traversals (`map`, `foreach`, `foldLeft`, `flatMap`,
  `exists`, `forall`, `find`, `foldRight`) on any IMMUTABLE collection,
  `Set` and `Map` included, not only a `Seq`; `Set.map`/`flatMap` still
  answer a `Set` (duplicates collapse);
- `Either`'s `map` and `flatMap` (the left side widened by the evidence
  the call had, never a cast);
- `Try`'s `getOrElse`, whose default is not under a catch.

**Fixed on the way:** `exists`/`find` turned the receiver into a `List`
first, so over an infinite `LazyList` they never returned. They walk a
memoised `LazyList` now, so an early stop forces nothing after it, and a
resumed `k` walks the same cells (multi-shot safe).

**Opaque on purpose**, each pinned by a test that its meaning holds:
- `Try(…)` and `Try`'s `map`/`flatMap`/`fold`/`recover` catch what their
  code throws, and with a lazy `k` the rest would run outside them;
- a mutable collection's traversal would read it after the body could
  have changed it;
- a `LazyList`'s `map` is lazy and must not be forced.

**A plain (non-inline) `def` that takes `k`** is now a documented rule
rather than a transform. Make it `inline`, or write it in Cont style
(`k(helper(x))`). Reading its TASTy would need `-Yretain-trees` in every
caller's build.

The one shape left open, PState's function answer, moved to backlog
cont-fun-answer to be re-measured on the frame machine.

Tests: TestContMacro. Docs: docs/cont-stack.md.
