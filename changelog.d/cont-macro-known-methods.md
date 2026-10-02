## cont-macro-known-methods - Option, Either, &&, || and Seq's flatMap/exists/forall/find/foldRight with k in them are read by the Cont macro

The table of known methods, cont-stack-layer1-c item (2) (the operator:
"Дальше", "next"). Each known call is rewritten into a shape the CPS
transform already reads:
- **`Option`** (`getOrElse`, `map`, `flatMap`, `fold`, `orElse`) and
  **`Either`** (`fold`, `getOrElse`) become a `match`, handled by the
  join point;
- **`&&` and `||`** become an `if`;
- **`Seq`'s `flatMap`** is the traversal plus a flatten;
- **`exists` / `forall` / `find`** stop at the first element that
  decides (`Cont.existsIn` / `findIn`);
- **`foldRight`** is `foldIn` over the reversed elements.

The receiver is evaluated once, and a by-name argument only in its
branch. `B >: A` comes from evidence summoned at macro time, never a
cast.

- **Depth.** A million each, on a 128 KB thread, with zero switches.
  They failed with StackOverflowError first.
- **Meaning is kept:** a defined option never calls `k` in `getOrElse`,
  `exists` stops calling `k` after the first true, `foldRight` goes from
  the right, the collection type is kept, and multi-shot works.
- **No library or benchmark body changes form.**

Tests: TestContMacro. Docs: docs/cont-stack.md.
