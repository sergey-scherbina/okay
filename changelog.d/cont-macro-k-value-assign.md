## cont-macro-k-value-assign - k passed to map/foreach as a value, and an assignment from k, are read by the Cont macro

The rest of cont-stack-layer1-c's item (2), and assignment, after
cont-macro-join-points (the operator: "Продолжай исправлять", "keep
fixing").
- **`k` itself passed to a traversal:** `List(x).map(k)` and `foreach(k)`
  on an immutable `Seq` are read as `x => k(x)`, through the same
  traversal.
- **An assignment whose value calls `k`:** `v = k(1)`, or
  `seen += k(x)` inside a traversal's lambda. The value is computed
  first, in its own order (a variable read on the right is read before
  the call, as on the strict road), then stored.
- **Depth.** A million of each on a 128 KB thread, with zero switches.
  They failed with StackOverflowError first.
- **Meaning is kept**, multi-shot included: each resumption assigns
  again.
- **No library or benchmark body changes form.** A store of `k`
  (`resume.k = k`, Handler.control) stays opaque, since `k` is not
  called.
- **The doc's opaque example changes.** It now uses `k` under `try`,
  which stays opaque on purpose: with a lazy `k`, the rest would run
  outside the `try`.

Tests: TestContMacro, TestDocExamplesContStack. Docs: docs/cont-stack.md.
