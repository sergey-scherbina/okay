## cont-macro-join-points - a conditional not in tail position, and k inside map/foreach/foldLeft, no longer make a Cont body opaque

cont-stack-layer1-c item (1), the operator's "Продолжай исправлять" ("keep
fixing"). `ContMacro`'s CPS transform gave up on an `if` or `match` whose
branches call `k` when something followed the conditional:
`1 + (if c then k(1) else 2)`, or a `match` feeding an expression. The
whole body became the strict leaf, which nests the host stack.

- **The fix is a join point.** The rest is bound once as a local
  function, and every branch ends by calling it. The rest is not copied
  into each branch, so nested conditionals stay linear in size.
- **Depth.** A million such bodies on a 128 KB thread give the answer
  with zero stack switches. Before, the same test failed with a
  StackOverflowError (watched first).
- **Meaning is kept**: if and match, `k` in some branches only,
  multi-shot across the join, and the order "condition, then `k`'s rest,
  then the body's own rest".

Item (2), in part: `k` called inside the lambda of `map`, `foreach` or
`foldLeft` on a `List`, `Vector` or immutable `Seq`
(`List(1, 2).map(x => k(x)).sum`).
- **How.** The lambda's body becomes a program over the lazy `k`, and
  the traversal becomes `Cont.traverse` / `Cont.foldIn`: binds the
  machine runs, on an immutable list, so resuming `k` twice
  re-traverses and shares no iterator.
- **Depth.** A million on 128 KB with zero switches. The test failed
  with StackOverflowError first.
- **Meaning is kept.** Evaluation order is element by element, each
  call of `k` running its rest before the next element. Multi-shot
  works, and the collection type is kept (`Vector` stays `Vector`).
- **No existing code changes form.** Neither shape occurs outside the
  new tests, so no benchmark lane is touched.

Tests: TestContMacro. Docs: docs/cont-stack.md. Backlog:
cont-stack-layer1-c (item 1 done, item 2 in part, the rest left).
