## cont-macro-join-points - a conditional not in tail position no longer makes a Cont body opaque

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

Tests: TestContMacro. Backlog: cont-stack-layer1-c (item 1 done, items
2-5 left).
