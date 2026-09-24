## okay2-tref - transactional cells and typed-key maps in okay2

okay's `TRef` (a versioned CAS cell with waiters, wrapped or bare),
`TMap` (typed keys by `Same`, typed iteration), `TDict` and `TList` in
okay2, with `Same.byValue` and the `===`/`=!=`/`sameAs` operators
(specs/okay2.md stage 15). The Scala 3 TestTMap and TestSame, plus
TRef's own: 80 000 increments from 8 threads landing exactly once, the
bare cell's no-change rule, waiters firing once and in order.

Docs: docs/okay2.md section 19.
