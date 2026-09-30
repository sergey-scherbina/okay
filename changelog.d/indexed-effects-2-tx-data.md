## indexed-effects-2-tx-data — the transaction protocol as data on the row `TxOp +~ Unary[Async]`, the connection typed by the index

Stage 2 of specs/indexed-effects.md, on the row by the operator's
word. `Tx.TxOp[S, R, +X]` says the transitions once, in the signature
(`Begin: Idle -> Open`, `Commit`/`Rollback` back, statements at any
state); `Tx.Data[A, From, To]` reads left to right over the tree's
`Freer[Row, To, From, A]`; `Tx.Data.begin/commit/rollback/update/
batch/describe` are the doors, no `transition` claim anywhere;
`Tx.Data.async` lifts a whole `Async` program onto the diagonal
through the new core `Indexed.lift`, so a body may wait, log or call
another service. `Tx.Data.interpret` is `State.handle`'s shape with
the OUTPUT an `Async` program and the state a `Conn[S]` moved only by
the arm that runs the driver's `begin`/`commit`/`rollback`: `closed`
on a `Conn[Idle]` and `opened` on a `Conn[Open]` do not type, inside
the handler included (TestTxData pins both). Found: a match type whose
indexes are provably disjoint WARNS (E184) unless it has a default
case, so `Unary` reduces to `Nothing` there now; an extension on a
GADT-refined receiver is found lexically, not through the companion's
implicit scope. The `Prog` facade `Tx` stays beside it. Gate:
TestTxData + TestTx + TestFreerPara, `affected master Test/compile`.
