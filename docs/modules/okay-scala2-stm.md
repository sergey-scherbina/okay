# okay-scala2-stm

okay-stm for **Scala 2.13**. The cell, `TRef`, is okay's own and is used
directly. `Tx` is the transaction language as a capability of `Eff`, and
`Stm.atomically` runs a transaction as one atomic step:

| | |
|---|---|
| `Tx.read(r)`, `Tx.write(r, a)`, `Tx.modify(r)(f)`, `Tx.update(r)(f)` | the cell operations, inside a transaction |
| `Tx.retry`, `Tx.check(cond)` | park until something the transaction read changes |
| `Tx.orElse(a, b)` | `b` if `a` retries; `a`'s writes are discarded |
| `Stm.atomically(tx)` | `Eff[Tx, A]` to `Eff[Async, A]`, committed together or not at all |

I/O inside a transaction is a type error, as in Scala 3.

The walkthrough is section 8l of
[okay from Scala 2.13](../scala2.md#8l-transactions-okay-stm), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
