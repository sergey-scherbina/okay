# okay-tx

Transactions as a subtree of the program, on the kernel's words (`okay.core`).

The effect is an algebraic data type:

```
Tx[A] = Atomic(A ! Tx + Reader % C + T) | Rollback(Error)
```

`Atomic` is a bracket that holds its body as a subtree, so brackets cannot mismatch: the closing one is the return
from the node, there is no commit to forget. `Rollback` has no value: nothing after it type-checks. Inside a bracket
the body has the transaction's effect again (brackets inside brackets) and the resource `C` — a connection — to read;
`T` is the row outside, the resource's owner's.

The owner (`Tx.Transactor[C, T]`) says how a transaction begins, commits and rolls back — programs over `T`, so a
pool is a handler there, no connection global — and whether a failure is to be run again (a deadlock). The handler
walks the tree with one bit, closed or open: the first bracket met closed opens; every inner bracket is nothing but
its body; the return from the first commits; a `Rollback`, or any failure, anywhere inside rolls back — the rest of
the subtree dropped, the finalizers between run — and fails the rest where the bracket was (`k.failed`), so a
`recover` around the bracket sees it. One begin, one commit, however deep. An abort or a cancel crossing the bracket
rolls back.

```scala
  def sql(s: String): Eff[B, Unit] = ask[Conn].at[B].flatMap(c => tell(s"$s on $c").at[B])
  def charge: Eff[B, Unit] = atomic[Conn, T, Unit](sql("update").flatMap(_ => sql("insert payment"))).at[B]
  def place: Eff[Txs[Conn, T] +: T, Unit] =
    atomic[Conn, T, Unit](sql("insert order").flatMap(_ => charge)).at[Txs[Conn, T] +: T]
    val r = run(writer[String, Unit, Pure](transactions[Conn, Unit, T](owner)(place)))
```

(`TestTxWords`: `charge`'s bracket a child of `place`'s, whoever called it — one BEGIN, one COMMIT in the log.)

The JVM's own: `Jdbc[T](connect, isolation, readOnly, retries)` — a connection from a pool's `connect`, auto-commit
off at the isolation asked, the driver's commit and rollback, the connection closed after either; SQLSTATE 40001
and 40P01 run the bracket again on a fresh connection. `Jdbc.execute`, `Jdbc.queryInt` on the bracket's connection.

`Sessions[T](connect, …)` is the same transactor with the connection guarded: the bracket's resource is a `Session`
(`execute`, `queryInt`, `withConnection`), closed when the bracket commits or rolls back, before the connection is.
A session kept past its bracket — a closure over it, a reference stashed — throws `Session.Closed` naming the
statement, instead of running it on a connection already closed or given back to its pool. `Sessions.execute`,
`Sessions.queryInt` on the bracket's session; `Session` is a `caps.SharedCapability` for capture-checked callers
([okay-caps](okay-caps.md)).

Tests: `TestTx` (brackets inside brackets, a rollback in the inner one, a failure, an abort crossing, the owner's
retry, a recover around the bracket, a rollback with none open), `TestTxWords`, `TestJdbc` and `TestSessions` on H2, `TestTxEscape`.

Where it stands to `okay-sql`'s `Typed.transact`: that region is on okay-freer's words and refuses a nested begin at
run time; this one is on the kernel's words and nests by construction. The owner for the `Sql` seam comes with the
platform's move to the kernel's words.
