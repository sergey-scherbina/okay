package okay.sql

import okay.Async
import okay.core.{+:, Eff, In, Paths, Row as Rows}
import okay.one.Cont
import okay.one.std.{%, Reader}
import okay.std.Chunk
import okay.tx.Txs

/**
 * TRANSACTIONS AS A TREE, over okay-sql's driver (okay-tx; specs/freer-on-core.md). `Sql.begin`/`commit`/`rollback`
 * are three programs whose ORDER is the protocol; okay-sql used to check the order in the types, the index of an
 * indexed tree (`Idle -> Open -> Idle`). On the kernel the protocol is the program's SHAPE instead: a transaction is
 * a bracket that holds its body (`atomic`), so a begin without its commit, a commit without its begin, a nested
 * begin cannot be written at all — the closing bracket is the return from the node. A `rollback` is a leaf.
 *
 *     Tx.interpret(db)(
 *       Tx.atomic[T, Long, Tx.Txn[T] +: T](
 *         Tx.update[Tx.Inside[T]]("insert into t values (1)")))
 *
 * okay-tx's handler does the rest: one `begin` and one `commit` however deep the brackets, a `rollback`, a failure
 * anywhere inside, an abort or a cancel crossing all roll back (the caveat of the indexed tree — a failure dropped
 * the commit and left the transaction open — is gone), and a serialization failure or a deadlock (`Sql.retryable`)
 * runs the bracket again when `retries` allows. Inside a bracket the connection is read (`Reader % Conn`), so a
 * statement types only there; any `Async` program runs inside too (`async`).
 *
 * Kernel words (`Eff[R, A]`, rows `E +: T`), as okay-tx's are: the transaction effect names the row outside, `T`,
 * which a row-polymorphic `A ! Es` cannot. `T` holds `Async`, where the driver's programs run.
 */
object Tx:
  /** THE CONNECTION inside a bracket: the driver, and the isolation the server granted at `begin` */
  final class Conn private[sql] (val db: Sql, val granted: Granted)

  /** the transaction effect of okay-sql's driver, the row outside `T` */
  type Txn[T <: Rows] = Txs[Conn, T]
  /** the row inside a bracket: brackets again, the connection, the row outside */
  type Inside[T <: Rows] = Txn[T] +: Reader % Conn +: T

  /** THE OWNER of the brackets: the driver's `begin`, `commit`, `rollback`, run where `Async` is in `T`; a
   * serialization failure or a deadlock (`Sql.retryable` of the driver's `sqlState`) runs the bracket again, up to
   * `retries` more times */
  final class Transactor[T <: Rows](db: Sql, isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false,
      retries: Int = 0)(using a: In[Async, T]) extends okay.tx.Tx.Transactor[Conn, T]:
    def begin: Eff[T, Conn] = db.begin(isolation, readOnly).on(Paths.One(a)).map(g => Conn(db, g))
    def commit(c: Conn): Eff[T, Unit] = c.db.commit().on(Paths.One(a))
    def rollback(c: Conn): Eff[T, Unit] = c.db.rollback().on(Paths.One(a))
    override def retry(e: Throwable, attempt: Int): Boolean =
      attempt <= retries && db.sqlState(e).exists(Sql.retryable)

  /** a bracket: `body` atomic — in the transaction open, or in one opened for it */
  def atomic[T <: Rows, A, R <: Rows](body: Eff[Inside[T], A])(using In[Txn[T], R]): Eff[R, A] =
    okay.tx.Tx.atomic[Conn, T, A, R](body)
  /** the transaction rolled back for `reason`: a leaf; outside its bracket, `okay.tx.Tx.Rolled(reason)` */
  def rollback[T <: Rows, R <: Rows](reason: Throwable)(using In[Txn[T], R]): Eff[R, Nothing] =
    okay.tx.Tx.rollback[Conn, T, R](reason)

  /** the connection of the bracket */
  def conn[R <: Rows](using In[Reader % Conn, R]): Eff[R, Conn] = Reader.ask[Conn, R]
  /** the isolation the server granted the bracket */
  def granted[R <: Rows](using In[Reader % Conn, R]): Eff[R, Granted] = conn[R].map(_.granted)
  /** a statement on the bracket's connection: the affected-row count */
  def update[R <: Rows](sql: String, params: Vector[SqlValue] = Vector.empty)(using In[Reader % Conn, R],
      In[Async, R]): Eff[R, Long] = conn[R].flatMap(c => async[Long, R](c.db.update(sql, params)))
  def batch[R <: Rows](sql: String, rows: Chunk[Vector[SqlValue]])(using In[Reader % Conn, R],
      In[Async, R]): Eff[R, Long] = conn[R].flatMap(c => async[Long, R](c.db.batch(sql, rows)))
  def describe[R <: Rows](sql: String)(using In[Reader % Conn, R], In[Async, R]): Eff[R, Vector[Col]] =
    conn[R].flatMap(c => async[Vector[Col], R](c.db.describe(sql)))
  /** any `Async` program where `Async` is in the row: a transaction body is not the protocol alone */
  def async[A, R <: Rows](p: okay.![A, Async])(using a: In[Async, R]): Eff[R, A] = p.on(Paths.One(a))

  /** THE BRACKETS RUN against the driver: `p`'s transactions begun, committed and rolled back by `Transactor` */
  def interpret[A, T <: Rows](db: Sql, isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false,
      retries: Int = 0)(p: Eff[Txn[T] +: T, A])(using In[Async, T]): Eff[T, A] =
    Cont.handle(p)(okay.tx.Tx.closed[Conn, T, A](Transactor[T](db, isolation, readOnly, retries)))
