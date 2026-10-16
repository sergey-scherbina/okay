package okay.caps

import language.experimental.captureChecking

/** a database as a transaction needs it: begin, commit, rollback, a statement */
trait Db:
  def begin(): Unit
  def commit(): Unit
  def rollback(): Unit
  def run(sql: String): Int

/** A TRANSACTION, A CAPABILITY: it exists inside its bracket only. A body whose result holds it — the transaction
 * itself, a closure over it, an iterator reading it — does not compile (capture checking, in the CALLER's file:
 * `import language.experimental.captureChecking`), so a statement after the commit cannot be written. What it does
 * not see, measured: a caller compiled without capture checking, a `LazyList`, a kernel program (`A ! R`, its
 * closures uncaptured) — specs/okay-caps.md; those fail at run time instead, the transaction CLOSED when its bracket
 * ends (`Tx.Closed`) */
final class Tx private[caps] (db: Db) extends caps.SharedCapability:
  /** open while the bracket runs; closed at its commit or rollback, for good */
  private val open = java.util.concurrent.atomic.AtomicBoolean(true)

  /** a statement in the transaction — after its bracket, `Tx.Closed`: THE GUARD for what the checker cannot see */
  def execute(sql: String): Int =
    if !open.get then throw Tx.Closed(sql)
    db.run(sql)

  private[caps] def close(): Unit = open.set(false)

object Tx:
  /** the bracket: begin, the body with its transaction, commit — rollback, and the failure on, if it fails */
  def transaction[T](db: Db)(body: Tx => T): T =
    db.begin()
    val tx = new Tx(db)
    val t =
      try body(tx)
      catch case e: Throwable =>
        tx.close()
        db.rollback()
        throw e
    tx.close()
    db.commit()
    t

  /** the same, the transaction given: `Tx(db) { Tx.execute("...") }` */
  def apply[T](db: Db)(body: Tx ?=> T): T = transaction(db)(tx => body(using tx))

  /** a statement after its transaction's bracket ended: a leak the checker did not refuse, caught here */
  final class Closed(sql: String) extends IllegalStateException(
    s"transaction closed: '$sql' after the bracket's commit or rollback — the transaction leaked out of it")

  /** a statement in the transaction given */
  def execute(sql: String)(using tx: Tx): Int = tx.execute(sql)
