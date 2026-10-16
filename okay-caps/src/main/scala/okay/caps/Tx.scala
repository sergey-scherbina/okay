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
 * closures uncaptured) — specs/okay-caps.md */
final class Tx private[caps] (db: Db) extends caps.SharedCapability:
  def execute(sql: String): Int = db.run(sql)

object Tx:
  /** the bracket: begin, the body with its transaction, commit — rollback, and the failure on, if it fails */
  def transaction[T](db: Db)(body: Tx => T): T =
    db.begin()
    val t =
      try body(new Tx(db))
      catch case e: Throwable =>
        db.rollback()
        throw e
    db.commit()
    t

  /** the same, the transaction given: `Tx(db) { Tx.execute("...") }` */
  def apply[T](db: Db)(body: Tx ?=> T): T = transaction(db)(tx => body(using tx))

  /** a statement in the transaction given */
  def execute(sql: String)(using tx: Tx): Int = tx.execute(sql)
