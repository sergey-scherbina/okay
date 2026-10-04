package okay2.jdbc

import okay2.!
import okay2.async.Async
import okay2.sql.{Granted, Isolation, Sql, SqlValue}
import okay2.stream.{Chunk, Source}
import Migrate.block

/**
 * The OLAP write posture (okay-jdbc's BulkLoad.scala, specs/data.md):
 * loading is BULK — stage a file, COPY it, under a LOAD ID the far end
 * dedups. Engines without a load history of their own get the posture's
 * OWN: a history table whose UNIQUE KEY is the dedup — WithKey at batch
 * granularity, in one transaction with the work.
 *
 * The COPY statement stays the caller's SQL (each engine's COPY dialect
 * is visible, not abstracted); BulkLoad owns only the load-id discipline.
 */
object BulkLoad {

  sealed trait Outcome

  object Outcome {
    /** this call loaded it; the count is the engine's answer */
    final case class Loaded(rows: Long) extends Outcome
    /** the id is in the history: a retry after a crash-after-commit,
     * landing exactly once by NOT landing again */
    case object AlreadyLoaded extends Outcome
  }

  /**
   * One load: the history row and the caller's COPY commit in ONE
   * transaction, so a crash between them rolls back BOTH and the retry
   * starts clean; a crash after the commit finds the id and answers
   * AlreadyLoaded.
   */
  def load(db: Sql, loadId: String, copySql: String,
           history: String = "okay_load_history"): Outcome ! Async =
    ensure(db, history).flatMap { _ =>
      db.begin(Isolation.ReadCommitted).flatMap { _ =>
        Async {
          try {
            val claimed =
              try {
                block(db.update(
                  s"insert into $history (load_id, loaded_at) values (?, ?)",
                  Vector(SqlValue.Text(loadId), SqlValue.I64(System.currentTimeMillis)))): Unit
                true
              } catch {
                case e: Exception =>
                  // a refused insert must mean the KEY, not a dead wire:
                  // verify before answering AlreadyLoaded
                  block(db.rollback())
                  val there = block(db.update(
                    s"update $history set loaded_at = loaded_at where load_id = ?",
                    Vector(SqlValue.Text(loadId))))
                  if (there == 1) false else throw e
              }
            if (!claimed) Outcome.AlreadyLoaded: Outcome
            else {
              val rows = block(db.update(copySql))
              block(db.commit())
              Outcome.Loaded(rows)
            }
          } catch {
            case e: Exception =>
              // a failing COPY rolls back the claim with it — the retry
              // with a fixed file starts clean, never half-loaded
              try block(db.rollback())
              catch { case _: Exception => () }
              throw e
          }
        }
      }
    }

  private def ensure(db: Sql, history: String): Long ! Async =
    db.update(s"""create table if not exists $history(
      load_id varchar(256) not null primary key,
      loaded_at bigint not null)""")

  /**
   * The posture, held by a wrapper: reads pass, transactions pass, but
   * row DML refuses BY NAME and says where the right door is.
   * `BulkLoad.load` takes the UNDERLYING db.
   */
  def olap(db: Sql): Sql = new Sql {
    private def refuse(sql: String): Nothing =
      throw new UnsupportedOperationException(
        s"row DML in the OLAP posture: '${sql.take(48)}…' — loading is bulk; stage a file and COPY it under a load id (BulkLoad.load)")
    def describe(sql: String) = db.describe(sql)
    def query(sql: String, params: Vector[SqlValue]): Source[Chunk[Vector[SqlValue]]] = db.query(sql, params)
    def update(sql: String, params: Vector[SqlValue]): Long ! Async = {
      val head = sql.trim.take(6).toLowerCase
      if (head.startsWith("insert") || head.startsWith("update") || head.startsWith("delete")) refuse(sql)
      else db.update(sql, params)
    }
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = refuse(sql)
    def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async = db.begin(isolation, readOnly)
    def cancel(): Unit = db.cancel()
    def commit(): Unit ! Async = db.commit()
    def rollback(): Unit ! Async = db.rollback()
    override def sqlState(t: Throwable): Option[String] = db.sqlState(t)
  }
}
