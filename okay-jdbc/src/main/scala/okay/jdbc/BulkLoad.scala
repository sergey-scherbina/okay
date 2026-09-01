package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, async}
import okay.given
import okay.sql.{Granted, Isolation, Sql, SqlValue}

/**
 * The OLAP write posture (specs/data.md): loading is BULK — stage a
 * file, COPY it, under a LOAD ID the far end dedups. Warehouses
 * carry their own load history (Snowflake per file, BigQuery per
 * job); engines that do not get the posture's OWN: a history table
 * whose UNIQUE KEY is the dedup — WithKey at batch granularity, the
 * Durable recovery story scaled up to files, and the Migrate shape
 * retold (a discipline table in the same database, one transaction
 * with the work).
 *
 * The COPY statement stays the caller's SQL (bind-don't-model: each
 * engine's COPY dialect is visible, not abstracted); BulkLoad owns
 * only the load-id discipline around it.
 */
object BulkLoad {

  enum Outcome:
    /** this call loaded it; the count is the engine's answer */
    case Loaded(rows: Long)
    /** the id is in the history: a retry after a crash-after-commit,
     * landing exactly once by NOT landing again */
    case AlreadyLoaded

  /**
   * One load: the history row and the caller's COPY commit in ONE
   * transaction, so a crash between them rolls back BOTH and the
   * retry starts clean; a crash after the commit finds the id and
   * answers AlreadyLoaded.
   */
  def load(db: Sql, loadId: String, copySql: String,
           history: String = "okay_load_history"): Outcome ! Async =
    ensure(db, history).flatMap { _ =>
      db.begin(Isolation.ReadCommitted).flatMap { _ =>
        async {
          try
            val claimed =
              try
                okay.!.run(Async.run[Long, Nothing](db.update(
                  s"insert into $history (load_id, loaded_at) values (?, ?)",
                  Vector(SqlValue.Text(loadId), SqlValue.I64(System.currentTimeMillis)))))
                true
              catch case e: Exception =>
                // a refused insert must mean the KEY, not a dead wire:
                // verify before answering AlreadyLoaded
                okay.!.run(Async.run[Unit, Nothing](db.rollback()))
                val there = okay.!.run(Async.run[Long, Nothing](db.update(
                  s"update $history set loaded_at = loaded_at where load_id = ?",
                  Vector(SqlValue.Text(loadId)))))
                if there == 1 then false else throw e
            if !claimed then
              Outcome.AlreadyLoaded
            else
              val rows = okay.!.run(Async.run[Long, Nothing](db.update(copySql)))
              okay.!.run(Async.run[Unit, Nothing](db.commit()))
              Outcome.Loaded(rows)
          catch case e: Exception =>
            // a failing COPY rolls back the claim with it — the retry
            // with a fixed file starts clean, never half-loaded
            try okay.!.run(Async.run[Unit, Nothing](db.rollback()))
            catch case _: Exception => ()
            throw e
        }
      }
    }

  private def ensure(db: Sql, history: String): Long ! Async =
    db.update(s"""create table if not exists $history(
      load_id varchar(256) not null primary key,
      loaded_at bigint not null)""")

  /**
   * The posture, held by a wrapper: reads pass, transactions pass,
   * but row DML refuses BY NAME — in this class row INSERTs are
   * wrong on cost and performance both, and the refusal says where
   * the right door is. `BulkLoad.load` takes the UNDERLYING db.
   */
  def olap(db: Sql): Sql = new Sql:
    private def refuse(sql: String): Nothing =
      throw UnsupportedOperationException(
        s"row DML in the OLAP posture: '${sql.take(48)}…' — loading is bulk; stage a file and COPY it under a load id (BulkLoad.load)")
    def describe(sql: String) = db.describe(sql)
    def query(sql: String, params: Vector[SqlValue]) = db.query(sql, params)
    def update(sql: String, params: Vector[SqlValue]): Long ! Async =
      val head = sql.trim.take(6).toLowerCase
      if head.startsWith("insert") || head.startsWith("update") || head.startsWith("delete")
      then refuse(sql) else db.update(sql, params)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = refuse(sql)
    def begin(isolation: Isolation): Granted ! Async = db.begin(isolation)
    def cancel(): Unit = db.cancel()
    def commit(): Unit ! Async = db.commit()
    def rollback(): Unit ! Async = db.rollback()
}
