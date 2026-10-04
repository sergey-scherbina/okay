package okay2.pg

import okay2.!
import okay2.async.Async
import okay2.sql.{Isolation, SqlValue}

/**
 * The bulk-load posture on Postgres (okay-pg's Load.scala, specs/data.md):
 * loading is BULK, under a LOAD ID the far end dedups — a loads REGISTRY
 * whose primary key is the load id, the registry row and the COPY
 * committed in ONE transaction, so a load retried after a crash lands
 * once.
 */
object Load {

  sealed trait Result

  object Result {
    final case class Loaded(rows: Long) extends Result
    /** the id is in the registry: the retry's honest answer */
    case object AlreadyLoaded extends Result
  }

  /** the registry, own-posture DDL */
  def ensure(db: PgSql): Unit ! Async =
    db.update(
      """create table if not exists okay_loads(
         load_id text primary key,
         loaded_at timestamptz not null default now())""").map(_ => ())

  /** one idempotent bulk load: BEGIN; claim the id (ON CONFLICT DO
   * NOTHING); if claimed, COPY and COMMIT with the claim; if not, the
   * load already happened and nothing runs */
  def load(db: PgSql, loadId: String, table: String, columns: Vector[String],
           rows: Vector[Vector[SqlValue]]): Result ! Async =
    db.begin(Isolation.ReadCommitted).flatMap { _ =>
      db.update("insert into okay_loads(load_id) values ($1) on conflict do nothing",
        Vector(SqlValue.Text(loadId))).flatMap { claimed =>
        if (claimed == 0) db.rollback().map[Result](_ => Result.AlreadyLoaded)
        else
          db.copyIn(s"copy $table (${columns.mkString(", ")}) from stdin", rows.iterator.map(PgSql.copyRow))
            .flatMap(n => db.commit().map[Result](_ => Result.Loaded(n)))
      }
    }
}
