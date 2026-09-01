package okay.pg

import okay.{!, Async}
import okay.sql.SqlValue

/**
 * The bulk-load posture (specs/data.md, OLAP write posture;
 * specs/sql.md's COPY box), exercised on the free engine first:
 * loading is BULK, under a LOAD ID the far end dedups — WithKey at
 * batch granularity. Where a warehouse has per-file load history,
 * plain Postgres gets the honest equivalent: a loads REGISTRY
 * whose primary key is the load id, and the registry row and the
 * COPY commit in ONE transaction — so a load retried after a
 * crash between journal and commit lands once, because either
 * both survived or neither did.
 */
object Load {

  enum Result:
    case Loaded(rows: Long)
    /** the id is in the registry: the far end says it already
     * happened — the retry's honest answer */
    case AlreadyLoaded

  /** the registry, own-posture DDL */
  def ensure(db: PgSql): Unit ! Async =
    db.update(
      """create table if not exists okay_loads(
         load_id text primary key,
         loaded_at timestamptz not null default now())""").map(_ => ())

  /**
   * One idempotent bulk load: BEGIN; claim the id (ON CONFLICT DO
   * NOTHING — their constraint is the dedup); if claimed, COPY and
   * COMMIT — atomically with the claim; if not, the load already
   * happened and nothing runs.
   */
  def load(db: PgSql, loadId: String, table: String, columns: Vector[String],
           rows: Vector[Vector[SqlValue]]): Result ! Async =
    db.begin(okay.sql.Isolation.ReadCommitted).flatMap { _ =>
      db.update("insert into okay_loads(load_id) values ($1) on conflict do nothing",
        Vector(SqlValue.Text(loadId))).flatMap { claimed =>
        if claimed == 0 then
          db.rollback().map(_ => Result.AlreadyLoaded)
        else
          db.copyIn(
            s"copy $table (${columns.mkString(", ")}) from stdin",
            rows.iterator.map(PgSql.copyRow))
            .flatMap { n => db.commit().map(_ => Result.Loaded(n)) }
      }
    }
}
