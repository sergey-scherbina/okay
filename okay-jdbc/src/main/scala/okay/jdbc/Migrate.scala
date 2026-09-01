package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, Stream}
import okay.given
import okay.sql.{Isolation, Sql, SqlValue}

/**
 * Migrations for OUR databases (specs/jdbc.md, Own relational
 * databases) — the industry's settled discipline adopted rather than
 * reinvented: versioned, authored SQL scripts, applied in order,
 * each recorded with its CHECKSUM in a schema-version table in the
 * SAME database, so the database itself answers "what am I". A
 * changed checksum on an applied script REFUSES loudly — the
 * fingerprint rule at yet another seam. Written against the `Sql`
 * trait, so any driver serves it.
 *
 * One script is one `update` call: engines that take multi-statement
 * scripts (H2, Postgres) take them whole; others get one-statement
 * scripts. The script and its version row share a transaction where
 * the engine allows transactional DDL (Postgres does); engines that
 * auto-commit DDL keep the DML atomicity and the caveat.
 */
final case class Script(version: Int, name: String, sql: String)
final case class Applied(version: Int, name: String, checksum: String, at: Long)

object Migrate {

  /** the fingerprint: sha-256 of the script text, hex */
  def checksum(sql: String): String =
    val d = java.security.MessageDigest.getInstance("SHA-256")
      .digest(sql.getBytes("UTF-8"))
    d.iterator.map(b => f"$b%02x").mkString

  /**
   * Apply what is pending, in version order; answer what THIS run
   * applied, or the refusal that stopped it. `record` is the ops
   * hook, called after each commit — wiring it to a topic is one
   * lambda.
   */
  def apply(db: Sql, scripts: Seq[Script],
            table: String = "okay_schema_version",
            record: Applied => Unit = _ => ()): Either[String, Vector[Applied]] ! Async =
    // refusals that need no database: duplicates and disorder
    val versions = scripts.map(_.version)
    if versions.distinct.length != versions.length then
      okay.pure(Left(s"duplicate script versions: ${versions.diff(versions.distinct).distinct.mkString(", ")}"))
    else if versions != versions.sorted then
      okay.pure(Left("scripts are not in version order"))
    else
      ensure(db, table).flatMap(_ => applied(db, table)).flatMap { rows =>
        val byVersion = scripts.map(s => s.version -> s).toMap
        // the fingerprint check over everything already applied
        val drift = rows.collectFirst {
          case a if !byVersion.contains(a.version) =>
            s"script v${a.version} '${a.name}' was applied but has VANISHED from the set"
          case a if checksum(byVersion(a.version).sql) != a.checksum =>
            s"script v${a.version} '${a.name}' CHANGED after it was applied (checksum mismatch)"
        }
        drift match
          case Some(why) => okay.pure(Left(why))
          case None =>
            val done = rows.map(_.version).toSet
            val pending = scripts.filter(s => !done.contains(s.version))
            run(db, table, pending.toList, record, Vector.empty)
      }

  private def run(db: Sql, table: String, pending: List[Script],
                  record: Applied => Unit, acc: Vector[Applied])
  : Either[String, Vector[Applied]] ! Async = pending match
    case Nil => okay.pure(Right(acc))
    case s :: rest =>
      val a = Applied(s.version, s.name, checksum(s.sql), System.currentTimeMillis)
      one(db, table, s, a).flatMap {
        case Left(why) => okay.pure(Left(why))
        case Right(()) =>
          record(a)
          run(db, table, rest, record, acc :+ a)
      }

  /** one script and its version row, one transaction — rolled back
   * together on failure (as far as the engine's DDL allows) */
  private def one(db: Sql, table: String, s: Script, a: Applied)
  : Either[String, Unit] ! Async =
    db.begin(Isolation.ReadCommitted).flatMap { _ =>
      okay.async {
        try
          okay.!.run(Async.run[Long, Nothing](db.update(s.sql)))
          okay.!.run(Async.run[Long, Nothing](db.update(
            s"insert into $table (version, name, checksum, applied_at) values (?, ?, ?, ?)",
            Vector(SqlValue.I32(a.version), SqlValue.Text(a.name),
              SqlValue.Text(a.checksum), SqlValue.I64(a.at)))))
          okay.!.run(Async.run[Unit, Nothing](db.commit()))
          Right(())
        catch case e: Exception =>
          okay.!.run(Async.run[Unit, Nothing](db.rollback()))
          Left(s"script v${s.version} '${s.name}' failed: ${e.getMessage}")
      }
    }

  private def ensure(db: Sql, table: String): Long ! Async =
    db.update(s"""create table if not exists $table(
      version int not null primary key,
      name varchar(256) not null,
      checksum varchar(64) not null,
      applied_at bigint not null)""")

  private def applied(db: Sql, table: String): Vector[Applied] ! Async =
    drain(db.query(s"select version, name, checksum, applied_at from $table order by version"))
      .map(_.flatMap {
        case Vector(SqlValue.I32(v), SqlValue.Text(n), SqlValue.Text(c), SqlValue.I64(at)) =>
          Some(Applied(v, n, c, at))
        case _ => None   // a foreign row shape in OUR table would be its own drift
      })

  private def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async))
  : Vector[Vector[SqlValue]] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(Vector.empty)
      case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
    }
}
