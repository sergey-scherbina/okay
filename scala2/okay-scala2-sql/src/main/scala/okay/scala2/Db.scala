package okay.scala2

import okay.Row.plus
import okay.codec.Schema
import okay.sql.{Bad, Drift, Isolation, SqlValue, Typed}

/**
 * SQL for Scala 2.13 (specs/scala2-facade.md, stage 8).
 *
 * Probed first: okay-sql's DATA is readable from scalac 2.13 —
 * `SqlValue`, `Bad`, `Drift`, `Isolation`, the `Sql` driver trait, and
 * okay-jdbc's `JdbcSql` — so a Scala 2 caller passes parameters and
 * reads errors with okay-sql's own types. What it cannot use is every
 * operation, because each one answers a program (`Long ! Async`, a
 * `Source`). This class is those operations as `Eff` and `Source`, each
 * a call into okay-sql's `Typed`: rows decoded by `Schema` by column
 * label (camelCase to snake_case), parameters bound positionally
 * through the driver's prepared path, the transaction as a `Resource`
 * region that commits on completion and rolls back on anything else.
 */
final class Db private (sql: okay.sql.Sql) {

  /** the driver, for the other facade modules (okay-scala2-services'
   * outbox runs in the same database, and so the same transaction) */
  private[scala2] def underlying: okay.sql.Sql = sql

  /** every row, streamed; a row that does not decode is a `Left(Bad)` in
   * the stream, never a throw */
  def rows[A](query: String, params: SqlValue*)(using s: Schema[A]): Source[Either[Bad, A]] =
    Db.flat(Typed.rows[A](sql, query, params.toVector))

  /** the same, with the parameters bound from a case class `p`, in
   * field order */
  def rowsOf[A, P](query: String, p: P)(using sa: Schema[A], sp: Schema[P]): Source[Either[Bad, A]] =
    Db.flat(Typed.rowsOf[A, P](sql, query)(p))

  /** every row, read in full; the first row that does not decode fails
   * the program with it, as a typed `Throws[Bad]` */
  def all[A](query: String, params: SqlValue*)(using s: Schema[A]): Eff[Async & Throws[Bad], Vector[A]] =
    Db.firstBad(rows[A](query, params*))

  def allOf[A, P](query: String, p: P)(using sa: Schema[A], sp: Schema[P]): Eff[Async & Throws[Bad], Vector[A]] =
    Db.firstBad(rowsOf[A, P](query, p))

  /** an INSERT/UPDATE/DELETE; the count of rows it touched */
  def update(query: String, params: SqlValue*): Eff[Async, Long] =
    Async.lift(sql.update(query, params.toVector))

  def updateOf[P](query: String, p: P)(using s: Schema[P]): Eff[Async, Long] =
    Async.lift(Typed.update[P](sql, query)(p))

  /** does the query's result still have the columns `A` expects? Each
   * difference is a `Drift` naming the column; empty means it matches */
  def verify[A](query: String)(using s: Schema[A]): Eff[Async, Vector[Drift]] =
    Async.lift(Typed.verify[A](sql, query))

  /**
   * `body` in one transaction: committed when it completes, rolled back
   * when it fails. This is okay-sql's `Typed.transact` under `Resource`,
   * the same region the Scala 3 API uses.
   */
  def transaction[A](isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false)
                    (body: Db => Eff[Async, A]): Eff[Async, A] =
    Async.lift(okay.Resource.run[A, okay.Async](
      Typed.transact[A, okay.Async](sql, isolation, readOnly)(_ => Async.core(body(this)).plus[okay.Resource])))
}

object Db {

  /** over any okay-sql driver */
  def apply(sql: okay.sql.Sql): Db = new Db(sql)

  /** over a JDBC connection, through okay-jdbc; the caller owns the
   * connection and closes it */
  def jdbc(connection: java.sql.Connection, fetchSize: Int = 64): Db =
    new Db(new okay.jdbc.JdbcSql(connection, fetchSize))

  private def flat[A](s: okay.Source[okay.Chunk[Either[Bad, A]]]): Source[Either[Bad, A]] =
    Source.of(okay.Writer.expand[okay.Chunk[Either[Bad, A]], Either[Bad, A], Unit, okay.Async](s)(c => c))

  private def firstBad[A](s: Source[Either[Bad, A]]): Eff[Async & Throws[Bad], Vector[A]] =
    s.runCollect.flatMap { rows =>
      rows.collectFirst { case Left(b) => b } match {
        case Some(b) => Throws.raise[Bad, Vector[A]](b)
        case None => Eff.pure(rows.collect { case Right(a) => a })
      }
    }
}
