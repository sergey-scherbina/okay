package okay2.jdbc

import okay2.{!, Pure}
import okay2.async.Async
import okay2.platform._
import okay2.sql.{Bad, Sql, SqlValue, Typed}
import okay2.stream.{Chunk, Source}
import okay2.codec.Schema

/** the suites' runner: a program executed on the platform, and a row
 * stream drained into its chunks */
object Run {
  def apply[A](prog: A ! Async): A = !.run(Async.run[A, Pure](prog))

  def chunks[A](s: Source[Chunk[A]]): List[Chunk[A]] =
    apply(Source.SourceOps(s).runCollect).toList

  def rows[A](db: Sql, sql: String, params: Vector[SqlValue] = Vector.empty)(implicit s: Schema[A]): Vector[Either[Bad, A]] =
    chunks(Typed.rows[A](db, sql, params)).flatten.toVector
}
