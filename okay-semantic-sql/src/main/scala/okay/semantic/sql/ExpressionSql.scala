package okay.semantic.sql

import okay.Async
import okay.freer.{!, Chunk, Writer}
import okay.sql.{Sql, SqlValue}
import okay.semantic.Result
import okay.semantic.ossie.{ExpressionPlan, Table}

/** Identifier-only input projection. All expression semantics stay in the portable plan. */
object ExpressionSql:
  final case class Input[A](table: String, columns: Vector[String], decode: Vector[SqlValue] => Either[String,A])
  def query[A](input: Input[A]): Either[Vector[String],String] =
    val names = input.table.split("\\.",-1).toVector
    val invalid = (names ++ input.columns).filterNot(_.matches("[A-Za-z_][A-Za-z0-9_]*"))
    if invalid.nonEmpty || input.columns.isEmpty || input.columns.distinct.size != input.columns.size then
      Left(Vector("SQL expression input: empty/duplicate columns or invalid identifiers " + invalid.mkString(", ")))
    else
      def quoted(s: String): String = "\"" + s + "\""
      Right(s"SELECT ${input.columns.map(quoted).mkString(", ")} FROM ${names.map(quoted).mkString(".")}")
  def execute[A](plan: ExpressionPlan[A], input: Input[A], tables: Map[String,Table] = Map.empty)(using sql: Sql)
      : Either[Vector[String],Result] ! Async = query(input) match
    case Left(es) => okay.freer.pure(Left(es))
    case Right(query) =>
      Writer.loopWith[Chunk[Vector[SqlValue]],Vector[Vector[SqlValue]],Unit,Either[Vector[String],Result],Async](sql.query(query))(Vector.empty)(
        (acc,chunk) => (acc ++ chunk.iterator.take((plan.maxRows + 1 - acc.size).max(0))).take(plan.maxRows + 1)) { (rows,_) =>
        if rows.size > plan.maxRows then Left(Vector("SQL expression input exceeds row budget"))
        else
          val decoded = rows.map(r => if r.size != input.columns.size then Left("SQL input column arity mismatch") else input.decode(r))
          val errors = decoded.zipWithIndex.flatMap((r,i) => r.left.toOption.map(e => s"SQL row $i: $e"))
          if errors.nonEmpty then Left(errors) else plan.run(decoded.flatMap(_.toOption),tables)
      }
