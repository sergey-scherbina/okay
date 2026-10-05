package okay.semantic.sql

import okay.{!, Async, Chunk, Writer}
import okay.sql.{Sql, SqlValue}
import okay.semantic.*

/** Explicit storage binding: business definitions do not contain SQL fragments. */
final case class Binding(table: String, dimensions: Map[String, String], measures: Map[String, String])

object Render:
  def apply[A](plan: Plan[A], binding: Binding): Either[Vector[String], Statement[A]] =
    val dimIds = (plan.dimensions.map(_.id) ++ plan.request.filters.map(_.dimension)).distinct
    val measureIds = plan.measures.map(_.id)
    val errors = Vector.newBuilder[String]
    def identifier(value: String, label: String): String =
      if !value.matches("[A-Za-z_][A-Za-z0-9_]*") then errors += s"$label: invalid SQL identifier $value"
      "\"" + value + "\""
    def column(id: String, columns: Map[String, String], label: String): String =
      columns.get(id) match
        case None => errors += s"$label $id: missing SQL binding"; ""
        case Some(c) => identifier(c, s"$label $id")
    val table = identifier(binding.table, "table")
    val dims = dimIds.map(id => id -> column(id, binding.dimensions, "dimension")).toMap
    val measures = measureIds.map(id => id -> column(id, binding.measures, "measure")).toMap
    val params = Vector.newBuilder[SqlValue]
    val filters = plan.request.filters.map { f =>
      f.equalTo match
        case Value.Null => s"${dims(f.dimension)} IS NULL"
        case other =>
          params += (other match
            case Value.Text(v) => SqlValue.Text(v)
            case Value.Number(v) => SqlValue.Num(v)
            case Value.Bool(v) => SqlValue.Bool(v)
            case Value.Null => SqlValue.Null)
          s"${dims(f.dimension)} = ?"
    }
    val found = errors.result()
    if found.nonEmpty then Left(found)
    else
      val groupColumns = plan.dimensions.map(d => dims(d.id))
      val select = groupColumns ++ Vector("COUNT(*)") ++ measureIds.flatMap { id =>
        Vector(s"SUM(${measures(id)})", s"COUNT(${measures(id)})") }
      val where = if filters.isEmpty then "" else " WHERE " + filters.mkString(" AND ")
      val group = if groupColumns.isEmpty then "" else " GROUP BY " + groupColumns.mkString(", ")
      Right(new Statement(plan, s"SELECT ${select.mkString(", ")} FROM $table$where$group", params.result()))

/** Decode errors are values; connection/query failures retain the Sql driver's semantics. */
final class Statement[A] private[sql] (val plan: Plan[A], val sql: String, val params: Vector[SqlValue]):
  def decode(frames: Vector[Vector[SqlValue]]): Either[Vector[String], Result] =
    val errors = Vector.newBuilder[String]
    val groups = Vector.newBuilder[Group]
    val width = plan.dimensions.size + 1 + plan.measures.size * 2
    frames.zipWithIndex.foreach { (frame, row) =>
      if frame.size != width then errors += s"row $row: expected $width cells, found ${frame.size}"
      else
        val damage = Vector.newBuilder[String]
        def decimal(cell: SqlValue, label: String): Option[BigDecimal] = cell match
          case SqlValue.Null => None
          case SqlValue.Num(n) => Some(n)
          case SqlValue.I32(n) => Some(BigDecimal(n))
          case SqlValue.I64(n) => Some(BigDecimal(n))
          case other => damage += s"row $row: $label: expected exact numeric, found $other"; None
        def count(cell: SqlValue, label: String): BigInt = decimal(cell, label) match
          case Some(n) if n.isWhole && n >= 0 => n.toBigInt
          case _ => damage += s"row $row: $label: expected nonnegative integer count"; BigInt(0)
        val key = frame.take(plan.dimensions.size).zip(plan.dimensions).map { (cell, d) =>
          cell match
            case SqlValue.Null => Value.Null
            case SqlValue.Text(v) => Value.Text(v)
            case SqlValue.Bool(v) => Value.Bool(v)
            case n => decimal(n, s"dimension ${d.id}").fold[Value](Value.Null)(Value.Number.apply)
        }
        val rows = count(frame(plan.dimensions.size), "row count")
        val totals = plan.measures.zipWithIndex.map { (m, i) =>
          val start = plan.dimensions.size + 1 + 2 * i
          Total(decimal(frame(start), s"measure ${m.id}"), count(frame(start + 1), s"count ${m.id}"))
        }
        val found = damage.result()
        if found.nonEmpty then errors ++= found
        else plan.finish(key, rows, totals) match
          case Left(es) => errors ++= es.map(e => s"row $row: $e")
          case Right(g) => groups += g
    }
    val found = errors.result()
    if found.nonEmpty then Left(found) else Right(plan.result(groups.result()))

  def execute(using db: Sql): Either[Vector[String], Result] ! Async =
    Writer.foldWith[Chunk[Vector[SqlValue]], Vector[Vector[SqlValue]], Unit, Async](db.query(sql, params))(Vector.empty)(
      (frames, chunk) => frames ++ chunk).map((frames, _) => decode(frames))
