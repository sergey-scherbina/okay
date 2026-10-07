package okay.semantic.sql

import okay.Async
import okay.freer.{!, Chunk, Writer}
import okay.sql.{Sql, SqlValue}
import okay.semantic.*

final case class ColumnRef(column: String, alias: String = "fact")
final case class Join(relation: String, table: String, alias: String, left: ColumnRef,
                      rightKey: String, cardinality: Cardinality = Cardinality.ManyToOne)
/** Bindings contain identifiers and structured transformations, never SQL fragments. */
final case class Binding(table: String, dimensions: Map[String, String], measures: Map[String, String],
                         buckets: Map[String, FixedBucket] = Map.empty, joins: Vector[Join] = Vector.empty,
                         dimensionRefs: Map[String, ColumnRef] = Map.empty, measureRefs: Map[String, ColumnRef] = Map.empty, materializedDimensions: Set[String] = Set.empty)

object Render:
  def apply[A](plan: Plan[A], binding: Binding): Either[Vector[String], Statement[A]] =
    val dimIds = plan.readDimensions.map(_.id)
    val measureIds = plan.measures.map(_.id)
    val errors = Vector.newBuilder[String]
    def identifier(value: String, label: String): String =
      if !value.matches("[A-Za-z_][A-Za-z0-9_]*") then errors += s"$label: invalid SQL identifier $value"
      "\"" + value + "\""
    val aliases = scala.collection.mutable.Set("fact")
    val table = identifier(binding.table, "table")
    var from = if binding.joins.isEmpty then table else s"$table AS \"fact\""
    val preflights = Vector.newBuilder[(String, String)]
    def reference(ref: ColumnRef, label: String): String =
      if !aliases(ref.alias) then errors += s"$label: unknown table alias ${ref.alias}"
      val col = identifier(ref.column, label)
      if binding.joins.isEmpty && ref.alias == "fact" then col
      else s"${identifier(ref.alias, label)}.$col"
    plan.model.relations.filterNot(r => binding.joins.exists(_.relation == r.id)).foreach(r => errors += s"relation ${r.id}: missing SQL join binding")
    if binding.joins.map(_.relation).distinct.size != binding.joins.size then errors += "join: duplicate relation binding"
    binding.joins.foreach { join =>
      plan.model.relations.find(_.id == join.relation) match
        case None => errors += s"join ${join.relation}: relation is not declared in the model"
        case Some(r) if r.cardinality != join.cardinality => errors += s"join ${join.relation}: cardinality contradicts the definition"
        case _ => ()
      if join.relation.trim.isEmpty then errors += "join: blank relation"
      if !Set(Cardinality.ManyToOne, Cardinality.OneToOne)(join.cardinality) then errors += s"join ${join.relation}: fanout requires allocation"
      val jt = identifier(join.table, "join table")
      val ja = identifier(join.alias, "join alias")
      val rk = identifier(join.rightKey, "join key")
      val left = reference(join.left, "left key")
      if aliases(join.alias) then errors += s"join ${join.relation}: duplicate alias ${join.alias}"
      if join.cardinality == Cardinality.OneToOne then
        preflights += ((s"SELECT $left FROM $from WHERE $left IS NOT NULL GROUP BY $left HAVING COUNT(*) > 1", s"relation ${join.relation}: duplicate left key"))
      preflights += ((s"SELECT $rk FROM $jt WHERE $rk IS NOT NULL GROUP BY $rk HAVING COUNT(*) > 1", s"relation ${join.relation}: duplicate right key"))
      aliases += join.alias
      from += s" LEFT JOIN $jt AS $ja ON $left = $ja.$rk"
    }
    def column(id: String, columns: Map[String, String], refs: Map[String, ColumnRef], label: String): String =
      refs.get(id).orElse(columns.get(id).map(ColumnRef(_))) match
        case None => errors += s"$label $id: missing SQL binding"; ""
        case Some(ref) => reference(ref, s"$label $id")
    val dims = dimIds.map { id =>
      val col = column(id, binding.dimensions, binding.dimensionRefs, "dimension")
      val definition = plan.readDimensions.find(_.id == id).flatMap(_.time)
      val bucket = definition match
        case Some(TimeTransform.Fixed(width, anchor)) =>
          if binding.buckets.get(id).exists(b => b.widthMicros != width || b.anchorMicros != anchor) then errors += s"dimension $id: bucket binding contradicts definition"
          FixedBucket.build(width, anchor).toOption
        case Some(TimeTransform.Civil(grain, zone)) =>
          if !binding.materializedDimensions(id) then errors += s"dimension $id: materialize $grain in $zone or use the row interpreter"
          None
        case None => binding.buckets.get(id)
      val transformed = if binding.materializedDimensions(id) then col else bucket.fold(col)(b =>
        s"(FLOOR((CAST($col AS NUMERIC(38,0)) - ${b.anchorMicros}) / ${b.widthMicros}) * ${b.widthMicros} + ${b.anchorMicros})")
      id -> transformed
    }.toMap
    val measures = measureIds.map(id => id -> column(id, binding.measures, binding.measureRefs, "measure")).toMap
    val params = Vector.newBuilder[SqlValue]
    def value(v: Value): String =
      params += (v match
        case Value.Text(x) => SqlValue.Text(x)
        case Value.Number(x) => SqlValue.Num(x)
        case Value.Bool(x) => SqlValue.Bool(x)
        case Value.Null => SqlValue.Null)
      "?"
    val filters = plan.request.filters.map { f =>
      val col = dims(f.dimension)
      f.comparison match
        case Comparison.IsNull => s"$col IS NULL"
        case Comparison.IsNotNull => s"$col IS NOT NULL"
        case Comparison.Eq if f.equalTo == Value.Null => s"$col IS NULL"
        case Comparison.Ne if f.equalTo == Value.Null => s"$col IS NOT NULL"
        case Comparison.In =>
          val present = f.values.filterNot(_ == Value.Null)
          val clauses = Option.when(present.nonEmpty)(s"$col IN (${present.map(value).mkString(", ")})").toVector ++
            Option.when(f.values.contains(Value.Null))(s"$col IS NULL").toVector
          clauses.mkString("(", " OR ", ")")
        case other =>
          val op = other match
            case Comparison.Eq => "="
            case Comparison.Ne => "<>"
            case Comparison.Lt => "<"
            case Comparison.Le => "<="
            case Comparison.Gt => ">"
            case Comparison.Ge => ">="
            case _ => "="
          s"$col $op ${value(f.equalTo)}"
    }
    val found = errors.result()
    if found.nonEmpty then Left(found)
    else
      val groupColumns = plan.dimensions.map(d => dims(d.id))
      val select = groupColumns ++ Vector("COUNT(*)") ++ measureIds.flatMap(id =>
        Vector(s"SUM(${measures(id)})", s"COUNT(${measures(id)})", s"MIN(${measures(id)})", s"MAX(${measures(id)})")) ++
        plan.distinctDimensions.map(d => s"COUNT(DISTINCT ${dims(d.id)})")
      val where = if filters.isEmpty then "" else " WHERE " + filters.mkString(" AND ")
      val group = if groupColumns.isEmpty then "" else " GROUP BY " + groupColumns.mkString(", ")
      Right(new Statement(plan, s"SELECT ${select.mkString(", ")} FROM $from$where$group", params.result(), preflights.result()))

final class Statement[A] private[sql] (val plan: Plan[A], val sql: String, val params: Vector[SqlValue],
                                      val preflights: Vector[(String, String)]):
  def decode(frames: Vector[Vector[SqlValue]]): Either[Vector[String], Result] =
    val errors = Vector.newBuilder[String]
    val groups = Vector.newBuilder[Group]
    val width = plan.dimensions.size + 1 + plan.measures.size * 4 + plan.distinctDimensions.size
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
          val start = plan.dimensions.size + 1 + 4 * i
          Total(decimal(frame(start), s"measure ${m.id}"), count(frame(start + 1), s"count ${m.id}"),
            decimal(frame(start + 2), s"minimum ${m.id}"), decimal(frame(start + 3), s"maximum ${m.id}"))
        }
        val distinctStart = plan.dimensions.size + 1 + plan.measures.size * 4
        val distinct = plan.distinctDimensions.zipWithIndex.map((d, i) => count(frame(distinctStart + i), s"distinct ${d.id}"))
        val found = damage.result()
        if found.nonEmpty then errors ++= found
        else plan.finish(key, rows, totals, distinct) match
          case Left(es) => errors ++= es.map(e => s"row $row: $e")
          case Right(g) => groups += g
    }
    val found = errors.result()
    if found.nonEmpty then Left(found) else Right(plan.result(groups.result()))

  def execute(using db: Sql): Either[Vector[String], Result] ! Async =
    def aggregate: Either[Vector[String], Result] ! Async =
      Writer.foldWith[Chunk[Vector[SqlValue]], Vector[Vector[SqlValue]], Unit, Async](db.query(sql, params))(Vector.empty)(
        (frames, chunk) => frames ++ chunk).map((frames, _) => decode(frames))
    // Build the preflight chain iteratively; effect interpretation trampolines its binds.
    val checked = preflights.foldLeft(okay.freer.pure[Async, Either[Vector[String], Unit]](Right(()))) { (program, check) =>
      program.flatMap {
        case bad @ Left(_) => okay.freer.pure(bad)
        case Right(_) =>
          Writer.foldWith[Chunk[Vector[SqlValue]], Boolean, Unit, Async](db.query(check._1))(false)(
            (found, chunk) => found || chunk.nonEmpty).map((found, _) =>
              if found then Left(Vector(check._2)) else Right(()))
      }
    }
    checked.flatMap {
      case Left(es) => okay.freer.pure(Left(es))
      case Right(_) => aggregate
    }
