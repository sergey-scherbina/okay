package okay.semantic.data

import okay.{!, Async, Aggregator, Bulk, Chunk, Source, Tables, Writer}
import okay.codec.{Json, Schema}
import okay.semantic.*

object Data:
  final case class State[A](accumulator: Accumulator[A], errors: Vector[String] = Vector.empty)
  def aggregator[A](plan: Plan[A]): Aggregator[A, State[A], Either[Vector[String], Result]] = new:
    def init: State[A] = State(plan.accumulator)
    def add(state: State[A], row: A): State[A] =
      state.accumulator.add(row)
      state
    def merge(left: State[A], right: State[A]): State[A] =
      val errors = left.accumulator.merge(right.accumulator.snapshot).left.toOption.toVector.flatten
      left.copy(errors = left.errors ++ right.errors ++ errors)
    def present(state: State[A]): Either[Vector[String], Result] =
      if state.errors.nonEmpty then Left(state.errors) else state.accumulator.result

  def source[A](plan: Plan[A], rows: Source[A]): Either[Vector[String], Result] ! Async =
    Writer.loopWith[A, Accumulator[A], Unit, Either[Vector[String], Result], Async](rows)(plan.accumulator)(
      (acc, row) => { acc.add(row); acc })((acc, _) => acc.result)
  def chunks[A](plan: Plan[A], rows: Source[Chunk[A]]): Either[Vector[String], Result] ! Async =
    Writer.loopWith[Chunk[A], Accumulator[A], Unit, Either[Vector[String], Result], Async](rows)(plan.accumulator)(
      (acc, chunk) => { chunk.foreach(acc.add); acc })((acc, _) => acc.result)
  def bulk[D[_], A](plan: Plan[A], rows: D[A])(using backend: Bulk[D]): Either[Vector[String], Result] =
    backend.aggregate(rows)(aggregator(plan))
  def table[A](plan: Plan[A], rows: Tables.Table[A]): Either[Vector[String], Result] ! Tables =
    rows.aggregate(aggregator(plan))
  def file[D[_], A](plan: Plan[A], path: String, format: Bulk.Format[A])(using backend: Bulk[D]): Either[Vector[String], Result] =
    bulk(plan, backend.read(path, format))
  /** Broadcast dimension lookup: facts remain on the platform; only dimension rows are local. */
  def lookup[D[_], A, B, K](join: Lookup[A, B, K], left: D[A], right: D[B])(using backend: Bulk[D])
      : Either[Vector[String], D[(A, Option[B])]] =
    import okay.Chunks.elements
    join.index(backend.toChunks(right).elements).flatMap { indexed =>
      val errors = if join.relation.cardinality != Cardinality.OneToOne then Vector.empty else
        val counts = backend.aggregate(left)(new Aggregator[A, Map[K, BigInt], Map[K, BigInt]]:
          def init: Map[K, BigInt] = Map.empty
          def add(acc: Map[K, BigInt], row: A): Map[K, BigInt] = join.leftKey(row).fold(acc)(k => acc.updated(k, acc.getOrElse(k, BigInt(0)) + 1))
          def merge(a: Map[K, BigInt], b: Map[K, BigInt]): Map[K, BigInt] = b.foldLeft(a)((m, pair) => m.updated(pair._1, m.getOrElse(pair._1, BigInt(0)) + pair._2))
          def present(acc: Map[K, BigInt]): Map[K, BigInt] = acc)
        counts.collect { case (k, n) if n > 1 => s"relation ${join.relation.id}: duplicate left key $k" }.toVector
      if errors.nonEmpty then Left(errors) else Right(backend.map(left)(row => join.enrich(row, indexed)))
    }
  def json[A](plan: Plan[A], rows: IterableOnce[Json])(using schema: Schema[A]): Either[Vector[String], Result] =
    val acc = plan.accumulator
    val errors = Vector.newBuilder[String]
    rows.iterator.zipWithIndex.foreach { (j, i) => Json.decode(schema)(j) match
      case Left(e) => errors += s"JSON row $i: $e"
      case Right(a) => acc.add(a)
    }
    val found = errors.result()
    if found.nonEmpty then Left(found) else acc.result

/** Explicit CSV scalar bindings; empty cells are null only when declared nullable. */
final case class CsvField(name: String, kind: Kind, nullable: Boolean = false)
object CsvData:
  type Record = Map[String, Value]
  def run(plan: Plan[Record], lines: Iterator[String], fields: Vector[CsvField]): Either[Vector[String], Result] =
    val errors = Vector.newBuilder[String]
    val names = fields.map(_.name)
    if names.distinct.size != names.size || names.exists(_.trim.isEmpty) then errors += "CSV binding: duplicate or blank field"
    if !lines.hasNext then
      val found = errors.result()
      if found.nonEmpty then Left(found) else plan.run(Vector.empty)
    else
      val header = okay.Csv.fields(lines.next().stripPrefix("﻿"))
      if header.distinct.size != header.size then errors += "CSV header: duplicate field"
      fields.filterNot(f => header.contains(f.name)).foreach(f => errors += s"CSV header: missing ${f.name}")
      val initial = errors.result()
      if initial.nonEmpty then Left(initial)
      else
        val acc = plan.accumulator
        val damage = Vector.newBuilder[String]
        lines.zipWithIndex.foreach { (line, row) =>
          val cells = okay.Csv.fields(line)
          if cells.size != header.size then damage += s"CSV row $row: expected ${header.size} cells, found ${cells.size}"
          else
            val parsed = fields.map { f =>
              val raw = cells(header.indexOf(f.name))
              val value: Either[String, Value] =
                if raw.isEmpty && f.nullable then Right(Value.Null)
                else f.kind match
                  case Kind.Text => Right(Value.Text(raw))
                  case Kind.Number => scala.util.Try(BigDecimal(raw)).toEither.left.map(_ => "expected decimal").map(Value.Number.apply)
                  case Kind.Bool => raw match
                    case "true" => Right(Value.Bool(true))
                    case "false" => Right(Value.Bool(false))
                    case _ => Left("expected true or false")
              f.name -> value
            }
            val bad = parsed.collect { case (name, Left(why)) => s"CSV row $row, $name: $why" }
            damage ++= bad
            if bad.isEmpty then acc.add(parsed.collect { case (name, Right(value)) => name -> value }.toMap)
        }
        val found = damage.result()
        if found.nonEmpty then Left(found) else acc.result
