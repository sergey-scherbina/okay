package okay.semantic.data

import okay.codec.{Json, Schema}
import okay.semantic.*

/** Exact decimals are tagged strings, so every JSON consumer can retain their precision. */
object Wire:
  case class Scalar(kind: String, value: String) derives Schema
  case class Predicate(field: String, comparison: String, values: Vector[Scalar]) derives Schema
  case class Sort(field: String, descending: Boolean, nullsFirst: Boolean) derives Schema
  case class Query(metrics: Vector[String], dimensions: Vector[String] = Vector.empty,
                   filters: Vector[Predicate] = Vector.empty, having: Vector[Predicate] = Vector.empty,
                   order: Vector[Sort] = Vector.empty, offset: Int = 0, limit: Option[Int] = None) derives Schema
  case class OutputGroup(key: Vector[Scalar], values: Vector[Option[String]]) derives Schema
  case class Output(source: String, version: String, dimensions: Vector[String], metrics: Vector[String],
                    groups: Vector[OutputGroup]) derives Schema
  case class Definition(id: String, description: String, kind: String, unit: String = "") derives Schema
  case class Description(id: String, source: String, version: String, grain: String,
                         dimensions: Vector[Definition], measures: Vector[Definition], metrics: Vector[Definition], relations: Vector[Definition]) derives Schema

  private def json[A](value: A)(using schema: Schema[A]): Json = Json.parse(Json.encode(schema)(value))

  def scalar(value: Value): Scalar = value match
    case Value.Null => Scalar("null", "")
    case Value.Text(v) => Scalar("text", v)
    case Value.Number(v) => Scalar("number", v.bigDecimal.toPlainString)
    case Value.Bool(v) => Scalar("bool", v.toString)
  def value(s: Scalar): Either[String, Value] = s.kind match
    case "null" if s.value.isEmpty => Right(Value.Null)
    case "text" => Right(Value.Text(s.value))
    case "number" => scala.util.Try(BigDecimal(s.value)).toEither.left.map(_ => "expected exact decimal string").map(Value.Number.apply)
    case "bool" if s.value == "true" => Right(Value.Bool(true))
    case "bool" if s.value == "false" => Right(Value.Bool(false))
    case _ => Left(s"invalid scalar ${s.kind}: ${s.value}")

  private def sequence[A](values: Vector[Either[String, A]]): Either[Vector[String], Vector[A]] =
    val errors = values.flatMap(_.left.toOption)
    if errors.nonEmpty then Left(errors) else Right(values.flatMap(_.toOption))
  private def predicate(p: Predicate): Either[Vector[String], Filter] =
    Comparison.values.find(_.toString == p.comparison) match
      case None => Left(Vector(s"${p.field}: unknown comparison ${p.comparison}"))
      case Some(comparison) =>
        sequence(p.values.map(value)).flatMap { values =>
          if values.isEmpty && !Set(Comparison.IsNull, Comparison.IsNotNull)(comparison) then Left(Vector(s"${p.field}: comparison requires a value"))
          else Right(Filter(p.field, values.headOption.getOrElse(Value.Null), comparison, values.drop(1)))
        }
  private def predicates(ps: Vector[Predicate]): Either[Vector[String], Vector[Filter]] =
    val values = ps.map(predicate)
    val errors = values.flatMap(_.left.toOption.toVector.flatten)
    if errors.nonEmpty then Left(errors) else Right(values.flatMap(_.toOption))

  def request(json: Json): Either[Vector[String], Request] =
    Json.decode(summon[Schema[Query]])(json).left.map(Vector(_)).flatMap { q =>
      for
        filters <- predicates(q.filters)
        hs <- predicates(q.having)
        having <- sequence(hs.map { h =>
          val numeric = h.values.forall {
            case Value.Number(_) | Value.Null => true
            case _ => false
          }
          if !numeric || h.others.contains(Value.Null) then Left(s"having ${h.dimension}: expected numbers; a null member must be first")
          else
            val number = h.equalTo match
              case Value.Number(n) => Some(n)
              case _ => None
            Right(Having(h.dimension, number, h.comparison, h.others.collect { case Value.Number(n) => n }))
        })
      yield Request(q.metrics, q.dimensions, filters, having, q.order.map(o => Order(o.field, o.descending, o.nullsFirst)), q.offset, q.limit)
    }
  def request(value: Request): Json =
    def p(f: Filter): Predicate = Predicate(f.dimension, f.comparison.toString, f.values.map(scalar))
    json(Query(value.metrics, value.dimensions, value.filters.map(p), value.having.map(h => p(h.filter)),
      value.order.map(o => Sort(o.field, o.descending, o.nullsFirst)), value.offset, value.limit))
  def result(value: Result): Json =
    json(Output(value.origin.source, value.origin.version, value.dimensions, value.metrics,
      value.groups.map(g => OutputGroup(g.key.map(scalar), g.values.map(_.map(_.bigDecimal.toPlainString))))))
  def describe[A](model: Model[A]): Json =
    json(Description(model.id, model.origin.source, model.origin.version, model.grain,
      model.dimensions.map(d => Definition(d.id, d.description, d.kind.toString + d.time.fold("")(t => s" ($t)"))),
      model.measures.map(m => Definition(m.id, m.description, "Number")),
      model.metrics.map(m => Definition(m.id, m.description, m.calculation.toString, m.unit)),
      model.relations.map(r => Definition(r.id, r.description, s"${r.from} -> ${r.to} (${r.cardinality})"))))
  def response(result: Either[Vector[String], Result]): Json = result match
    case Left(errors) => failure(errors)
    case Right(value) => Json.JObj(Vector("result" -> Wire.result(value)))
  def failure(errors: Vector[String]): Json = Json.JObj(Vector("errors" -> Json.JArr(errors.map(Json.JStr.apply))))
