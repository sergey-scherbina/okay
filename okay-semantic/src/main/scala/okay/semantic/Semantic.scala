package okay.semantic

/** Business identifiers are independent of Scala names and storage bindings. */
final case class Entity(id: String, description: String)
enum Cardinality:
  case OneToOne, ManyToOne, OneToMany, ManyToMany
final case class Relation(id: String, from: String, to: String,
                          cardinality: Cardinality, description: String)
final class Catalog private (val entities: Vector[Entity], val relations: Vector[Relation])
object Catalog:
  def build(entities: Vector[Entity], relations: Vector[Relation]): Either[Vector[String], Catalog] =
    val ids = entities.map(_.id).toSet
    val errors = Checks.names("entity", entities.map(_.id)) ++
      Checks.names("relation", relations.map(_.id)) ++
      entities.filter(_.description.trim.isEmpty).map(e => s"entity ${e.id}: blank description") ++
      relations.flatMap(r =>
        Vector(Option.when(!ids(r.from))(s"relation ${r.id}: unknown entity ${r.from}"),
          Option.when(!ids(r.to))(s"relation ${r.id}: unknown entity ${r.to}"),
          Option.when(r.description.trim.isEmpty)(s"relation ${r.id}: blank description")).flatten)
    if errors.nonEmpty then Left(errors) else Right(new Catalog(entities, relations))

final case class Origin(source: String, version: String)
enum Kind:
  case Text, Number, Bool
enum Value:
  case Text(value: String)
  case Number(value: BigDecimal)
  case Bool(value: Boolean)
  case Null
  def fits(kind: Kind): Boolean = this match
    case Text(_) => kind == Kind.Text
    case Number(_) => kind == Kind.Number
    case Bool(_) => kind == Kind.Bool
    case Null => true

final case class Dimension[A](id: String, description: String, kind: Kind, read: A => Value)
final case class Measure[A](id: String, description: String, read: A => Option[BigDecimal])
enum Calculation:
  case Sum(measure: String)
  case Count(measure: Option[String] = None)
  case Average(measure: String)
  case Ratio(numerator: String, denominator: String)
  def measures: Vector[String] = this match
    case Sum(m) => Vector(m)
    case Count(m) => m.toVector
    case Average(m) => Vector(m)
    case Ratio(n, d) => Vector(n, d)

final case class Metric(id: String, description: String, unit: String, calculation: Calculation)
final case class Filter(dimension: String, equalTo: Value)
final case class Request(metrics: Vector[String], dimensions: Vector[String] = Vector.empty,
                         filters: Vector[Filter] = Vector.empty)
final case class Group(key: Vector[Value], values: Vector[Option[BigDecimal]])
final case class Result(origin: Origin, dimensions: Vector[String], metrics: Vector[String], groups: Vector[Group])

private[semantic] object Checks:
  def names(what: String, ids: Vector[String]): Vector[String] =
    ids.filter(_.trim.isEmpty).map(_ => s"$what: blank id") ++
      ids.groupMapReduce(identity)(_ => 1)(_ + _).toVector.sortBy(_._1)
        .collect { case (id, n) if n > 1 => s"$what: duplicate $id" }

/** Construct through build: every metric reference is resolved before execution. */
final class Model[A] private (val id: String, val origin: Origin, val grain: String,
                             val dimensions: Vector[Dimension[A]], val measures: Vector[Measure[A]],
                             val metrics: Vector[Metric]):
  def plan(request: Request): Either[Vector[String], Plan[A]] =
    val dimIds = dimensions.map(_.id).toSet
    val metricIds = metrics.map(_.id).toSet
    val errors = Checks.names("selected dimension", request.dimensions) ++
      Checks.names("selected metric", request.metrics) ++
      Option.when(request.metrics.isEmpty)("request: at least one metric required").toVector ++
      request.dimensions.filterNot(dimIds).map(d => s"unknown dimension $d") ++
      request.metrics.filterNot(metricIds).map(m => s"unknown metric $m") ++
      request.filters.flatMap(f => dimensions.find(_.id == f.dimension) match
        case None => Vector(s"filter: unknown dimension ${f.dimension}")
        case Some(d) if !f.equalTo.fits(d.kind) => Vector(s"filter ${d.id}: expected ${d.kind}")
        case _ => Vector.empty)
    if errors.nonEmpty then Left(errors)
    else
      val selectedMetrics = request.metrics.flatMap(n => metrics.find(_.id == n))
      val needed = selectedMetrics.flatMap(_.calculation.measures).distinct
      Right(new Plan(this, request,
        request.dimensions.flatMap(n => dimensions.find(_.id == n)),
        needed.flatMap(n => measures.find(_.id == n)), selectedMetrics))

object Model:
  def build[A](id: String, origin: Origin, grain: String, dimensions: Vector[Dimension[A]],
               measures: Vector[Measure[A]], metrics: Vector[Metric]): Either[Vector[String], Model[A]] =
    val measureIds = measures.map(_.id).toSet
    val errors = Checks.names("dimension", dimensions.map(_.id)) ++
      Checks.names("measure", measures.map(_.id)) ++ Checks.names("metric", metrics.map(_.id)) ++
      Vector("model id" -> id, "source" -> origin.source, "version" -> origin.version, "grain" -> grain)
        .collect { case (n, v) if v.trim.isEmpty => s"$n: blank" } ++
      (dimensions.map(d => d.id -> d.description) ++ measures.map(m => m.id -> m.description) ++
        metrics.map(m => m.id -> m.description)).collect { case (n, d) if d.trim.isEmpty => s"$n: blank description" } ++
      metrics.filter(_.unit.trim.isEmpty).map(m => s"metric ${m.id}: blank unit") ++
      metrics.flatMap(m => m.calculation.measures.filterNot(measureIds).map(n => s"metric ${m.id}: unknown measure $n"))
    if errors.nonEmpty then Left(errors) else Right(new Model(id, origin, grain, dimensions, measures, metrics))

/** Sufficient statistics shared by interpreters. Counts are arbitrary precision. */
final case class Total(sum: Option[BigDecimal], count: BigInt):
  def add(value: Option[BigDecimal]): Total = value match
    case None => this
    case Some(v) => Total(Some(sum.fold(v)(s => BigDecimal(s.bigDecimal.add(v.bigDecimal)))), count + 1)

final class Plan[A] private[semantic] (val model: Model[A], val request: Request,
                                      val dimensions: Vector[Dimension[A]], val measures: Vector[Measure[A]],
                                      val metrics: Vector[Metric]):
  def explain: String =
    val selections = dimensions.map(d => s"dimension ${d.id}: ${d.description}") ++
      metrics.map(m => s"metric ${m.id} (${m.unit}): ${m.description}; ${m.calculation}") ++
      request.filters.map(f => s"filter ${f.dimension} = ${f.equalTo}")
    (Vector(s"model ${model.id}; source ${model.origin.source}; version ${model.origin.version}",
      s"grain: ${model.grain}", "filters before aggregation; nulls ignored; ratios of sums; zero denominator => null") ++ selections).mkString("\n")

  /** Finalize backend statistics without averaging averages or integer division. */
  def finish(key: Vector[Value], rows: BigInt, totals: Vector[Total]): Either[Vector[String], Group] =
    val errors = Option.when(key.size != dimensions.size)("group: dimension arity mismatch").toVector ++
      Option.when(totals.size != measures.size)("group: measure arity mismatch").toVector ++
      Option.when(rows < 0)("group: negative row count").toVector ++
      key.zip(dimensions).collect { case (v, d) if !v.fits(d.kind) => s"dimension ${d.id}: expected ${d.kind}" } ++
      totals.zip(measures).collect { case (t, m) if t.count < 0 || t.count > rows || (t.count == 0) != t.sum.isEmpty =>
        s"measure ${m.id}: inconsistent sum/count" }
    if errors.nonEmpty then Left(errors)
    else
      val byName = measures.map(_.id).zip(totals).toMap
      def divide(n: Option[BigDecimal], d: Option[BigDecimal]): Option[BigDecimal] =
        for x <- n; y <- d if y != 0 yield BigDecimal(x.bigDecimal.divide(y.bigDecimal, java.math.MathContext.DECIMAL128))
      val values = metrics.map(_.calculation match
        case Calculation.Sum(m) => byName(m).sum
        case Calculation.Count(m) => Some(BigDecimal(m.fold(rows)(n => byName(n).count)))
        case Calculation.Average(m) => divide(byName(m).sum, Some(BigDecimal(byName(m).count)))
        case Calculation.Ratio(n, d) => divide(byName(n).sum, byName(d).sum))
      Right(Group(key, values))

  def result(groups: Vector[Group]): Result = Result(model.origin, request.dimensions, request.metrics, groups)

  def run(rows: IterableOnce[A]): Either[Vector[String], Result] =
    val grouped = scala.collection.mutable.LinkedHashMap.empty[Vector[Value], (BigInt, Vector[Total])]
    val empty = Vector.fill(measures.size)(Total(None, BigInt(0)))
    if dimensions.isEmpty then grouped.update(Vector.empty, (BigInt(0), empty))
    val readDimensions = (dimensions ++ request.filters.flatMap(f => model.dimensions.find(_.id == f.dimension))).distinctBy(_.id)
    val errors = Vector.newBuilder[String]
    var rowNumber = BigInt(0)
    val iterator = rows.iterator
    while iterator.hasNext do
      val row = iterator.next()
      val values = readDimensions.map(d => d.id -> d.read(row)).toMap
      val bad = readDimensions.filter(d => !values(d.id).fits(d.kind))
      bad.foreach(d => errors += s"row $rowNumber: dimension ${d.id}: expected ${d.kind}")
      if bad.isEmpty && request.filters.forall(f => values(f.dimension) == f.equalTo) then
        val key = dimensions.map(d => values(d.id))
        val (count, totals) = grouped.getOrElse(key, (BigInt(0), empty))
        grouped.update(key, (count + 1, totals.zip(measures).map((t, m) => t.add(m.read(row)))))
      rowNumber += 1
    val found = errors.result()
    if found.nonEmpty then Left(found)
    else
      val finished = grouped.toVector.map { case (key, (count, totals)) => finish(key, count, totals) }
      val failures = finished.flatMap(_.left.toOption.toVector.flatten)
      if failures.nonEmpty then Left(failures) else Right(result(finished.flatMap(_.toOption)))
