package okay.semantic

/** Business identifiers are independent of Scala names and storage bindings. */
final case class Entity(id: String, description: String)
enum Cardinality:
  case OneToOne, ManyToOne, OneToMany, ManyToMany
final case class Relation(id: String, from: String, to: String,
                          cardinality: Cardinality, description: String)
final class Catalog private (val entities: Vector[Entity], val relations: Vector[Relation]):
  def route(from: String, to: String, via: Vector[String] = Vector.empty): Either[Vector[String], Vector[Relation]] =
    Routes.find(this, from, to, via)
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

object Value:
  def compare(a: Value, b: Value): Option[Int] = (a, b) match
    case (Value.Text(x), Value.Text(y)) => Some(x.compareTo(y))
    case (Value.Number(x), Value.Number(y)) => Some(x.compare(y))
    case (Value.Bool(x), Value.Bool(y)) => Some(java.lang.Boolean.compare(x, y))
    case _ => None

final case class Dimension[A](id: String, description: String, kind: Kind, read: A => Value, time: Option[TimeTransform] = None)
final case class Measure[A](id: String, description: String, read: A => Option[BigDecimal])
enum Calculation:
  case Constant(value: BigDecimal)
  case Sum(measure: String)
  case Count(measure: Option[String] = None)
  case Average(measure: String)
  case Ratio(numerator: String, denominator: String)
  case Minimum(measure: String)
  case Maximum(measure: String)
  case Distinct(dimension: String)
  case Add(left: String, right: String)
  case Subtract(left: String, right: String)
  case Multiply(left: String, right: String)
  case Divide(left: String, right: String)
  case Scale(metric: String, factor: BigDecimal)
  def measures: Vector[String] = this match
    case Sum(m) => Vector(m)
    case Count(m) => m.toVector
    case Average(m) => Vector(m)
    case Ratio(n, d) => Vector(n, d)
    case Minimum(m) => Vector(m)
    case Maximum(m) => Vector(m)
    case _ => Vector.empty
  def dependencies: Vector[String] = this match
    case Add(l, r) => Vector(l, r).distinct
    case Subtract(l, r) => Vector(l, r).distinct
    case Multiply(l, r) => Vector(l, r).distinct
    case Divide(l, r) => Vector(l, r).distinct
    case Scale(m, _) => Vector(m)
    case _ => Vector.empty

final case class Metric(id: String, description: String, unit: String, calculation: Calculation)
enum Comparison:
  case Eq, Ne, Lt, Le, Gt, Ge, In, IsNull, IsNotNull
final case class Filter(dimension: String, equalTo: Value = Value.Null,
                        comparison: Comparison = Comparison.Eq, others: Vector[Value] = Vector.empty):
  def values: Vector[Value] = equalTo +: others
  def accepts(value: Value): Boolean = comparison match
    case Comparison.IsNull => value == Value.Null
    case Comparison.IsNotNull => value != Value.Null
    case Comparison.In => values.contains(value)
    case Comparison.Eq => value == equalTo
    case Comparison.Ne => value != equalTo && (equalTo == Value.Null || value != Value.Null)
    case _ => Value.compare(value, equalTo).exists { c => comparison match
      case Comparison.Lt => c < 0
      case Comparison.Le => c <= 0
      case Comparison.Gt => c > 0
      case Comparison.Ge => c >= 0
      case _ => false }
final case class Having(metric: String, value: Option[BigDecimal], comparison: Comparison = Comparison.Eq,
                        others: Vector[BigDecimal] = Vector.empty):
  def filter: Filter = Filter(metric, value.fold[Value](Value.Null)(Value.Number.apply), comparison, others.map(Value.Number.apply))
final case class Order(field: String, descending: Boolean = false, nullsFirst: Boolean = false)
final case class Request(metrics: Vector[String], dimensions: Vector[String] = Vector.empty,
                         filters: Vector[Filter] = Vector.empty, having: Vector[Having] = Vector.empty,
                         order: Vector[Order] = Vector.empty, offset: Int = 0, limit: Option[Int] = None)
final case class Group(key: Vector[Value], values: Vector[Option[BigDecimal]])
final case class Result(origin: Origin, dimensions: Vector[String], metrics: Vector[String], groups: Vector[Group])

private[semantic] object Checks:
  def names(what: String, ids: Vector[String]): Vector[String] =
    ids.filter(_.trim.isEmpty).map(_ => s"$what: blank id") ++
      ids.groupMapReduce(identity)(_ => 1)(_ + _).toVector.sortBy(_._1)
        .collect { case (id, n) if n > 1 => s"$what: duplicate $id" }
  def filter(f: Filter, kind: Kind): Vector[String] =
    val ordered = Set(Comparison.Lt, Comparison.Le, Comparison.Gt, Comparison.Ge)(f.comparison)
    val active = if Set(Comparison.IsNull, Comparison.IsNotNull)(f.comparison) then Vector.empty else f.values
    active.filterNot(_.fits(kind)).map(_ => s"filter ${f.dimension}: expected $kind") ++
      Option.when(ordered && kind == Kind.Bool)(s"filter ${f.dimension}: Bool is not ordered").toVector ++
      Option.when(f.others.nonEmpty && f.comparison != Comparison.In)(s"filter ${f.dimension}: multiple values require In").toVector

  /** Kahn's algorithm: no recursion even for a deeply derived metric chain. */
  def metricOrder(metrics: Vector[Metric]): Either[Vector[String], Vector[Metric]] =
    val byName = metrics.map(m => m.id -> m).toMap
    val missing = metrics.flatMap(m => m.calculation.dependencies.filterNot(byName.contains).map(n => s"metric ${m.id}: unknown metric $n"))
    if missing.nonEmpty then Left(missing)
    else
      val degrees = scala.collection.mutable.Map.from(metrics.map(m => m.id -> m.calculation.dependencies.size))
      val dependents = metrics.flatMap(m => m.calculation.dependencies.map(_ -> m.id)).groupMap(_._1)(_._2)
      val ready = scala.collection.mutable.Queue.from(metrics.filter(_.calculation.dependencies.isEmpty).map(_.id))
      val out = Vector.newBuilder[Metric]
      var visited = 0
      while ready.nonEmpty do
        val name = ready.dequeue()
        out += byName(name)
        visited += 1
        dependents.getOrElse(name, Vector.empty).foreach { d =>
          degrees(d) -= 1
          if degrees(d) == 0 then ready.enqueue(d)
        }
      if visited != metrics.size then Left(Vector("metric cycle: " + metrics.filter(m => degrees(m.id) > 0).map(_.id).mkString(", ")))
      else Right(out.result())

final class Model[A] private (val id: String, val origin: Origin, val grain: String,
                             val dimensions: Vector[Dimension[A]], val measures: Vector[Measure[A]],
                             val metrics: Vector[Metric], private val ordered: Vector[Metric], val relations: Vector[Relation]) extends Serializable:
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
        case Some(d) => Checks.filter(f, d.kind)) ++
      request.having.flatMap(h => if !request.metrics.contains(h.metric) then Vector(s"having: unselected metric ${h.metric}") else Checks.filter(h.filter, Kind.Number)) ++
      request.order.flatMap(o =>
        val matches = request.dimensions.count(_ == o.field) + request.metrics.count(_ == o.field)
        if matches != 1 then Vector(s"order ${o.field}: expected one selected field, found $matches") else Vector.empty) ++
      Option.when(request.offset < 0 || request.limit.exists(_ < 0))("pagination: negative offset or limit").toVector
    if errors.nonEmpty then Left(errors)
    else
      val needed = scala.collection.mutable.Set.from(request.metrics)
      val todo = scala.collection.mutable.Queue.from(request.metrics)
      val byName = metrics.map(m => m.id -> m).toMap
      while todo.nonEmpty do
        byName(todo.dequeue()).calculation.dependencies.foreach { n =>
          if !needed(n) then { needed += n; todo.enqueue(n) }
        }
      val calculations = ordered.filter(m => needed(m.id))
      val measureIds = calculations.flatMap(_.calculation.measures).distinct
      val distinctIds = calculations.collect { case Metric(_, _, _, Calculation.Distinct(d)) => d }.distinct
      Right(new Plan(this, request,
        request.dimensions.flatMap(n => dimensions.find(_.id == n)),
        measureIds.flatMap(n => measures.find(_.id == n)), request.metrics.map(byName), calculations,
        distinctIds.flatMap(n => dimensions.find(_.id == n))))

object Model:
  def build[A](id: String, origin: Origin, grain: String, dimensions: Vector[Dimension[A]],
               measures: Vector[Measure[A]], metrics: Vector[Metric], relations: Vector[Relation] = Vector.empty): Either[Vector[String], Model[A]] =
    val measureIds = measures.map(_.id).toSet
    val dimIds = dimensions.map(_.id).toSet
    val metricMap = metrics.map(m => m.id -> m).toMap
    val reachable = scala.collection.mutable.Set(id)
    val relationErrors = relations.flatMap { r =>
      val errors = Option.when(!reachable(r.from) || r.to.trim.isEmpty || r.description.trim.isEmpty)(s"relation ${r.id}: invalid model endpoints or description").toVector
      reachable += r.to
      errors
    }
    val errors = Checks.names("model relation", relations.map(_.id)) ++ relationErrors ++
      Checks.names("dimension", dimensions.map(_.id)) ++
      Checks.names("measure", measures.map(_.id)) ++ Checks.names("metric", metrics.map(_.id)) ++
      Vector("model id" -> id, "source" -> origin.source, "version" -> origin.version, "grain" -> grain)
        .collect { case (n, v) if v.trim.isEmpty => s"$n: blank" } ++
      (dimensions.map(d => d.id -> d.description) ++ measures.map(m => m.id -> m.description) ++
        metrics.map(m => m.id -> m.description)).collect { case (n, d) if d.trim.isEmpty => s"$n: blank description" } ++
      dimensions.flatMap(d => d.time.toVector.flatMap { t =>
        Option.when(d.kind != Kind.Number)(s"dimension ${d.id}: temporal keys must be Number").toVector ++ (t match
          case TimeTransform.Fixed(w, _) if w <= 0 => Vector(s"dimension ${d.id}: bucket width must be positive")
          case TimeTransform.Civil(_, z) if z.trim.isEmpty => Vector(s"dimension ${d.id}: blank calendar zone")
          case _ => Vector.empty)
      }) ++
      metrics.filter(_.unit.trim.isEmpty).map(m => s"metric ${m.id}: blank unit") ++
      metrics.flatMap(m => m.calculation.measures.filterNot(measureIds).map(n => s"metric ${m.id}: unknown measure $n")) ++
      metrics.flatMap(m => m.calculation match
        case Calculation.Distinct(d) if !dimIds(d) => Vector(s"metric ${m.id}: unknown dimension $d")
        case Calculation.Add(l, r) => units(m, l, r, metricMap)
        case Calculation.Subtract(l, r) => units(m, l, r, metricMap)
        case _ => Vector.empty)
    if errors.nonEmpty then Left(errors)
    else Checks.metricOrder(metrics).map(order => new Model(id, origin, grain, dimensions, measures, metrics, order, relations))
  private def units(m: Metric, l: String, r: String, all: Map[String, Metric]): Vector[String] =
    (all.get(l), all.get(r)) match
      case (Some(a), Some(b)) if a.unit != b.unit || m.unit != a.unit => Vector(s"metric ${m.id}: incompatible additive units")
      case _ => Vector.empty

final case class Total(sum: Option[BigDecimal], count: BigInt, minimum: Option[BigDecimal] = None,
                       maximum: Option[BigDecimal] = None):
  def add(value: Option[BigDecimal]): Total = value match
    case None => this
    case Some(v) => merge(Total(Some(v), BigInt(1), Some(v), Some(v)))
  def merge(that: Total): Total =
    def both(a: Option[BigDecimal], b: Option[BigDecimal])(f: (BigDecimal, BigDecimal) => BigDecimal): Option[BigDecimal] =
      (a, b) match
        case (Some(x), Some(y)) => Some(f(x, y))
        case _ => a.orElse(b)
    Total(both(sum, that.sum)((a, b) => BigDecimal(a.bigDecimal.add(b.bigDecimal))), count + that.count,
      both(minimum, that.minimum)(_.min(_)), both(maximum, that.maximum)(_.max(_)))

final case class Signature(model: String, origin: Origin, grain: String, request: Request,
                          dimensions: Vector[(String, String, Kind)], measures: Vector[(String, String)], calculations: Vector[Metric],
                          temporal: Vector[(String, TimeTransform)], relations: Vector[Relation])
final case class Statistics(key: Vector[Value], rows: BigInt, totals: Vector[Total], distinct: Vector[Set[Value]])
final case class Partial(signature: Signature, groups: Vector[Statistics], errors: Vector[String])

final class Plan[A] private[semantic] (val model: Model[A], val request: Request,
                                      val dimensions: Vector[Dimension[A]], val measures: Vector[Measure[A]],
                                      val metrics: Vector[Metric], val calculations: Vector[Metric],
                                      val distinctDimensions: Vector[Dimension[A]]) extends Serializable:
  val readDimensions: Vector[Dimension[A]] = (dimensions ++ distinctDimensions ++
    request.filters.flatMap(f => model.dimensions.find(_.id == f.dimension))).distinctBy(_.id)
  val signature: Signature = Signature(model.id, model.origin, model.grain, request,
    readDimensions.map(d => (d.id, d.description, d.kind)), measures.map(m => m.id -> m.description), calculations, readDimensions.flatMap(d => d.time.map(d.id -> _)), model.relations)
  def explain: String =
    val selections = dimensions.map(d => s"dimension ${d.id}: ${d.description}") ++
      calculations.map(m => s"metric ${m.id} (${m.unit}): ${m.description}; ${m.calculation}") ++
      request.filters.map(f => s"filter ${f.dimension} ${f.comparison} ${f.values}") ++
      model.relations.map(r => s"relation ${r.id}: ${r.from} -> ${r.to}, ${r.cardinality}; ${r.description}")
    (Vector(s"model ${model.id}; source ${model.origin.source}; version ${model.origin.version}",
      s"grain: ${model.grain}", "filters before aggregation; nulls ignored; ratios of sums; zero denominator => null",
      s"having ${request.having}; order ${request.order}; offset ${request.offset}; limit ${request.limit}") ++ selections).mkString("\n")

  def finish(key: Vector[Value], rows: BigInt, totals: Vector[Total], distinct: Vector[BigInt] = Vector.empty): Either[Vector[String], Group] =
    val errors = Option.when(key.size != dimensions.size)("group: dimension arity mismatch").toVector ++
      Option.when(totals.size != measures.size)("group: measure arity mismatch").toVector ++
      Option.when(distinct.size != distinctDimensions.size)("group: distinct arity mismatch").toVector ++
      Option.when(rows < 0)("group: negative row count").toVector ++
      key.zip(dimensions).collect { case (v, d) if !v.fits(d.kind) => s"dimension ${d.id}: expected ${d.kind}" } ++
      totals.zip(measures).collect { case (t, m) if t.count < 0 || t.count > rows || (t.count == 0) != t.sum.isEmpty =>
        s"measure ${m.id}: inconsistent sum/count" } ++
      distinct.collect { case n if n < 0 || n > rows => "group: inconsistent distinct count" } ++
      totals.zip(measures).flatMap { (t, m) =>
        val needsMin = calculations.exists(_.calculation == Calculation.Minimum(m.id))
        val needsMax = calculations.exists(_.calculation == Calculation.Maximum(m.id))
        Vector(Option.when(t.count > 0 && needsMin && t.minimum.isEmpty)(s"measure ${m.id}: missing minimum"),
          Option.when(t.count > 0 && needsMax && t.maximum.isEmpty)(s"measure ${m.id}: missing maximum"),
          Option.when(t.minimum.zip(t.maximum).exists((a, b) => a > b))(s"measure ${m.id}: minimum exceeds maximum"),
          Option.when(t.count == 0 && (t.minimum.nonEmpty || t.maximum.nonEmpty))(s"measure ${m.id}: empty count has extrema")).flatten
      }
    if errors.nonEmpty then Left(errors)
    else
      val byName = measures.map(_.id).zip(totals).toMap
      val distinctByName = distinctDimensions.map(_.id).zip(distinct).toMap
      val values = scala.collection.mutable.Map.empty[String, Option[BigDecimal]]
      def divide(n: Option[BigDecimal], d: Option[BigDecimal]): Option[BigDecimal] =
        for x <- n; y <- d if y != 0 yield BigDecimal(x.bigDecimal.divide(y.bigDecimal, java.math.MathContext.DECIMAL128))
      def combine(l: String, r: String)(f: (BigDecimal, BigDecimal) => BigDecimal): Option[BigDecimal] =
        for x <- values(l); y <- values(r) yield f(x, y)
      calculations.foreach { m =>
        val value = m.calculation match
          case Calculation.Constant(n) => Some(n)
          case Calculation.Sum(n) => byName(n).sum
          case Calculation.Count(n) => Some(BigDecimal(n.fold(rows)(v => byName(v).count)))
          case Calculation.Average(n) => divide(byName(n).sum, Some(BigDecimal(byName(n).count)))
          case Calculation.Ratio(n, d) => divide(byName(n).sum, byName(d).sum)
          case Calculation.Minimum(n) => byName(n).minimum
          case Calculation.Maximum(n) => byName(n).maximum
          case Calculation.Distinct(d) => Some(BigDecimal(distinctByName(d)))
          case Calculation.Add(l, r) => combine(l, r)((x, y) => BigDecimal(x.bigDecimal.add(y.bigDecimal)))
          case Calculation.Subtract(l, r) => combine(l, r)((x, y) => BigDecimal(x.bigDecimal.subtract(y.bigDecimal)))
          case Calculation.Multiply(l, r) => combine(l, r)((x, y) => BigDecimal(x.bigDecimal.multiply(y.bigDecimal)))
          case Calculation.Divide(l, r) => divide(values(l), values(r))
          case Calculation.Scale(n, f) => values(n).map(v => BigDecimal(v.bigDecimal.multiply(f.bigDecimal)))
        values.update(m.id, value)
      }
      Right(Group(key, metrics.map(m => values(m.id))))

  def result(groups: Vector[Group]): Result =
    val filtered = groups.filter(g => request.having.forall { h =>
      h.filter.accepts(g.values(request.metrics.indexOf(h.metric)).fold[Value](Value.Null)(Value.Number.apply)) })
    def field(g: Group, name: String): Value =
      val d = request.dimensions.indexOf(name)
      if d >= 0 then g.key(d) else g.values(request.metrics.indexOf(name)).fold[Value](Value.Null)(Value.Number.apply)
    val sorted = if request.order.isEmpty then filtered else filtered.sortWith { (a, b) =>
      var comparison = 0
      val orders = request.order.iterator
      while comparison == 0 && orders.hasNext do
        val o = orders.next()
        val x = field(a, o.field); val y = field(b, o.field)
        if x == Value.Null || y == Value.Null then
          comparison = if x == y then 0 else if (x == Value.Null) == o.nullsFirst then -1 else 1
        else
          comparison = Value.compare(x, y).getOrElse(0)
          if o.descending then comparison = -comparison
      comparison < 0
    }
    val page = sorted.drop(request.offset)
    Result(model.origin, request.dimensions, request.metrics, request.limit.fold(page)(page.take))

  def accumulator: Accumulator[A] = new Accumulator(this)
  def summarize(rows: IterableOnce[A]): Partial =
    val acc = accumulator
    rows.iterator.foreach(acc.add)
    acc.snapshot
  def run(rows: IterableOnce[A]): Either[Vector[String], Result] =
    val acc = accumulator
    rows.iterator.foreach(acc.add)
    acc.result

final class Accumulator[A] private[semantic] (val plan: Plan[A]) extends Serializable:
  private val grouped = scala.collection.mutable.LinkedHashMap.empty[Vector[Value], Statistics]
  private val errors = scala.collection.mutable.ArrayBuffer.empty[String]
  private var rowNumber = BigInt(0)
  private def empty(key: Vector[Value]): Statistics = Statistics(key, BigInt(0),
    Vector.fill(plan.measures.size)(Total(None, BigInt(0))), Vector.fill(plan.distinctDimensions.size)(Set.empty))
  if plan.dimensions.isEmpty then grouped.update(Vector.empty, empty(Vector.empty))
  def add(row: A): Unit =
    val values = plan.readDimensions.map(d => d.id -> d.read(row)).toMap
    val bad = plan.readDimensions.filter(d => !values(d.id).fits(d.kind))
    bad.foreach(d => errors += s"row $rowNumber: dimension ${d.id}: expected ${d.kind}")
    if bad.isEmpty && plan.request.filters.forall(f => f.accepts(values(f.dimension))) then
      val key = plan.dimensions.map(d => values(d.id))
      val old = grouped.getOrElse(key, empty(key))
      val totals = old.totals.zip(plan.measures).map((t, m) => t.add(m.read(row)))
      val distinct = old.distinct.zip(plan.distinctDimensions).map { (set, d) =>
        if values(d.id) == Value.Null then set else set + values(d.id) }
      grouped.update(key, Statistics(key, old.rows + 1, totals, distinct))
    rowNumber += 1
  def snapshot: Partial = Partial(plan.signature, grouped.values.toVector, errors.toVector)
  def merge(partial: Partial): Either[Vector[String], Unit] =
    if partial.signature != plan.signature then Left(Vector("partial: incompatible model, version, definitions or request"))
    else
      val invalid = partial.groups.flatMap { s =>
        plan.finish(s.key, s.rows, s.totals, s.distinct.map(v => BigInt(v.size))).left.toOption.toVector.flatten ++
          s.distinct.zip(plan.distinctDimensions).flatMap { (values, d) =>
            Option.when(values.exists(v => v == Value.Null || !v.fits(d.kind)))(s"partial: invalid distinct values for ${d.id}").toVector }
      }
      if invalid.nonEmpty then Left(invalid)
      else
        partial.groups.foreach { s =>
          val old = grouped.getOrElse(s.key, empty(s.key))
          grouped.update(s.key, Statistics(s.key, old.rows + s.rows, old.totals.zip(s.totals).map((a, b) => a.merge(b)),
            old.distinct.zip(s.distinct).map(_ union _)))
        }
        errors ++= partial.errors
        Right(())
  def result: Either[Vector[String], Result] =
    if errors.nonEmpty then Left(errors.toVector)
    else
      val finished = grouped.values.toVector.map(s => plan.finish(s.key, s.rows, s.totals, s.distinct.map(v => BigInt(v.size))))
      val failures = finished.flatMap(_.left.toOption.toVector.flatten)
      if failures.nonEmpty then Left(failures) else Right(plan.result(finished.flatMap(_.toOption)))
