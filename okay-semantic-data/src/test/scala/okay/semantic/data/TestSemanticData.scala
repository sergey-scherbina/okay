package okay.semantic.data


import okay.{Bulk, Chunks, Csv, Tables}
import okay.freer.*
import okay.codec.{Schema, Json}
import okay.semantic.*

object SemanticSamples:
  case class Event(segment: String, cents: Long) derives Schema
  val events = Vector(Event("A", 100), Event("B", 200), Event("A", 300))
  val model = Model.build[Event]("events", Origin("events", "1"), "one event",
    Vector(Dimension("segment", "Segment", Kind.Text, e => Value.Text(e.segment))),
    Vector(Measure("amount", "EUR cents", e => Some(BigDecimal(e.cents)))),
    Vector(Metric("sum", "Total", "cent", Calculation.Sum("amount")),
      Metric("avg", "Average", "cent", Calculation.Average("amount")),
      Metric("distinct", "Segments", "count", Calculation.Distinct("segment")))).toOption.get
  val plan = model.plan(Request(Vector("sum", "avg", "distinct"), Vector("segment"))).toOption.get
  val backend: Bulk[Vector] = new Bulk[Vector]:
    def of[A](xs: Iterable[A]): Vector[A] = xs.toVector
    def csv(path: String): Vector[Csv.Row] = Vector.empty
    def map[A, B](xs: Vector[A])(f: A => B): Vector[B] = xs.map(f)
    def flatMap[A, B](xs: Vector[A])(f: A => IterableOnce[B]): Vector[B] = xs.flatMap(f)
    def filter[A](xs: Vector[A])(f: A => Boolean): Vector[A] = xs.filter(f)
    def join[K, A, B](left: Vector[(K, A)], right: Vector[(K, B)]): Vector[(K, (A, B))] =
      val indexed = right.groupMap(_._1)(_._2)
      left.flatMap((k, a) => indexed.getOrElse(k, Vector.empty).map(b => k -> (a, b)))
    def cache[A](xs: Vector[A]): Vector[A] = xs
    def aggregate[A, S, O](xs: Vector[A])(agg: Aggregator[A, S, O]): O =
      val parts = xs.grouped(2).map(_.foldLeft(agg.init)(agg.add)).toVector
      agg.present(parts.foldLeft(agg.init)(agg.merge))
    def toChunks[A](xs: Vector[A]): Chunks[A] = Chunks.fromIterator(xs.iterator)

class TestSemanticData extends okay.testkit.Munit.Diagnosed:
  import SemanticSamples.*
  test("partitioned Bulk and Tables execute the shared aggregation algebra") {
    note(plan.explain)
    val locally = plan.run(events)
    val distributed = Data.bulk(plan, events)(using backend)
    val program = Tables.of(events).flatMap(t => Data.table(plan, t))
    val viaTables = Tables.run(backend)(program)
    assertEquals(distributed, locally)
    assertEquals(viaTables, locally)
    assertEquals(Data.aggregator(plan).run(events), plan.run(events))
  }
  test("CSV has explicit numeric and null bindings, rejects missing and damaged data") {
    type Record = CsvData.Record
    val m = Model.build[Record]("csv", Origin("csv", "1"), "one row",
      Vector(Dimension("segment", "Segment", Kind.Text, _( "segment"))),
      Vector(Measure("amount", "Amount", r => r("amount") match
        case Value.Number(n) => Some(n)
        case _ => None)), Vector(Metric("sum", "Sum", "EUR", Calculation.Sum("amount")))).toOption.get
    val p = m.plan(Request(Vector("sum"), Vector("segment"))).toOption.get
    note(p.explain)
    val fields = Vector(CsvField("segment", Kind.Text), CsvField("amount", Kind.Number, nullable = true))
    val result = CsvData.run(p, Iterator("segment,amount", "A,0.1", "A,0.2", "B,"), fields).toOption.get
    assertEquals(result.groups.head.values, Vector(Some(BigDecimal("0.3"))))
    assertEquals(result.groups.last.values, Vector(None))
    assert(CsvData.run(p, Iterator("segment", "A"), fields).isLeft)
    assert(CsvData.run(p, Iterator("segment,amount", "A,broken"), fields).left.toOption.get.head.contains("amount"))
    assert(CsvData.run(p, Iterator("segment,amount", "A,1,extra"), fields).isLeft)
  }
  test("typed JSON row decoding agrees and retains row errors") {
    note(plan.explain)
    val json = events.map(e => Json.parse(Json.encode(summon[Schema[Event]])(e)))
    assertEquals(Data.json(plan, json), plan.run(events))
    assert(Data.json(plan, json :+ Json.JStr("damaged")).left.toOption.get.head.contains("row 3"))
  }
  test("JSON query wire roundtrip preserves operators and exact decimals") {
    val exact = BigDecimal("12345678901234567890.123456789")
    val r = Request(Vector("sum"), Vector("segment"), Vector(Filter("segment", Value.Text("A"), Comparison.In, Vector(Value.Null))),
      Vector(Having("sum", Some(exact), Comparison.Ge)), Vector(Order("sum", descending = true)), 2, Some(10))
    note(Json.print(Wire.request(r)))
    assertEquals(Wire.request(Wire.request(r)), Right(r))
    assertEquals(Wire.value(Wire.scalar(Value.Number(exact))), Right(Value.Number(exact)))
    assert(Wire.value(Wire.Scalar("number", "NaN")).isLeft)
    assert(Wire.request(Json.JStr("invalid")).isLeft)
    assert(Json.print(Wire.result(plan.run(events).toOption.get)).contains("400"))
  }
  test("broadcast lookups keep facts on Bulk and reject fanout") {
    case class Customer(name: String, tier: String)
    val dim = Dimension[Customer]("tier", "Tier", Kind.Text, c => Value.Text(c.tier))
    val right = Model.build("customer", Origin("CRM", "1"), "customer", Vector(dim), Vector.empty, Vector.empty).toOption.get
    val join = Lookup.build(model, right, Relation("customer", "events", "customer", Cardinality.ManyToOne, "Owner"),
      (e: Event) => Some(e.segment), (c: Customer) => c.name, Vector(dim)).toOption.get
    val customers = Vector(Customer("A", "enterprise"))
    val enriched = Data.lookup(join, events, customers)(using backend).toOption.get
    note(join.model.origin.toString)
    assertEquals(enriched.size, events.size)
    val p = join.model.plan(Request(Vector("sum"), Vector("customer.tier"))).toOption.get
    assertEquals(Data.bulk(p, enriched)(using backend), p.run(enriched))
    assert(Data.lookup(join, events, customers ++ customers)(using backend).isLeft)
  }
