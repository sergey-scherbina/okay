package okay.semantic

class TestSemantic extends okay.testkit.Munit.Diagnosed:
  case class Sale(segment: String, month: String, revenue: Option[BigDecimal], cost: Option[BigDecimal])
  val origin = Origin("sales", "v1")
  val dimensions = Vector(
    Dimension[Sale]("segment", "Customer segment", Kind.Text, s => Value.Text(s.segment)),
    Dimension[Sale]("month", "Recognition month", Kind.Text, s => Value.Text(s.month)))
  val measures = Vector(Measure[Sale]("revenue", "Recognized revenue EUR", _.revenue),
    Measure[Sale]("cost", "Cost EUR", _.cost))
  val metrics = Vector(
    Metric("revenue", "Revenue", "EUR", Calculation.Sum("revenue")),
    Metric("rows", "Sales", "rows", Calculation.Count()),
    Metric("paid", "Non-null sales", "rows", Calculation.Count(Some("revenue"))),
    Metric("average", "Average sale", "EUR", Calculation.Average("revenue")),
    Metric("ratio", "Revenue/cost", "ratio", Calculation.Ratio("revenue", "cost")))
  def model = Model.build("sales", origin, "one sale", dimensions, measures, metrics).toOption.get
  val rows = Vector(Sale("A", "2026-01", Some(BigDecimal("0.1")), Some(BigDecimal(1))),
    Sale("A", "2026-01", Some(BigDecimal("0.2")), Some(BigDecimal(9))),
    Sale("A", "2026-01", None, None), Sale("B", "2026-02", Some(BigDecimal(0)), Some(BigDecimal(0))))
  def run(request: Request, input: Vector[Sale] = rows) =
    val plan = model.plan(request).toOption.get
    note(plan.explain)
    plan.run(input).toOption.get

  test("exact sums, non-null averages and ratio of sums") {
    val result = run(Request(metrics.map(_.id), Vector("segment")))
    assertEquals(result.groups.head.values, Vector(Some(BigDecimal("0.3")), Some(BigDecimal(3)),
      Some(BigDecimal(2)), Some(BigDecimal("0.15")), Some(BigDecimal("0.03"))))
    assertEquals(result.groups(1).values.last, None)
    assertEquals(result.origin, origin)
  }
  test("filters before grouping, stable keys in requested dimension order") {
    val result = run(Request(Vector("rows"), Vector("month", "segment"), Vector(Filter("segment", Value.Text("A")))))
    assertEquals(result.groups, Vector(Group(Vector(Value.Text("2026-01"), Value.Text("A")), Vector(Some(BigDecimal(3))))))
  }
  test("empty global aggregation and empty grouped aggregation") {
    assertEquals(run(Request(metrics.map(_.id)), Vector.empty).groups.head.values,
      Vector(None, Some(BigDecimal(0)), Some(BigDecimal(0)), None, None))
    assertEquals(run(Request(Vector("rows"), Vector("segment")), Vector.empty).groups, Vector.empty)
    assertEquals(run(Request(Vector("revenue", "average", "ratio")), Vector(rows(2))).groups.head.values,
      Vector(None, None, None))
  }
  test("catalog validates ids, descriptions and endpoints") {
    note("Relation endpoints must exist; cardinality alone never enables a join")
    assert(Catalog.build(Vector(Entity("sale", "Sale")), Vector(Relation("owner", "sale", "customer", Cardinality.ManyToOne, "Owner"))).isLeft)
    assert(Catalog.build(Vector(Entity("x", "X"), Entity("x", "X")), Vector.empty).isLeft)
    assert(Catalog.build(Vector(Entity("", "")), Vector.empty).isLeft)
    assert(Catalog.build(Vector(Entity("sale", "Sale"), Entity("customer", "Customer")),
      Vector(Relation("owner", "sale", "customer", Cardinality.ManyToOne, "Owner"))).isRight)
  }
  test("definitions and requests fail with named errors") {
    note("All metadata and references are checked before touching rows")
    assert(Model.build("sales", origin, "grain", dimensions ++ dimensions, measures, metrics).isLeft)
    assert(Model.build("sales", origin, "grain", dimensions, measures,
      Vector(Metric("x", "X", "EUR", Calculation.Sum("missing")))).left.toOption.get.exists(_.contains("missing")))
    assert(Model.build("", Origin("", ""), "", dimensions, measures, metrics).isLeft)
    Vector(Request(Vector.empty), Request(Vector("missing")), Request(Vector("rows", "rows")),
      Request(Vector("rows"), Vector("missing")), Request(Vector("rows"), Vector("month", "month")),
      Request(Vector("rows"), filters = Vector(Filter("segment", Value.Number(BigDecimal(1))))),
      Request(Vector("rows"), filters = Vector(Filter("missing", Value.Null))))
      .foreach(r => assert(model.plan(r).isLeft))
  }
  test("bad dimension extractor and bad backend statistics are diagnosed") {
    val bad = Model.build("sales", origin, "sale", Vector(Dimension[Sale]("bad", "Bad", Kind.Text, _ => Value.Bool(true))), measures, metrics).toOption.get
    val plan = bad.plan(Request(Vector("rows"), Vector("bad"))).toOption.get
    note(plan.explain)
    assert(plan.run(rows).left.toOption.get.head.contains("row 0"))
    assert(plan.finish(Vector.empty, 0, Vector.empty).isLeft)
    val normal = model.plan(Request(Vector("revenue"))).toOption.get
    assert(normal.finish(Vector.empty, 0, Vector(Total(Some(BigDecimal(1)), BigInt(0)))).isLeft)
  }
  test("null dimensions and null filters; explanation preserves business definitions") {
    val nullable = Model.build("sales", origin, "sale", Vector(Dimension[Sale]("segment", "Nullable segment", Kind.Text, _ => Value.Null)), measures, metrics).toOption.get
    val plan = nullable.plan(Request(Vector("rows"), Vector("segment"), Vector(Filter("segment", Value.Null)))).toOption.get
    note(plan.explain)
    assertEquals(plan.run(rows).toOption.get.groups.head.key, Vector(Value.Null))
    assert(plan.explain.contains("v1")); assert(plan.explain.contains("Nullable segment"))
  }
  test("large input folds without recursive stack growth") {
    val plan = model.plan(Request(Vector("rows"))).toOption.get
    note(plan.explain)
    assertEquals(plan.run(Iterator.fill(200000)(rows.head)).toOption.get.groups.head.values, Vector(Some(BigDecimal(200000))))
  }

  test("sums preserve more than 34 digits and divisions have a fixed context") {
    val large = BigDecimal("1234567890123456789012345678901234567890")
    val result = run(Request(Vector("revenue")), Vector(rows.head.copy(revenue = Some(large)), rows.head.copy(revenue = Some(BigDecimal(1)))))
    note(s"large decimal sum: ${result.groups}")
    assertEquals(result.groups.head.values, Vector(Some(BigDecimal("1234567890123456789012345678901234567891"))))
    val third = run(Request(Vector("ratio")), Vector(rows.head.copy(revenue = Some(BigDecimal(1)), cost = Some(BigDecimal(3)))))
    assertEquals(third.groups.head.values, Vector(Some(BigDecimal("0.3333333333333333333333333333333333"))))
  }
