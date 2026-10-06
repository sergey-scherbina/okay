package okay.semantic

class TestSemanticLayer extends okay.testkit.Munit.Diagnosed:
  case class Sale(segment: Option[String], amount: Option[BigDecimal], cost: Option[BigDecimal], at: Long)
  val dims = Vector(Dimension[Sale]("segment", "Segment", Kind.Text, s => s.segment.fold[Value](Value.Null)(Value.Text.apply)),
    Dimension[Sale]("at", "Epoch microseconds", Kind.Number, s => Value.Number(BigDecimal(s.at))))
  val measures = Vector(Measure[Sale]("amount", "Revenue EUR", _.amount), Measure[Sale]("cost", "Cost EUR", _.cost))
  val metrics = Vector(Metric("revenue", "Revenue", "EUR", Calculation.Sum("amount")),
    Metric("cost", "Cost", "EUR", Calculation.Sum("cost")),
    Metric("profit", "Profit", "EUR", Calculation.Subtract("revenue", "cost")),
    Metric("margin", "Margin", "ratio", Calculation.Divide("profit", "revenue")),
    Metric("minimum", "Minimum", "EUR", Calculation.Minimum("amount")),
    Metric("maximum", "Maximum", "EUR", Calculation.Maximum("amount")),
    Metric("segments", "Segments", "count", Calculation.Distinct("segment")),
    Metric("average", "Average", "EUR", Calculation.Average("amount")))
  def model = Model.build("sales", Origin("ledger", "2"), "one sale", dims, measures, metrics).toOption.get
  val rows = Vector(Sale(Some("B"), Some(BigDecimal(10)), Some(BigDecimal(2)), -1),
    Sale(Some("A"), Some(BigDecimal(20)), Some(BigDecimal(8)), 0),
    Sale(Some("A"), Some(BigDecimal(30)), Some(BigDecimal(10)), 10), Sale(None, None, None, 20))
  def plan(request: Request) =
    val p = model.plan(request).toOption.get
    note(p.explain)
    p
  test("derived metrics aggregate first, extremes and distinct exclude null") {
    val result = plan(Request(Vector("profit", "margin", "minimum", "maximum", "segments"))).run(rows).toOption.get
    assertEquals(result.groups.head.values, Vector(Some(BigDecimal(40)),
      Some(BigDecimal("0.6666666666666666666666666666666667")), Some(BigDecimal(10)), Some(BigDecimal(30)), Some(BigDecimal(2))))
  }
  test("unknown references, cycles and incompatible additive units are rejected") {
    note("Graph validation is iterative and runs before planning")
    def build(ms: Vector[Metric]) = Model.build("sales", Origin("x", "1"), "one sale", dims, measures, ms)
    assert(build(Vector(Metric("bad", "Bad", "EUR", Calculation.Add("missing", "missing")))).isLeft)
    assert(build(Vector(Metric("a", "A", "EUR", Calculation.Scale("b", BigDecimal(1))), Metric("b", "B", "EUR", Calculation.Scale("a", BigDecimal(1))))).isLeft)
    assert(build(metrics :+ Metric("bad", "Bad", "EUR", Calculation.Add("revenue", "margin"))).isLeft)
  }
  test("deep dependency chains are stack safe") {
    val chain = Metric("m0", "Base", "EUR", Calculation.Sum("amount")) +:
      Vector.tabulate(20000)(i => Metric(s"m${i + 1}", "Derived", "EUR", Calculation.Scale(s"m$i", BigDecimal(1))))
    val m = Model.build("sales", Origin("x", "1"), "one sale", dims, measures, chain).toOption.get
    note(s"metric chain length ${chain.size}")
    assertEquals(m.plan(Request(Vector("m20000"))).toOption.get.run(rows).toOption.get.groups.head.values, Vector(Some(BigDecimal(60))))
  }
  test("comparisons, having, sort, null placement and pagination") {
    val request = Request(Vector("revenue"), Vector("segment"),
      filters = Vector(Filter("at", Value.Number(BigDecimal(0)), Comparison.Ge)),
      having = Vector(Having("revenue", Some(BigDecimal(40)), Comparison.Gt)), order = Vector(Order("revenue", descending = true)), limit = Some(1))
    assertEquals(plan(request).run(rows).toOption.get.groups.head.key, Vector(Value.Text("A")))
    assertEquals(plan(Request(Vector("revenue"), Vector("segment"), order = Vector(Order("segment", nullsFirst = true)))).run(rows).toOption.get.groups.head.key, Vector(Value.Null))
    assertEquals(plan(Request(Vector("revenue"), filters = Vector(Filter("segment", Value.Text("A"), Comparison.In, Vector(Value.Null))))).run(rows).toOption.get.groups.head.values, Vector(Some(BigDecimal(50))))
    assert(model.plan(Request(Vector("revenue"), offset = -1)).isLeft)
    assert(model.plan(Request(Vector("revenue"), having = Vector(Having("cost", Some(BigDecimal(1)))))).isLeft)
    assert(model.plan(Request(Vector("revenue"), order = Vector(Order("unknown")))).isLeft)
    assert(model.plan(Request(Vector("revenue"), filters = Vector(Filter("at", Value.Text("bad"), Comparison.Lt)))).isLeft)
  }
  test("all filter comparisons have explicit null behavior") {
    note("SQL-style three-valued ordered comparisons, explicit null equality")
    val one = Value.Number(BigDecimal(1)); val two = Value.Number(BigDecimal(2))
    assert(Filter("x", two, Comparison.Lt).accepts(one))
    assert(Filter("x", one, Comparison.Le).accepts(one))
    assert(Filter("x", one, Comparison.Gt).accepts(two))
    assert(Filter("x", one, Comparison.Ge).accepts(one))
    assert(Filter("x", one, Comparison.Ne).accepts(two))
    assert(!Filter("x", one, Comparison.Ne).accepts(Value.Null))
    assert(Filter("x", comparison = Comparison.IsNull).accepts(Value.Null))
    assert(Filter("x", comparison = Comparison.IsNotNull).accepts(one))
    assert(!Filter("x", one, Comparison.Lt).accepts(Value.Null))
  }
  test("partition merges equal one pass and snapshots do not alias") {
    val p = plan(Request(Vector("average", "margin", "segments"), Vector("segment")))
    val acc = p.accumulator
    val saved = acc.snapshot
    rows.grouped(2).foreach(part => assert(acc.merge(p.summarize(part)).isRight))
    assertEquals(acc.result, p.run(rows))
    assertEquals(saved.groups, Vector.empty)
    val other = model.plan(Request(Vector("revenue"))).toOption.get
    assert(acc.merge(other.summarize(rows)).isLeft)
    val empty = p.accumulator
    assert(empty.merge(saved).isRight)
  }
  test("fixed buckets and half-open windows include pre-epoch and overflow edges") {
    val b = FixedBucket.build(10L).toOption.get
    note(s"fixed bucket ${b.widthMicros}")
    assertEquals(b.start(-1), BigInt(-10))
    assertEquals(b.start(0), BigInt(0))
    assertEquals(b.start(10), BigInt(10))
    assert(FixedBucket.build(0).isLeft)
    assert(Time.window("at", 10, 0).isLeft)
    val request = Request(Vector("revenue"), filters = Time.window("at", 0, 10).toOption.get)
    assertEquals(plan(request).run(rows).toOption.get.groups.head.values, Vector(Some(BigDecimal(20))))
    assert(FixedBucket.build(Long.MaxValue, Long.MinValue).toOption.get.start(Long.MaxValue) <= BigInt(Long.MaxValue))
  }
  test("safe lookup retains unmatched facts, checks keys and records both origins") {
    case class Customer(id: Int, segment: String)
    val dimension = Dimension[Customer]("segment", "Customer segment", Kind.Text, c => Value.Text(c.segment))
    val customer = Model.build("customer", Origin("CRM", "3"), "one customer", Vector(dimension), Vector.empty, Vector.empty).toOption.get
    val relation = Relation("customer", "sales", "customer", Cardinality.ManyToOne, "Sold to")
    val lookup = Lookup.build(model, customer, relation, (s: Sale) => s.segment.map(_.head.toInt), (c: Customer) => c.id, Vector(dimension)).toOption.get
    val right = Vector(Customer('A'.toInt, "enterprise"))
    val enriched = lookup.rows(rows, right).toOption.get
    assertEquals(enriched.size, rows.size)
    assertEquals(enriched.head._2, None)
    val p = lookup.model.plan(Request(Vector("revenue"), Vector("customer.segment"))).toOption.get
    note(p.explain)
    assertEquals(p.run(enriched).toOption.get.groups.size, 2)
    assert(p.model.origin.version.contains("3"))
    assert(lookup.rows(rows, right ++ right).isLeft)
    assert(Lookup.build(model, customer, relation.copy(cardinality = Cardinality.OneToMany), (s: Sale) => s.segment.map(_.head.toInt), (c: Customer) => c.id, Vector(dimension)).isLeft)
    val unique = Lookup.build(model, customer, relation.copy(cardinality = Cardinality.OneToOne), (s: Sale) => s.segment.map(_.head.toInt), (c: Customer) => c.id, Vector(dimension)).toOption.get
    assert(unique.rows(rows, right).isLeft)
  }
  test("catalog routes reject ambiguity, cycles and fanout, with explicit disambiguation") {
    val entities = Vector("sale", "customer", "country").map(n => Entity(n, n))
    val owner = Relation("owner", "sale", "customer", Cardinality.ManyToOne, "Owner")
    val country = Relation("country", "customer", "country", Cardinality.ManyToOne, "Country")
    val catalog = Catalog.build(entities, Vector(owner, country)).toOption.get
    note("Routes count paths in a DAG; no exponential path enumeration")
    assertEquals(catalog.route("sale", "country"), Right(Vector(owner, country)))
    val direct = Relation("direct", "sale", "country", Cardinality.ManyToOne, "Direct")
    val ambiguous = Catalog.build(entities, Vector(owner, country, direct)).toOption.get
    assert(ambiguous.route("sale", "country").isLeft)
    assertEquals(ambiguous.route("sale", "country", Vector("owner", "country")), Right(Vector(owner, country)))
    val back = Relation("back", "customer", "sale", Cardinality.ManyToOne, "Back")
    assert(Catalog.build(entities, Vector(owner, country, back)).toOption.get.route("sale", "country").isLeft)
    assert(Catalog.build(entities, Vector(owner.copy(cardinality = Cardinality.OneToMany), country)).toOption.get.route("sale", "country").isLeft)
  }
  test("typed lookup chains follow snowflake relations without changing fact grain") {
    case class Customer(id: String, country: String)
    case class Country(id: String, name: String)
    val countryName = Dimension[Country]("name", "Country name", Kind.Text, c => Value.Text(c.name))
    val customer = Model.build[Customer]("customer", Origin("crm", "1"), "customer", Vector.empty, Vector.empty, Vector.empty).toOption.get
    val country = Model.build("country", Origin("countries", "2"), "country", Vector(countryName), Vector.empty, Vector.empty).toOption.get
    val owner = Lookup.build(model, customer, Relation("owner", "sales", "customer", Cardinality.ManyToOne, "Owner"),
      (s: Sale) => s.segment, (c: Customer) => c.id, Vector.empty).toOption.get
    val countries = Lookup.build(owner.model, country, Relation("country", "customer", "country", Cardinality.ManyToOne, "Country"),
      (p: (Sale, Option[Customer])) => p._2.map(_.country), (c: Country) => c.id, Vector(countryName)).toOption.get
    val first = owner.rows(rows, Vector(Customer("A", "PL"))).toOption.get
    val enriched = countries.rows(first, Vector(Country("PL", "Poland"))).toOption.get
    val p = countries.model.plan(Request(Vector("revenue"), Vector("country.name"))).toOption.get
    note(p.explain)
    assertEquals(enriched.size, rows.size)
    assertEquals(p.model.relations.size, 2)
    assertEquals(p.run(enriched).toOption.get.groups.find(_.key == Vector(Value.Text("Poland"))).get.values, Vector(Some(BigDecimal(50))))
  }
