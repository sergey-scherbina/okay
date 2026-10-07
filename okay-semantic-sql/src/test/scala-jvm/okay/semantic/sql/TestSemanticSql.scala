package okay.semantic.sql

import okay.*

import okay.given
import okay.freer.given
import okay.semantic.*
import okay.sql.SqlValue
import okay.jdbc.JdbcSql

class TestSemanticSql extends okay.testkit.Munit.Diagnosed:
  case class Sale(segment: Option[String], amount: Option[BigDecimal], cost: Option[BigDecimal])
  val rows = Vector(Sale(Some("A"), Some(BigDecimal("0.1")), Some(BigDecimal(1))),
    Sale(Some("A"), Some(BigDecimal("0.2")), Some(BigDecimal(9))),
    Sale(Some("A"), None, None), Sale(Some("B"), Some(BigDecimal(0)), Some(BigDecimal(0))),
    Sale(None, None, None), Sale(Some("O'Reilly"), Some(BigDecimal(7)), Some(BigDecimal(3))))
  val metrics = Vector(Metric("revenue", "Revenue", "EUR", Calculation.Sum("amount")),
    Metric("rows", "Sales", "rows", Calculation.Count()),
    Metric("present", "Paid sales", "rows", Calculation.Count(Some("amount"))),
    Metric("average", "Average", "EUR", Calculation.Average("amount")),
    Metric("ratio", "Revenue/cost", "ratio", Calculation.Ratio("amount", "cost")),
    Metric("cost", "Cost", "EUR", Calculation.Sum("cost")),
    Metric("profit", "Profit", "EUR", Calculation.Subtract("revenue", "cost")),
    Metric("margin", "Margin", "ratio", Calculation.Divide("profit", "revenue")),
    Metric("minimum", "Minimum", "EUR", Calculation.Minimum("amount")),
    Metric("maximum", "Maximum", "EUR", Calculation.Maximum("amount")),
    Metric("segments", "Segments", "count", Calculation.Distinct("segment")))
  val model = Model.build("sales", Origin("orders", "1"), "one order",
    Vector(Dimension[Sale]("segment", "Segment", Kind.Text, s => s.segment.fold[Value](Value.Null)(Value.Text.apply))),
    Vector(Measure[Sale]("amount", "Revenue EUR", _.amount), Measure[Sale]("cost", "Cost EUR", _.cost)), metrics).toOption.get
  val binding = Binding("sales", Map("segment" -> "segment"), Map("amount" -> "amount", "cost" -> "cost"))

  def parity(request: Request, data: Vector[Sale]): Unit =
    val plan = model.plan(request).toOption.get
    val statement = Render(plan, binding).toOption.get
    note(plan.explain); note(statement.sql); note(s"params: ${statement.params}")
    val conn = H2Fixture.open()
    try
      val setup = conn.createStatement()
      try setup.executeUpdate("CREATE TABLE \"sales\" (\"segment\" VARCHAR, \"amount\" NUMERIC(38,10), \"cost\" NUMERIC(38,10))"): Unit
      finally setup.close()
      val insert = conn.prepareStatement("INSERT INTO \"sales\" VALUES (?, ?, ?)")
      try data.foreach { row =>
        insert.setString(1, row.segment.orNull)
        insert.setBigDecimal(2, row.amount.map(_.bigDecimal).orNull)
        insert.setBigDecimal(3, row.cost.map(_.bigDecimal).orNull)
        insert.executeUpdate(): Unit
      }
      finally insert.close()
      val actual = statement.execute(using JdbcSql(conn, fetchSize = 2)).runWith.toOption.get
      val expected = plan.run(data).toOption.get
      assertEquals(actual.origin, expected.origin)
      assertEquals(actual.metrics, expected.metrics)
      assertEquals(actual.dimensions, expected.dimensions)
      assertEquals(actual.groups.map(g => g.key -> g.values).toMap, expected.groups.map(g => g.key -> g.values).toMap)
      if request.order.nonEmpty then assertEquals(actual.groups, expected.groups)
    finally conn.close()

  test("SQL fixture works without globally registered JDBC drivers") {
    val executable = java.nio.file.Path.of(System.getProperty("java.home"), "bin", "java").toString
    val classes = java.nio.file.Path.of(classOf[H2Fixture].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val h2 = java.nio.file.Path.of(classOf[org.h2.Driver].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val child = new ProcessBuilder(executable, "-cp", classes + java.io.File.pathSeparator + h2, classOf[H2Fixture].getName)
      .redirectErrorStream(true).start()
    val finished = child.waitFor(30L, java.util.concurrent.TimeUnit.SECONDS)
    if !finished then child.destroyForcibly(): Unit
    assert(finished, "isolated H2 fixture exceeded 30 seconds")
    val output = new String(child.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    note(output)
    assertEquals(child.exitValue(), 0, output)
    assert(output.contains("registry absent; direct query returned 42"), output)
  }
  test("real SQL agrees on exact sums, counts, weighted ratios and null groups") {
    parity(Request(metrics.map(_.id), Vector("segment")), rows)
    parity(Request(metrics.map(_.id)), rows)
  }
  test("empty inputs and all-null measures agree") {
    parity(Request(metrics.map(_.id)), Vector.empty)
    parity(Request(metrics.map(_.id), Vector("segment")), Vector.empty)
    parity(Request(metrics.map(_.id)), Vector(rows(4)))
  }
  test("bound quoted values and IS NULL filtering agree") {
    parity(Request(metrics.map(_.id), filters = Vector(Filter("segment", Value.Text("O'Reilly")))), rows)
    parity(Request(metrics.map(_.id), Vector("segment"), Vector(Filter("segment", Value.Null))), rows)
    val statement = Render(model.plan(Request(Vector("rows"), filters = Vector(Filter("segment", Value.Text("'; DROP TABLE sales; --"))))).toOption.get, binding).toOption.get
    note(statement.sql)
    assert(!statement.sql.contains("DROP TABLE"))
    assertEquals(statement.params, Vector(SqlValue.Text("'; DROP TABLE sales; --")))
  }
  test("missing and unsafe bindings are refused before execution") {
    val plan = model.plan(Request(Vector("revenue"), Vector("segment"))).toOption.get
    note(plan.explain)
    assert(Render(plan, binding.copy(table = "sales; DROP TABLE sales")).isLeft)
    assert(Render(plan, binding.copy(measures = Map.empty)).isLeft)
    assert(Render(plan, binding.copy(dimensions = Map("segment" -> "lower(segment)"))).isLeft)
    assert(Render(plan, binding.copy(table = "public.sales")).isLeft)
    assert(Render(plan, binding.copy(dimensions = Map.empty)).isLeft)
  }
  test("decode rejects wrong arity, floating totals, wrong kinds and inconsistent counts") {
    val plan = model.plan(Request(Vector("revenue"), Vector("segment"))).toOption.get
    val statement = Render(plan, binding).toOption.get
    note(statement.sql)
    assert(statement.decode(Vector(Vector.empty)).isLeft)
    assert(statement.decode(Vector(Vector(SqlValue.Text("A"), SqlValue.I64(1), SqlValue.F64(0.1), SqlValue.I64(1), SqlValue.Num(BigDecimal(1)), SqlValue.Num(BigDecimal(1))))).isLeft)
    assert(statement.decode(Vector(Vector(SqlValue.Bool(true), SqlValue.I64(1), SqlValue.Num(BigDecimal(1)), SqlValue.I64(1), SqlValue.Num(BigDecimal(1)), SqlValue.Num(BigDecimal(1))))).isLeft)
    assert(statement.decode(Vector(Vector(SqlValue.Text("A"), SqlValue.I64(1), SqlValue.Null, SqlValue.I64(1), SqlValue.Null, SqlValue.Null))).isLeft)
    assert(statement.decode(Vector(Vector(SqlValue.Text("A"), SqlValue.Num(BigDecimal("0.5")), SqlValue.Null, SqlValue.I64(0), SqlValue.Null, SqlValue.Null))).isLeft)
  }

  test("parity across generated datasets and combined filters") {
    val generated = Vector.tabulate(127)(i => Sale(
      if i % 7 == 0 then None else Some(s"segment-${i % 5}"),
      if i % 3 == 0 then None else Some(BigDecimal(i - 60) / 10),
      if i % 4 == 0 then None else Some(BigDecimal(i % 11))))
    parity(Request(metrics.map(_.id), Vector("segment")), generated)
    parity(Request(metrics.map(_.id), filters = Vector(Filter("segment", Value.Text("segment-2")), Filter("segment", Value.Text("segment-3")))), generated)
  }

  test("comparisons, distinct, derived metrics, having and pagination have SQL parity") {
    val requests = Vector(
      Request(metrics.map(_.id), Vector("segment"), order = Vector(Order("revenue", descending = true)), offset = 1, limit = Some(2)),
      Request(Vector("profit", "margin"), Vector("segment"), having = Vector(Having("profit", Some(BigDecimal(0)), Comparison.Lt))),
      Request(metrics.map(_.id), filters = Vector(Filter("segment", Value.Text("A"), Comparison.In, Vector(Value.Null)))),
      Request(metrics.map(_.id), filters = Vector(Filter("segment", Value.Text("A"), Comparison.Ne))),
      Request(metrics.map(_.id), filters = Vector(Filter("segment", comparison = Comparison.IsNotNull))),
      Request(metrics.map(_.id), filters = Vector(Filter("segment", Value.Text("A"), Comparison.Ge))))
    requests.foreach(r => parity(r, rows))
  }
  test("lookup join preserves the fact grain and rejects duplicate dimension keys") {
    case class Customer(id: String, tier: String)
    val dim = Dimension[Customer]("tier", "Customer tier", Kind.Text, c => Value.Text(c.tier))
    val customer = Model.build("customer", Origin("CRM", "3"), "one customer", Vector(dim), Vector.empty, Vector.empty).toOption.get
    val relation = Relation("customer", "sales", "customer", Cardinality.ManyToOne, "Sold to")
    val lookup = Lookup.build(model, customer, relation, (s: Sale) => s.segment, (c: Customer) => c.id, Vector(dim)).toOption.get
    val customers = Vector(Customer("A", "enterprise"), Customer("B", "consumer"))
    val p = lookup.model.plan(Request(Vector("revenue", "rows", "margin"), Vector("customer.tier"))).toOption.get
    val bound = binding.copy(joins = Vector(Join("customer", "customer", "c", ColumnRef("segment"), "id")),
      dimensionRefs = Map("customer.tier" -> ColumnRef("tier", "c")))
    val statement = Render(p, bound).toOption.get
    note(statement.sql)
    val conn = H2Fixture.open()
    try
      val setup = conn.createStatement()
      try
        setup.executeUpdate("CREATE TABLE \"sales\" (\"segment\" VARCHAR, \"amount\" NUMERIC(38,10), \"cost\" NUMERIC(38,10))"): Unit
        setup.executeUpdate("CREATE TABLE \"customer\" (\"id\" VARCHAR, \"tier\" VARCHAR)"): Unit
        setup.executeUpdate("INSERT INTO \"customer\" VALUES ('A', 'enterprise'), ('B', 'consumer')"): Unit
      finally setup.close()
      val insert = conn.prepareStatement("INSERT INTO \"sales\" VALUES (?, ?, ?)")
      try rows.foreach { r =>
        insert.setString(1, r.segment.orNull); insert.setBigDecimal(2, r.amount.map(_.bigDecimal).orNull); insert.setBigDecimal(3, r.cost.map(_.bigDecimal).orNull)
        insert.executeUpdate(): Unit
      }
      finally insert.close()
      val db = JdbcSql(conn, fetchSize = 2)
      val actual = statement.execute(using db).runWith.toOption.get
      val expected = p.run(lookup.rows(rows, customers).toOption.get).toOption.get
      assertEquals(actual.groups.map(g => g.key -> g.values).toMap, expected.groups.map(g => g.key -> g.values).toMap)
      val duplicate = conn.createStatement()
      try duplicate.executeUpdate("INSERT INTO \"customer\" VALUES ('A', 'duplicate')"): Unit
      finally duplicate.close()
      assert(statement.execute(using db).runWith.left.toOption.get.head.contains("duplicate right"))
      assert(Render(p, bound.copy(joins = bound.joins.map(_.copy(cardinality = Cardinality.ManyToMany)))).isLeft)
    finally conn.close()
  }
  test("fixed time transformations are derived from the semantic dimension") {
    val bucket = FixedBucket.build(10L).toOption.get
    val m = Model.build[Long]("times", Origin("clock", "1"), "one time",
      Vector(Time.dimension("bucket", "Ten microseconds", t => Some(t), bucket)),
      Vector.empty, Vector(Metric("rows", "Rows", "count", Calculation.Count()))).toOption.get
    val p = m.plan(Request(Vector("rows"), Vector("bucket"), order = Vector(Order("bucket")))).toOption.get
    val statement = Render(p, Binding("times", Map("bucket" -> "at"), Map.empty)).toOption.get
    note(statement.sql)
    val conn = H2Fixture.open()
    try
      val setup = conn.createStatement()
      try
        setup.executeUpdate("CREATE TABLE \"times\" (\"at\" BIGINT)"): Unit
        setup.executeUpdate("INSERT INTO \"times\" VALUES (-11), (-1), (0), (9), (10)"): Unit
      finally setup.close()
      assertEquals(statement.execute(using JdbcSql(conn)).runWith, p.run(Vector(-11L, -1L, 0L, 9L, 10L)))
    finally conn.close()
  }
