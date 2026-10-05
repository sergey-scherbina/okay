package okay.semantic.sql

import okay.*
import okay.given
import okay.semantic.*
import okay.sql.SqlValue
import okay.jdbc.JdbcSql
import java.sql.DriverManager

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
    Metric("ratio", "Revenue/cost", "ratio", Calculation.Ratio("amount", "cost")))
  val model = Model.build("sales", Origin("orders", "1"), "one order",
    Vector(Dimension[Sale]("segment", "Segment", Kind.Text, s => s.segment.fold[Value](Value.Null)(Value.Text.apply))),
    Vector(Measure[Sale]("amount", "Revenue EUR", _.amount), Measure[Sale]("cost", "Cost EUR", _.cost)), metrics).toOption.get
  val binding = Binding("sales", Map("segment" -> "segment"), Map("amount" -> "amount", "cost" -> "cost"))

  def parity(request: Request, data: Vector[Sale]): Unit =
    val plan = model.plan(request).toOption.get
    val statement = Render(plan, binding).toOption.get
    note(plan.explain); note(statement.sql); note(s"params: ${statement.params}")
    val conn = DriverManager.getConnection("jdbc:h2:mem:")
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
    finally conn.close()

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
    assert(statement.decode(Vector(Vector(SqlValue.Text("A"), SqlValue.I64(1), SqlValue.F64(0.1), SqlValue.I64(1)))).isLeft)
    assert(statement.decode(Vector(Vector(SqlValue.Bool(true), SqlValue.I64(1), SqlValue.Num(BigDecimal(1)), SqlValue.I64(1)))).isLeft)
    assert(statement.decode(Vector(Vector(SqlValue.Text("A"), SqlValue.I64(1), SqlValue.Null, SqlValue.I64(1)))).isLeft)
    assert(statement.decode(Vector(Vector(SqlValue.Text("A"), SqlValue.Num(BigDecimal("0.5")), SqlValue.Null, SqlValue.I64(0)))).isLeft)
  }

  test("parity across generated datasets and combined filters") {
    val generated = Vector.tabulate(127)(i => Sale(
      if i % 7 == 0 then None else Some(s"segment-${i % 5}"),
      if i % 3 == 0 then None else Some(BigDecimal(i - 60) / 10),
      if i % 4 == 0 then None else Some(BigDecimal(i % 11))))
    parity(Request(metrics.map(_.id), Vector("segment")), generated)
    parity(Request(metrics.map(_.id), filters = Vector(Filter("segment", Value.Text("segment-2")), Filter("segment", Value.Text("segment-3")))), generated)
  }
