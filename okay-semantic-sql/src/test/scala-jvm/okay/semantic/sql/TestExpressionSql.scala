package okay.semantic.sql


import okay.given
import okay.freer.given
import okay.std.given
import okay.codec.Json.*
import okay.semantic.{Dimension, Kind, Measure, Origin, Request, Value}
import okay.semantic.ossie.{Bindings, Document, Execution, FieldKey}
import okay.sql.SqlValue
import okay.jdbc.JdbcSql

class TestExpressionSql extends okay.testkit.Munit.Diagnosed:
  type Row = Vector[SqlValue]
  def obj(fields: (String,okay.codec.Json)*): okay.codec.Json = JObj(fields.toVector)
  def expr(text: String): okay.codec.Json = obj("dialects" -> JArr(Vector(obj("dialect" -> JStr("ANSI_SQL"),"expression" -> JStr(text)))))
  val expressions = Vector("median" -> "MEDIAN(amount)","conditional" -> "SUM(CASE WHEN segment = 'A' THEN amount * 2 ELSE 0 END)",
    "total" -> "SUM(SUM(amount)) OVER ()", "rounded" -> "ROUND(SUM(amount) / 3, 2)")
  val document = Document.fromJson(obj("version" -> JStr(Document.version),"name" -> JStr("sales"),
    "datasets" -> JArr(Vector(obj("name" -> JStr("sales"),"source" -> JStr("ignored source SQL"),"fields" -> JArr(
      Vector("segment" -> "String","amount" -> "Decimal").map((n,t) => obj("name" -> JStr(n),"datatype" -> JStr(t),"expression" -> expr(n))))))),
    "metrics" -> JArr(expressions.map((n,e) => obj("name" -> JStr(n),"expression" -> expr(e)))))).toOption.get
  val bindings = Bindings(Origin("sql","v1"),"one sale",expressions.map((n,_) => n -> "EUR").toMap,
    Map(FieldKey("sales","segment") -> Dimension[Row]("segment","Segment",Kind.Text,r => r(0) match
      case SqlValue.Text(s) => Value.Text(s)
      case _ => Value.Null)),
    Map(FieldKey("sales","amount") -> Measure[Row]("amount","Amount",r => r(1) match
      case SqlValue.Num(n) => Some(n)
      case _ => None)))
  val model = Execution.bind(document,"sales",bindings).toOption.get
  val plan = model.plan(Request(expressions.map(_._1),Vector("segment"))).toOption.get
  val rows = Vector(Vector(SqlValue.Text("A"),SqlValue.Num(BigDecimal(1))),Vector(SqlValue.Text("A"),SqlValue.Num(BigDecimal(3))),Vector(SqlValue.Text("B"),SqlValue.Num(BigDecimal(10))))
  test("real SQL projection agrees with portable collection expressions") {
    val conn = H2Fixture.open()
    try
      val statement = conn.createStatement()
      try
        statement.executeUpdate("CREATE TABLE \"sales\" (\"segment\" VARCHAR, \"amount\" NUMERIC(38,10))"): Unit
        statement.executeUpdate("INSERT INTO \"sales\" VALUES ('A', 1), ('A', 3), ('B', 10)"): Unit
      finally statement.close()
      val input = ExpressionSql.Input[Row]("sales",Vector("segment","amount"),Right(_))
      note(plan.explain)
      val actual = ExpressionSql.execute(plan,input)(using JdbcSql(conn,fetchSize = 1)).runWith.toOption.get
      val expected = plan.run(rows).toOption.get
      assertEquals(actual.groups.map(g => g.key -> g.values).toMap,expected.groups.map(g => g.key -> g.values).toMap)
      val limited = model.plan(Request(Vector("median")),maxRows = 1).toOption.get
      assert(ExpressionSql.execute(limited,input)(using JdbcSql(conn)).runWith.isLeft)
      val damaged = input.copy[Row](decode = _ => Left("bad cell"))
      assert(ExpressionSql.execute(plan,damaged)(using JdbcSql(conn)).runWith.left.toOption.get.exists(_.contains("bad cell")))
    finally conn.close()
    assert(ExpressionSql.query(ExpressionSql.Input[Row]("sales; DROP TABLE x",Vector("amount"),Right(_))).isLeft)
    assert(ExpressionSql.query(ExpressionSql.Input[Row]("sales",Vector("amount","amount"),Right(_))).isLeft)
  }
