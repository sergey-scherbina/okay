package okay.semantic.ossie

import okay.semantic.{Calculation, Having, Order, Request, Value}
import okay.semantic.Metric as CoreMetric
import okay.codec.Json.*

class TestExpressions extends okay.testkit.Munit.Diagnosed:
  import Samples.*
  val input = Vector(Sale("a",Some(BigDecimal(2)),Some(BigDecimal(3)),Some("A")),
    Sale("b",Some(BigDecimal(4)),Some(BigDecimal(2)),Some("A")),
    Sale("c",Some(BigDecimal(6)),None,Some("B")),Sale("d",None,None,None))
  def document(expressions: (String,String)*): Document = Document.fromJson(replace(raw,"metrics",
    JArr(expressions.toVector.map((n,e) => metric(n,e))))).toOption.get
  def expressionModel(expressions: (String,String)*): ExpressionModel[Sale] =
    val bound = Execution.bind(document(expressions*),"orders",bindings.copy(units = expressions.map((n,_) => n -> "EUR").toMap))
    note(bound.toString)
    bound.toOption.get
  def evaluate(expression: String, rs: Vector[Sale] = input): Option[BigDecimal] =
    val plan = expressionModel("answer" -> expression).plan(Request(Vector("answer"))).toOption.get
    note(plan.explain)
    val result = plan.run(rs)
    note(result.toString)
    result.toOption.get.groups.head.values.head

  test("constants, offsets and reciprocal divisions use the same core arithmetic") {
    val expressions = Vector("third" -> "SUM(amount) / 3","offset" -> "SUM(amount) + 100", "reciprocal" -> "100 / SUM(amount)",
      "constant" -> "42","zero" -> "SUM(amount) / 0","fraction" -> "1 / 3", "large" -> "SUM(amount) * 9007199254740993")
    val doc = document(expressions*)
    val b = bindings.copy(units = expressions.map((n,_) => n -> "EUR").toMap)
    val core = Bridge.bind(doc,"orders",b).toOption.get.plan(Request(expressions.map(_._1))).toOption.get
    val extended = Execution.bind(doc,"orders",b).toOption.get.plan(Request(expressions.map(_._1))).toOption.get
    assertEquals(extended.run(input),core.run(input))
    assertEquals(extended.run(Vector.empty),core.run(Vector.empty))
    assertEquals(core.run(input).toOption.get.groups.head.values(0),Some(BigDecimal(4)))
    val authored = okay.semantic.Model.build[Sale]("constant",b.origin,b.grain,Vector.empty,Vector.empty,
      Vector(CoreMetric("answer","Answer","EUR",Calculation.Constant(BigDecimal(42))))).toOption.get
    val exported = Export.model(authored,"orders",Map.empty).toOption.get
    assertEquals(Bridge.bind(exported,"constant",b.copy(units = Map("answer" -> "EUR"),dimensions = Map.empty,measures = Map.empty)).toOption.get
      .plan(Request(Vector("answer"))).toOption.get.run(input).toOption.get.groups.head.values,Vector(Some(BigDecimal(42))))
  }
  test("row arithmetic, searched/simple CASE and conditional aggregates") {
    assertEquals(evaluate("SUM(amount * cost)"),Some(BigDecimal(14)))
    assertEquals(evaluate("SUM(CASE WHEN segment = 'A' THEN amount ELSE 0 END)"),Some(BigDecimal(6)))
    assertEquals(evaluate("SUM(CASE segment WHEN 'A' THEN amount WHEN 'B' THEN 100 ELSE 0 END)"),Some(BigDecimal(106)))
    assertEquals(evaluate("COUNT(CASE WHEN amount > 3 THEN 1 END)"),Some(BigDecimal(2)))
    assertEquals(evaluate("SUM(amount) FILTER (WHERE segment = 'A')"),Some(BigDecimal(6)))
    assertEquals(evaluate("SUM(DISTINCT amount)",input ++ input),Some(BigDecimal(12)))
    assertEquals(evaluate("COUNT(DISTINCT COALESCE(segment, 'missing'))"),Some(BigDecimal(3)))
  }
  test("scalar functions work at both row and metric levels with decimal rounding") {
    assertEquals(evaluate("ROUND(SUM(amount) / 7, 2)"),Some(BigDecimal("1.71")))
    assertEquals(evaluate("SUM(COALESCE(amount, 10))"),Some(BigDecimal(22)))
    assertEquals(evaluate("COALESCE(SUM(amount), 10)",Vector.empty),Some(BigDecimal(10)))
    assertEquals(evaluate("SUM(IF(segment IN ('A', 'B'), amount, 0))"),Some(BigDecimal(12)))
    assertEquals(evaluate("SUM(CASE WHEN amount BETWEEN 3 AND 7 AND segment LIKE 'B%' THEN amount ELSE 0 END)"),Some(BigDecimal(6)))
    assertEquals(evaluate("SUM(NULLIF(amount, 4))"),Some(BigDecimal(8)))
    assertEquals(evaluate("ROUND(-1.235, 2)"),Some(BigDecimal("-1.24")))
    assertEquals(evaluate("SUM(LENGTH(UPPER(segment)))"),Some(BigDecimal(3)))
    assertEquals(evaluate("CASE WHEN NULL OR TRUE THEN 9 ELSE 0 END"),Some(BigDecimal(9)))
    assertEquals(evaluate("CASE WHEN NULL AND FALSE THEN 9 ELSE 0 END"),Some(BigDecimal(0)))
  }
  test("median, percentiles and statistics include singleton and empty/null cases") {
    assertEquals(evaluate("MEDIAN(amount)"),Some(BigDecimal(4)))
    assertEquals(evaluate("PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY amount)"),Some(BigDecimal(3)))
    assertEquals(evaluate("PERCENTILE_DISC(0.25) WITHIN GROUP (ORDER BY amount)"),Some(BigDecimal(2)))
    assertEquals(evaluate("VAR_SAMP(amount)"),Some(BigDecimal(4)))
    assertEquals(evaluate("STDDEV(amount)"),Some(BigDecimal(2)))
    assertEquals(evaluate("MEDIAN(amount)",Vector.empty),None)
    assertEquals(evaluate("VAR_SAMP(amount)",input.take(1)),None)
    assertEquals(evaluate("VAR_POP(amount)",input.take(1)),Some(BigDecimal(0)))
    assertEquals(evaluate("MEDIAN(amount)",input.take(2)),Some(BigDecimal(3)))
  }
  test("window aggregate, ranking and offsets operate before output pagination") {
    val model = expressionModel("revenue" -> "SUM(amount)","total" -> "SUM(SUM(amount)) OVER ()",
      "cumulative" -> "SUM(SUM(amount)) OVER (ORDER BY segment ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)",
      "rank" -> "RANK() OVER (ORDER BY revenue DESC)","dense" -> "DENSE_RANK() OVER (ORDER BY revenue DESC)",
      "previous" -> "LAG(revenue, 1, 0) OVER (ORDER BY segment)","next" -> "LEAD(revenue) OVER (ORDER BY segment)",
      "first" -> "FIRST_VALUE(revenue) OVER (ORDER BY segment)","last" -> "LAST_VALUE(revenue) OVER (ORDER BY segment ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)",
      "row" -> "ROW_NUMBER() OVER (ORDER BY segment)","tile" -> "NTILE(2) OVER (ORDER BY segment)")
    val names = Vector("revenue","total","cumulative","rank","dense","previous","next","first","last","row","tile")
    val result = model.plan(Request(names,Vector("segment"),order = Vector(Order("segment")))).toOption.get.run(input).toOption.get
    note(result.toString)
    assertEquals(result.groups.head.values,Vector(6,12,6,1,1,0,6,6,0,1,1).zipWithIndex.map((n,i) => if i == 8 then None else Some(BigDecimal(n))))
    assertEquals(result.groups(1).values(2),Some(BigDecimal(12)))
    assertEquals(result.groups(1).values(3),Some(BigDecimal(1))) // peers share rank
    assertEquals(result.groups(2).values(3),Some(BigDecimal(3)))
    assertEquals(result.groups(2).values(4),Some(BigDecimal(2)))
    val page = model.plan(Request(names,Vector("segment"),having = Vector(Having("total",Some(BigDecimal(12)))),
      order = Vector(Order("segment")),offset = 1,limit = Some(1))).toOption.get.run(input).toOption.get
    assertEquals(page.groups,Vector(result.groups(1)))
    assertEquals(evaluate("COUNT(*) OVER ()"),Some(BigDecimal(1))) // one result group
  }
  test("ROWS frames, partitions and default RANGE peers have distinct semantics") {
    val m = expressionModel("revenue" -> "SUM(amount)",
      "peers" -> "SUM(revenue) OVER (ORDER BY revenue DESC)",
      "rows" -> "SUM(revenue) OVER (ORDER BY segment ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)",
      "partition" -> "SUM(revenue) OVER (PARTITION BY segment)")
    val p = m.plan(Request(Vector("peers","rows","partition"),Vector("segment"),order = Vector(Order("segment")))).toOption.get
    val result = p.run(input).toOption.get
    assertEquals(result.groups.take(2).map(_.values.head),Vector.fill(2)(Some(BigDecimal(12))))
    assertEquals(result.groups(1).values(1),Some(BigDecimal(12)))
    assertEquals(result.groups(0).values(2),Some(BigDecimal(6)))
  }
  test("checked relationship enrichment supports foreign fields and preserves unmatched facts") {
    val customer = obj("name" -> JStr("customers"),"source" -> JStr("customers"),"primary_key" -> arr(JStr("id")),
      "fields" -> arr(field("id","String"),field("weight","Decimal")))
    val relation = obj("name" -> JStr("customer"),"from" -> JStr("orders"),"to" -> JStr("customers"),
      "from_columns" -> arr(JStr("id")),"to_columns" -> arr(JStr("id")))
    val d = Document.fromJson(replace(replace(document("answer" -> "SUM(customers.weight * amount)").raw,
      "datasets",JArr(Read.array(raw,"datasets") :+ customer)),"relationships",arr(relation))).toOption.get
    val m = Execution.bind(d,"orders",bindings.copy(units = Map("answer" -> "EUR"))).toOption.get
    val p = m.plan(Request(Vector("answer"))).toOption.get
    val t = Table.of("customers",Vector("a" -> 10,"b" -> 20),Map[String,((String,Int)) => Value](
      "id" -> (p => Value.Text(p._1)),"weight" -> (p => Value.Number(BigDecimal(p._2))))).toOption.get
    assertEquals(p.run(input,Map("customers" -> t)).toOption.get.groups.head.values,Vector(Some(BigDecimal(100))))
    assert(p.run(input).isLeft)
    assert(p.run(input,Map("customers" -> t.copy(rows = t.rows ++ t.rows))).left.toOption.get.exists(_.contains("duplicate right key")))
    assert(p.run(input,Map("customers" -> t.copy(rows = Vector(Map.empty)))).isLeft)
    assert(p.run(input,Map("customers" -> t.copy(rows = t.rows.map(_ - "weight")))).left.toOption.get.exists(_.contains("missing field")))
  }
  test("raw row windows use an explicit primary-key grain, with duplicate-key diagnoses") {
    val m = expressionModel("total" -> "SUM(amount) OVER ()", "rowValue" -> "CASE WHEN amount > 3 THEN amount ELSE 0 END")
    val p = m.plan(Request(Vector("total","rowValue"),Vector("id"),order = Vector(Order("id")))).toOption.get
    val result = p.run(input).toOption.get
    assertEquals(result.groups.map(_.values.head),Vector.fill(4)(Some(BigDecimal(12))))
    assertEquals(result.groups.map(_.values(1)),Vector(0,4,6,0).map(n => Some(BigDecimal(n))))
    assert(p.run(input ++ input).isLeft)
  }
  test("composite lookup keys and ambiguous routes are validated") {
    val customer = obj("name" -> JStr("customers"),"source" -> JStr("customers"),"primary_key" -> arr(JStr("id"),JStr("region")),
      "fields" -> arr(field("id","String"),field("region","String"),field("weight","Decimal")))
    val relationship = obj("name" -> JStr("customer"),"from" -> JStr("orders"),"to" -> JStr("customers"),
      "from_columns" -> arr(JStr("id"),JStr("segment")),"to_columns" -> arr(JStr("id"),JStr("region")))
    val initial = replace(replace(document("answer" -> "SUM(customers.weight)").raw,"datasets",
      JArr(Read.array(raw,"datasets") :+ customer)),"relationships",arr(relationship))
    val b = bindings.copy(units = Map("answer" -> "EUR"))
    val m = Execution.bind(Document.fromJson(initial).toOption.get,"orders",b).toOption.get
    val p = m.plan(Request(Vector("answer"))).toOption.get
    val t = Table("customers",Vector(Map("id" -> Value.Text("a"),"region" -> Value.Text("A"),"weight" -> Value.Number(BigDecimal(10))),
      Map("id" -> Value.Text("a"),"region" -> Value.Text("B"),"weight" -> Value.Number(BigDecimal(20)))))
    assertEquals(p.run(input,Map("customers" -> t)).toOption.get.groups.head.values,Vector(Some(BigDecimal(10))))
    val duplicateRoute = Document.fromJson(replace(initial,"relationships",arr(relationship,replace(relationship,"name",JStr("other"))))).toOption.get
    val ambiguous = Execution.bind(duplicateRoute,"orders",b).toOption.get
    assert(ambiguous.plan(Request(Vector("answer"))).isLeft)
    assertEquals(ambiguous.plan(Request(Vector("answer")),via = Vector("customer")).toOption.get.run(input,Map("customers" -> t)),p.run(input,Map("customers" -> t)))
  }
  test("function and language facades extend the same plan without changing callers") {
    val custom = new Functions:
      def accepts(n: String,a: Int): Boolean = n == "DOUBLE" && a == 1
      def call(n: String,a: Vector[Value]): Either[String,Value] =
        note(n)
        Scalar.binary("*",a.head,Value.Number(BigDecimal(2)))
    val d = document("answer" -> "DOUBLE(SUM(amount))")
    val b = bindings.copy(units = Map("answer" -> "EUR"))
    assert(Execution.bind(d,"orders",b).isLeft)
    val m = Execution.bind(d,"orders",b,functions = Functions.orElse(custom,Functions.portable)).toOption.get
    assertEquals(m.plan(Request(Vector("answer"))).toOption.get.run(input).toOption.get.groups.head.values,Vector(Some(BigDecimal(24))))
    val vendor = Document.fromJson(replace(d.raw,"metrics",arr(obj("name" -> JStr("answer"),"expression" -> expr("ROUND(SUM(`amount`), 2)","BIGQUERY"))))).toOption.get
    assert(Execution.bind(vendor,"orders",b,dialect = "BIGQUERY").isLeft)
    assertEquals(Execution.bind(vendor,"orders",b,dialect = "BIGQUERY",language = Language.sqlFamily).toOption.get
      .plan(Request(Vector("answer"))).toOption.get.run(input).toOption.get.groups.head.values,Vector(Some(BigDecimal(12))))
  }
  test("exact long decimals, selected dependency closure and iterative metric chains") {
    val factor = "12345678901234567890123456789012345678901234567890"
    assertEquals(evaluate(s"SUM(amount) * $factor"),Some(BigDecimal(new java.math.BigDecimal(factor).multiply(new java.math.BigDecimal("12")))))
    assertEquals(evaluate(s"-$factor"),Some(BigDecimal(new java.math.BigDecimal(factor).negate())))
    assertEquals(evaluate("SUM(1)"),Some(BigDecimal(4)))
    val definitions = Vector.tabulate(1000)(i => s"m$i" -> (if i == 0 then "COUNT(*)" else s"m${i-1} + 1")) :+ ("unknown" -> "ALIEN(amount)")
    val b = bindings.copy(units = definitions.map((n,_) => n -> "count").toMap)
    val m = Execution.bind(document(definitions*),"orders",b,metricNames = Vector("m999")).toOption.get
    assertEquals(m.plan(Request(Vector("m999"))).toOption.get.run(input).toOption.get.groups.head.values,Vector(Some(BigDecimal(1003))))
  }
  test("invalid units, logical types, filters and numeric ranges carry diagnoses") {
    val unitsDoc = document("revenue" -> "SUM(amount)","cost" -> "SUM(cost)","answer" -> "revenue + cost")
    val mismatch = Execution.bind(unitsDoc,"orders",bindings.copy(units = Map("revenue" -> "EUR","cost" -> "USD","answer" -> "EUR"))).toOption.get
    assert(mismatch.plan(Request(Vector("answer"))).left.toOption.get.exists(_.contains("units")))
    val badType = bindings.copy(units = Map("answer" -> "EUR"),measures = bindings.measures.updated(FieldKey("orders","segment"),
      okay.semantic.Measure[Sale]("segment","Wrong numeric reader",_ => Some(BigDecimal(1)))))
    val typed = Execution.bind(document("answer" -> "SUM(segment)"),"orders",badType).toOption.get.plan(Request(Vector("answer"))).toOption.get
    assert(typed.run(input).left.toOption.get.exists(_.contains("declared type")))
    val condition = expressionModel("answer" -> "SUM(amount) FILTER (WHERE amount)").plan(Request(Vector("answer"))).toOption.get
    assert(condition.run(input).left.toOption.get.exists(_.contains("Boolean")))
    val huge = input.take(2).zipWithIndex.map((r,i) => r.copy(amount = Some(BigDecimal(if i == 0 then "1e1000" else "-1e1000"))))
    val standardDeviation = expressionModel("answer" -> "STDDEV(amount)").plan(Request(Vector("answer"))).toOption.get
    assert(standardDeviation.run(huge).left.toOption.get.exists(_.contains("finite floating-point range")))
  }
  test("planning rejects malformed expressions, invalid stages, cycles and unsafe resource requests") {
    Vector("SELECT amount","SUM(amount); DROP TABLE x","SUM()","SUM(SUM(amount))","RANK()","unknown(amount)","COUNT(DISTINCT *)").foreach { e =>
      val b = Execution.bind(document("answer" -> e),"orders",bindings.copy(units = Map("answer" -> "EUR")))
      val p = b.flatMap(_.plan(Request(Vector("answer"))))
      note(s"$e => $p"); assert(p.isLeft)
    }
    assert(expressionModel("answer" -> "SUM(amount) + amount").plan(Request(Vector("answer"))).isLeft)
    assert(expressionModel("answer" -> "SUM(amount)").plan(Request(Vector("answer")),maxRows = -1).isLeft)
    val cycle = Execution.bind(document("a" -> "b + 1","b" -> "a + 1"),"orders",bindings.copy(units = Map("a" -> "EUR","b" -> "EUR")))
    assert(cycle.isLeft)
    assert(Program.parse("(" * 200 + "42" + ")" * 200).isLeft)
    assert(expressionModel("answer" -> "SUM(amount)").plan(Request(Vector("answer")),maxRows = 2).toOption.get.run(input).isLeft)
    assert(Table.of("customers",input,Map.empty[String,Sale => Value],2).isLeft)
    assert(expressionModel("answer" -> "SUM(amount)").plan(Request(Vector("answer")),maxRows = Int.MaxValue).isLeft)
  }
