package okay.semantic.data



import okay.{Source}
import okay.given
import okay.freer.given
import okay.semantic.*
import okay.semantic.ossie.{Bindings, Document, Execution, FieldKey}

class TestExpressionApi extends okay.testkit.Munit.Diagnosed:
  val document = Document.readJson("""{"version":"0.2.0.dev0","name":"events","datasets":[{"name":"events","source":"ignored","fields":[{"name":"amount","datatype":"Decimal","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"amount"}]}}]}],"metrics":[{"name":"median","expression":{"dialects":[{"dialect":"ANSI_SQL","expression":"MEDIAN(amount)"}]}}]}""").toOption.get
  val rows = Vector(1L,3L,10L)
  val model = Execution.bind(document,"events",Bindings(Origin("input","v1"),"one event",Map("median" -> "EUR"),
    Map.empty[FieldKey,Dimension[Long]],Map(FieldKey("events","amount") -> Measure[Long]("amount","Amount",n => Some(BigDecimal(n)))))).toOption.get
  test("the same request/response protocol serves expression collections and sources") {
    val request = Request(Vector("median"))
    val local = Api.local(model,() => rows)
    var fetched = 0
    val source = Api.source(model,() => { fetched += 1; Source.of(rows.toList) })
    val expected = Wire.response(model.plan(request).toOption.get.run(rows))
    note(source.explain(Wire.request(request)).toString)
    assertEquals(local.describe,document.raw)
    assertEquals(local.query(Wire.request(request)).runWith,expected)
    assertEquals(source.query(Wire.request(request)).runWith,expected)
    assertEquals(fetched,1)
    val invalid = Request(Vector("missing"))
    val rejected = source.query(Wire.request(invalid)).runWith
    assert(rejected != expected)
    assertEquals(fetched,1)
  }
