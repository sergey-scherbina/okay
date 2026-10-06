package okay.semantic.ossie

import okay.codec.Json
import Json.*
import okay.semantic.{Origin, Model, Dimension, Measure, Kind, Value, Request, Calculation, Time, FixedBucket}
import okay.semantic.Metric as CoreMetric

object Samples:
  def obj(fields: (String,Json)*): Json = JObj(fields.toVector)
  def arr(values: Json*): Json = JArr(values.toVector)
  def expr(text: String, dialect: String = "ANSI_SQL"): Json = obj("dialects" -> arr(obj("dialect" -> JStr(dialect),"expression" -> JStr(text))))
  def field(name: String, datatype: String): Json = obj("name" -> JStr(name),"datatype" -> JStr(datatype),"expression" -> expr(name))
  def metric(name: String,text: String): Json = obj("name" -> JStr(name),"expression" -> expr(text))
  val metricDefinitions = Vector(
    "revenue" -> "SUM(orders.amount)", "cost" -> "SUM(orders.cost)", "profit" -> "revenue - cost",
    "margin" -> "profit / revenue", "average" -> "AVG(amount)", "rows" -> "COUNT(*)",
    "distinct" -> "COUNT(DISTINCT segment)", "present" -> "COUNT(amount)", "ids" -> "COUNT(id)",
    "minimum" -> "MIN(amount)", "maximum" -> "MAX(amount)", "scaled" -> "revenue * 2 / 4", "negative" -> "-revenue")
  val units = metricDefinitions.map { (n,_) => n -> (if Set("rows","distinct","present","ids")(n) then "count" else if n == "margin" then "ratio" else "EUR") }.toMap
  val metadata = obj("instructions" -> JStr("Use recognized revenue"),"synonyms" -> arr(JStr("sales")),
    "examples" -> arr(JStr("Revenue by segment")),"future" -> obj("enabled" -> JBool(true),"threshold" -> JNum(1.25)))
  val raw = obj("version" -> JStr(Document.version),"name" -> JStr("sales"),"ai_context" -> metadata,
    "datasets" -> arr(obj("name" -> JStr("orders"),"source" -> JStr("warehouse.sales.orders"),"primary_key" -> arr(JStr("id")),
      "fields" -> arr(field("id","String"),field("amount","Decimal"),field("cost","Decimal"),field("segment","String"),
        field("date","Date"), JObj(Read.get(field("audit","DateTimeTz"),"name").toVector.map("name" -> _) ++
          Vector("datatype" -> JStr("DateTimeTz"),"expression" -> expr("audit"),"dimension" -> obj("is_time" -> JBool(false))))))),
    "metrics" -> JArr(metricDefinitions.map((n,e) => metric(n,e))),
    "custom_extensions" -> arr(obj("vendor_name" -> JStr("ACME"),"data" -> JStr("{\"exact\":9007199254740993,\"meaning\":\"keep\"}"))))
  case class Sale(id: String, amount: Option[BigDecimal], cost: Option[BigDecimal], segment: Option[String])
  val rows = Vector(Sale("a",Some(BigDecimal("0.1")),Some(BigDecimal("0.05")),Some("A")),
    Sale("b",Some(BigDecimal("0.2")),Some(BigDecimal("0.1")),Some("A")), Sale("c",None,None,None))
  val id = Dimension[Sale]("id","Order identity",Kind.Text,s => Value.Text(s.id))
  val segment = Dimension[Sale]("segment","Segment",Kind.Text,s => s.segment.fold[Value](Value.Null)(Value.Text.apply))
  val amount = Measure[Sale]("amount","Amount",_.amount)
  val cost = Measure[Sale]("cost","Cost",_.cost)
  val bindings = Bindings(Origin("ledger","v1"),"one order",units,
    Map(FieldKey("orders","id") -> id, FieldKey("orders","segment") -> segment),
    Map(FieldKey("orders","amount") -> amount, FieldKey("orders","cost") -> cost))
  def replace(value: Json,key: String,next: Json): Json = value match
    case JObj(fs) => JObj(fs.filterNot(_._1 == key) :+ (key -> next))
    case _ => value

class TestOssie extends okay.testkit.Munit.Diagnosed:
  import Samples.*
  test("metadata, omissions and dialect expressions survive JSON and portable YAML") {
    val d = Document.fromJson(raw).toOption.get
    note(s"pinned ${Document.revision}")
    assertEquals(Document.readJson(d.json).toOption.get.raw,raw)
    assertEquals(Document.readYaml(d.yaml).toOption.get.raw,raw)
    assertEquals(d.aiContext,Some(metadata))
    assertEquals(d.extensions.head.data,"{\"exact\":9007199254740993,\"meaning\":\"keep\"}")
    assert(d.datasets.head.fields.find(_.name == "date").get.isTime)
    assert(!d.datasets.head.fields.find(_.name == "audit").get.isTime)
    assert(!d.json.contains("relationships"))
    val opaque = replace(raw,"metrics",arr(obj("name" -> JStr("opaque"),"expression" -> expr("SOME_VENDOR_FUNCTION(x)","DAX"))))
    val saved = Document.fromJson(opaque).toOption.get
    assertEquals(Document.readJson(saved.json).toOption.get.raw,opaque)
    assert(Bridge.bind(saved,"orders",bindings.copy(units = Map("opaque" -> "EUR"))).isLeft)
  }
  test("pinned schema rejects invalid types, enums, envelopes, versions and extra properties") {
    val damaged = Vector(replace(raw,"version",JStr("0.1.1")), replace(raw,"datasets",arr()),
      replace(raw,"name",JNum(1)), replace(raw,"ai_context",JNull), replace(raw,"unexpected",JBool(true)),
      obj("semantic_model" -> arr(raw)),replace(raw,"metrics",arr(obj("name" -> JStr("x"),"expression" -> expr("x","ALIEN")))))
    damaged.foreach { j => note(Json.print(j)); assert(Document.fromJson(j).isLeft) }
    assert(Document.readJson("{\"version\":").isLeft)
    assert(Document.readJson(Json.print(raw).replace("\"sales\"","\"sales\",\"name\":\"again\"")).isLeft)
  }
  test("composite keys and relationships resolve without executing joins") {
    val orders = obj("name" -> JStr("orders"),"source" -> JStr("orders"),"fields" -> arr(field("id","String"),field("region","String")))
    val customers = obj("name" -> JStr("customers"),"source" -> JStr("customers"),"fields" -> arr(field("id","String"),field("region","String")),
      "primary_key" -> arr(JStr("id"),JStr("region")),"unique_keys" -> arr(arr(JStr("id"),JStr("region"))))
    val relation = obj("name" -> JStr("customer"),"from" -> JStr("orders"),"to" -> JStr("customers"),
      "from_columns" -> arr(JStr("id"),JStr("region")),"to_columns" -> arr(JStr("id"),JStr("region")))
    val doc = replace(replace(raw,"datasets",arr(orders,customers)),"relationships",arr(relation))
    assertEquals(Document.fromJson(doc).toOption.get.relationships.head.fromColumns,Vector("id","region"))
    assert(Document.fromJson(replace(doc,"relationships",arr(replace(relation,"to_columns",arr(JStr("id")))))).isLeft)
    assert(Document.fromJson(replace(doc,"relationships",arr(replace(relation,"to",JStr("missing"))))).isLeft)
    assert(Document.fromJson(replace(doc,"relationships",arr(replace(relation,"from_columns",arr(JStr("x"),JStr("region")))))).isLeft)
    assert(Document.fromJson(replace(raw,"datasets",arr(orders,orders))).isLeft)
  }
  test("selected metric execution agrees with an authored plan and exact decimals") {
    val d = Document.fromJson(raw).toOption.get
    val model = Bridge.bind(d,"orders",bindings).toOption.get
    val request = Request(metricDefinitions.map(_._1),Vector("segment"))
    val result = model.plan(request).toOption.get.run(rows).toOption.get
    note(result.toString)
    val a = result.groups.find(_.key == Vector(Value.Text("A"))).get.values.flatten
    assertEquals(a,Vector(BigDecimal("0.3"),BigDecimal("0.15"),BigDecimal("0.15"),BigDecimal("0.5"),BigDecimal("0.15"),
      BigDecimal(2),BigDecimal(1),BigDecimal(2),BigDecimal(2),BigDecimal("0.1"),BigDecimal("0.2"),BigDecimal("0.15"),BigDecimal("-0.3")))
    val authored = Model.build("orders",bindings.origin,bindings.grain,Vector(segment),Vector(amount,cost),
      Vector(CoreMetric("revenue","Revenue","EUR",Calculation.Sum("amount")),CoreMetric("cost","Cost","EUR",Calculation.Sum("cost")),
        CoreMetric("profit","Profit","EUR",Calculation.Subtract("revenue","cost")),CoreMetric("margin","Margin","ratio",Calculation.Divide("profit","revenue")))).toOption.get
    val subset = Request(Vector("profit","margin"),Vector("segment"))
    assertEquals(model.plan(subset).toOption.get.run(rows),authored.plan(subset).toOption.get.run(rows))
    assert(Bridge.bind(d,"orders",bindings.copy(units = Map.empty)).isLeft)
    assert(Bridge.bind(d,"orders",bindings.copy(dimensions = bindings.dimensions.updated(FieldKey("orders","segment"),
      Dimension[Sale]("segment","Incorrect kind",Kind.Bool,_ => Value.Bool(true))))).isLeft)
  }
  test("unsupported functions and fields can be stored, but selected execution refuses them") {
    val cases = Vector("CASE WHEN amount > 0 THEN amount END","SUM(customers.amount)","SUM(missing)","SUM(amount); DROP TABLE orders",
      "COUNT(DISTINCT *)","SUM(DISTINCT amount)","SUM(amount) revenue","(revenue")
    cases.foreach { text =>
      val j = replace(raw,"metrics",JArr(Read.array(raw,"metrics") :+ metric("unsupported",text)))
      val d = Document.fromJson(j).toOption.get
      val b = bindings.copy(units = units + ("unsupported" -> "EUR"))
      assert(Bridge.bind(d,"orders",b,Vector("revenue")).isRight)
      val bad = Bridge.bind(d,"orders",b,Vector("unsupported"))
      note(s"$text => $bad")
      assert(bad.isLeft)
    }
    val p = Document.fromJson(replace(raw,"metrics",arr(metric("good","SUM(\"orders\".\"amount\")")))).toOption.get
    assert(Bridge.bind(p,"orders",bindings.copy(units = Map("good" -> "EUR")),Vector("good")).isRight)
    val ambiguous = Document.fromJson(replace(raw,"metrics",arr(metric("X","COUNT(*)"),metric("x","COUNT(*)")))).toOption.get
    assert(Bridge.bind(ambiguous,"orders",bindings.copy(units = Map("X" -> "count","x" -> "count")),Vector("x")).isLeft)
  }
  test("cycles, distinct-prefixed field names, nested expressions and long dependency chains") {
    val cycle = Document.fromJson(replace(raw,"metrics",arr(metric("a","b * 2"),metric("b","a * 2")))).toOption.get
    assert(Bridge.bind(cycle,"orders",bindings.copy(units = Map("a" -> "count","b" -> "count"))).isLeft)
    assert(Expressions.parse("COUNT(distinct_field)").toOption.get.head == Expressions.Token.Aggregate("COUNT",Some(Expressions.Ref(Vector(Expressions.Part("distinct_field",false)))),false))
    val nested = "(" * 20000 + "SUM(amount)" + ")" * 20000
    val d = Document.fromJson(replace(raw,"metrics",arr(metric("deep",nested)))).toOption.get
    assert(Bridge.bind(d,"orders",bindings.copy(units = Map("deep" -> "EUR"))).isRight)
    val metrics = Vector.tabulate(2000)(i => metric(s"m$i",if i == 0 then "COUNT(*)" else s"m${i-1} * 1"))
    val chain = Document.fromJson(replace(raw,"metrics",JArr(metrics))).toOption.get
    val model = Bridge.bind(chain,"orders",bindings.copy(units = (0 until 2000).map(i => s"m$i" -> "count").toMap),Vector("m1999")).toOption.get
    assertEquals(model.plan(Request(Vector("m1999"))).toOption.get.run(rows).toOption.get.groups.head.values,Vector(Some(BigDecimal(3))))
  }
  test("numeric metadata cannot silently round, while numbers inside expressions remain exact") {
    val text = Json.print(raw).replace("1.25","9007199254740993")
    assert(Document.readJson(text).left.toOption.get.exists(_.contains("precision")))
    assert(Document.readJson(Json.print(raw).replace("1.25","1.234567890123456789")).isLeft)
    assert(Document.readJson(Json.print(raw).replace("1.25","1e309")).isLeft)
    val document = Document.fromJson(replace(raw,"metrics",arr(metric("scaled","SUM(amount) * 1.234567890123456789")))).toOption.get
    assert(Bridge.bind(document,"orders",bindings.copy(units = Map("scaled" -> "EUR"))).isRight)
  }
  test("core export preserves derived meanings, source/version, units and temporal metadata") {
    case class Event(at: Long, money: Option[BigDecimal])
    val bucket = FixedBucket.build(1000L).toOption.get
    val time = Time.dimension[Event]("time","Event time",e => Some(e.at),bucket)
    val money = Measure[Event]("money","Money",_.money)
    val metrics = Vector(CoreMetric("revenue","Revenue","EUR",Calculation.Sum("money")),CoreMetric("half","Half","EUR",Calculation.Scale("revenue",BigDecimal("0.5"))))
    val model = Model.build("events",Origin("journal","7"),"one event",Vector(time),Vector(money),metrics).toOption.get
    val exported = Export.model(model,"files/events.parquet",Map("time" -> "event_at","money" -> "eur")).toOption.get
    val reimported = Document.readJson(exported.json).toOption.get
    assertEquals(reimported.raw,exported.raw)
    assert(reimported.extensions.head.data.contains("one event"))
    assert(reimported.datasets.head.fields.find(_.name == "time").get.extensions.head.data.contains("1000"))
    val business = Export.business(reimported).toOption.get
    assertEquals(business.origin,model.origin)
    assertEquals(business.grain,model.grain)
    assertEquals(Export.temporal(reimported.datasets.head.fields.find(_.name == "time").get),Right(Some(okay.semantic.TimeTransform.Fixed(1000L,0L))))
    val rebound = Bridge.bind(reimported,"events",business.bindings(
      Map(FieldKey("events","time") -> time),Map(FieldKey("events","money") -> money))).toOption.get
    val request = Request(Vector("revenue","half"),Vector("time"))
    val events = Vector(Event(-1L,Some(BigDecimal("0.1"))),Event(1L,Some(BigDecimal("0.2"))))
    assertEquals(rebound.plan(request).toOption.get.run(events),model.plan(request).toOption.get.run(events))
    assert(Export.model(model,"files/events.parquet",Map.empty).isLeft)
    val joined = Model.build("events",model.origin,model.grain,Vector(time),Vector(money),metrics,
      Vector(okay.semantic.Relation("customer","events","customers",okay.semantic.Cardinality.ManyToOne,"Customer"))).toOption.get
    assert(Export.model(joined,"events",Map("time" -> "event_at","money" -> "eur")).isLeft)
  }
  test("invalid schema references, scalar bindings and exported metadata are refused by name") {
    val invalid = Vector(
      replace(raw,"ai_context",obj("synonyms" -> arr(JNum(1)))),
      replace(raw,"metrics",arr(obj("name" -> JStr("x"),"expression" -> obj("dialects" -> arr())))),
      replace(raw,"custom_extensions",arr(obj("vendor_name" -> JStr("x"),"data" -> obj()))))
    invalid.foreach(j => assert(Document.fromJson(j).isLeft))
    val stringSum = Document.fromJson(replace(raw,"metrics",arr(metric("bad","SUM(id)")))).toOption.get
    val incompatible = bindings.copy(units = Map("bad" -> "count"),
      measures = bindings.measures.updated(FieldKey("orders","id"),Measure[Sale]("id","Wrong type",_ => Some(BigDecimal(1)))))
    assert(Bridge.bind(stringSum,"orders",incompatible).isLeft)
    val stringMetric = Document.fromJson(replace(raw,"metrics",arr(replace(metric("rows","COUNT(*)"),"datatype",JStr("String"))))).toOption.get
    assert(Bridge.bind(stringMetric,"orders",bindings).isLeft)
    val doc = Document.fromJson(raw).toOption.get
    assert(Export.business(doc).isLeft)
    val corrupted = doc.datasets.head.fields.head.copy(extensions = Vector(Extension("OKAY","{\"time\":{\"kind\":\"fixed\",\"width_micros\":\"0\",\"anchor_micros\":\"0\"}}")))
    assert(Export.temporal(corrupted).isLeft)
  }
  test("deep arbitrary context uses explicit validation worklists") {
    val deep = Json.parse("{\"nested\":" * 20000 + "true" + "}" * 20000)
    assert(Document.fromJson(replace(raw,"ai_context",deep)).isRight)
    assertEquals(Pinned.revision.length,40)
    assertEquals(Pinned.version,"0.2.0.dev0")
  }
