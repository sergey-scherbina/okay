package okay.dlm.remote

import munit.FunSuite
import okay.codec.Json
import okay.codec.Json.*
import okay.dlm.{Head, Judge}

class TestSystemOne extends FunSuite:

  val question = Judge.Question(Vector("billing" -> "refunds and invoices", "tech" -> "bugs and outages"),
    "which team?")

  val reply = """{"answers":{"q":{"choice":"billing","probabilities":{"billing":0.94,"tech":0.06},
    "confidence":0.91,"answer_confidence":0.91}},"usage":{"input_tokens":42,"output_tokens":0}}"""

  test("a choice question is the vendors' documented shape: state, questions, criteria") {
    val body = SystemOne.encodeChoice("billed twice, refund please", question)
    val fs = body match { case JObj(fs) => fs; case _ => fail("not an object") }
    assertEquals(fs.collectFirst { case ("state", JObj(s)) => s }, Some(Vector("text" -> JStr("billed twice, refund please"))))
    val q = fs.collectFirst { case ("questions", JObj(qs)) => qs }.get.collectFirst { case ("q", JObj(q)) => q }.get
    assertEquals(q.collectFirst { case ("type", JStr(t)) => t }, Some("choice"))
    assertEquals(q.collectFirst { case ("instructions", JStr(i)) => i }, Some("which team?"))
    assertEquals(q.collectFirst { case ("criteria", JObj(c)) => c },
      Some(Vector("billing" -> JStr("refunds and invoices"), "tech" -> JStr("bugs and outages"))))
    // an option with no description is described by its own name, never sent empty
    val bare = SystemOne.encodeChoice("x", Judge.Question.of(Vector("a")))
    assert(Json.print(bare).contains("\"a\":\"a\""))
  }

  test("an answer decodes to the choice, the probabilities and the confidence") {
    assertEquals(SystemOne.decodeChoice(reply), Right(SystemOne.Answer("billing", Map("billing" -> 0.94, "tech" -> 0.06), Some(0.91))))
    assert(SystemOne.decodeChoice("""{"answers":{}}""").isLeft)
    assert(SystemOne.decodeChoice("not json").isLeft)
    assertEquals(SystemOne.decodeNoul("""{"answers":{"q":{"noul":0.83}}}"""), Right(0.83))
    assertEquals(SystemOne.decodeScore("""{"answers":{"q":{"score":2.4,"probabilities":{"1":0.1,"2":0.4,"3":0.5},"confidence":0.7}}}"""),
      Right(SystemOne.Scored(2.4, Map("1" -> 0.1, "2" -> 0.4, "3" -> 0.5), Some(0.7))))
  }

  test("the client posts to the configured host with the bearer key, and is the model's judge") {
    given wire: Wire.Canned = Wire.canned(reply)
    val jev = Jev.client("secret", base = "https://example.test")
    val a = jev.choose("billed twice", question)
    assertEquals(a.map(_.choice), Right("billing"))
    val (url, headers, body) = wire.seen.head
    assertEquals(url, "https://example.test/v1/systemone")
    assertEquals(headers("Authorization"), "Bearer secret")
    assert(body.contains("\"type\":\"choice\""))
    // as a judge: the vendor's choice ranks first, the probabilities are over what was asked
    val j = jev.judge()
    assertEquals(j.name, "jev")
    val c = j.choose("billed twice", question).get
    assertEquals((c.best, c.runnerUp, c.confidence), ("billing", Some("tech"), Some(0.91)))
    assertEqualsDouble(c.margin, 0.88, 1e-9)
    // …and a head over it answers on the margin like any other
    val head = new Head(j, question, margin = 0.5f)
    assertEquals(head.of("billed twice"), Some("billing"))
    assertEquals(head.scores("billed twice").head._1, "billing")
  }

  test("Laya is the same wire from a container of one's own, key optional") {
    given wire: Wire.Canned = Wire.canned(reply)
    val laya = Laya.client()
    laya.choose("x", question): Unit
    val (url, headers, _) = wire.seen.head
    assertEquals(url, "http://127.0.0.1:8000/v1/systemone")
    assert(!headers.contains("Authorization"))
    assertEquals(Laya.client(apiKey = Some("k")).judge().name, "laya")
  }

  test("a wire that fails is an abstention with a reason, never a guess — and the guarded judge retires it") {
    var why = Vector.empty[String]
    given Wire = Wire.failing("HTTP 503")
    val j = Jev.client("k").judge(why :+= _)
    assertEquals(j.choose("x", question), None)
    assertEquals(why, Vector("HTTP 503"))
    val guarded = Jev.judge("k", report = why :+= _)
    assertEquals(guarded.choose("x", question), None)
    // ours behind it keeps the model answering
    val ours = Judge.probe(okay.dlm.Exemplars.compile(Vector("tech" -> "the site is down", "billing" -> "charged twice"),
      okay.rag.Vectors.hashing(64), "hashing-64"), okay.rag.Vectors.hashing(64))
    assertEquals(Judge.orElse(guarded, ours).choose("the site is down", Judge.Question.of(Vector("tech", "billing"))).map(_.best), Some("tech"))
  }

  test("the vendor's choice stands even where its probabilities disagree, and an option not asked is dropped") {
    given Wire = Wire.canned("""{"answers":{"q":{"choice":"tech","probabilities":{"billing":0.6,"tech":0.4,"other":0.9}}}}""")
    val c = Laya.client().judge().choose("x", question).get
    assertEquals(c.best, "tech")
    assertEquals(c.probabilities.map(_._1), Vector("tech", "billing"))
    assertEquals(c.confidence, None)
  }

  test("from the environment: nothing configured is nothing plugged, and the model stays ours") {
    given Wire = Wire.canned(reply)
    if !sys.env.contains(Jev.keyVariable) then assertEquals(Jev.fromEnv().isDefined, false)
    if !sys.env.contains("LAYA_BASE") then assertEquals(Laya.fromEnv().isDefined, false)
  }

  test("a remote encoder over the embeddings wire, named by host and model") {
    given wire: Wire.Canned = Wire.canned("""{"data":[{"embedding":[0.1,0.2,0.3],"index":0}],"model":"m"}""")
    val e = Embeddings.openAi("https://models.example.test/", "minilm", Some("k"))
    assertEquals(e.name, "models.example.test/minilm")
    assertEquals(e("привет").toVector, Vector(0.1f, 0.2f, 0.3f))
    assertEquals(e.dim, 3)
    val (url, headers, body) = wire.seen.head
    assertEquals(url, "https://models.example.test/v1/embeddings")
    assertEquals(headers("Authorization"), "Bearer k")
    assert(body.contains("\"input\":\"привет\""))
    val broken: Wire = Wire.failing("refused")
    intercept[IllegalStateException](Embeddings.openAi("https://x.test", "m")(using broken)("x"))
  }
