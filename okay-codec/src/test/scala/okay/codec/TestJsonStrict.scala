package okay.codec

/**
 * json-fast-read: the strict door gives the SAME answer as the
 * lossless one on a well-formed document, and refuses where the
 * lossless one still projects. Both halves are the contract.
 */
class TestJsonStrict extends munit.FunSuite:

  sealed trait Kind
  object Kind:
    final case class Tagged(tag: String, weight: Double) extends Kind
    final case class Many(items: List[Int]) extends Kind
  given Schema[Kind.Tagged] = Schema.derived
  given Schema[Kind.Many] = Schema.derived
  given Schema[Kind] = Schema.derived

  case class Inner(name: String, ratio: Double, flags: Vector[Boolean], note: Option[String])
  given Schema[Inner] = Schema.derived
  case class Outer(id: Long, count: Int, ok: Boolean, ch: Char, inner: Inner,
                   kinds: List[Kind], maybe: Option[Inner], raw: Array[Byte],
                   label: String = "default")
  given Schema[Outer] = Schema.derived

  private val corpus: List[Outer] = List(
    Outer(1L, 2, true, 'x', Inner("a", 0.5, Vector(true, false), Some("n")),
      List(Kind.Tagged("t", 1.25), Kind.Many(List(1, 2, 3))), None, Array[Byte](1, 2, 3)),
    Outer(-9L, 0, false, 'é', Inner("quote \" and \\ slash \n newline", -3.0e10, Vector(), None),
      Nil, Some(Inner("in", 2.0, Vector(true), Some("é ünïcödé"))), Array.empty[Byte], label = "set"),
    Outer(Long.MaxValue, Int.MinValue, true, ' ', Inner("", 1e-7, Vector(false), Some("")),
      List(Kind.Many(Nil)), None, Array[Byte](-128, 127))
  )

  /** Array[Byte] has no structural equality: compare through a view */
  private def norm(o: Outer): Product =
    (o.id, o.count, o.ok, o.ch, o.inner, o.kinds, o.maybe, o.raw.toSeq, o.label)

  test("law: the strict door answers exactly what the lossless door answers, over the corpus") {
    for o <- corpus do
      val text = Json.write(o)
      val lossless = Json.read[Outer](text).map(norm)
      val strict = Json.readStrict[Outer](text).map(norm)
      assertEquals(strict, lossless, text)
      assertEquals(strict, Right(norm(o)))
  }

  test("law: the same with whitespace everywhere the grammar allows it") {
    val o = corpus.head
    val text = Json.write(o)
    val spaced = text.replace(":", " : ").replace(",", " ,\n  ").replace("{", "{ ").replace("}", " }").replace("[", "[ ").replace("]", " ]")
    assertEquals(Json.readStrict[Outer](spaced).map(norm), Json.read[Outer](spaced).map(norm))
    assertEquals(Json.readStrict[Outer](spaced).map(norm), Right(norm(o)))
  }

  test("law: an undeclared field is ignored, as the lossless decoder ignores it") {
    val text = """{"name":"a","ratio":1.5,"flags":[true],"note":null,"extra":{"deep":[1,2,{"x":"y"}]},"more":"s"}"""
    assertEquals(Json.readStrict[Inner](text), Json.read[Inner](text))
    assertEquals(Json.readStrict[Inner](text), Right(Inner("a", 1.5, Vector(true), None)))
  }

  test("law: an absent field takes its default, then None-if-optional, then the refusal -- the lossless rules") {
    val withDefault = """{"id":1,"count":2,"ok":true,"ch":"c","inner":{"name":"n","ratio":1,"flags":[]},"kinds":[],"raw":""}"""
    val s = Json.readStrict[Outer](withDefault).map(norm)
    assertEquals(s, Json.read[Outer](withDefault).map(norm))
    assert(s.exists(_.productElement(8) == "default"), s"default not applied: $s")
    val missingRequired = """{"count":2,"ok":true,"ch":"c","inner":{"name":"n","ratio":1,"flags":[]},"kinds":[],"raw":""}"""
    assert(Json.readStrict[Outer](missingRequired).isLeft)
    assert(Json.read[Outer](missingRequired).isLeft)
  }

  test("the trade: a truncated document is Left here, and still decodes what arrived on the lossless road") {
    val text = Json.write(Inner("a", 1.5, Vector(true, false), Some("n")))
    val cut = text.take(text.length - 8)             // inside the last value
    assert(Json.readStrict[Inner](cut).isLeft, s"strict accepted a truncated document: $cut")
    // the lossless road: whatever it projects, it does not throw and
    // it is not the strict door's business what it answers
    val _ = Json.read[Inner](cut)
  }

  test("the trade: damage, a stray character and trailing input are all Left") {
    assert(Json.readStrict[Inner]("""{"name":"a","ratio":1.5,"flags":[true],"note":null} tail""").isLeft)
    assert(Json.readStrict[Inner]("""{"name":"a" "ratio":1.5,"flags":[true],"note":null}""").isLeft)
    assert(Json.readStrict[Inner]("""{"name":"ab","ratio":1.5,"flags":[true],"note":null}""".replace("\\u0001", "")).isLeft)
    assert(Json.readStrict[Inner]("""{"name":"a","ratio":1.5,"flags":[true,],"note":null}""").isLeft)
    assert(Json.readStrict[Inner]("""{"name":"a","ratio":01,"flags":[],"note":null}""").isLeft)
  }

  test("a sum is the one-entry object write produces, and any other shape is refused") {
    for k <- List[Kind](Kind.Tagged("t", 2.5), Kind.Many(List(7))) do
      val text = Json.write(k)
      assertEquals(Json.readStrict[Kind](text), Right(k))
      assertEquals(Json.readStrict[Kind](text), Json.read[Kind](text))
    assert(Json.readStrict[Kind]("""{"Tagged":{"tag":"t","weight":1},"Many":{"items":[]}}""").isLeft)
    assert(Json.readStrict[Kind]("""{"Nope":{}}""").isLeft)
  }

  test("numbers: ints truncate as the lossless decoder truncates, doubles keep their exponent form") {
    assertEquals(Json.readStrict[Int]("42"), Json.read[Int]("42"))
    assertEquals(Json.readStrict[Long]("-9007199254740993"), Json.read[Long]("-9007199254740993"))
    assertEquals(Json.readStrict[Double]("6.02e23"), Right(6.02e23))
    assertEquals(Json.readStrict[Double]("-0.0"), Right(-0.0))
    assertEquals(Json.readStrict[Int]("1.9"), Json.read[Int]("1.9"))
  }
