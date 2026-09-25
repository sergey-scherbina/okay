package okay2.codec

sealed trait Kind
object Kind {
  final case class Tagged(tag: String, weight: Double) extends Kind
  final case class Many(items: List[Int]) extends Kind
}

final case class Linked(next: Option[Linked])
object Linked {
  implicit lazy val schema: Schema[Linked] = Schema.derived
}

final case class Inner(name: String, ratio: Double, flags: Vector[Boolean], note: Option[String])
final case class Outer(id: Long, count: Int, ok: Boolean, ch: Char, inner: Inner,
                       kinds: List[Kind], maybe: Option[Inner], raw: Array[Byte],
                       label: String = "default")

/** The strict door gives the SAME answer as the lossless one on a
 * well-formed document, and refuses where the lossless one still
 * projects (okay-codec's TestJsonStrict). Every schema here is derived
 * where it is needed, with no declaration. */
class TestJsonStrict extends munit.FunSuite {

  private val corpus: List[Outer] = List(
    Outer(1L, 2, true, 'x', Inner("a", 0.5, Vector(true, false), Some("n")),
      List(Kind.Tagged("t", 1.25), Kind.Many(List(1, 2, 3))), None, Array[Byte](1, 2, 3)),
    Outer(-9L, 0, false, 'é', Inner("quote \" and \\ slash \n newline", -3.0e10, Vector(), None),
      Nil, Some(Inner("in", 2.0, Vector(true), Some("é ünïcödé"))), Array.empty[Byte], label = "set"),
    Outer(Long.MaxValue, Int.MinValue, true, ' ', Inner("", 1e-7, Vector(false), Some("")),
      List(Kind.Many(Nil)), None, Array[Byte](-128, 127)))

  /** Array[Byte] has no structural equality: compare through a view */
  private def norm(o: Outer): Product =
    (o.id, o.count, o.ok, o.ch, o.inner, o.kinds, o.maybe, o.raw.toSeq, o.label)

  test("law: the strict door answers exactly what the lossless door answers, over the corpus") {
    for (o <- corpus) {
      val text = Json.write(o)
      val strict = Json.readStrict[Outer](text).map(norm)
      assertEquals(strict, Json.read[Outer](text).map(norm), text)
      assertEquals(strict, Right(norm(o)))
    }
  }

  test("law: the same with whitespace everywhere the grammar allows it") {
    val o = corpus.head
    val spaced = Json.write(o).replace(":", " : ").replace(",", " ,\n  ").replace("{", "{ ")
      .replace("}", " }").replace("[", "[ ").replace("]", " ]")
    assertEquals(Json.readStrict[Outer](spaced).map(norm), Json.read[Outer](spaced).map(norm))
    assertEquals(Json.readStrict[Outer](spaced).map(norm), Right(norm(o)))
  }

  test("law: an undeclared field is ignored, as the lossless decoder ignores it") {
    val text = """{"name":"a","ratio":1.5,"flags":[true],"note":null,"extra":{"deep":[1,2,{"x":"y"}]},"more":"s"}"""
    assertEquals(Json.readStrict[Inner](text), Json.read[Inner](text))
    assertEquals(Json.readStrict[Inner](text), Right(Inner("a", 1.5, Vector(true), None)))
  }

  test("law: an absent field takes its default, then None-if-optional, then the refusal") {
    val withDefault = """{"id":1,"count":2,"ok":true,"ch":"c","inner":{"name":"n","ratio":1,"flags":[]},"kinds":[],"raw":""}"""
    val s = Json.readStrict[Outer](withDefault).map(norm)
    assertEquals(s, Json.read[Outer](withDefault).map(norm))
    assert(s.exists(_.productElement(8) == "default"), s"default not applied: $s")
    val missingRequired = """{"count":2,"ok":true,"ch":"c","inner":{"name":"n","ratio":1,"flags":[]},"kinds":[],"raw":""}"""
    assert(Json.readStrict[Outer](missingRequired).isLeft)
    assert(Json.read[Outer](missingRequired).isLeft)
  }

  test("the trade: a truncated document is Left here") {
    val text = Json.write(Inner("a", 1.5, Vector(true, false), Some("n")))
    val cut = text.take(text.length - 8)
    assert(Json.readStrict[Inner](cut).isLeft, s"strict accepted a truncated document: $cut")
    val _ = Json.read[Inner](cut)
  }

  test("the trade: damage, a stray character and trailing input are all Left") {
    assert(Json.readStrict[Inner]("""{"name":"a","ratio":1.5,"flags":[true],"note":null} tail""").isLeft)
    assert(Json.readStrict[Inner]("""{"name":"a" "ratio":1.5,"flags":[true],"note":null}""").isLeft)
    assert(Json.readStrict[Inner]("{\"name\":\"a\u0001b\",\"ratio\":1.5,\"flags\":[true],\"note\":null}").isLeft)
    assert(Json.readStrict[Inner]("""{"name":"a","ratio":1.5,"flags":[true,],"note":null}""").isLeft)
    assert(Json.readStrict[Inner]("""{"name":"a","ratio":01,"flags":[],"note":null}""").isLeft)
  }

  test("a sum is the one-entry object write produces, and any other shape is refused") {
    for (k <- List[Kind](Kind.Tagged("t", 2.5), Kind.Many(List(7)))) {
      val text = Json.write(k)
      assertEquals(Json.readStrict[Kind](text), Right(k))
      assertEquals(Json.readStrict[Kind](text), Json.read[Kind](text))
    }
    assert(Json.readStrict[Kind]("""{"Tagged":{"tag":"t","weight":1},"Many":{"items":[]}}""").isLeft)
    assert(Json.readStrict[Kind]("""{"Nope":{}}""").isLeft)
  }

  test("numbers: ints read as the lossless decoder reads them") {
    assertEquals(Json.readStrict[Int]("42"), Json.read[Int]("42"))
    assertEquals(Json.readStrict[Long]("-9007199254740993"), Json.read[Long]("-9007199254740993"))
    assertEquals(Json.readStrict[Double]("6.02e23"), Right(6.02e23))
    assertEquals(Json.readStrict[Double]("-0.0"), Right(-0.0))
    assertEquals(Json.readStrict[Int]("1.9"), Json.read[Int]("1.9"))
  }

  // stack-safety-json: past NativeThreshold the strict reader walks on
  // Cont, but an UNKNOWN field was skipped by a direct call back into the
  // field loop, one frame per skipped field
  test("an object past the threshold skips any number of unknown fields without the stack") {
    val depth = 40
    val unknown = (0 until 200000).map(i => s""""x$i":$i""").mkString(",")
    val doc = ("{\"next\":" * depth) + "{" + unknown + "}" + ("}" * depth)
    def depthOf(c: Linked): Int = { var d = 1; var at = c; while (at.next.isDefined) { d += 1; at = at.next.get }; d }
    assertEquals(Json.readStrict[Linked](doc).map(depthOf), Right(depth + 1))
  }
}
