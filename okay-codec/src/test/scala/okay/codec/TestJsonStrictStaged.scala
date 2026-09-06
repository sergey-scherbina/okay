package okay.codec

/**
 * json-strict-staged: the staged strict reader is the interpreted
 * strict reader's answer, faster. The law is equality with
 * `Json.readStrict` on everything -- well-formed, whitespace,
 * unknown fields, defaults, and the refusals -- and, through it,
 * equality with the lossless `Json.read` on well-formed input.
 */
class TestJsonStrictStaged extends munit.FunSuite:

  sealed trait Kind
  object Kind:
    final case class Tagged(tag: String, weight: Double) extends Kind
    final case class Many(items: List[Int]) extends Kind
  given Schema[Kind.Tagged] = Schema.derived
  given Schema[Kind.Many] = Schema.derived
  given Schema[Kind] = Schema.derived

  case class Inner(name: String, ratio: Double, flags: Vector[Boolean], note: Option[String])
  given Schema[Inner] = Schema.derived
  case class Outer(id: Long, count: Int, ok: Boolean, inner: Inner,
                   kinds: List[Kind], maybe: Option[Inner], label: String = "default")
  given Schema[Outer] = Schema.derived

  private val outer = Staged.strict[Outer]
  private val inner = Staged.strict[Inner]
  private val kind = Staged.strict[Kind]

  private val corpus: List[Outer] = List(
    Outer(1L, 2, true, Inner("a", 0.5, Vector(true, false), Some("n")),
      List(Kind.Tagged("t", 1.25), Kind.Many(List(1, 2, 3))), None),
    Outer(-9L, 0, false, Inner("quote \" and \\ slash \n newline", -3.0e10, Vector(), None),
      Nil, Some(Inner("in", 2.0, Vector(true), Some("é ünïcödé"))), label = "set"),
    Outer(Long.MaxValue, Int.MinValue, true, Inner("", 1e-7, Vector(false), Some("")),
      List(Kind.Many(Nil)), None)
  )

  test("law: staged == interpreted strict == lossless, over the corpus") {
    for o <- corpus do
      val text = Json.write(o)
      assertEquals(outer.decode(text), Json.readStrict[Outer](text), text)
      assertEquals(outer.decode(text), Json.read[Outer](text), text)
      assertEquals(outer.decode(text), Right(o))
  }

  test("law: the same with whitespace everywhere the grammar allows it") {
    val text = Json.write(corpus.head)
    val spaced = text.replace(":", " : ").replace(",", " ,\n  ").replace("{", "{ ").replace("}", " }").replace("[", "[ ").replace("]", " ]")
    assertEquals(outer.decode(spaced), Json.readStrict[Outer](spaced))
    assertEquals(outer.decode(spaced), Right(corpus.head))
  }

  test("law: unknown fields skipped, defaults applied, missing required refused -- as the interpreted reader") {
    val extra = """{"name":"a","ratio":1.5,"flags":[true],"note":null,"extra":{"deep":[1,2,{"x":"y"}]},"more":"s"}"""
    assertEquals(inner.decode(extra), Json.readStrict[Inner](extra))
    assertEquals(inner.decode(extra), Right(Inner("a", 1.5, Vector(true), None)))
    val withDefault = """{"id":1,"count":2,"ok":true,"inner":{"name":"n","ratio":1,"flags":[]},"kinds":[]}"""
    assertEquals(outer.decode(withDefault), Json.readStrict[Outer](withDefault))
    assert(outer.decode(withDefault).exists(_.label == "default"))
    val missing = """{"count":2,"ok":true,"inner":{"name":"n","ratio":1,"flags":[]},"kinds":[]}"""
    assertEquals(outer.decode(missing).isLeft, Json.readStrict[Outer](missing).isLeft)
    assert(outer.decode(missing).isLeft)
  }

  test("law: sums by the one-entry object, and any other shape refused") {
    for k <- List[Kind](Kind.Tagged("t", 2.5), Kind.Many(List(7))) do
      val text = Json.write(k)
      assertEquals(kind.decode(text), Right(k))
      assertEquals(kind.decode(text), Json.readStrict[Kind](text))
    assert(kind.decode("""{"Tagged":{"tag":"t","weight":1},"Many":{"items":[]}}""").isLeft)
    assert(kind.decode("""{"Nope":{}}""").isLeft)
  }

  test("the trade, staged: truncated, damaged, stray and trailing input are Left, as the interpreted reader answers") {
    val good = Json.write(Inner("a", 1.5, Vector(true, false), Some("n")))
    val cut = good.take(good.length - 8)
    for bad <- List(cut,
                    good + " tail",
                    """{"name":"a" "ratio":1.5,"flags":[true],"note":null}""",
                    """{"name":"a","ratio":1.5,"flags":[true,],"note":null}""",
                    """{"name":"a","ratio":01,"flags":[],"note":null}""") do
      assert(inner.decode(bad).isLeft, s"staged accepted: $bad")
      assertEquals(inner.decode(bad).isLeft, Json.readStrict[Inner](bad).isLeft)
  }

  test("numbers: ints truncate as the interpreted reader truncates") {
    val ints = Staged.strict[Int]
    assertEquals(ints.decode("42"), Json.readStrict[Int]("42"))
    assertEquals(ints.decode("1.9"), Json.readStrict[Int]("1.9"))
    assertEquals(Staged.strict[Double].decode("6.02e23"), Right(6.02e23))
  }
