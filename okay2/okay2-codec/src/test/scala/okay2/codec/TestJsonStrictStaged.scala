package okay2.codec

sealed trait SsKind
object SsKind {
  final case class Tagged(tag: String, weight: Double) extends SsKind
  final case class Many(items: List[Int]) extends SsKind
}
final case class SsInner(name: String, ratio: Double, flags: Vector[Boolean], note: Option[String])
final case class SsOuter(id: Long, count: Int, ok: Boolean, inner: SsInner,
                         kinds: List[SsKind], maybe: Option[SsInner], label: String = "default")

/**
 * The staged strict reader is the interpreted strict reader's answer
 * (okay-codec's TestJsonStrictStaged): equal to `Json.readStrict` on
 * everything — well-formed, whitespace, unknown fields, defaults, the
 * refusals — and through it to the lossless `Json.read` on well-formed
 * input.
 */
class TestJsonStrictStaged extends munit.FunSuite {

  private val outer = Staged.strict[SsOuter]
  private val inner = Staged.strict[SsInner]
  private val kind = Staged.strict[SsKind]

  private val corpus: List[SsOuter] = List(
    SsOuter(1L, 2, true, SsInner("a", 0.5, Vector(true, false), Some("n")),
      List(SsKind.Tagged("t", 1.25), SsKind.Many(List(1, 2, 3))), None),
    SsOuter(-9L, 0, false, SsInner("quote \" and \\ slash \n newline", -3.0e10, Vector(), None),
      Nil, Some(SsInner("in", 2.0, Vector(true), Some("é ünïcödé"))), label = "set"),
    SsOuter(Long.MaxValue, Int.MinValue, true, SsInner("", 1e-7, Vector(false), Some("")),
      List(SsKind.Many(Nil)), None))

  test("law: staged == interpreted strict == lossless, over the corpus") {
    for (o <- corpus) {
      val text = Json.write(o)
      assertEquals(outer.decode(text), Json.readStrict[SsOuter](text), text)
      assertEquals(outer.decode(text), Json.read[SsOuter](text), text)
      assertEquals(outer.decode(text), Right(o))
    }
  }

  test("law: the same with whitespace everywhere the grammar allows it") {
    val text = Json.write(corpus.head)
    val spaced = text.replace(":", " : ").replace(",", " ,\n  ").replace("{", "{ ").replace("}", " }").replace("[", "[ ").replace("]", " ]")
    assertEquals(outer.decode(spaced), Json.readStrict[SsOuter](spaced))
    assertEquals(outer.decode(spaced), Right(corpus.head))
  }

  test("law: unknown fields skipped, defaults applied, missing required refused — as the interpreted reader") {
    val extra = """{"name":"a","ratio":1.5,"flags":[true],"note":null,"extra":{"deep":[1,2,{"x":"y"}]},"more":"s"}"""
    assertEquals(inner.decode(extra), Json.readStrict[SsInner](extra))
    assertEquals(inner.decode(extra), Right(SsInner("a", 1.5, Vector(true), None)))
    val withDefault = """{"id":1,"count":2,"ok":true,"inner":{"name":"n","ratio":1,"flags":[]},"kinds":[]}"""
    assertEquals(outer.decode(withDefault), Json.readStrict[SsOuter](withDefault))
    assert(outer.decode(withDefault).exists(_.label == "default"))
    val missing = """{"count":2,"ok":true,"inner":{"name":"n","ratio":1,"flags":[]},"kinds":[]}"""
    assertEquals(outer.decode(missing).isLeft, Json.readStrict[SsOuter](missing).isLeft)
    assert(outer.decode(missing).isLeft)
  }

  test("law: sums by the one-entry object, and any other shape refused") {
    for (k <- List[SsKind](SsKind.Tagged("t", 2.5), SsKind.Many(List(7)))) {
      val text = Json.write(k)
      assertEquals(kind.decode(text), Right(k))
      assertEquals(kind.decode(text), Json.readStrict[SsKind](text))
    }
    assert(kind.decode("""{"Tagged":{"tag":"t","weight":1},"Many":{"items":[]}}""").isLeft)
    assert(kind.decode("""{"Nope":{}}""").isLeft)
  }

  test("truncated, damaged, stray and trailing input are Left, as the interpreted reader answers") {
    val good = Json.write(SsInner("a", 1.5, Vector(true, false), Some("n")))
    for (bad <- List(good.take(good.length - 8),
                     good + " tail",
                     """{"name":"a" "ratio":1.5,"flags":[true],"note":null}""",
                     """{"name":"a","ratio":1.5,"flags":[true,],"note":null}""",
                     """{"name":"a","ratio":01,"flags":[],"note":null}""")) {
      assert(inner.decode(bad).isLeft, s"staged accepted: $bad")
      assertEquals(inner.decode(bad).isLeft, Json.readStrict[SsInner](bad).isLeft)
    }
  }

  test("numbers: ints as the interpreted reader reads them; a deep recursive type past the threshold") {
    val ints = Staged.strict[Int]
    assertEquals(ints.decode("42"), Json.readStrict[Int]("42"))
    assertEquals(ints.decode("1.9"), Json.readStrict[Int]("1.9"))
    assertEquals(Staged.strict[Double].decode("6.02e23"), Right(6.02e23))
    val deep = DeepJson.kidsChain(1000)
    assertEquals(Staged.strict[Kids].decode(deep).map(DeepJson.kidsDepth), Right(1001))
  }
}
