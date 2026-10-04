package okay2.codec

import okay2.{Applicative, Validated}

sealed trait VaColour
object VaColour {
  case object Red extends VaColour
  case object Green extends VaColour
}
final case class VaEmail(value: String)
object VaEmail {
  implicit val schema: Schema[VaEmail] = Schema.refine[VaEmail, String](
    s => if (s.contains('@')) Right(VaEmail(s)) else Left(s"not an email: '$s'"), _.value)
}
final case class VaAddress(city: String, zip: Int = 0)
final case class VaOrder(id: Int, tags: List[String], colour: VaColour, email: Option[VaEmail],
                         amounts: Vector[Double], address: VaAddress, note: Option[String])
sealed trait VaShape
object VaShape {
  case object Dot extends VaShape
  final case class Box(w: Int, h: Int) extends VaShape
}

/**
 * `Validate` is `Json.decode`'s applicative twin (okay-codec's
 * TestValidate): same rules, every refusal, each at its path. The laws
 * are against `decode` itself, on the same fixtures: same verdict on
 * every input, the same value on success, and on failure decode's one
 * message is among Validate's many.
 */
class TestValidate extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private val address: Schema[VaAddress] = implicitly
  private val order: Schema[VaOrder] = implicitly
  private val shape: Schema[VaShape] = implicitly

  private val good = VaOrder(7, List("a", "b"), VaColour.Green, Some(VaEmail("x@y.z")), Vector(1.5, -2.0),
    VaAddress("Wrocław", 50), None)

  private def agree[A](s: Schema[A], j: Json)(implicit loc: munit.Location): Either[Validate.Errors, A] = {
    val v = Validate.decode(s)(j)
    val d = Json.decode(s)(j)
    assertEquals(v.isRight, d.isRight, s"verdicts differ: validate=$v decode=$d")
    (v, d) match {
      case (Right(a), Right(b)) => assertEquals(a, b)
      case (Left(es), Left(m)) =>
        assert(es.nonEmpty)
        assert(es.map(_._2).contains(m), s"decode's message '$m' is not among $es")
      case _ => ()
    }
    v
  }

  test("on a valid document, the same value as decode") {
    agree(order, Json.parse(Json.write(good))): Unit
    agree(shape, Json.parse(Json.write[VaShape](VaShape.Box(2, 3)))): Unit
    agree(shape, Json.parse(Json.write[VaShape](VaShape.Dot))): Unit
    assertEquals(agree(address, Json.parse("""{"city":"Kraków"}""")), Right(VaAddress("Kraków", 0)))
  }

  test("every refusal, each at its path — where decode stops at the first") {
    val j = Json.parse("""{"id":"seven","tags":["a"],"colour":{"Green":{}},"email":"nope","amounts":[1,2],"address":{"city":3},"note":null}""")
    val paths = agree(order, j).left.getOrElse(Vector.empty).map(_._1)
    assertEquals(paths, Vector("id", "email", "address.city"))
  }

  test("a missing required field, an unknown case: decode's own words, at a path") {
    val missing = Json.parse("""{"id":1,"tags":[],"colour":{"Red":{}},"amounts":[],"note":null}""")
    assertEquals(agree(order, missing).left.getOrElse(Vector.empty), Vector("address" -> "missing field 'address' in VaOrder"))
    assertEquals(agree(shape, Json.parse("""{"Triangle":{}}""")).left.getOrElse(Vector.empty),
      Vector("" -> "unknown case 'Triangle' of VaShape"))
  }

  test("the SList rule: a damaged element is skipped, the ones that arrived survive; a damaged optional is absent") {
    import Json._
    val damaged = JObj(Vector(
      "id" -> JNum(1), "tags" -> JArr(Vector(JStr("a"), JErr("torn"), JStr("b"))),
      "colour" -> JObj(Vector("Red" -> JObj(Vector.empty))), "email" -> JErr("torn"),
      "amounts" -> JArr(Vector.empty), "address" -> JObj(Vector("city" -> JStr("x"))),
      "note" -> JNull))
    assertEquals(agree(order, damaged).map(o => (o.tags, o.email)), Right((List("a", "b"), None)))
  }

  test("a genuinely deep value validates — Step's depth safety, for free") {
    var t = Kids(Vector.empty)
    var i = 0
    while (i < 100000) { t = Kids(Vector(t)); i += 1 }
    Validate.decode(implicitly[Schema[Kids]])(Json.parse(Json.write(t))) match {
      case Left(es) => fail(s"expected a value, got $es")
      case Right(back) => assertEquals(DeepJson.kidsDepth(back), 100001)
    }
  }

  test("errors: the form-shaped view, empty on a valid document") {
    assertEquals(Validate.errors(order)(Json.parse(Json.write(good))), Vector.empty)
  }

  test("the bridge: a walk read as a Validated equals decode, and two walks combined keep both sides' paths") {
    val A = implicitly[Applicative[({ type L[X] = Validated[Validate.Errors, X] })#L]]
    val pair = A.pure((x: VaOrder) => (y: VaAddress) => (x, y))
    val bad = Json.parse("""{"id":"x","tags":[],"colour":"Red","amounts":[],"address":{"city":"K"}}""")
    val addr = Json.parse("""{"city": 5}""")
    val o = Validate.validated(order)(bad)
    val a = Validate.validated(address)(addr)
    assertEquals(o.toEither, Validate.decode(order)(bad))
    A.app(A.app(pair, o), a).toEither match {
      case Left(es) =>
        assert(es.exists(_._1 == "id"), es.toString)
        assert(es.exists(_._1 == "city"), es.toString)
        assertEquals(es, Validate.errors(order)(bad) ++ Validate.errors(address)(addr), "both walks' errors, in order")
      case Right(v) => fail(s"valid: $v")
    }
    val goodO = Validate.validated(order)(Json.parse(Json.write(good)))
    val goodA = Validate.validated(address)(Json.parse("""{"city":"Kraków"}"""))
    assertEquals(A.app(A.app(pair, goodO), goodA).toEither, Right((good, VaAddress("Kraków", 0))))
  }
}
