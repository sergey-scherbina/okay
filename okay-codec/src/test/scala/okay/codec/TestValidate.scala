package okay.codec

/**
 * specs/schema-fold.md, stage 3: `Validate` is `Json.decode`'s
 * applicative twin — same rules, every refusal, each at its path.
 * The laws are against `decode` itself, on the same fixtures, not
 * against remembered strings: same verdict on every input, the same
 * value on success, and on failure decode's one message is among
 * Validate's many.
 */
class TestValidate extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  enum Colour derives Schema:
    case Red, Green
  final case class Email(value: String)
  given Schema[Email] = Schema.refine[Email, String](
    s => if s.contains('@') then Right(Email(s)) else Left(s"not an email: '$s'"), _.value)
  final case class Address(city: String, zip: Int = 0)
  given Schema[Address] = Schema.derived
  final case class Order(id: Int, tags: List[String], colour: Colour, email: Option[Email],
                         amounts: Vector[Double], address: Address, note: Option[String])
  given Schema[Order] = Schema.derived
  enum Shape derives Schema:
    case Dot
    case Box(w: Int, h: Int)
  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  val good = Order(7, List("a", "b"), Colour.Green, Some(Email("x@y.z")), Vector(1.5, -2.0), Address("Wrocław", 50), None)

  def agree[A](s: Schema[A], j: Json)(using loc: munit.Location): Either[Validate.Errors, A] =
    val v = Validate.decode(s)(j)
    val d = Json.decode(s)(j)
    assertEquals(v.isRight, d.isRight, s"verdicts differ: validate=$v decode=$d")
    (v, d) match
      case (Right(a), Right(b)) => assertEquals(a, b)
      case (Left(es), Left(m)) =>
        assert(es.nonEmpty)
        assert(es.map(_._2).contains(m), s"decode's message '$m' is not among ${es}")
      case _ => ()
    v

  test("on a valid document, the same value as decode") {
    val _ = agree(summon[Schema[Order]], Json.parse(Json.write(good)))
    val _ = agree(summon[Schema[Shape]], Json.parse(Json.write(Shape.Box(2, 3))))
    val _ = agree(summon[Schema[Shape]], Json.parse(Json.write(Shape.Dot)))
    // a defaulted field absent takes its default, an optional one absent is None
    assertEquals(agree(summon[Schema[Address]], Json.parse("""{"city":"Kraków"}""")), Right(Address("Kraków", 0)))
  }

  test("every refusal, each at its path — where decode stops at the first") {
    val j = Json.parse("""{"id":"seven","tags":["a"],"colour":{"Green":{}},"email":"nope","amounts":[1,2],"address":{"city":3},"note":null}""")
    val v = agree(summon[Schema[Order]], j)
    val paths = v.left.getOrElse(Vector.empty).map(_._1)
    assertEquals(paths, Vector("id", "email", "address.city"))
  }

  test("a missing required field, an unknown case: decode's own words, at a path") {
    val missing = Json.parse("""{"id":1,"tags":[],"colour":{"Red":{}},"amounts":[],"note":null}""")
    val v = agree(summon[Schema[Order]], missing)
    assertEquals(v.left.getOrElse(Vector.empty), Vector("address" -> "missing field 'address' in Order"))
    val unknown = Json.parse("""{"Triangle":{}}""")
    assertEquals(agree(summon[Schema[Shape]], unknown).left.getOrElse(Vector.empty),
      Vector("" -> "unknown case 'Triangle' of Shape"))
  }

  test("the SList rule: a damaged element is skipped, the ones that arrived survive; a damaged optional is absent") {
    val damaged = Json.JObj(Vector(
      "id" -> Json.JNum(1), "tags" -> Json.JArr(Vector(Json.JStr("a"), Json.JErr("torn"), Json.JStr("b"))),
      "colour" -> Json.JObj(Vector("Red" -> Json.JObj(Vector.empty))), "email" -> Json.JErr("torn"),
      "amounts" -> Json.JArr(Vector.empty), "address" -> Json.JObj(Vector("city" -> Json.JStr("x"))),
      "note" -> Json.JNull))
    assertEquals(agree(summon[Schema[Order]], damaged).map(o => (o.tags, o.email)), Right((List("a", "b"), None)))
  }

  test("a genuinely deep value validates — Step's depth safety, for free") {
    var t = Tree(Vector.empty)
    var i = 0
    while i < 100000 do { t = Tree(Vector(t)); i += 1 }
    val j = Json.parse(Json.write(t))
    Validate.decode(summon[Schema[Tree]])(j) match
      case Left(es) => fail(s"expected a value, got $es")
      case Right(back) =>
        var d = 0; var at = back
        while at.kids.nonEmpty do { d += 1; at = at.kids.head }
        assertEquals(d, 100000)
  }

  test("errors: the form-shaped view, empty on a valid document") {
    assertEquals(Validate.errors(summon[Schema[Order]])(Json.parse(Json.write(good))), Vector.empty)
  }
