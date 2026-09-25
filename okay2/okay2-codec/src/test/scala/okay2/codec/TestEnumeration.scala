package okay2.codec

sealed trait Hue
object Hue {
  case object Red extends Hue
  case object Green extends Hue
  case object Blue extends Hue
  val values: Vector[Hue] = Vector(Red, Green, Blue)
  implicit val schema: Schema[Hue] = Schema.enumeration[Hue, String](values, _.toString.toLowerCase)
}
final case class Paint(colour: Hue, litres: Int)

/** `Schema.enumeration`: a refinement over a finite vocabulary, the
 * vocabulary kept on the node (okay-codec's TestEnumeration, wire half). */
class TestEnumeration extends munit.FunSuite {

  test("the name goes out, the name comes back, an unknown name names the vocabulary") {
    assertEquals(Json.write[Hue](Hue.Green), "\"green\"")
    assertEquals(Json.read[Hue]("\"blue\""), Right(Hue.Blue))
    assertEquals(Json.read[Paint]("""{"colour":"red","litres":2}"""), Right(Paint(Hue.Red, 2)))
    Json.read[Hue]("\"puce\"") match {
      case Left(e) => assert(e.contains("puce") && e.contains("red, green, blue"), e)
      case Right(v) => fail(s"decoded $v")
    }
  }

  test("the vocabulary is on the node; a plain refine has none") {
    Hue.schema match {
      case i: Schema.SIso[_, _] => assertEquals(i.vocabulary: Option[Any], Some(Vector("red", "green", "blue")): Option[Any])
      case other => fail(s"expected an iso, got $other")
    }
    Schema.refine[Hue, String](s => Hue.values.find(_.toString.equalsIgnoreCase(s)).toRight(s"unknown '$s'"), _.toString) match {
      case i: Schema.SIso[_, _] => assertEquals(i.vocabulary: Option[Any], None: Option[Any])
      case other => fail(s"expected an iso, got $other")
    }
  }

  test("the vocabulary can be any underlying type") {
    sealed trait Level
    case object One extends Level
    case object Two extends Level
    implicit val level: Schema[Level] = Schema.enumeration[Level, Int](Vector(One, Two), l => if (l == One) 1 else 2)
    assertEquals(Json.write[Level](Two), "2")
    assertEquals(Json.read[Level]("1"), Right(One))
  }
}
