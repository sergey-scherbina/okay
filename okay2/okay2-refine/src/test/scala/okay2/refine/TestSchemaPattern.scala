package okay2.refine

import okay2.codec.{Json, Schema}
import java.nio.charset.StandardCharsets.UTF_8

/** the fixtures live outside the suite: an inner case class carries an outer
 * reference Scala 2 cannot check in a pattern (-Xlint, -Werror) */
object TestSchemaPattern {
  final case class Swap(id: String, notional: Double, fixedRate: Double)
  final case class Forward(id: String, notional: Double, forwardPrice: Double)
  implicit val swapSchema: Schema[Swap] = Schema.derived[Swap]
  implicit val forwardSchema: Schema[Forward] = Schema.derived[Forward]
}

/** a Schema is a pattern; the whole path bytes → format → value → instrument, and back */
class TestSchemaPattern extends munit.FunSuite {
  import TestSchemaPattern._

  val swap: Refine[Json, Swap] = Refine.schema[Swap]("swap")
  val forward: Refine[Json, Forward] = Refine.schema[Forward]("forward")

  private def bytes(s: String): Array[Byte] = s.getBytes(UTF_8)

  test("a derived Schema reads a matching value and writes it back") {
    val j = Json.parse("""{"id": "s1", "notional": 1000000.0, "fixedRate": 0.03}""")
    assertEquals(swap.run(j), Verdict.Took(Swap("s1", 1000000.0, 0.03), Path("swap"), Vector.empty))
    assertEquals(swap.write(Swap("s1", 1000000.0, 0.03)), Right(j))
  }

  test("a Schema declines in the codec's own words") {
    swap.run(Json.parse("""{"id": "s1"}""")) match {
      case Verdict.Declined(Vector(Refusal(at, reason))) =>
        assertEquals(at, Path("swap"))
        assert(reason.contains("notional"), reason)
      case other => fail(s"expected one refusal, got $other")
    }
  }

  test("bytes → format → value → instrument: the whole path, the path back is JSON; XML text is text, and a Schema says so") {
    val instrument: Refine[Array[Byte], Product] =
      Format.detect andThen Format.value andThen (swap.widen[Product] <|> forward.widen[Product])
    val asJson = bytes("""{"id": "f1", "notional": 500.0, "forwardPrice": 101.5}""")
    instrument.run(asJson) match {
      case Verdict.Took(Forward("f1", 500.0, 101.5), by, declined) =>
        assertEquals(by, Path("text", "json", "value", "forward"))
        assertEquals(declined.map(_.at), Vector(Path("text", "xml"), Path("text", "json", "value", "swap")))
      case other => fail(s"expected the forward, got $other")
    }
    // written back through the same path: JSON, since the value bridge renders JSON
    val back = instrument.write(Forward("f1", 500.0, 101.5)).map(new String(_, UTF_8))
    assertEquals(back, Right("""{"id":"f1","notional":500,"forwardPrice":101.5}"""))
    assertEquals(instrument.run(back.toOption.get.getBytes(UTF_8)).toOption, Some(Forward("f1", 500.0, 101.5): Product))
    // XML: the element's text is a string, and a derived Schema wants a number — the refusal is the codec's own words,
    // under the element's name; a document-level pattern reads XML with `json.num`, which takes text (okay-fin's road)
    val fromXml = bytes("""<forward><id>f1</id><notional>500.0</notional><forwardPrice>101.5</forwardPrice></forward>""")
    val under = Format.detect andThen Format.value andThen Refine.json.field("forward") andThen forward
    under.run(fromXml) match {
      case Verdict.Declined(tried) =>
        assertEquals(tried.map(_.at), Vector(Path("text", "json"), Path("text", "xml", "value", "forward", "forward")))
        assert(tried.last.reason.contains("SDouble") && tried.last.reason.contains("JStr"), tried.last.reason)
      case other => fail(s"expected the codec's refusal, got $other")
    }
    val price = Format.detect andThen Format.value andThen Refine.json.field("forward") andThen Refine.json.field("forwardPrice") andThen Refine.json.num
    assertEquals(price.run(fromXml).toOption, Some(101.5))
  }

  test("XML projects to a value: elements as objects, attributes as @name, repeats as arrays") {
    val v = (Format.detect andThen Format.value).run(bytes("""<a x="1"><b>t</b><b>u</b></a>"""))
    assertEquals(v.toOption.map(Json.print), Some("""{"a":{"@x":"1","b":["t","u"]}}"""))
  }

  test("json steps: field, str, num, each — a path of them names itself and writes a skeleton") {
    import Refine.json._
    val legs = field("trade") andThen each("leg") 
    val doc = Json.parse("""{"trade": {"leg": [{"rate": "0.03"}, {"rate": 0.02}]}}""")
    assertEquals(legs.run(doc).toOption.map(_.length), Some(2))
    val rate = field("rate") andThen num
    assertEquals(rate.run(Json.parse("""{"rate": "0.03"}""")), Verdict.Took(0.03, Path("rate", "number"), Vector.empty))
    assertEquals(rate.write(0.05).map(Json.print), Right("""{"rate":0.05}"""))
    assertEquals(field("x").run(Json.parse("{}")), Verdict.Declined(Vector(Refusal(Path("x"), "no field `x`"))))
  }
}
