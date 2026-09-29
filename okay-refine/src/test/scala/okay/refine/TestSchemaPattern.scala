package okay.refine

import okay.{!, Logic, pure, runChoice}
import okay.codec.{Json, Schema}
import okay.testkit.Munit.Diagnosed
import java.nio.charset.StandardCharsets.UTF_8

/** specs/refine.md, stage 2: a Schema is a pattern; a pattern is a search */
class TestSchemaPattern extends Diagnosed:

  final case class Swap(id: String, notional: Double, fixedRate: Double)
  final case class Forward(id: String, notional: Double, forwardPrice: Double)
  given Schema[Swap] = Schema.derived
  given Schema[Forward] = Schema.derived

  val swap: Refine[Json, Swap] = Refine.schema[Swap]("swap")
  val forward: Refine[Json, Forward] = Refine.schema[Forward]("forward")

  private def bytes(s: String): Array[Byte] = s.getBytes(UTF_8)

  test("a derived Schema reads a matching value and writes it back") {
    val j = Json.parse("""{"id": "s1", "notional": 1000000.0, "fixedRate": 0.03}""")
    assertEquals(swap.run(j), Verdict.Took(Swap("s1", 1000000.0, 0.03), Path("swap"), Vector.empty))
    assertEquals(swap.write(Swap("s1", 1000000.0, 0.03)), Right(j))
  }

  test("a Schema declines in the codec's own words") {
    val v = swap.run(Json.parse("""{"id": "s1"}"""))
    note(v.toString)
    v match
      case Verdict.Declined(Vector(Refusal(at, reason))) =>
        assertEquals(at, Path("swap"))
        assert(reason.contains("notional"), reason)
      case other => fail(s"expected one refusal, got $other")
  }

  test("bytes → format → value → instrument: the whole path, and the path back is JSON") {
    val instrument: Refine[Array[Byte], Product] =
      Format.detect andThen Format.value andThen (swap.widen[Product] <|> forward.widen[Product])
    val fromYaml = bytes("id: f1\nnotional: 500.0\nforwardPrice: 101.5\n")
    val v = instrument.run(fromYaml)
    note(v.toString)
    v match
      case Verdict.Took(Forward("f1", 500.0, 101.5), by, declined) =>
        assertEquals(by, Path("text", "yaml", "value", "forward"))
        assertEquals(declined.map(_.at), Vector(Path("cbor"), Path("text", "json"), Path("text", "xml"), Path("text", "yaml", "value", "swap")))
      case other => fail(s"expected the forward, got $other")
    // written back through the same path: JSON, since the value bridge renders JSON
    val back = instrument.write(Forward("f1", 500.0, 101.5)).map(new String(_, UTF_8))
    assertEquals(back, Right("""{"id":"f1","notional":500,"forwardPrice":101.5}"""))
    // and what it wrote reads back as the same instrument, by the json branch this time
    assertEquals(instrument.run(back.toOption.get.getBytes(UTF_8)).toOption, Some(Forward("f1", 500.0, 101.5)))
  }

  test("XML projects to a value too; CBOR has none without a schema, and says so") {
    val v = (Format.detect andThen Format.value).run(bytes("""<a x="1"><b>t</b><b>u</b></a>"""))
    assertEquals(v.toOption.map(Json.print), Some("""{"a":{"@x":"1","b":["t","u"]}}"""))
    val c = (Format.detect andThen Format.value).run(Array[Byte](0xA1.toByte, 0x61, 0x61, 0x01))
    assertEquals(c.reasons.find(_.at == Path("cbor", "value")).map(_.reason), Some("no value projection for cbor without a schema"))
  }

  test("search: Took is one answer, Unclear is a choice point, Declined kills the branch") {
    val even = Refine.step[Int, Int]("even")(n => if n % 2 == 0 then Right(n) else Left("odd"))(identity)
    val small = Refine.step[Int, Int]("small")(n => if n < 10 then Right(n) else Left("big"))(identity)
    val both = even <|> small
    def all(n: Int) = !.run(runChoice[Int, okay.Pure](both.search(n)))
    assertEquals(all(20), Seq(20))
    assertEquals(all(4), Seq(4, 4))
    assertEquals(all(11), Seq.empty)
  }

  test("search under the soft cut: if this is a swap then …, else …, other readings kept") {
    val j = Json.parse("""{"id": "s1", "notional": 1.0, "fixedRate": 0.03}""")
    def said(j: Json): Seq[String] =
      !.run(runChoice[String, okay.Pure](
        Logic.ifte[Swap, String, okay.Pure](swap.search(j))(s => pure(s"swap ${s.id}"))(pure("not a swap"))))
    assertEquals(said(j), Seq("swap s1"))
    assertEquals(said(Json.parse("""{"id": "x"}""")), Seq("not a swap"))
  }
