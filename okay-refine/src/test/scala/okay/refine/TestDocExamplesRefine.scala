package okay.refine

import java.nio.charset.StandardCharsets.UTF_8
import okay.freer.{!}
import okay.std.{runChoice}
import okay.codec.{Json, Schema}

/** the snippets in docs/modules/okay-refine.md, VERBATIM */
class TestDocExamplesRefine extends munit.FunSuite:

  test("docs: a hierarchy of three steps") {
    // ---- snippet begins
    val int   = Refine.step[String, Int]("int")(s => s.toIntOption.toRight(s"'$s' is not an integer"))(_.toString)
    val even  = Refine.step[Int, Int]("even")(n => if n % 2 == 0 then Right(n) else Left(s"$n is odd"))(identity)
    val small = Refine.step[Int, Int]("small")(n => if n < 10 then Right(n) else Left(s"$n is not small"))(identity)
    val evenOrSmall = int andThen (even <|> small)
    val seven = evenOrSmall.run("7")    // Took(7, int/small, Vector(int/even: 7 is odd))
    val four  = evenOrSmall.run("4")    // Unclear(Vector((int/even, 4), (int/small, 4)), Vector())
    val bad   = evenOrSmall.run("x")    // Declined(Vector(int: 'x' is not an integer))
    val seven2 = evenOrSmall.write(7)   // Right("7")
    // ---- snippet ends
    assertEquals(seven, Verdict.Took(7, Path("int", "small"), Vector(Refusal(Path("int", "even"), "7 is odd"))))
    assertEquals(four, Verdict.Unclear(Vector((Path("int", "even"), 4), (Path("int", "small"), 4)), Vector()))
    assertEquals(bad, Verdict.Declined(Vector(Refusal(Path("int"), "'x' is not an integer"))))
    assertEquals(seven2, Right("7"))
  }

  test("docs: the format level") {
    // ---- snippet begins
    val verdict = Format.detect.run("""{"a": [1, 2]}""".getBytes(UTF_8))
    val path = verdict match
      case Verdict.Took(_, by, _) => by.toString               // "text/json"
      case other => other.toString
    val tried = verdict.reasons.map(_.at.toString)              // Vector("cbor", "text/xml", "text/yaml")
    val back = verdict.toOption.flatMap(doc => Format.detect.write(doc).toOption)
    val text = back.map(new String(_, UTF_8))                   // Some("""{"a": [1, 2]}""")
    // ---- snippet ends
    assertEquals(path, "text/json")
    assertEquals(tried, Vector("cbor", "text/xml", "text/yaml"))
    assertEquals(text, Some("""{"a": [1, 2]}"""))
  }

  test("docs: into a sum") {
    val int = Refine.step[String, Int]("int")(s => s.toIntOption.toRight(s"'$s' is not an integer"))(_.toString)
    val decimal = Refine.step[String, Double]("decimal")(s =>
      s.toDoubleOption.filter(_ => s.contains('.')).toRight(s"'$s' is not a decimal"))(_.toString)
    // ---- snippet begins
    val num: Refine[String, AnyVal] = int.widen[AnyVal] <|> decimal.widen[AnyVal]
    val half = num.run("4.5").toOption     // Some(4.5)
    val i42  = num.write(42)               // Right("42")
    val no   = num.write(true)             // Left("int|decimal: no alternative writes this value")
    // ---- snippet ends
    assertEquals(half, Some(4.5))
    assertEquals(i42, Right("42"))
    assertEquals(no, Left("int|decimal: no alternative writes this value"))
  }

  test("docs: a Schema is a pattern, a path is a conversion, a pattern is a search") {
    // ---- snippet begins
    final case class Swap(id: String, notional: Double, fixedRate: Double)
    given Schema[Swap] = Schema.derived
    val swap: Refine[Json, Swap] = Refine.schema[Swap]("swap")
    val fromBytes: Refine[Array[Byte], Swap] = Format.detect andThen Format.value andThen swap
    val read = fromBytes.run("id: s1\nnotional: 1000000.0\nfixedRate: 0.03\n".getBytes(UTF_8))
    val where = read match
      case Verdict.Took(_, by, _) => by.toString                // "text/yaml/value/swap"
      case other => other.toString
    val asJson = fromBytes.write(Swap("s1", 1000000.0, 0.03)).map(new String(_, UTF_8))
    // Right("{\"id\":\"s1\",\"notional\":1000000,\"fixedRate\":0.03}") — read from YAML, written as JSON: a conversion
    val readings = !.run(runChoice[Swap, okay.freer.Pure](swap.search(Json.parse("""{"id": "s1", "notional": 1.0, "fixedRate": 0.03}"""))))
    // Seq(Swap("s1", 1.0, 0.03)) — a pattern is a search: Unclear is a choice point, Declined an empty one
    // ---- snippet ends
    assertEquals(where, "text/yaml/value/swap")
    assertEquals(asJson, Right("""{"id":"s1","notional":1000000,"fixedRate":0.03}"""))
    assertEquals(readings, Seq(Swap("s1", 1.0, 0.03)))
  }
