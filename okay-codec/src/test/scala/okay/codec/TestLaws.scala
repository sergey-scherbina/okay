package okay.codec

import okay.lex.Scan
import okay.parse.Cst
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

/**
 * The laws under RANDOM input, which is a different consumer again:
 * the examples in the other suites were chosen by whoever wrote the
 * code, and agree with it by construction. A generator does not.
 *
 * The strongest law here is losslessness — for EVERY string, in every
 * dialect, the tree reproduces its input exactly. Totality is what
 * makes it testable this way: there is no input to exclude.
 */
class TestLaws extends munit.ScalaCheckSuite {

  /** strings that look like the dialects, so the generator reaches
   * structure rather than only garbage */
  val jsonish: Gen[String] = Gen.listOf(Gen.oneOf(
    Gen.const("{"), Gen.const("}"), Gen.const("["), Gen.const("]"),
    Gen.const(":"), Gen.const(","), Gen.const(" "), Gen.const("\n"),
    Gen.const("\"a\""), Gen.const("\"b c\""), Gen.const("12"), Gen.const("-3.5e2"),
    Gen.const("true"), Gen.const("null"), Gen.const("@"), Gen.const("\\"),
    Gen.const("\"unterminated")
  )).map(_.mkString)

  val yamlish: Gen[String] = Gen.listOf(Gen.oneOf(
    Gen.const("a: 1\n"), Gen.const("b:\n"), Gen.const("  - x\n"),
    Gen.const("# note\n"), Gen.const("\"q\": v\n"), Gen.const(": orphan\n"),
    Gen.const("  nested: true\n"), Gen.const("\n"), Gen.const("- 5\n"),
    Gen.const("url: http://x/y\n"), Gen.const("weird\n")
  )).map(_.mkString)

  val markdownish: Gen[String] = Gen.listOf(Gen.oneOf(
    Gen.const("# h\n"), Gen.const("text "), Gen.const("*"), Gen.const("_"),
    Gen.const("`"), Gen.const("\n"), Gen.const("more words "), Gen.const("#")
  )).map(_.mkString)

  val xmlish: Gen[String] = Gen.listOf(Gen.oneOf(
    Gen.const("<a>"), Gen.const("</a>"), Gen.const("<b class=\"x\">"),
    Gen.const("</b>"), Gen.const("<br>"), Gen.const("<img src='y'/>"),
    Gen.const("text "), Gen.const("\n"), Gen.const("<!-- c -->"),
    Gen.const("<![CDATA[ </z> ]]>"), Gen.const("</z>"), Gen.const("<unclosed"),
    Gen.const("<"), Gen.const(">")
  )).map(_.mkString)

  val anything: Gen[String] = Arbitrary.arbitrary[String]

  property("JSON: the CST reproduces ANY input, exactly") {
    forAll(Gen.oneOf(jsonish, anything)) { (s: String) =>
      Json.render(Json.cst(s)) == s
    }
  }

  property("YAML: the CST reproduces ANY input, exactly") {
    forAll(Gen.oneOf(yamlish, anything)) { (s: String) =>
      Yaml.render(Yaml.cst(s)) == s
    }
  }

  property("Markdown: the CST reproduces ANY input, exactly") {
    forAll(Gen.oneOf(markdownish, anything)) { (s: String) =>
      Cst.lexemes(Markdown.parse(s)) == s
    }
  }

  property("lexing is total: every character lands in some token") {
    forAll(Gen.oneOf(jsonish, anything)) { (s: String) =>
      Scan.all(okay.lex.Json.scan)(s).tokens.map(_.lexeme).mkString == s
    }
  }

  property("chunked lexing agrees with element-wise, at any chunk size") {
    forAll(Gen.oneOf(jsonish, anything), Gen.choose(1, 40)) { (s: String, size: Int) =>
      val elementwise = Scan.all(okay.lex.Json.scan)(s).tokens.toSeq
      val chunked = okay.Chunks.fold(
        Scan.chunks(okay.lex.Json.scan)(okay.Chunks.fromIterator(s.iterator, size)))
      chunked == elementwise
    }
  }

  property("XML: the CST reproduces ANY input, exactly") {
    forAll(Gen.oneOf(xmlish, anything)) { (s: String) =>
      Xml.render(Xml.cst(s)) == s
    }
  }

  property("XML: an incremental reparse equals a full one") {
    forAll(xmlish.suchThat(_.nonEmpty)) { (doc: String) =>
      forAll(Gen.choose(0, doc.length), Gen.choose(0, 5),
        Gen.oneOf("", "x", "</a>", "<b>", "\n", "<")) {
        (at0: Int, drop0: Int, insert: String) =>
          val at = math.min(at0, doc.length)
          val drop = math.min(drop0, doc.length - at)
          val edited = doc.take(at) + insert + doc.drop(at + drop)
          val re = Xml.reparse(Xml.parse(doc, 16), doc, edited,
            at, at + drop, at + insert.length, 16)
          re.tree == Xml.parse(edited, 16).tree
      }
    }
  }

  property("a decoded value re-encodes to something that decodes the same") {
    case class P(name: String, age: Int, tags: List[String])
    given Schema[P] = Schema.derived
    forAll(Arbitrary.arbitrary[String], Arbitrary.arbitrary[Int],
      Gen.listOf(Gen.alphaStr)) { (n: String, a: Int, ts: List[String]) =>
      val p = P(n, a, ts)
      // JSON and CBOR carry the same value, and both round-trip
      Json.read[P](Json.write(p)) == Right(p) &&
        Cbor.read[P](Cbor.write(p)) == Right(p)
    }
  }

  property("YAML projects into the same Json shape it decodes from") {
    forAll(Gen.listOf(Gen.alphaLowerStr.suchThat(_.nonEmpty))) { (keys: List[String]) =>
      val doc = keys.distinct.zipWithIndex.map((k, i) => s"$k: $i\n").mkString
      val parsed = Yaml.parse(doc)
      parsed match
        case Json.JObj(fs) => fs.length == keys.distinct.length
        case _ => keys.distinct.isEmpty
    }
  }
}
