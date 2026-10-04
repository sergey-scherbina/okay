package okay2.codec

import okay2.parse.Cst
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

final case class LawP(name: String, age: Int, tags: List[String])

/**
 * The laws under RANDOM input (okay-codec's TestLaws, the dialects this
 * module now has): the tree reproduces EVERY input exactly, in every
 * text dialect, and a value carried by one wire comes back from each.
 */
class TestTextLaws extends munit.ScalaCheckSuite {

  private val yamlish: Gen[String] = Gen.listOf(Gen.oneOf(
    Gen.const("a: 1\n"), Gen.const("b:\n"), Gen.const("  - x\n"),
    Gen.const("# note\n"), Gen.const("\"q\": v\n"), Gen.const(": orphan\n"),
    Gen.const("  nested: true\n"), Gen.const("\n"), Gen.const("- 5\n"),
    Gen.const("url: http://x/y\n"), Gen.const("weird\n"))).map(_.mkString)

  private val markdownish: Gen[String] = Gen.listOf(Gen.oneOf(
    Gen.const("# h\n"), Gen.const("text "), Gen.const("*"), Gen.const("_"),
    Gen.const("`"), Gen.const("\n"), Gen.const("more words "), Gen.const("#"))).map(_.mkString)

  private val anything: Gen[String] = Arbitrary.arbitrary[String]

  property("YAML: the CST reproduces ANY input, exactly") {
    forAll(Gen.oneOf(yamlish, anything))((s: String) => Yaml.render(Yaml.cst(s)) == s)
  }

  property("Markdown: the CST reproduces ANY input, exactly") {
    forAll(Gen.oneOf(markdownish, anything))((s: String) => Cst.lexemes(Markdown.parse(s)) == s)
  }

  property("a value re-encodes to something that decodes the same, on JSON, CBOR and EDN") {
    implicit val p: Schema[LawP] = Schema.derived
    forAll(Arbitrary.arbitrary[String], Arbitrary.arbitrary[Int], Gen.listOf(Gen.alphaStr)) {
      (n: String, a: Int, ts: List[String]) =>
        val v = LawP(n, a, ts)
        Json.read[LawP](Json.write(v)) == Right(v) &&
          Cbor.read[LawP](Cbor.write(v)) == Right(v) &&
          Edn.read[LawP](Edn.write(v)) == Right(v)
    }
  }

  property("YAML projects into the same Json shape it decodes from") {
    forAll(Gen.listOf(Gen.alphaLowerStr.suchThat(_.nonEmpty))) { (keys: List[String]) =>
      val doc = keys.distinct.zipWithIndex.map { case (k, i) => s"$k: $i\n" }.mkString
      Yaml.parse(doc) match {
        case Json.JObj(fs) => fs.length == keys.distinct.length
        case _ => keys.distinct.isEmpty
      }
    }
  }
}
