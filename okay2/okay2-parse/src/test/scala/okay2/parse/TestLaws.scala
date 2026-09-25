package okay2.parse

import okay2.lex.{Scan, Json => JsonLex}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** okay-parse's TestLaws on okay2: an incremental reparse agrees with a
 * full one for EVERY document and EVERY edit — the generator puts edits
 * on token boundaries, inside strings, across newlines, at both ends */
class TestLaws extends munit.ScalaCheckSuite {

  val document: Gen[String] =
    Gen.listOf(Gen.oneOf(
      Gen.const("{\"a\": 1}\n"), Gen.const("[1, 2, 3]\n"), Gen.const("\n"),
      Gen.const("{\"k\": \"v\", \"n\": [true, null]}\n"),
      Gen.const("  \"loose\" \n"), Gen.const("@@\n"), Gen.const("{\n"),
      Gen.const("}\n"), Gen.const("12.5e-3\n")
    )).map(_.mkString).suchThat(_.nonEmpty)

  def edits(doc: String): Gen[(Int, Int, String)] =
    for {
      at <- Gen.choose(0, doc.length)
      drop <- Gen.choose(0, math.min(6, doc.length - at))
      insert <- Gen.oneOf(Gen.const(""), Gen.const("x"), Gen.const("\"s\""), Gen.const("},\n"),
        Gen.const("999"), Gen.const("\n"), Gen.const("@"), Gen.const("["))
    } yield (at, drop, insert)

  property("an incremental reparse equals a full one, for any edit") {
    forAll(document) { (doc: String) =>
      forAll(edits(doc)) { (e: (Int, Int, String)) =>
        val (at, drop, insert) = e
        val edited = doc.take(at) + insert + doc.drop(at + drop)
        val session = Parse.full(JsonLex.scan, JsonParse.instrs)(doc, 16)
        val re = Parse.reparse(JsonLex.scan, JsonParse.instrs)(session, doc, edited, at, at + drop, at + insert.length, 16)
        re.tree == Parse.full(JsonLex.scan, JsonParse.instrs)(edited, 16).tree
      }
    }
  }

  property("a reparsed tree is still lossless") {
    forAll(document) { (doc: String) =>
      forAll(edits(doc)) { (e: (Int, Int, String)) =>
        val (at, drop, insert) = e
        val edited = doc.take(at) + insert + doc.drop(at + drop)
        val session = Parse.full(JsonLex.scan, JsonParse.instrs)(doc, 16)
        val re = Parse.reparse(JsonLex.scan, JsonParse.instrs)(session, doc, edited, at, at + drop, at + insert.length, 16)
        Cst.lexemes(re.tree) == edited
      }
    }
  }

  property("relexing alone equals a full lex, for any edit") {
    forAll(document) { (doc: String) =>
      forAll(edits(doc)) { (e: (Int, Int, String)) =>
        val (at, drop, insert) = e
        val edited = doc.take(at) + insert + doc.drop(at + drop)
        val old = Scan.all(JsonLex.scan)(doc, 16)
        Scan.relex(JsonLex.scan)(old, doc, edited, at, at + drop, at + insert.length, 16).tokens ==
          Scan.all(JsonLex.scan)(edited, 16).tokens
      }
    }
  }

  property("the builder is total: any instruction stream folds to a tree") {
    val instr: Gen[Instr[JsonLex.K]] = Gen.oneOf(
      Gen.const(Instr.Open[JsonLex.K]("node", None): Instr[JsonLex.K]),
      Gen.const(Instr.Close[JsonLex.K](None): Instr[JsonLex.K]),
      Gen.const(Instr.Bad[JsonLex.K](None, "bad"): Instr[JsonLex.K]))
    forAll(Gen.listOf(instr)) { (is: List[Instr[JsonLex.K]]) =>
      Parse.toCst(is) match {
        case Cst.Node("root", _) => true
        case _ => false
      }
    }
  }
}
