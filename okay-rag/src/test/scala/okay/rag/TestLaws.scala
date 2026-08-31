package okay.rag

import okay.codec.{Markdown, Yaml}
import okay.parse.Cst
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

/**
 * Retrieval's laws under random input. The provenance law in
 * particular is the module's whole claim, and it is exactly the kind
 * of thing that holds on the examples one thought of and breaks on
 * the ones one did not.
 */
class TestLaws extends munit.ScalaCheckSuite {

  val code: Gen[String] = Gen.listOf(Gen.oneOf(
    Gen.const("def f(x: Int): Int = x + 1\n"),
    Gen.const("/** doc */\n"),
    Gen.const("class A {\n"), Gen.const("}\n"), Gen.const("\n"),
    Gen.const("  val v = \"text\"\n"), Gen.const("// line\n"),
    Gen.const("object O { def g = 1 }\n"), Gen.const("/* block */"),
    Gen.const("\"unterminated\n"), Gen.const("@@\n")
  )).map(_.mkString)

  val budgets: Gen[Int] = Gen.oneOf(5, 12, 40, 200, 5000)

  property("provenance: every segment quotes its source, whatever the input") {
    forAll(Gen.oneOf(code, Arbitrary.arbitrary[String]), budgets) {
      (text: String, budget: Int) =>
        val src = Source("s", text)
        val segs = Split.structural(src, Code.parse(text).tree, budget)(_.length)
        segs.forall(_.quotes(src))
    }
  }

  property("a structural split reassembles its source exactly") {
    forAll(Gen.oneOf(code, Arbitrary.arbitrary[String]), budgets) {
      (text: String, budget: Int) =>
        val src = Source("s", text)
        val segs = Split.structural(src, Code.parse(text).tree, budget)(_.length)
        segs.map(_.text).mkString == text
    }
  }

  property("the code tree is lossless for any input") {
    forAll(Gen.oneOf(code, Arbitrary.arbitrary[String])) { (text: String) =>
      Cst.lexemes(Code.parse(text).tree) == text
    }
  }

  property("an incremental code reparse equals a full one") {
    forAll(code.suchThat(_.nonEmpty)) { (doc: String) =>
      forAll(Gen.choose(0, doc.length), Gen.choose(0, 5),
        Gen.oneOf("", "x", "}", "def h = 2\n", "\n", "\"")) {
        (at0: Int, drop0: Int, insert: String) =>
          val at = math.min(at0, doc.length)
          val drop = math.min(drop0, doc.length - at)
          val edited = doc.take(at) + insert + doc.drop(at + drop)
          val session = Code.parse(doc, 16)
          val re = Code.reparse(session, doc, edited, at, at + drop,
            at + insert.length, 16)
          re.tree == Code.parse(edited, 16).tree
      }
    }
  }

  /** fragments per language, mixing well-formed code with damage */
  val fragments: Map[Language, Gen[String]] = Map(
    Language.scala -> code,
    Language.java -> Gen.listOf(Gen.oneOf(
      "public class A {\n", "  private int x;\n", "  void f() { g(); }\n",
      "}\n", "/** doc */\n", "// line\n", "\"unterminated\n", "@@\n"
    )).map(_.mkString),
    Language.javascript -> Gen.listOf(Gen.oneOf(
      "function f(a) { return a; }\n", "const x = `t${y}`;\n",
      "class C {\n", "}\n", "// line\n", "'unterminated\n", "/* b */"
    )).map(_.mkString),
    Language.rust -> Gen.listOf(Gen.oneOf(
      "pub fn f(x: u32) -> u32 { x }\n", "/// doc\n", "struct S { a: u8 }\n",
      "impl S {\n", "}\n", "// line\n", "\"unterminated\n"
    )).map(_.mkString),
    Language.python -> Gen.listOf(Gen.oneOf(
      "class A:\n", "def f(x):\n", "    return x\n", "        deep = 1\n",
      "# comment\n", "\n", "    \"\"\"doc\"\"\"\n", "'unterminated\n", "@@\n"
    )).map(_.mkString))

  for (lang, gen) <- fragments do
    property(s"${lang.name}: the tree is lossless for any input") {
      forAll(Gen.oneOf(gen, Arbitrary.arbitrary[String])) { (text: String) =>
        Cst.lexemes(Code.parse(text, 64, lang).tree) == text
      }
    }

    property(s"${lang.name}: an incremental reparse equals a full one") {
      forAll(gen.suchThat(_.nonEmpty)) { (doc: String) =>
        forAll(Gen.choose(0, doc.length), Gen.choose(0, 5),
          Gen.oneOf("", "x", "}", "\n", "\"", "    ", "def h():\n")) {
          (at0: Int, drop0: Int, insert: String) =>
            val at = math.min(at0, doc.length)
            val drop = math.min(drop0, doc.length - at)
            val edited = doc.take(at) + insert + doc.drop(at + drop)
            val re = Code.reparse(Code.parse(doc, 16, lang), doc, edited,
              at, at + drop, at + insert.length, 16, lang)
            re.tree == Code.parse(edited, 16, lang).tree
        }
      }
    }

    property(s"${lang.name}: every definition quotes its file exactly") {
      forAll(gen) { (text: String) =>
        val src = Source(s"f.${lang.extensions.head}", text)
        val idx = Symbols.of(src.id, Code.parse(text, 64, lang).tree)
        idx.defs.values.flatten.forall(s => Symbols.segment(s, src).quotes(src))
      }
    }

  property("widening a passage keeps it quoting, and contains it") {
    forAll(code.suchThat(_.nonEmpty), Gen.choose(0, 200)) {
      (text: String, by: Int) =>
        val src = Source("s", text)
        val corpus = Corpus.of(Seq(src))
        val segs = Split.structural(src, Code.parse(text).tree, 40)(_.length)
        segs.forall { s =>
          corpus.widen(s, by).exists(w =>
            w.quotes(src) && w.text.length >= s.text.length)
        }
    }
  }

  property("the symbol index is a Monoid: files merge, order-free") {
    forAll(Gen.listOfN(3, code)) { (texts: List[String]) =>
      val sources = texts.zipWithIndex.map((t, i) => Source(s"f$i.scala", t))
      val whole = Symbols.project(sources)
      val merged = sources.map(s => Symbols.of(s.id, Code.parse(s.text).tree))
        .foldLeft(Index())(_.merge(_))
      whole.names == merged.names
    }
  }

  property("the keyword index merges to the same postings, any split") {
    forAll(Gen.listOf(Gen.alphaLowerStr.suchThat(_.nonEmpty))) { (words: List[String]) =>
      val segs = words.zipWithIndex.map((w, i) =>
        Segment("s", okay.lex.Span(i, 0, 0, w.length), w, Seq("x")))
      val whole = Keyword.index(segs)
      val M = summon[okay.Monoid[Postings]]
      (0 to segs.length).forall { at =>
        val (l, r) = segs.splitAt(at)
        val merged = M.combine(Keyword.index(l), Keyword.index(r))
        merged.docs == whole.docs && merged.byTerm.keySet == whole.byTerm.keySet
      }
    }
  }
}
