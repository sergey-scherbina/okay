package okay.codec

import okay.lex.Json.K
import okay.parse.Cst

/**
 * specs/codecs.md, "The batch road": `Json.cst` used to lex through
 * the effect system one character at a time. It takes
 * `Parse.full` now, and this file is the evidence that the TREE is
 * the same one — a stronger claim than TestJsonValue's, which
 * compares values.
 *
 * The old road is reconstructed here rather than kept in the module,
 * so what is compared is exactly what was replaced.
 */
class TestJsonCst extends munit.FunSuite {

  /** the road that was replaced, defined once in ProfCst */
  private def streamingCst(s: String): Cst[K] = ProfCst.streamingCst(s)

  // the same corpus TestJsonValue uses, so the two files agree on
  // what "every document" means
  private val corpus = JsonCorpus.wellFormed ++ JsonCorpus.damaged

  private def sameTree(s: String)(using munit.Location): Unit =
    assertEquals(Json.cst(s), streamingCst(s), s"the trees differ on <$s>")

  test("the batch road builds the SAME tree as the streaming one, document by document") {
    corpus.foreach(sameTree)
  }

  test("the prefix sweep: every truncation of every document, both roads, same tree") {
    corpus.foreach { s => (0 to s.length).foreach(k => sameTree(s.take(k))) }
  }

  test("the lossless law still holds: render puts every document back byte for byte") {
    corpus.foreach { s =>
      assertEquals(Json.render(Json.cst(s)), s, s"render lost something on <$s>")
    }
  }

  test("damage is still IN the tree, with the same errors in the same order") {
    JsonCorpus.damaged.foreach { s =>
      assertEquals(Cst.errors(Json.cst(s)).map(_._2), Cst.errors(streamingCst(s)).map(_._2),
        s"the diagnostics differ on <$s>")
    }
  }

  test("a document with trivia, duplicate keys and deep nesting survives both roads alike") {
    val fussy = " \n\t{ \"a\" : 1 , \"a\" : [ 2 , { } , null ] , \"b\" : \"x\\u0041\" } \r\n"
    sameTree(fussy)
    assertEquals(Json.render(Json.cst(fussy)), fussy)
    // and the projection off it is what the value road says
    assertEquals(Json.value(Json.cst(fussy)), Json.parse(fussy))
  }

  test("a large document: the two roads agree where the cost actually was") {
    val big = Json.print(Json.JObj(Vector(
      "rows" -> Json.JArr(Vector.tabulate(2000)(i =>
        Json.JObj(Vector("i" -> Json.JNum(i.toDouble), "s" -> Json.JStr(s"row $i"))))))))
    sameTree(big)
    assertEquals(Json.render(Json.cst(big)), big)
  }
}
