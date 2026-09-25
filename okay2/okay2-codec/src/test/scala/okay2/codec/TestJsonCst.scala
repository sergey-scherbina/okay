package okay2.codec

import okay2.parse.Cst

/** The lossless layer: `render` puts every document back byte for
 * byte, damage is IN the tree, and the tree's projection is what the
 * value road says (okay-codec's TestJsonCst, without the retired
 * streaming road it compared against). */
class TestJsonCst extends munit.FunSuite {

  private val corpus = JsonCorpus.wellFormed ++ JsonCorpus.damaged

  test("the lossless law: render puts every document and every prefix back byte for byte") {
    corpus.foreach(s => (0 to s.length).foreach { k =>
      val p = s.take(k)
      assertEquals(Json.render(Json.cst(p)), p, s"render lost something on <$p>")
    })
  }

  test("well-formed documents carry no diagnostics; the projection equals the value road") {
    JsonCorpus.wellFormed.foreach { s =>
      assertEquals(Cst.errors(Json.cst(s)), Vector.empty, s)
      assertEquals(Json.value(Json.cst(s)), Json.parse(s), s)
    }
  }

  test("a document with trivia, duplicate keys and escapes survives") {
    val fussy = " \n\t{ \"a\" : 1 , \"a\" : [ 2 , { } , null ] , \"b\" : \"x\\u0041\" } \r\n"
    assertEquals(Json.render(Json.cst(fussy)), fussy)
    assertEquals(Json.value(Json.cst(fussy)), Json.parse(fussy))
  }

  test("a large document renders back and projects to what printed it") {
    val j = Json.JObj(Vector("rows" -> Json.JArr(Vector.tabulate(2000)(i =>
      Json.JObj(Vector("i" -> Json.JNum(i.toDouble), "s" -> Json.JStr(s"row $i")))))))
    val big = Json.print(j)
    assertEquals(Json.render(Json.cst(big)), big)
    assertEquals(Json.lossless(big), j)
  }
}
