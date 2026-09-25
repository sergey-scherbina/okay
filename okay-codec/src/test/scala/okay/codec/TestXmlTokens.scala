package okay.codec

import okay.lex.Scan

/** `Xml.tokens` is `Xml.scan` on a mutable buffer: the same tokens,
 * whatever the input and wherever the chunks are cut */
class TestXmlTokens extends munit.FunSuite:

  def scanned(s: String): Vector[Xml.T] = Scan.all(Xml.scan)(s).tokens

  def streamed(s: String, chunk: Int): Vector[Xml.T] =
    val out = Vector.newBuilder[Xml.T]
    Xml.tokens(java.io.StringReader(s), chunk)(out += _)
    out.result()

  // the characters every mode turns on, and enough ordinary ones
  private val alphabet = "<>/!-[]CDATA\"' \n\tabx=?"

  test("random input: the same tokens as scan, cut into chunks of every size") {
    val rnd = scala.util.Random(20260925)
    for _ <- 1 to 3000 do
      val s = String(Array.fill(rnd.nextInt(120))(alphabet(rnd.nextInt(alphabet.length))))
      val want = scanned(s)
      for chunk <- Seq(1, 2, 3, 7, 1 << 16) do
        assertEquals(streamed(s, chunk), want, s"input ${s.replace("\n", "\\n")} chunk $chunk")
  }

  test("a document with every construct, and the unterminated tail") {
    val doc =
      """<?xml version="1.0"?>
        |<!-- a comment with <tags> and --> text
        |<list a='1' b="x > y"><e>one &amp; two</e>  <br/>
        |<![CDATA[ <raw> ]] ]]><x:y z="w">tail</x:y></list>
        |<unterminated attr="""".stripMargin
    assertEquals(streamed(doc, 5), scanned(doc))
    assertEquals(streamed(doc, 1 << 16), scanned(doc))
  }

  test("empty input: no tokens") {
    assertEquals(streamed("", 8), Vector.empty)
  }
