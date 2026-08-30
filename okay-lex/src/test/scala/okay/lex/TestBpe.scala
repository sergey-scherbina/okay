package okay.lex



/** BPE as a Scan, against an independent whole-string reference. */
class TestBpe extends munit.FunSuite {

  val merges = List(
    ("h", "e"), ("l", "l"), ("he", "ll"), ("hell", "o"),
    ("w", "o"), ("r", "l"), ("wo", "rl"), ("worl", "d"), ("e", "r"))

  val bpe = Bpe(merges)

  /** the reference: plain whole-word BPE, written independently */
  def reference(text: String): List[String] =
    val rank = merges.zipWithIndex.toMap
    def enc(w: String): List[String] =
      var parts = w.map(_.toString).toList
      var going = true
      while going do
        val cands = parts.zip(parts.drop(1)).filter(rank.contains)
        if cands.isEmpty then going = false
        else
          val best = cands.minBy(rank)
          def m(l: List[String]): List[String] = l match
            case x :: y :: t if (x, y) == best => (x + y) :: m(t)
            case x :: t => x :: m(t)
            case Nil => Nil
          parts = m(parts)
      parts
    text.split("\\s+").filter(_.nonEmpty).toList.flatMap(enc)

  test("the streaming Scan tokenizes a corpus identically to the reference") {
    val corpus = "hello hello world hell her herd\nworldly wold hollow"
    val got = Scan.all(bpe)(corpus).tokens
      .filter(_.channel == Channel.Syntax).map(_.lexeme).toList
    assertEquals(got, reference(corpus))
  }

  test("the merge order is by rank, not by position") {
    // "her": (h,e) has rank 0, (e,r) rank 8 — he+r, not h+er
    assertEquals(bpe.encode("her").toList, List("he", "r"))
    assertEquals(bpe.encode("hello").toList, List("hello"))
    assertEquals(bpe.encode("world").toList, List("world"))
    assertEquals(bpe.encode("xyz").toList, List("x", "y", "z"))
  }

  test("the scan stays lossless: all channels concatenate to the input") {
    val corpus = "hello  world\n her"
    assertEquals(Scan.all(bpe)(corpus).tokens.map(_.lexeme).mkString, corpus)
  }
}
