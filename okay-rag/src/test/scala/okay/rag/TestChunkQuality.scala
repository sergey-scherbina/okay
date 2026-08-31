package okay.rag

/**
 * The claim structural chunking actually makes is not about speed —
 * it is that a retrieved chunk is a WHOLE definition. That is
 * measurable, and it is the number worth citing, so it is asserted
 * here rather than asserted in prose.
 *
 * The comparison is against `Split.windows`, which is the shape every
 * conventional RAG stack ships (slide a fixed window, overlap a
 * little). Ours is exact even there — the windows land on token
 * boundaries, not mid-character — so this is an honest comparison of
 * METHOD, not of one careful implementation against a sloppy one.
 */
class TestChunkQuality extends munit.FunSuite {

  val text: String = (0 until 24).map(i =>
    s"""/** what member $i is for. */
       |final case class Member$i(name: String, count: Int) {
       |  def describe: String = s"member $i called $$name"
       |  def combine(that: Member$i): Member$i =
       |    Member$i(name + that.name, count + that.count)
       |}
       |""".stripMargin).mkString("package bench\n\n", "\n", "\n")

  val src: Source = Source("Bench.scala", text)
  val index: Index = Symbols.source(src)

  /** the top-level definitions a reader would want returned whole */
  val wanted: Vector[Symbol] =
    index.defs.values.flatten.filter(_.path.isEmpty).toVector.sortBy(_.span.offset)

  /** a definition is INTACT if some single chunk contains all of it */
  def intact(segs: Seq[Segment]): Int =
    wanted.count(sym => segs.exists(s =>
      s.span.offset <= sym.span.offset &&
        s.span.offset + s.span.length >= sym.span.offset + sym.span.length))

  test("structural chunking returns whole definitions; windows do not") {
    val structural = Split.structural(src, Code.source(src).tree, 600)(_.length)
    // a token budget chosen to produce a comparable number of chunks
    val windows = Split.windows(src, Code.scan, 200, overlap = 20)

    val s = intact(structural)
    val w = intact(windows)

    println(f"  ${wanted.size} top-level definitions, " +
      f"${structural.size} structural chunks vs ${windows.size} windows: " +
      f"structural keeps $s%d (${100.0 * s / wanted.size}%.0f%%), " +
      f"windows keep $w%d (${100.0 * w / wanted.size}%.0f%%)")

    assertEquals(s, wanted.size,
      "a structural chunk cut a top-level definition in half")
    assert(w < wanted.size,
      "the window split kept every definition whole — the comparison is vacuous")
  }

  test("both splits are exact: every chunk quotes its file") {
    // the point is that windows are not a straw man — they are exact
    // too, because they land on the lexer's own spans
    val structural = Split.structural(src, Code.source(src).tree, 600)(_.length)
    val windows = Split.windows(src, Code.scan, 200, overlap = 0)
    assert(structural.forall(_.quotes(src)))
    assert(windows.forall(_.quotes(src)))
    // and with no overlap, a window split still reassembles exactly
    assertEquals(windows.map(_.text).mkString, text)
  }

  test("a definition retrieved by name is whole, with its doc comment") {
    val sym = index.definition("Member7").head
    val seg = Symbols.segment(sym, src)
    assert(seg.quotes(src))
    assert(seg.text.contains("what member 7 is for"), "the doc comment was lost")
    assert(seg.text.contains("def combine"), "the body was cut")
    assert(seg.text.trim.endsWith("}"), s"not a whole definition:\n${seg.text}")
  }
}
