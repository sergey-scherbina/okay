package okay.rag

import okay.codec.{Markdown, Yaml}
import okay.lex.{Bpe, Scan, Json as JsonLex}
import okay.parse.{Cst, JsonParse, Parse}

/**
 * The laws that make retrieval citable: a segment quotes its source
 * exactly, the split covers the document, and boundaries land on
 * structure.
 */
class TestSplit extends munit.FunSuite {

  val chars: String => Int = _.length

  val md = Source("doc",
    "# Title\n" +
      "intro line\n" +
      "# Second\n" +
      "a much longer paragraph that will not fit in a small budget at all\n" +
      "# Third\n" +
      "short\n")

  test("provenance: every segment quotes its source EXACTLY") {
    for budget <- List(8, 20, 40, 200) do
      val segs = Split.structural(md, Markdown.parse(md.text), budget)(chars)
      assert(segs.nonEmpty, s"budget $budget produced nothing")
      for s <- segs do
        assert(s.quotes(md),
          s"segment does not quote its source at budget $budget: $s")
  }

  test("coverage: the split accounts for every character") {
    for budget <- List(8, 20, 40, 200) do
      val segs = Split.structural(md, Markdown.parse(md.text), budget)(chars)
      assert(Split.covers(md, segs), s"budget $budget left a hole")
      // and a non-overlapping structural split reassembles the source
      assertEquals(segs.map(_.text).mkString, md.text, s"budget $budget")
  }

  test("boundaries land on structure, not on a character count") {
    // a budget that fits a heading+line section but not two of them
    val segs = Split.structural(md, Markdown.parse(md.text), 30)(chars)
    // no segment starts or ends in the middle of a word
    for s <- segs do
      val before = if s.span.offset == 0 then '\n' else md.text.charAt(s.span.offset - 1)
      val endIdx = s.span.offset + s.span.length
      val after = if endIdx >= md.text.length then '\n' else md.text.charAt(endIdx)
      assert(!before.isLetter || !s.text.headOption.exists(_.isLetter),
        s"cut mid-word at the start: '$s'")
      assert(!after.isLetter || !s.text.lastOption.exists(_.isLetter),
        s"cut mid-word at the end: '$s'")
  }

  test("the structural path is carried as free metadata") {
    val segs = Split.structural(md, Markdown.parse(md.text), 12)(chars)
    assert(segs.exists(_.path.length > 1), "no segment recorded a deeper path")
    assert(segs.forall(_.path.headOption.contains("root")))
  }

  test("the same splitter works on any dialect we can lex") {
    val json = Source("j",
      """{"a": [1, 2, 3], "b": {"c": "text"}, "d": 4}""")
    val segs = Split.structural(json, Json.cst(json.text), 15)(chars)
    for s <- segs do assert(s.quotes(json), s"json segment misquotes: $s")
    assertEquals(segs.map(_.text).mkString, json.text)

    val yaml = Source("y", "a: 1\nb:\n  - x\n  - y\nc: 3\n")
    val ysegs = Split.structural(yaml, Yaml.cst(yaml.text), 10)(chars)
    for s <- ysegs do assert(s.quotes(yaml), s"yaml segment misquotes: $s")
    assertEquals(ysegs.map(_.text).mkString, yaml.text)
  }

  test("a damaged document still splits (totality all the way up)") {
    val broken = Source("b", """{"a": @@, "b": 2}""")
    val segs = Split.structural(broken, Json.cst(broken.text), 8)(chars)
    assert(segs.nonEmpty)
    for s <- segs do assert(s.quotes(broken))
    assertEquals(segs.map(_.text).mkString, broken.text)
  }

  test("token windows with overlap, counted by the model's own tokenizer") {
    val bpe = Bpe(List(("h", "e"), ("l", "l"), ("he", "ll"), ("hell", "o")))
    val src = Source("t", "hello hello hello hello hello")
    val segs = Split.windows(src, bpe, budget = 4, overlap = 2)
    for s <- segs do assert(s.quotes(src), s"window misquotes: $s")
    assert(segs.length > 1, "expected several windows")
    // consecutive windows overlap in the source
    for Seq(a, b) <- segs.sliding(2).toSeq do
      assert(b.span.offset < a.span.offset + a.span.length,
        "windows do not overlap")
  }

  test("a window split with no overlap covers the source exactly") {
    val bpe = Bpe(Nil)   // no merges: one token per character-run
    val src = Source("t", "alpha beta gamma delta")
    val segs = Split.windows(src, bpe, budget = 3)
    assertEquals(segs.map(_.text).mkString, src.text)
    assert(Split.covers(src, segs))
  }

  test("lineage: a passage widens from its source, no second retrieval") {
    val corpus = Corpus.of(Seq(md))
    val segs = Split.structural(md, Markdown.parse(md.text), 12)(chars)
    val small = segs.find(_.text.contains("Second")).get

    // the prompt carried a projection; more is a substring away
    val wider = corpus.widen(small, 40).get
    assert(wider.text.length > small.text.length)
    assert(wider.quotes(md), "the widened passage stopped quoting its source")
    assert(wider.text.contains(small.text.trim),
      s"the widening lost the passage it grew from: '${wider.text}'")
    // it snapped to line boundaries, so a reader sees whole lines
    assert(wider.text.startsWith("#") || wider.text.startsWith("intro"), wider.text)

    // and the whole document is available when that is what is wanted
    val all = corpus.whole(small).get
    assertEquals(all.text, md.text)
    assert(all.quotes(md))
  }

  test("lineage: widening past the edges clips instead of failing") {
    val corpus = Corpus.of(Seq(md))
    val segs = Split.structural(md, Markdown.parse(md.text), 12)(chars)
    for s <- segs do
      val w = corpus.widen(s, 10_000).get
      assertEquals(w.text, md.text)
      assert(w.quotes(md))
  }

  test("lineage: an index that drifted from the file is detectable") {
    val corpus = Corpus.of(Seq(md))
    val segs = Split.structural(md, Markdown.parse(md.text), 40)(chars)
    assert(corpus.current(segs.head).isDefined)
    // a segment whose text no longer matches its span is not current
    val stale = segs.head.copy(text = "something else entirely")
    assertEquals(corpus.current(stale), None)
  }

  /** the JSON dialect's CST, via okay-codec */
  object Json:
    def cst(s: String): Cst[JsonLex.K] = okay.codec.Json.cst(s)
}
