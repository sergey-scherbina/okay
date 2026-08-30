package okay.rag

import okay.parse.Cst

/**
 * Code as a corpus: whole definitions with their doc comments, exact
 * quotes, symbol retrieval with no embeddings, and re-indexing that
 * costs the edit rather than the file.
 */
class TestCode extends munit.FunSuite {

  val src = Source("Demo.scala",
    """package demo
      |
      |/** The greeter.
      | *  Says hello.
      | */
      |class Greeter(name: String) {
      |  def hello: String = {
      |    val prefix = "Hello, "
      |    prefix + name
      |  }
      |
      |  // not a doc comment
      |  def bye: String = "Bye"
      |}
      |
      |object Main {
      |  def run(g: Greeter): Unit = println(g.hello)
      |}
      |""".stripMargin)

  val parsed = Code.parse(src.text)

  test("lossless: the code tree reproduces the file byte for byte") {
    assertEquals(Cst.lexemes(parsed.tree), src.text)
  }

  test("definitions come out whole, with their doc comment inside") {
    val segs = Split.structural(src, parsed.tree, 400)(_.length)
    for s <- segs do assert(s.quotes(src), s"segment misquotes: $s")
    val greeter = segs.find(_.text.contains("class Greeter"))
    assert(greeter.isDefined, s"no Greeter segment among ${segs.map(_.text.take(20))}")
    assert(greeter.get.text.contains("The greeter."),
      "the doc comment did not travel with its definition")
    assert(greeter.get.text.contains("prefix + name"),
      "the class body was cut short")
  }

  test("a small budget cuts between definitions, never mid-token") {
    val segs = Split.structural(src, parsed.tree, 60)(_.length)
    for s <- segs do assert(s.quotes(src))
    assertEquals(segs.map(_.text).mkString, src.text)   // exact reassembly
  }

  test("symbols: definitions and mentions, with no vectors in play") {
    val idx = Symbols.of(src.id, parsed.tree)
    assertEquals(idx.definition("Greeter").map(_.kind), Vector("class"))
    assertEquals(idx.definition("hello").map(_.kind), Vector("def"))
    assertEquals(idx.definition("run").map(_.kind), Vector("def"))
    assert(idx.names.contains("bye"))

    // the definition's span quotes the definition, exactly
    val sym = idx.definition("hello").head
    val seg = Symbols.segment(sym, src)
    assert(seg.quotes(src))
    assert(seg.text.startsWith("def hello"), seg.text)
    assert(seg.text.contains("prefix + name"))

    // mentions include the use inside Main.run
    assert(idx.mentions("Greeter").length >= 2, "the parameter mention is missing")
  }

  test("the index is a Monoid: a project is the merge of its files") {
    val a = Source("A.scala", "class A { def x: Int = 1 }\n")
    val b = Source("B.scala", "object B { def y: Int = 2 }\n")
    val whole = Symbols.project(Seq(a, b))
    val merged = Symbols.of(a.id, Code.parse(a.text).tree)
      .merge(Symbols.of(b.id, Code.parse(b.text).tree))
    assertEquals(whole.names, merged.names)
    assertEquals(whole.definition("x").map(_.source), Vector("A.scala"))
    assertEquals(whole.definition("y").map(_.source), Vector("B.scala"))
  }

  test("re-index after an edit costs the damage, not the file") {
    // change one string literal inside one method, same length
    val edited = src.text.replace("\"Bye\"", "\"Ciao\"")
    val at = src.text.indexOf("\"Bye\"")
    val re = Code.reparse(parsed, src.text, edited, at, at + 5, at + 6)
    val fresh = Code.parse(edited)

    assertEquals(Cst.lexemes(re.tree), edited)          // still lossless
    assertEquals(re.tree, fresh.tree)                   // and exactly right

    // the definitions that did not change keep their text
    val before = Symbols.of(src.id, parsed.tree)
    val after = Symbols.of(src.id, re.tree)
    val src2 = Source(src.id, edited)
    assertEquals(
      Symbols.segment(after.definition("hello").head, src2).text,
      Symbols.segment(before.definition("hello").head, src).text)
    // and the one that changed did change
    assert(Symbols.segment(after.definition("bye").head, src2).text.contains("Ciao"))
  }

  test("re-index COST is the damage: most of the file is never re-driven") {
    // a bigger file, so the claim has room to show
    val many = Source("Many.scala",
      (1 to 40).map(i =>
        s"/** doc $i */\nobject O$i {\n  def m$i(x: Int): Int = x + $i\n}\n").mkString)
    val base = Code.parse(many.text, snapshotEvery = 16)
    val at = many.text.lastIndexOf("x + 40")
    val edited = many.text.patch(at, "x + 41", 6)

    var steps = 0
    val probe: okay.parse.Parse.Step[Code.K, Code.D] = (d, t) =>
      steps += 1
      Code.step(d, t)
    val re = okay.parse.Parse.reparseWith(Code.scan, probe, Code.initD, Code.finish)(
      base, many.text, edited, at, at + 6, at + 6, 16)

    assertEquals(okay.parse.Cst.lexemes(re.tree), edited)      // still exact
    val total = base.lexed.tokens.length
    assert(steps < total / 2,
      s"not incremental: re-drove $steps of $total tokens")
  }

  test("an unparsable file still indexes (totality, all the way up)") {
    val broken = Source("Broken.scala", "class X { def f = { \"unterminated\n")
    val t = Code.parse(broken.text).tree
    assertEquals(Cst.lexemes(t), broken.text)
    val segs = Split.structural(broken, t, 20)(_.length)
    for s <- segs do assert(s.quotes(broken))
    assert(Symbols.of(broken.id, t).names.contains("X"))
  }
}
