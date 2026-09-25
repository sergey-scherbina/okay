package okay.rag

/**
 * cst-walks-remaining: a code CST nests definitions by brace depth, as
 * deep as the source does, and the walks over it — the token run of a
 * subtree (Split, Symbols' span), the structural split's descent, the
 * symbol index's walk — must take any depth the builder builds. Run on
 * a SMALL stack (JVM only), where a per-level recursion overflows.
 */
class TestCodeDepth extends munit.FunSuite:

  private def smallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  private val n = 3000
  private val text = (0 until n).map(i => s"class C$i {\n").mkString + ("}\n" * n)
  private val src = Source("deep.scala", text)

  test("the structural split walks a 3 000-deep nest of definitions") {
    val tree = Code.source(src).tree
    val segs = smallStack(Split.structural(src, tree, 200)(_.length))
    assertEquals(segs.map(_.text).mkString, text)
  }

  test("the symbol index walks it too, every definition found with its span") {
    val tree = Code.source(src).tree
    val ix = smallStack(Symbols.of(src.id, tree))
    assertEquals(ix.defs.size, n)
    assert(ix.defs("C0").head.span.length > 0)
  }
