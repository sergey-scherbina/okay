package okay.codec

/**
 * cst-walks-remaining: YAML nests by indentation, so `Yaml.cst` builds a
 * tree as deep as its input, and the projection to `Json` must walk any
 * depth the builder does. Run on a SMALL stack (JVM only: a thread's
 * stack size is a JVM notion), where a walk that recursed per level
 * overflows at a few thousand.
 */
class TestYamlDepth extends munit.FunSuite:

  private def smallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  test("the builder takes that depth on the small stack (a fold): only the projection is under test") {
    val doc = ("- " * 5000) + "x\n"
    assertEquals(Yaml.render(smallStack(Yaml.cst(doc))), doc)
  }

  test("a sequence nested 5 000 deep projects to the same nesting of arrays") {
    val n = 5000
    val doc = ("- " * n) + "x\n"
    var j = smallStack(Yaml.parse(doc))
    var d = 0
    var go = true
    while go do j match
      case Json.JArr(Vector(one)) => d += 1; j = one
      case _ => go = false
    assertEquals(d, n)
    assertEquals(j, Json.JStr("x"))
  }
