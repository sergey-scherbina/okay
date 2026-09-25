package okay.codec

/**
 * stack-safety-codec-rest: `JsonOptic.path` and `Policy.hide` descend the
 * schema once per SEGMENT of a key, and on a recursive schema a key can
 * name a level for every segment it has — a key is a string a caller
 * hands over, so it is bounded here (`JsonOptic.MaxSegments`) rather than
 * walked. Run on a 256 KB stack (JVM only), where the descent of a
 * 100 000-segment key would otherwise overflow.
 */
class TestJsonOpticDepth extends munit.FunSuite:

  private def smallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  // through a SUM at every level: the descent through a case is a search
  // over the cases, which is the one step of `path` that is not a loop
  enum Node derives Schema:
    case Leaf(label: String)
    case Next(next: Node)
  private val s = summon[Schema[Node]]

  test("a key with as many segments as the limit resolves on a recursive schema") {
    val key = (List.fill(JsonOptic.MaxSegments - 1)("next") :+ "label").mkString(".")
    assert(smallStack(JsonOptic.path(s, key)).isDefined)
  }

  test("a key past the limit names nothing, on a small stack") {
    val key = (List.fill(100000)("next") :+ "label").mkString(".")
    assertEquals(smallStack(JsonOptic.path(s, key)), None)
  }

  test("a policy key past the limit is refused by name at construction") {
    val key = (List.fill(100000)("next") :+ "label").mkString(".")
    val e = smallStack(Policy.hide[Node](key)(using s))
    assert(e.left.exists(_.contains("names no field")), e.toString)
  }
