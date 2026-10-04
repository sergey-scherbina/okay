package okay2.codec

sealed trait DpNode
object DpNode {
  final case class Leaf(label: String) extends DpNode
  final case class Next(next: DpNode) extends DpNode
  implicit lazy val schema: Schema[DpNode] = Schema.derived
}

/**
 * `JsonOptic.path` and `Policy.hide` descend the schema once per SEGMENT
 * of a key, and on a recursive schema a key can name a level for every
 * segment it has (okay-codec's TestJsonOpticDepth): the key is bounded
 * by `JsonOptic.MaxSegments` rather than walked. JVM only, on a 256 KB
 * stack.
 */
class TestJsonOpticDepth extends munit.FunSuite {

  private def smallStack[A](body: => A): A = {
    var out: Either[Throwable, A] = Left(new IllegalStateException("never ran"))
    val t = new Thread(null, () => out = try Right(body) catch { case e: Throwable => Left(e) }, "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)
  }

  test("a key with as many segments as the limit resolves on a recursive schema") {
    val key = (List.fill(JsonOptic.MaxSegments - 1)("next") :+ "label").mkString(".")
    assert(smallStack(JsonOptic.path(DpNode.schema, key)).isDefined)
  }

  test("a key past the limit names nothing, on a small stack") {
    val key = (List.fill(100000)("next") :+ "label").mkString(".")
    assertEquals(smallStack(JsonOptic.path(DpNode.schema, key)), None)
  }

  test("a policy key past the limit is refused by name at construction") {
    val key = (List.fill(100000)("next") :+ "label").mkString(".")
    val e = smallStack(Policy.hide[DpNode](key))
    assert(e.left.exists(_.contains("names no field")), e.toString)
  }
}
