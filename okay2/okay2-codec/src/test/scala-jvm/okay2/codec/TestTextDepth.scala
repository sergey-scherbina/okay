package okay2.codec

final case class DepthLink(value: Int, next: Option[DepthLink])
object DepthLink {
  implicit lazy val schema: Schema[DepthLink] = Schema.derived
}

/**
 * The recursions are BOUNDED on a SMALL stack (okay-codec's
 * TestCborEdnDepth and TestYamlDepth; JVM only, a thread's stack size
 * is a JVM notion): CBOR and EDN encode and decode a value nested
 * 20 000 deep, EDN's text and YAML's projection walk any depth, on a
 * 256 KB stack where a road that recursed per level would overflow at a
 * few thousand.
 */
class TestTextDepth extends munit.FunSuite {

  private def smallStack[A](body: => A): A = {
    var out: Either[Throwable, A] = Left(new IllegalStateException("never ran"))
    val t = new Thread(null, () => out = try Right(body) catch { case e: Throwable => Left(e) }, "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)
  }

  private val n = 20000
  private val chain = (1 to n).foldLeft(Option.empty[DepthLink])((l, i) => Some(DepthLink(i, l))).get
  private def lengthOf(l: DepthLink): Int = Iterator.iterate(Option(l))(_.flatMap(_.next)).takeWhile(_.isDefined).size

  test("Cbor writes and reads a value nested 20 000 deep") {
    assertEquals(smallStack(Cbor.read[DepthLink](Cbor.write(chain))).map(lengthOf), Right(n))
  }

  test("Edn writes and reads a value nested 20 000 deep") {
    assertEquals(smallStack(Edn.read[DepthLink](Edn.write(chain))).map(lengthOf), Right(n))
  }

  test("Edn parses and shows text nested 20 000 deep") {
    val text = "[" * n + "]" * n
    assertEquals(smallStack(Edn.parse(text).map(Edn.show)), Right(text))
  }

  test("YAML: the builder takes 5 000 levels on the small stack; the projection walks them") {
    val doc = ("- " * 5000) + "x\n"
    assertEquals(Yaml.render(smallStack(Yaml.cst(doc))), doc)
    var j = smallStack(Yaml.parse(doc))
    var d = 0
    var go = true
    while (go) j match {
      case Json.JArr(Vector(one)) => d += 1; j = one
      case _ => go = false
    }
    assertEquals(d, 5000)
    assertEquals(j, Json.JStr("x"): Json)
  }
}
