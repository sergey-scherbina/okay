package okay.codec

/**
 * stack-safety-cbor-edn-wire: Cbor's and Edn's recursions are BOUNDED —
 * a direct call per open container only below `Codecs.NativeThreshold`,
 * the `Cont` trampoline past it (the inventory's rows say so). The
 * existing depth tests run on the default stack; these run the same
 * shapes, encode AND decode, on a 256 KB one (JVM only), where a road that
 * recursed per level past the threshold would overflow at a few thousand.
 */
class TestCborEdnDepth extends munit.FunSuite:

  private def smallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  private val n = 20000
  private val chain = (1 to n).foldLeft(Option.empty[TestCborEdnDepth.Link])((l, i) => Some(TestCborEdnDepth.Link(i, l))).get

  private def lengthOf(l: TestCborEdnDepth.Link): Int =
    Iterator.iterate(Option(l))(_.flatMap(_.next)).takeWhile(_.isDefined).size

  test("Cbor writes and reads a value nested 20 000 deep") {
    val back = smallStack(Cbor.read[TestCborEdnDepth.Link](Cbor.write(chain)))
    assertEquals(back.map(lengthOf), Right(n))
  }

  test("Edn writes and reads a value nested 20 000 deep") {
    val back = smallStack(Edn.read[TestCborEdnDepth.Link](Edn.write(chain)))
    assertEquals(back.map(lengthOf), Right(n))
  }

  test("Edn parses and shows text nested 20 000 deep") {
    val text = "[" * n + "]" * n
    assertEquals(smallStack(Edn.parse(text).map(Edn.show)), Right(text))
  }

object TestCborEdnDepth:
  final case class Link(value: Int, next: Option[Link]) derives Schema
