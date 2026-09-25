package okay.r

import okay.codec.Schema

/** stack-safety-py-r: `RCodec.enc`/`dec` recurse once per level of a
 * VALUE; past `Codecs.NativeThreshold` they continue on the Cont
 * trampoline now (okay-py's TestPyCodecDepth, the same shape) */
class TestRCodecDepth extends munit.FunSuite:
  import TestRCodecDepth.*

  private val n = 200000
  private val chain = (1 to n).foldLeft(Option.empty[Link])((l, i) => Some(Link(i, l))).get
  private def lengthOf(l: Link): Int = Iterator.iterate(Option(l))(_.flatMap(_.next)).takeWhile(_.isDefined).size

  test("RCodec encodes and decodes a value nested 200 000 deep") {
    val v = RCodec.encode(chain)
    assertEquals(RCodec.decode[Link](v).map(lengthOf), Right(n))
  }

  test("RCodec refuses damage at depth by its path, not with a stack overflow") {
    var v: RValue = RValue.Named(Vector("value" -> RValue.Str("x"), "next" -> RValue.RNull))
    for i <- 1 until n do v = RValue.Named(Vector("value" -> RValue.I32(i), "next" -> v))
    val e = RCodec.decode[Link](v)
    assert(e.left.exists(_.message.contains("expected an integer")), e.toString)
  }

object TestRCodecDepth:
  final case class Link(value: Int, next: Option[Link]) derives Schema
