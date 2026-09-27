package okay.foreign

import okay.codec.Schema

/**
 * stack-safety-py-r: `ValueCodec.enc`/`dec` and `Shape.json`'s Json <->
 * Value walks recurse once per level of a VALUE, and a value of a
 * recursive type — or a nested dict a worker sent — is as deep as its
 * sender made it. Past `Codecs.NativeThreshold` the codec continues on
 * the Cont trampoline now (Json's own road), and the Json/Value
 * conversions are explicit stacks.
 */
class TestValueCodecDepth extends munit.FunSuite:
  import TestValueCodecDepth.*

  private val n = 200000
  private val chain = (1 to n).foldLeft(Option.empty[Link])((l, i) => Some(Link(i, l))).get
  private def lengthOf(l: Link): Int = Iterator.iterate(Option(l))(_.flatMap(_.next)).takeWhile(_.isDefined).size

  test("ValueCodec encodes and decodes a value nested 200 000 deep") {
    val v = ValueCodec.encode(chain)
    assertEquals(ValueCodec.decode[Link](v).map(lengthOf), Right(n))
  }

  test("ValueCodec refuses damage at depth by its path, not with a stack overflow") {
    // the innermost `value` is a str where an int is declared
    var v: Value = Value.Dict(Vector("value" -> Value.Str("x"), "next" -> Value.Null))
    for i <- 1 until n do v = Value.Dict(Vector("value" -> Value.I64(i), "next" -> v))
    val e = ValueCodec.decode[Link](v)
    assert(e.left.exists(_.message.contains("expected an int")), e.toString)
  }

  test("Shape.json takes the same depth both ways") {
    val v = Shape.json.encode(chain)
    assertEquals(Shape.json.decode[Link](v).map(lengthOf), Right(n))
  }

object TestValueCodecDepth:
  final case class Link(value: Int, next: Option[Link]) derives Schema
