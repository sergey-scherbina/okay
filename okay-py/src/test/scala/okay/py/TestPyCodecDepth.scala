package okay.py

import okay.codec.Schema

/**
 * stack-safety-py-r: `PyCodec.enc`/`dec` and `Shape.json`'s Json <->
 * PyValue walks recurse once per level of a VALUE, and a value of a
 * recursive type — or a nested dict a worker sent — is as deep as its
 * sender made it. Past `Codecs.NativeThreshold` the codec continues on
 * the Cont trampoline now (Json's own road), and the Json/PyValue
 * conversions are explicit stacks.
 */
class TestPyCodecDepth extends munit.FunSuite:
  import TestPyCodecDepth.*

  private val n = 200000
  private val chain = (1 to n).foldLeft(Option.empty[Link])((l, i) => Some(Link(i, l))).get
  private def lengthOf(l: Link): Int = Iterator.iterate(Option(l))(_.flatMap(_.next)).takeWhile(_.isDefined).size

  test("PyCodec encodes and decodes a value nested 200 000 deep") {
    val v = PyCodec.encode(chain)
    assertEquals(PyCodec.decode[Link](v).map(lengthOf), Right(n))
  }

  test("PyCodec refuses damage at depth by its path, not with a stack overflow") {
    // the innermost `value` is a str where an int is declared
    var v: PyValue = PyValue.Dict(Vector("value" -> PyValue.Str("x"), "next" -> PyValue.PyNone))
    for i <- 1 until n do v = PyValue.Dict(Vector("value" -> PyValue.I64(i), "next" -> v))
    val e = PyCodec.decode[Link](v)
    assert(e.left.exists(_.message.contains("expected an int")), e.toString)
  }

  test("Shape.json takes the same depth both ways") {
    val v = Shape.json.encode(chain)
    assertEquals(Shape.json.decode[Link](v).map(lengthOf), Right(n))
  }

object TestPyCodecDepth:
  final case class Link(value: Int, next: Option[Link]) derives Schema
