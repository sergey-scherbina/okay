package okay.fs2

import okay.Chunks
import okay.given
import Fs2Interop.*
import _root_.cats.effect.unsafe.implicits.global

class TestFs2Interop extends munit.FunSuite {

  test("chunked streams cross to fs2 chunk for chunk, lazily") {
    assertEquals(toFs2(Chunks.range(0, 100, 16)).compile.fold(0L)(_ + _),
      (0L until 100L).sum)
    // infinite okay stream, finite fs2 take: laziness crosses too
    assertEquals(toFs2(Chunks.nats[Int]()).take(5).compile.toList, List(0, 1, 2, 3, 4))
    // chunk boundaries preserved
    assertEquals(toFs2(Chunks.range(0, 10, 4)).chunks.map(_.size).compile.toList,
      List(4, 4, 2))
  }

  test("fs2 streams cross back through a channel of chunks") {
    val s = _root_.fs2.Stream.range(0, 100).covary[_root_.cats.effect.IO]
    val c = fromFs2(s)
    var sum = 0L
    var ch = c.receive()
    while ch.isDefined do
      sum += ch.get.map(_.toLong).sum
      ch = c.receive()
    assertEquals(sum, (0 until 100).map(_.toLong).sum)
  }
}
