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

  test("fs2 streams cross back, backpressured by THEIR queue, lazily consumed") {
    val s = _root_.fs2.Stream.range(0, 100).covary[_root_.cats.effect.IO]
    val c = fromFs2(s)
    assertEquals(okay.Chunks.fold(c)(using okay.Fold.sum[Int]).toLong, (0 until 100).sum.toLong)
    // a bounded queue holds an infinite fs2 stream: take a little, leave the rest suspended
    val inf = fromFs2(_root_.fs2.Stream.iterate(0)(_ + 1).covary[_root_.cats.effect.IO], capacity = 2)
    assertEquals(okay.Chunks.fold(okay.Chunks.take(inf)(10))(using okay.Fold.count), 10L)
  }
}
