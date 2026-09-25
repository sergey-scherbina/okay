package okay2.http

import java.nio.channels.ServerSocketChannel
import scala.collection.immutable.ArraySeq
import okay2.{!, Pure, Resource, Writer, pure}
import okay2.async.Async
import okay2.platform._
import okay2.stream.{Chunk, Pipe}

/** raw NIO: two ends, chunks between them (okay-http's TestNio) */
class TestNio extends Live {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Pure](p))

  def linesOf(c: Nio.Conn): Vector[String] ! Async =
    Writer.collect[String, Unit, Async](Pipe.intoIn[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(Http.framing)).map(_._1)

  test("two ends exchange bytes") {
    val got = !.run(Resource.run[Vector[String], Pure](
      Nio.listen(0) { conn =>
        Writer.unconsIn[Chunk[Byte], Unit, Async](conn.bytes).flatMap {
          case Right((c, _)) =>
            conn.send(ArraySeq.unsafeWrapArray(new String(c.toArray, "UTF-8").toUpperCase.getBytes("UTF-8"))).flatMap(_ => conn.close())
          case Left(_) => conn.close()
        }
      }.map(server => run(Nio.connect("127.0.0.1", Nio.port(server)).flatMap(c => c.send("hello\n").flatMap(_ => linesOf(c)))))))
    assertEquals(got, Vector("HELLO"))
  }

  def sending(n: Int)(conn: Nio.Conn): Unit ! Async = {
    def go(i: Int): Unit ! Async = if (i >= n) conn.close() else conn.send(s"line-$i\n").flatMap(_ => go(i + 1))
    go(0)
  }

  test("a source of many chunks arrives whole and in order") {
    val n = 500
    val ls = !.run(Resource.run[Vector[String], Pure](
      Nio.listen(0)(sending(n)).map(server => run(Nio.connect("127.0.0.1", Nio.port(server)).flatMap(linesOf)))))
    assertEquals(ls.head, "line-0")
    assertEquals(ls.last, s"line-${n - 1}")
    assertEquals(ls.length, n)
  }

  test("a big write is drained: a partial write does not lose bytes") {
    val big = "x" * 300000
    val got = !.run(Resource.run[Int, Pure](
      Nio.listen(0)(conn => conn.send(big + "\n").flatMap(_ => conn.close()))
        .map(server => run(Nio.connect("127.0.0.1", Nio.port(server)).flatMap(linesOf)).map(_.length).sum)))
    assertEquals(got, big.length)
  }

  test("churn: one listener, hundreds of connections lose nothing") {
    val n = 20
    val got = !.run(Resource.run[Int, Pure](
      Nio.listen(0)(sending(n)).map { server =>
        val port = Nio.port(server)
        (1 to 500).count(_ => run(Nio.connect("127.0.0.1", port).flatMap(linesOf)).length == n)
      }))
    assertEquals(got, 500)
  }

  test("the listener is a Resource: it is CLOSED after the scope") {
    val server = !.run(Resource.run[ServerSocketChannel, Pure](Nio.listen(0)(_ => pure[Async, Unit](()))))
    assert(!server.isOpen, "the listener outlived its Resource scope")
  }
}
