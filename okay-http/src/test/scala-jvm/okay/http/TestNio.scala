package okay.http

import java.nio.channels.ServerSocketChannel

import okay.*
import okay.given

/**
 * Raw NIO: two ends, chunks between them, and nothing parked. (MCP
 * over a bare socket is okay-mcp-http's TestMcpLinks since
 * http-mcp-agent-edge.)
 */
class TestNio extends munit.FunSuite {

  // nio-port-scope (2026-09-03): every test here binds a real port,
  // so the RESULT depends on what else on the machine is binding
  // them. Moved out of the default gate the way okay-netty was;
  // `sbt integrationTest` runs it.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))

  test("two ends exchange bytes") {
    val got = Resource.run[Seq[String], Pure](
      Nio.listen(0) { conn =>
        // echo every chunk back, upper-cased, then close
        Writer.uncons[Chunk[Byte], Unit, Async](conn.bytes).flatMap {
          case Right((c, _)) =>
            conn.send(scala.collection.immutable.ArraySeq.unsafeWrapArray(
              new String(c.toArray, "UTF-8").toUpperCase.getBytes("UTF-8")))
              .flatMap(_ => conn.close())
          case Left(_) => conn.close()
        }
      }.map { server =>
        Async.run[Seq[String], Pure](
          Nio.connect("127.0.0.1", Nio.port(server)).flatMap { c =>
            c.send("hello\n").flatMap(_ =>
              Writer.run[String, Unit, Async](
                through[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(
                  !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](
                    Http.framing))).map(_._1))
          }).runWith
      }).runWith
    assertEquals(got, Seq("HELLO"))
  }

  test("a source of many chunks arrives whole and in order") {
    val n = 500
    val got = Resource.run[Int, Pure](
      Nio.listen(0) { conn =>
        def go(i: Int): Unit ! Async =
          if i >= n then conn.close()
          else conn.send(s"line-$i\n").flatMap(_ => go(i + 1))
        go(0)
      }.map { server =>
        Async.run[Int, Pure](
          Nio.connect("127.0.0.1", Nio.port(server)).flatMap { c =>
            Writer.run[String, Unit, Async](
              through[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(
                !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](
                  Http.framing))).map { (ls, _) =>
              assertEquals(ls.head, "line-0")
              assertEquals(ls.last, s"line-${n - 1}")
              ls.length
            }
          }).runWith
      }).runWith
    assertEquals(got, n)
  }

  test("a big write is drained: a partial write does not lose bytes") {
    // ByteBuffer writes are partial by contract, so `send` loops; this
    // is the test that says the loop is there
    val big = "x" * 300_000
    val got = Resource.run[Int, Pure](
      Nio.listen(0) { conn =>
        conn.send(big + "\n").flatMap(_ => conn.close())
      }.map { server =>
        Async.run[Int, Pure](
          Nio.connect("127.0.0.1", Nio.port(server)).flatMap { c =>
            Writer.run[String, Unit, Async](
              through[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(
                !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](
                  Http.framing))).map(_._1.map(_.length).sum)
          }).runWith
      }).runWith
    assertEquals(got, big.length)
  }

  test("churn: one listener, hundreds of connections lose nothing") {
    // the regression gate for nio-serve-stall (okay-http/BUGS.md).
    // One STABLE listener on purpose: under listener churn macOS
    // itself loses fresh backlog connections at ~1.2/1000 rounds —
    // measured identically on blocking and asynchronous channels, so
    // no transport code can gate it. What the transport does
    // guarantee is per-connection delivery, and this holds it to that
    // (8000/8000 at fix time; 500 here for suite time).
    val n = 20
    val got = Resource.run[Int, Pure](
      Nio.listen(0) { conn =>
        def go(i: Int): Unit ! Async =
          if i >= n then conn.close()
          else conn.send(s"line-$i\n").flatMap(_ => go(i + 1))
        go(0)
      }.map { server =>
        val port = Nio.port(server)
        var ok = 0
        for _ <- 1 to 500 do
          val lines = Async.run[Int, Pure](
            Nio.connect("127.0.0.1", port).flatMap { c =>
              Writer.run[String, Unit, Async](
                through[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(
                  !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](
                    Http.framing))).map(_._1.length)
            }).runWith
          if lines == n then ok += 1
        ok
      }).runWith
    assertEquals(got, 500)
  }

  test("the listener is a Resource: it is CLOSED after the scope") {
    // This asked the question through the port until 2026-09-03: take
    // the ephemeral port the listener got, close the scope, and assert
    // that connecting to it now fails. Under the full matrix that is
    // not a fact about our Resource at all — the port goes back to the
    // ephemeral pool the moment we release it, a sibling suite binds
    // it, and our connect reaches THEIR listener and succeeds. The
    // assertion then reports "the listener outlived its Resource
    // scope" about a listener that closed exactly on time.
    //
    // The claim is about the listener, so it is asked of the listener:
    // `Nio.listen`'s resource value IS the ServerSocketChannel, and a
    // closed channel says so. No port, no pool, no neighbours.
    val server = Resource.run[ServerSocketChannel, Pure](
      Nio.listen(0)(_ => pure(()))).runWith
    assert(!server.isOpen, "the listener outlived its Resource scope")
  }
}
