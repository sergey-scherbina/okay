package okay.http

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.mcp.{Mcp, Rpc}

/**
 * Raw NIO: two ends, chunks between them, and nothing parked.
 *
 * The interesting assertion is the last one — an MCP session over a
 * bare TCP socket, no HTTP anywhere. `Http.framing` turns the byte
 * source into lines and `mcp.Link` wants exactly lines, so the glue is
 * two lines and the protocol code is untouched. That is three
 * transports for okay-mcp now: pipes, a WebSocket, and this.
 */
class TestNio extends munit.FunSuite {

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

  test("MCP over a raw socket — the third transport, no HTTP anywhere") {
    val sent = Rpc.encode(Rpc.Request(Json.JNum(1), Mcp.Initialize,
      Mcp.initializeParams(Mcp.Info("client", "1"))))

    val back = Resource.run[Seq[String], Pure](
      // the server end: read one line, send it back, close
      Nio.listen(0) { conn =>
        val link = Nio.link(conn)
        Writer.uncons[String, Unit, Async](link.lines).flatMap {
          case Right((line, _)) => link.send(line).flatMap(_ => conn.close())
          case Left(_) => conn.close()
        }
      }.map { server =>
        Async.run[Seq[String], Pure](
          Nio.connect("127.0.0.1", Nio.port(server)).flatMap { c =>
            val link = Nio.link(c)
            link.send(sent).flatMap(_ =>
              Writer.uncons[String, Unit, Async](link.lines).flatMap {
                case Right((l, _)) => c.close().map(_ => Seq(l))
                case Left(_) => c.close().map(_ => Seq.empty[String])
              })
          }).runWith
      }).runWith

    assertEquals(back, Seq(sent))
    // and it decodes back to the identical message, not just to bytes
    assertEquals(Rpc.decode(back.head), Rpc.Request(Json.JNum(1), Mcp.Initialize,
      Mcp.initializeParams(Mcp.Info("client", "1"))))
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

  test("the listener is a Resource: the port is free after the scope") {
    val port = Resource.run[Int, Pure](
      Nio.listen(0)(_ => pure(())).map(Nio.port)).runWith
    val failed =
      try { Async.run[Unit, Pure](Nio.connect("127.0.0.1", port).map(_ => ())).runWith; false }
      catch case _: Throwable => true
    assert(failed, "the listener outlived its Resource scope")
  }
}
