package okay.http

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The parts of this module that are PURE — and that is most of it.
 *
 * Framing is a Stage, a session is a Stage, the text projection is a
 * Stage; none of them needs a socket, a clock or a thread, so none of
 * them is tested with one. What is left for the JVM suite is the two
 * transports, which is the only place I/O actually lives.
 */
class TestFraming extends munit.FunSuite {

  def chunk(s: String): Chunk[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(s.getBytes(UTF_8))

  /** drive the framer with these chunks, collect the lines */
  def lines(cs: Chunk[Byte]*): Seq[String] =
    !.run(Writer.run(through(Writer.of(cs.toList))(Http.framing)))._1

  test("a body frames into lines, whatever the chunk boundaries") {
    assertEquals(lines(chunk("a\nb\nc\n")), Seq("a", "b", "c"))
    // the same bytes, split at every awkward place
    assertEquals(lines(chunk("a\nb"), chunk("\nc\n")), Seq("a", "b", "c"))
    assertEquals(lines(chunk("a"), chunk("\n"), chunk("b\n")), Seq("a", "b"))
  }

  test("a last line without a newline is still a line") {
    assertEquals(lines(chunk("a\nb")), Seq("a", "b"))
    assertEquals(lines(chunk("only")), Seq("only"))
  }

  test("empty input is no lines, and empty lines are lines") {
    assertEquals(lines(), Seq.empty)
    assertEquals(lines(chunk("")), Seq.empty)
    assertEquals(lines(chunk("\n\n")), Seq("", ""))
  }

  test("CRLF is stripped, a bare CR is not") {
    assertEquals(lines(chunk("a\r\nb\r\n")), Seq("a", "b"))
    assertEquals(lines(chunk("a\rb\n")), Seq("a\rb"))
  }

  test("a multi-byte character split across chunks survives") {
    // the reason framing happens on BYTES: this would corrupt if each
    // chunk were decoded on its own
    val bs = "héllo — ok\n".getBytes(UTF_8)
    val split = bs.length / 2
    val got = lines(
      scala.collection.immutable.ArraySeq.unsafeWrapArray(bs.take(split)),
      scala.collection.immutable.ArraySeq.unsafeWrapArray(bs.drop(split)))
    assertEquals(got, Seq("héllo — ok"))
  }

  // ---- the WebSocket side, equally pure

  /** drive a session stage with these frames, collect what it tells */
  def talk(session: Stage[Frame, Frame, Unit])(fs: Frame*): Seq[Frame] =
    !.run(Writer.run(through(Writer.of(fs.toList))(session)))._1

  /** the echo session, written the way a user would write it */
  def echo: Stage[Frame, Frame, Unit] =
    // named, for the reason `Sse.events` documents: as the receiver of
    // a call the transduce gets no expected type
    val framed: Stage[Frame, Frame, Unit] =
      Stage.transduce(())((_, f) =>
        f match
          case Frame.Text(t) => Stage.tell[Frame, Frame](Frame.Text(t))
          case Frame.Binary(b) => Stage.tell[Frame, Frame](Frame.Binary(b))
          case _ => pure(()),
        _ => pure(()))
    framed

  test("a session is a Stage: an echo round-trips text and binary") {
    val bin = scala.collection.immutable.ArraySeq.unsafeWrapArray(Array[Byte](1, 2, 3))
    assertEquals(
      talk(echo)(Frame.Text("a"), Frame.Binary(bin), Frame.Text("b")),
      Seq(Frame.Text("a"), Frame.Binary(bin), Frame.Text("b")))
  }

  test("control frames are the session's to ignore, and it can answer them") {
    val empty = scala.collection.immutable.ArraySeq.empty[Byte]
    assertEquals(talk(echo)(Frame.Ping(empty), Frame.Text("x")), Seq(Frame.Text("x")))

    val pinger: Stage[Frame, Frame, Unit] =
      Stage.transduce(())((_, f) =>
        f match
          case Frame.Ping(b) => Stage.tell[Frame, Frame](Frame.Pong(b))
          case _ => pure(()),
        _ => pure(()))
    assertEquals(talk(pinger)(Frame.Ping(empty)), Seq(Frame.Pong(empty)))
  }

  test("texts projects a frame stream to its lines, dropping the rest") {
    val empty = scala.collection.immutable.ArraySeq.empty[Byte]
    val got = !.run(Writer.run(through(Writer.of(List[Frame](
      Frame.Text("one"), Frame.Ping(empty), Frame.Binary(empty),
      Frame.Text("two"), Frame.Close(1000, "bye"))))(Ws.texts)))._1
    assertEquals(got, Seq("one", "two"))
  }

  // ---- the contract inherited rather than re-invented

  final case class Person(name: String, age: Int)
  given Schema[Person] = Schema.derived

  test("a TRUNCATED json body decodes to what it carried") {
    // the promise streaming-parse.md and codecs.md make, arriving here
    // for free because Json.read runs the total stack underneath
    val whole = Json.write(Person("ann", 41))
    assertEquals(Json.read[Person](whole), Right(Person("ann", 41)))
    val cut = whole.dropRight(1)
    assert(Json.read[Person](cut).isRight, s"a cut body must still decode: $cut")
  }

  test("status is data: no exception anywhere in this module's types") {
    val r = Response(404, Seq(("x", "y")), pure(()))
    assert(!r.ok)
    assertEquals(r.status, 404)
    assertEquals(r.header("X"), Some("y"))   // header lookup is case-insensitive
    assertEquals(r.header("nope"), None)
  }

  test("a request carries its verb and encodes a json body by schema") {
    val r = Request.json("http://x/y", Person("bo", 7))
    assertEquals(r.method, Method.Post)
    assertEquals(r.method.name, "POST")
    assert(r.headers.contains(("content-type", "application/json")))
    assertEquals(new String(r.body.bytes, UTF_8), Json.write(Person("bo", 7)))
  }
}
