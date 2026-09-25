package okay2.http

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.ArraySeq
import okay2.{!, Pure, Writer, pure}
import okay2.codec.Json
import okay2.stream.{Chunk, Pipe, Stage}

final case class Person(name: String, age: Int)

/** the PURE parts, which is most of the module (okay-http's
 * TestFraming): framing, a session and the text projection are Stages,
 * tested with no socket, clock or thread — on every platform */
class TestFraming extends munit.FunSuite {

  def chunk(s: String): Chunk[Byte] = ArraySeq.unsafeWrapArray(s.getBytes(UTF_8))

  def feed[A](xs: Seq[A]): Unit ! Writer[A] =
    xs.foldRight(pure[Writer[A], Unit](())) { (x, rest) => Writer.tell(x).flatMap(_ => rest) }

  /** drive a stage with these inputs, collect what it tells */
  def drive[I, O](s: Stage[I, O, _])(in: I*): Seq[O] =
    !.run(Writer.collect[O, Any, Pure](Pipe.into(feed(in))(s)))._1

  def lines(cs: Chunk[Byte]*): Seq[String] = drive(Http.framing)(cs: _*)

  test("a body frames into lines, whatever the chunk boundaries") {
    assertEquals(lines(chunk("a\nb\nc\n")), Seq("a", "b", "c"))
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
    val bs = "héllo — ok\n".getBytes(UTF_8)
    val split = bs.length / 2
    assertEquals(lines(ArraySeq.unsafeWrapArray(bs.take(split)), ArraySeq.unsafeWrapArray(bs.drop(split))), Seq("héllo — ok"))
  }

  def echo: Stage[Frame, Frame, Unit] =
    Stage.transduce[Frame, Frame, Unit](())((_, f) => f match {
      case Frame.Text(t) => Stage.tell[Frame, Frame](Frame.Text(t))
      case Frame.Binary(b) => Stage.tell[Frame, Frame](Frame.Binary(b))
      case _ => pure(())
    }, _ => pure(()))

  test("a session is a Stage: an echo round-trips text and binary") {
    val bin = ArraySeq.unsafeWrapArray(Array[Byte](1, 2, 3))
    assertEquals(drive(echo)(Frame.Text("a"), Frame.Binary(bin), Frame.Text("b")),
      Seq(Frame.Text("a"), Frame.Binary(bin), Frame.Text("b")))
  }

  test("control frames are the session's to ignore, and it can answer them") {
    val empty = ArraySeq.empty[Byte]
    assertEquals(drive(echo)(Frame.Ping(empty), Frame.Text("x")), Seq(Frame.Text("x")))
    val pinger: Stage[Frame, Frame, Unit] =
      Stage.transduce[Frame, Frame, Unit](())((_, f) => f match {
        case Frame.Ping(b) => Stage.tell[Frame, Frame](Frame.Pong(b))
        case _ => pure(())
      }, _ => pure(()))
    assertEquals(drive(pinger)(Frame.Ping(empty)), Seq(Frame.Pong(empty)))
  }

  test("texts projects a frame stream to its lines, dropping the rest") {
    val empty = ArraySeq.empty[Byte]
    assertEquals(drive(Ws.texts)(Frame.Text("one"), Frame.Ping(empty), Frame.Binary(empty),
      Frame.Text("two"), Frame.Close(1000, "bye")), Seq("one", "two"))
  }

  test("SSE: data lines join into events at an empty line; other fields are dropped") {
    assertEquals(drive(okay2.stream.Sse.events)("data: one", "", "data: two", "", "ignored", ""), Seq("one", "two"))
    assertEquals(drive(okay2.stream.Sse.events)("data: a", "data: b", ""), Seq("a\nb"))
  }

  test("a TRUNCATED json body decodes to what it carried") {
    val whole = Json.write(Person("ann", 41))
    assertEquals(Json.read[Person](whole), Right(Person("ann", 41)))
    val cut = whole.dropRight(1)
    assert(Json.read[Person](cut).isRight, s"a cut body must still decode: $cut")
  }

  test("status is data: no exception anywhere in this module's types") {
    val r = Response(404, Seq(("x", "y")), pure(()))
    assert(!r.ok)
    assertEquals(r.status, 404)
    assertEquals(r.header("X"), Some("y"))
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
