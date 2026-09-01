package okay.http

import okay.*
import okay.given
import okay.codec.Schema
import java.nio.charset.StandardCharsets.UTF_8

/**
 * REST, in the vocabulary that is already here.
 *
 * The three types below are DATA, and the seam is a trait speaking
 * `Async` — not an effect signature. That is the house rule and it is
 * consistent: `llm.Transport`, `mcp.Link` and `cluster.Remote` are all
 * values, and a new signature is minted for domain logic ABOVE the
 * wire, never for the wire itself.
 *
 * The one decision worth reading twice is `Response.body`. It is a
 * `Source[Chunk[Byte]]` — that is, `Unit ! (Writer % Chunk[Byte] +
 * Async)` — so nothing has been read when the head arrives, and the
 * body goes `through` a decoding `Stage` exactly as SSE lines already
 * do. `Pipe.scala` names that case in its own doc comment as "the
 * generalization the LLM client walks by hand"; this module is the
 * caller that stops walking it.
 */
enum Method:
  case Get, Head, Post, Put, Patch, Delete, Options

  def name: String = toString.toUpperCase

/**
 * A request body. Deliberately not streamed: the JVM could
 * (`BodyPublishers.fromPublisher`) but `fetch` needs a duplex mode
 * that is not reliably there, and a shared type one platform fakes is
 * worse than an honest omission. See specs/http.md, Out of scope.
 */
enum Body:
  case Empty
  case Text(s: String)
  case Bytes(b: Chunk[Byte])

  def bytes: Array[Byte] = this match
    case Empty => Array.empty
    case Text(s) => s.getBytes(UTF_8)
    case Bytes(b) => b.toArray

final case class Request(method: Method, url: String,
                         headers: Seq[(String, String)] = Nil,
                         body: Body = Body.Empty)

object Request:
  def get(url: String, headers: Seq[(String, String)] = Nil): Request =
    Request(Method.Get, url, headers)

  def post(url: String, body: Body, headers: Seq[(String, String)] = Nil): Request =
    Request(Method.Post, url, headers, body)

  /** a JSON body, encoded by the schema and typed by the header */
  def json[A](url: String, a: A, headers: Seq[(String, String)] = Nil)
             (using Schema[A]): Request =
    post(url, Body.Text(okay.codec.Json.write(a)),
      ("content-type", "application/json") +: headers)

/**
 * The head, and a body that has not been read.
 *
 * A 4xx or a 5xx is a `Response` like any other — status is data. No
 * `Throws` appears anywhere in this module, which is the same contract
 * `streaming-parse.md` and `codecs.md` state for damaged input.
 */
final case class Response(status: Int,
                          headers: Seq[(String, String)],
                          body: Source[Chunk[Byte]],
                          /**
                           * Let the body go WITHOUT reading it.
                           *
                           * A transport that holds an open stream needs
                           * this: the JDK documents that an unconsumed
                           * `ofInputStream` body can keep an
                           * `HttpClient` from shutting down cleanly, and
                           * draining a large body only to throw it away
                           * is the wrong fix. The default is `pure(())`
                           * for the transports that hold nothing.
                           */
                          release: Unit ! Async = pure(())):
  def header(name: String): Option[String] =
    val n = name.toLowerCase
    headers.collectFirst { case (k, v) if k.toLowerCase == n => v }

  def ok: Boolean = status >= 200 && status < 300

/** The seam. One method, like `llm.Transport` — but carrying the verb,
 * the status and the headers back, which Transport could not. */
trait Http:
  def send(r: Request): Response ! Async

object Http {

  /**
   * A `Typeable` for a chunk of bytes.
   *
   * `Writer`'s row split tests the operation's own class first (it is
   * a `Say`) and the told value's class only to separate two writers
   * in one row. A row here holds exactly one, so the test is the
   * erasure of `ArraySeq` and there is nothing finer to ask for.
   */
  private given scala.reflect.Typeable[Chunk[Byte]] = new scala.reflect.Typeable[Chunk[Byte]]:
    def unapply(x: Any): Option[x.type & Chunk[Byte]] = x match
      case _: scala.collection.immutable.ArraySeq[?] =>
        Some(x.asInstanceOf[x.type & Chunk[Byte]])
      case _ => None

  /** a body that is already in hand — one chunk, then done */
  def one(bs: Array[Byte]): Source[Chunk[Byte]] =
    if bs.isEmpty then pure(())
    else effect[Writer % Chunk[Byte] + Async, Unit](
      Writer(scala.collection.immutable.ArraySeq.unsafeWrapArray(bs)))

  /** the whole body, drained */
  def bytes(r: Response): Chunk[Byte] ! Async =
    given Fold[Chunk[Byte], scala.collection.mutable.ArrayBuilder[Byte]] = new:
      def init = scala.collection.mutable.ArrayBuilder.make[Byte]
      def add(b: scala.collection.mutable.ArrayBuilder[Byte], c: Chunk[Byte]) =
        b ++= c.toArray; b
    Writer.fold[Chunk[Byte], scala.collection.mutable.ArrayBuilder[Byte], Unit, Async](r.body)
      .map((b, _) => scala.collection.immutable.ArraySeq.unsafeWrapArray(b.result()))

  /** let a body go unread — the counterpart of draining it */
  def discard(r: Response): Unit ! Async = r.release

  /** the whole body as text */
  def text(r: Response): String ! Async =
    bytes(r).map(c => new String(c.toArray, UTF_8))

  /**
   * The body as lines, STREAMED — a `Stage` over bytes, not a split of
   * a materialized string.
   *
   * Framing on the byte side rather than after decoding is not fussiness:
   * a chunk boundary can fall inside a multi-byte UTF-8 sequence, and
   * decoding each chunk separately would corrupt it. A newline byte
   * cannot appear inside such a sequence (continuation bytes are all
   * >= 0x80), so splitting bytes first and decoding whole lines after
   * is both simpler and correct.
   */
  def lines(r: Response): Source[String] =
    okay.through[Chunk[Byte], String, Async, Unit, Unit](r.body)(
      !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](framing))

  /** bytes in, lines out — the framer, in the shape `Sse.events` uses */
  def framing: Stage[Chunk[Byte], String, Unit] =
    def spill(buf: Array[Byte]): Stage[Chunk[Byte], String, Array[Byte]] =
      var i = 0
      while i < buf.length && buf(i) != '\n'.toByte do i += 1
      if i >= buf.length then i = -1
      if i < 0 then pure(buf)
      else
        val line = new String(buf, 0, if i > 0 && buf(i - 1) == '\r' then i - 1 else i, UTF_8)
        Stage.tell[Chunk[Byte], String](line)
          .flatMap(_ => spill(java.util.Arrays.copyOfRange(buf, i + 1, buf.length)))

    val framed: Stage[Chunk[Byte], String, Array[Byte]] =
      Stage.transduce(Array.empty[Byte])(
        (buf, c) => spill(buf ++ c.toArray),
        // a body that does not end in a newline still has a last line
        rest =>
          if rest.isEmpty then pure(rest)
          else Stage.tell[Chunk[Byte], String](new String(rest, UTF_8))
            .map(_ => Array.empty[Byte]))

    framed.map(_ => ())

  /**
   * The body, decoded by its schema.
   *
   * Total, and inherited rather than re-invented: a truncated body
   * decodes to the value it carried with the damage visible, because
   * `Json.read` runs the total scanner and parser underneath. This
   * module adds no error type of its own.
   */
  def json[A](r: Response)(using Schema[A]): Either[String, A] ! Async =
    text(r).map(okay.codec.Json.read[A](_))

  /** the body as server-sent events: our lines, through the stage
   * okay-llm already wrote — the same payloads, because it IS that
   * stage */
  def sse(r: Response): Source[String] =
    okay.through[String, String, Async, Unit, Unit](lines(r))(
      !.widen[Unit, Take % String + Writer % String, Async](okay.llm.Sse.events))
}
