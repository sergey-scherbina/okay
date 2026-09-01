package okay.http

import okay.*
import okay.given
import okay.codec.{Json, Schema}

/**
 * The acceptance run's shared half — the fixture ONE program on two
 * platforms agrees about.
 *
 * okay-cluster established this shape and the reason is the same here:
 * a JS client and a JVM server prove nothing if each carries its own
 * copy of what to expect. The routes, the session, the values and the
 * schema all live here, compiled to both platforms, so the two ends
 * cannot drift and still pass.
 *
 * What it is for: okay-http's JS transports compile and, until this,
 * had never run. That is the failure mode `js.Dynamic` is worst at —
 * a mistyped field name is `undefined`, not an error, so a transport
 * can be entirely broken and entirely green.
 */
object Acceptance {

  final case class Person(name: String, age: Int)
  given Schema[Person] = Schema.derived

  val person: Person = Person("ann", 41)

  /** a body the JS side must reassemble from however many chunks
   * `ReadableStream` hands it — long enough that it will be several */
  val lines: Seq[String] = (1 to 200).map(i => s"line-$i")
  val body: String = lines.mkString("\n") + "\n"

  val greeting = "hello from js"
  val echoed = "echo:" + greeting

  /** the session BOTH ends run — the client tells, the server echoes */
  def echo: Stage[Frame, Frame, Unit] =
    val framed: Stage[Frame, Frame, Unit] =
      Stage.transduce(())((_, f) =>
        f match
          case Frame.Text(t) => Stage.tell[Frame, Frame](Frame.Text("echo:" + t))
          case Frame.Binary(b) => Stage.tell[Frame, Frame](Frame.Binary(b))
          case _ => pure(()),
        _ => pure(()))
    framed

  /** what the client says, and what it expects back */
  def say: Stage[Frame, Frame, Seq[Frame]] =
    Stage.tell[Frame, Frame](Frame.Text(greeting)).flatMap(_ =>
      Stage.await[Frame, Frame].map {
        case Some(f) => Seq(f)
        case None => Seq.empty
      })

  private def text(s: String, kind: String): Response =
    Response(200, Seq(("content-type", kind)),
      Http.one(s.getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  /** the routes the JVM side serves — shared, so there is one truth */
  def routes: PartialFunction[Request, Response ! Async] =
    case r if r.url.startsWith("/person") =>
      pure(text(Json.write(person), "application/json"))
    case r if r.url.startsWith("/lines") =>
      pure(text(body, "text/plain; charset=utf-8"))
    case r if r.url.startsWith("/echo") =>
      Http.text(Response(200, Nil, Http.one(r.body.bytes)))
        .map(t => text("you said: " + t, "text/plain; charset=utf-8"))

  /**
   * The client's whole run, as a program — shared source, so the JVM
   * suite can run the IDENTICAL check and the acceptance is a
   * comparison rather than a hope.
   */
  def check(http: Http, sockets: Sockets, port: Int): Seq[(String, Boolean)] ! Async =
    val base = s"http://127.0.0.1:$port"
    for
      p <- http.send(Request.get(s"$base/person")).flatMap(Http.json[Person])
      ls <- http.send(Request.get(s"$base/lines"))
        .flatMap(r => Writer.run[String, Unit, Async](Http.lines(r)).map(_._1))
      posted <- http.send(Request.post(s"$base/echo", Body.Text(greeting)))
        .flatMap(Http.text)
      frames <- sockets.connect(s"ws://127.0.0.1:$port/ws").flatMap(sock =>
        Ws.over(sock)(say).flatMap(fs => sock.close().map(_ => fs)))
    yield Seq(
      "json body decodes to the shared value" -> (p == Right(person)),
      "a streamed body reassembles into every line" -> (ls == lines),
      "a POST body reaches the route" -> (posted == "you said: " + greeting),
      "a websocket session round-trips" -> (frames == Seq(Frame.Text(echoed))))
}
