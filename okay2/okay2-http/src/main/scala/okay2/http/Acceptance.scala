package okay2.http

import java.nio.charset.StandardCharsets.UTF_8
import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.codec.Json
import okay2.stream.Stage

/**
 * One acceptance check every transport pair runs (okay-http's
 * Acceptance): a JSON value, a streamed body of 200 lines, a POST body,
 * and a WebSocket session, served by `router` and read back by the
 * transports under test.
 */
object Acceptance {

  final case class Person(name: String, age: Int)

  val person: Person = Person("ann", 41)

  val lines: Seq[String] = (1 to 200).map(i => s"line-$i")
  val body: String = lines.mkString("\n") + "\n"

  val greeting = "hello from js"
  val echoed = "echo:" + greeting

  /** the server's session: text echoed with a prefix, binary as is */
  def echo: Stage[Frame, Frame, Unit] =
    Stage.transduce[Frame, Frame, Unit](())((_, f) => f match {
      case Frame.Text(t) => Stage.tell[Frame, Frame](Frame.Text("echo:" + t))
      case Frame.Binary(b) => Stage.tell[Frame, Frame](Frame.Binary(b))
      case _ => pure(())
    }, _ => pure(()))

  /** the client's session: say the greeting, keep the one answer */
  def say: Stage[Frame, Frame, Seq[Frame]] =
    Stage.tell[Frame, Frame](Frame.Text(greeting)).flatMap(_ =>
      Stage.await[Frame, Frame].map {
        case Some(f) => Seq(f)
        case None => Seq.empty
      })

  private def text(s: String, kind: String): Response = Response(200, Seq(("content-type", kind)), Http.one(s.getBytes(UTF_8)))

  val router: Router = Router.empty
    .on(Method.Get, Route.lit("person"))(_ => pure[Async, Response](text(Json.write(person), "application/json")))
    .on(Method.Get, Route.lit("lines"))(_ => pure[Async, Response](text(body, "text/plain; charset=utf-8")))
    .at(Method.Post, Route.lit("echo"))((_, r) =>
      Http.text(Response(200, Nil, Http.one(r.body.bytes))).map(t => text("you said: " + t, "text/plain; charset=utf-8")))

  def routes: PartialFunction[Request, Response ! Async] = router.routes

  def check(http: Http, sockets: Sockets, port: Int): Seq[(String, Boolean)] ! Async =
    rest(http, port).flatMap { r =>
      sockets.connect(s"ws://127.0.0.1:$port/ws")
        .flatMap(sock => Ws.over(sock)(say).flatMap(fs => sock.close().map(_ => fs)))
        .map(frames => r :+ ("a websocket session round-trips" -> (frames == Seq(Frame.Text(echoed)))))
    }

  def rest(http: Http, port: Int): Seq[(String, Boolean)] ! Async = {
    val base = s"http://127.0.0.1:$port"
    for {
      p <- http.send(Request.get(s"$base/person")).flatMap(r => Http.json[Person](r))
      ls <- http.send(Request.get(s"$base/lines")).flatMap(r => Writer.collect[String, Unit, Async](Http.lines(r)).map(_._1))
      posted <- http.send(Request.post(s"$base/echo", Body.Text(greeting))).flatMap(Http.text)
    } yield Seq(
      "json body decodes to the shared value" -> (p == Right(person)),
      "a streamed body reassembles into every line" -> (ls == lines),
      "a POST body reaches the route" -> (posted == "you said: " + greeting))
  }
}
