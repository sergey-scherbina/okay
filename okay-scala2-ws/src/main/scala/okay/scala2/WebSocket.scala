package okay.scala2

import okay.%
import okay.RowLift.plus
import okay.given
import okay.http.{Frame, Request}

/**
 * WebSockets for Scala 2.13 (specs/scala2-facade.md, stage 12).
 *
 * Probed first: okay-http's `Frame` (and matching on it), `Socket`,
 * `Transports.sockets()` and okay-jetty's `Jetty` are readable from
 * scalac 2.13. What is not usable: every socket operation answers a
 * program, and a server session is a `Stage[Frame, Frame, Unit]`, a
 * program that awaits frames and tells frames. So a client socket here
 * is `Eff` operations and a `Source` of frames, and a server session is
 * written the way Scala 2 writes a state machine, as a fold:
 * `(state, frame) => (state, frames to send)`. Underneath it is
 * okay-stream's `Stage.transduce`, the same shape a Scala 3 session has.
 */
final class WsClient private[scala2] (socket: okay.http.Socket) {

  def send(f: Frame): Eff[Async, Unit] = Async.lift(socket.send(f))

  def sendText(text: String): Eff[Async, Unit] = send(Frame.Text(text))

  /** every frame from the server, until it closes */
  def frames: Source[Frame] = Source.of(socket.frames)

  /** the text frames only */
  def texts: Source[String] =
    frames.mapConcat {
      case Frame.Text(t) => List(t)
      case _ => Nil
    }

  def close(): Eff[Async, Unit] = Async.lift(socket.close())
}

object WebSocket {

  /** a binary frame; okay's `Chunk` is `ArraySeq`, which Scala 2 can
   * also build itself (a Scala 3 alias is invisible from Scala 2, the
   * type it names is not) */
  def binary(bytes: Array[Byte]): Frame = Frame.Binary(scala.collection.immutable.ArraySeq.unsafeWrapArray(bytes))

  /** a binary frame's bytes */
  def bytes(f: Frame): Option[Array[Byte]] = f match {
    case Frame.Binary(b) => Some(b.toArray)
    case _ => None
  }

  /** open a client socket over okay-http's JDK transport */
  def connect(url: String): Eff[Async, WsClient] =
    Async.lift(okay.http.Transports.sockets().connect(url).map(s => new WsClient(s)))
}

/** a server-side session, held out of the constructor (the program
 * type names the row; see `ProgBody`) */
final class WsSession private (private val body: WsSessionBody)

private[scala2] final class WsSessionBody(val stage: okay.Stage[Frame, Frame, Unit]) extends AnyVal

object WsSession {

  /**
   * A session as a fold: for each frame the client sends, the next state
   * and the frames to send back; `init` is the state before the first
   * frame. The session ends when the client closes.
   */
  def fold[S](init: S)(step: (S, Frame) => (S, Seq[Frame])): WsSession = {
    def tells(fs: Seq[Frame]): okay.Stage[Frame, Frame, Unit] =
      fs.foldLeft(okay.pure(()): okay.Stage[Frame, Frame, Unit])((acc, f) =>
        acc.flatMap(_ => okay.Stage.tell[Frame, Frame](f)))
    val stage = okay.Stage.transduce[Frame, Frame, S](init)(
      (s, f) => { val (next, out) = step(s, f); tells(out).map(_ => next) },
      s => okay.pure(s)).map(_ => ())
    new WsSession(new WsSessionBody(stage))
  }

  /** every text frame sent straight back */
  val echo: WsSession = fold(()) {
    case (_, t @ Frame.Text(_)) => ((), Seq(t))
    case (_, _) => ((), Seq.empty)
  }

  private[scala2] def stage(s: WsSession): okay.Stage[Frame, Frame, Unit] = s.body.stage

  /** run a session against `incoming` with no socket at all: the frames
   * it sends back, in order. A session is a pure program, so this is
   * exactly what a client would receive. */
  def replay(s: WsSession, incoming: Seq[Frame]): Vector[Frame] = {
    val client: okay.![Unit, okay.Writer % Frame] =
      incoming.foldLeft(okay.pure(()): okay.![Unit, okay.Writer % Frame])((acc, f) =>
        acc.flatMap(_ => okay.Writer.tell(f)))
    okay.!.run(okay.Writer.collect[Frame, Unit, Nothing](okay.through(client)(stage(s))))._1
  }
}

/**
 * An HTTP server that also accepts WebSockets, over okay-jetty. `routes`
 * answer ordinary requests (the `Routes` of okay-scala2-http);
 * `sessions` choose a session for an upgrade request by its path, and
 * a request they do not match is refused the upgrade.
 */
object WsServer {

  def use[A](port: Int)(routes: Request => Eff[Async, Response])(sessions: PartialFunction[Request, WsSession])
            (body: Int => Eff[Async, A]): Eff[Async, A] =
    Async.lift(okay.Resource.run[A, okay.Async](
      okay.jetty.Jetty.serve(port)({ case r => Async.core(routes(r)).map(_.core) })(sessions.andThen(WsSession.stage))
        .plus[okay.Async]
        .flatMap(s => Async.core(body(okay.jetty.Jetty.port(s))).plus[okay.Resource])))
}
