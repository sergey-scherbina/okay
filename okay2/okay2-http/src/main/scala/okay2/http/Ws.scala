package okay2.http

import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.stream.{Chunk, Pipe, Source, Stage}

/** a WebSocket frame (okay-http's Ws.scala) */
sealed trait Frame

object Frame {
  final case class Text(s: String) extends Frame
  final case class Binary(b: Chunk[Byte]) extends Frame
  final case class Ping(b: Chunk[Byte]) extends Frame
  final case class Pong(b: Chunk[Byte]) extends Frame
  final case class Close(code: Int, reason: String) extends Frame

  /** 1000, a normal closure */
  val Normal = 1000
}

/** one open socket: frames out, frames in */
trait Socket {
  def send(f: Frame): Unit ! Async
  def frames: Source[Frame]
  def close(code: Int, reason: String): Unit ! Async
  def close(): Unit ! Async = close(Frame.Normal, "")
}

/** how sockets are opened: the client side */
trait Sockets {
  def connect(url: String, headers: Seq[(String, String)] = Nil, subprotocols: Seq[String] = Nil): Socket ! Async
}

/** a session is a Stage from the frames that arrive to the frames sent,
 * so it is written and tested with no socket (TestFraming) */
object Ws {

  /** run a session over a socket: its tells are sent as they are made */
  def over[A](s: Socket)(session: Stage[Frame, Frame, A]): A ! Async = {
    val answered: A ! (Writer[Frame] with Async) = Pipe.intoIn[Frame, Frame, Async, Unit, A](s.frames)(session)

    def drain(p: A ! (Writer[Frame] with Async)): A ! Async =
      Writer.unconsIn[Frame, A, Async](p).flatMap {
        case Left(a) => pure[Async, A](a)
        case Right((f, rest)) => s.send(f).flatMap(_ => drain(rest))
      }

    drain(answered)
  }

  /** the text frames, as strings; the rest dropped */
  def texts: Stage[Frame, String, Unit] =
    Stage.transduce[Frame, String, Unit](())((_, f) => f match {
      case Frame.Text(t) => Stage.tell[Frame, String](t)
      case _ => pure(())
    }, _ => pure(()))
}
