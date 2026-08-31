package okay.http

import okay.*
import okay.given

/**
 * A WebSocket frame.
 *
 * Fragmentation is NOT here: the JDK reports a long message as several
 * listener calls with a `last` flag, and a browser reports it as one
 * `onmessage`. The transports join the pieces so a session sees one
 * `Text` or one `Binary` either way — a session that had to reassemble
 * would be a session written against one platform.
 *
 * Ping is answered by the transports (the JDK does it itself unless
 * overridden, and a browser never even shows it), so a session sees a
 * `Ping` only as information. An explicit `Pong` a session tells is
 * still sent, because an unsolicited pong is a legitimate heartbeat.
 */
enum Frame:
  case Text(s: String)
  case Binary(b: Chunk[Byte])
  case Ping(b: Chunk[Byte])
  case Pong(b: Chunk[Byte])
  case Close(code: Int, reason: String)

object Frame:
  /** the normal closure, RFC 6455 §7.4.1 */
  val Normal = 1000

/**
 * An open socket, in the shape okay-mcp already consumes.
 *
 * `frames` is a `Source`, which is to say a program that tells them —
 * so it is an ordinary `Stream` in `Async` and every combinator
 * applies. Note what is NOT here: `request(n)`. The JDK's listener is
 * genuinely pull-based (demand starts at zero, `request` raises it, the
 * socket stops calling at zero), and browser and Node WebSocket have no
 * receive-side lever at all. A shared method one platform silently
 * fakes is worse than an honest asymmetry, so the demand stays inside
 * the JVM transport and the JS one states its buffer bound.
 */
trait Socket:
  def send(f: Frame): Unit ! Async
  def frames: Source[Frame]
  def close(code: Int, reason: String): Unit ! Async

  /** the normal closure */
  def close(): Unit ! Async = close(Frame.Normal, "")

trait Sockets:
  def connect(url: String, headers: Seq[(String, String)] = Nil,
              subprotocols: Seq[String] = Nil): Socket ! Async

object Ws {

  /**
   * Run a session over a socket.
   *
   * The session is a `Stage[Frame, Frame, A]` — it awaits incoming
   * frames and tells outgoing ones — which is not an analogy for what a
   * WebSocket is but a description of it. `Mcp.over` is the same six
   * lines over `Rpc`, and that is the point: the vocabulary was already
   * here, so this module contributes a transport and not a paradigm.
   */
  def over[A](s: Socket)(session: Stage[Frame, Frame, A]): A ! Async =
    val answered: A ! (Writer % Frame + Async) =
      through[Frame, Frame, Async, Unit, A](s.frames)(
        !.widen[A, Take % Frame + Writer % Frame, Async](session))

    def drain(p: A ! (Writer % Frame + Async)): A ! Async =
      Writer.uncons[Frame, A, Async](p).flatMap {
        case Left(a) => pure(a)
        case Right((f, rest)) => s.send(f).flatMap(_ => drain(rest))
      }

    drain(answered)

  /** text frames out, everything else dropped — the projection a
   * line-oriented protocol wants */
  def texts: Stage[Frame, String, Unit] =
    Stage.transduce(())((_, f) =>
      f match
        case Frame.Text(t) => Stage.tell[Frame, String](t)
        case _ => pure(()),
      _ => pure(()))

  /**
   * A socket AS an MCP link — which is the whole reason the shapes were
   * kept the same.
   *
   * MCP has two standard transports: stdio, which okay-mcp has, and
   * HTTP+SSE, which it did not. A `Link` is `send(line)` plus
   * `lines: Source[String]`, and a WebSocket is exactly that with
   * frames around it, so `Mcp.run(link(socket), serving)` is the same
   * server over a different wire, with no protocol code changed.
   */
  def link(s: Socket): okay.mcp.Link = new okay.mcp.Link:
    def send(line: String): Unit ! Async = s.send(Frame.Text(line))

    def lines: Source[String] =
      through[Frame, String, Async, Unit, Unit](s.frames)(
        !.widen[Unit, Take % Frame + Writer % String, Async](texts))
}
