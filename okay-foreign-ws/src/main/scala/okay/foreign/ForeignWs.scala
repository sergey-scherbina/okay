package okay.foreign

import java.util.concurrent.LinkedBlockingQueue

import okay.{Channel, Source, Stage, drained}
import okay.freer.*
import okay.http.{Frame, Request}

/**
 * THE OKAY WIRE OVER A WEBSOCKET a server accepts (foreign-ws,
 * specs/foreign-ws.md): a far side in a browser — okay-ts in a page — or
 * behind a proxy that passes HTTP only, driven as every far side is
 * (`ForeignWorker`: `Foreign.program`, callbacks, multi-shot, direct style).
 * One JSON text frame per message; the far side says hello in its first.
 *
 * A Jetty session is a `Stage` that awaits — the client's frames and the
 * server's `push`, in arrival order — and tells. `WireLink` blocks, on the
 * engine's thread. The engine's lines go out as `push` frames (marked: no
 * JSON message starts with `\u0001`), the session telling each to the
 * client; the client's frames go into the queue the engine takes from. No
 * Jetty thread waits; the engine's does, as on every link.
 *
 * {{{
 * val ws = ForeignWs.route("/okay") { w => given okay.Answers[ForeignEval] = w.handler; ... }
 * Jetty.serve(0)(routes)(ws.sessions, ws.pushes)
 * }}}
 */
object ForeignWs:

  /** a WebSocket path's sessions and their pushes, for `Jetty.serve(port)(routes)(sessions, pushes)` */
  final class Route(val sessions: PartialFunction[Request, Stage[Frame, Frame, Unit]],
                    val pushes: PartialFunction[Request, Source[Frame]])

  private val Out = '\u0001'
  private val End = "\u0002"

  /**
   * Every far side connecting on `path`: its engine given to `session` on a
   * thread of its own, closed after it — the socket closed then. A far side
   * that never says hello, a session that throws: told to `failed`.
   */
  def route(path: String,
            failed: Throwable => Unit = e => System.err.println(s"okay.foreign.ForeignWs: ${e.getMessage}"))
           (session: ForeignWorker => Unit)
           (using WireFormat, WireCompression, okay.codec.WireAuth, WireDeadline)(using okay.codec.FrameFormat): Route =
    // a session's bridge, made when its upgrade is dispatched and taken by its push: Jetty asks both of ONE request
    val opening = java.util.Collections.synchronizedMap(java.util.IdentityHashMap[Request, Bridge]())
    def at(r: Request): Boolean = r.url.takeWhile(_ != '?') == path
    Route(
      { case r if at(r) =>
        val b = Bridge()
        opening.put(r, b)
        val t = Thread(() =>
          try
            val w = ForeignWorker.over(b.link, s"the far side at ${r.peer.getOrElse("?")} (WebSocket)")
            try session(w) finally w.close()
          catch case e: Exception =>
            b.link.close()
            failed(e)
        , "okay-foreign-ws")
        t.setDaemon(true)
        t.start()
        b.stage
      },
      { case r if at(r) && opening.containsKey(r) => opening.remove(r).pushes })

  /** one connection's two directions */
  private final class Bridge:
    /** the client's text frames; None: it is gone */
    private val in = LinkedBlockingQueue[Option[String]]()
    /** the engine's lines, marked, and the end */
    private val out = Channel[Frame](Int.MaxValue)

    def pushes: Source[Frame] = out.drained

    private def send(line: String): Unit = out.offer(Frame.Text(s"$Out$line")): Unit
    private def take(): Option[String] = in.take()

    val link: WireLink = new WireLink:
      def hello(): Option[String] = take()
      def roundTrip(line: String): Option[String] =
        send(line)
        take()
      def exchange(message: Array[Byte]): Option[Array[Byte]] =
        throw IllegalStateException("okay-foreign-ws carries JSON text frames, not binary frames: no format but JSON is configured over a WebSocket")
      def close(): Unit =
        out.offer(Frame.Text(End)): Unit
        in.offer(None): Unit
      override def network: Boolean = true
      override def duplex: Option[WireLink.Duplex] = Some(new WireLink.Duplex:
        def sendLine(line: String): Unit = send(line)
        def receiveLine(): Option[String] = take()
        def sendFrame(message: Array[Byte]): Unit = exchange(message): Unit
        def receiveFrame(): Option[Array[Byte]] = exchange(Array.emptyByteArray))

    /** the session: the engine's lines to the client, the client's into the queue; the end a Close */
    def stage: Stage[Frame, Frame, Unit] =
      Stage.await[Frame, Frame].flatMap {
        case Some(Frame.Text(t)) if t == End =>
          Stage.tell[Frame, Frame](Frame.Close(Frame.Normal, ""))
        case Some(Frame.Text(t)) if t.nonEmpty && t.charAt(0) == Out =>
          Stage.tell[Frame, Frame](Frame.Text(t.substring(1))).flatMap(_ => stage)
        case Some(Frame.Text(t)) =>
          in.put(Some(t))
          stage
        case Some(Frame.Close(_, _)) | None =>
          in.put(None)
          pure(())
        case Some(_) => stage
      }
