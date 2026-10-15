package okay.foreign

import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}

/**
 * A HOST THAT ACCEPTS far sides (foreign-server, specs/foreign-server.md):
 * a far side — a TypeScript program in another process or on another
 * machine, okay-ts's programs as data, any worker that dials — connects,
 * says its hello, and is driven as every far side is: `Foreign.program`,
 * callbacks, multi-shot, direct style, held values, mux, `Durable`. Only the
 * connection's direction is new; the engine is `ForeignWorker.over` an
 * accepted socket (`WireLink.accepted`). PLAIN TCP, as `WireLink.tcp`.
 *
 * {{{
 * val server = ForeignServer.listen(9000)
 * server.serve { w =>
 *   given okay.Answers[ForeignEval] = w.handler
 *   Ts.program[Long]("client:main").calling(Ts.callbacks(transfer, balance))().runWith
 * }
 * }}}
 */
final class ForeignServer private (socket: ServerSocket, helloMillis: Int)
                                  (using WireFormat, WireCompression, okay.codec.WireAuth, WireDeadline)
                                  (using okay.codec.FrameFormat):

  /** the port it listens on (an ephemeral one when listened on 0) */
  def port: Int = socket.getLocalPort

  /** the next far side to connect, as an engine — its hello read (a far side
   * silent for `helloMillis` refused by name, its connection closed) */
  def accept(): ForeignWorker = engine(socket.accept())

  /**
   * Every far side to connect, each on a thread of its own: its engine given
   * to `session`, closed when the session returns or throws. A far side
   * refused (its hello) or a session that throws is told to `failed`, and the
   * server goes on accepting. Returns at once; `close` ends it.
   */
  def serve(session: ForeignWorker => Unit,
            failed: Throwable => Unit = e => System.err.println(s"okay.foreign.ForeignServer: ${e.getMessage}")): Unit =
    val loop = Thread(() =>
      while !socket.isClosed do
        try
          val s = socket.accept()
          val t = Thread(() =>
            try
              val w = engine(s)
              try session(w) finally w.close()
            catch case e: Exception => failed(e)
          , "okay-foreign-session")
          t.setDaemon(true)
          t.start()
        catch case _: java.net.SocketException => ()   // closed: the loop ends
    , "okay-foreign-server")
    loop.setDaemon(true)
    loop.start()

  /** stops listening; engines already accepted are their sessions' to close */
  def close(): Unit = try socket.close() catch case _: java.io.IOException => ()

  private def engine(s: Socket): ForeignWorker =
    try ForeignWorker.over(WireLink.accepted(s, helloMillis), s"the far side at ${s.getRemoteSocketAddress}")
    catch case e: Throwable =>
      try s.close() catch case _: java.io.IOException => ()
      throw e

object ForeignServer:
  /** listen on `host:port` (port 0: any free one) for far sides to connect */
  def listen(port: Int = 0, host: String = "127.0.0.1", helloMillis: Int = 10000)
            (using WireFormat, WireCompression, okay.codec.WireAuth, WireDeadline)
            (using okay.codec.FrameFormat): ForeignServer =
    val s = ServerSocket()
    s.bind(InetSocketAddress(InetAddress.getByName(host), port))
    new ForeignServer(s, helloMillis)
