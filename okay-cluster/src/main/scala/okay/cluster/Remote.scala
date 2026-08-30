package okay.cluster

import okay.{Channel, Chunk, Chunks, Scheduler}
import okay.given
import okay.codec.{Json, Schema}
import java.net.{ServerSocket, Socket}
import java.io.{BufferedReader, InputStreamReader, PrintWriter}

/**
 * The remote channel (specs/cluster.md): the Channel discipline with
 * a socket underneath — send chunks on one node, receive them on
 * another, the SAME consumer code either way. Values travel by their
 * Schema through the JSON dialect, one chunk per line (CBOR takes
 * over when its dialect lands); the wire closing closes the channel,
 * after the buffered chunks drain — exactly a local channel's
 * contract. Chunks are the shipping unit, as everywhere.
 */
object Remote {

  /**
   * Listen for one peer: accepted chunks land in an ordinary local
   * Channel — downstream code cannot tell it is remote.
   */
  def listen[A](server: ServerSocket)(using Schema[List[A]], Scheduler): Channel[Chunk[A]] =
    val ch = Channel[Chunk[A]]()
    summon[Scheduler].fork { () =>
      okay.async:
        try
          val sock = server.accept()
          val in = BufferedReader(InputStreamReader(sock.getInputStream))
          var line = in.readLine()
          while line != null do
            Json.read[List[A]](line) match
              case Right(xs) =>
                ch.send(Chunks.wrap[A](xs.toArray[Any].asInstanceOf[Array[AnyRef]]))
              case Left(_) => ()   // a damaged frame is dropped, the stream lives
            line = in.readLine()
          sock.close()
        finally ch.close()
    }
    ch

  /** the sending end: chunks out, one JSON frame per line */
  final class Sender[A](sock: Socket)(using Schema[List[A]]):
    private val out = PrintWriter(sock.getOutputStream, true)

    def send(c: Chunk[A]): Unit = out.println(Json.write(c.toList))

    def close(): Unit = { out.flush(); sock.close() }

  def connect[A](host: String, port: Int)(using Schema[List[A]]): Sender[A] =
    Sender(Socket(host, port))
}
