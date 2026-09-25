package okay.cluster

import okay.{Channel, Chunk, Scheduler}
import okay.given
import okay.codec.{Cbor, Schema}
import okay.arrow.{OkayArrow, Rows}
import java.net.{ServerSocket, Socket}
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, EOFException}

/**
 * How a chunk travels between nodes (okay-arrow stage 7b,
 * specs/okay-arrow.md). The default is ARROW: a chunk is a batch of
 * records, and measured (arrow-vs-cbor) uncompressed Arrow is smaller,
 * lighter on the heap and 3.5–5x faster than CBOR on every shape.
 *
 * {{{
 * import okay.cluster.RemoteFormat.Cbor.given   // fewer bytes for text or nested records, compressed
 * import okay.cluster.RemoteFormat.Json.given   // the old wire
 * }}}
 */
final case class RemoteFormat(name: String, tag: Int)

object RemoteFormat:
  given arrow: RemoteFormat = RemoteFormat("arrow", 'A')
  object Cbor:
    given cbor: RemoteFormat = RemoteFormat("cbor", 'C')
  object Json:
    given json: RemoteFormat = RemoteFormat("json", 'J')

/** Whether a chunk is compressed on the wire: none by default; LZ4 or
 * ZSTD (okay-compress). Arrow compresses per buffer inside its IPC body;
 * CBOR and JSON compress the whole payload. */
final case class RemoteCompression(codec: Option[okay.compress.Codec], tag: Int)

object RemoteCompression:
  given none: RemoteCompression = RemoteCompression(None, 0)
  object Lz4:
    given lz4: RemoteCompression = RemoteCompression(Some(okay.compress.Lz4Frame), 1)
  object Zstd:
    given zstd: RemoteCompression = RemoteCompression(Some(okay.compress.Zstd), 2)

/**
 * The remote channel (specs/cluster.md): the Channel discipline with
 * a socket underneath — send chunks on one node, receive them on
 * another, the SAME consumer code either way. Chunks are the shipping
 * unit, as everywhere.
 *
 * The wire is FRAMES: a 4-byte big-endian length, a format tag, a
 * compression tag, the payload. The receiver reads the tags, so one
 * listener takes any sender's format; a frame it cannot decode is
 * dropped and the stream lives; the wire closing closes the channel,
 * after the buffered chunks drain — exactly a local channel's contract.
 */
object Remote {

  /**
   * Listen for one peer: accepted chunks land in an ordinary local
   * Channel — downstream code cannot tell it is remote.
   */
  def listen[A](server: ServerSocket)(using Schema[A], Scheduler): Channel[Chunk[A]] =
    val ch = Channel[Chunk[A]]()
    val _ = summon[Scheduler].fork { () =>
      okay.async:
        try
          val sock = server.accept()
          val in = DataInputStream(BufferedInputStream(sock.getInputStream))
          var more = true
          while more do
            frame(in) match
              case None => more = false
              case Some((fmt, cmp, payload)) =>
                decode[A](fmt, cmp, payload) match
                  case Right(xs) => ch.sendBlocking(okay.ChunkBuf.of(xs)): Unit
                  case Left(_) => ()   // a damaged frame is dropped, the stream lives
          sock.close()
        // the same hole Channel.merge had: without this a reset
        // connection or a broken stream dies on this fiber, `finally`
        // closes the channel, and the consumer reads a perfectly
        // ordinary end — a truncated remote stream indistinguishable
        // from a complete one
        catch case e: Throwable => ch.fail(e)
        finally ch.close()
    }
    ch

  /** the next frame, or None at a clean end of the wire */
  private def frame(in: DataInputStream): Option[(Int, Int, Array[Byte])] =
    val len =
      try in.readInt()
      catch case _: EOFException => return None
    if len < 2 then throw IllegalStateException(s"a frame of $len bytes: not the okay remote wire")
    val fmt = in.readUnsignedByte()
    val cmp = in.readUnsignedByte()
    val payload = new Array[Byte](len - 2)
    in.readFully(payload)
    Some((fmt, cmp, payload))

  private def decode[A](fmt: Int, cmp: Int, payload: Array[Byte])(using Schema[A]): Either[String, List[A]] =
    try
      def plain(): Array[Byte] = cmp match
        case 0 => payload
        case 1 => okay.compress.Lz4Frame.decompress(payload)
        case 2 => okay.compress.Zstd.decompress(payload)
        case other => throw IllegalStateException(s"compression tag $other")
      fmt match
        case 'A' => OkayArrow.decode[A](payload).map(_.toList)      // Arrow carries its own compression
        case 'C' => Cbor.read[List[A]](plain())
        case 'J' => okay.codec.Codecs.readJson[List[A]](String(plain(), java.nio.charset.StandardCharsets.UTF_8))
        case other => Left(s"format tag $other")
    catch case e: RuntimeException => Left(String.valueOf(e.getMessage))

  /** the sending end: chunks out, one frame each, in the given format */
  final class Sender[A](sock: Socket)(using Schema[A], RemoteFormat, RemoteCompression):
    private val out = DataOutputStream(BufferedOutputStream(sock.getOutputStream))
    private val format = summon[RemoteFormat]
    private val compression = summon[RemoteCompression]
    private lazy val json = okay.codec.Codecs.json(summon[Schema[List[A]]])

    def send(c: Chunk[A]): Unit =
      val payload = format.tag match
        case 'A' => OkayArrow.write(Rows.table(c.toList), compression.codec)
        case 'C' => packed(Cbor.write(c.toList))
        case _ => packed(json.encode(c.toList).getBytes(java.nio.charset.StandardCharsets.UTF_8))
      out.writeInt(payload.length + 2)
      out.writeByte(format.tag)
      out.writeByte(compression.tag)
      out.write(payload)
      out.flush()

    private def packed(b: Array[Byte]): Array[Byte] = compression.codec.fold(b)(_.compress(b))

    def close(): Unit = { out.flush(); sock.close() }

  def connect[A](host: String, port: Int)(using Schema[A], RemoteFormat, RemoteCompression): Sender[A] =
    Sender(Socket(host, port))
}
