package okay

/**
 * The cross-platform byte-stream seam (specs/net.md): three wire
 * protocols in this stack speak raw TCP, and every platform gets
 * ONE honest way in — a blocking socket behind Async.Run where
 * blocking is real (JVM virtual threads, Native), buffered pulls
 * over Node's `net` where nothing may block (JS). The seam moves
 * BYTES; framing stays with the protocol that owns it.
 */
trait NetConn:
  /** exactly n bytes, or a throw naming the shortfall — a
   * half-frame is damage at the transport, not a hang */
  def readFully(n: Int): Array[Byte] ! Async
  def write(bytes: Array[Byte]): Unit ! Async
  def close(): Unit

/** truncated mid-read: the far end closed inside a frame */
final case class NetEof(wanted: Int, got: Int)
  extends RuntimeException(s"connection ended mid-read: wanted $wanted bytes, got $got")

trait Net:
  def connect(host: String, port: Int): NetConn ! Async

object Net:
  /** the platform's given decides how bytes move */
  def connect(host: String, port: Int)(using n: Net): NetConn ! Async =
    n.connect(host, port)
