package okay2.platform

import okay2._
import okay2.async._

/** `ScopedValue`'s shape, over a `ThreadLocal` with no public `set`:
 * `where` binds `value` for `body`'s extent only and restores whatever
 * was bound before, however `body` exits */
final class Scoped[A] private (default: () => A) {
  private val local: ThreadLocal[A] = new ThreadLocal[A] {
    override def initialValue(): A = default()
  }

  def current: A = local.get()

  def where[B](value: A)(body: => B): B = {
    val prior = local.get()
    local.set(value)
    try body finally local.set(prior)
  }
}

object Scoped {
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
}

/** the byte-stream seam: a blocking socket behind Async.Run */
trait NetConn {
  /** exactly n bytes, or a throw naming the shortfall */
  def readFully(n: Int): Array[Byte] ! Async
  def write(bytes: Array[Byte]): Unit ! Async
  def close(): Unit
}

/** truncated mid-read: the far end closed inside a frame */
final case class NetEof(wanted: Int, got: Int)
  extends RuntimeException(s"connection ended mid-read: wanted $wanted bytes, got $got")

trait Net { def connect(host: String, port: Int): NetConn ! Async }

object Net {
  def connect(host: String, port: Int)(implicit n: Net): NetConn ! Async = n.connect(host, port)
}
