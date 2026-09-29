package okay.desktop

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException, ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

/**
 * ONE COPY OF THE APP, known by its data folder, not by a port
 * (specs/app-in-process.md): the first copy holds a lock on
 * `<data>/app.lock` for as long as it runs and listens on a Unix-domain
 * socket, `<data>/app.sock` — a file, which nothing else on the computer
 * can answer for; a second start finds the lock held, says `front` on
 * the socket, and ends.
 *
 * Before this the app asked "is a copy running?" of whoever answered on
 * its port, and a Docker copy of the same service answered it (okay-watch
 * bug desktop-port-conflict, 2026-09-28).
 */
final class Instance private (channel: FileChannel, lock: FileLock, sock: Path):
  @volatile private var server: Option[ServerSocketChannel] = None

  /** `front` from a second start calls `onFront`; where a Unix-domain
   * socket cannot be made (a path too long for it) there is none, and a
   * second start still ends */
  def listen(onFront: () => Unit): Boolean =
    scala.util.Try {
      Files.deleteIfExists(sock)
      val s = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
      s.bind(UnixDomainSocketAddress.of(sock))
      server = Some(s)
      Thread.ofPlatform().daemon(true).name("app-instance").start { () =>
        while s.isOpen do
          val _ = scala.util.Try {
            val c = s.accept()
            try
              val buf = ByteBuffer.allocate(64)
              c.read(buf)
              if new String(buf.array, 0, buf.position, UTF_8).trim == Instance.Front then onFront()
            finally c.close()
          }
      }
    }.isSuccess

  /** the lock and the socket given back (the process ending does it too) */
  def release(): Unit =
    server.foreach(s => scala.util.Try(s.close()))
    val _ = scala.util.Try(Files.deleteIfExists(sock))
    val _ = scala.util.Try(lock.release())
    scala.util.Try(channel.close()): Unit

object Instance:
  val LockFile = "app.lock"
  val SocketFile = "app.sock"
  private val Front = "front"

  /** this copy is the first in `data`: the lock, held; or None when
   * another copy holds it */
  def claim(data: Path): Option[Instance] =
    Files.createDirectories(data)
    val ch = FileChannel.open(data.resolve(LockFile), StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    val got =
      try Option(ch.tryLock())
      catch case _: OverlappingFileLockException => None
    got match
      case Some(l) => Some(Instance(ch, l, data.resolve(SocketFile)))
      case None => ch.close(); None

  /** tell the first copy to come to the front; whether it heard */
  def front(data: Path): Boolean =
    scala.util.Try {
      val c = SocketChannel.open(UnixDomainSocketAddress.of(data.resolve(SocketFile)))
      try c.write(ByteBuffer.wrap((Front + "\n").getBytes(UTF_8))) finally c.close()
    }.isSuccess
