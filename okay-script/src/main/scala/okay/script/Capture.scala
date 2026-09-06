package okay.script

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}

/** Per-THREAD stdout capture -- what makes `Compiled.invoke` safe to
 * run concurrently on many threads, which a server does.
 *
 * Before okay-script-site, `invoke()` did `System.setOut(buffer)` for
 * its duration: JVM-global, so two pages answering two requests on
 * two threads would have captured each other's output. Now ONE
 * routing `PrintStream` is installed as `System.out` (once, lazily,
 * keeping the original as the fallback) for a page's Java-level
 * `System.out` writes and for `include`'s own print, and
 * `scala.Console.withOut` (a `DynamicVariable`, per thread by
 * construction; the script shares the host's `Console` since `scala.*`
 * is delegated) covers `println`; per invocation only a `ThreadLocal`
 * buffer changes. A thread with no buffer set writes through to the
 * original `System.out` unchanged, so the host program's own
 * printing is untouched. Nested capture (a page `include`-ing another
 * on the same thread) saves and restores the enclosing buffer.
 */
object Capture:
  @volatile private var original: PrintStream = System.out
  private val local: ThreadLocal[OutputStream] = new ThreadLocal

  private def target: OutputStream =
    val t = local.get()
    if t == null then original else t

  /** the routing stream: every write goes to the calling thread's
   * capture buffer, or to the original `System.out` when none is set */
  val stream: PrintStream = new PrintStream(
    new OutputStream:
      override def write(b: Int): Unit = target.write(b)
      override def write(b: Array[Byte], off: Int, len: Int): Unit = target.write(b, off, len)
      override def flush(): Unit = target.flush(),
    true,
    "UTF-8",
  )

  /** installs `stream` as `System.out` if it is not already */
  def install(): Unit = synchronized:
    if System.out ne stream then
      original = System.out
      System.setOut(stream)

  /** runs `body` with this thread's stdout captured; returns the
   * body's value and everything it printed */
  def capturing[A](body: => A): (A, String) =
    install()
    val prev = local.get()
    val buf = new ByteArrayOutputStream()
    local.set(buf)
    try
      val a = scala.Console.withOut(stream)(body)
      stream.flush()
      (a, buf.toString("UTF-8"))
    finally
      if prev == null then local.remove() else local.set(prev)
