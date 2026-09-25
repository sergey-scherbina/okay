package okay2

/**
 * Run a body on a thread with a SMALL stack (specs/stack-safety.md): a
 * recursion that uses the stack overflows here at a few thousand
 * levels, so a stack-safety test builds its depth in milliseconds. JVM
 * only: a thread's stack size is a JVM notion.
 */
object SmallStack {
  def run[A](kb: Int = 128)(body: => A): A = {
    var out: Either[Throwable, A] = Left(new IllegalStateException("never ran"))
    val t = new Thread(null, () => out = try Right(body) catch { case e: Throwable => Left(e) }, "small-stack", kb.toLong * 1024)
    t.start()
    t.join()
    out.fold(e => throw e, identity)
  }
}
