package okay

/**
 * Run a body on a thread with a SMALL stack (specs/stack-safety.md):
 * a recursion that uses the stack overflows here at a few thousand
 * levels, so a stack-safety test needs a depth it can build in
 * milliseconds rather than the hundreds of thousands a default stack
 * holds. A body that is a loop or a trampoline does not care.
 */
object SmallStack:
  def apply[A](kb: Int = 128)(body: => A): Either[Throwable, A] =
    var out: Either[Throwable, A] = Left(new IllegalStateException("never ran"))
    val t = new Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", kb.toLong * 1024)
    t.start()
    t.join()
    out

  /** the answer, or the failure rethrown as the test's own: a
   * StackOverflowError names the recursion in its trace */
  def run[A](kb: Int = 128)(body: => A): A = apply(kb)(body).fold(e => throw e, identity)
