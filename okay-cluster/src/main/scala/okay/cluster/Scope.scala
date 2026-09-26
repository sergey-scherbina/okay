package okay.cluster

/**
 * ONE PARTITION'S LIFE, as the engine sees it (stateful-early-stop): opened
 * before a partition's chunks are built, closed when the engine is done
 * reading them — at their end, when a `take` downstream stopped early, or
 * when a step threw. A stage that HOLDS something for the partition (a
 * leased interpreter, a far-side state) registers its close here, because a
 * chunked stream is a pure value: a consumer that stops simply stops, and
 * nothing in the stream tells the stage above it.
 *
 * Closes run once, in reverse registration order; a close that throws does
 * not stop the ones after it, and the first failure is rethrown at the end.
 */
final class Scope:
  private var closes: List[() => Unit] = Nil
  private var closed = false

  /** run `f` when the partition ends; at once if it already has */
  def onEnd(f: () => Unit): Unit =
    val now = synchronized { if closed then true else { closes = f :: closes; false } }
    if now then f()

  def close(): Unit =
    val fs = synchronized { closed = true; val c = closes; closes = Nil; c }
    var first: Throwable | Null = null
    fs.foreach(f => try f() catch case t: Throwable => if first == null then first = t)
    if first != null then throw first.nn

object Scope:
  /** `body` under a scope that is closed when it returns or throws */
  def using[T](body: Scope => T): T =
    val s = Scope()
    try body(s) finally s.close()
