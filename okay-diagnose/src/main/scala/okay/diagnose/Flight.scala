package okay.diagnose

/**
 * A FLIGHT RECORDER: the last `capacity` notes a test made, oldest first,
 * each with the thread that made it (specs/okay-diagnose.md). Bounded, so a
 * test that notes in a loop cannot grow it without end. Thread-safe,
 * because the notes that matter come from the threads a test starts.
 */
final class Flight(capacity: Int = 256):
  require(capacity > 0, "a flight recorder holds at least one note")
  private val buf = scala.collection.mutable.ArrayDeque.empty[String]
  private var dropped = 0L
  private val t0 = System.nanoTime()

  def note(msg: String): Unit = synchronized {
    if buf.size >= capacity then
      buf.removeHead(): Unit
      dropped += 1
    val ms = (System.nanoTime() - t0) / 1000000L
    buf.append(s"+${ms}ms [${Thread.currentThread().getName}] $msg"): Unit
  }

  def isEmpty: Boolean = synchronized(buf.isEmpty)

  /** every note kept, oldest first; how many older ones fell off is said */
  def dump: String = synchronized {
    val head = if dropped > 0 then s"  ($dropped older notes dropped)\n" else ""
    head + buf.map("  " + _).mkString("\n")
  }
