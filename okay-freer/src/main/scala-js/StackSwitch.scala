package okay

/** Scala.js: no thread to switch to; deep opaque bodies are bounded by the engine's stack. */
private[okay] object StackSwitch:
  val coldBytesPerLevel: Long = 2600L
  val firstRoom: Int = Int.MaxValue
  /** no second stack here: a strict `k` too deep is RE-EXECUTED (ContReplay, cont-js-depth stage 4) */
  val replayByDefault: Boolean = true
  /** at the end of a room, levels this stack still takes: none is known here (nothing reads the stack) */
  def more(): Int = 0

  val levelsPerStack: Int = Int.MaxValue
  var switches: Long = 0L
  def fresh[R](body: Int => R): R = body(Int.MaxValue)
