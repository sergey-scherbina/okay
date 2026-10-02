package okay

/** Scala.js: no thread to switch to; deep opaque bodies are bounded by the engine's stack. */
private[okay] object StackSwitch:
  val coldBytesPerLevel: Long = 1200L
  val firstRoom: Int = Int.MaxValue
  var switches: Long = 0L
  def fresh[R](body: Int => R): R = body(Int.MaxValue)
