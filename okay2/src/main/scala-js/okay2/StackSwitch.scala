package okay2

/** Scala.js: no thread to switch to and no stack to read — the engine's
 * stack is the written bound (specs/cont-stack.md, the JS row); the
 * room never counts down, so `more` is never asked */
private[okay2] object StackSwitch {
  val coldBytesPerLevel: Long = 1200L
  val firstRoom: Int = Int.MaxValue
  var switches: Long = 0L
  def more(g: ContImpl.Gauge): Int = 0
  def fresh[R](body: Int => R): R = body(Int.MaxValue)
}
