package okay

/**
 * Scala.js has no threads to switch to and no stack to read
 * (specs/cont-stack.md, the JS row): the rest continues on the same
 * stack, and the depth of shifts in a row whose OPAQUE bodies call
 * their continuation is bounded by the engine's own stack (~10 800
 * frames on V8's default 984 KB; `node --stack-size` raises it).
 * Written down in the spec and the docs as this platform's bound. The
 * room never counts down here, so `more` is never asked.
 */
private[okay] object StackSwitch:
  val coldBytesPerLevel: Long = 1200L
  val firstRoom: Int = Int.MaxValue
  var switches: Long = 0L
  def more(g: Cont.Gauge): Int = 0
  def fresh[R](body: Int => R): R = body(Int.MaxValue)
