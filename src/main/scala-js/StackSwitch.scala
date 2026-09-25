package okay

/**
 * Scala.js has no threads to switch to (specs/stack-safety.md stage
 * 1c): the rest continues on the same stack, and the depth of shifts in
 * a row whose bodies call their continuation is bounded by the engine's
 * own stack. Written down in the spec as this platform's bound.
 */
private[okay] object StackSwitch:
  val firstRoom: Int = Int.MaxValue
  def fresh[R](body: Int => R): R = body(Int.MaxValue)
