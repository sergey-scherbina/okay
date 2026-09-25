package okay2

/**
 * What the JVM can say about the stack it is running on — NOTHING on
 * this side of JDK 22 (specs/cont-stack.md Layer 3, in the Scala 2
 * core): every method answers −1 and `StackSwitch.more` turns −1 into
 * "no grant: switch". The Scala 3 core's JDK 22+ variant (`jdk22/`, a
 * Multi-Release class reading the pointer through FFM) has no okay2
 * twin yet — okay2/backlog.d names it — so okay2 counts on every JDK.
 */
private[okay2] object StackRoom {
  def sp(): Long = -1L
  def top(): Long = -1L
  def floor(): Long = -1L
}
