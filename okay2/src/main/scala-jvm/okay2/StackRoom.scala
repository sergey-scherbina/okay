package okay2

/**
 * What the JVM can say about the stack it is running on — NOTHING on this side of JDK 22 (specs/cont-stack.md
 * Layer 3, in the Scala 2 core): every method answers −1 and `StackSwitch.more` turns −1 into "no grant: switch".
 * On JDK 22+ the Multi-Release variant in okay2's jar (`jdk22/StackRoom.scala`, FFM; okay2-stackroom-jdk22) reads
 * the pointer and the bounds instead. The same public shape in both.
 */
private[okay2] object StackRoom {
  def readable: Boolean = false
  def readableWithout(symbol: String): Boolean = false
  def sp(): Long = -1L
  def top(): Long = -1L
  def floor(): Long = -1L
}
