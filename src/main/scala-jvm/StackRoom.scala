package okay

/**
 * What the JVM can say about the stack it is running on — NOTHING, on
 * this side of JDK 22 (specs/cont-stack.md Layer 3). Every method
 * answers −1, and `StackSwitch.more` turns −1 into "no grant: switch".
 *
 * THE OTHER SIDE is `jdk22/StackRoom.scala` — the same object, compiled
 * with `-java-output-version 22` against `java.lang.foreign` and
 * packaged under `META-INF/versions/22/` of this jar (build.sbt
 * `versioned`/`multiRelease`, JEP 238): a JVM of 22 or newer loads
 * THAT class in place of this one, and reads the stack pointer and
 * the thread's bounds exactly, when the user allowed native access.
 * No version test and no lookup here: the JVM picks the class.
 *
 * Bytes, not frames: a frame is 112 B compiled and ~1.2 KB
 * interpreted (measured), and an opaque body's frame is any size, so a
 * count of frames says nothing a grant can be built on.
 */
private[okay] object StackRoom:
  /** the stack pointer, or −1 when unreadable */
  def sp(): Long = -1L
  /** the highest address of this thread's stack, or −1 */
  def top(): Long = -1L
  /** the lowest address a frame may reach — the stack's end plus the VM's guard and shadow zones (the 22+ variant knows them) — or −1 */
  def floor(): Long = -1L
