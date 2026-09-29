package okay.diagnose

/**
 * ONE TEST'S DIAGNOSIS: a flight recorder and the snapshots to take if it
 * fails (specs/okay-diagnose.md). It is independent of any framework.
 *
 *     val d = Diagnostics()
 *     Diagnostics.around(d) {
 *       d.note(s"round \$r")
 *       d.onFailure(channel.debugState)
 *       ...
 *     }
 *
 * A snapshot is by-name and is taken only if the body throws. A passing
 * body pays for its notes and nothing else. A framework adapter
 * (okay-test's `Munit.Diagnosed`) makes one per test and wraps every test body in
 * `around`, so a suite only calls `note` and `onFailure`.
 */
final class Diagnostics(capacity: Int = 256):
  private val flight = Flight(capacity)
  private val snapshots = java.util.concurrent.ConcurrentLinkedQueue[() => String]()

  def note(msg: => String): Unit = flight.note(msg)

  def onFailure(snapshot: => String): Unit =
    val _ = snapshots.add(() => snapshot)

  def recorded: String = flight.dump

  /** everything this test gathered, as the text a failure carries; "" when
   * nothing was noted and nothing registered */
  def report: String =
    val parts = scala.collection.mutable.ArrayBuffer.empty[String]
    if !flight.isEmpty then parts += s"flight recorder:\n${flight.dump}"
    snapshots.forEach { s =>
      val taken = try s() catch case t: Throwable => s"(the snapshot threw: $t)"
      parts += s"snapshot:\n  ${taken.replace("\n", "\n  ")}"
    }
    if parts.isEmpty then "" else parts.mkString("--- diagnosis ---\n", "\n", "")

object Diagnostics:
  /** run the body; a throw leaves with the diagnosis added (`FailureFormat`) */
  def around[A](d: Diagnostics)(body: => A)(using f: FailureFormat): A =
    try body
    catch case e: Throwable => throw f.extend(e, d.report)
