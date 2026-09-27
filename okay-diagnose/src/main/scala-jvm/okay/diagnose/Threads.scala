package okay.diagnose

import scala.jdk.CollectionConverters.*

/** what a thread is doing, as text a failure message can carry */
object Threads:
  /** name, state and the top `frames` of its stack */
  def stateOf(t: Thread, frames: Int = 12): String =
    val stack = if frames > 0 then t.getStackTrace.take(frames).map("\n    at " + _).mkString else ""
    s"${t.getName} ${t.getState}$stack"

  /** every PLATFORM thread `filter` keeps (virtual threads are not listed
   * by the JDK here; hold a reference and use `stateOf`) */
  def dump(filter: Thread => Boolean = _ => true, frames: Int = 12): String =
    Thread.getAllStackTraces.asScala.keys.filter(filter).toVector
      .sortBy(_.getName).map(stateOf(_, frames)).mkString("\n")
