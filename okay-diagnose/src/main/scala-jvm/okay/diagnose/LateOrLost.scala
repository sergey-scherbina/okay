package okay.diagnose

import java.time.Duration

/**
 * SLOW UNDER LOAD, OR NEVER (specs/okay-diagnose.md; first written into
 * TestChannelLaws for sentinel-single-consumer-lost-end). A liveness law
 * that joins a thread with one deadline cannot tell a thread starved in a
 * loaded JVM from one parked for good, and the first turns every busy gate
 * red. Join with `first`. If that misses, take the snapshot there (the
 * thread's state tells parked from starved), then give it `grace` more.
 */
object LateOrLost:
  enum Outcome:
    case OnTime
    case Late(at: String)
    case Lost(at: String)

  def join(t: Thread, first: Duration, grace: Duration)(snapshot: => String): Outcome =
    // join(millis) and isAlive, not join(Duration): that one is JDK 19,
    // and the module's floor is 17
    t.join(first.toMillis max 1L)
    if !t.isAlive then Outcome.OnTime
    else
      val at = s"at ${first.toMillis} ms: ${Threads.stateOf(t)}\n  $snapshot"
      t.join(grace.toMillis max 1L)
      if !t.isAlive then Outcome.Late(at) else Outcome.Lost(at)
