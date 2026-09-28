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
    case Starved(at: String)

  def join(t: Thread, first: Duration, grace: Duration)(snapshot: => String): Outcome =
    // join(millis) and isAlive, not join(Duration): that one is JDK 19,
    // and the module's floor is 17
    t.join(first.toMillis max 1L)
    if !t.isAlive then Outcome.OnTime
    else
      val at = s"at ${first.toMillis} ms: ${Threads.stateOf(t)}\n  $snapshot"
      t.join(grace.toMillis max 1L)
      if !t.isAlive then Outcome.Late(at)
      else
        // STARVED IS NOT LOST (sentinel-single-consumer-lost-end,
        // 2026-09-28). A liveness law asks whether a wakeup was lost, and
        // a thread that is RUNNABLE at the last deadline has not missed
        // one: it has work and no carrier — a whole-build JVM sharing
        // its cores with every other suite's virtual threads. The
        // second snapshot names the state the verdict rests on; the
        // caller logs a Starved and fails a Lost.
        val last = Threads.stateOf(t)
        val both = s"$at\n  at ${(first.toMillis + grace.toMillis)} ms: $last"
        if t.getState == Thread.State.RUNNABLE then Outcome.Starved(both) else Outcome.Lost(both)
