package okay


/**
 * THE PRIMITIVES OF A WAIT — what a consumer can do between two looks
 * before it blocks, each a rung: spin, yield the core, sleep a few
 * microseconds, and at last block (register and park — the one rung
 * the producer pays for, since somebody must wake a sleeper). The
 * platform implements them by default (`PlatformPause`: JVM and Native
 * with threads, JS without); a debugger or a test substitutes its own
 * — counting, recording, or refusing to sleep — with one `given`.
 * `threads` is the platform's fact a strategy needs: whether a poll
 * can find what the last one missed at all.
 */
trait Pause:
  /** producers run on threads of their own: waiting can be answered */
  def threads: Boolean
  def spin(): Unit
  def yieldNow(): Unit
  def nano(): Unit
  /** the wait gives up; the caller is about to register and block */
  def block(): Unit

object Pause:
  given Pause = PlatformPause

/**
 * HOW A CONSUMER WAITS for a condition before it blocks
 * (specs/ready-merge.md, poll-then-park and its hybrid) — a closed loop
 * over `Pause`'s rungs: `until` polls `ready` on the strategy's own
 * schedule and answers whether the condition came; false means it gave
 * up, and the caller registers and parks. Every rung but the last costs
 * the consumer alone. This is LMAX Disruptor's WaitStrategy (BusySpin /
 * Yielding / Sleeping / Blocking) as a value the caller chooses, under
 * Karlin, Manasse, McGeoch and Owicki's bound: spin about as long as
 * a block costs before blocking, and no fixed choice does better than
 * twice the optimum. Not the merge's alone: any consumer with a poll.
 */
trait Wait:
  def until(ready: () => Boolean)(using p: Pause): Boolean

object Wait:
  /** never wait: give up at once — JS's shape, and the road before
   * the hybrid */
  object Register extends Wait:
    def until(ready: () => Boolean)(using p: Pause): Boolean =
      p.block(); false

  /** poll `polls` times, spinning between looks */
  final case class Spin(polls: Int) extends Wait:
    def until(ready: () => Boolean)(using p: Pause): Boolean =
      if !p.threads then { p.block(); false }
      else
        var i = 0
        var got = false
        while !got && i < polls do { got = ready(); if !got then p.spin(); i += 1 }
        if !got then p.block()
        got

  /** the straight ladder: `spins` polls, then `yields` polls each after
   * a yield, then `sleeps` polls each after a nano-sleep, then give up.
   * MEASURED the shape that took the chunked ring road's tail away
   * (ready-merge-chunk-forward): 100/50/4 read 200.4 us against the
   * shared channel's 200.0 with no fork above 225 us */
  final case class Ladder(spins: Int, yields: Int, sleeps: Int) extends Wait:
    def until(ready: () => Boolean)(using p: Pause): Boolean =
      if !p.threads then { p.block(); false }
      else
        var got = false
        var i = 0
        while !got && i < spins do { got = ready(); if !got then p.spin(); i += 1 }
        i = 0
        while !got && i < yields do { p.yieldNow(); got = ready(); i += 1 }
        i = 0
        while !got && i < sleeps do { p.nano(); got = ready(); i += 1 }
        if !got then p.block()
        got

  /** the cycle: (`spins` polls, `yields` yield-polls, one nano-sleep)
   * `cycles` times, then give up — after a sleep the data has most
   * likely arrived, so spin for it again. MEASURED 2026-09-28 beside
   * the ladder on the same road: 100/50/4 read 208.0 us with 3 of 10
   * forks at 220-247, where the ladder read 200.4 with none — the
   * re-spin brings the consumer back to the producer's cache line too
   * soon. Kept as a choice, not the default */
  final case class Cycle(spins: Int, yields: Int, cycles: Int) extends Wait:
    def until(ready: () => Boolean)(using p: Pause): Boolean =
      if !p.threads then { p.block(); false }
      else
        var got = false
        var c = 0
        while !got && c < cycles do
          var i = 0
          while !got && i < spins do { got = ready(); if !got then p.spin(); i += 1 }
          i = 0
          while !got && i < yields do { p.yieldNow(); got = ready(); i += 1 }
          if !got then p.nano()
          c += 1
        if !got then got = ready()
        if !got then p.block()
        got

  given Wait = Ladder(100, 50, 4)
