package okay

import okay.!.*

/**
 * THE CLOCK AS A PORT (specs/audit-ready.md, stage 2).
 *
 * A program that needs the time asks for it; which time it gets is the
 * handler's decision — the system's wall clock in production
 * (`Ambient.clock`, okay-platform), a fixed instant in a test, the
 * recorded reading on a replay. `Replayable` lists the clock among the
 * effects whose re-execution IS observable, which is exactly why it must
 * be a port: the handler that answers it is what a journal records.
 *
 * The core itself reads no clock: `System.currentTimeMillis` lives in
 * okay-platform, a `runtime` module of okay-audit, and nowhere a
 * `business` module can reach.
 */
enum Clock[+A] derives Effect:
  /** milliseconds since the epoch */
  case Now() extends Clock[Long]

object Clock:
  /** the time, in milliseconds since the epoch */
  inline def now: Long ! Clock = effect(Now())

  /** the handler over a source of milliseconds — the only door to a real clock */
  def at(source: () => Long): Handler[Clock, [A] =>> A] =
    Handler.answerOf[Clock]([X] => (e: Clock[X]) => e match
      case Now() => source())

  /** every reading is `millis`: a test's clock */
  def fixed(millis: Long): Handler[Clock, [A] =>> A] = at(() => millis)

  /** readings `first`, `first + increment`, …: a test's clock that moves */
  def ticking(first: Long, increment: Long): Handler[Clock, [A] =>> (Long, A)] = new Handler.Stepped[Clock, Long, [A] =>> (Long, A)]:
    def run[A, F[+_]](p: A ! Clock + F)(using A <:< Any, Distinct[Clock + F], Handler.Nothing[F]): (Long, A) ! F =
      HandleFrames.stateRun[Clock, Long, A, (Long, A), F](summon[TypeableK[Clock]], (s, a) => pure((s, a)))(
        (s, _) => (s + increment, s))(first, p)
    def takes: TypeableK[Clock] = summon[TypeableK[Clock]]
    def init: Long = first
    def step(s: Long, op: Any): (Long, Any) | Handler.Halt[Long] = (s + increment, s)
    def ret[A, F[+_]](s: Long, a: A): (Long, A) ! F = pure((s, a))

  /** answer every `now` with `millis`, forwarding the effects F */
  def run[A, F[+_]](millis: Long)(p: A ! Clock + F)(using Distinct[Clock + F]): A ! F = fixed(millis).run(p)
