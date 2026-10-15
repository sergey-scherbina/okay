package okay.std

import okay.{!, Effect, Handler, effect}
import okay.core.Split


/**
 * THE CLOCK AS A PORT (specs/audit-ready.md, stage 2).
 *
 * A program that needs the time asks for it; which time it gets is the
 * handler's decision — the system's wall clock in production
 * (`Ambient.clock`, okay-platform), a fixed instant in a test, the
 * recorded reading on a replay. A clock is among the effects whose
 * re-execution IS observable, which is exactly why it must
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

  /** the handler over a source of milliseconds — the only door to a real clock: each reading answered in place (the
   * kernel's `Answering`) */
  def at(source: () => Long): Handler[Clock, [A] =>> A] =
    Handler.answering[Clock]([X] => (e: Clock[X]) => e match
      case Now() => source())

  /** every reading is `millis`: a test's clock */
  def fixed(millis: Long): Handler[Clock, [A] =>> A] = at(() => millis)

  /** readings `first`, `first + increment`, …: a test's clock that moves — the next reading a register of the
   * machine (the kernel's `Stateful`), the answer carrying it */
  def ticking(first: Long, increment: Long): Handler[Clock, [A] =>> (Long, A)] =
    Handler.state[Clock, Long](first)([X] => (s: Long, e: Clock[X]) => e match
      case Now() => (s + increment, s))

  /** answer every `now` with `millis`, wherever Clock is among the program's effects: the rest of them forwarded */
  def run[A, Es[_]](millis: Long)(p: A ! Es)(using s: Split[Es, Clock]): A ! s.Rest = p.handle(fixed(millis))
