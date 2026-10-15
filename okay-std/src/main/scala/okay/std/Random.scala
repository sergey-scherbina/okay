package okay.std

import okay.{!, Effect, Handler, effect}
import okay.core.Split


/**
 * RANDOMNESS AS A PORT (specs/audit-ready.md, stage 2) — the clock's
 * twin. One operation, a 64-bit draw; everything else is arithmetic on
 * it, so a journal records one number per draw and a replay is exact.
 *
 * Handlers: `Ambient.random` (okay-platform) over `scala.util.Random`;
 * `seeded` is SplitMix64 threaded as a seed, so a test is repeatable and
 * a multi-shot handler (Choose) continues each branch from the seed it
 * was captured with, as `Supply` does. The core itself draws nothing.
 *
 * Not `scala.util.Random`'s name by accident: in package `okay` this is
 * THE random source; an explicit `import scala.util.Random` still wins
 * where a file wants the JDK's.
 */
enum Random[+A] derives Effect:
  /** 64 uniformly random bits */
  case NextLong() extends Random[Long]

object Random:
  /** 64 random bits */
  inline def nextLong: Long ! Random = effect(NextLong())

  /** a uniform `[0, 1)` */
  def nextDouble: Double ! Random = nextLong.map(l => (l >>> 11) * (1.0 / (1L << 53)))

  /** a uniform `[0, bound)`, `bound > 0` */
  def nextInt(bound: Int): Int ! Random =
    require(bound > 0, "bound must be positive")
    nextDouble.map(d => (d * bound).toInt)

  /** the handler over a source of 64-bit draws — the only door to a real one: each draw answered in place (the
   * kernel's `Answering`) */
  def at(source: () => Long): Handler[Random, [A] =>> A] =
    Handler.answering[Random]([X] => (e: Random[X]) => e match
      case NextLong() => source())

  private final val Golden = 0x9E3779B97F4A7C15L
  /** SplitMix64's output function over a seed already stepped */
  def mix(z0: Long): Long =
    var z = z0
    z = (z ^ (z >>> 30)) * 0xBF58476D1CE4E5B9L
    z = (z ^ (z >>> 27)) * 0x94D049BB133111EBL
    z ^ (z >>> 31)

  /** repeatable draws from `seed` (SplitMix64); the answer carries the next seed — a register of the machine (the
   * kernel's `Stateful`), so a `k` taken past it continues from the seed it was captured with */
  def seeded(seed: Long): Handler[Random, [A] =>> (Long, A)] =
    Handler.state[Random, Long](seed)([X] => (s: Long, e: Random[X]) => e match
      case NextLong() => { val s2 = s + Golden; (s2, mix(s2)) })

  /** repeatable draws from `seed`, wherever Random is among the program's effects: the rest of them forwarded */
  def run[A, Es[_]](seed: Long)(p: A ! Es)(using s: Split[Es, Random]): A ! s.Rest =
    p.handle(seeded(seed)).map(_._2)
