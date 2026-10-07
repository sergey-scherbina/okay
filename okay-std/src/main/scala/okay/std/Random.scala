package okay.std

import okay.freer.*
import okay.freer.given

import okay.{Effect, TypeableK}


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

  /** the handler over a source of 64-bit draws — the only door to a real one */
  def at(source: () => Long): Handler[Random, [A] =>> A] =
    Handler.answerOf[Random]([X] => (e: Random[X]) => e match
      case NextLong() => source())

  private final val Golden = 0x9E3779B97F4A7C15L
  /** SplitMix64's output function over a seed already stepped */
  def mix(z0: Long): Long =
    var z = z0
    z = (z ^ (z >>> 30)) * 0xBF58476D1CE4E5B9L
    z = (z ^ (z >>> 27)) * 0x94D049BB133111EBL
    z ^ (z >>> 31)

  /** repeatable draws from `seed` (SplitMix64); the answer carries the next seed */
  def seeded(seed: Long): Handler[Random, [A] =>> (Long, A)] = new Handler.Stepped[Random, Long, [A] =>> (Long, A)]:
    def run[A, F[+_]](p: A ! Random + F)(using A <:< Any, Distinct[Random + F], Handler.Nothing[F]): (Long, A) ! F =
      HandleFrames.stateRun[Random, Long, A, (Long, A), F](summon[TypeableK[Random]], (s, a) => pure((s, a)))(
        (s, _) => { val s2 = s + Golden; (s2, mix(s2)) })(seed, p)
    def takes: TypeableK[Random] = summon[TypeableK[Random]]
    def init: Long = seed
    def step(s: Long, op: Any): (Long, Any) | Handler.Halt[Long] = { val s2 = s + Golden; (s2, mix(s2)) }
    def ret[A, F[+_]](s: Long, a: A): (Long, A) ! F = pure((s, a))

  /** repeatable draws from `seed`, forwarding the effects F */
  def run[A, F[+_]](seed: Long)(p: A ! Random + F)(using Distinct[Random + F]): A ! F = seeded(seed).run(p).map(_._2)
