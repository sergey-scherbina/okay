package okay2

import scala.annotation.tailrec
import scala.util.Random
import Free.{Return, Inject, Bind}

/**
 * PROBABILISTIC PROGRAMMING AS AN EFFECT — the Hansei design (Kiselyov
 * & Shan, "Embedded probabilistic programming", DSL 2009): `dist` names
 * a weighted choice, `observe` conditions on evidence, and INFERENCE IS
 * A HANDLER over the ordinary program — `Choose`'s shape, with weights.
 *
 * `runExact` explores EVERY branch by MULTI-SHOT capture, invoking the
 * delimited continuation once per alternative — what a one-shot effect
 * runtime cannot do at all. `runRejection` reads the SAME program by
 * sampling, single-shot. `Once` memoises a shared sub-model the way it
 * memoises anything: share the VALUE, not the effect.
 */
sealed trait Dist extends Row { type Op[+A] = Dist.Op[A] }

object Dist {
  /** a weighted choice; no alternatives is zero weight — a pruned branch */
  final case class Op[+A](choices: Seq[(A, Double)])

  implicit val effect: Effect[Dist] = Effect.of[Dist]
}

/** Everything else under an object, as in the Scala 3 core: `Logic.observe`
 * already names an unrelated operation, and a package-level `observe`
 * would be shadowed by it under `import Logic._` */
object Prob {
  /** a weighted choice: pick one of the given (value, weight) pairs;
   * weights need not sum to 1 */
  def dist[A](choices: (A, Double)*): A ! Dist = Free.inject[Dist, A](Dist.Op(choices))

  /** every alternative equally likely */
  def uniform[A](as: A*): A ! Dist = dist(as.map(_ -> 1.0): _*)

  /** CONDITION: a false observation prunes this branch entirely */
  def observe(cond: Boolean): Unit ! Dist =
    if (cond) pure[Dist, Unit](()) else Free.inject[Dist, Unit](Dist.Op(Seq.empty))

  private def merge[A](a: Map[A, Double], b: Map[A, Double]): Map[A, Double] =
    b.foldLeft(a) { case (m, (k, w)) => m.updated(k, m.getOrElse(k, 0.0) + w) }

  /**
   * EXACT INFERENCE: every branch, weighted, the rest of the row
   * forwarded. Answers the JOINT weight of every value the program can
   * reach; `.posterior` normalizes it. Multi-shot: `k` is invoked once
   * per alternative, each a fresh continuation.
   */
  def runExact[A, R <: Row](p: Free[Dist with R, A]): Map[A, Double] ! R =
    // `Dist` takes no type parameter, so the row cannot hold a second
    // signature of its class: this split needs no Distinct check
    Effects.handle[Dist, R](p)(a => pure[R, Map[A, Double]](Map(a -> 1.0)))(
      new Interpr[Dist, Map[A, Double] ! R] {
        def apply[X](c: Dist.Op[X]): Cont[X, Map[A, Double] ! R, Map[A, Double] ! R] =
          shift[X, Map[A, Double] ! R, Map[A, Double] ! R] { k =>
            c.choices.foldLeft(pure[R, Map[A, Double]](Map.empty)) { case (acc, (x, w)) =>
              acc.flatMap(m => k(x).map(sub => merge(m, sub.map { case (a, v) => a -> v * w })))
            }
          }
      })(implicitly, Distinct.unchecked)

  implicit final class PosteriorOps[A](private val m: Map[A, Double]) extends AnyVal {
    /** the posterior: weights normalized to sum to 1 (empty if every
     * branch was pruned) */
    def posterior: Map[A, Double] = {
      val total = m.values.sum
      if (total == 0.0) m else m.map { case (a, w) => a -> w / total }
    }
  }

  /**
   * ONE SAMPLE, drawn by weight at each `dist` — a single-shot walk;
   * `None` is a run rejected by an `observe`.
   */
  def sampleOnce[A](p: Free[Dist, A])(implicit rng: Random): Option[A] = {
    def pick(choices: Seq[(Any, Double)]): Option[Any] = {
      val total = choices.iterator.map(_._2).sum
      if (total <= 0.0) None
      else {
        val r = rng.nextDouble() * total
        var acc = 0.0
        choices.find { case (_, w) => acc += w; acc >= r }.map(_._1)
      }
    }
    @tailrec def loop(x: Free[Dist, A]): Option[A] = Free.resume(x) match {
      case Return(a) => Some(a)
      case Inject(e) => loop(Bind(Inject[Dist, A](e), (v: A) => Return[Dist, A](v)))
      case Bind(Inject(e), k) =>
        pick(Split.only[Dist, Any](e).choices) match {
          case None => None
          case Some(v) => loop(k(v))
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    loop(p)
  }

  /**
   * REJECTION SAMPLING: `n` runs, the ones that survive every `observe`
   * kept, their empirical frequencies answered. `p` is BY NAME: one
   * program re-evaluated per sample.
   */
  def runRejection[A](n: Int)(p: => Free[Dist, A])(implicit rng: Random): Map[A, Double] = {
    val kept = (1 to n).flatMap(_ => sampleOnce(p))
    if (kept.isEmpty) Map.empty
    else kept.groupBy(identity).map { case (a, xs) => a -> xs.size.toDouble / kept.size }
  }
}
