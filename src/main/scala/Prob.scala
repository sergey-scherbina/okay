package okay

import scala.annotation.tailrec

/**
 * PROBABILISTIC PROGRAMMING AS AN EFFECT — the Hansei design (Kiselyov
 * & Shan, "Embedded probabilistic programming", DSL 2009): `dist`
 * names a weighted choice, `observe` conditions on evidence, and
 * INFERENCE IS A HANDLER over the ordinary program — the same
 * "several handlers, one program" shape `Choose`'s own doc states for
 * nondeterminism (Choice.scala), with weights instead of a bare set
 * of alternatives.
 *
 * `runExact` is the showcase this earns its keep for: it explores
 * EVERY branch by MULTI-SHOT capture, invoking the delimited
 * continuation once per alternative — exactly what `Effects.handle`
 * already gives `runChoice` for free, and exactly what a ONE-SHOT
 * effect runtime cannot do at all (docs/benchmarks.md's kyo/cats
 * measurements are lanes that could not host this handler, not lanes
 * that would run it slower). `Once` (Fischer, Kiselyov & Shan,
 * "Purely functional lazy non-deterministic programming", ICFP 2009
 * — call-time choice) memoises a shared sub-model for free the same
 * way it memoises anything else: share the VALUE, not the effect.
 */
case class Dist[+A](choices: Seq[(A, Double)]) derives Effect

/**
 * Everything else lives HERE, under the object, matching `Logic`'s
 * own placement (`import Prob.*` at a use site, exactly as `Logic`'s
 * combinators are reached) rather than at package level — `Logic.
 * observe(n: Int)` already names its OWN, unrelated operation (a
 * bounded search), and a package-level `observe(cond: Boolean)`
 * would be shadowed by it the instant a file also writes `import
 * Logic.*`, silently, since an explicit import outranks a same-
 * package member. Namespacing avoids the collision rather than
 * documenting around it.
 */
object Prob:
  /** a weighted choice: pick one of the given (value, weight) pairs.
   * Weights need not sum to 1 — a handler normalizes if it wants to */
  inline def dist[A](choices: (A, Double)*): A ! Dist = effect(Dist(choices))

  /** an unweighted choice, one of Hansei's own idioms: every
   * alternative equally likely */
  inline def uniform[A](as: A*): A ! Dist = dist(as.map(_ -> 1.0)*)

  /** CONDITION: a false observation prunes this branch entirely — a
   * `Dist` with no alternatives contributes ZERO WEIGHT, the same
   * shape `Choose(Seq.empty)` prunes a search (Choice.scala's
   * `empty`). Named as `Prob.observe`, never bare — see the object's
   * own doc comment for why. */
  inline def observe(cond: Boolean): Unit ! Dist =
    if cond then pure(()) else effect(Dist(Seq.empty))

  /** merge two weight maps, adding weight where a value appears in both */
  private def merge[A](a: Map[A, Double], b: Map[A, Double]): Map[A, Double] =
    b.foldLeft(a) { case (m, (k, w)) => m.updated(k, m.getOrElse(k, 0.0) + w) }

  /**
   * EXACT INFERENCE: every branch, weighted, forwarding the effects
   * F. Answers the JOINT weight of every value the program can reach
   * — `Return(a)` alone is weight 1 for `a`; a `Dist` multiplies each
   * branch's own weight into whatever the rest of the program (from
   * that branch onward) answers. `.posterior` normalizes the result
   * to sum to 1 — the Bayesian conditioning `observe` sets up.
   *
   * Multi-shot: `k` is invoked once per alternative in `choices`,
   * each invocation a FRESH continuation — the defining property
   * `Choose`'s own comment names, priced the same way here.
   */
  def runExact[A, F[+_]](p: A ! Dist + F): Map[A, Double] ! F =
    Effects[Free].handle[Dist, F](p)(a => pure[F, Map[A, Double]](Map(a -> 1.0))):
      [X] => (c: Dist[X]) => shift: k =>
        c.choices.foldLeft(pure[F, Map[A, Double]](Map.empty)): (acc, choice) =>
          val (x, w) = choice
          acc.flatMap(m => k(x).map(sub => merge(m, sub.view.mapValues(_ * w).toMap)))

  extension [A](m: Map[A, Double])
    /** the posterior: weights normalized to sum to 1 (empty if every
     * branch was pruned — an observation nothing satisfied) */
    def posterior: Map[A, Double] =
      val total = m.values.sum
      if total == 0.0 then m else m.view.mapValues(_ / total).toMap

  /**
   * ONE SAMPLE, drawn by weight at each `dist` — a single-shot,
   * comonadic walk (no capture, no `Effects.handle`: the SAME bespoke
   * resume-loop shape `State.handle`/`Once.run` use, because a
   * program that may FAIL (an empty `Dist`, from a failed `observe`)
   * has no answer to give the ordinary `Return` case). `None` is a
   * rejected run.
   */
  def sampleOnce[A](p: A ! Dist)(using rng: scala.util.Random): Option[A] =
    def pick[X](choices: Seq[(X, Double)]): Option[X] =
      val total = choices.iterator.map(_._2).sum
      if total <= 0.0 then None
      else
        val r = rng.nextDouble() * total
        var acc = 0.0
        choices.find { case (_, w) => acc += w; acc >= r }.map(_._1)
    @tailrec def loop(x: A ! Dist): Option[A] = (x.resume: @unchecked) match
      case Free.Return(a) => Some(a)
      case Free.Inject(c: Dist[A]) => pick(c.choices)
      case Free.Bind(Free.Inject(c: Dist[x]), k) =>
        pick(c.choices) match
          case None => None
          case Some(v) => loop(k(v))
    loop(p)

  /**
   * REJECTION SAMPLING: draw `n` runs, keep the ones that survive
   * every `observe`, answer their empirical frequencies. The other
   * reading of the SAME program `runExact` reads exhaustively — no
   * capture here at all, which is the point of naming both: exact
   * inference is multi-shot because it must see every branch,
   * rejection sampling is single-shot because it only ever needs one.
   *
   * `p` is BY NAME: one program VALUE, run `n` times — `!.once`'s own
   * warning applies in reverse here (a shared VALUE would memoise
   * the first draw for every one of the n samples), so each call
   * re-evaluates `p` from its own definition.
   */
  def runRejection[A](n: Int)(p: => A ! Dist)(using rng: scala.util.Random): Map[A, Double] =
    val kept = (1 to n).flatMap(_ => sampleOnce(p))
    if kept.isEmpty then Map.empty
    else kept.groupBy(identity).view.mapValues(_.size.toDouble / kept.size).toMap
