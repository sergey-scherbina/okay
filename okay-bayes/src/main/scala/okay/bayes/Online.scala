package okay.bayes

import scala.util.Random
import okay.Stage
import okay.freer.{!}
/**
 * AN ONLINE PARTICLE FILTER (specs/okay-bayes.md stage 6; the bootstrap
 * filter of Gordon, Salmond & Smith 1993): particles of a hidden state S,
 * advanced one observation at a time by `step(s, o)` — a model that draws
 * the next state from its prior and weighs it by how well it explains `o`
 * — reweighed, resampled (systematic) when the effective sample size falls
 * under half, the log evidence accumulated. A filter is a VALUE: `push`
 * answers the next one, and `stage` runs it over a stream.
 */
final class Filter[S, O] private[bayes] (step: (S, O) => S ! Model, val states: Vector[S], logWeights: Vector[Double],
  val logEvidence: Double, val seen: Int, seed: Long):

  /** the posterior over the state after the observations so far */
  def particles: Particles[S] =
    val top = logWeights.max
    val w = logWeights.map(l => math.exp(l - top))
    val total = w.sum
    Particles(states, Vector.fill(states.length)(Map.empty), w.map(_ / total), logEvidence + top + math.log(total / states.length), 0)

  /** the next filter, after `o` */
  def push(o: O): Filter[S, O] =
    val rng = new Random(seed + 7919L * (seen + 1))
    val n = states.length
    val runs = states.map(s => Bayes.prior(step(s, o), rng))
    val lw = logWeights.zip(runs).map((l, r) => l + r.logLik)
    val top = lw.max
    require(top > Distribution.NegInf, s"Filter: every particle has weight zero after observation ${seen + 1}")
    val ws = lw.map(l => math.exp(l - top))
    val sum = ws.sum
    val next = runs.map(_.value)
    if 1 / ws.map(w => (w / sum) * (w / sum)).sum < n / 2.0 then
      // resampled: the evidence so far is banked, and every particle starts again at equal weight
      val logZ = logEvidence + top + math.log(sum / n)
      val cum = ws.map(_ / sum).scanLeft(0.0)(_ + _).tail
      val u = rng.nextDouble() / n
      var j = 0
      val idx = Vector.tabulate(n) { i =>
        while j < n - 1 && cum(j) < u + i.toDouble / n do j += 1
        j
      }
      Filter(step, idx.map(next), Vector.fill(n)(0.0), logZ, seen + 1, seed)
    else Filter(step, next, lw, logEvidence, seen + 1, seed)

  /** the filter over a stream: each observation in, the posterior after it out; the last filter is the stage's answer */
  def stage: Stage[O, Particles[S], Filter[S, O]] =
    Stage.mapAccumulate(this)((f, o) => { val g = f.push(o); (g, g.particles) })

object Online:
  /** `particles` states drawn from `start`, to be advanced by `step` */
  def filter[S, O](start: S ! Model, step: (S, O) => S ! Model, particles: Int, seed: Long = 42L): Filter[S, O] =
    require(particles > 0, "Online.filter: at least one particle")
    val rng = new Random(seed)
    Filter(step, Vector.fill(particles)(Bayes.prior(start, rng).value), Vector.fill(particles)(0.0), 0.0, 0, seed)
