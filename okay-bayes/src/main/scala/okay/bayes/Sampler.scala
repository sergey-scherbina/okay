package okay.bayes

/**
 * THE SAMPLER AS A FACADE (specs/okay-bayes.md stage 4;
 * specs/own-or-standard.md): what draws from a `Target` — a log density
 * and its gradient on ℝᵈ, the one format every sampler can be handed. Ours
 * is the default given; another is an import away (`okay.bayes.PyMC.given`
 * on the JVM), and `Bayes.nuts` / `Smooth.nuts` use whichever is in scope,
 * so the caller's code does not change.
 */
trait Sampler:
  def name: String
  /** `samples` draws after `burn` of warmup, tuned toward the acceptance `delta` */
  def run(target: Target, samples: Int, burn: Int, seed: Long, delta: Double): NutsChain

object Sampler:
  /** ours: `Nuts.sample` */
  object Okay extends Sampler:
    val name = "okay"
    def run(target: Target, samples: Int, burn: Int, seed: Long, delta: Double): NutsChain =
      Nuts.sample(target, samples, burn, seed, delta)

  given okay: Sampler = Okay
