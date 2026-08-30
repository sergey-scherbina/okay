package okay.agent

import okay.{!, +, Choose, Logic, TypeableK, effect, guard, pure, runChoice}
import okay.given

/**
 * Search over what the model says (specs/llm-agentic.md). Sampling
 * IS nondeterminism, so the strategies every agent framework
 * hand-rolls are one-liners over `Choose` and `Logic` here:
 *
 * - best-of-N   = choose among N completions, `once` to commit;
 * - retry-until = `ifte` — the SOFT cut: use every answer that
 *   validates, and re-prompt ONLY when none did (a plain flatMap
 *   cannot say "no answer"; a hard cut would lose the good ones);
 * - tree search = `msplit` with `guard` to prune, `observe(n)` to
 *   take n leaves, `interleave` to split the budget fairly.
 *
 * Handler ORDER decides whether branches share a conversation:
 * put `Memory.handle` INSIDE the search and every branch explores
 * its own context; put it outside and the transcript records
 * everything tried. Neither needs a flag — it is where you run it.
 */
object Search {

  /** the row of a searching agent: nondeterminism beside the rest */
  type Searching[F[+_]] = Choose + F

  /** N samples of the same program — the multi-shot handler runs
   * each branch, so the model is asked N times */
  def samples[A, F[+_]](n: Int)(gen: A ! (Choose + F)): A ! (Choose + F) =
    effect[Choose + F, Int](Choose(1 to n)).flatMap(_ => gen)

  /** the first sample that passes the check; none pass = no answer */
  def bestOf[A, F[+_] : TypeableK](n: Int)(gen: A ! (Choose + F))
                                  (ok: A => Boolean): A ! (Choose + F) =
    Logic.once(samples(n)(gen).flatMap(a =>
      guard[[X] =>> X ! (Choose + F)](ok(a)).map(_ => a)))

  /** every sample that passes, in order (the fold-friendly form:
   * majority vote and confidence are Aggregators over this) */
  def all[A, F[+_] : TypeableK](n: Int)(gen: A ! (Choose + F))
                               (ok: A => Boolean): Seq[A] ! F =
    runChoice[A, F](samples(n)(gen).flatMap(a =>
      guard[[X] =>> X ! (Choose + F)](ok(a)).map(_ => a)))

  /**
   * The soft cut as a prompt loop: try `attempt`; every answer that
   * validates is used, and `fallback` runs ONLY when none did. This
   * is the shape of "parse the model's JSON, and re-prompt if it is
   * unusable" — and `ifte` is the only combinator that expresses it
   * without either losing the good answers or swallowing the failure.
   */
  def validated[A, B, F[+_] : TypeableK](attempt: A ! (Choose + F))
                                        (use: A => B ! (Choose + F))
                                        (fallback: => B ! (Choose + F))
  : B ! (Choose + F) =
    Logic.ifte(attempt)(use)(fallback)

  /** majority vote over the samples: an Aggregator, of course */
  def majority[A](answers: Seq[A]): Option[A] =
    answers.groupBy(identity).maxByOption(_._2.size).map(_._1)
}
