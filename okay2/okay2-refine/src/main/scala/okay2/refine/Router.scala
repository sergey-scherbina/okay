package okay2.refine

import scala.reflect.ClassTag
import okay2.{!, pure}
import okay2.async.{Async, Scheduler}
import okay2.stream.{Channel, Source}
import okay2.stream.Source.SourceOps

/**
 * ROUTING: one stream of documents in, one stream per kind of document
 * out — okay-refine's `Router` (okay's specs/refine.md, refine-route) on
 * the Scala 2 core. A `Router` is a VALUE — a pattern and a routing
 * table, read top to bottom like a `match`:
 *
 * {{{
 * Router(any)
 *   .route[Swap](swaps)                                         // by type
 *   .route { case c: Cds if c.ccy == "EUR" => c }(eurCds)      // by pattern
 *   .byName("fx")(fx)                                           // by the pattern that took it
 *   .tap(audit)                                                 // a copy of everything taken
 *   .otherwise(rejected)                                        // and nothing is lost
 *   .run(documents)
 * }}}
 *
 * The PATTERN decides what a document is — an `Unclear` one is never
 * routed, it goes to `otherwise` with its verdict. The TABLE decides
 * where a recognised value goes, first rule that fits, like a `match`.
 * A value no rule fits goes to `otherwise` as "no route"; without an
 * `otherwise` the rejects are still COUNTED in `Routed`. When the input
 * ends, `run` closes every channel it was given, once each; when it
 * fails, it fails them with the same error.
 *
 * Scala 2 has no union types, so `route[X]` tests the class of `X` —
 * exact for a class or a case; several kinds into one stream are a
 * pattern, `route { case d @ (_: Swap | _: Cds) => d }`. For a generic
 * `X[T]` the class is all that survives erasure: route those by pattern.
 */
final class Router[A, B] private (
    pattern: Refine[A, B],
    rules: Vector[Router.Rule[B, _]],
    taps: Vector[Channel[B]],
    rejects: Option[Channel[Router.Rejected[A, B]]]) {

  import Router._

  /** every recognised value OF CLASS `X`, under the class's name */
  def route[X <: B](to: Channel[X])(implicit ct: ClassTag[X]): Router[A, B] =
    routeAs[X](ct.runtimeClass.getSimpleName.stripSuffix("$"), to)

  /** `route[X]` under a name of the caller's choosing */
  def routeAs[X <: B](name: String, to: Channel[X])(implicit ct: ClassTag[X]): Router[A, B] =
    add(new Rule[B, X](name, (b, _) => ct.unapply(b), to))

  /** by PATTERN MATCHING: the values the partial function is defined at,
   * sent as it maps them — a guard, several cases, a projection */
  def route[X](pf: PartialFunction[B, X])(to: Channel[X]): Router[A, B] =
    add(new Rule[B, X](s"case #${rules.length + 1}", (b, _) => pf.lift(b), to))

  /** by the NAME of the pattern that took the document — the last step
   * of the verdict's path — for kinds that share a value type */
  def byName(name: String)(to: Channel[B]): Router[A, B] =
    add(new Rule[B, B](name, (b, by) => if (by.steps.lastOption.contains(name)) Some(b) else None, to))

  /** a copy of every recognised value, whichever route it then takes */
  def tap(to: Channel[B]): Router[A, B] = new Router(pattern, rules, taps :+ to, rejects)

  /** where what is not routed goes, with its input, its verdict and why */
  def otherwise(to: Channel[Rejected[A, B]]): Router[A, B] = new Router(pattern, rules, taps, Some(to))

  private def add(r: Rule[B, _]): Router[A, B] = new Router(pattern, rules :+ r, taps, rejects)

  /** where one input would go: the name of its route, or why not */
  def decide(a: A): Either[Rejected[A, B], String] = plan(a).map { case (i, _) => rules(i).name }

  /** the index of the input's rule and the send to its channel, typed inside the rule */
  private def plan(a: A): Either[Rejected[A, B], (Int, Unit ! Async)] =
    pattern.run(a) match {
      case v @ Verdict.Took(b, by, _) =>
        rules.iterator.zipWithIndex.map { case (r, i) => r.fit(b, by).map(i -> _) }.collectFirst { case Some(hit) => hit }
          .toRight(Rejected(a, v, s"no route for $by"))
      case v @ Verdict.Unclear(cs, _) => Left(Rejected(a, v, s"unclear: ${cs.map(_._1).mkString(" | ")}"))
      case v @ Verdict.Declined(_) => Left(Rejected(a, v, "declined by every pattern"))
    }

  /** route the whole source; answers how many went where */
  def run(source: Source[A])(implicit S: Scheduler): Routed ! Async = {
    val counts = Array.fill(rules.length)(0)
    var rejected = 0
    def one(a: A): Unit ! Async = {
      val tapped = pattern.run(a).toOption.fold(pure[Async, Unit](()))(b =>
        taps.foldLeft(pure[Async, Unit](()))((m, t) => m.flatMap(_ => t.send(b).map(_ => ()))))
      tapped.flatMap(_ => plan(a) match {
        case Right((i, send)) =>
          counts(i) += 1
          send
        case Left(r) =>
          rejected += 1
          rejects.fold(pure[Async, Unit](()))(_.send(r).map(_ => ()))
      })
    }
    Async.attempt(source.runForeach(one)).flatMap {
      case Right(()) =>
        channels.foreach(_.close())
        pure[Async, Routed](Routed(rules.map(_.name).zip(counts.toVector), rejected))
      case Left(e) =>
        channels.foreach(_.fail(e))
        Async.await[Routed] { k => k(Left(e)); () => () }
    }
  }

  /** every channel this router writes, once each (a channel may take several routes) */
  private def channels: Vector[Channel[_]] =
    (rules.map(_.channel: Channel[_]) ++ taps ++ rejects.toVector).foldLeft(Vector.empty[Channel[_]])((seen, c) =>
      if (seen.exists(_ eq c)) seen else seen :+ c)
}

object Router {
  def apply[A, B](pattern: Refine[A, B]): Router[A, B] = new Router(pattern, Vector.empty, Vector.empty, None)

  /** what was not routed: the input, the verdict, and why */
  final case class Rejected[+A, +B](input: A, verdict: Verdict[B], why: String)

  /** how many went down each route (in the table's order), and how many were rejected */
  final case class Routed(delivered: Vector[(String, Int)], rejected: Int) {
    def total: Int = delivered.map(_._2).sum + rejected
    /** a SUBTREE's count, lanes being named by path (`Dispatch`):
     * `under("rates")` sums "rates", "rates/swaps", "rates/swaps/eur", … (not "ratesx") */
    def under(prefix: String): Int =
      delivered.collect { case (n, k) if n == prefix || n.startsWith(prefix + "/") => k }.sum
  }

  /** one line of the table: which values it takes, as what `X`, and the
   * channel of `X` they go to — `fit` answers the SEND itself, so the
   * value never leaves the rule untyped */
  final class Rule[B, X](val name: String, take: (B, Path) => Option[X], val channel: Channel[X]) {
    def fit(b: B, by: Path): Option[Unit ! Async] = take(b, by).map(x => channel.send(x).map(_ => ()))
  }
}
