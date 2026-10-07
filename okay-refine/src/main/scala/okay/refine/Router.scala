package okay.refine

import scala.reflect.TypeTest
import okay.{Async, Channel, Scheduler, Source, runForeach}
import okay.freer.{!, effect, pure}
/**
 * ROUTING: one stream of documents in, one stream per kind of document
 * out (specs/refine.md, refine-route). A `Router` is a VALUE — a
 * pattern and a routing table — read top to bottom like a `match`:
 *
 * {{{
 * Router(Fin.any)
 *   .route[Swap](swaps)                                  // by type
 *   .route { case c: Cds if c.notional.currency == "EUR" => c }(eurCds)   // by pattern
 *   .byName("fxForward")(fx)                             // by the pattern that took it
 *   .tap(audit)                                          // a copy of everything taken
 *   .otherwise(rejected)                                 // and nothing is lost
 *   .run(documents)
 * }}}
 *
 * The recognition and the routing are two decisions, kept apart. The
 * PATTERN decides what a document is, and says `Unclear` when two
 * readings hold — such a document is never routed, it goes to
 * `otherwise` with its verdict. The TABLE decides where a recognised
 * value goes, and like a `match` the first rule that fits wins: the
 * table is the author's, written in order, not a set of readings of
 * the document. A value no rule fits goes to `otherwise` too, as "no
 * route". Nothing is dropped silently: without an `otherwise` the
 * rejects are still COUNTED in the `Routed` answer.
 *
 * `run` sends each value to its channel (a bounded channel slows the
 * router, and with it every route: the capacity the caller chose is the
 * backpressure policy) and, when the input ends, CLOSES every channel
 * it was given, once each, so every consumer's `drained` ends; when the
 * input fails, it FAILS them with the same error, so no consumer waits
 * for a stream that will not come.
 */
final class Router[A, B] private (
    pattern: Refine[A, B],
    rules: Vector[Router.Rule[B, ?]],
    taps: Vector[Channel[B]],
    rejects: Option[Channel[Router.Rejected[A, B]]]):

  import Router.*

  /** every recognised value OF TYPE `X` — a class, a case, or a UNION of
   * them (`route[Swap | Cds]`): the test is the compiler's own
   * `TypeTest`, exact for a union, where a `ClassTag` would be the
   * union's least upper bound and take every sibling too */
  inline def route[X <: B](to: Channel[X])(using tt: TypeTest[B, X]): Router[A, B] =
    routeAs[X](TypeName.of[X], to)

  /** `route[X]` under a name of the caller's choosing (the name is what
   * `Routed` counts it under) */
  def routeAs[X <: B](name: String, to: Channel[X])(using tt: TypeTest[B, X]): Router[A, B] =
    add(Rule[B, X](name, (b, _) => tt.unapply(b), to))

  /** by PATTERN MATCHING: the values the partial function is defined at,
   * sent as it maps them — a guard, a union of cases, a projection */
  def route[X](pf: PartialFunction[B, X])(to: Channel[X]): Router[A, B] =
    add(Rule[B, X](s"case #${rules.length + 1}", (b, _) => pf.lift(b), to))

  /** by the NAME of the pattern that took the document — the last step
   * of the verdict's path (`…/trade/fxForward`) — for kinds that share a
   * value type */
  def byName(name: String)(to: Channel[B]): Router[A, B] =
    add(Rule[B, B](name, (b, by) => Option.when(by.steps.lastOption.contains(name))(b), to))

  /** a copy of every recognised value, whichever route it then takes */
  def tap(to: Channel[B]): Router[A, B] = Router(pattern, rules, taps :+ to, rejects)

  /** where what is not routed goes, with its input, its verdict and why */
  def otherwise(to: Channel[Rejected[A, B]]): Router[A, B] = Router(pattern, rules, taps, Some(to))

  private def add(r: Rule[B, ?]): Router[A, B] = Router(pattern, rules :+ r, taps, rejects)

  /** where one input would go: the name of its route, or why not */
  def decide(a: A): Either[Rejected[A, B], String] = plan(a).map((i, _) => rules(i).name)

  /** the index of the input's rule and the send to its channel, typed inside the rule */
  private def plan(a: A): Either[Rejected[A, B], (Int, Unit ! Async)] =
    pattern.run(a) match
      case v @ Verdict.Took(b, by, _) =>
        rules.iterator.zipWithIndex.map((r, i) => r.fit(b, by).map(i -> _)).collectFirst { case Some(hit) => hit }
          .toRight(Rejected(a, v, s"no route for ${by}"))
      case v @ Verdict.Unclear(cs, _) => Left(Rejected(a, v, s"unclear: ${cs.map(_._1).mkString(" | ")}"))
      case v @ Verdict.Declined(_) => Left(Rejected(a, v, "declined by every pattern"))

  /** route the whole source; answers how many went where */
  def run(source: Source[A])(using Scheduler): Routed ! Async =
    val counts = Array.fill(rules.length)(0)
    var rejected = 0
    def one(a: A): Unit ! Async =
      val taken = pattern.run(a).toOption
      val tapped = taken.fold(pure[Async, Unit](()))(b => taps.foldLeft(pure[Async, Unit](()))((m, t) => m.flatMap(_ => t.send(b).map(_ => ()))))
      tapped.flatMap(_ => plan(a) match
        case Right((i, send)) =>
          counts(i) += 1
          send
        case Left(r) =>
          rejected += 1
          rejects.fold(pure[Async, Unit](()))(_.send(r).map(_ => ())))
    Async.attempt(source.runForeach(one)).flatMap {
      case Right(()) =>
        channels.foreach(_.close())
        pure(Routed(rules.map(_.name).zip(counts.toVector), rejected))
      case Left(e) =>
        channels.foreach(_.fail(e))
        effect(Async.Await[Routed](k => { k(Left(e)); () => () }))
    }

  /** every channel this router writes, once each (a channel may take several routes) */
  private def channels: Vector[Channel[?]] =
    (rules.map(_.channel) ++ taps ++ rejects.toVector).foldLeft(Vector.empty[Channel[?]])((seen, c) =>
      if seen.exists(_ eq c) then seen else seen :+ c)

object Router:
  def apply[A, B](pattern: Refine[A, B]): Router[A, B] = new Router(pattern, Vector.empty, Vector.empty, None)

  private def apply[A, B](p: Refine[A, B], rs: Vector[Rule[B, ?]], ts: Vector[Channel[B]], rj: Option[Channel[Rejected[A, B]]]): Router[A, B] =
    new Router(p, rs, ts, rj)

  /** what was not routed: the input, the verdict, and why */
  final case class Rejected[+A, +B](input: A, verdict: Verdict[B], why: String)

  /** how many went down each route (in the table's order), and how many were rejected */
  final case class Routed(delivered: Vector[(String, Int)], rejected: Int):
    def total: Int = delivered.map(_._2).sum + rejected
    /** a SUBTREE's count, lanes being named by path: `under("rates")` sums
     * "rates", "rates/swaps", "rates/swaps/eur", … (not "ratesx") */
    def under(prefix: String): Int =
      delivered.collect { case (n, k) if n == prefix || n.startsWith(prefix + "/") => k }.sum

  /** one line of the table: which values it takes (`fit`), as what `X`,
   * and the channel of `X` they go to — the value never leaves the rule
   * untyped: `fit` answers the SEND itself */
  final class Rule[B, X](val name: String, take: (B, Path) => Option[X], val channel: Channel[X]):
    def fit(b: B, by: Path): Option[Unit ! Async] = take(b, by).map(x => channel.send(x).map(_ => ()))
