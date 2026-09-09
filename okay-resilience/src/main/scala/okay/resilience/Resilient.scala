package okay.resilience

import okay.*
import okay.http.{Http, Method, Request, Response}

/**
 * The five pieces around `trait Http`, in ONE order, and a refusal
 * turned into a status on the server side (specs/resilience.md,
 * stage 1).
 *
 * The order is the decision: deadline outermost (the budget bounds
 * the waits under it), then breaker (an open circuit spends no permit
 * and no token), then bulkhead, then limiter (a refused permit burns
 * no token), hedge, and balancing innermost (each hedged attempt
 * picks its own endpoint; the breaker counts per SERVICE).
 */
object Resilient:

  private def wall(): Long = System.currentTimeMillis

  /** the limiter key a server usually wants: who sent it, as the
    * transport knows it (`Request.peer`, specs/http.md); unknown
    * senders share one bucket rather than each getting a fresh one */
  val byPeer: Request => String = _.peer.getOrElse("")

  /** the methods safe to repeat, so safe to hedge */
  val safeMethods: Request => Boolean =
    r => r.method == Method.Get || r.method == Method.Head || r.method == Method.Options

  /**
   * A client. `budgetMillis` is the most one call may take; a
   * deadline the request ALREADY carries (propagated from an inbound
   * one with `Deadline.carry`) is honoured too, and the earlier of
   * the two is what the call gets and what the outgoing header says.
   * A 5xx counts as a breaker failure, a 4xx does not — the far end
   * answered. Hedging applies to `hedgeable` requests only.
   */
  def http(inner: Http,
           budgetMillis: Option[Long] = None,
           breaker: Option[Breaker] = None,
           bulkhead: Option[Bulkhead] = None,
           limiter: Option[(Limiter, Request => String)] = None,
           hedge: Option[(Long, Int)] = None,
           hedgeable: Request => Boolean = safeMethods,
           balanced: Option[Balanced] = None,
           clock: () => Long = wall)
          (using Scheduler, Timer): Http = new Http:
    def send(r: Request): Response ! Async =
      val now = clock()
      val carried = Deadline.read(r, clock)
      val budget = budgetMillis.map(ms => Deadline(now + ms))
      val deadline = (carried, budget) match
        case (Some(a), Some(b)) => Some(Deadline(math.min(a.atMillis, b.atMillis)))
        case (a, b) => a.orElse(b)
      val req = deadline.fold(r)(d => Deadline.carry(r, d, clock))

      // balancing is innermost: each hedged attempt picks its own
      // endpoint, and the breaker above counts per SERVICE
      val wire: Http = balanced.fold(inner)(_.http(inner))
      def call: Response ! Async = hedge match
        case Some((after, max)) if hedgeable(req) => Hedge.run(after, max)(wire.send(req))
        case _ => wire.send(req)
      def limited: Response ! Async = limiter match
        case Some((l, key)) => l.admit(key(req))(call)
        case None => call
      def bulk: Response ! Async = bulkhead.fold(limited)(_.limit(limited))
      def broken: Response ! Async =
        breaker.fold(bulk)(_.protect(bulk)(_.fold(_ => true, _.status >= 500)))
      deadline.fold(broken)(d => Deadline.enforce(d, clock)(broken))

  /**
   * A server. The wrapped routes stay defined exactly where they
   * were; a refusal becomes a status with `Retry-After` in whole
   * seconds where the refusal knows one: 429 for a limiter, 503 for a
   * bulkhead (or a breaker, should one refuse inside the route), 504
   * for a deadline the request carried and outlived. `deadlines`
   * off means the header is ignored here.
   */
  def route(limiter: Option[(Limiter, Request => String)] = None,
            bulkhead: Option[Bulkhead] = None,
            deadlines: Boolean = true,
            clock: () => Long = wall)
           (routes: PartialFunction[Request, Response ! Async])
           (using Scheduler, Timer): PartialFunction[Request, Response ! Async] = {
    case r if routes.isDefinedAt(r) =>
      def inner: Response ! Async = routes(r)
      def limited: Response ! Async = limiter match
        case Some((l, key)) => l.admit(key(r))(inner)
        case None => inner
      def bulk: Response ! Async = bulkhead.fold(limited)(_.limit(limited))
      def dead: Response ! Async =
        if deadlines then Deadline.read(r, clock).fold(bulk)(d => Deadline.enforce(d, clock)(bulk))
        else bulk
      Attempt(dead).map {
        case Right(resp) => resp
        case Left(e: Refused) => refused(e)
        case Left(e) => throw e
      }
  }

  /** the status a refusal maps to, as a value */
  def status(e: Refused): Int = e match
    case _: Refused.Exhausted => 429
    case _: Refused.BulkheadFull => 503
    case _: Refused.BreakerOpen => 503
    case _: Refused.DeadlineExceeded => 504
    case _: Refused.NoEndpoint => 503

  private def refused(e: Refused): Response =
    val retry = e.retryAfterMillis.map(ms => ("retry-after", math.max(1L, (ms + 999) / 1000).toString))
    Response(status(e), Seq(("content-type", "text/plain; charset=utf-8")) ++ retry,
      Http.one(e.getMessage.getBytes("UTF-8")))
