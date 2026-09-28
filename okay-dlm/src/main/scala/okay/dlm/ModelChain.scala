package okay.dlm

import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

/**
 * THE ONE PLACE A GENERATIVE MODEL MAY RUN, and the budget around it
 * (specs/dlm.md, "The door").
 *
 * A deterministic model answers what it knows without generating a
 * word. What it does not know, at the door and only there, goes down
 * this chain: a lane answers or fails, the next is asked, and when
 * none answers the reply is what the model already says when it does
 * not know. A lane may enliven a reply and choose a tool; it never
 * supplies data — that guard is the caller's, over the reply.
 *
 * A LANE IS A PLAIN FUNCTION. Providers, handlers and the agent loop
 * are built by the caller, where its givens live; this knows only
 * "text in, reply out, or nothing", which is what lets it be tested
 * with lambdas and keeps the ordering rules in one readable place.
 *
 * FAILED means thrown, timed out, or empty. The request path does not
 * wait for a rate limit to lift; it moves on. `retireAfter` failures
 * running retire a lane for a cooldown, so a provider with no credits
 * does not cost every turn its timeout. `dailyCap` counts every
 * ATTEMPT per lane per UTC day — a failed call is paid for too — and
 * `seed` refills the count from a journal at boot, so a restart does
 * not reopen a spent budget.
 */
final case class Lane(name: String,
                      run: Lane.Ask => String,
                      /** the PROMPT this lane speaks, as a fingerprint —
                       * what a recorded answer is scored against later;
                       * empty records nothing */
                      fingerprint: String = "")

object Lane:
  /** what a lane is asked: the text, the history as (role, text), the
   * tool table it may call, and the language to answer in */
  final case class Ask(text: String, history: Seq[(String, String)],
                       tools: Map[String, okay.agent.ToolCall => String], lang: String)

final class ModelChain(val lanes: Vector[Lane],
                       timeoutMs: Long = 25000L,
                       retireAfter: Int = 3,
                       cooldownMs: Long = 5 * 60 * 1000L,
                       now: () => Long = () => System.currentTimeMillis(),
                       /** calls per lane per day; 0 is no cap */
                       dailyCap: Int = 0,
                       /** what happened, for the caller's log */
                       report: ModelChain.Event => Unit = _ => ()):
  import ModelChain.Event

  private val strikes = java.util.concurrent.ConcurrentHashMap[String, Int]()
  private val retiredUntil = java.util.concurrent.ConcurrentHashMap[String, Long]()
  private val spent = java.util.concurrent.ConcurrentHashMap[String, Int]()

  private def day(ms: Long): String =
    java.time.Instant.ofEpochMilli(ms).atZone(java.time.ZoneOffset.UTC).toLocalDate.toString
  private def key(lane: String, ms: Long) = s"$lane@${day(ms)}"

  /** calls made on this lane today, by this process or seeded from a journal */
  def spentToday(lane: String): Int = Option(spent.get(key(lane, now()))).getOrElse(0)
  def capped(lane: String): Boolean = dailyCap > 0 && spentToday(lane) >= dailyCap
  /** a call a journal says was made at `atMs` — replayed at boot */
  def seed(lane: String, atMs: Long): Unit = spent.merge(key(lane, atMs), 1, _ + _): Unit

  private val pool = Executors.newCachedThreadPool(r => {
    val t = Thread(r, "model-chain"); t.setDaemon(true); t
  })

  /** the lanes that may be asked right now: not retired, not capped */
  def live: Vector[Lane] = lanes.filter(l =>
    Option(retiredUntil.get(l.name)).forall(_ <= now()) && !capped(l.name))

  /** the reply and who gave it, or `None` when nobody could */
  def answer(ask: Lane.Ask): Option[(String, String)] =
    var got: Option[(String, String)] = None
    val it = live.iterator
    while got.isEmpty && it.hasNext do
      val lane = it.next()
      val n = spent.merge(key(lane.name, now()), 1, _ + _)
      if dailyCap > 0 && n == dailyCap then report(Event.Capped(lane.name, dailyCap))
      val fut = pool.submit[String](() => lane.run(ask))
      val reply =
        try
          val r = fut.get(timeoutMs, TimeUnit.MILLISECONDS)
          if r == null then "" else r.trim
        catch
          case _: TimeoutException =>
            fut.cancel(true)
            report(Event.TimedOut(lane.name, timeoutMs))
            ""
          case e: Throwable =>
            report(Event.Failed(lane.name, Option(e.getCause).getOrElse(e)))
            ""
      if reply.isEmpty then
        val k = strikes.merge(lane.name, 1, _ + _)
        if k >= retireAfter then
          retiredUntil.put(lane.name, now() + cooldownMs)
          strikes.put(lane.name, 0)
          report(Event.Retired(lane.name, cooldownMs))
      else
        strikes.put(lane.name, 0)
        if lane.fingerprint.nonEmpty then report(Event.Answered(lane.name, lane.fingerprint, ask.text, reply))
        got = Some(lane.name -> reply)
    got

object ModelChain:

  /** what the chain reports to the caller's log — never printed here */
  enum Event:
    case Capped(lane: String, cap: Int)
    case TimedOut(lane: String, afterMs: Long)
    case Failed(lane: String, cause: Throwable)
    case Retired(lane: String, forMs: Long)
    /** an answer worth recording: the lane, its prompt fingerprint,
     * the text and the reply — so the door can be measured offline */
    case Answered(lane: String, fingerprint: String, text: String, reply: String)

  /**
   * THE SET IS THE OPERATOR'S, THE PRIORITY IS THE CODE'S. The lanes
   * that cost nothing per turn go FIRST, whatever order the operator
   * wrote; the paid lanes are the BACKUP, in the order written, and
   * the first that answers wins — so the door needs ONE of them to be
   * up, not a particular one. What makes that safe is that a lane
   * cannot supply data in any case, so what a weaker lane costs is the
   * wording, not the facts.
   */
  def prioritise(names: Vector[String], free: Set[String]): Vector[String] =
    names.filter(free) ++ names.filterNot(free)
