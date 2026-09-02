package okay.subscription

import okay.agent.ToolSpec
import okay.codec.Json.*

/**
 * Gate a resource behind a paid period (specs/subscription.md):
 * a subject shows/matches FREE for its first calendar month; after
 * that, only a period actually PAID keeps it visible. Unpaid: gated
 * — but NEVER deleted. This module has no opinion about what a
 * `uuid` names — a marketplace profile today, anything with an id
 * tomorrow.
 *
 * Extracted 2026-09-02 from okay-demo's ChatDemo.scala
 * (demo-subscription-gate): a pure move, no behavior change.
 */
object Subscription:

  final case class Period(y: Int, m: Int):
    def key: String = f"$y%04d-$m%02d"

  object Period:
    def of(epochMillis: Long): Period =
      val d = java.time.Instant.ofEpochMilli(epochMillis).atZone(java.time.ZoneOffset.UTC)
      Period(d.getYear, d.getMonthValue)
    def now(): Period = of(System.currentTimeMillis())

  private val joinedPeriod =
    java.util.concurrent.ConcurrentHashMap[String, Period]()
  private val paidPeriods =
    java.util.concurrent.ConcurrentHashMap[String, java.util.Set[String]]()

  /** the first period a subject was ever gate-checked — anchored
   * LAZILY, so a subject this module never touched defaults to "just
   * joined" rather than surprise-gated */
  private def joinedOf(uuid: String, now: Period): Period =
    joinedPeriod.computeIfAbsent(uuid, _ => now)

  private def paidThisPeriod(uuid: String, now: Period): Boolean =
    Option(paidPeriods.get(uuid)).exists(_.contains(now.key))

  /** free for the join period; after that, paid-this-period or gated */
  def subscribed(uuid: String, now: Period = Period.now()): Boolean =
    joinedOf(uuid, now) == now || paidThisPeriod(uuid, now)

  /** takes effect IMMEDIATELY — the very next `subscribed` check
   * (same turn, even) sees it */
  def pay(uuid: String, now: Period = Period.now()): Unit =
    paidPeriods.computeIfAbsent(uuid, _ => java.util.concurrent.ConcurrentHashMap.newKeySet[String]())
      .add(now.key): Unit

  /** the test seam for "a month passed": `subscribed` only ANCHORS
   * `joined` on a subject's first-ever check (so production never
   * needs this) — a test that wants a subject ALREADY past its free
   * month calls this to force the anchor back in time */
  def backdateJoin(uuid: String, period: Period): Unit =
    joinedPeriod.put(uuid, period): Unit

  def subscriptionNotice(uuid: String, now: Period = Period.now()): Option[String] =
    if subscribed(uuid, now) then None
    else Some("your free month is over — say \"pay\" (\"оплатить\") to show up in search and matching again this month")

  /** the static contract for "pay" as an LLM tool — pure data, so
   * every consumer gets the identical schema instead of re-deriving
   * it. WHERE this is wired into a tool table, and what side effects
   * ride along the actual payment action, is per-app: this module
   * ships the contract, not the wiring */
  val paySpec: ToolSpec = ToolSpec("subscription_pay",
    "Pay the profile's subscription for the current period — call it when the user asks to pay/subscribe.",
    JObj(Vector("type" -> JStr("object"), "properties" -> JObj(Vector(
      "profile" -> JObj(Vector("type" -> JStr("string"))))))))
