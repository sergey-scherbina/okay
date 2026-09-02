package okay.subscription

import okay.subscription.Subscription.*

/**
 * specs/subscription.md — the gate's own behavior, unit-level (no
 * HTTP, no MatchStore): TestChatDemo's SUBSCRIPTION GATE tests still
 * prove the end-to-end wiring through real routes; these prove the
 * module in isolation.
 */
class TestSubscription extends munit.FunSuite {

  def freshUuid(): String = java.util.UUID.randomUUID().toString

  test("a fresh subject is subscribed in its join period, with no notice") {
    val u = freshUuid()
    val now = Period(2026, 9)
    assert(subscribed(u, now))
    assertEquals(subscriptionNotice(u, now), None)
  }

  test("backdateJoin gates a subject past its free month; subscribed false, notice present") {
    val u = freshUuid()
    backdateJoin(u, Period(2000, 1))
    assert(!subscribed(u))
    assert(subscriptionNotice(u).exists(n => n.contains("pay") && n.contains("оплатить")))
  }

  test("pay un-gates the SAME check that follows it, no delay") {
    val u = freshUuid()
    backdateJoin(u, Period(2000, 1))
    assert(!subscribed(u))
    pay(u)
    assert(subscribed(u))
    assertEquals(subscriptionNotice(u), None)
  }

  test("subscribed is a QUERY, not a mutator: re-checking with an old period after the real anchor is a no-op") {
    val u = freshUuid()
    val real = Period.now()
    assert(subscribed(u, real))          // anchors `joined` to `real`, lazily
    assert(!subscribed(u, Period(2000, 1)))  // does NOT retroactively move the anchor
    assert(subscribed(u, real))          // the real anchor still holds
  }

  test("pay only covers the period it names — a different period stays gated") {
    val u = freshUuid()
    backdateJoin(u, Period(2000, 1))
    pay(u, Period(2026, 9))
    assert(subscribed(u, Period(2026, 9)))
    assert(!subscribed(u, Period(2026, 10)))
  }

  test("a subject this module never saw defaults to subscribed — no surprise-gating") {
    val u = freshUuid()
    assert(subscribed(u))
  }

  test("Period.key formats as zero-padded year-month") {
    assertEquals(Period(2026, 9).key, "2026-09")
    assertEquals(Period(7, 1).key, "0007-01")
  }

  test("Period.of / Period.now round-trip through epoch millis") {
    val p = Period.of(java.time.Instant.parse("2026-09-15T12:00:00Z").toEpochMilli)
    assertEquals(p, Period(2026, 9))
  }

  test("paySpec is the static tool contract, reusable as-is") {
    assertEquals(paySpec.name, "subscription_pay")
    assert(paySpec.description.nonEmpty)
  }
}
