package okay.persist

import munit.FunSuite

/**
 * The election battery (specs/consensus.md), written against ANY
 * totally-ordered control topic — the two-engine acceptance is the
 * point: memory, the file arbiter and Kafka must all pass it
 * unchanged, because the machinery consumes total order and
 * nothing else.
 */
abstract class ElectionSuite extends FunSuite:

  /** a fresh single-partition control topic per test */
  def mkControl(): Topic

  var now = 0L
  def mkNode(control: Topic, name: String): Election =
    Election(control, name, leaseMillis = 5000, skewMillis = 1000, clock = () => now)

  override def beforeEach(context: BeforeEach): Unit = { now = 0L }

  test("concurrent claims at one epoch: exactly one wins, on EVERY node's fold") {
    val control = mkControl()
    val a = mkNode(control, "A")
    val b = mkNode(control, "B")
    val observer = mkNode(control, "C")
    // the race, made literal: both claims land before either refolds
    val typed = Typed[Election.Claim](control, 1, Map.empty)
    typed.append(0, Array.empty, Election.Claim.Take(0, 1, "A"), Ack.Durable): Unit
    typed.append(0, Array.empty, Election.Claim.Take(0, 1, "B"), Ack.Durable): Unit

    for n <- Vector(a, b, observer) do
      assertEquals(n.leader(0), Some((1L, "A")), s"${n.node}'s fold disagreed")
    // a raw winner that never leases WOULD lose the seat (that is
    // the liveness rule working); once it holds a lease, the loser
    // observes its loss and cannot claim
    typed.append(0, Array.empty, Election.Claim.Lease(0, 1, "A", now + 5000), Ack.Durable): Unit
    assertEquals(b.tryTakeover(0), None)
  }

  test("a takeover through the API: the winner holds a lease; a second claimant sees no vacancy") {
    val control = mkControl()
    val a = mkNode(control, "A")
    val b = mkNode(control, "B")
    assertEquals(a.tryTakeover(0), Some(1L))
    assertEquals(b.tryTakeover(0), None)
    assertEquals(b.leader(0), Some((1L, "A")))
  }

  test("an expired lease past the skew allowance opens the seat; renewal keeps it shut") {
    val control = mkControl()
    val a = mkNode(control, "A")
    val b = mkNode(control, "B")
    assertEquals(a.tryTakeover(0), Some(1L))

    now = 5500                       // inside lease
    assert(!b.vacant(0))
    now = 5800; a.heartbeat()        // the renewal
    now = 10500                      // past the ORIGINAL lease, inside the renewed one
    assert(!b.vacant(0), "the renewal did not hold the seat")
    now = 5800 + 5000 + 1001         // past renewed until + skew
    assert(b.vacant(0))
    assertEquals(b.tryTakeover(0), Some(2L))
    assertEquals(a.leader(0), Some((2L, "B")), "the deposed node's fold disagreed")
  }

  test("the operator record outranks a concurrent automatic claim at the same epoch") {
    val control = mkControl()
    val a = mkNode(control, "A")
    val b = mkNode(control, "B")
    val typed = Typed[Election.Claim](control, 1, Map.empty)
    // the automatic claim lands FIRST; the human still wins the epoch
    typed.append(0, Array.empty, Election.Claim.Take(0, 1, "A"), Ack.Durable): Unit
    typed.append(0, Array.empty, Election.Claim.Operator(0, 1, "B"), Ack.Durable): Unit
    for n <- Vector(a, b) do
      assertEquals(n.leader(0), Some((1L, "B")), s"${n.node}'s fold let automation outrank the operator")
  }

  test("partitions elect independently") {
    val control = mkControl()
    val a = mkNode(control, "A")
    val b = mkNode(control, "B")
    assertEquals(a.tryTakeover(0), Some(1L))
    assertEquals(b.tryTakeover(1), Some(1L))
    assertEquals(a.leader(1), Some((1L, "B")))
    assertEquals(b.leader(0), Some((1L, "A")))
  }
