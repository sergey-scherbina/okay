package okay.persist

import munit.FunSuite
import okay.{!, Condition}
import okay.Condition.Decision.*
import okay.codec.Schema

/**
 * The repair road over a real topic (specs/condition.md, the first
 * consumer): damaged records ask, and ONE log answers three ways
 * under three policies — patched in place, skipped, or aborted
 * naming the offset.
 */
class TestRepair extends FunSuite:

  final case class Ev(id: String, n: Int)
  given Schema[Ev] = Schema.derived

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")

  /** a topic with damage in the middle: offsets 0 and 2 decode,
   * offset 1 is garbage bytes */
  def seeded(): Typed[Ev] =
    val t = MemoryStore().topic("events")
    val typed = Typed[Ev](t, version = 1, upcasts = Map.empty)
    typed.append(0, bytes("k"), Ev("a", 1), Ack.Durable)
    t.append(0, bytes("k"), Array[Byte](9, 9, 9), Ack.Durable)
    typed.append(0, bytes("k"), Ev("c", 3), Ack.Durable)
    typed

  def road(typed: Typed[Ev]): Vector[(Long, Ev)] ! Condition.Op =
    Repair.read(typed, 0, 0L, 10)

  test("patch: the corrected value flows in exactly where the damage sat") {
    val typed = seeded()
    val out = !.run(Condition.run[Vector[(Long, Ev)], okay.Pure] {
      case (Repair.Damaged(off, _, raw), menu) =>
        assertEquals(menu, Vector("skip"))
        assertEquals(off, 1L)
        assert(raw.value.nonEmpty)
        Resume(Ev("patched", 2))
      case _ => Fail
    }(road(typed)))
    assertEquals(out, Vector((0L, Ev("a", 1)), (1L, Ev("patched", 2)), (2L, Ev("c", 3))))
  }

  test("skip: the element vanishes, order and offsets of the rest survive") {
    val typed = seeded()
    val out = !.run(Condition.run[Vector[(Long, Ev)], okay.Pure] {
      case (_: Repair.Damaged, _) => Invoke("skip", ())
      case _ => Fail
    }(road(typed)))
    assertEquals(out, Vector((0L, Ev("a", 1)), (2L, Ev("c", 3))))
  }

  test("fail: the abort names the offset and the error") {
    val typed = seeded()
    val e = intercept[Condition.Unhandled](
      !.run(Condition.run[Vector[(Long, Ev)], okay.Pure]((_, _) => Fail)(road(typed))))
    e.condition match
      case Repair.Damaged(off, err, _) =>
        assertEquals(off, 1L)
        assert(err.nonEmpty)
      case other => fail(s"expected Damaged, got $other")
    assertEquals(e.menu, Vector("skip"))
  }

  test("a clean slice never signals: the policy is never consulted") {
    val t = MemoryStore().topic("clean")
    val typed = Typed[Ev](t, 1, Map.empty)
    typed.append(0, bytes("k"), Ev("a", 1), Ack.Durable)
    var consulted = false
    val out = !.run(Condition.run[Vector[(Long, Ev)], okay.Pure] { (_, _) =>
      consulted = true; Fail
    }(Repair.read(typed, 0, 0L, 10)))
    assertEquals(out.map(_._2), Vector(Ev("a", 1)))
    assert(!consulted)
  }
