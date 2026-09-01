package okay.kafka

import munit.FunSuite
import okay.{!, Condition}
import okay.Condition.Decision.*
import okay.codec.Schema
import okay.persist.{Ack, Policy, Repair, Typed}

/**
 * The repair road over the REAL engine (live; skips where the
 * broker is absent — the okay-kafka docker on 9092). The seam is
 * engine-agnostic by construction (Repair works over Typed, Typed
 * over any Topic, KafkaStore is a Topic); this suite is the proof
 * on the wire, with the production shape of damage: a FOREIGN
 * producer wrote garbage bytes into the topic, and one reader
 * answers three ways under three policies.
 */
class TestKafkaRepair extends FunSuite:

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(120, "s")

  val bootstrap = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  lazy val available: Boolean =
    try
      val s = KafkaStore(bootstrap)
      try { s.topics; true } finally s.close()
    catch case _: Throwable => false

  final case class Ev(id: String, n: Int)
  given Schema[Ev] = Schema.derived

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def fresh(): String = s"okay-repair-${System.nanoTime()}"

  /** offsets 0 and 2 decode; offset 1 is a foreign producer's
   * garbage, straight onto the raw topic under the typed view */
  def seeded(store: KafkaStore): Typed[Ev] =
    val t = store.topic(fresh(), 1, Policy())
    val typed = Typed[Ev](t, version = 1, upcasts = Map.empty)
    typed.append(0, bytes("k"), Ev("a", 1), Ack.Durable)
    t.append(0, bytes("k"), Array[Byte](9, 9, 9), Ack.Durable)
    typed.append(0, bytes("k"), Ev("c", 3), Ack.Durable)
    typed

  def road(typed: Typed[Ev]): Vector[(Long, Ev)] ! Condition.Op =
    Repair.read(typed, 0, 0L, 10)

  test("patch over the wire: the corrected value sits at the damaged offset") {
    assume(available, s"no Kafka at $bootstrap — the live suite skips")
    val store = KafkaStore(bootstrap)
    try
      val out = !.run(Condition.run[Vector[(Long, Ev)], okay.Pure] {
        case (Repair.Damaged(off, _, _), _) => Resume(Ev("patched", off.toInt))
        case _ => Fail
      }(road(seeded(store))))
      assertEquals(out, Vector(0L -> Ev("a", 1), 1L -> Ev("patched", 1), 2L -> Ev("c", 3)))
    finally store.close()
  }

  test("skip over the wire: the damaged offset vanishes, the rest survives") {
    assume(available, s"no Kafka at $bootstrap — the live suite skips")
    val store = KafkaStore(bootstrap)
    try
      val out = !.run(Condition.run[Vector[(Long, Ev)], okay.Pure] {
        case (_: Repair.Damaged, menu) =>
          assertEquals(menu, Vector("skip"))
          Invoke("skip", ())
        case _ => Fail
      }(road(seeded(store))))
      assertEquals(out, Vector(0L -> Ev("a", 1), 2L -> Ev("c", 3)))
    finally store.close()
  }

  test("fail over the wire: the abort names the offset the broker assigned") {
    assume(available, s"no Kafka at $bootstrap — the live suite skips")
    val store = KafkaStore(bootstrap)
    try
      val e = intercept[Condition.Unhandled](
        !.run(Condition.run[Vector[(Long, Ev)], okay.Pure] { (_, _) => Fail }(
          road(seeded(store)))))
      e.condition match
        case Repair.Damaged(off, _, _) => assertEquals(off, 1L)
        case other => fail(s"unexpected condition: $other")
    finally store.close()
  }
