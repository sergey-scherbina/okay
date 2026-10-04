package okay2.codec

import okay2.{Answers, Row}

/** a signature whose operations perform and decode THEMSELVES — the
 * per-case road Scala 2 takes where Scala 3 refines `A` in a match */
sealed trait Calc extends Row { type Op[+A] = Calc.Op[A] }
object Calc {
  sealed trait Op[+A] {
    def name: String
    def fingerprint: String
    def withKey(key: String): Op[A]
    def run(h: Answers[Calc]): (A, String)
    def decode(written: String): A
  }
  final case class Lookup(k: String, key: Option[String] = None) extends Op[Int] {
    def name = "lookup"
    def fingerprint = s"lookup($k)"
    def withKey(key: String): Op[Int] = copy(key = Some(key))
    def run(h: Answers[Calc]): (Int, String) = { val n: Int = h.handle(this); (n, n.toString) }
    def decode(written: String): Int = written.toInt
  }
  final case class Greet(who: String) extends Op[String] {
    def name = "greet"
    def fingerprint = s"greet($who)"
    def withKey(key: String): Op[String] = this
    def run(h: Answers[Calc]): (String, String) = { val s: String = h.handle(this); (s, s) }
    def decode(written: String): String = written
  }

  /** the instance forwards to the cases: no match, so no cast */
  implicit val journalled: Journalled[Calc] = new Journalled[Calc] {
    def name[A](op: Op[A]): String = op.name
    def fingerprint[A](op: Op[A]): String = op.fingerprint
    def withKey[A](op: Op[A], key: String): Op[A] = op.withKey(key)
    def perform[A](op: Op[A], inner: Answers[Calc]): (A, String) = op.run(inner)
    def decode[A](op: Op[A], written: String): A = op.decode(written)
  }
}

/** What a journal needs of an operation (okay-codec's Journalled):
 * performed, written down, read back as the same typed answer. */
class TestJournalled extends munit.FunSuite {

  val calls = scala.collection.mutable.ArrayBuffer.empty[Any]
  val answers: Answers[Calc] = new Answers[Calc] {
    def handle[A](a: Calc.Op[A]): A = { calls += a; handleOp[A](a) }
    override def handleOp[A](op: Any): A = op match {
      case Calc.Lookup(k, _) => k.length.asInstanceOf[A]   // a test handler's answer, asserted as okay2's tests do
      case Calc.Greet(w) => s"hi $w".asInstanceOf[A]
      case other => throw new IllegalStateException(other.toString)
    }
  }
  val J = implicitly[Journalled[Calc]]

  test("perform answers the typed value beside its written form, and decode reads it back") {
    val (n, w) = J.perform(Calc.Lookup("abcd"), answers)
    assertEquals(n + 1, 5)
    assertEquals(w, "4")
    assertEquals(J.decode(Calc.Lookup("abcd"), w), 4)
    val (s, ws) = J.perform(Calc.Greet("ada"), answers)
    assertEquals((s, ws), ("hi ada", "hi ada"))
    assertEquals(J.decode(Calc.Greet("ada"), ws), "hi ada")
  }

  test("name, fingerprint, the key on a retry, and asked defaulting to the fingerprint") {
    assertEquals(J.name(Calc.Lookup("k")), "lookup")
    assertEquals(J.fingerprint(Calc.Lookup("k")), "lookup(k)")
    assertEquals(J.withKey(Calc.Lookup("k"), "x-1"), Calc.Lookup("k", Some("x-1")))
    assertEquals(J.withKey(Calc.Greet("w"), "x-1"), Calc.Greet("w"))
    assertEquals(J.asked(Calc.Greet("w")), Json.JStr("greet(w)"): Json)
  }
}
