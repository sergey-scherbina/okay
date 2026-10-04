package okay2.refine

import java.nio.file.Files
import scala.jdk.CollectionConverters._
import okay2._
import okay2.async.Async
import okay2.platform._
import okay2.stream.{Bulk, Chunks, Source}
import okay2.stream.Source.SourceOps
import okay2.stream.Chunks.ChunksOps

/** a sealed domain, two levels deep, and a desk's table over it — at the
 * top level (-Xlint's outer references; a table is an `object`) */
object DispatchFixtures {
  sealed trait Instrument
  sealed trait Rate extends Instrument
  final case class Swap(id: String, ccy: String) extends Rate
  final case class Cds(id: String, name: String) extends Rate
  final case class Fx(id: String, pair: String) extends Instrument
  final case class Payment(id: String, amount: Double) extends Instrument

  def kind(prefix: String): Refine[String, Vector[String]] =
    Refine.step[String, Vector[String]](prefix)(s =>
      if (s.startsWith(prefix + ":")) Right(s.drop(prefix.length + 1).split(',').toVector) else Left(s"not a $prefix"))(p => prefix + ":" + p.mkString(","))
  val swap: Refine[String, Swap] = kind("swap").map[Swap]("swap")(p => Swap(p(0), p(1)), (s: Swap) => Vector(s.id, s.ccy))
  // the same instrument from another standard: a CDM swap
  val cdmSwap: Refine[String, Swap] = kind("cdm").map[Swap]("cdmSwap")(p => Swap(p(0), p(1)), (s: Swap) => Vector(s.id, s.ccy))
  val cds: Refine[String, Cds] = kind("cds").map[Cds]("cds")(p => Cds(p(0), p(1)), (c: Cds) => Vector(c.id, c.name))
  val fx: Refine[String, Fx] = kind("fx").map[Fx]("fx")(p => Fx(p(0), p(1)), (f: Fx) => Vector(f.id, f.pair))
  val payment: Refine[String, Payment] =
    kind("pay").map[Payment]("payment")(p => Payment(p(0), p(1).toDouble), (q: Payment) => Vector(q.id, q.amount.toString))
  val any: Refine[String, Instrument] =
    swap.widen[Instrument] or cdmSwap.widen[Instrument] or cds.widen[Instrument] or fx.widen[Instrument] or payment.widen[Instrument]

  object Desk extends Dispatch(any) {
    val eurSwaps = lane[Swap]("rates/swaps/eur")
    val swaps = lane[Swap]("rates/swaps/other")
    val credit = lane[String]("rates/credit")
    val fxs = lane[Fx]("fx")
    val payments = lane[Payment]("payments")
    // a primitive lane: the ClassTag knows the boxed class
    val amounts = lane[Double]("amounts")

    def table(i: Instrument): To = i match {
      case r: Rate => rates(r)
      case f: Fx => fxs(f)
      case p: Payment if p.amount > 1000 => amounts(p.amount)
      case p: Payment if p.amount > 0 => payments(p)
      case p: Payment => unrouted(s"payment ${p.id} has no positive amount")
    }

    def rates(r: Rate): To = r match {
      case s: Swap if s.ccy == "EUR" => eurSwaps(s)
      case s: Swap => swaps(s)
      case c: Cds => credit(c.name) // a lane may carry a projection, typed by the lane
    }
  }

  val docs = Vector("swap:s1,EUR", "fx:f1,EURUSD", "cds:c1,ACME", "swap:s2,USD", "pay:p1,100", "pay:p2,-5", "letter:hello", "cdm:s3,EUR", "pay:p3,5000")

  /** routing on WHERE it was read: the same Swap, from FpML or from CDM */
  object ByStandard extends Dispatch(any) {
    val fpmlSwaps = lane[Swap]("swaps/fpml")
    val cdmSwaps = lane[Swap]("swaps/cdm")
    val rest = lane[Instrument]("rest")
    def table(i: Instrument): To = rest(i)
    override def table(i: Instrument, by: Path): To = i match {
      // the verdict's path names the STEPS that took the input ("cdm"), not a `map`'s name
      case s: Swap if by.steps.lastOption.contains("cdm") => cdmSwaps(s)
      case s: Swap => fpmlSwaps(s)
      case other => table(other)
    }
  }

  /** a table with a bug: it throws for payments */
  object Buggy extends Dispatch(any) {
    val all = lane[Instrument]("all")
    def table(i: Instrument): To = i match {
      case p: Payment => sys.error(s"payments are not wired yet (${p.id})")
      case other => all(other)
    }
  }

  /** a lane name declared twice: refused when the table is built */
  object Twice extends Dispatch(any) {
    val a = lane[Swap]("x")
    val b = lane[Fx]("x")
    def table(i: Instrument): To = unrouted("no")
  }

  val localBulk: Bulk[Chunks] = Bulk.local(p => Files.lines(java.nio.file.Path.of(p)).iterator().asScala)
}

/** specs/refine-dispatch.md stage 4: a Scala 2 match as the routing table.
 * JVM-only, as okay's: a Source split needs a scheduler */
class TestDispatch extends munit.FunSuite {
  import DispatchFixtures._

  private def go[X](p: X ! Async): X = !.run(Async.run(p))

  test("the match routes each document to the lane its case names, typed by the lane; sub-tables and paths") {
    val out = Desk.split(docs)
    val eur: Vector[Swap] = out(Desk.eurSwaps)
    assertEquals(eur, Vector(Swap("s1", "EUR"), Swap("s3", "EUR")))
    assertEquals(out(Desk.swaps), Vector(Swap("s2", "USD")))
    val credit: Vector[String] = out(Desk.credit)
    assertEquals(credit, Vector("ACME"), "a case may deliver a projection")
    assertEquals(out(Desk.fxs), Vector(Fx("f1", "EURUSD")))
    assertEquals(out(Desk.payments), Vector(Payment("p1", 100)))
    val amounts: Vector[Double] = out(Desk.amounts)
    assertEquals(amounts, Vector(5000.0), "a primitive lane takes its boxed values back")
    val counts = out.counts
    assertEquals(counts.under("rates"), 4)
    assertEquals(counts.under("rates/swaps"), 3)
    assertEquals(counts.under("rate"), 0, "a prefix is a path, not a string prefix")
    assertEquals(counts.under("fx"), 1)
    assertEquals(counts.total, docs.length)
    assertEquals(Desk.lanes.map(_.name), Vector("rates/swaps/eur", "rates/swaps/other", "rates/credit", "fx", "payments", "amounts"))
  }

  test("unrouted says the table's reason; what the PATTERN did not take is rejected with the verdict") {
    val rj = Desk.split(docs).rejected.map(r => (r.input, r.why))
    assertEquals(rj, Vector(("pay:p2,-5", "payment p2 has no positive amount"), ("letter:hello", "declined by every pattern")))
    assertEquals(Desk.decide("swap:s9,EUR"), Right("rates/swaps/eur"))
    assertEquals(Desk.decide("letter:x").left.map(_.why), Left("declined by every pattern"))
  }

  test("ONE table over every carrier: a Vector, a Bulk collection, a Source — the same lanes and counts") {
    implicit val B: Bulk[Chunks] = localBulk
    val v = Desk.split(docs)
    val c = Desk.split(B.of(docs))
    val s = Desk.split(Source(docs: _*))
    val routed = go(s.counts)
    assertEquals(c(Desk.eurSwaps).elements.toVector, v(Desk.eurSwaps))
    assertEquals(c(Desk.amounts).elements.toVector, v(Desk.amounts))
    assertEquals(go(s(Desk.credit).runCollect), v(Desk.credit))
    assertEquals(go(s.rejected.runCollect).map(_.why), v.rejected.map(_.why))
    assertEquals(c.counts, v.counts)
    assertEquals(routed, v.counts)
  }

  test("table(b, by): the same Swap routed by the standard it was read from") {
    val out = ByStandard.split(docs)
    assertEquals(out(ByStandard.fpmlSwaps).map(_.id), Vector("s1", "s2"))
    assertEquals(out(ByStandard.cdmSwaps).map(_.id), Vector("s3"))
    assertEquals(out(ByStandard.rest).length, 5)
  }

  test("a table that throws costs ONE document, named — the rest of the input is routed") {
    val out = Buggy.split(docs)
    assertEquals(out(Buggy.all).length, 5)
    val rj = out.rejected.map(r => (r.input, r.why))
    assert(rj.contains(("pay:p1,100", "the table threw RuntimeException: payments are not wired yet (p1)")), rj.toString)
    assertEquals(rj.length, 4)
  }

  test("a lane name is declared once") {
    // an ExceptionInInitializerError is a LinkageError, which munit's `intercept` rethrows as fatal: caught by hand
    val cause = try { Twice.lanes; None } catch { case e: ExceptionInInitializerError => Option(e.getCause) }
    assert(cause.exists(_.getMessage.contains("a lane named x is declared twice")), cause.toString)
  }
}
