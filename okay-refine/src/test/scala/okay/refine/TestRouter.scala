package okay.refine

import okay.{!, %, Async, Channel, Source, Writer, drained, pure, runCollect, through}
import okay.given
import okay.testkit.Munit.Diagnosed
import scala.util.chaining.*

/** specs/refine.md, refine-route: one stream of documents in, one stream per kind out */
class TestRouter extends Diagnosed:

  // a small domain: three kinds of "document", recognised from text
  sealed trait Doc
  final case class Swap(id: String, ccy: String) extends Doc
  final case class Fx(id: String, pair: String) extends Doc
  final case class Cds(id: String, ccy: String) extends Doc

  def kind(prefix: String): Refine[String, Array[String]] =
    Refine.step[String, Array[String]](prefix)(s =>
      if s.startsWith(prefix + ":") then Right(s.drop(prefix.length + 1).split(',')) else Left(s"not a $prefix"))(p => prefix + ":" + p.mkString(","))
  val swap: Refine[String, Swap] = kind("swap").map("swap")(p => Swap(p(0), p(1)), s => Array(s.id, s.ccy))
  val fx: Refine[String, Fx] = kind("fx").map("fx")(p => Fx(p(0), p(1)), f => Array(f.id, f.pair))
  val cds: Refine[String, Cds] = kind("cds").map("cds")(p => Cds(p(0), p(1)), c => Array(c.id, c.ccy))
  val any: Refine[String, Doc] = swap.widen[Doc] or fx.widen[Doc] or cds.widen[Doc]

  val docs = Vector("swap:s1,EUR", "fx:f1,EURUSD", "cds:c1,EUR", "cds:c2,USD", "swap:s2,USD", "letter:hello", "fx:f2,GBPUSD")

  private def drain[X](c: Channel[X]): Vector[X] = c.drained.runCollect.runWith

  test("route[X] by type, route { case … } by pattern, otherwise for the rest: every document lands exactly once") {
    val swaps = Channel[Swap](); val fxs = Channel[Fx](); val eurCds = Channel[Cds](); val rejected = Channel[Router.Rejected[String, Doc]]()
    val routed = Router(any)
      .route[Swap](swaps)
      .route[Fx](fxs)
      .route { case c: Cds if c.ccy == "EUR" => c }(eurCds)
      .otherwise(rejected)
      .run(Source(docs*)).runWith
    note(routed.toString)
    assertEquals(drain(swaps), Vector(Swap("s1", "EUR"), Swap("s2", "USD")))
    assertEquals(drain(fxs), Vector(Fx("f1", "EURUSD"), Fx("f2", "GBPUSD")))
    assertEquals(drain(eurCds), Vector(Cds("c1", "EUR")))
    val rj = drain(rejected)
    assertEquals(rj.map(_.input), Vector("cds:c2,USD", "letter:hello"))
    assertEquals(rj.map(_.why), Vector("no route for cds", "declined by every pattern"))
    assertEquals(routed, Router.Routed(Vector("Swap" -> 2, "Fx" -> 2, "case #3" -> 1), rejected = 2))
    assertEquals(routed.total, docs.length)
  }

  test("several kinds into ONE stream: a union by pattern, or two rules to the same channel — closed once") {
    val rates = Channel[Doc](); val rest = Channel[Fx]()
    val u = Router(any).route { case d @ (_: Swap | _: Cds) => d }(rates).route[Fx](rest).run(Source(docs*)).runWith
    assertEquals(u.delivered, Vector("case #1" -> 4, "Fx" -> 2))
    assertEquals(drain(rates).map { case Swap(id, _) => id; case Cds(id, _) => id; case Fx(id, _) => id }, Vector("s1", "c1", "c2", "s2"))
    val same = Channel[Doc]()
    val r = Router(any).route { case s: Swap => s: Doc }(same).route { case c: Cds => c: Doc }(same).run(Source(docs*)).runWith
    assertEquals(drain(same).length, 4)
    assert(same.isClosed)
    assertEquals(r.rejected, 3, "the fx and the letter: no route, and counted even without an otherwise")
  }

  test("route[P | Q]: a union routes exactly its cases — the sibling it shares a supertype with is NOT taken") {
    val rates = Channel[Swap | Cds](); val fxs = Channel[Fx]()
    val r = Router(any).route[Swap | Cds](rates).route[Fx](fxs).run(Source(docs*)).runWith
    assertEquals(drain(rates).map { case Swap(id, _) => id; case Cds(id, _) => id }, Vector("s1", "c1", "c2", "s2"))
    assertEquals(drain(fxs).length, 2)
    assertEquals(r.delivered, Vector("Swap | Cds" -> 4, "Fx" -> 2))
  }

  test("first rule that fits wins, like a match; tap sees every recognised document whichever route it takes") {
    val all = Channel[Doc](); val eur = Channel[Doc](); val swaps = Channel[Swap](); val audit = Channel[Doc]()
    Router(any)
      .route { case s: Swap if s.ccy == "EUR" => s: Doc }(eur)
      .route[Swap](swaps)
      .route { case d => d }(all)
      .tap(audit)
      .run(Source(docs*)).runWith
      .pipe(r => assertEquals(r.delivered.map(_._2), Vector(1, 1, 4)))
    assertEquals(drain(eur), Vector(Swap("s1", "EUR")))
    assertEquals(drain(swaps), Vector(Swap("s2", "USD")))
    assertEquals(drain(all).length, 4)
    assertEquals(drain(audit).length, 6, "every recognised document, the letter is not one")
  }

  test("byName: the pattern that took the document, for kinds that share a value type") {
    val a: Refine[String, Array[String]] = kind("swap") or kind("fx")
    val swapsRaw = Channel[Array[String]](); val fxRaw = Channel[Array[String]]()
    assertEquals(Router(a).byName("swap")(swapsRaw).byName("fx")(fxRaw).run(Source(docs*)).runWith.delivered, Vector("swap" -> 2, "fx" -> 2))
    assertEquals(drain(swapsRaw).map(_.head), Vector("s1", "s2"))
    assertEquals(drain(fxRaw).map(_.head), Vector("f1", "f2"))
  }

  test("an Unclear document is never routed: it goes to otherwise with both readings named") {
    val both: Refine[String, Doc] = any or kind("swap").map("alsoSwap")(p => Swap(p(0), "?"), s => Array(s.id, s.ccy)).widen[Doc]
    val swaps = Channel[Swap](); val rejected = Channel[Router.Rejected[String, Doc]]()
    assertEquals(Router(both).route[Swap](swaps).otherwise(rejected).run(Source("swap:s1,EUR")).runWith.rejected, 1)
    assertEquals(drain(swaps), Vector.empty)
    val rj = drain(rejected)
    assert(rj.head.why.startsWith("unclear: swap | swap"), rj.head.why)
    assert(rj.head.verdict.isInstanceOf[Verdict.Unclear[?]])
  }

  test("decide: where one document would go, without running anything") {
    val r = Router(any).route[Swap](Channel[Swap]()).route[Fx](Channel[Fx]())
    assertEquals(r.decide("fx:f1,EURUSD"), Right("Fx"))
    assertEquals(r.decide("cds:c1,EUR").left.map(_.why), Left("no route for cds"))
  }

  test("a failing input fails every channel with the same error: no consumer waits forever") {
    val swaps = Channel[Swap]()
    val boom: Source[String] =
      Source.of(List("swap:s1,EUR")).flatMap(_ =>
        !.widen[Unit, Async, Writer % String](okay.effect[Async, Unit](Async.Run(() => throw IllegalStateException("disk gone")))))
    val e = intercept[IllegalStateException](Router(any).route[Swap](swaps).run(boom).runWith)
    assertEquals(e.getMessage, "disk gone")
    assertEquals(swaps.failed.map(_.getMessage), Some("disk gone"))
  }

  test("routed: the synchronous twin — one stream, each element tagged with its key, the rest Left with why") {
    val inputs: Unit ! Writer % String = docs.foldLeft(pure[Writer % String, Unit](()))((m, s) => m.flatMap(_ => Writer.tell(s)))
    val (out, _) = !.run(Writer.run(through(inputs)(any.routed(_.getClass.getSimpleName))))
    assertEquals(out.collect { case Right((k, _)) => k }, Seq("Swap", "Fx", "Cds", "Cds", "Swap", "Fx"))
    assertEquals(out.collect { case Left(r) => r.input }, Seq("letter:hello"))
  }
