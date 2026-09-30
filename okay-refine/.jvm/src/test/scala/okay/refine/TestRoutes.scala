package okay.refine

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.Files
import okay.{Bulk, Channel, Chunks, Source, drained, localBulk, runCollect}
import okay.Chunks.elements
import okay.given
import okay.testkit.Munit.Diagnosed

/** the fixtures and the table live at the top level: a table is an
 * `object`, re-created by reference wherever a task lands */
object RoutesFixtures:
  sealed trait Doc
  final case class Swap(id: String, ccy: String) extends Doc
  final case class Fx(id: String, pair: String) extends Doc
  final case class Cds(id: String, ccy: String) extends Doc

  def kind(prefix: String): Refine[String, Vector[String]] =
    Refine.step[String, Vector[String]](prefix)(s =>
      if s.startsWith(prefix + ":") then Right(s.drop(prefix.length + 1).split(',').toVector) else Left(s"not a $prefix"))(p => prefix + ":" + p.mkString(","))
  val swap: Refine[String, Swap] = kind("swap").map("swap")(p => Swap(p(0), p(1)), s => Vector(s.id, s.ccy))
  val fx: Refine[String, Fx] = kind("fx").map("fx")(p => Fx(p(0), p(1)), f => Vector(f.id, f.pair))
  val cds: Refine[String, Cds] = kind("cds").map("cds")(p => Cds(p(0), p(1)), c => Vector(c.id, c.ccy))
  val any: Refine[String, Doc] = swap.widen[Doc] or fx.widen[Doc] or cds.widen[Doc]

  /** documents as files carry their text; the table reads the bytes */
  val fromBytes: Refine[(String, Array[Byte]), Doc] =
    Refine.step[(String, Array[Byte]), String]("text")(f => Right(new String(f._2, "UTF-8")))(s => ("?", s.getBytes("UTF-8"))) >>> any

  object Kinds extends Routes(fromBytes):
    val swaps = route[Swap]
    val rates = route[Fx | Cds]
    val usdSwaps = route("usdSwaps") { case s: Swap if s.ccy == "USD" => s }

  val docs = Vector("swap:s1,EUR", "fx:f1,EURUSD", "cds:c1,EUR", "cds:c2,USD", "swap:s2,USD", "letter:hello", "fx:f2,GBPUSD")

  /** the documents as a directory of files, one each */
  def folder(): String =
    val dir = Files.createTempDirectory("okay-refine-docs")
    docs.zipWithIndex.foreach((d, i) => Files.writeString(dir.resolve(f"doc$i%02d.txt"), d): Unit)
    dir.toString

/** specs/refine.md, refine-bulk: a routing table as a value, run over a Bulk and into channels */
class TestRoutes extends Diagnosed:
  import RoutesFixtures.*

  test("the table: lanes in declaration order, named as written, each document decided once") {
    assertEquals(Kinds.lanes.map(_.name), Vector("Swap", "Fx | Cds", "usdSwaps"))
    assertEquals(Kinds.decide(("a", "fx:f1,EURUSD".getBytes)), Right("Fx | Cds"))
    // the FIRST lane that fits wins: a USD swap is a Swap before it is a usdSwap
    assertEquals(Kinds.decide(("a", "swap:s2,USD".getBytes)), Right("Swap"))
    assertEquals(Kinds.decide(("a", "letter:hello".getBytes)).left.map(_.why), Left("declined by every pattern"))
  }

  test("split over a Bulk (one JVM): each lane typed, the rejects with why, the counts in one pass") {
    given Bulk[Chunks] = localBulk
    def all[X](c: Chunks[X]): Vector[X] = c.elements.toVector
    val out = Kinds.split[Chunks](localBulk.read(folder(), Documents.files))
    val swaps: Vector[Swap] = all(out(Kinds.swaps))
    assertEquals(swaps, Vector(Swap("s1", "EUR"), Swap("s2", "USD")))
    assertEquals(all(out(Kinds.rates)), Vector(Fx("f1", "EURUSD"), Cds("c1", "EUR"), Cds("c2", "USD"), Fx("f2", "GBPUSD")))
    assertEquals(all(out(Kinds.usdSwaps)), Vector.empty, "shadowed by Swap, as a match would be")
    assertEquals(all(out.rejected).map(r => (r.input._1, r.why)), Vector(("doc05.txt", "declined by every pattern")))
    assertEquals(out.counts, Router.Routed(Vector("Swap" -> 2, "Fx | Cds" -> 4, "usdSwaps" -> 0), rejected = 1))
  }

  test("run into channels: bound lanes deliver, an unbound lane's values are rejected as not bound, never dropped") {
    val swapsCh = Channel[Swap](); val dead = Channel[Router.Rejected[(String, Array[Byte]), Doc]]()
    val src = Source(docs.map(d => ("x", d.getBytes("UTF-8")))*)
    val r = Kinds.run(src)(Kinds.swaps ~> swapsCh, Kinds.rejected ~> dead).runWith
    assertEquals(swapsCh.drained.runCollect.runWith, Vector(Swap("s1", "EUR"), Swap("s2", "USD")))
    val why = dead.drained.runCollect.runWith.map(_.why)
    assertEquals(why.count(_ == "lane Fx | Cds is not bound here"), 4)
    assertEquals(why.count(_ == "declined by every pattern"), 1)
    assertEquals(r, Router.Routed(Vector("Swap" -> 2, "Fx | Cds" -> 0, "usdSwaps" -> 0), rejected = 5))
  }

  test("a pattern and a table serialize and come back working — what a distributed Bulk needs") {
    def roundTrip[T](t: T): T =
      val bytes = ByteArrayOutputStream()
      val out = ObjectOutputStream(bytes); out.writeObject(t); out.close()
      // resolve against the test's own loader, as Spark resolves against its executor's (sbt's default
      // "latest user-defined loader" is not the one that loaded okay-refine here)
      val loader = getClass.getClassLoader
      val in = new ObjectInputStream(ByteArrayInputStream(bytes.toByteArray)):
        override def resolveClass(d: java.io.ObjectStreamClass): Class[?] = Class.forName(d.getName, false, loader)
      // readObject answers AnyRef: the one unchecked step, and T is what was written two lines up
      in.readObject() match { case back: T @unchecked => back }
    val money = (Refine.json.field("amount") >>> Refine.json.num) and (Refine.json.field("currency") >>> Refine.json.str)
    val back = roundTrip(money)
    assertEquals(back.run(okay.codec.Json.parse("""{"amount": 5, "currency": "EUR"}""")).toOption, Some((5.0, "EUR")))
    assertEquals(roundTrip(any orElse Refine.empty).run("cds:c9,JPY").toOption, Some(Cds("c9", "JPY")))
    assert(roundTrip(Kinds) eq Kinds, "a table object comes back as itself")
    val lane = roundTrip(Kinds.rates)
    assertEquals(lane.project(Cds("c", "EUR")), Some(Cds("c", "EUR")))
    assertEquals(lane.project(Swap("s", "EUR")), None, "the union's test travels exact")
  }
