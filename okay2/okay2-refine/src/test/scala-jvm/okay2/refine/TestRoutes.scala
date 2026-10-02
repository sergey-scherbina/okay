package okay2.refine

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamClass}
import java.nio.file.Files
import scala.jdk.CollectionConverters._
import okay2._
import okay2.async.Async
import okay2.platform._
import okay2.stream.{Bulk, Channel, Chunks, Source}
import okay2.stream.Channel.ChannelOps
import okay2.stream.Source.SourceOps
import okay2.stream.Chunks.ChunksOps

/** the fixtures and the table live at the top level (-Xlint's outer
 * references; and a table is an `object`, re-created by reference) */
object RoutesFixtures {
  sealed trait Doc
  final case class Swap(id: String, ccy: String) extends Doc
  final case class Fx(id: String, pair: String) extends Doc
  final case class Cds(id: String, ccy: String) extends Doc

  def kind(prefix: String): Refine[String, Vector[String]] =
    Refine.step[String, Vector[String]](prefix)(s =>
      if (s.startsWith(prefix + ":")) Right(s.drop(prefix.length + 1).split(',').toVector) else Left(s"not a $prefix"))(p => prefix + ":" + p.mkString(","))
  val any: Refine[String, Doc] =
    kind("swap").map[Swap]("swap")(p => Swap(p(0), p(1)), (s: Swap) => Vector(s.id, s.ccy)).widen[Doc] or
      kind("fx").map[Fx]("fx")(p => Fx(p(0), p(1)), (f: Fx) => Vector(f.id, f.pair)).widen[Doc] or
      kind("cds").map[Cds]("cds")(p => Cds(p(0), p(1)), (c: Cds) => Vector(c.id, c.ccy)).widen[Doc]
  val fromBytes: Refine[(String, Array[Byte]), Doc] =
    Refine.step[(String, Array[Byte]), String]("text")(f => Right(new String(f._2, "UTF-8")))(s => ("?", s.getBytes("UTF-8"))) >>> any

  object Kinds extends Routes(fromBytes) {
    val swaps = route[Swap]
    val rates = route("rates") { case d @ (_: Fx | _: Cds) => d }
    val usdSwaps = route("usdSwaps") { case s: Swap if s.ccy == "USD" => s }
  }

  val docs = Vector("swap:s1,EUR", "fx:f1,EURUSD", "cds:c1,EUR", "cds:c2,USD", "swap:s2,USD", "letter:hello", "fx:f2,GBPUSD")

  def folder(): String = {
    val dir = Files.createTempDirectory("okay2-refine-docs")
    docs.zipWithIndex.foreach { case (d, i) => val _ = Files.writeString(dir.resolve(f"doc$i%02d.txt"), d) }
    dir.toString
  }

  val localBulk: Bulk[Chunks] = Bulk.local(p => Files.lines(java.nio.file.Path.of(p)).iterator().asScala)
}

/** okay's refine-bulk on the Scala 2 core. JVM-only: files, channels, Java serialization */
class TestRoutes extends munit.FunSuite {
  import RoutesFixtures._

  private def go[X](p: X ! Async): X = !.run(Async.run(p))

  test("the table: lanes in declaration order, each document decided once, first lane wins") {
    assertEquals(Kinds.lanes.map(_.name), Vector("Swap", "rates", "usdSwaps"))
    assertEquals(Kinds.decide(("a", "fx:f1,EURUSD".getBytes)), Right("rates"))
    assertEquals(Kinds.decide(("a", "swap:s2,USD".getBytes)), Right("Swap"))
    assertEquals(Kinds.decide(("a", "letter:hello".getBytes)).left.map(_.why), Left("declined by every pattern"))
  }

  test("split over a Bulk (one JVM): each lane typed, the rejects with why, the counts in one pass") {
    implicit val B: Bulk[Chunks] = localBulk
    val out = Kinds.split(Documents.files[Chunks](folder()))
    val swaps: Vector[Swap] = out(Kinds.swaps).elements.toVector
    assertEquals(swaps, Vector(Swap("s1", "EUR"), Swap("s2", "USD")))
    assertEquals(out(Kinds.rates).elements.toVector, Vector(Fx("f1", "EURUSD"), Cds("c1", "EUR"), Cds("c2", "USD"), Fx("f2", "GBPUSD")))
    assertEquals(out(Kinds.usdSwaps).elements.toVector, Vector.empty[Swap])
    assertEquals(out.rejected.elements.toVector.map(r => (r.input._1, r.why)), Vector(("doc05.txt", "declined by every pattern")))
    assertEquals(out.counts, Router.Routed(Vector("Swap" -> 2, "rates" -> 4, "usdSwaps" -> 0), rejected = 1))
  }

  test("run into channels: bound lanes deliver, an unbound lane's values are rejected as not bound, never dropped") {
    val swapsCh = Channel[Swap](); val dead = Channel[Router.Rejected[(String, Array[Byte]), Doc]]()
    val src = Source(docs.map(d => ("x", d.getBytes("UTF-8"))): _*)
    val r = go(Kinds.run(src)(Kinds.swaps ~> swapsCh, Kinds.rejected ~> dead))
    assertEquals(go(swapsCh.drained.runCollect), Vector(Swap("s1", "EUR"), Swap("s2", "USD")))
    val why = go(dead.drained.runCollect).map(_.why)
    assertEquals(why.count(_ == "lane rates is not bound here"), 4)
    assertEquals(why.count(_ == "declined by every pattern"), 1)
    assertEquals(r, Router.Routed(Vector("Swap" -> 2, "rates" -> 0, "usdSwaps" -> 0), rejected = 5))
  }

  test("a pattern and a table serialize and come back working — what a distributed Bulk needs") {
    def roundTrip[T](t: T): T = {
      val bytes = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytes); out.writeObject(t); out.close()
      val loader = getClass.getClassLoader
      val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray)) {
        override def resolveClass(d: ObjectStreamClass): Class[_] = Class.forName(d.getName, false, loader)
      }
      // readObject answers AnyRef: the one unchecked step, and T is what was written above
      in.readObject().asInstanceOf[T]
    }
    import Refine.json._
    val money = (field("amount") >>> num) and (field("currency") >>> str)
    assertEquals(roundTrip(money).run(okay2.codec.Json.parse("""{"amount": 5, "currency": "EUR"}""")).toOption, Some((5.0, "EUR")))
    assert(roundTrip(Kinds) eq Kinds, "a table object comes back as itself")
    assertEquals(roundTrip(Kinds.swaps).project(Swap("s", "EUR")), Some(Swap("s", "EUR")))
    assertEquals(roundTrip(Kinds.swaps).project(Cds("c", "EUR")), None)
  }

  test("ONE table, every carrier: a Vector, a Bulk collection, a Source stream — the same lanes, rejects and counts") {
    implicit val B: Bulk[Chunks] = localBulk
    val inputs = docs.map(d => (d, d.getBytes("UTF-8")))
    val v = Kinds.split(inputs)
    val swapsV: Vector[Swap] = v(Kinds.swaps)
    val c = Kinds.split(B.of(inputs))
    val s = Kinds.split(Source(inputs: _*))
    val routed = go(s.counts)
    val swapsS: Vector[Swap] = go(s(Kinds.swaps).runCollect)
    assertEquals(swapsV, Vector(Swap("s1", "EUR"), Swap("s2", "USD")))
    assertEquals(c(Kinds.swaps).elements.toVector, swapsV)
    assertEquals(swapsS, swapsV)
    assertEquals(go(s(Kinds.rates).runCollect), v(Kinds.rates))
    assertEquals(go(s.rejected.runCollect).map(_.input._1), v.rejected.map(_.input._1))
    assertEquals(routed, v.counts)
    assertEquals(c.counts, v.counts)
    assertEquals(v.counts, Router.Routed(Vector("Swap" -> 2, "rates" -> 4, "usdSwaps" -> 0), rejected = 1))
  }

  test("a bounded stream: the readers run WITH the driver, the slowest paces the source, nothing lost") {
    val inputs = (0 until 300).map(i => (s"d$i", (if (i % 3 == 0) s"swap:s$i,EUR" else if (i % 3 == 1) s"fx:f$i,EURUSD" else "junk").getBytes("UTF-8")))
    val s = Kinds.split(Source(inputs: _*))(Routable.stream[(String, Array[Byte])](capacity = 4))
    val ((routed, swaps), (rates, rejected)) = go(Async.par(
      Async.par(s.counts, s(Kinds.swaps).runCollect),
      Async.par(s(Kinds.rates).runCollect, s.rejected.runCollect)))
    assertEquals((swaps.length, rates.length, rejected.length), (100, 100, 100))
    assertEquals(routed.rejected, 100)
  }

  test("a stream lane is read ONCE: the second run is refused by name, not left to split the channel") {
    val inputs = docs.map(d => (d, d.getBytes("UTF-8")))
    val s = Kinds.split(Source(inputs: _*))
    val lane = s(Kinds.swaps)
    assertEquals(go(s.counts).rejected, 1)
    assertEquals(go(lane.runCollect), Vector(Swap("s1", "EUR"), Swap("s2", "USD")))
    val again = intercept[IllegalStateException](go(lane.runCollect))
    assert(again.getMessage.contains("read ONCE"), again.getMessage)
    assert(intercept[IllegalStateException](go(s.rejected.runCollect.flatMap(_ => s.rejected.runCollect))).getMessage.contains("the rejects"))
  }

  test("a Vector's lanes are grouped once; release is harmless where nothing is pinned") {
    val v = Kinds.split(docs.map(d => (d, d.getBytes("UTF-8"))))
    assertEquals(v(Kinds.swaps), v(Kinds.swaps))
    v.release()
    assertEquals(v(Kinds.rates).length, 4)
  }
}
