package okay.spark

import java.nio.file.Files
import okay.{Bulk, Chunks, localBulk}
import okay.Chunks.elements
import okay.refine.{Documents, Refine, Router, Routes}
import okay.spark.SparkBulk.Rows
import org.apache.spark.sql.SparkSession

/** a table is an `object`, so a task that lands on an executor finds the
 * same table there by reference */
object SparkRoutesFixtures:
  sealed trait Doc
  final case class Swap(id: String, ccy: String) extends Doc
  final case class Fx(id: String, pair: String) extends Doc
  final case class Cds(id: String, ccy: String) extends Doc

  def kind(prefix: String): Refine[String, Vector[String]] =
    Refine.step[String, Vector[String]](prefix)(s =>
      if s.startsWith(prefix + ":") then Right(s.drop(prefix.length + 1).trim.split(',').toVector) else Left(s"not a $prefix"))(p => prefix + ":" + p.mkString(","))
  val doc: Refine[(String, Array[Byte]), Doc] =
    Refine.step[(String, Array[Byte]), String]("text")(f => Right(new String(f._2, "UTF-8")))(s => ("?", s.getBytes("UTF-8"))) >>>
      (kind("swap").map("swap")(p => Swap(p(0), p(1)), (s: Swap) => Vector(s.id, s.ccy)).widen[Doc] or
        kind("fx").map("fx")(p => Fx(p(0), p(1)), (f: Fx) => Vector(f.id, f.pair)).widen[Doc] or
        kind("cds").map("cds")(p => Cds(p(0), p(1)), (c: Cds) => Vector(c.id, c.ccy)).widen[Doc])

  object Kinds extends Routes(doc):
    val swaps = route[Swap]
    val rates = route[Fx | Cds]

/** specs/refine.md, refine-bulk: the SAME routing table, over Spark and over one JVM, answers the same */
class TestSparkRoutes extends munit.FunSuite:
  import SparkRoutesFixtures.*
  override def munitTimeout = scala.concurrent.duration.Duration(5, "min")

  lazy val spark = SparkSession.builder().master("local[4]").appName("routes").config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  /** 400 documents, a quarter of them not documents at all */
  lazy val folder: String =
    val dir = Files.createTempDirectory("okay-spark-routes")
    for i <- 0 until 400 do
      val text = i % 4 match
        case 0 => s"swap:s$i,${if i % 8 == 0 then "EUR" else "USD"}"
        case 1 => s"fx:f$i,EURUSD"
        case 2 => s"cds:c$i,EUR"
        case _ => s"letter $i"
      Files.writeString(dir.resolve(f"doc$i%04d.txt"), text): Unit
    dir.toString

  test("split on SparkBulk equals split in one JVM: every lane, the rejects, the counts") {
    val onSpark = SparkBulk(spark)
    val local: Bulk[Chunks] = localBulk
    val s = { given Bulk[Rows] = onSpark; Kinds.split(onSpark.read(folder, Documents.files)) }
    val l = { given Bulk[Chunks] = local; Kinds.split(local.read(folder, Documents.files)) }
    def all[X](d: Chunks[X]): Vector[X] = d.elements.toVector
    def fromSpark[X](d: Rows[X]): Vector[X] = all(onSpark.toChunks(d))
    assertEquals(fromSpark(s(Kinds.swaps)).sortBy(_.id), all(l(Kinds.swaps)).sortBy(_.id))
    assertEquals(fromSpark(s(Kinds.rates)).length, 200)
    assertEquals(fromSpark(s(Kinds.rates)).map(_.toString).sorted, all(l(Kinds.rates)).map(_.toString).sorted)
    assertEquals(fromSpark(s.rejected).map(_.input._1).sorted, all(l.rejected).map(_.input._1).sorted)
    assertEquals(s.counts, Router.Routed(Vector("Swap" -> 100, "Fx | Cds" -> 200), rejected = 100))
    assertEquals(s.counts, l.counts)
  }
