package okay2.spark

import java.nio.file.Files
import scala.jdk.CollectionConverters._
import okay2.refine.{Documents, Refine, Router, Routes}
import okay2.spark.SparkBulk.Rows
import okay2.stream.{Bulk, Chunks}
import okay2.stream.Chunks.ChunksOps
import org.apache.spark.sql.SparkSession

/** a table is an `object`: an executor finds it by reference */
object SparkRoutesFixtures {
  sealed trait Doc
  final case class Swap(id: String, ccy: String) extends Doc
  final case class Fx(id: String, pair: String) extends Doc
  final case class Cds(id: String, ccy: String) extends Doc

  def kind(prefix: String): Refine[String, Vector[String]] =
    Refine.step[String, Vector[String]](prefix)(s =>
      if (s.startsWith(prefix + ":")) Right(s.drop(prefix.length + 1).trim.split(',').toVector) else Left(s"not a $prefix"))(p => prefix + ":" + p.mkString(","))
  val doc: Refine[(String, Array[Byte]), Doc] =
    Refine.step[(String, Array[Byte]), String]("text")(f => Right(new String(f._2, "UTF-8")))(s => ("?", s.getBytes("UTF-8"))) >>>
      (kind("swap").map[Swap]("swap")(p => Swap(p(0), p(1)), (s: Swap) => Vector(s.id, s.ccy)).widen[Doc] or
        kind("fx").map[Fx]("fx")(p => Fx(p(0), p(1)), (f: Fx) => Vector(f.id, f.pair)).widen[Doc] or
        kind("cds").map[Cds]("cds")(p => Cds(p(0), p(1)), (c: Cds) => Vector(c.id, c.ccy)).widen[Doc])

  object Kinds extends Routes(doc) {
    val swaps = route[Swap]
    val rates = route("rates") { case d @ (_: Fx | _: Cds) => d }
  }
}

/** okay's refine-bulk on the Scala 2 core: the SAME routing table on Spark and in one JVM answers the same */
class TestSparkRoutes extends munit.FunSuite {
  import SparkRoutesFixtures._
  override def munitTimeout = scala.concurrent.duration.Duration(5, "min")

  lazy val spark = SparkSession.builder().master("local[4]").appName("okay2-routes").config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  lazy val folder: String = {
    val dir = Files.createTempDirectory("okay2-spark-routes")
    for (i <- 0 until 400) {
      val text = i % 4 match {
        case 0 => s"swap:s$i,${if (i % 8 == 0) "EUR" else "USD"}"
        case 1 => s"fx:f$i,EURUSD"
        case 2 => s"cds:c$i,EUR"
        case _ => s"letter $i"
      }
      val _ = Files.writeString(dir.resolve(f"doc$i%04d.txt"), text)
    }
    dir.toString
  }

  test("split on SparkBulk equals split in one JVM: every lane, the rejects, the counts") {
    val onSpark = SparkBulk(spark)
    val local: Bulk[Chunks] = Bulk.local(p => Files.lines(java.nio.file.Path.of(p)).iterator().asScala)
    val s = { implicit val B: Bulk[Rows] = onSpark; Kinds.split(Documents.files[Rows](folder)) }
    val l = { implicit val B: Bulk[Chunks] = local; Kinds.split(Documents.files[Chunks](folder)) }
    def fromSpark[X](d: Rows[X]): Vector[X] = onSpark.toChunks(d).elements.toVector
    assertEquals(fromSpark(s(Kinds.swaps)).sortBy(_.id), l(Kinds.swaps).elements.toVector.sortBy(_.id))
    assertEquals(fromSpark(s(Kinds.rates)).length, 200)
    assertEquals(fromSpark(s(Kinds.rates)).map(_.toString).sorted, l(Kinds.rates).elements.toVector.map(_.toString).sorted)
    assertEquals(fromSpark(s.rejected).map(_.input._1).sorted, l.rejected.elements.toVector.map(_.input._1).sorted)
    assertEquals(s.counts, Router.Routed(Vector("Swap" -> 100, "rates" -> 200), rejected = 100))
    assertEquals(s.counts, l.counts)
  }
}
