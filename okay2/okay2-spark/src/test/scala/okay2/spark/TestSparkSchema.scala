package okay2.spark

import okay2.codec.Schema
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._

sealed trait STag
object STag {
  case object Spend extends STag
  case object Mint extends STag
  case object Cert extends STag
}
sealed trait SCred
object SCred {
  final case class KeyHash(hash: Array[Byte]) extends SCred
  final case class ScriptHash(hash: Array[Byte]) extends SCred
}
sealed trait SDRep
object SDRep {
  final case class Key(hash: Array[Byte]) extends SDRep
  case object AlwaysAbstain extends SDRep
  case object AlwaysNoConfidence extends SDRep
}
final case class SOut(id: Int, tag: STag, cred: SCred, drep: SDRep, note: Option[String],
                      qty: BigInt, parts: Vector[Long])

final case class SchemaTree(label: String, kids: Vector[SchemaTree])
object SchemaTree {
  implicit lazy val schema: Schema[SchemaTree] = Schema.derived
}
final case class SchemaDoc(name: String, tree: SchemaTree)
object SchemaDoc {
  implicit lazy val schema: Schema[SchemaDoc] = Schema.derived
}

final case class SchemaA(b: Option[SchemaB])
final case class SchemaB(a: Vector[SchemaA], n: Int)
object SchemaA {
  implicit lazy val schema: Schema[SchemaA] = Schema.derived
}
object SchemaB {
  implicit lazy val schema: Schema[SchemaB] = Schema.derived
}

sealed trait SCred2
object SCred2 {
  final case class KeyHash(hash: Array[Byte]) extends SCred2
  final case class ScriptHash(hash: Array[Byte]) extends SCred2
  final case class Pointer(slot: Long) extends SCred2 // a new era's case
}
final case class SV1(id: Int, cred: SCred)
final case class SV2(id: Int, cred: SCred2)

final case class SchemaBig(n: BigInt)

/**
 * specs/scalus.md §4, on a real SparkSession: every encoding queried.
 * The recursive-type case differs from the Scala 3 core's own
 * TestSparkSchema: `okay2-codec` has not ported `Cbor`, so a recursive
 * type here is `struct<json: variant>` alone, not `struct<cbor, json>`
 * (`Columns.scala`'s own comment) — the `cbor`-column assertions and
 * the CBOR round-trip are dropped, everything else ported as-is.
 */
class TestSparkSchema extends munit.FunSuite {

  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(3, "min")

  lazy val spark = SparkSession.builder()
    .master("local[2]").appName("okay2-spark-schema")
    .config("spark.ui.enabled", "false")
    .getOrCreate()
  override def afterAll(): Unit = spark.stop()

  private val outs = Seq(
    SOut(1, STag.Spend, SCred.KeyHash(Array[Byte](1, 2)), SDRep.AlwaysAbstain, None, (BigInt(1) << 64) - 1, Vector(1L, 2L)),
    SOut(2, STag.Cert, SCred.ScriptHash(Array[Byte](9)), SDRep.Key(Array[Byte](7)), Some("x"), BigInt(5), Vector.empty))

  test("a pure enum is a string of the case NAME") {
    // a plain paren list right after a method whose only parameter list
    // is implicit fills THAT list in Scala 2 (Scala 3's `using` needs the
    // keyword to do the same) — split so `("tag")` calls StructType.apply
    val st = SparkSchema.structOf[SOut]
    val t = st("tag")
    assertEquals(t.dataType, StringType)
    val names = SparkSchema.dataFrame(spark, outs).select("tag").collect().map(_.getString(0)).toList
    assertEquals(names, List("Spend", "Cert"))
  }

  test("a sum with payloads is kind + one nullable struct per case WITH fields; exactly one is set") {
    val st = SparkSchema.structOf[SOut]
    val drep = st("drep").dataType.asInstanceOf[StructType]
    assertEquals(drep.fieldNames.toList, List("kind", "Key")) // the singletons have no branch
    val df = SparkSchema.dataFrame(spark, outs)
    df.createOrReplaceTempView("outs")
    val rows = spark.sql("SELECT id, cred.kind, cred.KeyHash IS NULL, cred.ScriptHash IS NULL, drep.kind FROM outs ORDER BY id").collect()
    assertEquals(rows(0).toSeq.toList, List[Any](1, "KeyHash", false, true, "AlwaysAbstain"))
    assertEquals(rows(1).toSeq.toList, List[Any](2, "ScriptHash", true, false, "Key"))
    val hash = spark.sql("SELECT cred.ScriptHash.hash FROM outs WHERE cred.kind = 'ScriptHash'").collect()
    assertEquals(hash.map(_.getAs[Array[Byte]](0).toList).toList, List(List[Byte](9)))
  }

  test("Option is nullable, Vector an array, BigInt a decimal(38,0) that holds a uint64") {
    val st = SparkSchema.structOf[SOut]
    assert(st("note").nullable)
    assertEquals(st("parts").dataType, ArrayType(LongType, false))
    assertEquals(st("qty").dataType, DecimalType(38, 0))
    val q = SparkSchema.dataFrame(spark, outs).select("qty").collect().head.getDecimal(0)
    assertEquals(BigInt(q.toBigInteger), (BigInt(1) << 64) - 1)
  }

  test("a recursive type is struct<json: variant>, found by reachability, queryable with variant_get") {
    assertEquals(SparkSchema.recursiveNames(implicitly[Schema[SchemaDoc]]), Set("SchemaTree"))
    val st = SparkSchema.structOf[SchemaDoc]
    assertEquals(st("tree").dataType.asInstanceOf[StructType].fieldNames.toList, List("json"))
    val d = SchemaDoc("d", SchemaTree("root", Vector(SchemaTree("a", Vector.empty), SchemaTree("b", Vector(SchemaTree("c", Vector.empty))))))
    val df = SparkSchema.dataFrame(spark, Seq(d))
    df.createOrReplaceTempView("docs")
    val got = spark.sql("SELECT variant_get(tree.json, '$.kids[1].kids[0].label', 'string') FROM docs").collect().head.getString(0)
    assertEquals(got, "c")
  }

  test("mutual recursion: both nodes are recursive, from either root") {
    assertEquals(SparkSchema.recursiveNames(implicitly[Schema[SchemaA]]), Set("SchemaA", "SchemaB"))
    assertEquals(SparkSchema.recursiveNames(implicitly[Schema[SchemaB]]), Set("SchemaA", "SchemaB"))
    assertEquals(SparkSchema.structOf[SchemaA].fieldNames.toList, List("json"))
  }

  test("every table writes to Parquet and reads back (no empty struct anywhere)") {
    val dir = java.nio.file.Files.createTempDirectory("okay2-spark-schema").toString
    SparkSchema.dataFrame(spark, outs).write.mode("overwrite").parquet(dir + "/outs")
    assertEquals(spark.read.parquet(dir + "/outs").count(), 2L)
  }

  test("a new case is a new nullable column: old Parquet files read under the new schema") {
    val dir = java.nio.file.Files.createTempDirectory("okay2-spark-evolve").toString
    SparkSchema.dataFrame(spark, Seq(SV1(1, SCred.KeyHash(Array[Byte](1))))).write.parquet(dir + "/p=1")
    SparkSchema.dataFrame(spark, Seq(SV2(2, SCred2.Pointer(42L)))).write.parquet(dir + "/p=2")
    val df = spark.read.option("mergeSchema", "true").parquet(dir)
    df.createOrReplaceTempView("evolved")
    val rows = spark.sql("SELECT id, cred.kind, cred.Pointer.slot FROM evolved ORDER BY id").collect().map(_.toSeq.toList).toList
    assertEquals(rows, List(List[Any](1, "KeyHash", null), List[Any](2, "Pointer", 42L)))
  }

  test("a BigInt past 38 digits is refused with the reason, not rounded") {
    val e = intercept[IllegalArgumentException](SparkSchema.rows(Seq(SchemaBig(BigInt(10).pow(40)))))
    assert(e.getMessage.contains("38 digits"), e.getMessage)
  }
}
