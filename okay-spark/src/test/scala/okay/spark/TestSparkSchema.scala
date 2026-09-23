package okay.spark

import okay.codec.Schema
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types.*

/** specs/scalus.md §4, on a real SparkSession: every encoding queried */
class TestSparkSchema extends munit.FunSuite:

  override def munitIgnore: Boolean = Runtime.version().feature() == 24   // see TestSparkInterop

  lazy val spark = SparkSession.builder()
    .master("local[2]").appName("okay-spark-schema")
    .config("spark.ui.enabled", "false")
    .getOrCreate()
  override def afterAll(): Unit = spark.stop()

  enum Tag derives Schema:
    case Spend, Mint, Cert

  enum Cred derives Schema:
    case KeyHash(hash: Array[Byte])
    case ScriptHash(hash: Array[Byte])

  enum DRep derives Schema:
    case Key(hash: Array[Byte])
    case AlwaysAbstain
    case AlwaysNoConfidence

  final case class Out(id: Int, tag: Tag, cred: Cred, drep: DRep, note: Option[String],
                       qty: BigInt, parts: Vector[Long]) derives Schema

  private val outs = Seq(
    Out(1, Tag.Spend, Cred.KeyHash(Array[Byte](1, 2)), DRep.AlwaysAbstain, None, (BigInt(1) << 64) - 1, Vector(1L, 2L)),
    Out(2, Tag.Cert, Cred.ScriptHash(Array[Byte](9)), DRep.Key(Array[Byte](7)), Some("x"), BigInt(5), Vector.empty))

  test("a pure enum is a string of the case NAME") {
    val t = SparkSchema.structOf[Out]("tag")
    assertEquals(t.dataType, StringType)
    val names = SparkSchema.dataFrame(spark, outs).select("tag").collect().map(_.getString(0)).toList
    assertEquals(names, List("Spend", "Cert"))
  }

  test("a sum with payloads is kind + one nullable struct per case WITH fields; exactly one is set") {
    val st = SparkSchema.structOf[Out]
    val drep = st("drep").dataType.asInstanceOf[StructType]
    assertEquals(drep.fieldNames.toList, List("kind", "Key"))      // the singletons have no branch
    val df = SparkSchema.dataFrame(spark, outs)
    df.createOrReplaceTempView("outs")
    val rows = spark.sql("SELECT id, cred.kind, cred.KeyHash IS NULL, cred.ScriptHash IS NULL, drep.kind FROM outs ORDER BY id").collect()
    assertEquals(rows(0).toSeq.toList, List(1, "KeyHash", false, true, "AlwaysAbstain"))
    assertEquals(rows(1).toSeq.toList, List(2, "ScriptHash", true, false, "Key"))
    val hash = spark.sql("SELECT cred.ScriptHash.hash FROM outs WHERE cred.kind = 'ScriptHash'").collect()
    assertEquals(hash.map(_.getAs[Array[Byte]](0).toList).toList, List(List[Byte](9)))
  }

  test("Option is nullable, Vector an array, BigInt a decimal(38,0) that holds a uint64") {
    val st = SparkSchema.structOf[Out]
    assert(st("note").nullable)
    assertEquals(st("parts").dataType, ArrayType(LongType, false))
    assertEquals(st("qty").dataType, DecimalType(38, 0))
    val q = SparkSchema.dataFrame(spark, outs).select("qty").collect().head.getDecimal(0)
    assertEquals(BigInt(q.toBigInteger), (BigInt(1) << 64) - 1)
  }

  final case class Tree(label: String, kids: Vector[Tree])
  given Schema[Tree] = Schema.derived
  final case class Doc(name: String, tree: Tree)
  given Schema[Doc] = Schema.derived

  test("a recursive type is struct<cbor, json: variant>, found by reachability, queryable with variant_get") {
    assertEquals(SparkSchema.recursiveNames(implicitly[Schema[Doc]]), Set("Tree"))
    val st = SparkSchema.structOf[Doc]
    assertEquals(st("tree").dataType.asInstanceOf[StructType].fieldNames.toList, List("cbor", "json"))
    val d = Doc("d", Tree("root", Vector(Tree("a", Vector.empty), Tree("b", Vector(Tree("c", Vector.empty))))))
    val df = SparkSchema.dataFrame(spark, Seq(d))
    df.createOrReplaceTempView("docs")
    val got = spark.sql("SELECT variant_get(tree.json, '$.kids[1].kids[0].label', 'string') FROM docs").collect().head.getString(0)
    assertEquals(got, "c")
    val bytes = spark.sql("SELECT tree.cbor FROM docs").collect().head.getAs[Array[Byte]](0)
    assertEquals(okay.codec.Cbor.read[Tree](bytes), Right(d.tree))
  }

  final case class A(b: Option[B])
  final case class B(a: Vector[A], n: Int)
  given Schema[A] = Schema.derived
  given Schema[B] = Schema.derived

  test("mutual recursion: both nodes are recursive, from either root") {
    assertEquals(SparkSchema.recursiveNames(implicitly[Schema[A]]), Set("A", "B"))
    assertEquals(SparkSchema.recursiveNames(implicitly[Schema[B]]), Set("A", "B"))
    assertEquals(SparkSchema.structOf[A].fieldNames.toList, List("cbor", "json"))
  }

  test("every table writes to Parquet and reads back (no empty struct anywhere)") {
    val dir = java.nio.file.Files.createTempDirectory("okay-spark-schema").toString
    SparkSchema.dataFrame(spark, outs).write.mode("overwrite").parquet(dir + "/outs")
    assertEquals(spark.read.parquet(dir + "/outs").count(), 2L)
  }

  enum Cred2 derives Schema:
    case KeyHash(hash: Array[Byte])
    case ScriptHash(hash: Array[Byte])
    case Pointer(slot: Long)                       // a new era's case
  final case class V1(id: Int, cred: Cred) derives Schema
  final case class V2(id: Int, cred: Cred2) derives Schema

  test("a new case is a new nullable column: old Parquet files read under the new schema") {
    val dir = java.nio.file.Files.createTempDirectory("okay-spark-evolve").toString
    SparkSchema.dataFrame(spark, Seq(V1(1, Cred.KeyHash(Array[Byte](1))))).write.parquet(dir + "/p=1")
    SparkSchema.dataFrame(spark, Seq(V2(2, Cred2.Pointer(42L)))).write.parquet(dir + "/p=2")
    val df = spark.read.option("mergeSchema", "true").parquet(dir)
    df.createOrReplaceTempView("evolved")
    val rows = spark.sql("SELECT id, cred.kind, cred.Pointer.slot FROM evolved ORDER BY id").collect().map(_.toSeq.toList).toList
    assertEquals(rows, List(List(1, "KeyHash", null), List(2, "Pointer", 42L)))
  }

  final case class Big(n: BigInt) derives Schema
  test("a BigInt past 38 digits is refused with the reason, not rounded") {
    val e = intercept[IllegalArgumentException](SparkSchema.rows(Seq(Big(BigInt(10).pow(40)))))
    assert(e.getMessage.contains("38 digits"), e.getMessage)
  }
