package okay.spark

import okay.codec.Schema
import org.apache.spark.sql.SparkSession

/** the snippet in docs/modules/okay-spark.md ("ADTs as DataFrames"), VERBATIM */
class TestDocExamplesSparkSchema extends munit.FunSuite:
  override def munitIgnore: Boolean = Runtime.version().feature() == 24
  lazy val spark = SparkSession.builder().master("local[1]").appName("doc")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  enum Credential derives Schema:
    case KeyHash(hash: Array[Byte])
    case ScriptHash(hash: Array[Byte])
  final case class Output(id: Int, owner: Credential, lovelace: BigInt) derives Schema

  test("docs: a sum as kind + branches, queried in SQL") {
    // ---- snippet begins
    val df = SparkSchema.dataFrame(spark, Seq(
      Output(1, Credential.KeyHash(Array[Byte](1)), BigInt(2_000_000)),
      Output(2, Credential.ScriptHash(Array[Byte](2)), BigInt(5_000_000))))
    // owner: struct<kind: string, KeyHash: struct<hash: binary>, ScriptHash: struct<hash: binary>>
    df.createOrReplaceTempView("outputs")
    val scripts = spark.sql("SELECT id, lovelace FROM outputs WHERE owner.kind = 'ScriptHash'").collect()
    // Array([2,5000000])
    // ---- snippet ends
    assertEquals(scripts.map(r => (r.getInt(0), BigInt(r.getDecimal(1).toBigInteger))).toList, List((2, BigInt(5_000_000))))
    assertEquals(df.schema("owner").dataType.simpleString,
      "struct<kind:string,KeyHash:struct<hash:binary>,ScriptHash:struct<hash:binary>>")
  }
