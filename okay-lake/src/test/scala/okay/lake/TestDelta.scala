package okay.lake

import okay.given
import okay.codec.Schema
import okay.cluster.Flows
import java.nio.file.{Files, Path}

final case class Visit(id: Long, city: String) derives Schema

/**
 * A DELTA TABLE AS A SOURCE (specs/dataflow.md, stage 18), written by
 * ANOTHER engine: delta-rs, through pyarrow's `deltalake` — a python with
 * it (`OKAY_PYARROW_PYTHON`, else python3 when it has it); skips without.
 * The hand-written logs below need nothing.
 */
class TestDelta extends munit.FunSuite:

  lazy val python: Option[String] =
    sys.env.get("OKAY_PYARROW_PYTHON").orElse(Some("python3")).filter { py =>
      scala.util.Try(ProcessBuilder(py, "-c", "import deltalake").start().waitFor() == 0).getOrElse(false)
    }

  def py(script: String, args: String*): String =
    val p = ProcessBuilder((Seq(python.get, "-c", script) ++ args)*).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    if p.waitFor() != 0 then throw IllegalStateException(s"python: $said")
    said.trim

  def lakeAt(dir: Path): String =
    val name = s"delta-${System.nanoTime()}"
    Lakes.register(name, okay.blob.Fs(dir))
    name

  test("delta-rs writes — appends, a DELETE that removes files, a checkpoint, commits after it — and ours reads the same rows") {
    assume(python.isDefined, "no python with deltalake")
    val dir = Files.createTempDirectory("okay-delta")
    val said = py("""
import sys, json, pyarrow as pa
from deltalake import write_deltalake, DeltaTable
d = sys.argv[1] + "/visits"
t = lambda lo, hi: pa.table({"id": pa.array(range(lo, hi), pa.int64()), "city": pa.array(["c%d" % (i % 3) for i in range(lo, hi)])})
write_deltalake(d, t(0, 1000), partition_by=["city"])
write_deltalake(d, t(1000, 2000), mode="append", partition_by=["city"])
DeltaTable(d).delete("id < 500")
DeltaTable(d).create_checkpoint()
write_deltalake(d, t(2000, 2500), mode="append", partition_by=["city"])
DeltaTable(d).delete("id >= 2400")
dt = DeltaTable(d)
removed = set()
import glob
for f in sorted(glob.glob(d + "/_delta_log/*.json")):
  for line in open(f):
    a = json.loads(line)
    if "remove" in a: removed.add(a["remove"]["path"])
ids = sorted(dt.to_pyarrow_table().column("id").to_pylist())
print(json.dumps({"version": dt.version(), "rows": len(ids), "sum": sum(ids), "removed": sorted(removed)}))
""", dir.toString)
    val expect = okay.codec.Json.parse(said)
    def f(k: String) = expect match { case okay.codec.Json.JObj(fs) => fs.collectFirst { case (`k`, v) => v }.get; case _ => fail(said) }
    val lake = lakeAt(dir)
    val snap = DeltaSource.snapshot(lake, "visits")
    f("version") match { case okay.codec.Json.JNum(v) => assertEquals(snap.version, v.toLong); case _ => fail(said) }
    val removed = f("removed") match { case okay.codec.Json.JArr(vs) => vs.collect { case okay.codec.Json.JStr(s) => s }; case _ => Vector() }
    assert(removed.nonEmpty, "the DELETEs removed no file — the test tests nothing")
    val plan = DeltaSource.plan(lake, "visits")
    assert(!plan.parts.exists(p => removed.exists(r => p.key.endsWith(r))), "a removed file is in the plan")
    val rows = Flows.collect(ParquetSource.flow[Visit](plan)).runWith
    f("rows") match { case okay.codec.Json.JNum(n) => assertEquals(rows.length.toLong, n.toLong); case _ => fail(said) }
    f("sum") match { case okay.codec.Json.JNum(n) => assertEquals(rows.map(_.id).sum, n.toLong); case _ => fail(said) }
    assert(rows.forall(v => v.city == s"c${v.id % 3}"), "a partition value is not the row's")
  }

  def handWritten(protocol: String, add: String): String =
    val dir = Files.createTempDirectory("okay-delta-hand")
    Files.createDirectories(dir.resolve("t/_delta_log"))
    Files.writeString(dir.resolve("t/_delta_log/00000000000000000000.json"),
      protocol + "\n" +
        """{"metaData":{"id":"x","format":{"provider":"parquet","options":{}},"schemaString":"{\"type\":\"struct\",\"fields\":[{\"name\":\"id\",\"type\":\"long\",\"nullable\":true,\"metadata\":{}}]}","partitionColumns":[],"configuration":{}}}""" + "\n" +
        add + "\n"): Unit
    lakeAt(dir)

  test("a deletion vector is refused by name, never read past") {
    val lake = handWritten("""{"protocol":{"minReaderVersion":3,"minWriterVersion":7,"readerFeatures":["deletionVectors"],"writerFeatures":["deletionVectors"]}}""",
      """{"add":{"path":"a.parquet","partitionValues":{},"size":1,"modificationTime":0,"dataChange":true,"deletionVector":{"storageType":"u","pathOrInlineDv":"x","offset":1,"sizeInBytes":1,"cardinality":1}}}""")
    val e = intercept[IllegalStateException](DeltaSource.snapshot(lake, "t"))
    assert(e.getMessage.contains("deletionVectors"), e.getMessage)
  }

  test("column mapping is refused by name") {
    val lake = handWritten("""{"protocol":{"minReaderVersion":2,"minWriterVersion":5}}""",
      """{"add":{"path":"a.parquet","partitionValues":{},"size":1,"modificationTime":0,"dataChange":true}}""")
    val e = intercept[IllegalStateException](DeltaSource.snapshot(lake, "t"))
    assert(e.getMessage.contains("column mapping"), e.getMessage)
  }

  test("a gap in the commits is refused, not read around") {
    val lake = handWritten("""{"protocol":{"minReaderVersion":1,"minWriterVersion":2}}""",
      """{"add":{"path":"a.parquet","partitionValues":{},"size":1,"modificationTime":0,"dataChange":true}}""")
    val root = Lakes(lake)
    Run(root.putBytes("t/_delta_log/00000000000000000002.json", """{"remove":{"path":"a.parquet"}}""".getBytes)): Unit
    val e = intercept[IllegalStateException](DeltaSource.snapshot(lake, "t"))
    assert(e.getMessage.contains("missing"), e.getMessage)
  }
