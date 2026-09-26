package okay.lake

import okay.given
import okay.cluster.Flows
import okay.codec.Json
import java.nio.file.{Files, Path}

/**
 * AN ICEBERG TABLE AS A SOURCE (specs/dataflow.md, stage 18), written by
 * pyiceberg with a SQLite catalog — a python with it (`OKAY_PYARROW_PYTHON`,
 * else python3 when it has it); skips without one. And Avro read by ours
 * and by Apache Avro, the same records (specs/own-or-standard.md).
 */
class TestIceberg extends munit.FunSuite:

  lazy val python: Option[String] =
    sys.env.get("OKAY_PYARROW_PYTHON").orElse(Some("python3")).filter { py =>
      scala.util.Try(ProcessBuilder(py, "-c", "import pyiceberg.catalog.sql").start().waitFor() == 0).getOrElse(false)
    }

  def py(script: String, args: String*): String =
    val p = ProcessBuilder((Seq(python.get, "-c", script) ++ args)*).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    if p.waitFor() != 0 then throw IllegalStateException(s"python: $said")
    said.trim.linesIterator.toVector.last

  /** a table pyiceberg wrote: appends, a delete, an overwrite; answers the
   * lake's directory and what pyiceberg itself reads */
  lazy val table: (Path, Json) =
    val dir = Files.createTempDirectory("okay-iceberg").toRealPath()
    val said = py("""
import sys, json, pyarrow as pa
from pyiceberg.catalog.sql import SqlCatalog
from pyiceberg.expressions import LessThan
d = sys.argv[1]
cat = SqlCatalog("t", uri=f"sqlite:///{d}/cat.db", warehouse=f"file://{d}/wh")
cat.create_namespace("ns")
t = lambda lo, hi: pa.table({"id": pa.array(range(lo, hi), pa.int64()), "city": pa.array(["c%d" % (i % 3) for i in range(lo, hi)])})
tbl = cat.create_table("ns.visits", schema=t(0, 1).schema)
tbl.append(t(0, 1000))
tbl.append(t(1000, 2000))
tbl.append(t(2000, 2300))
tbl.delete(LessThan("id", 500))
tbl = cat.load_table("ns.visits")
# the delete is the LAST snapshot: its manifests carry DELETED entries,
# which is what the plan must skip (an append after it would drop them)
deleted_entries = sum(1 for m in tbl.current_snapshot().manifests(tbl.io)
                      for e in m.fetch_manifest_entry(tbl.io, discard_deleted=False) if e.status.value == 2)
current = {f.file_path for f in tbl.scan().plan_files() for f in [f.file]}
ever = set()
for s in tbl.metadata.snapshots:
  for m in s.manifests(tbl.io):
    for e in m.fetch_manifest_entry(tbl.io, discard_deleted=False):
      ever.add(e.data_file.file_path)
ids = sorted(tbl.scan().to_arrow().column("id").to_pylist())
print(json.dumps({"metadata": tbl.metadata_location, "rows": len(ids), "sum": sum(ids),
                  "removed": sorted(ever - current), "current": len(current), "deleted_entries": deleted_entries}))
""", dir.toString)
    (dir, Json.parse(said))

  def f(j: Json, k: String): Json = j match
    case Json.JObj(fs) => fs.collectFirst { case (`k`, v) => v }.getOrElse(Json.JNull)
    case _ => Json.JNull
  def n(j: Json): Long = j match { case Json.JNum(x) => x.toLong; case _ => -1L }
  def s(j: Json): String = j match { case Json.JStr(x) => x; case _ => "" }

  test("pyiceberg's table — appends, then a delete — read equal, a DELETED manifest entry not planned") {
    assume(python.isDefined, "no python with pyiceberg")
    val (dir, said) = table
    val lake = s"iceberg-${System.nanoTime()}"
    Lakes.register(lake, okay.blob.Fs(dir))
    val root = s"file://$dir"
    val metadata = s(f(said, "metadata")).stripPrefix(root + "/")
    val plan = IcebergSource.plan(lake, metadata, root)
    val removed = f(said, "removed") match { case Json.JArr(vs) => vs.map(s); case _ => Vector() }
    assert(removed.nonEmpty, "the delete removed no file — the test tests nothing")
    assert(n(f(said, "deleted_entries")) > 0, "no DELETED entry in the current snapshot — the status filter is not exercised")
    assert(!plan.parts.exists(p => removed.exists(_.endsWith(p.key))), "a removed file is in the plan")
    assertEquals(plan.parts.map(_.key).distinct.length.toLong, n(f(said, "current")))
    val rows = Flows.collect(ParquetSource.flow[Visit](plan)).runWith
    assertEquals(rows.length.toLong, n(f(said, "rows")))
    assertEquals(rows.map(_.id).sum, n(f(said, "sum")))
  }

  /** a record's values with arrays of bytes made comparable */
  def plain(v: Any): Any = v match
    case b: Array[Byte] => b.toVector
    case xs: Vector[?] => xs.map(plain)
    case (k, x) => (k, plain(x))
    case m: Map[?, ?] => m.map((k, x) => k -> plain(x))
    case other => other

  test("Avro: ours and Apache Avro read every manifest and manifest list the same") {
    assume(python.isDefined, "no python with pyiceberg")
    val (dir, _) = table
    val avros = Files.walk(dir).toArray.map(_.asInstanceOf[Path]).filter(_.toString.endsWith(".avro")).toVector
    assert(avros.length >= 4, s"${avros.length} Avro files")
    for p <- avros do
      val bytes = Files.readAllBytes(p)
      assertEquals(plain(OkayAvro.records(bytes)), plain(ApacheAvro.records(bytes)), p.toString)
    assertEquals(summon[AvroReader].name, "okay")
    locally {
      import ApacheAvro.given
      assertEquals(summon[AvroReader].name, "apache-avro")
    }
  }

  test("what is not an Avro container is refused by name") {
    val e = intercept[AvroRefused](OkayAvro.records("not avro".getBytes))
    assert(e.getMessage.contains("object container"), e.getMessage)
  }
