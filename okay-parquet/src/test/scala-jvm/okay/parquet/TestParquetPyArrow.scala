package okay.parquet

import java.nio.file.Files

/**
 * pyarrow AS A SECOND ORACLE (specs/parquet.md): a writer that is not
 * parquet-java — its data page v2, its dictionary pages — read by ours,
 * and ours read by it. Needs a python with pyarrow (`OKAY_PYARROW_PYTHON`,
 * else python3 when it has it); skips without one.
 */
class TestParquetPyArrow extends munit.FunSuite:
  import ParquetSamples.*

  lazy val python: Option[String] =
    sys.env.get("OKAY_PYARROW_PYTHON").orElse(Some("python3")).filter { py =>
      scala.util.Try(ProcessBuilder(py, "-c", "import pyarrow").start().waitFor() == 0).getOrElse(false)
    }

  def py(script: String, args: String*): String =
    val p = ProcessBuilder((Seq(python.get, "-c", script) ++ args)*).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    if p.waitFor() != 0 then throw IllegalStateException(s"python: $said")
    said.trim

  test("pyarrow writes — data page v2 and v1, dictionary and plain, snappy and zstd — ours reads the rows") {
    assume(python.isDefined, "no python with pyarrow")
    val dir = Files.createTempDirectory("okay-parquet-pyarrow")
    py("""
import sys, os, pyarrow as pa, pyarrow.parquet as pq
d = sys.argv[1]
n = 5000
t = pa.table({
  "id": pa.array(range(n), pa.int64()),
  "i32": pa.array([None if i % 7 == 3 else i * 3 for i in range(n)], pa.int32()),
  "x": pa.array([None if i % 5 == 0 else i / 3 for i in range(n)], pa.float64()),
  "city": pa.array([None if i % 11 == 0 else "city %d" % (i % 9) for i in range(n)], pa.string()),
  "ok": pa.array([i % 2 == 0 for i in range(n)], pa.bool_()),
})
for v in ["1.0", "2.0"]:
  for c in ["snappy", "zstd", "none"]:
    for dic in [True, False]:
      pq.write_table(t, os.path.join(d, "t-%s-%s-%s.parquet" % (v, c, dic)), data_page_version=v,
                     compression=c, use_dictionary=dic, row_group_size=1500)
""", dir.toString)
    val files = Files.list(dir).toArray.map(_.toString).sorted.toVector
    assertEquals(files.length, 12)
    for f <- files do
      val t = OkayParquet.read(ReadAt.of(Files.readAllBytes(java.nio.file.Path.of(f))))
      assertEquals(t.rows, 5000, f)
      assertEquals(t.cols.map(_._1), Vector("id", "i32", "x", "city", "ok"), f)
      val sums = t.cols.map { (name, c) => name -> (0 until c.length).count(c.validity) }.toMap
      assertEquals(sums, Map("id" -> 5000, "i32" -> (5000 - (0 until 5000).count(_ % 7 == 3)),
        "x" -> 4000, "city" -> (5000 - (0 until 5000).count(_ % 11 == 0)), "ok" -> 5000), f)
      t.cols(3)._2 match
        case okay.arrow.Column.Utf8(v, ok) => assertEquals(v(4), "city 4", f); assert(!ok(0), f)
        case other => fail(s"$f: city read as ${okay.arrow.Column.describe(other)}")
  }

  test("ours writes, pyarrow reads the same rows") {
    assume(python.isDefined, "no python with pyarrow")
    val t = sample(3000)
    val file = Files.createTempFile("okay-parquet", ".parquet")
    Files.write(file, OkayParquet.write(t, groupRows = 1000)): Unit
    val said = py("""
import sys, pyarrow.parquet as pq
t = pq.read_table(sys.argv[1])
f = pq.ParquetFile(sys.argv[1])
print(t.num_rows, f.num_row_groups, t.column("name").null_count, t.column("name")[1].as_py(), t.column("id")[2].as_py())
""", file.toString)
    assertEquals(said, s"3000 3 ${(0 until 3000).count(_ % 7 == 3)} row 1 — чай ${2L * 1000003L}")
  }
