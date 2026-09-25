package okay.arrow

/**
 * pyarrow as the independent oracle for every type (stage 4). A python
 * with pyarrow: `OKAY_PYARROW_PYTHON`, else `python3` when it has it;
 * Live-tagged, skipped without one.
 */
class TestPyArrowOracle extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private lazy val python: Option[String] =
    sys.env.get("OKAY_PYARROW_PYTHON").orElse(Some("python3")).filter { py =>
      scala.util.Try(ProcessBuilder(py, "-c", "import pyarrow").start().waitFor() == 0).getOrElse(false)
    }
  override def munitIgnore: Boolean = python.isEmpty

  /** run a script with a stream file as argv[1] (read from, or written to) */
  private def run(script: String, file: java.nio.file.Path): String =
    val p = ProcessBuilder(python.get, "-c", script, file.toString).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8).trim
    if p.waitFor() != 0 then fail(s"python: $out")
    out

  private def file(bytes: Array[Byte] = Array.emptyByteArray) =
    val f = java.nio.file.Files.createTempFile("okay-arrow", ".arrows")
    java.nio.file.Files.write(f, bytes)

  test("pyarrow validates every type OkayArrow writes, names them, and its own writer's stream reads back the same") {
    val f = file(OkayArrow.write(Tables.everything))
    val types = run("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
t = ipc.open_stream(open(sys.argv[1], "rb").read()).read_all()
t.validate(full=True)
with ipc.new_stream(sys.argv[1], t.schema) as w: w.write_table(t)
print("|".join(str(f.type) for f in t.schema))
""", f)
    assertEquals(types.split('|').toVector, Vector("int64", "double", "string", "bool", "null", "int8", "uint8", "int16",
      "uint16", "int32", "uint32", "uint64", "float", "binary", "fixed_size_binary[2]", "decimal128(10, 2)",
      "decimal128(38, 0)", "date32[day]", "date64[ms]", "timestamp[us, tz=Europe/Kyiv]", "timestamp[ns]",
      "duration[ms]", "list<item: string>", "list<item: list<item: int64>>",
      "struct<name: string, age: int32, tags: list<item: int64>>"))
    assertEquals(Tables.same(Tables.everything, OkayArrow.read(java.nio.file.Files.readAllBytes(f))), None)
  }

  test("OkayArrow reads what only pyarrow makes: large forms, dictionaries, float16, several batches, slices") {
    val f = file()
    val _ = run("""
import sys, struct, pyarrow as pa, pyarrow.ipc as ipc
big = pa.table({
  "ls": pa.array(["a", None, "ü", "z"], pa.large_string()),
  "lb": pa.array([b"x", b"", None, b"yz"], pa.large_binary()),
  "ll": pa.array([[1], [], None, [2, 3]], pa.large_list(pa.int64())),
  "dict": pa.array(["kyiv", "lviv", None, "kyiv"]).dictionary_encode(),
  # float16 from its raw bytes: no numpy needed
  "half": pa.Array.from_buffers(pa.float16(), 4, [None, pa.py_buffer(struct.pack("<4e", 1.5, -2.0, 0.0, 65504.0))]),
})
# sliced: offsets that do not start at 0 in the batch pyarrow writes
sl = pa.table({"s": pa.array(["drop", "k1", "k2", "k3", "k4"]).slice(1),
               "l": pa.array([[0], [1, 2], [3], [], [4]]).slice(1)})
with ipc.new_stream(sys.argv[1], big.schema) as w:
    w.write_table(big.slice(0, 2)); w.write_table(big.slice(2))
open(sys.argv[1] + ".sliced", "wb").close()
with ipc.new_stream(sys.argv[1] + ".sliced", sl.schema) as w: w.write_table(sl)
""", f)
    val t = OkayArrow.read(java.nio.file.Files.readAllBytes(f))
    assertEquals(t.cols.map((n, c) => n -> Tables.cells(c)), Vector(
      "ls" -> Vector(Some("a"), None, Some("ü"), Some("z")),
      "lb" -> Vector(Some(Vector[Byte]('x')), Some(Vector.empty[Byte]), None, Some(Vector[Byte]('y', 'z'))),
      "ll" -> Vector(Some(Vector(Some(1L))), Some(Vector()), None, Some(Vector(Some(2L), Some(3L)))),
      "dict" -> Vector(Some("kyiv"), Some("lviv"), None, Some("kyiv")),
      "half" -> Vector(1.5f, -2.0f, 0.0f, 65504.0f).map(x => Some(java.lang.Float.floatToRawIntBits(x)))))
    val sliced = OkayArrow.read(java.nio.file.Files.readAllBytes(java.nio.file.Path.of(f.toString + ".sliced")))
    assertEquals(sliced.cols.map((n, c) => n -> Tables.cells(c)), Vector(
      "s" -> Vector(Some("k1"), Some("k2"), Some("k3"), Some("k4")),
      "l" -> Vector(Some(Vector(Some(1L), Some(2L))), Some(Vector(Some(3L))), Some(Vector()), Some(Vector(Some(4L))))))
  }

  test("a type outside the model is refused by name") {
    val f = file()
    val _ = run("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
t = pa.table({"m": pa.array([{"a": 1}], pa.map_(pa.string(), pa.int64()))})
with ipc.new_stream(sys.argv[1], t.schema) as w: w.write_table(t)
""", f)
    val e = intercept[IllegalStateException](OkayArrow.read(java.nio.file.Files.readAllBytes(f)))
    assert(e.getMessage.contains("column 'm' has Arrow type Map; the model does not hold it"), e.getMessage)
  }

  test("typed rows (Rows, through okay-codec's Schema) are ordinary Arrow to pyarrow: structs, lists, names") {
    import RowsModel.*
    val f = file(OkayArrow.encode(orders.take(1)))
    val out = run("""
import sys, json, pyarrow.ipc as ipc
t = ipc.open_stream(open(sys.argv[1], "rb").read()).read_all()
t.validate(full=True)
r = t.to_pylist()[0]
print(json.dumps({k: r[k] for k in ["sku", "qty", "tags", "lines", "colour", "shape", "total"]}, default=str, ensure_ascii=False))
""", f)
    assertEquals(out, """{"sku": "tea", "qty": 3, "tags": ["hot", "green"], "lines": [{"product": "cup", "n": 2}], """ +
      """"colour": "Red", "shape": {"kind": "Circle", "Circle": {"r": 1.5}, "Square": null}, "total": "123456789012345678901234567890"}""")
  }

  test("compressed IPC both ways: pyarrow's LZ4 and ZSTD bodies read here; ours validate in pyarrow") {
    for codec <- Vector("lz4", "zstd") do
      val f = file()
      val _ = run(s"""
import sys, pyarrow as pa, pyarrow.ipc as ipc
t = pa.table({"id": pa.array(range(5000), pa.int64()), "city": pa.array(["kyiv", "lviv", None, "odesa"] * 1250),
              "score": pa.array([i * 0.5 for i in range(5000)])})
opts = ipc.IpcWriteOptions(compression="$codec")
with ipc.new_stream(sys.argv[1], t.schema, options=opts) as w: w.write_table(t)
""", f)
      val t = OkayArrow.read(java.nio.file.Files.readAllBytes(f))
      assertEquals(t.rows, 5000, codec)
      assertEquals(Tables.cells(t.cols(1)._2).take(4), Vector(Some("kyiv"), Some("lviv"), None, Some("odesa")), codec)
    for codec <- Vector(okay.compress.Lz4Frame, okay.compress.Zstd) do
      val f = file(OkayArrow.write(Tables.everything, Some(codec)))
      val out = run("""
import sys, pyarrow.ipc as ipc
t = ipc.open_stream(open(sys.argv[1], "rb").read()).read_all()
t.validate(full=True)
with ipc.new_stream(sys.argv[1], t.schema) as w: w.write_table(t)
print(t.num_rows, t.num_columns)
""", f)
      assertEquals(out, "4 25", codec.name)
      assertEquals(Tables.same(Tables.everything, OkayArrow.read(java.nio.file.Files.readAllBytes(f))), None, codec.name)
  }

  test("IPC files both ways: pyarrow opens ours; ours finds pyarrow's batches from the footer") {
    val ours = file(OkayArrow.writeFile(Tables.everything, None))
    assertEquals(run("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
r = ipc.open_file(pa.memory_map(sys.argv[1]))
t = r.read_all(); t.validate(full=True)
print(r.num_record_batches, t.num_rows, t.num_columns)
""", ours), "1 4 25")
    val f = file()
    val _ = run("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
s = pa.schema([("n", pa.int64()), ("tag", pa.string())])
with ipc.new_file(sys.argv[1], s) as w:
    for k in range(3):
        w.write_batch(pa.record_batch([pa.array([k * 10 + j for j in range(4)]), pa.array(["b%d" % k] * 4)], schema=s))
""", f)
    val bytes = java.nio.file.Files.readAllBytes(f)
    assertEquals(OkayArrow.fileBatches(bytes), 3)
    assertEquals(Tables.cells(OkayArrow.readFileBatch(bytes, 1).cols(0)._2), Vector(10L, 11L, 12L, 13L).map(Some(_)))
    assertEquals(Tables.cells(OkayArrow.readFileBatch(bytes, 2).cols(1)._2), Vector.fill(4)(Some("b2")))
    assertEquals(OkayArrow.readFile(bytes).rows, 12)
  }

