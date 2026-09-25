package okay.compress

import java.nio.file.Files

/** pyarrow's codecs as the oracle: `pa.compress` / `pa.decompress` with
 * `lz4` (the frame format), `lz4_raw` (a block) and `zstd`. A python with
 * pyarrow: `OKAY_PYARROW_PYTHON`, else python3 when it has it. */
object PyArrow:
  lazy val python: Option[String] =
    sys.env.get("OKAY_PYARROW_PYTHON").orElse(Some("python3")).filter { py =>
      scala.util.Try(ProcessBuilder(py, "-c", "import pyarrow").start().waitFor() == 0).getOrElse(false)
    }

  /** every sample through one python call: `op` is compress or decompress,
   * `extra` the keyword arguments; answers the outputs in order */
  def apply(op: String, codec: String, inputs: Vector[Array[Byte]], extra: Vector[String] = Vector.empty): Vector[Array[Byte]] =
    val dir = Files.createTempDirectory("okay-compress")
    inputs.zipWithIndex.foreach((b, i) => Files.write(dir.resolve(s"in$i"), b): Unit)
    val args = inputs.indices.map(i => if extra.isEmpty then "" else extra(i)).map(a => s"'$a'").mkString("[", ",", "]")
    val script = s"""
import sys, os, pyarrow as pa
d = sys.argv[1]
extra = $args
for i in range(${inputs.length}):
    data = open(os.path.join(d, "in%d" % i), "rb").read()
    kw = {}
    if extra[i]: kw["decompressed_size"] = int(extra[i])
    out = pa.$op(data, codec="$codec", asbytes=True, **kw)
    open(os.path.join(d, "out%d" % i), "wb").write(out)
"""
    val p = ProcessBuilder(python.get, "-c", script, dir.toString).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    if p.waitFor() != 0 then throw IllegalStateException(s"python: $said")
    inputs.indices.toVector.map(i => Files.readAllBytes(dir.resolve(s"out$i")))

  /** pyarrow's ZSTD at a compression level */
  def zstd(inputs: Vector[Array[Byte]], level: Int): Vector[Array[Byte]] =
    val dir = Files.createTempDirectory("okay-zstd")
    inputs.zipWithIndex.foreach((b, i) => Files.write(dir.resolve(s"in$i"), b): Unit)
    val script = s"""
import sys, os, pyarrow as pa
d = sys.argv[1]
c = pa.Codec("zstd", compression_level=$level)
for i in range(${inputs.length}):
    data = open(os.path.join(d, "in%d" % i), "rb").read()
    open(os.path.join(d, "out%d" % i), "wb").write(c.compress(data, asbytes=True))
"""
    val p = ProcessBuilder(python.get, "-c", script, dir.toString).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    if p.waitFor() != 0 then throw IllegalStateException(s"python: $said")
    inputs.indices.toVector.map(i => Files.readAllBytes(dir.resolve(s"out$i")))

