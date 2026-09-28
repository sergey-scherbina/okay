package okay.dlm

import okay.codec.Json
import okay.codec.Json.*
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}

/**
 * THE EXPERIENCE, COMPACTED — a checkpoint that boots in one read
 * (specs/dlm.md, "Checkpoint").
 *
 * A table of labelled vectors as decimal JSON is megabytes of text
 * parsed at every start to produce a fraction of that in floats. This
 * is the same numbers in the format the rest of the world already
 * agreed on.
 *
 * THE CONTAINER IS BORROWED AND THE POLICY IS OURS. `safetensors` is
 * the whole of it:
 *
 *   8 bytes   N, unsigned little-endian u64 — the header length
 *   N bytes   a JSON header, UTF-8, starting with `{`
 *   the rest  tensor bytes, row-major, little-endian, no holes
 *
 * with `{"name": {"dtype", "shape", "data_offsets": [b, e]}}` and a
 * reserved `__metadata__` of string→string. It is designed for
 * zero-copy loading by memory mapping, which is what a reader here
 * does.
 *
 * WHAT IS OURS is `__metadata__`: the encoder that produced the
 * vectors, the dimension, our own format version, and whatever the
 * caller asks to remember beside them (a journal offset, a corpus
 * hash). **A checkpoint whose encoder is not the one running is
 * REFUSED, not read**: numbers from two different functions are not
 * comparable, and a reader that mixes them is confidently wrong.
 *
 * STRINGS. safetensors carries tensors and no strings, and every
 * vector here has a label. Encoded the way Arrow encodes a UTF-8
 * column and staying inside the one container: `labels.bytes` (U8)
 * and `labels.offsets` (U64).
 *
 * `MappedByteBuffer` AND NOT `MemorySegment`, deliberately: consumers
 * build on JDK 21, where the foreign-memory API is preview.
 */
object Checkpoint:

  /** our own version of what these tensors MEAN, beside the
   * container's. A reader refuses a file it predates. */
  val format = 1

  final case class Loaded(encoder: String, dim: Int, labels: Vector[String],
                          vecs: Vector[Array[Float]], meta: Map[String, String],
                          /** one more row of numbers where an artifact has
                           * one — a static table's per-unit weights. `F64`
                           * because that is what it is */
                          weights: Array[Double] = Array.empty)

  /** the checkpoint beside a JSON artifact: the same name, the
   * container's own extension */
  def binaryOf(json: Path): Path =
    json.resolveSibling(json.getFileName.toString.stripSuffix(".vec.json").stripSuffix(".json") + ".safetensors")

  /** the same, for a resource name */
  def binaryOf(json: String): String =
    json.stripSuffix(".vec.json").stripSuffix(".json") + ".safetensors"

  // ---- writing ---------------------------------------------------------

  private def utf8(s: String) = s.getBytes("UTF-8")

  /**
   * IEEE 754 BINARY16, the two functions safetensors' own `F16` dtype
   * names. Round to nearest, ties to even, on the mantissa's own last
   * kept bit — the same rounding every FPU does in hardware, written
   * here because the JVM has no half-precision type to call it on.
   */
  def floatToHalf(f: Float): Short =
    val bits = java.lang.Float.floatToRawIntBits(f)
    val sign = (bits >>> 16) & 0x8000
    val absBits = bits & 0x7fffffff
    val exp = (absBits >>> 23) & 0xff
    val mant = absBits & 0x7fffff
    if exp == 0xff then
      // inf or NaN: NaN keeps a payload bit so it stays a NaN
      (sign | 0x7c00 | (if mant != 0 then 0x200 else 0)).toShort
    else
      val hExp = exp - 127 + 15
      if hExp >= 0x1f then
        (sign | 0x7c00).toShort
      else if hExp <= 0 then
        if hExp < -10 then sign.toShort
        else
          val m = mant | 0x800000
          val shift = 14 - hExp
          var half = m >>> shift
          if ((m >>> (shift - 1)) & 1) == 1 then half += 1
          (sign | half).toShort
      else
        var half = mant >>> 13
        val roundBit = (mant >>> 12) & 1
        val stickyRest = mant & 0xfff
        if roundBit == 1 && (stickyRest != 0 || (half & 1) == 1) then half += 1
        var e = hExp
        if half == 0x400 then { half = 0; e += 1 }
        if e >= 0x1f then (sign | 0x7c00).toShort
        else (sign | (e << 10) | half).toShort

  def halfToFloat(h: Short): Float =
    val v = h & 0xffff
    val sign = (v & 0x8000) << 16
    val exp = (v >>> 10) & 0x1f
    val mant = v & 0x3ff
    val bits =
      if exp == 0 then
        if mant == 0 then sign
        else
          var e = 1; var m = mant
          while (m & 0x400) == 0 do { m <<= 1; e -= 1 }
          sign | ((e - 15 + 127) << 23) | ((m & 0x3ff) << 13)
      else if exp == 0x1f then sign | 0x7f800000 | (mant << 13)
      else sign | ((exp - 15 + 127) << 23) | (mant << 13)
    java.lang.Float.intBitsToFloat(bits)

  /**
   * One checkpoint as BYTES: the vectors, their labels, and who made
   * them. `extra` travels in `__metadata__`, where every value is a
   * string because the format says so.
   *
   * A CHECKPOINT IS A PURE FUNCTION OF ITS NUMBERS: no timestamp, so
   * compiling the same table twice produces the same file. What
   * identifies it is the encoder, the shape and whatever the caller
   * put in `extra`, and every one of those is a property of the
   * content.
   */
  def bytes(encoder: String, dim: Int, labels: Vector[String], vecs: Vector[Array[Float]],
            extra: Map[String, String] = Map.empty, weights: Array[Double] = Array.empty,
            f16: Boolean = false): ByteBuffer =
    require(labels.length == vecs.length, "a label per vector")
    require(vecs.forall(_.length == dim), s"every vector is $dim long")
    val n = vecs.length
    val labelBytes = labels.map(utf8)
    val offsets = labelBytes.scanLeft(0L)((a, b) => a + b.length)
    val bytesPerFloat = if f16 then 2 else 4
    val vecsLen = n.toLong * dim * bytesPerFloat
    val offLen = offsets.length.toLong * 8
    val bytesLen = offsets.last
    val wLen = weights.length.toLong * 8
    val meta = Map(
      "format" -> format.toString, "encoder" -> encoder, "dim" -> dim.toString,
      "count" -> n.toString) ++ extra
    val header = JObj(Vector(
      "__metadata__" -> JObj(meta.toVector.sortBy(_._1).map((k, v) => k -> JStr(v))),
      "vecs" -> tensor(if f16 then "F16" else "F32", Vector(n, dim), 0L, vecsLen),
      "labels.offsets" -> tensor("U64", Vector(offsets.length), vecsLen, vecsLen + offLen),
      "labels.bytes" -> tensor("U8", Vector(bytesLen.toInt), vecsLen + offLen, vecsLen + offLen + bytesLen)) ++
      Option.when(weights.nonEmpty)("weights" ->
        tensor("F64", Vector(weights.length), vecsLen + offLen + bytesLen,
          vecsLen + offLen + bytesLen + wLen)))
    val headerBytes = utf8(Json.print(header))
    val out = ByteBuffer.allocate(8 + headerBytes.length + (vecsLen + offLen + bytesLen + wLen).toInt)
      .order(ByteOrder.LITTLE_ENDIAN)
    out.putLong(headerBytes.length.toLong)
    out.put(headerBytes)
    if f16 then vecs.foreach(v => v.foreach(x => out.putShort(floatToHalf(x))))
    else vecs.foreach(v => v.foreach(out.putFloat))
    offsets.foreach(out.putLong)
    labelBytes.foreach(out.put)
    weights.foreach(out.putDouble)
    out.flip()
    out

  /** …written whole and moved into place: a half-written checkpoint
   * that a boot reads is a worse failure than no checkpoint */
  def write(path: Path, encoder: String, dim: Int,
            labels: Vector[String], vecs: Vector[Array[Float]],
            extra: Map[String, String] = Map.empty,
            weights: Array[Double] = Array.empty,
            f16: Boolean = false): Unit =
    val out = bytes(encoder, dim, labels, vecs, extra, weights, f16)
    Files.createDirectories(path.toAbsolutePath.getParent)
    val tmp = path.resolveSibling(path.getFileName.toString + ".part")
    val ch = FileChannel.open(tmp, StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE)
    try ch.write(out) finally ch.close()
    Files.move(tmp, path, java.nio.file.StandardCopyOption.REPLACE_EXISTING): Unit

  private def tensor(dtype: String, shape: Vector[Int], begin: Long, end: Long): Json =
    JObj(Vector(
      "dtype" -> JStr(dtype),
      "shape" -> JArr(shape.map(x => JNum(x.toDouble))),
      "data_offsets" -> JArr(Vector(JNum(begin.toDouble), JNum(end.toDouble)))))

  // ---- reading ---------------------------------------------------------

  /** the header's own limit, from the specification: a file claiming a
   * header larger than this is refused rather than parsed */
  private val maxHeader = 100 * 1024 * 1024

  /**
   * THE FILE, MAPPED. `Left` is a sentence naming what disagreed —
   * never a throw, and never a silent fallback: a boot that cannot
   * use a checkpoint says which field it was and builds the long way.
   */
  def read(path: Path, expect: Option[(String, Int)] = None): Either[String, Loaded] =
    if !Files.exists(path) then Left(s"$path: no checkpoint")
    else
      val ch = FileChannel.open(path, StandardOpenOption.READ)
      try
        val size = ch.size()
        if size < 8 then Left(s"$path: shorter than a header length")
        else of(ch.map(FileChannel.MapMode.READ_ONLY, 0, size), expect, path.toString)
      finally ch.close()

  /** the same bytes from wherever they came — a resource inside a jar
   * has no address to map, and the cost being paid is the parse, not
   * the read */
  def of(bytes: ByteBuffer, expect: Option[(String, Int)], name: String): Either[String, Loaded] =
    val map = bytes.order(ByteOrder.LITTLE_ENDIAN)
    if map.capacity() < 8 then Left(s"$name: shorter than a header length")
    else
      val n = map.getLong(0)
      if n <= 0 || n > maxHeader || 8 + n > map.capacity() then
        Left(s"$name: header length $n is not in the file")
      else
        val head = new Array[Byte](n.toInt)
        map.position(8): Unit
        map.get(head): Unit
        parse(new String(head, "UTF-8"), map, 8 + n.toInt, expect, name)

  /** …and from a jar, on the caller's class path */
  def resource(name: String, expect: Option[(String, Int)] = None,
               loader: ClassLoader = Thread.currentThread.getContextClassLoader): Either[String, Loaded] =
    val path = name.stripPrefix("/")
    Option(loader.getResourceAsStream(path)).orElse(Option(getClass.getResourceAsStream("/" + path))) match
      case None => Left(s"$name: not in the image")
      case Some(in) =>
        val raw = try in.readAllBytes() finally in.close()
        of(ByteBuffer.wrap(raw), expect, name)

  /** the `__metadata__` alone, without reading a vector — what a
   * listing of many checkpoints needs to say which is which */
  def meta(path: Path): Either[String, Map[String, String]] =
    if !Files.exists(path) then Left(s"$path: no checkpoint")
    else
      val ch = FileChannel.open(path, StandardOpenOption.READ)
      try
        val len = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
        if ch.read(len, 0L) < 8 then Left(s"$path: shorter than a header length")
        else
          val n = len.getLong(0)
          if n <= 0 || n > maxHeader || 8 + n > ch.size() then Left(s"$path: header length $n is not in the file")
          else
            val head = ByteBuffer.allocate(n.toInt)
            ch.read(head, 8L): Unit
            (try Json.parse(new String(head.array(), "UTF-8")) catch case _: Exception => JNull) match
              case JObj(fs) => Right(fs.collectFirst { case ("__metadata__", JObj(m)) =>
                m.collect { case (k, JStr(v)) => k -> v }.toMap }.getOrElse(Map.empty))
              case _ => Left(s"$path: the header is not a JSON object")
      finally ch.close()

  /** a refusal that means «there is no such file», as opposed to one
   * that names a disagreement — a caller falls back silently on the
   * first and says so on the second */
  def absent(why: String): Boolean =
    why.endsWith("not in the image") || why.endsWith("no checkpoint")

  private def parse(head: String, map: ByteBuffer, base: Int,
                    expect: Option[(String, Int)], name: String): Either[String, Loaded] =
    val j = try Json.parse(head) catch case e: Exception => JStr(s"broken: ${e.getMessage}")
    j match
      case JObj(fs) =>
        def obj(k: String) = fs.collectFirst { case (n, JObj(v)) if n == k => v }
        val meta = obj("__metadata__").map(_.collect { case (k, JStr(v)) => k -> v }.toMap)
          .getOrElse(Map.empty)
        def num(o: Vector[(String, Json)], k: String) =
          o.collectFirst { case (n, JArr(xs)) if n == k => xs.collect { case JNum(x) => x.toLong } }
            .getOrElse(Vector.empty)
        def str(o: Vector[(String, Json)], k: String) =
          o.collectFirst { case (n, JStr(v)) if n == k => v }
        val encoder = meta.getOrElse("encoder", "")
        val dim = meta.get("dim").flatMap(_.toIntOption).getOrElse(0)
        val ver = meta.get("format").flatMap(_.toIntOption).getOrElse(0)
        if ver != format then Left(s"$name: format $ver, this reader is $format")
        else expect match
          case Some((want, _)) if encoder != want =>
            Left(s"$name: made by «$encoder», this process runs «$want» — refused")
          case Some((_, wantDim)) if dim != wantDim =>
            Left(s"$name: dim $dim, this process has $wantDim — refused")
          case _ =>
            val v = obj("vecs"); val lo = obj("labels.offsets"); val lb = obj("labels.bytes")
            (v, lo, lb) match
              case (Some(vt), Some(ot), Some(bt)) =>
                val Vector(vb, _) = num(vt, "data_offsets"): @unchecked
                val shape = num(vt, "shape")
                val count = shape.headOption.getOrElse(0L).toInt
                val Vector(ob, _) = num(ot, "data_offsets"): @unchecked
                val Vector(bb, _) = num(bt, "data_offsets"): @unchecked
                // F32 or F16 — the DTYPE IN THE HEADER decides, not a
                // flag the caller has to remember to pass
                val bpf = str(vt, "dtype") match
                  case Some("F16") => 2
                  case _ => 4
                val vecs = Vector.tabulate(count) { i =>
                  val a = new Array[Float](dim)
                  var k = 0
                  while k < dim do
                    val at = base + (vb + (i.toLong * dim + k) * bpf).toInt
                    a(k) = if bpf == 2 then halfToFloat(map.getShort(at)) else map.getFloat(at)
                    k += 1
                  a
                }
                val offs = Vector.tabulate(count + 1)(i => map.getLong(base + (ob + i * 8L).toInt))
                val labels = Vector.tabulate(count) { i =>
                  val from = offs(i).toInt; val to = offs(i + 1).toInt
                  val a = new Array[Byte](to - from)
                  var k = 0
                  while k < a.length do { a(k) = map.get(base + (bb + from + k).toInt); k += 1 }
                  new String(a, "UTF-8")
                }
                val ws = obj("weights").map { wt =>
                  val Vector(wb, we) = num(wt, "data_offsets"): @unchecked
                  Array.tabulate(((we - wb) / 8).toInt)(i => map.getDouble(base + (wb + i * 8L).toInt))
                }.getOrElse(Array.empty[Double])
                Right(Loaded(encoder, dim, labels, vecs, meta, ws))
              case _ => Left(s"$name: a tensor is missing (vecs, labels.offsets, labels.bytes)")
      case _ => Left(s"$name: the header is not a JSON object")
