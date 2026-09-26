package okay.lake

import okay.codec.Json

/**
 * AVRO OBJECT CONTAINER FILES, READ (specs/dataflow.md, stage 18) —
 * what Iceberg keeps its manifest lists and manifests in.
 *
 * A record reads as its fields in order, `Vector[(String, Any)]`; an
 * array as a `Vector`, a map as a `Map[String, Any]`, a union as the
 * branch's value, `null` as null; int and long as `Long`, float as
 * `Float`, double as `Double`, string as `String`, bytes and fixed as
 * `Array[Byte]`, an enum as its symbol. The same values from either
 * implementation (specs/own-or-standard.md).
 */
trait AvroReader:
  def name: String
  /** every record of an object container file */
  def records(bytes: Array[Byte]): Vector[Any]

object AvroReader:
  /** THE DEFAULT: ours */
  given own: AvroReader = OkayAvro

/** refused Avro, and why */
final class AvroRefused(why: String) extends IllegalArgumentException(why)

/**
 * OUR AVRO: the container (magic, a metadata map, a sync marker, blocks),
 * the binary encoding driven by the writer's schema, and the blocks'
 * codecs — null, deflate (raw), snappy (with its CRC), zstandard.
 * Decoding recurses per level of the SCHEMA and is bounded by `MaxDepth`,
 * checked on the way down: a recursive named type describing a deep
 * linked list is refused past it rather than overflowing the stack.
 */
object OkayAvro extends AvroReader:
  def name = "okay"
  val MaxDepth: Int = 64

  private final class In(b: Array[Byte], var at: Int, until: Int):
    def byte(): Int =
      if at >= until then throw AvroRefused("Avro data cut short")
      val v = b(at) & 0xff
      at += 1
      v
    def long(): Long =
      var v = 0L
      var shift = 0
      var more = true
      while more do
        if shift > 63 then throw AvroRefused("an Avro varint longer than 64 bits")
        val x = byte()
        v |= (x & 0x7fL) << shift
        shift += 7
        more = (x & 0x80) != 0
      (v >>> 1) ^ -(v & 1)
    def take(n: Long): Array[Byte] =
      if n < 0 || at + n > until then throw AvroRefused(s"an Avro value of $n bytes past the end")
      val out = java.util.Arrays.copyOfRange(b, at, at + n.toInt)
      at += n.toInt
      out
    def done: Boolean = at >= until

  def records(bytes: Array[Byte]): Vector[Any] =
    if bytes.length < 4 || bytes(0) != 'O' || bytes(1) != 'b' || bytes(2) != 'j' || bytes(3) != 1 then
      throw AvroRefused("not an Avro object container file")
    val in = In(bytes, 4, bytes.length)
    val meta = map(in, _ => in.take(in.long()))
    val schema = Json.parse(String(meta.getOrElse("avro.schema", throw AvroRefused("an Avro file without a schema")), "UTF-8"))
    val codec = meta.get("avro.codec").map(String(_, "UTF-8")).getOrElse("null")
    val sync = in.take(16)
    val names = scala.collection.mutable.Map.empty[String, Json]
    register(schema, names, 0)
    val out = Vector.newBuilder[Any]
    while !in.done do
      val count = in.long()
      val size = in.long()
      val block = inflate(codec, in.take(size))
      val bin = In(block, 0, block.length)
      var k = 0L
      while k < count do { out += value(bin, schema, names, 0); k += 1 }
      if !java.util.Arrays.equals(in.take(16), sync) then throw AvroRefused("an Avro block without its sync marker")
    out.result()

  /** every named type in the schema, before any value is read: a name
   * defined inside a union branch the data never takes is still a type a
   * later field may refer to */
  private def register(s: Json, names: scala.collection.mutable.Map[String, Json], depth: Int): Unit =
    if depth > MaxDepth then throw AvroRefused(s"an Avro schema nested deeper than $MaxDepth")
    s match
      case Json.JArr(bs) => bs.foreach(register(_, names, depth + 1))
      case o @ Json.JObj(fs) =>
        fs.collectFirst { case ("name", Json.JStr(n)) => n }.foreach(n => names.update(n, o))
        fs.foreach {
          case ("fields", Json.JArr(xs)) =>
            xs.foreach { case Json.JObj(ffs) => ffs.collectFirst { case ("type", t) => t }.foreach(register(_, names, depth + 1)); case _ => () }
          case ("items" | "values", t) => register(t, names, depth + 1)
          case _ => ()
        }
      case _ => ()

  private def inflate(codec: String, b: Array[Byte]): Array[Byte] = codec match
    case "null" => b
    case "deflate" =>
      val inf = java.util.zip.Inflater(true)
      inf.setInput(b)
      val out = java.io.ByteArrayOutputStream()
      val buf = new Array[Byte](64 * 1024)
      while !inf.finished() do
        val n = inf.inflate(buf)
        if n == 0 && (inf.needsInput() || inf.needsDictionary()) then
          throw AvroRefused("a deflate block cut short")
        out.write(buf, 0, n)
      inf.end()
      out.toByteArray
    case "snappy" => okay.compress.Compression.Okay.snappy.decompress(b.dropRight(4))
    case "zstandard" => okay.compress.Compression.Okay.zstd.decompress(b)
    case other => throw AvroRefused(s"an Avro block compressed with $other: read are null, deflate, snappy and zstandard")

  private def map[A](in: In, v: In => A): Map[String, A] =
    val out = Map.newBuilder[String, A]
    var n = in.long()
    while n != 0 do
      if n < 0 then { n = -n; in.long(): Unit }
      var k = 0L
      while k < n do { out += String(in.take(in.long()), "UTF-8") -> v(in); k += 1 }
      n = in.long()
    out.result()

  private def value(in: In, s: Json, names: scala.collection.mutable.Map[String, Json], depth: Int): Any =
    if depth > MaxDepth then throw AvroRefused(s"Avro data nested deeper than $MaxDepth")
    s match
      case Json.JStr(t) => primitive(in, t, names, depth)
      case Json.JArr(branches) =>
        val i = in.long()
        if i < 0 || i >= branches.length then throw AvroRefused(s"an Avro union branch $i of ${branches.length}")
        value(in, branches(i.toInt), names, depth + 1)
      case o @ Json.JObj(fs) =>
        def f(k: String): Option[Json] = fs.collectFirst { case (`k`, v) => v }
        val t = f("type").collect { case Json.JStr(x) => x }.getOrElse(throw AvroRefused(s"an Avro schema without a type: $o"))
        f("name").collect { case Json.JStr(n) => n }.foreach(n => names.update(n, o))
        t match
          case "record" | "error" =>
            f("fields").collect { case Json.JArr(xs) => xs }.getOrElse(Vector.empty).map { fld =>
              val fname = fld match { case Json.JObj(ffs) => ffs.collectFirst { case ("name", Json.JStr(n)) => n }.getOrElse(""); case _ => "" }
              val ftype = fld match { case Json.JObj(ffs) => ffs.collectFirst { case ("type", ty) => ty }.getOrElse(Json.JNull); case _ => Json.JNull }
              fname -> value(in, ftype, names, depth + 1)
            }
          case "array" =>
            val items = f("items").getOrElse(throw AvroRefused("an Avro array without items"))
            val out = Vector.newBuilder[Any]
            var n = in.long()
            while n != 0 do
              if n < 0 then { n = -n; in.long(): Unit }
              var k = 0L
              while k < n do { out += value(in, items, names, depth + 1); k += 1 }
              n = in.long()
            out.result()
          case "map" =>
            val values = f("values").getOrElse(throw AvroRefused("an Avro map without values"))
            map(in, i => value(i, values, names, depth + 1))
          case "fixed" => in.take(f("size").collect { case Json.JNum(n) => n.toLong }.getOrElse(0L))
          case "enum" =>
            val symbols = f("symbols").collect { case Json.JArr(xs) => xs.collect { case Json.JStr(x) => x } }.getOrElse(Vector.empty)
            symbols.lift(in.long().toInt).getOrElse(throw AvroRefused("an Avro enum index outside its symbols"))
          case other => primitive(in, other, names, depth)
      case other => throw AvroRefused(s"an Avro schema this reader does not know: $other")

  private def primitive(in: In, t: String, names: scala.collection.mutable.Map[String, Json], depth: Int): Any = t match
    case "null" => null
    case "boolean" => in.byte() != 0
    case "int" | "long" => in.long()
    case "float" =>
      val b = in.take(4)
      java.lang.Float.intBitsToFloat((b(0) & 0xff) | (b(1) & 0xff) << 8 | (b(2) & 0xff) << 16 | (b(3) & 0xff) << 24)
    case "double" =>
      val b = in.take(8)
      var v = 0L
      var k = 0
      while k < 8 do { v |= (b(k) & 0xffL) << (8 * k); k += 1 }
      java.lang.Double.longBitsToDouble(v)
    case "bytes" => in.take(in.long())
    case "string" => String(in.take(in.long()), "UTF-8")
    case named =>
      val s = names.getOrElse(named, throw AvroRefused(s"an Avro type named '$named' before its definition"))
      value(in, s, names, depth + 1)
