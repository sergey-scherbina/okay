package okay.parquet

import scala.annotation.tailrec

/**
 * THRIFT'S COMPACT PROTOCOL, the part Parquet's metadata uses
 * (specs/parquet.md): structs of numbered fields, zig-zag varints,
 * binaries, lists, and booleans folded into the field header. Read into
 * a small tree — `Thrift.Struct` is field id to value — and written from
 * one, so the metadata code is a mapping over field ids, which is
 * exactly how parquet.thrift describes it.
 *
 * Nesting is BOUNDED (`MaxDepth`): a struct inside a list inside a
 * struct is a recursive read, and a hostile footer could nest without
 * end; Parquet's own metadata nests five deep.
 */
object Thrift:

  enum Value:
    /** an integer as read (i8, i16, i32 or i64); written as i64 */
    case I(v: Long)
    /** an integer WRITTEN as i32 — the field's declared width is part of
     * its tag, and parquet-java checks it */
    case I32(v: Int)
    /** an integer WRITTEN as i8 (IntType's bit width) */
    case I8(v: Byte)
    case D(v: Double)
    case Bool(v: Boolean)
    case Bin(v: Array[Byte])
    case L(elemType: Int, items: Vector[Value])
    case S(fields: Struct)

  final case class Struct(fields: Map[Int, Value]):
    def int(id: Int): Option[Long] = fields.get(id).collect {
      case Value.I(v) => v; case Value.I32(v) => v.toLong; case Value.I8(v) => v.toLong }
    def bool(id: Int): Option[Boolean] = fields.get(id).collect { case Value.Bool(v) => v }
    def bin(id: Int): Option[Array[Byte]] = fields.get(id).collect { case Value.Bin(v) => v }
    def str(id: Int): Option[String] = bin(id).map(String(_, "UTF-8"))
    def struct(id: Int): Option[Struct] = fields.get(id).collect { case Value.S(s) => s }
    def list(id: Int): Vector[Value] = fields.get(id).collect { case Value.L(_, xs) => xs }.getOrElse(Vector.empty)
    def structs(id: Int): Vector[Struct] = list(id).collect { case Value.S(s) => s }

  object Struct:
    def of(fields: (Int, Value)*): Struct = Struct(fields.toMap)

  /** nesting a footer may use; Parquet's metadata uses five */
  val MaxDepth: Int = 32

  // compact type ids
  private val TTrue = 1
  private val TFalse = 2
  private val TByte = 3
  private val TI16 = 4
  private val TI32 = 5
  private val TI64 = 6
  private val TDouble = 7
  private val TBinary = 8
  private val TList = 9
  private val TSet = 10
  private val TMap = 11
  private val TStruct = 12


  // ---------------------------------------------------------------- read

  final class Reader(bytes: Array[Byte], var at: Int):
    def byte(): Int =
      if at >= bytes.length then throw Refused("Thrift metadata cut short")
      val b = bytes(at) & 0xff
      at += 1
      b
    def varint(): Long =
      var v = 0L
      var shift = 0
      var more = true
      while more do
        if shift > 63 then throw Refused("a Thrift varint longer than 64 bits")
        val b = byte()
        v |= (b & 0x7fL) << shift
        shift += 7
        more = (b & 0x80) != 0
      v
    def zigzag(): Long =
      val v = varint()
      (v >>> 1) ^ -(v & 1)
    def binary(): Array[Byte] =
      val n = varint()
      if n < 0 || at + n > bytes.length then throw Refused(s"a Thrift binary of $n bytes past the end")
      val out = java.util.Arrays.copyOfRange(bytes, at, at + n.toInt)
      at += n.toInt
      out
    def double(): Double =
      if at + 8 > bytes.length then throw Refused("Thrift metadata cut short")
      var v = 0L
      var k = 0
      while k < 8 do { v |= (bytes(at + k) & 0xffL) << (8 * k); k += 1 }
      at += 8
      java.lang.Double.longBitsToDouble(v)

  /** one struct from `bytes` at `at`; answers it and where it ended */
  def read(bytes: Array[Byte], at: Int = 0): (Struct, Int) =
    val r = Reader(bytes, at)
    val s = struct(r, 0)
    (s, r.at)

  private def struct(r: Reader, depth: Int): Struct =
    if depth > MaxDepth then throw Refused(s"Thrift metadata nested deeper than $MaxDepth")
    val out = Map.newBuilder[Int, Value]
    @tailrec def fields(last: Int): Unit =
      val h = r.byte()
      if h != 0 then
        val tpe = h & 0x0f
        val delta = h >>> 4
        val id = if delta != 0 then last + delta else r.zigzag().toInt
        val v = tpe match
          case TTrue => Value.Bool(true)
          case TFalse => Value.Bool(false)
          case _ => value(r, tpe, depth)
        out += id -> v
        fields(id)
    fields(0)
    Struct(out.result())

  private def value(r: Reader, tpe: Int, depth: Int): Value = tpe match
    case TByte => Value.I(r.byte().toByte.toLong)
    case TI16 | TI32 | TI64 => Value.I(r.zigzag())
    case TDouble => Value.D(r.double())
    case TBinary => Value.Bin(r.binary())
    case TList | TSet =>
      val h = r.byte()
      val n = if (h >>> 4) == 15 then r.varint().toInt else h >>> 4
      val et = h & 0x0f
      if n < 0 then throw Refused("a Thrift list of negative size")
      Value.L(et, Vector.tabulate(n) { _ =>
        et match
          // a boolean in a collection is a byte: 1 true, anything else false
          case TTrue | TFalse => Value.Bool(r.byte() == 1)
          case _ => value(r, et, depth + 1)
      })
    case TMap =>
      val n = r.varint().toInt
      if n > 0 then
        val kv = r.byte()
        for _ <- 0 until n do { value(r, kv >>> 4, depth + 1): Unit; value(r, kv & 0x0f, depth + 1): Unit }
      Value.L(0, Vector.empty)            // no map Parquet reads is kept
    case TStruct => Value.S(struct(r, depth + 1))
    case other => throw Refused(s"a Thrift value of type $other")

  // --------------------------------------------------------------- write

  final class Writer:
    private val out = java.io.ByteArrayOutputStream()
    def bytes: Array[Byte] = out.toByteArray
    def byte(b: Int): Unit = out.write(b)
    def varint(v: Long): Unit =
      var x = v
      while (x & ~0x7fL) != 0 do { out.write(((x & 0x7f) | 0x80).toInt); x >>>= 7 }
      out.write(x.toInt)
    def zigzag(v: Long): Unit = varint((v << 1) ^ (v >> 63))
    def binary(b: Array[Byte]): Unit = { varint(b.length.toLong); out.write(b) }

  /** the tag a value is written under (a list's element tag too) */
  private def tag(v: Value): Int = v match
    case Value.I(_) => TI64
    case Value.I32(_) => TI32
    case Value.I8(_) => TByte
    case Value.D(_) => TDouble
    case Value.Bool(b) => if b then TTrue else TFalse
    case Value.Bin(_) => TBinary
    case Value.L(_, _) => TList
    case Value.S(_) => TStruct

  def write(s: Struct): Array[Byte] =
    val w = Writer()
    struct(w, s, 0)
    w.bytes

  private def struct(w: Writer, s: Struct, depth: Int): Unit =
    if depth > MaxDepth then throw Refused(s"Thrift metadata nested deeper than $MaxDepth")
    var last = 0
    for (id, v) <- s.fields.toVector.sortBy(_._1) do
      val t = tag(v)
      if id > last && id - last <= 15 then w.byte(((id - last) << 4) | t)
      else { w.byte(t); w.zigzag(id.toLong) }
      last = id
      v match
        case Value.Bool(_) => ()
        case other => value(w, other, depth)
    w.byte(0)

  private def value(w: Writer, v: Value, depth: Int): Unit = v match
    case Value.I(x) => w.zigzag(x)
    case Value.I32(x) => w.zigzag(x.toLong)
    case Value.I8(x) => w.byte(x & 0xff)
    case Value.D(x) =>
      val bits = java.lang.Double.doubleToLongBits(x)
      for k <- 0 until 8 do w.byte(((bits >>> (8 * k)) & 0xff).toInt)
    case Value.Bool(b) => w.byte(if b then 1 else 2)
    case Value.Bin(b) => w.binary(b)
    case Value.L(et, items) =>
      val t = if et != 0 then et else items.headOption.fold(TStruct)(tag)
      if items.length < 15 then w.byte((items.length << 4) | t)
      else { w.byte(0xf0 | t); w.varint(items.length.toLong) }
      items.foreach {
        case Value.Bool(b) => w.byte(if b then 1 else 2)
        case Value.I(x) => w.zigzag(x)
        case Value.I32(x) => w.zigzag(x.toLong)
        case Value.I8(x) => w.byte(x & 0xff)
        case Value.S(s) => struct(w, s, depth + 1)
        case other => value(w, other, depth + 1)
      }
    case Value.S(s) => struct(w, s, depth + 1)
