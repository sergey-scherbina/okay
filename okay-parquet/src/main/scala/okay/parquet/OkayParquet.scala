package okay.parquet

import okay.arrow.{Column, Table, TimeUnit}
import okay.compress.Compression
import okay.parquet.Thrift.{Struct, Value}

/**
 * OUR PARQUET (specs/parquet.md): the footer and pages read and written
 * over okay-arrow's `Table`, nested columns included (stage 2), no
 * dependency beyond okay. The field numbers below are parquet.thrift's;
 * each struct is read and written as the file format defines it, so the
 * comments name the thrift fields rather than restate the format.
 *
 * A NESTED COLUMN IS ITS LEAVES' LEVELS. Every leaf column chunk holds,
 * per entry, a repetition level (which repeated ancestor a value starts
 * a new element of), a definition level (how many optional or repeated
 * ancestors are present) and — when the leaf itself is present — a
 * value. `Shape` is the Arrow column a schema subtree becomes, with the
 * levels that decide it; `assemble` builds a column from its leaves'
 * levels and `shred` makes the levels of a column (Dremel, in the
 * columnar form: see the spec's Decision).
 *
 * RECURSION here follows a column's TYPE — a schema tree, a shape, a
 * column — never its data, and every one is bounded by
 * `Column.MaxNesting` (64), checked when the schema is read and by
 * `Table` for what is written.
 */
object OkayParquet extends ParquetCodec:
  def name = "okay"

  private val Magic = "PAR1".getBytes("US-ASCII")

  // physical types
  private val TBoolean = 0
  private val TInt32 = 1
  private val TInt64 = 2
  private val TInt96 = 3
  private val TFloat = 4
  private val TDouble = 5
  private val TByteArray = 6
  private val TFixed = 7

  // encodings
  private val Plain = 0
  private val PlainDictionary = 2
  private val RleEncoding = 3
  private val DeltaBinaryPacked = 5
  private val DeltaLengthByteArray = 6
  private val DeltaByteArray = 7
  private val RleDictionary = 8
  private val ByteStreamSplit = 9

  // page types
  private val DataPage = 0
  private val DictionaryPage = 2
  private val DataPageV2 = 3

  // repetition
  private val Required = 0
  private val Optional = 1
  private val Repeated = 2

  /** what a leaf decodes to */
  private enum Kind:
    case KBool
    case KInt32(bits: Int, signed: Boolean)
    case KDate
    case KInt64(signed: Boolean)
    case KTimestamp(unit: TimeUnit, utc: Boolean)
    case KInt96
    case KFloat
    case KDouble
    case KString
    case KBinary
    case KFixed(width: Int)
    case KDecimal(precision: Int, scale: Int)
  import Kind.*

  /** a leaf column: its dotted path, physical type, and its levels' maxima */
  private final case class Leaf(name: String, physical: Int, width: Int, kind: Kind, maxDef: Int, maxRep: Int)

  /** a schema element and its children, as the footer lists them pre-order */
  private final case class Node(name: String, repetition: Int, e: Struct, children: Vector[Node]):
    def group: Boolean = e.int(5).exists(_ > 0) || (children.nonEmpty)
    def converted: Option[Int] = e.int(6).map(_.toInt)
    def logical: Option[Struct] = e.struct(10)
    def isList: Boolean = converted.contains(3) || logical.exists(_.struct(3).isDefined)
    def isMap: Boolean = converted.exists(c => c == 1 || c == 2) || logical.exists(_.struct(2).isDefined)

  /** the Arrow column a schema subtree becomes, and the levels that decide it */
  private enum Shape:
    /** leaf `leaf`; present when an entry's definition level is `maxDef` */
    case SLeaf(leaf: Int, maxDef: Int)
    /** a struct; present at definition level `present` */
    case SStruct(fields: Vector[(String, Shape)], present: Int)
    /** a list: non-null at `present`, holding an element at `element`,
     * each element after the first starting at repetition level `rep` */
    case SList(child: Shape, present: Int, element: Int, rep: Int)
  import Shape.*

  // ------------------------------------------------------------ the footer

  /** the footer as this codec reads it, kept in `Footer.parsed` */
  private final case class Parsed(f: Struct, leaves: Vector[Leaf], columns: Vector[(String, Shape)])

  def footer(in: ReadAt): Footer =
    val p = meta(in)
    Footer(p.columns.map((n, s) => n -> Column.describe(emptyOf(p.leaves, s, 0))), p.f.structs(4).map(_.int(3).getOrElse(0L)),
      p.f.structs(5).map(kv => kv.str(1).getOrElse("") -> kv.str(2).getOrElse("")), p.f.str(6))(p)

  private def meta(in: ReadAt): Parsed =
    val size = in.size
    if size < 12 then throw Refused(s"not a Parquet file: $size bytes")
    val tail = in.read(size - 8, 8)
    if !java.util.Arrays.equals(tail.drop(4), Magic) then throw Refused("not a Parquet file: no PAR1 at its end")
    val len = (tail(0) & 0xff) | (tail(1) & 0xff) << 8 | (tail(2) & 0xff) << 16 | (tail(3) & 0xff) << 24
    if len <= 0 || len > size - 12 then throw Refused(s"a Parquet footer of $len bytes in a file of $size")
    val (f, _) = Thrift.read(in.read(size - 8 - len, len))
    val (leaves, columns) = shapes(tree(f.structs(2)))
    Parsed(f, leaves, columns)

  /** the schema's pre-order list as a tree: the root's children */
  private def tree(schema: Vector[Struct]): Vector[Node] =
    if schema.isEmpty then throw Refused("a Parquet footer with no schema")
    var at = 1
    def node(depth: Int): Node =
      if depth > Column.MaxNesting then throw Refused(s"a Parquet schema nested deeper than ${Column.MaxNesting}")
      if at >= schema.length then throw Refused("a Parquet schema whose children run past its end")
      val e = schema(at)
      at += 1
      val n = e.int(5).getOrElse(0L).toInt
      Node(e.str(4).getOrElse(""), e.int(3).getOrElse(0L).toInt, e, Vector.fill(n)(node(depth + 1)))
    val out = Vector.fill(schema.head.int(5).getOrElse(0L).toInt)(node(1))
    if at != schema.length then throw Refused(s"a Parquet schema of ${schema.length} elements whose tree holds $at")
    out

  /** the leaves in file order and the top-level columns' shapes */
  private def shapes(top: Vector[Node]): (Vector[Leaf], Vector[(String, Shape)]) =
    val leaves = Vector.newBuilder[Leaf]
    var count = 0
    def leaf(n: Node, path: String, d: Int, r: Int): Shape =
      val physical = n.e.int(1).getOrElse(throw Refused(s"column '$path' has no type")).toInt
      leaves += Leaf(path, physical, n.e.int(2).getOrElse(0L).toInt, kindOf(path, physical, n.e), d, r)
      count += 1
      SLeaf(count - 1, d)
    /** node `n` under a parent at levels (d0, r0) */
    def shape(n: Node, path: String, d0: Int, r0: Int): Shape =
      val d = d0 + (if n.repetition != Required then 1 else 0)
      val r = r0 + (if n.repetition == Repeated then 1 else 0)
      if n.repetition == Repeated then
        // an unannotated repeated field: a list that is never null
        SList(element(n, path, d, r), d0, d, r)
      else if n.group && (n.isList || n.isMap) then
        if n.children.length != 1 || n.children.head.repetition != Repeated then
          throw Refused(s"column '$path' is a LIST or MAP whose child is not one repeated field")
        val c = n.children.head
        val dc = d + 1
        val rc = r + 1
        val elem =
          if n.isMap then SStruct(c.children.map(k => k.name -> shape(k, s"$path.${c.name}.${k.name}", dc, rc)), dc)
          else if !c.group then leaf(c, s"$path.${c.name}", dc, rc)
          else if c.children.length > 1 || c.name == "array" || c.name == s"${n.name}_tuple" then
            SStruct(c.children.map(k => k.name -> shape(k, s"$path.${c.name}.${k.name}", dc, rc)), dc)
          else shape(c.children.head, s"$path.${c.name}.${c.children.head.name}", dc, rc)
        SList(elem, d, dc, rc)
      else if n.group then SStruct(n.children.map(k => k.name -> shape(k, s"$path.${k.name}", d, r)), d)
      else leaf(n, path, d, r)
    /** a repeated node's element: the node itself, present at its own level */
    def element(n: Node, path: String, d: Int, r: Int): Shape =
      if n.group then SStruct(n.children.map(k => k.name -> shape(k, s"$path.${k.name}", d, r)), d)
      else leaf(n, path, d, r)
    val cols = top.map(n => n.name -> shape(n, n.name, 0, 0))
    (leaves.result(), cols)

  private def kindOf(name: String, physical: Int, e: Struct): Kind =
    val logical = e.struct(10)
    val converted = e.int(6).map(_.toInt)
    def decimal: Option[Kind] =
      logical.flatMap(_.struct(5)).map(d => KDecimal(d.int(2).getOrElse(0L).toInt, d.int(1).getOrElse(0L).toInt))
        .orElse(if converted.contains(5) then Some(KDecimal(e.int(8).getOrElse(0L).toInt, e.int(7).getOrElse(0L).toInt)) else None)
    def integer: Option[(Int, Boolean)] =
      logical.flatMap(_.struct(10)).map(i => (i.int(1).getOrElse(32L).toInt, i.bool(2).getOrElse(true)))
        .orElse(converted.collect {
          case 11 => (8, false); case 12 => (16, false); case 13 => (32, false); case 14 => (64, false)
          case 15 => (8, true); case 16 => (16, true); case 17 => (32, true); case 18 => (64, true) })
    physical match
      case TBoolean => KBool
      case TInt32 =>
        decimal.orElse(
          if logical.exists(_.struct(6).isDefined) || converted.contains(6) then Some(KDate) else None)
          .getOrElse { val (b, s) = integer.getOrElse((32, true)); KInt32(b, s) }
      case TInt64 =>
        val ts = logical.flatMap(_.struct(8)).map { t =>
          val u = t.struct(2)
          val unit =
            if u.exists(_.struct(1).isDefined) then TimeUnit.Milli
            else if u.exists(_.struct(2).isDefined) then TimeUnit.Micro
            else TimeUnit.Nano
          KTimestamp(unit, t.bool(1).getOrElse(false))
        }.orElse(converted.collect {
          case 9 => KTimestamp(TimeUnit.Milli, true)
          case 10 => KTimestamp(TimeUnit.Micro, true) })
        ts.orElse(decimal).getOrElse(KInt64(integer.forall(_._2)))
      case TInt96 => KInt96
      case TFloat => KFloat
      case TDouble => KDouble
      case TByteArray =>
        val text = logical.exists(l => l.struct(1).isDefined || l.struct(4).isDefined || l.struct(12).isDefined) ||
          converted.exists(c => c == 0 || c == 4 || c == 19)
        if text then KString else decimal.getOrElse(KBinary)
      case TFixed => decimal.getOrElse(KFixed(e.int(2).getOrElse(0L).toInt))
      case other => throw Refused(s"column '$name' has physical type $other")

  /** a column of `n` nulls of a leaf's kind */
  private def empty(k: Kind, n: Int): Column =
    val no = new Array[Boolean](n)
    k match
      case KBool => Column.Bool(new Array[Boolean](n), no)
      case KInt32(b, s) => Column.Ints(b, s, new Array[Long](n), no)
      case KDate => Column.Date32(new Array[Int](n), no)
      case KInt64(true) => Column.Int64(new Array[Long](n), no)
      case KInt64(false) => Column.Ints(64, false, new Array[Long](n), no)
      case KTimestamp(u, utc) => Column.Timestamp(u, if utc then Some("UTC") else None, new Array[Long](n), no)
      case KInt96 => Column.Timestamp(TimeUnit.Nano, Some("UTC"), new Array[Long](n), no)
      case KFloat => Column.Float32(new Array[Float](n), no)
      case KDouble => Column.Float64(new Array[Double](n), no)
      case KString => Column.Utf8(Array.fill(n)(""), no)
      case KBinary => Column.Binary(Array.fill(n)(Array.emptyByteArray), no)
      case KFixed(w) => Column.FixedBinary(w, Array.fill(n)(new Array[Byte](w)), no)
      case KDecimal(p, s) => Column.Decimal(p, s, Array.fill(n)(BigInt(0)), no)

  /** a column of `n` nulls of a shape */
  private def emptyOf(leaves: Vector[Leaf], s: Shape, n: Int): Column = s match
    case SLeaf(l, _) => empty(leaves(l).kind, n)
    case SStruct(fs, _) => Column.Struct(fs.map((k, f) => k -> emptyOf(leaves, f, n)), new Array[Boolean](n))
    case SList(c, _, _, _) => Column.ListOf(new Array[Int](n + 1), emptyOf(leaves, c, 0), new Array[Boolean](n))

  // ------------------------------------------------------------- reading

  /** one leaf chunk's entries: levels per entry, and the present values */
  private final class Levels(val reps: Array[Int], val defs: Array[Int], val dense: Column, maxDef: Int):
    /** the dense index of every entry (meaningful where it is present) */
    val index: Array[Int] =
      val out = new Array[Int](defs.length)
      var k = 0
      var i = 0
      while i < defs.length do { out(i) = k; if defs(i) == maxDef then k += 1; i += 1 }
      out

  def group(in: ReadAt, footer: Footer, g: Int, columns: Option[Set[String]] = None)
           (using Compression): Table =
    val p = footer.parsed match
      case p: Parsed => p
      case _ => meta(in)
    val groups = p.f.structs(4)
    if g < 0 || g >= groups.length then throw Refused(s"row group $g of ${groups.length}")
    val rg = groups(g)
    val rows = rg.int(3).getOrElse(0L)
    if rows > Int.MaxValue then throw Refused(s"a row group of $rows rows")
    val chunks = rg.structs(1)
    if chunks.length != p.leaves.length then
      throw Refused(s"row group $g has ${chunks.length} column chunks for ${p.leaves.length} leaf columns")
    val wanted = p.columns.filter((n, _) => columns.forall(_.contains(n)))
    val used = wanted.flatMap((_, s) => leavesOf(s)).toSet
    val levels = p.leaves.indices.map(i => if used(i) then Some(chunk(in, p.leaves(i), chunks(i))) else None).toVector
    val cols = wanted.map { (name, s) =>
      val ls = leavesOf(s)
      // the rows: every entry at repetition level zero starts one
      val ranges = ls.map { l =>
        val lv = levels(l).get
        val starts = (0 until lv.reps.length).filter(lv.reps(_) == 0).toArray
        if starts.length != rows then
          throw Refused(s"column '$name': ${starts.length} records in a leaf of a group of $rows rows")
        l -> (starts, starts.drop(1) :+ lv.reps.length)
      }.toMap
      name -> assemble(p.leaves, levels, s, ranges, rows.toInt)
    }
    Table(cols, p.f.structs(5).map(kv => kv.str(1).getOrElse("") -> kv.str(2).getOrElse("")))

  private def leavesOf(s: Shape): Vector[Int] = s match
    case SLeaf(l, _) => Vector(l)
    case SStruct(fs, _) => fs.flatMap((_, f) => leavesOf(f))
    case SList(c, _, _, _) => leavesOf(c)

  /**
   * THE COLUMN FOR `s`, over `n` instances; `ranges(leaf)` are each
   * instance's entries `[start, end)` in that leaf. A struct's validity
   * and a list's offsets come from its FIRST leaf (every leaf agrees);
   * each leaf's element ranges come from its own levels.
   */
  private def assemble(leaves: Vector[Leaf], levels: Vector[Option[Levels]], s: Shape,
                       ranges: Map[Int, (Array[Int], Array[Int])], n: Int): Column = s match
    case SLeaf(l, maxDef) =>
      val lv = levels(l).get
      val (starts, _) = ranges(l)
      val valid = Array.tabulate(n)(k => lv.defs(starts(k)) == maxDef)
      if !valid.exists(identity) then empty(leaves(l).kind, n)
      else lv.dense.take(Array.tabulate(n)(k => if valid(k) then lv.index(starts(k)) else 0), valid)
    case SStruct(fs, present) =>
      val first = leavesOf(s).head
      val lv = levels(first).get
      val valid = Array.tabulate(n)(k => lv.defs(ranges(first)._1(k)) >= present)
      Column.Struct(fs.map((name, f) => name -> assemble(leaves, levels, f, ranges, n)), valid)
    case SList(c, present, element, rep) =>
      val ls = leavesOf(c)
      val split = ls.map { l =>
        val lv = levels(l).get
        val (starts, ends) = ranges(l)
        val es = Array.newBuilder[Int]
        val ee = Array.newBuilder[Int]
        val counts = new Array[Int](n)
        val valid = new Array[Boolean](n)
        var k = 0
        while k < n do
          val s0 = starts(k)
          val d0 = lv.defs(s0)
          valid(k) = d0 >= present
          if d0 >= element then
            var i = s0
            var open = s0
            i += 1
            while i < ends(k) do
              if lv.reps(i) <= rep then { es += open; ee += i; counts(k) += 1; open = i }
              i += 1
            es += open; ee += ends(k); counts(k) += 1
          k += 1
        (l, es.result(), ee.result(), counts, valid)
      }
      val (_, _, _, counts, valid) = split.head
      val offsets = new Array[Int](n + 1)
      var k = 0
      while k < n do { offsets(k + 1) = offsets(k) + counts(k); k += 1 }
      val inner = split.map((l, es, ee, _, _) => l -> (es, ee)).toMap
      Column.ListOf(offsets, assemble(leaves, levels, c, inner, offsets(n)), valid)

  private def chunk(in: ReadAt, leaf: Leaf, c: Struct)(using z: Compression): Levels =
    val m = c.struct(3).getOrElse(throw Refused(s"column '${leaf.name}': a chunk without metadata"))
    val codec = m.int(4).getOrElse(0L).toInt
    val total = m.int(5).getOrElse(0L)
    val data = m.int(9).getOrElse(throw Refused(s"column '${leaf.name}': no data page offset"))
    val start = m.int(11).filter(d => d > 0 && d < data).getOrElse(data)
    val size = m.int(7).getOrElse(0L)
    if size > Int.MaxValue then throw Refused(s"column '${leaf.name}': a chunk of $size bytes")
    val bytes = in.read(start, size.toInt)
    var at = 0
    var seen = 0L
    var dict: Option[Column] = None
    val reps = Array.newBuilder[Int]
    val defs = Array.newBuilder[Int]
    val parts = Vector.newBuilder[Column]
    val repWidth = Rle.width(leaf.maxRep)
    val defWidth = Rle.width(leaf.maxDef)
    def le32(b: Array[Byte], i: Int): Int =
      (b(i) & 0xff) | (b(i + 1) & 0xff) << 8 | (b(i + 2) & 0xff) << 16 | (b(i + 3) & 0xff) << 24
    while seen < total do
      if at >= bytes.length then throw Refused(s"column '${leaf.name}': its pages end before its $total values")
      val (h, body) = Thrift.read(bytes, at)
      val csize = h.int(3).getOrElse(0L).toInt
      val usize = h.int(2).getOrElse(0L).toInt
      if body + csize > bytes.length then throw Refused(s"column '${leaf.name}': a page past its chunk")
      h.int(1).getOrElse(-1L).toInt match
        case DictionaryPage =>
          val d = h.struct(7).getOrElse(throw Refused(s"column '${leaf.name}': a dictionary page without its header"))
          val page = inflate(leaf, codec, bytes, body, csize, usize)
          dict = Some(plain(leaf, page, 0, page.length, d.int(1).getOrElse(0L).toInt))
        case DataPage =>
          val d = h.struct(5).getOrElse(throw Refused(s"column '${leaf.name}': a data page without its header"))
          val n = d.int(1).getOrElse(0L).toInt
          val page = inflate(leaf, codec, bytes, body, csize, usize)
          var p = 0
          val r =
            if leaf.maxRep == 0 then new Array[Int](n)
            else { val len = le32(page, p); val v = Rle.decode(page, p + 4, p + 4 + len, repWidth, n); p += 4 + len; v }
          val dl =
            if leaf.maxDef == 0 then new Array[Int](n)
            else { val len = le32(page, p); val v = Rle.decode(page, p + 4, p + 4 + len, defWidth, n); p += 4 + len; v }
          reps ++= r; defs ++= dl
          parts += values(leaf, d.int(2).getOrElse(0L).toInt, page, p, page.length, dl.count(_ == leaf.maxDef), dict)
          seen += n
        case DataPageV2 =>
          val d = h.struct(8).getOrElse(throw Refused(s"column '${leaf.name}': a v2 data page without its header"))
          val n = d.int(1).getOrElse(0L).toInt
          val defLen = d.int(5).getOrElse(0L).toInt
          val repLen = d.int(6).getOrElse(0L).toInt
          val r = if leaf.maxRep == 0 || repLen == 0 then new Array[Int](n) else Rle.decode(bytes, body, body + repLen, repWidth, n)
          val dl =
            if leaf.maxDef == 0 || defLen == 0 then Array.fill(n)(leaf.maxDef)
            else Rle.decode(bytes, body + repLen, body + repLen + defLen, defWidth, n)
          val from = body + repLen + defLen
          val compressed = d.bool(7).getOrElse(true)
          val page =
            if compressed then inflate(leaf, codec, bytes, from, csize - repLen - defLen, usize - repLen - defLen)
            else java.util.Arrays.copyOfRange(bytes, from, body + csize)
          reps ++= r; defs ++= dl
          parts += values(leaf, d.int(4).getOrElse(0L).toInt, page, 0, page.length, dl.count(_ == leaf.maxDef), dict)
          seen += n
        case _ => ()                      // an index page, or one this reader need not read
      at = body + csize
    val all = parts.result().filter(_.length > 0)
    Levels(reps.result(), defs.result(), if all.isEmpty then empty(leaf.kind, 0) else Column.concat(all), leaf.maxDef)

  private def inflate(leaf: Leaf, codec: Int, bytes: Array[Byte], from: Int, len: Int, usize: Int)
                     (using z: Compression): Array[Byte] =
    val raw = java.util.Arrays.copyOfRange(bytes, from, from + len)
    val out = codec match
      case 0 => raw
      case 1 => z.snappy.decompress(raw)
      case 6 => z.zstd.decompress(raw)
      case 2 => Gzip.inflate(raw)
      case other =>
        val named = Vector("UNCOMPRESSED", "SNAPPY", "GZIP", "LZO", "BROTLI", "LZ4", "ZSTD", "LZ4_RAW").lift(other).getOrElse(other.toString)
        throw Refused(s"column '${leaf.name}' is compressed with $named: read are UNCOMPRESSED, SNAPPY, GZIP and ZSTD (specs/parquet.md)")
    if out.length != usize then throw Refused(s"column '${leaf.name}': a page of ${out.length} bytes declaring $usize")
    out

  /** a page's `present` values, all valid */
  private def values(leaf: Leaf, encoding: Int, page: Array[Byte], from: Int, until: Int,
                     present: Int, dict: Option[Column]): Column =
    encoding match
      case Plain => plain(leaf, page, from, until, present)
      case PlainDictionary | RleDictionary =>
        if present == 0 then empty(leaf.kind, 0)
        else
          val d = dict.getOrElse(throw Refused(s"column '${leaf.name}': dictionary indices before its dictionary"))
          val width = page(from) & 0xff
          val idx = Rle.decode(page, from + 1, until, width, present)
          if idx.exists(i => i < 0 || i >= d.length) then throw Refused(s"column '${leaf.name}': an index outside its dictionary")
          d.take(idx, Array.fill(present)(true))
      // booleans RLE-encoded (pyarrow's data page v2): a 4-byte length,
      // then the hybrid at width one
      case RleEncoding if leaf.kind == KBool =>
        if until - from < 4 then throw Refused(s"column '${leaf.name}': RLE booleans cut short")
        val len = (page(from) & 0xff) | (page(from + 1) & 0xff) << 8 | (page(from + 2) & 0xff) << 16 | (page(from + 3) & 0xff) << 24
        val bits = Rle.decode(page, from + 4, math.min(until, from + 4 + len), 1, present)
        Column.Bool(bits.map(_ == 1), Array.fill(present)(true))
      case DeltaBinaryPacked if leaf.physical == TInt32 || leaf.physical == TInt64 =>
        val (v, _) = Encodings.deltaInts(page, from, until, bits32 = leaf.physical == TInt32)
        if v.length < present then throw Refused(s"column '${leaf.name}': ${v.length} DELTA values for $present")
        longs(leaf, v.take(present))
      case DeltaLengthByteArray if leaf.physical == TByteArray =>
        byteValues(leaf, Encodings.deltaLengths(page, from, until, present))
      case DeltaByteArray if leaf.physical == TByteArray || leaf.physical == TFixed =>
        byteValues(leaf, Encodings.deltaStrings(page, from, until, present))
      case ByteStreamSplit =>
        val width = leaf.physical match
          case TFloat | TInt32 => 4
          case TDouble | TInt64 => 8
          case TFixed => leaf.width
          case other => throw Refused(s"column '${leaf.name}': BYTE_STREAM_SPLIT over physical type $other")
        val raw = Encodings.unsplit(page, from, until, present, width)
        plain(leaf, raw, 0, raw.length, present)
      case other =>
        val named = Map(5 -> "DELTA_BINARY_PACKED", 6 -> "DELTA_LENGTH_BYTE_ARRAY", 7 -> "DELTA_BYTE_ARRAY",
          3 -> "RLE (outside booleans)").getOrElse(other, other.toString)
        throw Refused(s"column '${leaf.name}' is encoded $named for its physical type: not read (specs/parquet.md)")

  /** integers decoded by an encoding, as the leaf's kind */
  private def longs(leaf: Leaf, v: Array[Long]): Column =
    val ok = Array.fill(v.length)(true)
    leaf.kind match
      case KInt32(bits, signed) => Column.Ints(bits, signed, if signed then v else v.map(_ & 0xffffffffL), ok)
      case KDate => Column.Date32(v.map(_.toInt), ok)
      case KInt64(true) => Column.Int64(v, ok)
      case KInt64(false) => Column.Ints(64, false, v, ok)
      case KTimestamp(u, utc) => Column.Timestamp(u, if utc then Some("UTC") else None, v, ok)
      case KDecimal(p, s) => Column.Decimal(p, s, v.map(BigInt(_)), ok)
      case other => throw Refused(s"column '${leaf.name}': integers for a ${other}")

  /** byte strings decoded by an encoding, as the leaf's kind */
  private def byteValues(leaf: Leaf, v: Array[Array[Byte]]): Column =
    val ok = Array.fill(v.length)(true)
    leaf.kind match
      case KString => Column.Utf8(v.map(String(_, "UTF-8")), ok)
      case KBinary => Column.Binary(v, ok)
      case KFixed(w) => Column.FixedBinary(w, v, ok)
      case KDecimal(p, s) => Column.Decimal(p, s, v.map(b => if b.isEmpty then BigInt(0) else BigInt(b)), ok)
      case other => throw Refused(s"column '${leaf.name}': byte strings for a ${other}")

  /** `n` PLAIN values, all present */
  private def plain(leaf: Leaf, b: Array[Byte], from: Int, until: Int, n: Int): Column =
    val ok = Array.fill(n)(true)
    var at = from
    def need(k: Int): Unit =
      if at + k > until then throw Refused(s"column '${leaf.name}': its values end early")
    def i32(): Int =
      need(4)
      val v = (b(at) & 0xff) | (b(at + 1) & 0xff) << 8 | (b(at + 2) & 0xff) << 16 | (b(at + 3) & 0xff) << 24
      at += 4
      v
    def i64(): Long =
      need(8)
      var v = 0L
      var k = 0
      while k < 8 do { v |= (b(at + k) & 0xffL) << (8 * k); k += 1 }
      at += 8
      v
    def bytes(k: Int): Array[Byte] =
      need(k)
      val out = java.util.Arrays.copyOfRange(b, at, at + k)
      at += k
      out
    def unscaled(raw: Array[Byte]): BigInt = if raw.isEmpty then BigInt(0) else BigInt(raw)
    leaf.kind match
      case KBool =>
        need((n + 7) / 8)
        val v = Array.tabulate(n)(i => ((b(at + (i >>> 3)) >>> (i & 7)) & 1) == 1)
        Column.Bool(v, ok)
      case KInt32(bits, signed) =>
        Column.Ints(bits, signed, Array.fill(n) { val v = i32(); if signed then v.toLong else v & 0xffffffffL }, ok)
      case KDate => Column.Date32(Array.fill(n)(i32()), ok)
      case KInt64(true) => Column.Int64(Array.fill(n)(i64()), ok)
      case KInt64(false) => Column.Ints(64, false, Array.fill(n)(i64()), ok)
      case KTimestamp(u, utc) => Column.Timestamp(u, if utc then Some("UTC") else None, Array.fill(n)(i64()), ok)
      case KInt96 =>
        Column.Timestamp(TimeUnit.Nano, Some("UTC"), Array.fill(n) {
          val nanos = i64()
          val day = i32()
          (day - 2440588L) * 86400000000000L + nanos
        }, ok)
      case KFloat => Column.Float32(Array.fill(n)(java.lang.Float.intBitsToFloat(i32())), ok)
      case KDouble => Column.Float64(Array.fill(n)(java.lang.Double.longBitsToDouble(i64())), ok)
      case KString => Column.Utf8(Array.fill(n)(String(bytes(i32()), "UTF-8")), ok)
      case KBinary => Column.Binary(Array.fill(n)(bytes(i32())), ok)
      case KFixed(w) => Column.FixedBinary(w, Array.fill(n)(bytes(w)), ok)
      case KDecimal(p, s) =>
        val v = leaf.physical match
          case TInt32 => Array.fill(n)(BigInt(i32()))
          case TInt64 => Array.fill(n)(BigInt(i64()))
          case TFixed => Array.fill(n)(unscaled(bytes(leaf.width)))
          case _ => Array.fill(n)(unscaled(bytes(i32())))
        Column.Decimal(p, s, v, ok)

  // ------------------------------------------------------------- writing

  /** rows per data page */
  val PageRows: Int = 64 * 1024

  /** a leaf being written: its path, its column of values, its levels' maxima */
  private final case class Out(path: Vector[String], values: Column, maxDef: Int, maxRep: Int)

  /** the shape of a column we WRITE, and its leaves: every node
   * optional, a list the three-level standard */
  private def writing(name: String, c: Column): (Shape, Vector[Out]) =
    val outs = Vector.newBuilder[Out]
    var count = 0
    def go(path: Vector[String], c: Column, d0: Int, r0: Int): Shape =
      val d = d0 + 1
      c match
        case Column.Struct(fs, _) => SStruct(fs.map((k, f) => k -> go(path :+ k, f, d, r0)), d)
        case Column.ListOf(_, child, _) =>
          SList(go(path :+ "list" :+ "element", child, d + 1, r0 + 1), d, d + 1, r0 + 1)
        case leafCol =>
          typeOf(path.mkString("."), leafCol): Unit
          outs += Out(path, leafCol, d, r0)
          count += 1
          SLeaf(count - 1, d)
    val s = go(Vector(name), c, 0, 0)
    (s, outs.result())

  /**
   * THE LEVELS OF A COLUMN, leaf by leaf (Dremel's shredding): for each
   * row, each leaf gets an entry per value or per absence, the first of
   * a row at repetition level zero; a leaf's present values are indices
   * into its own column.
   */
  private def shred(s: Shape, c: Column, leaves: Int): (Array[Array[Int]], Array[Array[Int]], Array[Array[Int]]) =
    val reps = Array.fill(leaves)(Array.newBuilder[Int])
    val defs = Array.fill(leaves)(Array.newBuilder[Int])
    val idx = Array.fill(leaves)(Array.newBuilder[Int])
    def absent(s: Shape, r: Int, d: Int): Unit =
      leavesOf(s).foreach(l => { reps(l) += r; defs(l) += d })
    def emit(s: Shape, c: Column, i: Int, r: Int, d: Int): Unit = (s, c) match
      case (SLeaf(l, maxDef), col) =>
        reps(l) += r
        if col.validity(i) then { defs(l) += maxDef; idx(l) += i } else defs(l) += d
      case (SStruct(fs, present), Column.Struct(cols, valid)) =>
        if !valid(i) then absent(s, r, d)
        else fs.zip(cols).foreach { case ((_, f), (_, fc)) => emit(f, fc, i, r, present) }
      case (SList(child, present, element, rep), Column.ListOf(offsets, cc, valid)) =>
        if !valid(i) then absent(s, r, d)
        else if offsets(i) == offsets(i + 1) then absent(s, r, present)
        else
          var j = offsets(i)
          while j < offsets(i + 1) do
            emit(child, cc, j, if j == offsets(i) then r else rep, element)
            j += 1
      case (other, col) => throw Refused(s"a ${Column.describe(col)} where the schema has $other")
    var i = 0
    while i < c.length do { emit(s, c, i, 0, 0); i += 1 }
    (reps.map(_.result()), defs.map(_.result()), idx.map(_.result()))

  def writer(out: Array[Byte] => Unit, compress: Compress = Compress.Snappy,
             metadata: Vector[(String, String)] = Vector.empty)
            (using z: Compression): ParquetWriter = new ParquetWriter:
    private var at = 0L
    private var schema: Option[Vector[(String, Column)]] = None
    private val groups = Vector.newBuilder[Struct]
    private var rows = 0L
    private var closed = false

    private def emit(b: Array[Byte]): Unit = { out(b); at += b.length }
    emit(Magic)

    def append(t: Table): Unit =
      if closed then throw IllegalStateException("a Parquet writer appended to after close")
      schema match
        case None => schema = Some(t.cols)
        case Some(first) =>
          val a = first.map((n, c) => n -> Column.describe(c))
          val b = t.cols.map((n, c) => n -> Column.describe(c))
          if a != b then throw Refused(s"a row group of columns $b after $a: every group of a file has one schema")
      val chunks = t.cols.flatMap { (name, c) =>
        val (s, outs) = writing(name, c)
        val (reps, defs, idx) = shred(s, c, outs.length)
        outs.indices.map(l => column(outs(l), reps(l), defs(l), idx(l)))
      }
      groups += Struct.of(
        1 -> Value.L(12, chunks.map(Value.S(_))),
        2 -> Value.I(chunks.map(_.struct(3).flatMap(_.int(6)).getOrElse(0L)).sum),
        3 -> Value.I(t.rows.toLong))
      rows += t.rows

    /** one leaf's column chunk: data pages of about `PageRows` rows, each
     * beginning at a row (repetition level zero) */
    private def column(o: Out, reps: Array[Int], defs: Array[Int], idx: Array[Int]): Struct =
      val (physical, _, _) = typeOf(o.path.mkString("."), o.values)
      val start = at
      var uncompressed = 0L
      val rowStarts = (0 until reps.length).filter(reps(_) == 0).toVector
      val cuts = (rowStarts.indices by PageRows).map(rowStarts(_)).toVector :+ reps.length
      val bounds = if cuts.length == 1 then Vector((0, 0)) else cuts.sliding(2).map(v => (v(0), v(1))).toVector
      var vi = 0
      for (from, until) <- bounds do
        val present = (from until until).count(e => defs(e) == o.maxDef)
        val page = dataPage(o, reps, defs, from, until, idx.slice(vi, vi + present))
        vi += present
        val body = compress match
          case Compress.None => page
          case Compress.Snappy => z.snappy.compress(page)
          case Compress.Zstd => z.zstd.compress(page)
        val header = Thrift.write(Struct.of(
          1 -> Value.I32(DataPage),
          2 -> Value.I32(page.length),
          3 -> Value.I32(body.length),
          5 -> Value.S(Struct.of(
            1 -> Value.I32(until - from),
            2 -> Value.I32(Plain),
            3 -> Value.I32(RleEncoding),
            4 -> Value.I32(RleEncoding)))))
        emit(header)
        emit(body)
        uncompressed += header.length + page.length
      Struct.of(
        2 -> Value.I(start),
        3 -> Value.S(Struct.of(
          1 -> Value.I32(physical),
          2 -> Value.L(5, Vector(Value.I32(Plain), Value.I32(RleEncoding))),
          3 -> Value.L(8, o.path.map(p => Value.Bin(p.getBytes("UTF-8")))),
          4 -> Value.I32(compress match { case Compress.None => 0; case Compress.Snappy => 1; case Compress.Zstd => 6 }),
          5 -> Value.I(reps.length.toLong),
          6 -> Value.I(uncompressed),
          7 -> Value.I(at - start),
          9 -> Value.I(start))))

    def close(): Unit =
      if !closed then
        closed = true
        val cols = schema.getOrElse(Vector.empty)
        val elements = Value.S(Struct.of(4 -> Value.Bin("schema".getBytes("UTF-8")), 5 -> Value.I32(cols.length))) +:
          cols.flatMap((name, c) => elementsOf(name, c).map(Value.S(_)))
        val kv = metadata.map((k, v) => Value.S(Struct.of(1 -> Value.Bin(k.getBytes("UTF-8")), 2 -> Value.Bin(v.getBytes("UTF-8")))))
        val footer = Thrift.write(Struct(Map(
          1 -> Value.I32(1),
          2 -> Value.L(12, elements),
          3 -> Value.I(rows),
          4 -> Value.L(12, groups.result().map(Value.S(_))),
          6 -> Value.Bin("okay-parquet".getBytes("UTF-8"))) ++
          (if kv.isEmpty then Map.empty else Map(5 -> Value.L(12, kv)))))
        emit(footer)
        emit(Array((footer.length & 0xff).toByte, ((footer.length >>> 8) & 0xff).toByte,
          ((footer.length >>> 16) & 0xff).toByte, ((footer.length >>> 24) & 0xff).toByte))
        emit(Magic)

  /** (physical type, converted type, logical type) of a leaf we write */
  private def typeOf(name: String, c: Column): (Int, Option[Int], Option[Struct]) =
    def integer(bits: Int, signed: Boolean) =
      Some(Struct.of(10 -> Value.S(Struct.of(1 -> Value.I8(bits.toByte), 2 -> Value.Bool(signed)))))
    c match
      case _: Column.Bool => (TBoolean, None, None)
      case _: Column.Int64 => (TInt64, None, None)
      case Column.Ints(bits, signed, _, _) if bits <= 32 =>
        val conv = (bits, signed) match
          case (8, true) => 15; case (16, true) => 16; case (32, true) => 17
          case (8, false) => 11; case (16, false) => 12; case _ => 13
        (TInt32, Some(conv), integer(bits, signed))
      case Column.Ints(64, signed, _, _) => (TInt64, Some(if signed then 18 else 14), integer(64, signed))
      case _: Column.Float32 => (TFloat, None, None)
      case _: Column.Float64 => (TDouble, None, None)
      case _: Column.Utf8 => (TByteArray, Some(0), Some(Struct.of(1 -> Value.S(Struct.of()))))
      case _: Column.Binary => (TByteArray, None, None)
      case _: Column.FixedBinary => (TFixed, None, None)
      case _: Column.Date32 => (TInt32, Some(6), Some(Struct.of(6 -> Value.S(Struct.of()))))
      case Column.Timestamp(unit, zone, _, _) =>
        val u = unit match
          case TimeUnit.Milli => 1
          case TimeUnit.Micro => 2
          case TimeUnit.Nano => 3
          case TimeUnit.Second =>
            throw Refused(s"column '$name' is a timestamp in seconds: Parquet has millis, micros and nanos")
        val conv = if zone.isDefined && u == 1 then Some(9) else if zone.isDefined && u == 2 then Some(10) else None
        (TInt64, conv, Some(Struct.of(8 -> Value.S(Struct.of(
          1 -> Value.Bool(zone.isDefined),
          2 -> Value.S(Struct.of(u -> Value.S(Struct.of()))))))))
      case other =>
        throw Refused(s"column '$name' is ${Column.describe(other)}: not written yet (specs/parquet.md)")

  /** a column's schema elements, pre-order: a struct an optional group,
   * a list the three-level standard, a leaf an optional primitive */
  private def elementsOf(name: String, c: Column): Vector[Struct] =
    def named(n: String) = 4 -> Value.Bin(n.getBytes("UTF-8"))
    c match
      case Column.Struct(fs, _) =>
        Struct.of(named(name), 3 -> Value.I32(Optional), 5 -> Value.I32(fs.length)) +:
          fs.flatMap((k, f) => elementsOf(k, f))
      case Column.ListOf(_, child, _) =>
        Vector(
          Struct.of(named(name), 3 -> Value.I32(Optional), 5 -> Value.I32(1), 6 -> Value.I32(3),
            10 -> Value.S(Struct.of(3 -> Value.S(Struct.of())))),
          Struct.of(named("list"), 3 -> Value.I32(Repeated), 5 -> Value.I32(1))) ++
          elementsOf("element", child)
      case leafCol =>
        val (physical, conv, logical) = typeOf(name, leafCol)
        Vector(Struct(Map[Int, Value](
          1 -> Value.I32(physical),
          3 -> Value.I32(Optional),
          named(name)) ++
          conv.map(6 -> Value.I32(_)) ++
          logical.map(10 -> Value.S(_)) ++
          (leafCol match { case Column.FixedBinary(w, _, _) => Map(2 -> Value.I32(w)); case _ => Map.empty })))

  /** one v1 data page, uncompressed: repetition levels, definition
   * levels, then the present values PLAIN */
  private def dataPage(o: Out, reps: Array[Int], defs: Array[Int], from: Int, until: Int, present: Array[Int]): Array[Byte] =
    val out = java.io.ByteArrayOutputStream()
    def le32(v: Int): Unit = { out.write(v & 0xff); out.write((v >>> 8) & 0xff); out.write((v >>> 16) & 0xff); out.write((v >>> 24) & 0xff) }
    def le64(v: Long): Unit = { var k = 0; while k < 8 do { out.write(((v >>> (8 * k)) & 0xff).toInt); k += 1 } }
    if o.maxRep > 0 then
      val levels = Rle.encode(reps.slice(from, until), Rle.width(o.maxRep))
      le32(levels.length); out.write(levels)
    if o.maxDef > 0 then
      val levels = Rle.encode(defs.slice(from, until), Rle.width(o.maxDef))
      le32(levels.length); out.write(levels)
    val name = o.path.mkString(".")
    o.values match
      case Column.Bool(v, _) =>
        val bits = new Array[Byte]((present.length + 7) / 8)
        for (i, k) <- present.zipWithIndex if v(i) do bits(k >>> 3) = (bits(k >>> 3) | (1 << (k & 7))).toByte
        out.write(bits)
      case Column.Int64(v, _) => present.foreach(i => le64(v(i)))
      case Column.Ints(bits, _, v, _) => if bits <= 32 then present.foreach(i => le32(v(i).toInt)) else present.foreach(i => le64(v(i)))
      case Column.Float32(v, _) => present.foreach(i => le32(java.lang.Float.floatToRawIntBits(v(i))))
      case Column.Float64(v, _) => present.foreach(i => le64(java.lang.Double.doubleToRawLongBits(v(i))))
      case Column.Utf8(v, _) => present.foreach { i => val b = v(i).getBytes("UTF-8"); le32(b.length); out.write(b) }
      case Column.Binary(v, _) => present.foreach { i => le32(v(i).length); out.write(v(i)) }
      case Column.FixedBinary(w, v, _) => present.foreach { i =>
        if v(i).length != w then throw Refused(s"column '$name': a value of ${v(i).length} bytes in a fixed($w)")
        out.write(v(i)) }
      case Column.Date32(v, _) => present.foreach(i => le32(v(i)))
      case Column.Timestamp(_, _, v, _) => present.foreach(i => le64(v(i)))
      case other => throw Refused(s"column '$name' is ${Column.describe(other)}: not written yet (specs/parquet.md)")
    out.toByteArray
