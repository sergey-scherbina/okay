package okay.spark

import okay.*
import okay.Tables.{Heap, Table}
import okay.codec.{Json, Schema}
import okay.sql.{Query, SqlValue, Structured}
import okay.spark.SparkBulk.{Rows, SparkBulk}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Column, DataFrame, Row as SRow, SparkSession}
import org.apache.spark.sql.functions.{col, lit, struct}
import org.apache.spark.sql.types.*

/**
 * DATAFRAMES IN A `Tables` PROGRAM (specs/streams-seam.md, lane 5): the
 * compatibility half and the Catalyst half of the structural operators,
 * on the heap `SparkBulk` threads.
 *
 * COMPATIBILITY. `load[A](df)` brings a DataFrame into a program as a
 * `Table[A]`, its rows decoded by `Schema[A]`; `read[A](path, format)`
 * has Spark read a Parquet/CSV/JSON file pruned to A's fields;
 * `frame[A](t)` hands a table back as a DataFrame — the table's own if it
 * is still DataFrame-born, else its rows encoded by `SparkSchema`.
 *
 * CATALYST. A table that was born a DataFrame and has met only
 * structural operators IS a DataFrame: `matching(w)` compiles the
 * `Query.Where` to a `Column` and filters there (on Parquet the filter
 * reaches the reader as `PushedFilters`), `joinOn(lf, rf)` joins the two
 * DataFrames on the named columns. The first opaque step (`select(f)`,
 * `where(p)`) leaves Catalyst: its RDD is the decoded rows, and from
 * there the table is the seam's like any other. A structural operator on
 * a table that is not DataFrame-born takes `Structured.viaTables`' road
 * on the RDD — the same answer, no Catalyst.
 *
 * How a table is known to be DataFrame-born: the RDD the heap holds for
 * it is registered here with its DataFrame (by identity). A forced table
 * whose plan is exactly that held slot gets the same RDD back, so the
 * registry answers; any opaque step compiles a new RDD, which it does not.
 *
 * DECODING is Spark Row → okay `Json` → `Json.decode(Schema[A])`: exact
 * for products of numbers, text, booleans, options and sequences, which
 * is where `Columns` (the encoder) and the JSON codec agree; a sum type,
 * a `BigInt` or a JSON-typed field is refused by name at `load`.
 */
final class SparkFrames(spark: SparkSession, bulk: SparkBulk):
  private type H = Heap[Rows]

  private val born = java.util.Collections.synchronizedMap(new java.util.IdentityHashMap[RDD[Any], DataFrame]())

  private def rowsOf[A](df: DataFrame)(using Schema[A]): RDD[Any] =
    val dec = SparkFrames.decoder[A](df.schema)
    val r: RDD[Any] = df.rdd.map(row => dec(row): Any)(using scala.reflect.ClassTag.Any)
    born.put(r, df): Unit
    r

  // ------------------------------------------------------------ Pred → Column
  private def literal(v: SqlValue): Any = v match
    case SqlValue.Null => null
    case SqlValue.Bool(b) => b
    case SqlValue.I32(n) => n
    case SqlValue.I64(n) => n
    case SqlValue.F64(n) => n
    case SqlValue.Text(s) => s
    case SqlValue.Num(d) => d.bigDecimal
    case SqlValue.Bytes(b) => b
    case SqlValue.Timestamp(micros) => java.sql.Timestamp.from(java.time.Instant.EPOCH.plusNanos(micros * 1000L))
    case SqlValue.Date(days) => java.sql.Date.valueOf(java.time.LocalDate.ofEpochDay(days.toLong))
    case SqlValue.Time(micros) => java.time.LocalTime.ofNanoOfDay(micros * 1000L).toString
    case SqlValue.Uuid(u) => u.toString
    case SqlValue.Json(text) => text
    case SqlValue.Arr(_) | SqlValue.Row(_) => throw IllegalArgumentException("SparkFrames: an array or row literal is not compared")

  private def leaf(p: Query.Pred): Column = p match
    case Query.Pred.True => lit(true)
    case Query.Pred.Null(field, _, _, isNull) => if isNull then col(field).isNull else col(field).isNotNull
    case Query.Pred.Cmp(field, _, _, op, value) =>
      val c = col(field)
      val v = literal(value)
      op match
        case Query.Op.Eq => c === v
        case Query.Op.Ne => c =!= v
        case Query.Op.Lt => c < v
        case Query.Op.Le => c <= v
        case Query.Op.Gt => c > v
        case Query.Op.Ge => c >= v
        case Query.Op.Like => c.like(v.toString)
    case other => throw IllegalStateException(s"SparkFrames: $other is not a leaf")

  /** the predicate as Catalyst's: fields by their Scala names, the names
   * `SparkSchema` gives the columns. An explicit stack, post-order, as
   * `Query.eval` walks the same tree: a predicate built by a fold of
   * `and`s is as deep as it is long */
  def column(p: Query.Pred): Column =
    val todo = scala.collection.mutable.Stack[(Query.Pred, Boolean)]((p, false))
    val out = scala.collection.mutable.Stack[Column]()
    while todo.nonEmpty do
      val (q, built) = todo.pop()
      if built then q match
        case Query.Pred.And(_, _) => { val r = out.pop(); val l = out.pop(); out.push(l && r) }
        case Query.Pred.Or(_, _) => { val r = out.pop(); val l = out.pop(); out.push(l || r) }
        case _ => out.push(!out.pop())
      else q match
        case Query.Pred.And(l, r) => todo.push((q, true)); todo.push((r, false)); todo.push((l, false))
        case Query.Pred.Or(l, r) => todo.push((q, true)); todo.push((r, false)); todo.push((l, false))
        case Query.Pred.Not(x) => todo.push((q, true)); todo.push((x, false))
        case other => out.push(leaf(other))
    out.pop()

  // ------------------------------------------------------------ doors
  /** a DataFrame as a table of the program, decoded by `Schema[A]` */
  def load[A](df: DataFrame)(using Schema[A]): Table[A] ! State % H =
    State.update[H, Table[A]](h => h.hold[A](SparkBulk.rows[A](rowsOf[A](df))))

  /** a file read by Spark, pruned to A's fields; `format` is Spark's
   * (`parquet`, `csv` with a header, `json`) */
  def read[A](path: String, format: String)(using s: Schema[A]): Table[A] ! State % H =
    val names = okay.codec.Columns.fields[A].map(_.name)
    val raw = format match
      case "csv" => spark.read.option("header", "true").option("inferSchema", "false").schema(SparkSchema.structOf[A]).csv(path)
      case other => spark.read.format(other).load(path)
    load[A](raw.select(names.map(col)*))

  /** a table as a DataFrame: its own when it is DataFrame-born, else its
   * rows encoded (`SparkSchema`) */
  def frame[A](t: Table[A])(using s: Schema[A]): DataFrame ! State % H =
    State.get[H].map: h =>
      val r = SparkBulk.rdd(h.force(t)(bulk))
      val df = born.get(r)
      if df != null then df
      else
        // encoded on the executors as value -> Json -> Row by the struct:
        // `Columns.table`'s row function is not serializable, the schema is
        val st = SparkFrames.checked(SparkSchema.structOf[A])
        val rows = r.map(x => SparkFrames.row(Json.parse(Json.encode(s)(x.asInstanceOf[A])), st))
        spark.createDataFrame(rows, st)

  // ------------------------------------------------------------ the handler
  /** `Structured` answered on Spark: in Catalyst where the table is
   * DataFrame-born, through the RDD otherwise */
  def structured[A, F[+_]](p: A ! Structured + F): A ! State % H + F =
    import okay.Row.plus
    !.interpret(p):
      [X] => (e: Structured[X]) => e match
        case Structured.Matching(t, w, s) =>
          State.update[H, X](h => matching(h, t, w)(using s)).plus[F]
        case Structured.JoinOn(l, r, lf, rf, sa, sb) =>
          State.update[H, X](h => joinOn(h, l, r, lf.name, rf.name, lf.idx, rf.idx)(using sa, sb)).plus[F]

  private def matching[A](h: H, t: Table[A], w: Query.Where[A])(using s: Schema[A]): (Table[A], H) =
    val r = SparkBulk.rdd(h.force(t)(bulk))
    val df = born.get(r)
    if df != null then h.hold[A](SparkBulk.rows[A](rowsOf[A](df.filter(column(w.pred)))))
    else h.hold[A](SparkBulk.rows[A](r.filter(x => w.test(x.asInstanceOf[A]))))

  private def joinOn[A, B](h: H, l: Table[A], r: Table[B], lf: String, rf: String, li: Int, ri: Int)
                          (using sa: Schema[A], sb: Schema[B]): (Table[(A, B)], H) =
    val rl = SparkBulk.rdd(h.force(l)(bulk))
    val rr = SparkBulk.rdd(h.force(r)(bulk))
    val dl = born.get(rl)
    val dr = born.get(rr)
    given scala.reflect.ClassTag[Any] = scala.reflect.ClassTag.Any
    if dl != null && dr != null then
      val (a, b) = (dl.alias("l"), dr.alias("r"))
      val j = a.join(b, col(s"l.$lf") === col(s"r.$rf"))
        .select(struct(dl.columns.map(c => col(s"l.`$c`"))*).as("_1"), struct(dr.columns.map(c => col(s"r.`$c`"))*).as("_2"))
      val (decA, decB) = (SparkFrames.decoder[A](dl.schema), SparkFrames.decoder[B](dr.schema))
      val out: RDD[Any] = j.rdd.map(row => (decA(row.getStruct(0)), decB(row.getStruct(1))): Any)
      born.put(out, j): Unit
      h.hold[(A, B)](SparkBulk.rows[(A, B)](out))
    else
      val kl = okay.sql.Structured.key(sa, li)
      val kr = okay.sql.Structured.key(sb, ri)
      val lp: RDD[(Any, Any)] = rl.map(x => (kl(x.asInstanceOf[A]): Any, x))
      val rp: RDD[(Any, Any)] = rr.map(x => (kr(x.asInstanceOf[B]): Any, x))
      h.hold[(A, B)](SparkBulk.rows[(A, B)](RDD.rddToPairRDDFunctions(lp).join(rp).map((_, ab) => ab: Any)))

  /** a program in `Tables + Structured`, run on Spark; it may also use
   * `load`/`read`/`frame`, whose row is the heap's `State` */
  def run[A](p: A ! Tables + Structured + State % H): A =
    State.run(Heap.empty[Rows])(structured(Tables.via(bulk)(p)))._2

/** the decoding, as functions of a serializable object: a closure an
 * executor runs must not capture the `SparkFrames` that holds the session */
object SparkFrames extends Serializable:
  // ------------------------------------------------------------ decoding
  private[spark] def json(v: Any, t: DataType): Json = (v, t) match
    case (null, _) => Json.JNull
    case (b: Boolean, _) => Json.JBool(b)
    case (n: Int, _) => Json.JNum(n.toDouble)
    case (n: Long, _) => Json.JNum(n.toDouble)
    case (n: Double, _) => Json.JNum(n)
    case (n: Float, _) => Json.JNum(n.toDouble)
    case (n: Short, _) => Json.JNum(n.toDouble)
    case (d: java.math.BigDecimal, _) => Json.JNum(d.doubleValue)
    case (s: String, _) => Json.JStr(s)
    case (r: SRow, st: StructType) => obj(r, st)
    case (xs: scala.collection.Seq[?], ArrayType(e, _)) => Json.JArr(xs.iterator.map(json(_, e)).toVector)
    case (other, _) => Json.JErr(s"a ${other.getClass.getSimpleName} value is not decoded by SparkFrames")

  private[spark] def obj(r: SRow, st: StructType): Json =
    Json.JObj(st.fields.toVector.zipWithIndex.map((f, i) => f.name -> json(r.get(i), f.dataType)))

  /** how deep a Spark type nests: the walks below go one frame per level,
   * so every door refuses a type deeper than `SparkSchema.MaxNesting` */
  private def depth(t: DataType): Int =
    var deepest = 0
    val todo = scala.collection.mutable.Stack[(DataType, Int)]((t, 1))
    while todo.nonEmpty do
      val (d, n) = todo.pop()
      if n > deepest then deepest = n
      d match
        case st: StructType => st.fields.foreach(f => todo.push((f.dataType, n + 1)))
        case ArrayType(e, _) => todo.push((e, n + 1))
        case _ => ()
    deepest

  private[spark] def checked(st: StructType): StructType =
    if depth(st) > SparkSchema.MaxNesting then
      throw IllegalArgumentException(s"SparkFrames: a type nested deeper than ${SparkSchema.MaxNesting} is refused")
    st

  private[spark] def decoder[A](st0: StructType)(using s: Schema[A]): SRow => A =
    val st = checked(st0)
    val refuse = st.fields.find(f => f.dataType.isInstanceOf[VariantType] || f.dataType.isInstanceOf[MapType])
    refuse.foreach(f => throw IllegalArgumentException(s"SparkFrames: column `${f.name}` (${f.dataType.simpleString}) is not decoded"))
    r => Json.decode(s)(obj(r, st)).fold(why => throw IllegalStateException(s"SparkFrames: a row is not a ${s}: $why"), identity)

  /** a JSON value as a Spark Row of `st`, the decoder's inverse: fields
   * by name, numbers to the column's own type, a missing field null */
  private[spark] def row(j: Json, st: StructType): SRow =
    val fs = j match
      case Json.JObj(fs) => fs.toMap
      case other => throw IllegalArgumentException(s"SparkFrames: a row is an object, not $other")
    SRow.fromSeq(st.fields.toSeq.map(f => cell(fs.getOrElse(f.name, Json.JNull), f.dataType)))

  private def cell(j: Json, t: DataType): Any = (j, t) match
    case (Json.JNull, _) => null
    case (Json.JBool(b), _) => b
    case (Json.JNum(n), LongType) => n.toLong
    case (Json.JNum(n), IntegerType) => n.toInt
    case (Json.JNum(n), ShortType) => n.toShort
    case (Json.JNum(n), FloatType) => n.toFloat
    case (Json.JNum(n), _: DecimalType) => java.math.BigDecimal.valueOf(n)
    case (Json.JNum(n), _) => n
    case (Json.JStr(v), _) => v
    case (Json.JArr(vs), ArrayType(e, _)) => vs.map(cell(_, e))
    case (o: Json.JObj, s: StructType) => row(o, s)
    case (other, _) => throw IllegalArgumentException(s"SparkFrames: $other is not a ${t.simpleString}")
