package okay.spark


import okay.{Tables}
import okay.freer.*
import okay.std.*
import okay.freer.given
import okay.std.given
import okay.Tables.{Heap, Table}
import okay.codec.Schema
import okay.sql.{Query, SqlValue, Structured}
import okay.spark.SparkBulk.{Rows, SparkBulk}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Column, DataFrame, SparkSession}
import org.apache.spark.sql.functions.{col, lit, struct}

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
 * DECODING is `SparkValues`: Schema-driven straight over Spark's values,
 * exact for numbers, reading a recursive type from its CBOR, a VARIANT
 * typed and a MAP as its entries (spark-values-exact).
 */
final class SparkFrames(spark: SparkSession, bulk: SparkBulk):
  private type H = Heap[Rows]

  private val born = java.util.Collections.synchronizedMap(new java.util.IdentityHashMap[RDD[Any], DataFrame]())

  private def rowsOf[A](df: DataFrame)(using Schema[A]): RDD[Any] =
    val dec = SparkValues.decoder[A](df.schema)
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
        // encoded by `Columns` ON THE EXECUTORS, one table per partition:
        // exact (a Long stays a Long, a recursive value its (cbor, json)),
        // and only the schema — serializable — crosses to them
        val st = SparkSchema.structOf[A]
        val rows = r.mapPartitions { it =>
          val (fs, toRow) = okay.codec.Columns.table[A](using s)
          it.map(x => SparkSchema.rowOf(fs, toRow(SparkFrames.elem[A](x))))
        }
        spark.createDataFrame(rows, st)

  // ------------------------------------------------------------ the handler
  /** `Structured` answered on Spark: in Catalyst where the table is
   * DataFrame-born, through the RDD otherwise */
  def structured[A, F[+_]](p: A ! Structured + F): A ! State % H + F =
    import okay.freer.Row.plus
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
    else h.hold[A](SparkBulk.rows[A](r.filter(x => w.test(SparkFrames.elem[A](x)))))

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
      val (decA, decB) = (SparkValues.decoder[A](dl.schema), SparkValues.decoder[B](dr.schema))
      val out: RDD[Any] = j.rdd.map(row => (decA(row.getStruct(0)), decB(row.getStruct(1))): Any)
      born.put(out, j): Unit
      h.hold[(A, B)](SparkBulk.rows[(A, B)](out))
    else
      val kl = okay.sql.Structured.key(sa, li)
      val kr = okay.sql.Structured.key(sb, ri)
      val lp: RDD[(Any, Any)] = rl.map(x => (kl(SparkFrames.elem[A](x)): Any, x))
      val rp: RDD[(Any, Any)] = rr.map(x => (kr(SparkFrames.elem[B](x)): Any, x))
      h.hold[(A, B)](SparkBulk.rows[(A, B)](RDD.rddToPairRDDFunctions(lp).join(rp).map((_, ab) => ab: Any)))

  /** a program in `Tables + Structured`, run on Spark; it may also use
   * `load`/`read`/`frame`, whose row is the heap's `State` */
  def run[A](p: A ! Tables + Structured + State % H): A =
    // the rest of the row named: through the bridge's match type `via` cannot solve its `F` from `p`
    val tabled: A ! Structured + State % H = Tables.via[A, Rows, Structured + State % H](bulk)(p)
    State.run(Heap.empty[Rows])(structured(tabled))._2

object SparkFrames:
  /** THE ONE CAST, as `SparkBulk`'s: the seam's RDD holds `Any`, and an
   * element of a `Rows[A]` is an `A` by construction (specs/bulk.md, "No
   * per-element evidence") */
  private[spark] def elem[A](x: Any): A = x.asInstanceOf[A]
