package okay.spark



import okay.{Tables}
import okay.freer.*
import okay.std.*
import okay.given
import okay.freer.given
import okay.codec.Schema
import okay.freer.Row.plus
import okay.Chunks.elements
import okay.Tables.{collect, select}
import okay.sql.{Query, Structured}
import okay.sql.Structured.{matching, joinOn}
import org.apache.spark.sql.SparkSession

/**
 * `SparkFrames` (specs/streams-seam.md, lane 5): a DataFrame enters a
 * `Tables` program and leaves it, and the structural operators on a
 * DataFrame-born table run in Catalyst — the law is the SAME ANSWER as
 * `Structured.viaTables` on the local platform, and Catalyst's own plan
 * is read to show the operator reached it.
 */
/** top level: a case class inside the suite carries `$outer`, and Spark
 * would have to ship the suite to an executor with every row */
final case class FrameTrip(id: Long, route: String, tram: Boolean)
final case class FrameStop(trip: Long, time: String, seq: Int)
final case class Wide(id: Long, big: BigInt, name: String)
final case class Tree(label: String, kids: List[Tree])
final case class Entry(key: String, value: Long)
final case class Tagged(id: Long, tags: List[Entry])
final case class Inner(id: Long, tags: List[String])
final case class Doc(v: Inner, raw: String)
final case class Deep(v: List[List[List[Long]]], j: String)
final case class Raw(v: String)
object FrameRows:
  given Schema[FrameTrip] = Schema.derived
  given Schema[FrameStop] = Schema.derived
  given Schema[Wide] = Schema.derived
  given Schema[Tree] = Schema.derived
  given Schema[Entry] = Schema.derived
  given Schema[Tagged] = Schema.derived
  given Schema[Inner] = Schema.derived
  given Schema[Doc] = Schema.derived
  given Schema[Deep] = Schema.derived
  given Schema[Raw] = Schema.derived

class TestSparkFrames extends munit.FunSuite {
  import FrameRows.given
  type Trip = FrameTrip
  type Stop = FrameStop
  val Trip = FrameTrip
  val Stop = FrameStop
  val route = Query.field[Trip, String]("route").toOption.get
  val tram = Query.field[Trip, Boolean]("tram").toOption.get
  val tripId = Query.field[Trip, Long]("id").toOption.get
  val stopTrip = Query.field[Stop, Long]("trip").toOption.get
  val seq = Query.field[Stop, Int]("seq").toOption.get

  val javaFeature: Int = Runtime.version().feature()
  override def munitIgnore: Boolean = javaFeature == 24
  lazy val spark = SparkSession.builder().master("local[2]").appName("okay-spark-frames")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = if !munitIgnore then spark.stop()
  lazy val frames = SparkFrames(spark, SparkBulk(spark))

  val trips = Vector.tabulate(30)(i => Trip(i.toLong, s"r${i % 5}", i % 3 == 0))
  val stops = Vector.tabulate(200)(i => Stop((i * 7 % 35).toLong, f"${i % 24}%02d:${i % 60}%02d", i % 13))
  def local[A](p: A ! Tables + Structured): A = Tables.run(localBulk)(Structured.viaTables(p))

  test("matching on a loaded DataFrame answers the local answer, and the filter is in Catalyst's plan") {
    val w = (route === "r1" or route === "r3") and (tram === true)
    val got = frames.run(for
      t <- frames.load[Trip](SparkSchema.dataFrame(spark, trips)).plus[Tables + Structured]
      m <- t.matching(w).plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      df <- frames.frame[Trip](m).plus[Tables + Structured]
      rows <- m.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield (rows.elements.toVector.sortBy(_.id), df.queryExecution.analyzed.toString))
    assertEquals(got._1, local(Tables.of(trips).plus[Structured].matching(w).collect.map(_.elements.toVector.sortBy(_.id))))
    // the ANALYZED plan: over rows already in memory Catalyst's optimizer
    // evaluates the Filter itself and leaves a LocalRelation, which proves
    // it had the filter and says nothing a test can read
    assert(got._2.contains("Filter"), got._2)
  }

  test("joinOn of two loaded DataFrames is a Catalyst join, and answers the local join") {
    val got = frames.run(for
      s <- frames.load[Stop](SparkSchema.dataFrame(spark, stops)).plus[Tables + Structured]
      t <- frames.load[Trip](SparkSchema.dataFrame(spark, trips)).plus[Tables + Structured]
      j <- s.joinOn(t)(stopTrip, tripId).plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      rows <- j.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield rows.elements.toVector.sortBy((s, _) => (s.trip, s.seq, s.time)))
    val expected = local(Tables.of(stops).plus[Structured].joinOn(Tables.of(trips).plus[Structured])(stopTrip, tripId)
      .collect.map(_.elements.toVector.sortBy((s, _) => (s.trip, s.seq, s.time))))
    assertEquals(got, expected)
    assert(got.nonEmpty)
  }

  test("a Parquet read pruned to A's fields takes matching as a pushed filter") {
    val dir = java.nio.file.Files.createTempDirectory("frames").resolve("trips.parquet").toString
    SparkSchema.dataFrame(spark, trips).write.parquet(dir)
    val got = frames.run(for
      t <- frames.read[Trip](dir, "parquet").plus[Tables + Structured]
      m <- t.matching(route === "r2").plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      df <- frames.frame[Trip](m).plus[Tables + Structured]
      rows <- m.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield (rows.elements.toVector.sortBy(_.id), df.queryExecution.executedPlan.toString))
    assertEquals(got._1, trips.filter(_.route == "r2"))
    assert(got._2.contains("PushedFilters: [") && got._2.contains("EqualTo(route,r2)"), got._2)
  }

  test("after an opaque step the table leaves Catalyst, and a structural step still answers through the RDD") {
    val got = frames.run(for
      t <- frames.load[Trip](SparkSchema.dataFrame(spark, trips)).plus[Tables + Structured]
      u <- t.select(x => x.copy(route = x.route.toUpperCase)).plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
      m <- u.matching(route === "R4").plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      df <- frames.frame[Trip](m).plus[Tables + Structured]
      rows <- m.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield (rows.elements.toVector.sortBy(_.id), df.collect().length))
    assertEquals(got._1, trips.filter(_.route == "r4").map(x => x.copy(route = "R4")))
    assertEquals(got._2, got._1.length, "frame of a table that is not DataFrame-born encodes its rows")
  }

  // ------------------------------------------------ exact values (spark-values-exact)

  private type H = State % Tables.Heap[SparkBulk.Rows]
  /** load, then out through `frame` of an RDD-side copy and back: the
   * decoder and the executor-side encoder, both ways */
  private def roundTrip[A: Schema](df: org.apache.spark.sql.DataFrame): (Vector[A], Vector[A]) = frames.run(for
    t <- frames.load[A](df).plus[Tables + Structured]
    rows <- t.collect.plus[Structured + H]
    copy <- t.select(identity).plus[Structured + H]
    out <- frames.frame[A](copy).plus[Tables + Structured]
    back <- frames.load[A](out).plus[Tables + Structured]
    again <- back.collect.plus[Structured + H]
  yield (rows.elements.toVector, again.elements.toVector))

  test("a Long above 2^53 and a BigInt of 38 digits arrive exactly, both ways") {
    val xs = Vector(Wide(9007199254740993L, BigInt("12345678901234567890123456789012345678"), "a"),
                    Wide(Long.MaxValue, BigInt(-1), "b"), Wide(Long.MinValue, BigInt(0), "c"))
    val (in, out) = roundTrip[Wide](SparkSchema.dataFrame(spark, xs))
    assertEquals(in.sortBy(_.name), xs)
    assertEquals(out.sortBy(_.name), xs)
  }

  test("a recursive type arrives through its CBOR, exactly") {
    val deep = (1 to 40).foldLeft(Tree("leaf", Nil))((t, i) => Tree(s"n$i", List(t, Tree(s"x$i", Nil))))
    val xs = Vector(deep, Tree("solo", Nil))
    val (in, out) = roundTrip[Tree](SparkSchema.dataFrame(spark, xs))
    assertEquals(in.sortBy(_.label), xs.sortBy(_.label))
    assertEquals(out.sortBy(_.label), xs.sortBy(_.label))
  }

  test("a MAP column is read as the list of its entries") {
    val df = spark.sql("select 7L as id, map('a', 9007199254740993L, 'b', 2L) as tags")
    val got = frames.run(frames.load[Tagged](df).plus[Tables + Structured].flatMap(_.collect.plus[Structured + H]))
      .elements.toVector
    assertEquals(got.map(t => t.copy(tags = t.tags.sortBy(_.key))),
      Vector(Tagged(7L, List(Entry("a", 9007199254740993L), Entry("b", 2L)))))
  }

  test("a VARIANT column is read typed: an object into a product, a long exactly, anything into a String as JSON") {
    val df = spark.sql("""select parse_json('{"id": 9007199254740993, "tags": ["a", "b"]}') as v, parse_json('{"x": [1, 2]}') as raw""")
    val got = frames.run(frames.load[Doc](df).plus[Tables + Structured].flatMap(_.collect.plus[Structured + H]))
      .elements.toVector
    assertEquals(got.map(_.v), Vector(Inner(9007199254740993L, List("a", "b"))))
    assertEquals(got.map(_.raw.replace(" ", "")), Vector("""{"x":[1,2]}"""))
  }

  test("a VARIANT 900 levels deep decodes: no depth is refused on our side") {
    // a String field takes it as JSON text; the list-of-list field walks it
    val json = "[" * 900 + "7" + "]" * 900
    val df = spark.sql(s"select parse_json('$json') as v")
    val got = frames.run(frames.load[Raw](df).plus[Tables + Structured].flatMap(_.collect.plus[Structured + H]))
      .elements.toVector
    assertEquals(got.map(_.v.replace(" ", "")), Vector(json))
  }

  test("a struct/array value 300 levels deep decodes through the trampoline") {
    import org.apache.spark.sql.Row
    import org.apache.spark.sql.types.*
    import scala.jdk.CollectionConverters.*
    // Deep(v: List[List[List[Long]]]) at the top, then the rest carried in a VARIANT-free
    // nesting of arrays: the decoder descends each level as a deferred step
    var t: DataType = LongType
    var v: Any = 7L
    for _ <- 1 to 3 do { t = ArrayType(t, true); v = Seq(v) }
    val st = StructType(Seq(StructField("v", t, true), StructField("j", StringType, true)))
    val df = spark.createDataFrame(List(Row(v, "x")).asJava, st)
    val got = frames.run(frames.load[Deep](df).plus[Tables + Structured].flatMap(_.collect.plus[Structured + H]))
      .elements.toVector
    assertEquals(got, Vector(Deep(List(List(List(7L))), "x")))
    // and the decoder itself, 300 levels of arrays, on a small stack
    var tt: DataType = LongType
    var vv: Any = 7L
    for _ <- 1 to 300 do { tt = ArrayType(tt, true); vv = Seq(vv) }
    // a Schema 300 lists deep, built by value: its type is not writable, so
    // it is held existentially and the decoder's own type parameter is
    // captured at the call
    var sc: Schema[?] = Schema.SLong
    for _ <- 1 to 300 do { val inner = sc; sc = Schema.SList(() => inner) }
    def decode[X](s: Schema[X]): Any = okay.freer.!.run(SparkValues.value(s, vv, tt, Set.empty))
    var out: Any = null
    val th = Thread(null, () => out = decode(sc), "small", 256 * 1024)
    th.start(); th.join()
    var x: Any = out
    var levels = 0
    var more = true
    while more do x match
      case l: List[?] => x = l.head; levels += 1
      case _ => more = false
    assertEquals((levels, x), (300, 7L))
  }
}

