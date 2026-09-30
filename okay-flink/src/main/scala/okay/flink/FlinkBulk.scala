package okay.flink

import okay.*
import org.apache.flink.api.common.RuntimeExecutionMode
import org.apache.flink.api.common.functions.{AggregateFunction, CoGroupFunction, FilterFunction, FlatMapFunction, MapFunction}
import org.apache.flink.api.common.typeinfo.TypeInformation
import org.apache.flink.api.java.functions.KeySelector
import org.apache.flink.streaming.api.datastream.DataStream
import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
import org.apache.flink.streaming.api.windowing.assigners.GlobalWindows
import org.apache.flink.util.Collector
import scala.jdk.CollectionConverters.*

/**
 * FLINK AS A PLATFORM (specs/streams-seam.md, lane 3; backlog bulk-flink):
 * a `Bulk` over Flink's DataStream API, run BOUNDED (`RuntimeExecutionMode.
 * BATCH`), so a `Tables` program that names no platform runs on Flink
 * unchanged — beside `localBulk`, `SparkBulk` and `FlowBulk`.
 *
 * The element is `AnyRef` under Flink's generic (Kryo) type information,
 * the seam's choice (specs/bulk.md, "No per-element evidence"): Flink
 * would ask a `TypeInformation` of every intermediate type, and the
 * Scala 3 macros that derived one do not exist. `join` is a `coGroup`
 * keyed on both sides in one end-of-stream window (every key's groups at
 * once, then the pairs); `aggregate` is the okay `Aggregator` as Flink's
 * `AggregateFunction` (`FlinkInterop.toFlink`) over the whole stream.
 *
 * AN OPTIONAL DEPENDENCY (AGENTS.md, "every dependency behind an
 * abstraction"): `flink-streaming-java` and `flink-clients` are
 * `optional` for okay-flink; `FlinkBulk.local` refuses BY NAME when they
 * are absent. The functions are top-level classes: Flink serializes each
 * into the job graph, and one written inside a method would capture the
 * environment.
 */
object FlinkBulk:
  opaque type Rows[A] = DataStream[AnyRef]

  /** the one cast: an element of `Rows[A]` is an `A` by construction */
  private[flink] inline def elem[A](x: AnyRef): A = x.asInstanceOf[A]

  private[flink] val any: TypeInformation[AnyRef] = TypeInformation.of(classOf[AnyRef])

  /** why FlinkBulk cannot run here, if it cannot */
  def missing: Option[String] =
    try { Class.forName("org.apache.flink.streaming.api.environment.StreamExecutionEnvironment"); None }
    catch case _: ClassNotFoundException => Some(
      "okay.flink.FlinkBulk needs org.apache.flink:flink-streaming-java and flink-clients (1.20), " +
        "optional dependencies of okay-flink: add them to your build")

  /** a local MiniCluster at `parallelism`, bounded */
  def local(parallelism: Int = 2): FlinkBulk =
    missing.foreach(why => throw IllegalStateException(why))
    val env = StreamExecutionEnvironment.createLocalEnvironment(parallelism)
    FlinkBulk(env)

  final class FlinkBulk(env: StreamExecutionEnvironment) extends Bulk[Rows]:
    env.setRuntimeMode(RuntimeExecutionMode.BATCH): Unit

    def of[A](xs: Iterable[A]): Rows[A] =
      val v = xs.iterator.map(x => x.asInstanceOf[AnyRef]).toVector
      // Flink 1.20's `fromData` over an EMPTY collection still generates one
      // record and fails ("Reached the end of the collection"): an empty
      // table is one placeholder dropped at once
      if v.isEmpty then env.fromData(java.util.List.of[AnyRef](""), any).filter(FlinkNone())
      else env.fromData(v.asJava, any)

    /** a header-first CSV, read by the client as `of` reads a collection:
     * a bounded job, and Flink's file source is another connector */
    def csv(path: String): Rows[Csv.Row] = csv(path, None)
    override def csv(path: String, columns: Option[Set[String]]): Rows[Csv.Row] =
      val lines = java.nio.file.Files.readAllLines(java.nio.file.Path.of(path)).asScala.iterator
      of(Csv.rows(lines, columns).toVector)

    override def size(path: String): Option[Long] =
      val f = java.io.File(path)
      Option.when(f.isFile)(f.length)

    def map[A, B](d: Rows[A])(f: A => B): Rows[B] = d.map(FlinkMap(f), any)
    def flatMap[A, B](d: Rows[A])(f: A => IterableOnce[B]): Rows[B] = d.flatMap(FlinkFlatMap(f), any)
    def filter[A](d: Rows[A])(p: A => Boolean): Rows[A] = d.filter(FlinkFilter(p))

    def join[K, A, B](l: Rows[(K, A)], r: Rows[(K, B)]): Rows[(K, (A, B))] =
      l.coGroup(r).where(FlinkKey[K, A](), any).equalTo(FlinkKey[K, B](), any)
        .window(GlobalWindows.createWithEndOfStreamTrigger())
        .apply(FlinkPairs[K, A, B](), any)

    def cache[A](d: Rows[A]): Rows[A] = of(collect[A](d))

    def aggregate[A, Acc, Out](d: Rows[A])(agg: Aggregator[A, Acc, Out]): Out =
      val out = d.windowAll(GlobalWindows.createWithEndOfStreamTrigger())
        .aggregate(FlinkAgg(agg), any, any).executeAndCollect(1).asScala
      out.headOption.fold(agg.present(agg.init))(x => elem[Out](x))

    /** deferred: the job runs when this is pulled, once per consumption */
    def toChunks[A](d: Rows[A]): Chunks[A] = Chunks.defer(Chunks.fromIterator(collect[A](d).iterator))

    private def collect[A](d: Rows[A]): Vector[A] =
      val it = d.executeAndCollect()
      try it.asScala.map(x => elem[A](x)).toVector finally it.close()

// ------------------------------------------------ the functions, top-level
private final class FlinkMap[A, B](f: A => B) extends MapFunction[AnyRef, AnyRef]:
  def map(x: AnyRef): AnyRef = f(FlinkBulk.elem[A](x)).asInstanceOf[AnyRef]

private final class FlinkFlatMap[A, B](f: A => IterableOnce[B]) extends FlatMapFunction[AnyRef, AnyRef]:
  def flatMap(x: AnyRef, out: Collector[AnyRef]): Unit =
    f(FlinkBulk.elem[A](x)).iterator.foreach(b => out.collect(b.asInstanceOf[AnyRef]))

private final class FlinkFilter[A](p: A => Boolean) extends FilterFunction[AnyRef]:
  def filter(x: AnyRef): Boolean = p(FlinkBulk.elem[A](x))

private final class FlinkNone extends FilterFunction[AnyRef]:
  def filter(x: AnyRef): Boolean = false

private final class FlinkKey[K, V] extends KeySelector[AnyRef, AnyRef]:
  def getKey(x: AnyRef): AnyRef = FlinkBulk.elem[(K, V)](x)._1.asInstanceOf[AnyRef]

private final class FlinkPairs[K, A, B] extends CoGroupFunction[AnyRef, AnyRef, AnyRef]:
  def coGroup(ls: java.lang.Iterable[AnyRef], rs: java.lang.Iterable[AnyRef], out: Collector[AnyRef]): Unit =
    val right = rs.asScala.map(x => FlinkBulk.elem[(K, B)](x)).toVector
    for x <- ls.asScala do
      val (k, a) = FlinkBulk.elem[(K, A)](x)
      right.foreach((_, b) => out.collect((k, (a, b))))

private final class FlinkAgg[A, Acc, Out](agg: Aggregator[A, Acc, Out]) extends AggregateFunction[AnyRef, AnyRef, AnyRef]:
  def createAccumulator(): AnyRef = agg.init.asInstanceOf[AnyRef]
  def add(x: AnyRef, acc: AnyRef): AnyRef = agg.add(FlinkBulk.elem[Acc](acc), FlinkBulk.elem[A](x)).asInstanceOf[AnyRef]
  def getResult(acc: AnyRef): AnyRef = agg.present(FlinkBulk.elem[Acc](acc)).asInstanceOf[AnyRef]
  def merge(a: AnyRef, b: AnyRef): AnyRef = agg.merge(FlinkBulk.elem[Acc](a), FlinkBulk.elem[Acc](b)).asInstanceOf[AnyRef]
