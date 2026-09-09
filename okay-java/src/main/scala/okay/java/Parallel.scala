package okay.java

import okay.{Aggregator, Bulk, Chunks, Csv}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.{ArrayList, List as JList}
import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*

/**
 * The `Bulk` seam on one machine's cores (specs/bulk.md): a
 * `java.util.List` is the collection, every step is a PARALLEL stream
 * over it, and the aggregation is the Collector bridge — which is
 * where `Aggregator.merge` earns its keep on the JDK, combining the
 * partial results the fork/join pool hands back. Lists are held, so a
 * value is consumed as often as the program likes and `cache` is
 * already true.
 */
object Parallel:

  given bulk: Bulk[JList] = new Bulk[JList]:
    def of[A](xs: Iterable[A]): JList[A] = new ArrayList[A](xs.asJavaCollection)

    def csv(path: String): JList[Csv.Row] =
      val lines = Files.lines(Path.of(path), UTF_8)
      try of(Csv.rows(lines.iterator().asScala).toVector) finally lines.close()

    def map[A, B](d: JList[A])(f: A => B): JList[B] =
      d.parallelStream().map(a => f(a)).collect(Collectors.toList[B])

    def flatMap[A, B](d: JList[A])(f: A => IterableOnce[B]): JList[B] =
      d.parallelStream().flatMap(a => f(a).iterator.asJava.asScala.toSeq.asJava.stream()).collect(Collectors.toList[B])

    def filter[A](d: JList[A])(p: A => Boolean): JList[A] =
      d.parallelStream().filter(a => p(a)).collect(Collectors.toList[A])

    /** a hash join: the right side grouped once, the left side in parallel */
    def join[K, A, B](l: JList[(K, A)], r: JList[(K, B)]): JList[(K, (A, B))] =
      val right = r.asScala.groupMap(_._1)(_._2)
      l.parallelStream()
        .flatMap((k, a) => right.getOrElse(k, Nil).map(b => (k, (a, b))).asJava.stream())
        .collect(Collectors.toList[(K, (A, B))])

    def cache[A](d: JList[A]): JList[A] = d

    def aggregate[A, Acc, Out](d: JList[A])(agg: Aggregator[A, Acc, Out]): Out =
      d.parallelStream().collect(Collect.collector(agg))

    def toChunks[A](d: JList[A]): Chunks[A] =
      Chunks.fromIteratorWith(d.iterator().asScala)(okay.ChunkBuf.factory[A](64))(64)
