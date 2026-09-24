package okay2.stream

import scala.collection.mutable
import okay2._
import Chunks.ChunksOps

/**
 * A collection too large to be in one place — what a data platform can
 * do with it, said once (the Scala 3 core's okay-stream `Bulk`). A
 * program written against `Bulk[D]` names no platform; the instance in
 * scope is the platform — `Chunks` in one JVM here, an RDD or a
 * machine's cores elsewhere. Deliberately NO evidence per element type:
 * a `ClassTag` per `map` is the API leak this trait exists to stop.
 * A `D[A]` may be consumed more than once: the local instance builds
 * every source under `Chunks.defer`, and `cache` is the request to hold.
 */
trait Bulk[D[_]] {
  /** hand a platform a collection it did not read itself */
  def of[A](xs: Iterable[A]): D[A]

  /** a header-first CSV file as named fields, read the platform's way */
  def csv(path: String): D[Csv.Row]

  /** the same, keeping only these columns — a DEFAULT, so an instance
   * that never heard of pruning still compiles */
  def csv(path: String, columns: Option[Set[String]]): D[Csv.Row] =
    columns.fold(csv(path))(cs => map(csv(path))(row => row.filter { case (k, _) => cs(k) }))

  /** what a source is worth in bytes, when the platform can tell */
  def size(path: String): Option[Long] = None

  def map[A, B](d: D[A])(f: A => B): D[B]
  def flatMap[A, B](d: D[A])(f: A => IterableOnce[B]): D[B]
  def filter[A](d: D[A])(p: A => Boolean): D[A]

  /** the equi-join: every pair of left and right rows sharing a key */
  def join[K, A, B](l: D[(K, A)], r: D[(K, B)]): D[(K, (A, B))]

  /** materialise, because what follows reads this more than once */
  def cache[A](d: D[A]): D[A]

  /** the aggregation contract: (init, add, merge) the platform's way */
  def aggregate[A, Acc, Out](d: D[A])(agg: Aggregator[A, Acc, Out]): Out

  /** back to the local world, one chunk at a time */
  def toChunks[A](d: D[A]): Chunks[A]
}

object Bulk {

  /** the collection view, for code generic in D (on a concrete
   * `Chunks` the program's own monadic map wins, as in the core) */
  implicit final class BulkOps[D[_], A](private val d: D[A]) extends AnyVal {
    def mapB[X](f: A => X)(implicit B: Bulk[D]): D[X] = B.map(d)(f)
    def flatMapB[X](f: A => IterableOnce[X])(implicit B: Bulk[D]): D[X] = B.flatMap(d)(f)
    def filterB(p: A => Boolean)(implicit B: Bulk[D]): D[A] = B.filter(d)(p)
    def aggregateB[Acc, Out](agg: Aggregator[A, Acc, Out])(implicit B: Bulk[D]): Out = B.aggregate(d)(agg)
  }

  /** one JVM: `Chunks`, with the platform's one contribution being how a
   * file becomes lines. Sources are deferred, so every run re-reads */
  def local(lines: String => Iterator[String], bytes: String => Option[Long] = _ => None): Bulk[Chunks] = new Bulk[Chunks] {
    def of[A](xs: Iterable[A]): Chunks[A] = Chunks.defer(Chunks.fromIterator(xs.iterator))
    def csv(path: String): Chunks[Csv.Row] = Chunks.defer(Chunks.fromIterator(Csv.rows(lines(path))))
    /** pruned at the parser: a dropped column is never put in a Map */
    override def csv(path: String, columns: Option[Set[String]]): Chunks[Csv.Row] =
      Chunks.defer(Chunks.fromIterator(Csv.rows(lines(path), columns)))
    override def size(path: String): Option[Long] = bytes(path)
    def map[A, B](d: Chunks[A])(f: A => B): Chunks[B] = Chunks.map(d)(f)
    def flatMap[A, B](d: Chunks[A])(f: A => IterableOnce[B]): Chunks[B] = Chunks.defer(Chunks.fromIterator(d.elements.flatMap(f)))
    def filter[A](d: Chunks[A])(p: A => Boolean): Chunks[A] = Chunks.filter(d)(p)

    /** a hash join: the right side in memory, the left side streamed */
    def join[K, A, B](l: Chunks[(K, A)], r: Chunks[(K, B)]): Chunks[(K, (A, B))] = Chunks.defer {
      val right = mutable.HashMap.empty[K, mutable.ArrayBuffer[B]]
      r.elements.foreach { case (k, b) => right.getOrElseUpdate(k, mutable.ArrayBuffer.empty) += b }
      Chunks.fromIterator(l.elements.flatMap { case (k, a) => right.get(k).iterator.flatMap(_.iterator.map(b => (k, (a, b)))) })
    }

    def cache[A](d: Chunks[A]): Chunks[A] = of(d.elements.toVector)
    def aggregate[A, Acc, Out](d: Chunks[A])(agg: Aggregator[A, Acc, Out]): Out = agg.present(Chunks.fold(d)(agg.fold[A]))
    def toChunks[A](d: Chunks[A]): Chunks[A] = d
  }
}

/**
 * Comma-separated values, the RFC 4180 subset that fits on a line:
 * quoted fields, doubled quotes inside them, commas inside quotes. A
 * quoted NEWLINE is not handled, and this says so rather than guessing.
 */
object Csv {
  /** one record, by column name */
  type Row = Map[String, String]

  /** the fields of one line */
  def fields(line: String): Vector[String] = {
    val out = Vector.newBuilder[String]
    val cur = new StringBuilder
    var quoted = false
    var i = 0
    while (i < line.length) {
      val c = line.charAt(i)
      if (quoted) {
        if (c == '"') {
          if (i + 1 < line.length && line.charAt(i + 1) == '"') { cur += '"'; i += 1 }
          else quoted = false
        } else cur += c
      } else if (c == '"') quoted = true
      else if (c == ',') { out += cur.result(); cur.clear() }
      else cur += c
      i += 1
    }
    out += cur.result()
    out.result()
  }

  /** one row back OUT, quoting only what needs it — the inverse of `fields` */
  def line(values: IterableOnce[String]): String =
    values.iterator.map { v =>
      if (v.exists(c => c == ',' || c == '"' || c == '\n' || c == '\r')) "\"" + v.replace("\"", "\"\"") + "\"" else v
    }.mkString(",")

  /** lines to rows: the first line names the columns (a BOM is stripped);
   * `keep` prunes at the parser */
  def rows(lines: Iterator[String], keep: Option[Set[String]] = None): Iterator[Row] =
    if (!lines.hasNext) Iterator.empty
    else {
      val header = fields(lines.next().stripPrefix("﻿"))
      val wanted = keep.fold[IndexedSeq[Int]](header.indices)(cs => header.indices.filter(i => cs(header(i))))
      lines.filter(_.nonEmpty).map { l =>
        val fs = fields(l)
        wanted.iterator.collect { case i if i < fs.length => header(i) -> fs(i) }.toMap
      }
    }
}
