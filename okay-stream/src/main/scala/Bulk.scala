package okay

import scala.collection.mutable
import Chunks.elements

/**
 * A collection too large to be in one place — what a data platform can
 * do with it, said once (specs/bulk.md).
 *
 * The P1 algebra runs one `Aggregator` locally or on a cluster
 * unchanged. This is the road TO the aggregation: read, project, join,
 * expand. A program written against `Bulk[D]` names no platform; the
 * instance in scope is the platform — `Chunks` in one JVM, an RDD on
 * Spark (okay-spark), a `java.util.List` over a machine's cores
 * (okay-java). The seam rule of specs/data.md: a platform enters as an
 * instance of this, never as an API surface.
 *
 * Deliberately NO evidence per element type. Spark asks a `ClassTag` of
 * every intermediate type and Flink a `TypeInformation`; threading that
 * through a generic program is an evidence parameter per `map`, which
 * is the API leak this trait exists to stop. Every platform stores an
 * element as an object anyway; the Spark instance says so in its type
 * and pays one documented cast at the boundary.
 *
 * A value of `D[A]` may be consumed more than once. The local instance
 * keeps that promise by building every source under `Chunks.defer`,
 * so a file is re-read and an `Iterable` re-iterated per run; `cache`
 * is the explicit request to hold the rows instead.
 */
trait Bulk[D[_]]:
  /** hand a platform a collection it did not read itself */
  def of[A](xs: Iterable[A]): D[A]

  /** a header-first CSV file as named fields, read the platform's way */
  def csv(path: String): D[Csv.Row]

  /**
   * The same, keeping only these columns — where the platform can prune
   * at the parser (Spark) it does; the default reads everything and
   * drops after. A DEFAULT, so an instance that never heard of pruning
   * still compiles: the seam grows by a capability, not by a demand.
   */
  def csv(path: String, columns: Option[Set[String]]): D[Csv.Row] =
    columns.fold(csv(path))(cs => map(csv(path))(row => row.filter((k, _) => cs(k))))

  /** what a source is worth in bytes, when the platform can tell —
   * the estimate a plan rewrite orders joins by (specs/bulk.md) */
  def size(path: String): Option[Long] = None

  def map[A, B](d: D[A])(f: A => B): D[B]
  def flatMap[A, B](d: D[A])(f: A => IterableOnce[B]): D[B]
  def filter[A](d: D[A])(p: A => Boolean): D[A]

  /** the equi-join: every pair of left and right rows sharing a key */
  def join[K, A, B](l: D[(K, A)], r: D[(K, B)]): D[(K, (A, B))]

  /** materialise, because what follows reads this more than once */
  def cache[A](d: D[A]): D[A]

  /** the P1 contract: (init, add, merge) the platform's way, presented */
  def aggregate[A, Acc, Out](d: D[A])(agg: Aggregator[A, Acc, Out]): Out

  /** back to the local world, one chunk at a time */
  def toChunks[A](d: D[A]): Chunks[A]

object Bulk:

  /** the collection view: a program over `D[_] : Bulk` reads as a collection */
  extension [D[_], A](d: D[A])(using B: Bulk[D])
    def map[X](f: A => X): D[X] = B.map(d)(f)
    def flatMap[X](f: A => IterableOnce[X]): D[X] = B.flatMap(d)(f)
    def filter(p: A => Boolean): D[A] = B.filter(d)(p)
    def cache: D[A] = B.cache(d)
    def aggregate[Acc, Out](agg: Aggregator[A, Acc, Out]): Out = B.aggregate(d)(agg)
    def toChunks: Chunks[A] = B.toChunks(d)

  extension [D[_], K, A](l: D[(K, A)])(using B: Bulk[D])
    def join[X](r: D[(K, X)]): D[(K, (A, X))] = B.join(l, r)

  /**
   * One JVM: `Chunks`, with the platform's one contribution being how a
   * file becomes lines (scala-jvm supplies `java.nio`; another platform
   * supplies its own). Sources are deferred, so every run re-reads.
   */
  def local(lines: String => Iterator[String], bytes: String => Option[Long] = _ => None): Bulk[Chunks] = new Bulk[Chunks]:
    def of[A](xs: Iterable[A]): Chunks[A] = Chunks.defer(Chunks.fromIterator(xs.iterator))
    def csv(path: String): Chunks[Csv.Row] = Chunks.defer(Chunks.fromIterator(Csv.rows(lines(path))))
    /** pruned at the parser: the dropped columns are never put in a Map */
    override def csv(path: String, columns: Option[Set[String]]): Chunks[Csv.Row] =
      Chunks.defer(Chunks.fromIterator(Csv.rows(lines(path), columns)))
    override def size(path: String): Option[Long] = bytes(path)
    def map[A, B](d: Chunks[A])(f: A => B): Chunks[B] = Chunks.map(d)(f)
    def flatMap[A, B](d: Chunks[A])(f: A => IterableOnce[B]): Chunks[B] =
      Chunks.defer(Chunks.fromIterator(d.elements.flatMap(f)))
    def filter[A](d: Chunks[A])(p: A => Boolean): Chunks[A] = Chunks.filter(d)(p)

    /** a hash join: the right side in memory, the left side streamed */
    def join[K, A, B](l: Chunks[(K, A)], r: Chunks[(K, B)]): Chunks[(K, (A, B))] = Chunks.defer:
      val right = mutable.HashMap.empty[K, mutable.ArrayBuffer[B]]
      for (k, b) <- r.elements do right.getOrElseUpdate(k, mutable.ArrayBuffer.empty) += b
      Chunks.fromIterator(l.elements.flatMap: (k, a) =>
        right.get(k).iterator.flatMap(_.iterator.map(b => (k, (a, b)))))

    def cache[A](d: Chunks[A]): Chunks[A] = of(d.elements.toVector)
    def aggregate[A, Acc, Out](d: Chunks[A])(agg: Aggregator[A, Acc, Out]): Out =
      agg.present(Chunks.fold(d)(using agg.fold))
    def toChunks[A](d: Chunks[A]): Chunks[A] = d

/**
 * Comma-separated values, the RFC 4180 subset that fits on a line:
 * quoted fields, doubled quotes inside them, commas inside quotes. A
 * quoted NEWLINE is not handled and this says so rather than guessing.
 */
object Csv:
  /** one record, by column name */
  type Row = Map[String, String]

  /** the fields of one line */
  def fields(line: String): Vector[String] =
    val out = Vector.newBuilder[String]
    val cur = new StringBuilder
    var quoted = false
    var i = 0
    while i < line.length do
      val c = line.charAt(i)
      if quoted then
        if c == '"' then
          if i + 1 < line.length && line.charAt(i + 1) == '"' then { cur += '"'; i += 1 }
          else quoted = false
        else cur += c
      else if c == '"' then quoted = true
      else if c == ',' then { out += cur.result(); cur.clear() }
      else cur += c
      i += 1
    out += cur.result()
    out.result()

  /** one row back OUT: a field with a comma, a quote or a newline in
   * it is quoted and its quotes doubled — the inverse of `fields`, so
   * a ledger this writes is a ledger this reads */
  def line(values: IterableOnce[String]): String =
    values.iterator.map { v =>
      if v.exists(c => c == ',' || c == '"' || c == '\n' || c == '\r')
      then "\"" + v.replace("\"", "\"\"") + "\"" else v
    }.mkString(",")

  /** lines to rows: the first line names the columns (a BOM is stripped);
   * `keep` prunes at the parser — a dropped column never enters a Map */
  def rows(lines: Iterator[String], keep: Option[Set[String]] = None): Iterator[Row] =
    if !lines.hasNext then Iterator.empty
    else
      val header = fields(lines.next().stripPrefix("﻿"))
      val wanted = keep.fold(header.indices)(cs => header.indices.filter(i => cs(header(i))))
      lines.filter(_.nonEmpty).map { l =>
        val fs = fields(l)
        wanted.iterator.collect { case i if i < fs.length => header(i) -> fs(i) }.toMap
      }
