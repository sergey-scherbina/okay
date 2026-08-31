package okay.java

import okay.{Aggregator, Chunk, Chunks}
import okay.given
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.*
import java.util.stream.{Collectors, LongStream, Stream}

class TestJavaInterop extends munit.FunSuite {

  def backing(c: Chunk[?]): String =
    c.asInstanceOf[ArraySeq[?]].unsafeArray.getClass.getSimpleName

  def collect[A](p: Chunks[A]): List[Chunk[A]] =
    def go(x: Chunks[A], acc: List[Chunk[A]]): List[Chunk[A]] =
      Chunks.pull(x) match
        case None => acc.reverse
        case Some((c, r)) => go(r, c :: acc)
    go(p, Nil)

  // ------------------------------------------------------------ streams

  test("chunks to a Stream and back is the same elements") {
    val src = Chunks.fromIterator((1 to 250).iterator, 64)
    val viaJdk = Streams.chunks(Streams.stream(src), 64)
    assertEquals(collect(viaJdk).flatMap(_.toList), (1 to 250).toList)
  }

  test("a PARALLEL stream over chunks agrees with the sequential one") {
    // trySplit hands over whole chunks, so the parallel machinery has
    // real work units — and the answer must not depend on that
    def src = Chunks.fromIterator((1 to 10000).iterator, 64)
    val seq = Streams.stream(src).mapToLong(_.toLong).sum()
    val par = Streams.stream(src, parallel = true).mapToLong(_.toLong).sum()
    assertEquals(par, seq)
    assertEquals(par, (1 to 10000).map(_.toLong).sum)
  }

  test("a LongStream crosses the seam UNBOXED, both sides primitive") {
    val chunks = collect(Streams.longs(LongStream.rangeClosed(1, 200), 64))
    assertEquals(chunks.flatMap(_.toList), (1L to 200L).toList)
    // the point of the primitive overload: no boxing at the boundary
    for c <- chunks do assertEquals(backing(c), "long[]")
  }

  test("ints and doubles too") {
    val is = collect(Streams.ints(java.util.stream.IntStream.range(0, 100), 32))
    for c <- is do assertEquals(backing(c), "int[]")
    assertEquals(is.flatMap(_.toList), (0 until 100).toList)

    val ds = collect(Streams.doubles(java.util.stream.DoubleStream.of(1.0, 2.0, 3.0), 2))
    for c <- ds do assertEquals(backing(c), "double[]")
    assertEquals(ds.flatMap(_.toList), List(1.0, 2.0, 3.0))
  }

  test("an empty stream is an empty producer, not a hang") {
    assertEquals(collect(Streams.chunks(Stream.empty[String]())), Nil)
    assertEquals(collect(Streams.longs(LongStream.empty())), Nil)
  }

  test("out to a primitive JDK stream: no boxing on the way back either") {
    // the chunks are already long[] (Chunks.range specializes), and
    // Spliterator.OfLong reads primitives, so the backing array is
    // handed over rather than copied element by element
    val p = Chunks.range(1L, 201L, 64)
    assertEquals(Streams.longStream(p).sum(), (1L to 200L).sum)
    assertEquals(Streams.longStream(p).max().getAsLong, 200L)

    val ints = Chunks.map(Chunks.range(0L, 100L, 32))(_.toInt)
    assertEquals(Streams.intStream(ints).sum(), (0 until 100).sum)

    val ds = Chunks.map(Chunks.range(1L, 4L, 8))(_.toDouble)
    assertEquals(Streams.doubleStream(ds).sum(), 6.0)
  }

  test("a primitive stream out is the same in PARALLEL") {
    val p = Chunks.range(1L, 20001L, 64)
    assertEquals(Streams.longStream(p, parallel = true).sum(), (1L to 20000L).sum)
  }

  test("a round trip through the primitive streams keeps it unboxed") {
    val there = Streams.longStream(Chunks.range(1L, 129L, 64))
    val back = collect(Streams.longs(there, 64))
    assertEquals(back.flatMap(_.toList), (1L to 128L).toList)
    for c <- back do assertEquals(backing(c), "long[]")
  }

  test("a BOXED chunk of Longs still crosses, one unbox per element") {
    // what a generic producer builds: the seam must work anyway.
    //
    // Built through a NON-inline generic boundary, which is the only
    // way to get a boxed chunk now that `fromIterator` specializes —
    // the fallback is what this test is about, so it has to be real.
    //
    // A `def`, not a `val`: the producer wraps a LIVE Iterator and is
    // single-use, so draining it once to check the backing would
    // leave nothing for the second check.
    def generic[A](xs: Seq[A]): Chunks[A] =
      Chunks.fromIteratorWith(xs.iterator)(
        () => okay.ChunkBuf.boxed[A](4))(4)
    def boxed: Chunks[Long] = generic(Seq(1L, 2L, 3L, 4L, 5L))
    assertEquals(backing(collect(boxed).head), "Object[]", "the probe is vacuous")
    assertEquals(Streams.longStream(boxed).sum(), 15L)
  }

  // --------------------------------------------------------- collectors

  test("the SAME aggregator: over chunks, and as a java Collector") {
    val agg = Aggregator.variance[Double]
    val xs = (1 to 5000).map(_.toDouble)

    val local = agg.present(
      Chunks.fold(Chunks.fromIterator(xs.iterator))(using agg.fold))
    val onJdk = xs.asJava.stream().collect(Collect.collector(agg))
    assert(math.abs(local - onJdk) < 1e-9, s"local $local vs jdk $onJdk")
  }

  test("and in PARALLEL, which is what `merge` was always for") {
    val agg = Aggregator.sum[Long].zip(Aggregator.count[Long])
    val xs = (1L to 20000L).toSeq
    val (sum, count) = xs.asJava.stream().parallel().collect(Collect.collector(agg))
    assertEquals(sum, xs.sum)
    assertEquals(count, 20000L)
  }

  test("a java Collector as an aggregator") {
    val c = Collectors.summingInt[Integer](i => i.intValue)
    val agg = Collect.aggregator(c)
    assert(agg.isRight, agg.left.getOrElse(""))
    val a = agg.toOption.get
    val xs = (1 to 100).map(Integer.valueOf)
    assertEquals(a.present(xs.foldLeft(a.init)(a.add)), Integer.valueOf(5050))
  }

  test("a CONCURRENT collector is refused, with the reason") {
    val c = Collectors.toConcurrentMap[Integer, Integer, Integer](
      i => i, i => i, (a, _) => a)
    val r = Collect.aggregator(c)
    assert(r.isLeft)
    assert(r.left.getOrElse("").contains("CONCURRENT"), r.toString)
  }

  // ---------------------------------------------------------- functions

  test("plain function conversions, both ways") {
    val f: java.util.function.Function[Int, String] = i => s"n$i"
    assertEquals(Functions.fn(f)(3), "n3")
    assertEquals(Functions.jfn((i: Int) => i * 2).apply(21), 42)
    assert(Functions.pred[Int](i => i > 0)(1))
    assert(Functions.jpred((i: Int) => i > 0).test(1))
    assertEquals(Functions.bifn[Int, Int, Int]((a, b) => a + b)(2, 3), 5)
    assertEquals(Functions.jbinary((a: Int, b: Int) => a max b).apply(2, 9), 9)
  }

  test("a Supplier is a PROGRAM, so it has not run yet") {
    var ran = 0
    val s: java.util.function.Supplier[Int] = () => { ran += 1; 7 }
    val p = Functions.supply(s)
    assertEquals(ran, 0, "building the program ran the supplier")
    assertEquals(p.runWith, 7)
    assertEquals(ran, 1)
    // and it can be run again, because it is a description
    assertEquals(p.runWith, 7)
    assertEquals(ran, 2)
  }

  test("a Consumer is a Fold, so it composes like one") {
    val seen = scala.collection.mutable.Buffer[Int]()
    val c: java.util.function.Consumer[Int] = i => { seen += i; () }
    val f = Functions.sink(c)
    Chunks.fold(Chunks.fromIterator((1 to 5).iterator, 2))(using f)
    assertEquals(seen.toList, List(1, 2, 3, 4, 5))
  }
}
