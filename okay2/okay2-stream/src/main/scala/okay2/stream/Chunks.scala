package okay2.stream

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import okay2._
import okay2.Free.{Return, Inject, Bind}

/**
 * A chunked stream is an ordinary pure writer stream of whole batches
 * — `Feed[Chunk[A]]` — so nothing is new in the stream layer. What
 * changes is the arithmetic: the freer tree steps once per CHUNK, and
 * an element inside costs an array index — the amortization the
 * chunked runtimes (ZStream, fs2, kyo) are built on. Generators fill
 * each chunk in a tight while-loop, so no tree node is ever paid per
 * element.
 */
object Chunks {

  /** unfold with a tight per-chunk loop: size elements of f over the
   * unfolding seed per emitted chunk; lazy, one chunk at a time */
  def generate[A, B](a: A)(f: A => B)(g: A => A)(size: Int = 64): Chunks[B] =
    generateWith(a)(ChunkBuf.filler[A, B](f)(g)(size))

  def generateWith[A, B](a: A)(fill: A => (Chunk[B], A)): Chunks[B] = {
    def go(s: A): Chunks[B] = defer {
      val (c, cur) = fill(s)
      Writer.tell(c).flatMap(_ => go(cur))
    }
    go(a)
  }

  /** the numbers from until (exclusive), a short tail chunk if needed;
   * a `long[]` underneath, no boxing */
  def range(from: Long, until: Long, size: Int = 64): Chunks[Long] = {
    def go(s: Long): Chunks[Long] = defer {
      if (s >= until) end[Long]
      else {
        val n = math.min(size.toLong, until - s).toInt
        val arr = new Array[Long](n)
        var i = 0
        while (i < n) { arr(i) = s + i; i += 1 }
        Writer.tell(ArraySeq.unsafeWrapArray(arr): Chunk[Long]).flatMap(_ => go(s + n))
      }
    }
    go(from)
  }

  /** a string as chunks of characters, in a `char[]` */
  def ofChars(text: String, size: Int = 64): Chunks[Char] = {
    def go(from: Int): Chunks[Char] = defer {
      if (from >= text.length) end[Char]
      else {
        val n = math.min(size, text.length - from)
        val arr = new Array[Char](n)
        text.getChars(from, from + n, arr, 0)
        Writer.tell(ArraySeq.unsafeWrapArray(arr): Chunk[Char]).flatMap(_ => go(from + n))
      }
    }
    go(0)
  }

  /** chunk up a (mutable, linear) iterator: size elements per chunk,
   * pulled lazily — an infinite iterator is fine */
  def fromIterator[A](it: Iterator[A], size: Int = 64): Chunks[A] =
    fromIteratorWith(it)(ChunkBuf.factory[A](size))(size)

  def fromIteratorWith[A](it: Iterator[A])(fresh: () => ChunkBuf[A])(size: Int): Chunks[A] = {
    def go(): Chunks[A] = defer {
      if (!it.hasNext) end[A]
      else {
        val buf = fresh()
        var i = 0
        while (i < size && it.hasNext) { buf(i) = it.next(); i += 1 }
        Writer.tell(buf.take(i)).flatMap(_ => go())
      }
    }
    go()
  }

  /** the naturals: 0, 1, 2, ... in chunks */
  def nats[N](size: Int = 64)(implicit N: Numeric[N]): Chunks[N] =
    generate(N.zero)(identity[N])(n => N.plus(n, N.one))(size)

  /** the Fibonacci numbers, in chunks */
  def fibs[N](size: Int = 64)(implicit N: Numeric[N]): Chunks[N] =
    generate((N.zero, N.one))(_._1) { case (x, y) => (y, N.plus(x, y)) }(size)

  /** defer one step: nothing before the bind runs at construction */
  def defer[X](x: => Feed[X]): Feed[X] = pure[Writer[X], Unit](()).flatMap(_ => x)

  /** the empty chunk */
  def emptyChunk[B]: Chunk[B] = ArraySeq.empty[AnyRef].asInstanceOf[Chunk[B]]

  /** the end of a chunked stream: its answer, which is no chunk */
  def end[B]: Chunks[B] = pure(())

  /** pull one chunk: the pure step of a chunked stream */
  def pull[A](p: Chunks[A]): Option[(Chunk[A], Chunks[A])] = Writer.uncons(p).toOption

  /** map, the elements boxed on the way (`mapTagged` for an unboxed result) */
  def map[A, B](p: Chunks[A])(f: A => B): Chunks[B] = mapWith(p)(ChunkBuf.mapper[A, B](f))

  /** map into a chunk whose backing the tag chooses: a `long[]` for Long */
  def mapTagged[A, B](p: Chunks[A])(f: A => B)(implicit ct: scala.reflect.ClassTag[B]): Chunks[B] =
    mapWith(p)(ChunkBuf.taggedMapper[A, B](f))

  def mapWith[A, B](p: Chunks[A])(g: Chunk[A] => Chunk[B]): Chunks[B] = defer {
    val Said = Writer.said[Chunk[A]]
    Free.resume(p) match {
      case Return(_) => end[B]
      case Inject(Said(c)) => Writer.tell(g(c))
      case Bind(Inject(Said(c)), k) => Writer.tell(g(c)).flatMap(_ => mapWith(k(()))(g))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
  }

  /** keep the elements satisfying pred (empty result chunks are skipped) */
  def filter[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = filterWith(p)(ChunkBuf.filterer[A](pred))

  def filterWith[A](p: Chunks[A])(g: Chunk[A] => Chunk[A]): Chunks[A] = defer {
    val Said = Writer.said[Chunk[A]]
    Free.resume(p) match {
      case Return(_) => end[A]
      case Inject(Said(c)) => Writer.tell(g(c))
      case Bind(Inject(Said(c)), k) =>
        val fc = g(c)
        if (fc.isEmpty) filterWith(k(()))(g)
        else Writer.tell(fc).flatMap(_ => filterWith(k(()))(g))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
  }

  /** the first n elements (the last chunk truncated) */
  def take[A](p: Chunks[A])(n: Int): Chunks[A] = defer {
    val Said = Writer.said[Chunk[A]]
    if (n <= 0) end[A]
    else Free.resume(p) match {
      case Return(_) => end[A]
      case Inject(Said(c)) => Writer.tell(c.take(n))
      case Bind(Inject(Said(c)), k) =>
        if (c.length >= n) Writer.tell(c.take(n))
        else Writer.tell(c).flatMap(_ => take(k(()))(n - c.length))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
  }

  /** all but the first n elements */
  def drop[A](p: Chunks[A])(n: Int): Chunks[A] = defer {
    val Said = Writer.said[Chunk[A]]
    if (n <= 0) p
    else Free.resume(p) match {
      case Return(_) => end[A]
      case Inject(Said(c)) => Writer.tell(c.drop(n))
      case Bind(Inject(Said(c)), k) =>
        if (c.length <= n) drop(k(()))(n - c.length)
        else Writer.tell(c.drop(n)).flatMap(_ => k(()))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
  }

  /** the longest prefix satisfying pred */
  def takeWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer {
    val Said = Writer.said[Chunk[A]]
    Free.resume(p) match {
      case Return(_) => end[A]
      case Inject(Said(c)) => Writer.tell(c.takeWhile(pred))
      case Bind(Inject(Said(c)), k) =>
        val i = c.indexWhere(a => !pred(a))
        if (i < 0) Writer.tell(c).flatMap(_ => takeWhile(k(()))(pred))
        else Writer.tell(c.take(i))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
  }

  /** the rest, after the longest prefix satisfying pred */
  def dropWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer {
    val Said = Writer.said[Chunk[A]]
    Free.resume(p) match {
      case Return(_) => end[A]
      case Inject(Said(c)) => Writer.tell(c.dropWhile(pred))
      case Bind(Inject(Said(c)), k) =>
        val i = c.indexWhere(a => !pred(a))
        if (i < 0) dropWhile(k(()))(pred)
        else if (i == 0) k(())
        else Writer.tell(c.drop(i)).flatMap(_ => k(()))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
  }

  /** the terminal with the step at the call site: an inner while per chunk */
  def foldLeft[A, S](p: Chunks[A])(z: S)(f: (S, A) => S): S = {
    var s = z
    val it = Stream.feedStream[Unit].iterator(p)
    while (it.hasNext) {
      val c = it.next()
      var i = 0
      while (i < c.length) { s = f(s, c(i)); i += 1 }
    }
    s
  }

  /** how many elements */
  def count[A](p: Chunks[A]): Long = foldLeft(p)(0L)((n, _) => n + 1L)

  /** the terminal: run a Fold, dispatched on its accumulator so the
   * four primitive shapes keep it unboxed across the scan */
  def fold[A, S](p: Chunks[A])(fo: Fold[A, S]): S = fo match {
    case l: Fold.OfLong[A @unchecked] => foldLeft(p)(l.initLong)((s, a) => l.addLong(s, a)).asInstanceOf[S]
    case i: Fold.OfInt[A @unchecked] => foldLeft(p)(i.initInt)((s, a) => i.addInt(s, a)).asInstanceOf[S]
    case d: Fold.OfDouble[A @unchecked] => foldLeft(p)(d.initDouble)((s, a) => d.addDouble(s, a)).asInstanceOf[S]
    case b: Fold.OfBoolean[A @unchecked] => foldLeft(p)(b.initBoolean)((s, a) => b.addBoolean(s, a)).asInstanceOf[S]
    case _ => foldLeft(p)(fo.init)((s, a) => fo.add(s, a))
  }

  /** `fold` with a stop: `done` is checked per element inside the
   * chunk's while, and BEFORE `hasNext` pulls the next chunk */
  def foldUntil[A, S, R](p: Chunks[A])(fo: FoldUntil[A, S, R]): R = {
    val it = Stream.feedStream[Unit].iterator(p)
    fo match {
      case l: FoldUntil.OfLong[A @unchecked, R @unchecked] =>
        var s = l.initLong
        while (!l.doneLong(s) && it.hasNext) {
          val c = it.next(); var i = 0
          while (i < c.length && !l.doneLong(s)) { s = l.addLong(s, c(i)); i += 1 }
        }
        l.endLong(s)
      case n: FoldUntil.OfInt[A @unchecked, R @unchecked] =>
        var s = n.initInt
        while (!n.doneInt(s) && it.hasNext) {
          val c = it.next(); var i = 0
          while (i < c.length && !n.doneInt(s)) { s = n.addInt(s, c(i)); i += 1 }
        }
        n.endInt(s)
      case d: FoldUntil.OfDouble[A @unchecked, R @unchecked] =>
        var s = d.initDouble
        while (!d.doneDouble(s) && it.hasNext) {
          val c = it.next(); var i = 0
          while (i < c.length && !d.doneDouble(s)) { s = d.addDouble(s, c(i)); i += 1 }
        }
        d.endDouble(s)
      case b: FoldUntil.OfBoolean[A @unchecked, R @unchecked] =>
        var s = b.initBoolean
        while (!b.doneBoolean(s) && it.hasNext) {
          val c = it.next(); var i = 0
          while (i < c.length && !b.doneBoolean(s)) { s = b.addBoolean(s, c(i)); i += 1 }
        }
        b.endBoolean(s)
      case _ =>
        var s = fo.init
        while (!fo.done(s) && it.hasNext) {
          val c = it.next(); var i = 0
          while (i < c.length && !fo.done(s)) { s = fo.add(s, c(i)); i += 1 }
        }
        fo.end(s)
    }
  }

  /** pair two chunked streams elementwise, realigning chunk boundaries;
   * the stream ends at the shorter side */
  def zip[A, B](pa: Chunks[A], pb: Chunks[B]): Chunks[(A, B)] = {
    def go(ca: Chunk[A], ia: Int, ra: Chunks[A], cb: Chunk[B], ib: Int, rb: Chunks[B]): Chunks[(A, B)] = defer {
      if (ia >= ca.length) pull(ra) match {
        case None => end[(A, B)]
        case Some((c, r)) => go(c, 0, r, cb, ib, rb)
      }
      else if (ib >= cb.length) pull(rb) match {
        case None => end[(A, B)]
        case Some((c, r)) => go(ca, ia, ra, c, 0, r)
      }
      else {
        val n = math.min(ca.length - ia, cb.length - ib)
        val buf = ChunkBuf[(A, B)](n)
        var i = 0
        while (i < n) { buf(i) = (ca(ia + i), cb(ib + i)); i += 1 }
        Writer.tell(buf.chunk).flatMap(_ => go(ca, ia + n, ra, cb, ib + n, rb))
      }
    }
    go(emptyChunk, 0, pa, emptyChunk, 0, pb)
  }

  /** normalize chunk sizes (the content unchanged, the tail shorter) */
  def rechunk[A](p: Chunks[A])(size: Int = 64): Chunks[A] = rechunkWith(p)(ChunkBuf.factory[A](size))(size)

  def rechunkWith[A](p: Chunks[A])(fresh: () => ChunkBuf[A])(size: Int): Chunks[A] = {
    def go(buf: ChunkBuf[A], have: Int, rest: Chunks[A]): Chunks[A] = defer {
      pull(rest) match {
        case None => if (have == 0) end[A] else Writer.tell(buf.take(have))
        case Some((c, r)) =>
          val room = size - have
          if (c.length < room) {
            var i = 0
            while (i < c.length) { buf(have + i) = c(i); i += 1 }
            go(buf, have + c.length, r)
          } else {
            var i = 0
            while (i < room) { buf(have + i) = c(i); i += 1 }
            val leftover = c.drop(room)
            val next: Chunks[A] = if (leftover.isEmpty) r else Writer.tell(leftover).flatMap(_ => r)
            Writer.tell(buf.chunk).flatMap(_ => go(fresh(), 0, next))
          }
      }
    }
    go(fresh(), 0, p)
  }

  /** pipe a chunked producer into an ELEMENTWISE consumer: an await is
   * served by an array index, the tree steps once per chunk */
  def pipe[W, B](p: Chunks[W])(c: Free[Take[W], B]): B = {
    @tailrec def fetch(ch: Chunk[W], i: Int, rest: Chunks[W]): (Option[W], Chunk[W], Int, Chunks[W]) =
      if (i < ch.length) (Some(ch(i)), ch, i + 1, rest)
      else pull(rest) match {
        case Some((c2, r)) => fetch(c2, 0, r)
        case None => (None, ch, i, end[W])
      }

    @tailrec def loop(ch: Chunk[W], i: Int, rest: Chunks[W], c: Free[Take[W], B]): B = Free.resume(c) match {
      case Return(b) => b
      case Inject(Take.Await()) => fetch(ch, i, rest)._1.asInstanceOf[B]
      case Bind(Inject(Take.Await()), k) =>
        val (o, ch2, i2, r2) = fetch(ch, i, rest)
        loop(ch2, i2, r2, k(o))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(emptyChunk, 0, p, c)
  }

  implicit final class ChunksOps[A](private val p: Chunks[A]) extends AnyVal {
    /** the element view: one tree step per chunk, an index per element */
    def elements: Iterator[A] = Stream.feedStream[Unit].iterator(p).flatMap(_.iterator)
    /** the chunks, memoized */
    def toLazyList: LazyList[Chunk[A]] = LazyList.from(Stream.feedStream[Unit].iterator(p))
  }
}
