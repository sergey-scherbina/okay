package okay2.stream

import scala.reflect.ClassTag
import okay2._

/**
 * The pipeline as a value: a typed operator tree — program-as-value
 * applied to stream pipelines. Build it, REWRITE it by rules (map
 * fusion, filter fusion, take pushdown into sources), then compile it
 * onto the chunked transformers. Every rule preserves semantics
 * (property-tested); what the optimizer buys is fewer passes and less
 * construction. The Scala 3 core's inline `Staged` half has no
 * counterpart without `inline`.
 */
sealed trait Pipeline[A] {
  def map[B: ClassTag](f: A => B): Pipeline[B] = Pipeline.Mapped(this, f)
  def filter(p: A => Boolean): Pipeline[A] = Pipeline.Filtered(this, p)
  def take(n: Int): Pipeline[A] = Pipeline.TakeN(this, n)
  def drop(n: Int): Pipeline[A] = Pipeline.DropN(this, n)
  def rechunk(size: Int): Pipeline[A] = Pipeline.Rechunked(this, size)
}

object Pipeline {
  final case class Gen[S, A](seed: S, f: S => A, g: S => S, size: Int) extends Pipeline[A]
  final case class NumRange(from: Long, until: Long, size: Int) extends Pipeline[Long]
  final case class FromChunks[A](p: Chunks[A]) extends Pipeline[A]
  /** the element type of a reified node is EXISTENTIAL at a `Mapped`,
   * so the tag travels WITH it, captured where B was still concrete:
   * that is what makes the compiled chunk unboxed */
  final case class Mapped[A, B](src: Pipeline[A], f: A => B)(implicit val tag: ClassTag[B]) extends Pipeline[B]
  final case class Filtered[A](src: Pipeline[A], p: A => Boolean) extends Pipeline[A]
  final case class TakeN[A](src: Pipeline[A], n: Int) extends Pipeline[A]
  final case class DropN[A](src: Pipeline[A], n: Int) extends Pipeline[A]
  final case class Rechunked[A](src: Pipeline[A], size: Int) extends Pipeline[A]

  def generate[S, A](seed: S)(f: S => A)(g: S => S, size: Int = 64): Pipeline[A] = Gen(seed, f, g, size)

  def range(from: Long, until: Long, size: Int = 64): Pipeline[Long] = NumRange(from, until, size)

  /**
   * The rewrite rules, applied bottom-up to a fixpoint: map/map and
   * filter/filter fuse into one pass; take/take and drop/drop combine;
   * take pushes through map and into a range; rechunk collapses into
   * rechunk and into a source's own chunk size.
   */
  def optimize[A](p: Pipeline[A]): Pipeline[A] = {
    def once[X](q: Pipeline[X]): Pipeline[X] = q match {
      case m @ Mapped(Mapped(s, f), g) => Mapped(once(s), f.andThen(g))(m.tag)
      case Filtered(Filtered(s, p1), p2) => Filtered(once(s), (x: X) => p1(x) && p2(x))
      case TakeN(TakeN(s, n), m) => TakeN(once(s), math.min(n, m))
      case DropN(DropN(s, n), m) => DropN(once(s), n + m)
      case TakeN(m @ Mapped(s, f), n) => Mapped(TakeN(once(s), n), f)(m.tag)
      case TakeN(NumRange(a, b, sz), n) => NumRange(a, math.min(b, a + n), sz).asInstanceOf[Pipeline[X]]
      case Rechunked(Rechunked(s, _), k) => Rechunked(once(s), k)
      case Rechunked(Gen(seed, f, g, _), k) => Gen(seed, f, g, k)
      case Rechunked(NumRange(a, b, _), k) => NumRange(a, b, k).asInstanceOf[Pipeline[X]]
      case m @ Mapped(s, f) => Mapped(once(s), f)(m.tag)
      case Filtered(s, p1) => Filtered(once(s), p1)
      case TakeN(s, n) => TakeN(once(s), n)
      case DropN(s, n) => DropN(once(s), n)
      case Rechunked(s, k) => Rechunked(once(s), k)
      case s => s
    }
    val q = once(p)
    if (q == p) p else optimize(q)
  }

  /** compile onto the chunked transformers */
  def chunks[A](p: Pipeline[A]): Chunks[A] = p match {
    case Gen(seed, f, g, size) => Chunks.generate(seed)(f)(g)(size)
    case NumRange(a, b, size) => Chunks.range(a, b, size).asInstanceOf[Chunks[A]]
    case FromChunks(c) => c
    case m @ Mapped(s, f) => Chunks.mapWith(chunks(s))(ChunkBuf.taggedMapper(f)(m.tag))
    case Filtered(s, pr) => Chunks.filter(chunks(s))(pr)
    case TakeN(s, n) => Chunks.take(chunks(s))(n)
    case DropN(s, n) => Chunks.drop(chunks(s))(n)
    case Rechunked(s, k) => Chunks.rechunk(chunks(s))(k)
  }

  /** optimize, compile, fold — the terminal */
  def fold[A, S](p: Pipeline[A])(fo: Fold[A, S]): S = Chunks.fold(chunks(optimize(p)))(fo)

  /** how many operator nodes (what fusion buys) */
  def depth[A](p: Pipeline[A]): Int = p match {
    case Mapped(s, _) => 1 + depth(s)
    case Filtered(s, _) => 1 + depth(s)
    case TakeN(s, _) => 1 + depth(s)
    case DropN(s, _) => 1 + depth(s)
    case Rechunked(s, _) => 1 + depth(s)
    case _ => 1
  }
}
