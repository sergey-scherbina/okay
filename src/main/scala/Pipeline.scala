package okay

/**
 * The pipeline as a value (specs/staged-pipelines.md): a typed
 * operator tree — program-as-value applied to stream pipelines, the
 * Catalyst idea in our native ground. Build it, REWRITE it by rules
 * (map fusion, filter fusion, take pushdown into sources), then
 * compile it onto the chunked transformers. Every rule preserves
 * semantics (property-tested); what the optimizer buys is fewer
 * passes and less construction.
 */
enum Pipeline[A]:
  case Gen[S, A](seed: S, f: S => A, g: S => S, size: Int) extends Pipeline[A]
  case NumRange(from: Long, until: Long, size: Int) extends Pipeline[Long]
  case FromChunks(p: Chunks[A])
  /**
   * The element type of a reified node is EXISTENTIAL: at a `Mapped`
   * the intermediate `A` is gone, and with it any hope of building an
   * unboxed chunk for it — that is where threading a `ClassTag`
   * through the streaming API ran out of road.
   *
   * So the tag travels WITH the existential, captured where the node
   * was built and `B` was still concrete. `Chunks.map`'s specializing
   * worker can then be reconstructed at compile time, from data.
   */
  case Mapped[A, B](src: Pipeline[A], f: A => B)
                   (using val tag: scala.reflect.ClassTag[B]) extends Pipeline[B]
  case Filtered(src: Pipeline[A], p: A => Boolean)
  case TakeN(src: Pipeline[A], n: Int)
  case DropN(src: Pipeline[A], n: Int)
  case Rechunked(src: Pipeline[A], size: Int)

  def map[B: scala.reflect.ClassTag](f: A => B): Pipeline[B] =
    Pipeline.Mapped(this, f)
  def filter(p: A => Boolean): Pipeline[A] = Pipeline.Filtered(this, p)
  def take(n: Int): Pipeline[A] = Pipeline.TakeN(this, n)
  def drop(n: Int): Pipeline[A] = Pipeline.DropN(this, n)
  def rechunk(size: Int): Pipeline[A] = Pipeline.Rechunked(this, size)

object Pipeline {

  def generate[S, A](seed: S)(f: S => A)(g: S => S, size: Int = 64): Pipeline[A] =
    Gen(seed, f, g, size)

  def range(from: Long, until: Long, size: Int = 64): Pipeline[Long] =
    NumRange(from, until, size)

  /**
   * The rewrite rules, applied bottom-up to a fixpoint:
   * map/map and filter/filter fuse into one pass; take/take and
   * drop/drop combine; take pushes through map and into a range
   * (construction becomes O(n)); rechunk collapses into rechunk and
   * into a source's own chunk size.
   */
  def optimize[A](p: Pipeline[A]): Pipeline[A] =
    def once[X](q: Pipeline[X]): Pipeline[X] = q match
      case m @ Mapped(Mapped(s, f), g) =>
        Mapped(once(s), f.andThen(g))(using m.tag)
      case Filtered(Filtered(s, p1), p2) => Filtered(once(s), x => p1(x) && p2(x))
      case TakeN(TakeN(s, n), m) => TakeN(once(s), math.min(n, m))
      case DropN(DropN(s, n), m) => DropN(once(s), n + m)
      case TakeN(m @ Mapped(s, f), n) => Mapped(TakeN(once(s), n), f)(using m.tag)
      case TakeN(NumRange(a, b, sz), n) => NumRange(a, math.min(b, a + n), sz)
      case Rechunked(Rechunked(s, _), k) => Rechunked(once(s), k)
      case Rechunked(Gen(seed, f, g, _), k) => Gen(seed, f, g, k)
      case Rechunked(NumRange(a, b, _), k) => NumRange(a, b, k)
      case m @ Mapped(s, f) => Mapped(once(s), f)(using m.tag)
      case Filtered(s, p1) => Filtered(once(s), p1)
      case TakeN(s, n) => TakeN(once(s), n)
      case DropN(s, n) => DropN(once(s), n)
      case Rechunked(s, k) => Rechunked(once(s), k)
      case s => s

    val q = once(p)
    if q == p then p else optimize(q)

  /** compile onto the chunked transformers */
  def chunks[A](p: Pipeline[A]): Chunks[A] = p match
    case Gen(seed, f, g, size) => Chunks.generate(seed)(f)(g)(size)
    case NumRange(a, b, size) => Chunks.range(a, b, size)
    case FromChunks(c) => c
    case m @ Mapped(s, f) =>
      // the tag the node carried is what makes this chunk unboxed
      Chunks.mapWith(chunks(s))(ChunkBuf.taggedMapper(f)(using m.tag))
    case Filtered(s, pr) => Chunks.filter(chunks(s))(pr)
    case TakeN(s, n) => Chunks.take(chunks(s))(n)
    case DropN(s, n) => Chunks.drop(chunks(s))(n)
    case Rechunked(s, k) => Chunks.rechunk(chunks(s))(k)

  /** optimize, compile, fold — the terminal */
  def fold[A, S](p: Pipeline[A])(using Fold[A, S]): S =
    Chunks.fold(chunks(optimize(p)))

  /** how many operator nodes (what fusion buys) */
  def depth[A](p: Pipeline[A]): Int = p match
    case Mapped(s, _) => 1 + depth(s)
    case Filtered(s, _) => 1 + depth(s)
    case TakeN(s, _) => 1 + depth(s)
    case DropN(s, _) => 1 + depth(s)
    case Rechunked(s, _) => 1 + depth(s)
    case _ => 1

}

/**
 * Whole-stage codegen (specs/staged-pipelines.md), the inline half of
 * staged-tagless: the staged artifact is an INLINE PROGRAM SHAPE, not
 * a carrier value — the same conclusion staged-effects reached, and
 * the same choice rule one level up: the Pipeline TREE is for tools
 * (optimize, inspect, ship), the inline shape is for speed. A GADT
 * tree cannot partially evaluate through `inline match` (pattern
 * binding erases inline-ness of subtrees), so the staged pipeline is
 * spelled with these combinators; nested calls beta-reduce into ONE
 * while-loop over the source with every lambda inlined at its use —
 * no operator dispatch, no per-element allocation.
 */
object Staged {

  /** a staged stream in push mode: give it an emit, it drives the
   * source; emit answering false stops the loop */
  type Push[A] = (A => Boolean) => Unit

  inline def range(a: Long, b: Long): Push[Long] = emit =>
    var i = a
    var go = true
    while go && i < b do
      go = emit(i)
      i += 1

  /** an infinite generator — bound it with take */
  inline def gen[S, A](seed: S, inline f: S => A, inline g: S => S): Push[A] = emit =>
    var s = seed
    var go = true
    while go do
      go = emit(f(s))
      s = g(s)

  inline def map[A, B](inline src: Push[A], inline f: A => B): Push[B] =
    emit => src(x => emit(f(x)))

  inline def filter[A](inline src: Push[A], inline p: A => Boolean): Push[A] =
    emit => src(x => !p(x) || emit(x))

  inline def take[A](inline src: Push[A], n: Int): Push[A] = emit =>
    var left = n
    if left > 0 then src { x =>
      left -= 1
      emit(x) && left > 0
    }

  inline def drop[A](inline src: Push[A], n: Int): Push[A] = emit =>
    var toDrop = n
    src(x => if toDrop > 0 then { toDrop -= 1; true } else emit(x))

  /** the staged terminal: one fused loop, an accumulator, nothing else */
  inline def fold[A, S](inline src: Push[A])(z: S)(inline add: (S, A) => S): S =
    var acc = z
    src { x =>
      acc = add(acc, x)
      true
    }
    acc
}
