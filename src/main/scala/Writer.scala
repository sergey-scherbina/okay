package okay

/**
 * The Writer effect IS a stream: telling w emits w, and a writer
 * program is already the stream — run is a fold over it, toLazyList
 * consumes it lazily, no reinterpretation pass anywhere. The element
 * type is kept SEPARATE from the program's answer: `A ! Writer % W`
 * reads "computes A, telling W". (Producer is the diagonal cousin: it
 * emits its own answers, one type for both.)
 *
 * The operation is a GADT with ONE constructor, and both halves of
 * that matter.
 *
 * One constructor, `Say(w): Writer[W, Unit]`, because a tell answers
 * NOTHING — it emits a value, it does not produce one. Anything a
 * caller wants back it says explicitly (`tell(w).map(_ => w)`).
 *
 * A GADT, because the constructor is what makes that answer type
 * RECOVERABLE. Under a `Bind` the answer type is existential; matching
 * `Say(w)` refines it to `Unit`, so resuming the continuation is
 * `k(())` and asserts nothing. The previous encoding was an identity
 * signature — `opaque type Writer[W, +A] = W`, the operation IS the
 * told value, no node at all — which cost nothing to build and could
 * not recover the answer type afterwards, so twelve sites asserted it.
 * Measured, the wrapper costs 25% of a build-and-fold (59.8 -> 75.0us
 * per 10k tells); it buys back every one of those assertions and one
 * real limitation besides: a told String used to be just a String, so
 * a row could only forward effects whose operations were
 * class-distinct from W. A `Say` is class-distinct from everything.
 * docs/existentials.md has the five encodings tried before this one.
 */
enum Writer[+W, +A]:
  case Say(w: W) extends Writer[W, Unit]

/**
 * An operation IS its element — now by pattern match rather than by
 * representation, so it is total and asserts nothing.
 */
def out[W, A](w: Writer[W, A]): W = w match
  case Writer.Say(x) => x

object Writer {

  /** the operation: telling w, answering nothing — the ONLY
   * constructor, which is what makes the answer type recoverable */
  inline def apply[W](w: W): Writer[W, Unit] = Say(w)

  /** tell w: emit it as an operation, which answers NOTHING */
  inline def tell[W](w: W): Unit ! Writer % W = effect(Writer(w))

  /**
   * The continuation after a value a stream view has just HANDED OVER,
   * applied now — the moment every consumer's pull counting has always
   * relied on — except that a throw from it is held back as the rest:
   * a program that throws when next stepped. Applying it bare lost the
   * value being handed over whenever the source's next step threw while
   * being built (`Source.of` over a stream whose `uncons` throws; found
   * by source-merge-via-ready, `TestWriterToldBeforeThrow`). Free while
   * nothing throws: a JVM `try` costs nothing on the path it does not
   * take, and the `Delay` is allocated only for the throw.
   */
  private[okay] inline def toldThen[F[+_], A](k: Unit => A ! F): A ! F =
    try k(()) catch case e: Throwable => Free.Delay(() => throw e)


  import scala.annotation.tailrec
  import !.*

  /**
   * Fold everything told into any Fold algebra, forwarding the
   * effects F. Like State.handle, a bespoke tail-recursive loop — the
   * accumulator has to be threaded through the loop itself, which an
   * answer-polymorphic relay handler cannot hold.
   */
  def fold[W, S, A, F[+_]](a: A ! Writer % W + F)(using Distinct[Writer % W + F])
                          (using TypeableK[Writer % W], Fold[W, S]): (S, A) ! F =
    val K = summon[Fold[W, S]]
    // GADT refinement needs a stable path, not an expression: bound to
    // a val it gives `S` back from each test, so none of this casts
    K match
      // the same dispatch `Chunks.fold` makes, for the same reason: a
      // fold that arrives as data has no step to inline, so the one
      // thing left is to ask what its accumulator is. GADT refinement
      // gives `S` back from each type test, so none of this casts.
      case l: Fold.OfLong[W @unchecked] => foldWith[W, Long, A, F](a)(l.initLong)((s, w) => l.addLong(s, w))
      case i: Fold.OfInt[W @unchecked] => foldWith[W, Int, A, F](a)(i.initInt)((s, w) => i.addInt(s, w))
      case d: Fold.OfDouble[W @unchecked] => foldWith[W, Double, A, F](a)(d.initDouble)((s, w) => d.addDouble(s, w))
      case b: Fold.OfBoolean[W @unchecked] => foldWith[W, Boolean, A, F](a)(b.initBoolean)((s, w) => b.addBoolean(s, w))
      case _ => foldWith(a)(K.init)((s, w) => K.add(s, w))

  /**
   * The loop itself, with the step taken at the call site.
   *
   * `inline` so the four dispatched branches above each beta-reduce
   * their step into the loop body rather than calling through a
   * `Function2`, whose `apply` erases `(Object, Object)Object` and
   * would put back exactly the boxing the dispatch removed. That trap
   * cost 27.5us against 7.8 when it was first written without inline
   * in `Fold.long`, so it is worth spelling out.
   */
  inline def foldWith[W, S, A, F[+_]](a: A ! Writer % W + F)(using Distinct[Writer % W + F])(z: S)
                                     (inline step: (S, W) => S)
                                     (using TypeableK[Writer % W]): (S, A) ! F =
    loopWith[W, S, A, (S, A), F](a)(z)(step)((s, a) => (s, a))

  /**
   * The loop itself, with a `finish` applied to the accumulator AND
   * the program's answer where the PROGRAM ends — inside the loop,
   * never as a `.map` over the residual: a map wrapped around a
   * program that still forwards effects makes every forwarded node
   * left-nested under it, and `resume` then rotates each of them
   * again (either-scalarised, 2026-09-09: one outer `.map` cost 61 KB
   * over 667 forwarded operations — more than the accumulator it was
   * finishing). The answer reaches the finisher so that a drain
   * wanting only the accumulator (`Source.runCollect`), or the
   * accumulator reshaped (`Source.concat`), finishes here too instead
   * of mapping the tuple away afterwards (writer-collect-loops).
   */
  inline def loopWith[W, S, A, R, F[+_]](a: A ! Writer % W + F)(z: S)
                                         (inline step: (S, W) => S)
                                         (inline finish: (S, A) => R)
                                         (using TypeableK[Writer % W]): R ! F = {
    def _loop(s: S)(x: A ! Writer % W + F): R ! F = loop(s)(x)

    // `split`, not `<|>` (split-without-either): no Either per tell.
    @tailrec def loop(s: S)(x: A ! Writer % W + F): R ! F = (x.resume: @unchecked) match
      case Return(a) => Return(finish(s, a))
      case Inject(e) => split[Writer % W, F](e) {
          // matching the constructor refines the answer type to Unit:
          // the program ends here, and a tell ends it with nothing —
          // the ascription is where the refined value meets the loop
          case Say(v) => Return(finish(step(s, v), ())): R ! F
        } { e => Inject(e).map(finish(s, _)) }
      case Bind(Inject(e), k) => split[Writer % W, F](e) { w0 =>
          // here it refines the CONTINUATION's domain, so this is an
          // ordinary call and not an assertion; the checker cannot see
          // that `Say` is the only constructor under an existential
          // answer type — the same claim `resume`'s @unchecked makes
          (w0: @unchecked) match
            case Say(v) => loop(step(s, v))(k(()))
        } { e => Inject(e).flatMap(x => _loop(s)(k(x))) }

    loop(z)(a)
  }

  /**
   * A fold that STOPS (specs/fold-until.md): `loopWith`'s walk with
   * an early `Return(end(s))` the moment the state is done. The
   * `Bind(Inject(Say), k)` arm does not call `k` then, which is what
   * stops the producer: nothing past the satisfying tell is built,
   * and an `F` operation that would have followed it is never
   * performed. Answers `R` alone — a fold that stopped early never
   * saw the program's answer, and a signature promising it would
   * have to invent one.
   */
  def foldUntil[W, S, A, R, F[+_]](a: A ! Writer % W + F)(using Distinct[Writer % W + F])
                                  (using TypeableK[Writer % W], FoldUntil[W, S, R]): R ! F = {
    val K = summon[FoldUntil[W, S, R]]
    def _loop(s: S)(x: A ! Writer % W + F): R ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! Writer % W + F): R ! F =
      if K.done(s) then Return(K.end(s))
      else (x.resume: @unchecked) match
        case Return(_) => Return(K.end(s))
        case Inject(e) => split[Writer % W, F](e) {
            case Say(v) => Return(K.end(K.add(s, v))): R ! F
          } { e => Inject(e).map(_ => K.end(s)) }
        case Bind(Inject(e), k) => split[Writer % W, F](e) { w0 =>
            (w0: @unchecked) match
              case Say(v) => loop(K.add(s, v))(k(()))
          } { e => Inject(e).flatMap(x => _loop(s)(k(x))) }

    loop(K.init)(a)
  }

  /** collect everything told, in order, forwarding the effects F */
  def run[W, A, F[+_]](a: A ! Writer % W + F)(using Distinct[Writer % W + F])
                      (using TypeableK[Writer % W]): (Seq[W], A) ! F =
    // a List built by prepending and reversed ONCE at the end, not a
    // Vector appended per tell: either-scalarised (2026-09-09) measured
    // `Writer.run` at 197 B per tell with the split, Either and Option
    // all costing NOTHING on this loop — the whole price was `:+`
    // (~150 B per append). A cons is 24 B, the reverse is one pass.
    loopWith[W, List[W], A, (Seq[W], A), F](a)(Nil)((s, w) => w :: s)((s, a) => (s.reverse, a))

  /**
   * `run` answering a `Vector`, with the answer KEPT — which a
   * `Blob.getSource` needs because its answer is the outcome.
   *
   * It used to be its own copy of the loop, split on G rather than on
   * `Writer % W`, because `writerK` at a parameterised W (`Writer %
   * Chunk[Byte]`) was an unchecked E092 test until
   * writer-typeablek-by-class (2026-09-19); with the test now the
   * class of `Say` there is one loop, and this is a call to it — a
   * cons per tell and one reverse, not a `Vector :+` per tell
   * (writer-collect-loops).
   */
  def collect[W, A, G[+_]](a: A ! Writer % W + G)(using Distinct[Writer % W + G])
                          (using TypeableK[Writer % W]): (Vector[W], A) ! G =
    loopWith[W, List[W], A, (Vector[W], A), G](a)(Nil)((s, w) => w :: s)((s, a) => (s.reverse.toVector, a))

  /**
   * Map the told values, keeping the PROGRAM.
   *
   * `Stream.map` exists already and lands in LazyList — which is the
   * right answer for a pure stream and the wrong one for a stream
   * that still has effects to perform: the elements would be pulled
   * by whoever forces the list, not by whoever consumes it. This one
   * transforms the telling in place and forwards the G-operations
   * untouched, in order, so the result is a source like the input.
   *
   * It is also what makes two DIFFERENTLY typed sources mergeable:
   * `Free` is invariant in its row, so re-telling at a common type is
   * a WALK over the program's nodes, not a subtyping step (see
   * Source.merge) — `Writer[+W, +A]` (2026-09-02) narrows what the
   * walk must actually rebuild: see `widen` below for the case with
   * no transform, where only the Free nodes need rebuilding and the
   * told OPERATION can be reused as is.
   */
  def map[W, V, A, G[+_] : TypeableK](a: A ! Writer % W + G)(using Distinct[Writer % W + G], TypeableK[Writer % W])(f: W => V)
  : A ! Writer % V + G = (a.resume: @unchecked) match
    case Free.Return(x) => Free.Return(x)
    // THE HANDLED SIGNATURE IS TESTED FIRST (distinct-on-handlers,
    // 2026-09-24). It was `split[G, Writer % W]`, the rest first; and
    // when the rest is inferred as the row itself — `Writer.collect(
    // Writer.map(p)(f))` solves G = Writer % W, `F | F` being `F` — every
    // Say passed G's test and was forwarded UNMAPPED: a silently wrong
    // answer (TestDistinct). Distinct cannot see that row, because the
    // union collapses; testing Writer first makes it right.
    case Inject(e) => split[Writer % W, G](e)
      // the constructor refines the answer type to Unit on both
      // sides, so the re-told operation types with nothing asserted
      { case Say(w) => Inject(Writer(f(w))): A ! Writer % V + G }
      (g => Inject(g): A ! Writer % V + G)
    case Bind(Inject(e), k) => split[Writer % W, G](e)
      { w0 => (w0: @unchecked) match
          case Say(w) => Inject(Writer(f(w))).flatMap(_ => map[W, V, A, G](k(()))(f)): A ! Writer % V + G }
      (g => Inject(g).flatMap(x => map[W, V, A, G](k(x))(f)))

  /**
   * ONE TOLD VALUE BECOMES MANY (merge-chunk-size-curve-inverted,
   * 2026-09-10) — `map`'s one-to-many sibling, and the reason it
   * exists is measured rather than aesthetic.
   *
   * `Source.unchunked` was `through(s)(Stage.unchunk)`: a Take/Writer
   * COROUTINE PAIRING, where every element of every chunk crosses the
   * handshake between two suspended programs. Its cost per element
   * grows with the chunk it came from — measured on §6b's merge at
   * chunk 16 / 256 / 1024, quiet box, with the merge alone flat
   * beside it:
   *
   *   merge alone       245.2 -> 237.4 -> 280.0 us
   *   merge + unchunk   231.7 -> 262.3 -> 430.3 us
   *   unchunk costs       ~0  ->  ~25  ->  ~150
   *
   * This walks the program ONCE and re-tells each element directly,
   * so no element crosses a coroutine boundary: what is rebuilt is a
   * plain Free chain the runner walks linearly.
   *
   * `f` may return any number of values, including none — an empty
   * result drops the told value, which makes this a filter as well as
   * an expansion.
   */
  def expand[W, V, A, G[+_] : TypeableK](a: A ! Writer % W + G)(using Distinct[Writer % W + G], TypeableK[Writer % W])(f: W => IndexedSeq[V])
  : A ! Writer % V + G =
    def tellAll(vs: IndexedSeq[V], i: Int): Unit ! Writer % V + G =
      if i >= vs.length then Free.Return(())
      else Inject(Writer(vs(i))).flatMap(_ => tellAll(vs, i + 1))

    (a.resume: @unchecked) match
      case Free.Return(x) => Free.Return(x)
      // Writer tested first, for `map`'s reason (above)
      case Inject(e) => split[Writer % W, G](e)
        { case Say(w) => tellAll(f(w), 0).asInstanceOf[A ! Writer % V + G] }
        (g => Inject(g): A ! Writer % V + G)
      case Bind(Inject(e), k) => split[Writer % W, G](e)
        { w0 => (w0: @unchecked) match
            case Say(w) => tellAll(f(w), 0).flatMap(_ => expand[W, V, A, G](k(()))(f)): A ! Writer % V + G }
        (g => Inject(g).flatMap(x => expand[W, V, A, G](k(x))(f)))

  /**
   * WHAT A PART OF THE PROGRAM TOLD, as its answer — mtl's `listen`,
   * the Writer dual of `Reader.local` and `Throws.recover`
   * (specs/core-gaps.md stage 2).
   *
   *     Writer.listen(step)   :  (A, Seq[W]) ! Writer % W + G
   *
   * The scope's tells are NOT taken away from the outer handler: each
   * one is re-told at its own place as it is heard, so the order
   * against every other effect is exactly the one without `listen`,
   * and a raise inside the scope still leaves the tells before it
   * told. What `listen` adds is a copy of the scope's tells, and only
   * the scope's, in the answer.
   */
  def listen[W, A, G[+_]](a: A ! Writer % W + G)(using Distinct[Writer % W + G], TypeableK[Writer % W])
  : (A, Seq[W]) ! Writer % W + G =
    def go(heard: List[W])(x: A ! Writer % W + G): (A, Seq[W]) ! Writer % W + G = (x.resume: @unchecked) match
      case Free.Return(v) => Free.Return((v, heard.reverse))
      // Writer tested first, for `map`'s reason (above)
      case Inject(e) => split[Writer % W, G](e)
        { case Say(w) => Inject[Writer % W + G, Unit](Writer(w)).map(u => (u, (w :: heard).reverse)): (A, Seq[W]) ! Writer % W + G }
        (g => Inject[Writer % W + G, A](g).map(v => (v, heard.reverse)))
      case Bind(Inject(e), k) => split[Writer % W, G](e)
        { w0 => (w0: @unchecked) match
            case Say(w) => Inject[Writer % W + G, Unit](Writer(w)).flatMap(_ => go(w :: heard)(k(()))): (A, Seq[W]) ! Writer % W + G }
        (g => Inject(g).flatMap(x => go(heard)(k(x))))
    go(Nil)(a)

  /**
   * A PART OF THE PROGRAM'S WHOLE OUTPUT, REWRITTEN — mtl's `censor`
   * (specs/core-gaps.md stage 2): `f` sees everything the scope told
   * and answers what to tell in its place.
   *
   *     Writer.censor(step)(ws => if ws.size > 1 then Seq(s"${ws.size} steps") else ws)
   *
   * Seeing it all means WAITING for all of it: the scope's tells are
   * held back and told at the scope's END, as `f` of them — so a raise
   * inside the scope drops them, and they come after every other
   * effect the scope performed. That is the definition, not a defect.
   * A rewrite of each told value on its own keeps every tell in its
   * place, and it already exists: `map` (one to one) and `expand` (one
   * to many, and a filter) over the same scope.
   */
  def censor[W, A, G[+_]](a: A ! Writer % W + G)(f: Seq[W] => Seq[W])
                         (using Distinct[Writer % W + G], TypeableK[Writer % W]): A ! Writer % W + G =
    def tellAll(ws: IndexedSeq[W], i: Int): Unit ! Writer % W + G =
      if i >= ws.length then Free.Return(())
      else Inject[Writer % W + G, Unit](Writer(ws(i))).flatMap(_ => tellAll(ws, i + 1))

    import okay.Row.at
    collect[W, A, G](a).at[Writer % W + G].flatMap((ws, x) => tellAll(f(ws).toIndexedSeq, 0).map(_ => x))

  /**
   * Re-tell at a WIDER element type with NO transform — `map`'s
   * identity case, priced separately because it is common (every
   * merge of differently-typed sources goes through it) and cheaper
   * now that `Writer[+W, +A]` is covariant: the told OPERATION
   * (`Say(w): Writer[W, Unit]`) already IS a `Writer[V, Unit]` for
   * any `V >: W`, so this only rebuilds the Free nodes the walk
   * cannot avoid (Free stays invariant in its row) — `map`'s per-
   * element `Writer(f(w))` allocation is gone, `f` never runs.
   */
  def widen[W, V >: W, A, G[+_] : TypeableK](a: A ! Writer % W + G)
  : A ! Writer % V + G = a match
    // a deferred head stays deferred, as in `!.widen`: the walk
    // begins when the program runs, not when it is widened
    case Free.Delay(t) => Free.Delay(() => widen[W, V, A, G](t()))
    case Bind(Free.Delay(t), f) => Free.defer(() => widen(t()))(x => widen[W, V, A, G](f(x)))
    case _ => (a.resume: @unchecked) match
      case Free.Return(x) => Free.Return(x)
      case Inject(e) => split[G, Writer % W](e)
        (g => Inject(g): A ! Writer % V + G)
        // Say is Writer's ONLY constructor, so a value that reaches
        // here IS one — sound by the enum's shape, same as map's
        // Say(w) destructure; @unchecked because BINDING the whole
        // instance (not just its field) needs W's erased type
        // argument to verify, which map's plain destructure does not
        { case sw @ (_: Say[W, Unit] @unchecked) => Inject(sw: Writer[V, Unit]) }
      case Bind(Inject(e), k) => split[G, Writer % W](e)
        (g => Inject(g).flatMap(x => widen[W, V, A, G](k(x))))
        { case sw @ (_: Say[W, Unit] @unchecked) => Inject(sw: Writer[V, Unit]).flatMap(_ => widen[W, V, A, G](k(()))) }

  /**
   * ANY stream as a writer program: its elements told one by one, its
   * own effects F performed at each pull.
   *
   * The direction the library was missing. A writer program is a
   * stream (the instances below), and every stream unfolds into
   * LazyList — but nothing turned a stream back into the program
   * shape that `through`, `pipe` and the stage combinators consume.
   * So a Channel, a List, a LazyList or a Producer becomes a source
   * here, and the whole pipeline vocabulary applies to it.
   *
   * Lazy: nothing is pulled until the result is consumed, one element
   * per pull, and the F-operations stay in the row rather than being
   * run behind the caller's back. The deferral is `pure(()).flatMap`
   * — a `Bind(Return(()), k)` node `!.resume` must ROTATE away before
   * reading past it (Effects.scala, the tailrec rotation cases) — and
   * it is load-bearing ONCE: the recursive step below already sits
   * inside the PREVIOUS step's `flatMap`, which is itself deferral
   * enough for the next pull. Re-wrapping every element cost a
   * rotation per pull for nothing (profiled: writer-of-resume-fix,
   * specs/writer-covariance.md — 38% of okaySourceMerge's CPU
   * samples were exactly these two rotation lines).
   */
  def of[S[_], F[+_], A](s: S[A])(using St: Stream[S, F]): Unit ! Writer % A + F =
    okay.pure[Writer % A + F, Unit](()).flatMap: _ =>
      ofLoop[S, F, A](s)

  private def ofLoop[S[_], F[+_], A](s: S[A])(using St: Stream[S, F]): Unit ! Writer % A + F =
    !.widen[Option[(A, S[A])], F, Writer % A](St.uncons(s)).flatMap:
      case Some((a, rest)) =>
        okay.effect[Writer % A + F, Unit](Writer(a)).flatMap(_ => ofLoop[S, F, A](rest))
      case None => okay.pure(())

  /**
   * The observation of the writer as codata: the same shape as
   * Stream.uncons but with a richer functor — Either[A, (W, rest)]
   * instead of Option[(W, rest)]. The told values come out one by one
   * through Right, on demand; when they end, Left carries the
   * program's own answer (nothing is lost — by then every told value
   * has already been observed). run/fold are this loop with a Fold
   * accumulating the Rights.
   */
  def uncons[W, A](a: A ! Writer % W): Either[A, (W, A ! Writer % W)] = (a.resume: @unchecked) match
    case Free.Return(a) => Left(a)
    case Inject(Say(w)) => Right((w, Free.Return(())))
    // `k(())` is applied as `w` is handed over (TestFoldUntilStreams
    // counts pulls by it) — but a THROW from it must not take `w` with
    // it: it becomes the rest, thrown at the next step (source-merge-
    // via-ready, TestWriterToldBeforeThrow): `Writer.toldThen`.
    case Bind(Inject(Say(w)), k) => Right((w, Writer.toldThen(k)))

  /**
   * The same observation for a writer program performing ARBITRARY
   * effects G alongside its telling: the next told value arrives
   * inside G — the G-operations met on the way are carried into the
   * answer (deferred, not run). Any structured effect handler (State,
   * Reader, Throws, ...) forwards the telling, so it can be run over
   * the program FIRST — handlers are stream transformers — and the
   * Handler-able residue (Async, say) is what the consumer pays at
   * each pull. G is split from the told values by its runtime class.
   */
  def uncons[W, A, G[+_] : TypeableK](a: A ! Writer % W + G)(using TypeableK[Writer % W])
  : Either[A, (W, A ! Writer % W + G)] ! G = (a.resume: @unchecked) match
    case Free.Return(a) => okay.pure(Left(a))
    // Writer tested first, for `map`'s reason: with the rest inferred
    // as the Writer itself, a rest-first split sent every Say to G and
    // answered Left — no elements, the told values escaped (TestDistinct)
    case Inject(e) => split[Writer % W, G](e)
      { case Say(w) => okay.pure(Right((w, Free.Return(())))): Either[A, (W, A ! Writer % W + G)] ! G }
      (g => Inject(g).map(Left(_)): Either[A, (W, A ! Writer % W + G)] ! G)
    case Bind(Inject(e), k) => split[Writer % W, G](e)
      { w0 => (w0: @unchecked) match
          // `toldThen`: the pure `uncons`'s reason above
          case Say(w) => okay.pure(Right((w, Writer.toldThen(k)))): Either[A, (W, A ! Writer % W + G)] ! G }
      (g => Inject(g).flatMap(x => uncons[W, A, G](k(x))))

  /**
   * Writer's split is COMPLETE, and by the CLASS of `Say` alone.
   *
   * `Say` is Writer's only constructor and is class-distinct from every
   * other signature's operations, so "is this a writer operation" is
   * one `isInstance` — total, needing no `Typeable[W]`, and therefore
   * free of the E092 "cannot be checked at runtime" warning that the
   * previous default paid at EVERY `Writer.fold`/`collect`/`run`/
   * `uncons` call site whose W was parameterised (`Chunk[Byte]`,
   * `Either[Bad, A]`, an abstract `O`): 23 `@nowarn`s across ten
   * modules, each explaining the same caveat (writer-typeablek-by-class,
   * 2026-09-19). What that default bought — telling `Writer % String`
   * from `Writer % Int` in ONE row by the told value's class — no
   * module used; it is `Writer.byValue` now, an opt-in
   * (`import okay.Writer.byValue.given`), and `Distinct` refuses a
   * two-Writer row without it, which is the direction Distinct.scala
   * says is the safe one: an unmarked instance is refused and fixed by
   * one import, where the reverse would pass a row that misroutes.
   *
   * IN Writer'S OWN COMPANION, not a bare top-level given (moved
   * 2026-09-19, writerk-companion-scope): a `given` here is in the
   * IMPLICIT SCOPE of every `TypeableK[Writer % W]` query, from any
   * package, with no import.
   */
  given writerK[W]: TypeableK[Writer % W] = typeableK[Writer % W](classOf[Writer.Say[?, ?]])
  // AND EVERY DOOR THAT SPLITS ON `Writer % W` TAKES ITS `TypeableK`
  // FROM THE CALLER (row-parametricity-forwarding-law, 2026-09-25):
  // a door that let `split` summon it HERE got this class test even
  // when the caller had `byValue` in scope, so `Distinct` accepted a
  // `Writer % Int + Writer % String` row on the finer test and
  // `collect[Int]` then took the String's Say by the coarser one — the
  // forwarding law caught it (TestRowForwarding). `run`, `fold` and
  // `foldUntil` already took it; `collect`, `map`, `expand` and
  // `uncons` do now. The `Stream` instance below (`writerStreamIn`)
  // cannot: a given has no per-W parameter, so it streams by class.

  /**
   * The finer test, opt-in: the told value's own class as well, which
   * is what separates `Writer % String + Writer % Int` in one row
   * (`TestRowIdentity` asserts they route correctly, `TestDistinct`
   * that the row is accepted) — declared `TypeableK.ByValue` so
   * `Distinct` can read that it is finer than the class. It needs a
   * `Typeable[W]`, and for a parameterised W that is an erased test
   * and an E092 warning at the call site: the price of the finer
   * question, paid only where it is asked.
   */
  object byValue:
    given writerK[W](using t: scala.reflect.Typeable[W]): TypeableK.ByValue[Writer % W] = new:
      // `Typeable.unapply` answers an Option for the told value's own
      // test (the JDK's Typeable has no boolean form)
      def test(x: Any): Boolean = x match
        case s: Writer.Say[?, ?] => t.unapply(s.w).isDefined
        case _ => false
}

/**
 * The fourth carrier, named: the pure (no other effect) writer
 * stream — one unfold, four carriers (LazyList by pure laziness,
 * Producer by identity operations, Feed by typed telling, Source by
 * the same telling plus Async). Every existing instance already
 * covers it — `Stream[[W] =>> A ! Writer % W, Pure]` below resolves
 * at `A = Unit`, and the elementwise extensions in Stream.scala match
 * its shape directly — so naming it adds nothing to the type system,
 * only to signatures and docs (producer-to-writer-carrier, stage 1:
 * a new streaming seam names its element in the type, `Feed[W]` when
 * it performs no other effect, `Source[W]` when it performs Async).
 *
 * THE TRAP `Produce` HAS IS MADE INSPECTABLE HERE, not closed by a
 * type error — verified directly (`sbt okayStreamJVM/compile` on
 * `val f: Feed[Int] = pure(5)` prints `[E190] ... Discarded non-Unit
 * value of type Int`), not assumed: `compileErrors` cannot see this,
 * since it reports hard errors only and munit's own macro drops
 * warnings entirely, so `TestGenerate` documents the fact rather than
 * asserting it through that tool. `Producer[A] = A ! Produce` makes
 * the element type the ANSWER type, so `pure(a): Producer[A]`
 * type-checks as an ORDINARY well-typed answer — nothing distinguishes
 * it from the honest `Producer[A]` that ends with `a`, and no warning
 * fires either — and it emits nothing: the note lives on `produce`'s
 * scaladoc, a comment, not a type (bit okay-watch, blob-source-seam).
 * `Feed[W]`'s answer is always `Unit`, so `pure(w): Feed[W]` for an
 * element `w` of any other type needs Scala's own value-discard
 * adaptation to compile at all, which — unlike Producer's identical-
 * looking mistake — a real compile FLAGS, and this repo's gate then
 * refuses as any other warning.
 */
type Feed[W] = Unit ! Writer % W

/**
 * `generate`/`put` materialize into `Feed` too — one unfold, four
 * carriers. `put` is tell as a delimited-control operation: shift
 * captures the continuation and resumes it with `()`, not the told
 * value.
 *
 * This replaces the diagonal `Teller[A] = A ! Writer % A`
 * (put-de-diagonal, 2026-09-19): its answer tied the program's own
 * result to the element type for no reason a real seam ever used —
 * nothing outside this file ever took a `Teller`, and `Put`'s answer
 * is `Unit` now, so the diagonal bought nothing the row's own
 * element didn't already carry.
 */
given Put[Feed] with
  final override inline def put[W](w: W): Unit /> Feed[W] =
    shift(k => Writer.tell(w).flatMap(_ => k(())))

/**
 * A writer program is a stream of its told values: the same
 * observation as Writer.uncons with the answer forgotten (Left becomes
 * the end) — an infinite teller unfolds on demand like any stream.
 * Named `feedStream` beside its G-effectful twin `writerStreamIn`
 * (feed-stream-given-name): a summon spells `feedStream[Unit]` instead
 * of the whole type lambda.
 */
given feedStream[A]: Stream[[W] =>> A ! Writer % W, Pure] = new:
  def uncons[W](s: A ! Writer % W): Option[(W, A ! Writer % W)] ! Pure =
    pure(Writer.uncons(s).toOption)

  /**
   * The specialized linear view of the PURE writer stream — the twin
   * of `Stream[Producer, Pure]`'s own override in Generate.scala, and
   * the one `Chunks.fold`/`foldLeft` will walk once `Chunks[A]` is
   * `Feed[Chunk[A]]`. No `Option`, no `Either`, no program built per
   * step: the DEFAULT `Iterator.unfold(s)(uncons(_).runWith)` pays all
   * three once per element — which on a chunked stream is once per
   * CHUNK, and was the whole of the ~2x stage 0 measured on
   * `Chunks.fold` (producer-to-writer-carrier, `## Results`). Needs
   * only `Handler[Pure]`, which every platform has — nothing here
   * blocks, so nothing here needs `CanBlock`, unlike `writerStreamIn`'s
   * G-forwarding twin below.
   */
  override def iterator[W](s: A ! Writer % W)(using Handler[Pure]): Iterator[W] =
    import !.*
    import scala.annotation.tailrec
    new Iterator[W]:
      private var cur: A ! Writer % W = s
      private var ready = false
      private var ended = false
      private var elem: W = scala.compiletime.uninitialized

      // `Say` is Writer's only constructor, so the two Inject shapes
      // are exhaustive over what a pure writer program can resume to
      @tailrec private def advance(): Unit = (cur: @unchecked) match
        case Free.Return(_) => ended = true
        case Inject(Writer.Say(w)) =>
          elem = w
          ready = true
          ended = true
        case Bind(Inject(Writer.Say(w)), k) =>
          elem = w
          ready = true
          cur = Writer.toldThen(k)
        case _ =>
          cur = cur.resume
          advance()

      def hasNext: Boolean =
        if !ready && !ended then advance()
        ready

      def next(): W =
        if !hasNext then throw java.util.NoSuchElementException("empty writer stream")
        ready = false
        elem

/**
 * And a writer program performing ARBITRARY effects G is a stream in
 * G: the same observation, with the G-operations met on the way
 * carried into the answer. This is the instance the concurrent
 * combinators ask for — `Channel.merge` needs a `Stream`, and an
 * asynchronous source (`Unit ! Writer % W + Async`) had none.
 */
given writerStreamIn[A, G[+_] : TypeableK]: Stream[[W] =>> A ! Writer % W + G, G] = new:
  def uncons[W](s: A ! Writer % W + G): Option[(W, A ! Writer % W + G)] ! G =
    Writer.uncons[W, A, G](s).map(_.toOption)

  /**
   * The specialized linear view, mirroring `Stream[Producer, Pure]`'s
   * own override in Generate.scala: no `Option`, no `Either`, no
   * per-step program construction — the DEFAULT `Iterator.unfold(s)
   * (uncons(_).runWith)` pays exactly those, once per CHUNK, which is
   * what was left over after `producer-writer-carrier-foldwriter-eager`
   * fixed the per-ELEMENT boxing (see Chunks.scala's `foldWriter` doc
   * and [[escape-analysis-box-elimination-boundary]]). A forwarded
   * `G`-operation is answered directly by `Handler[G].handle` — a
   * COMONADIC, single-operation interpretation (`Handler[Async]`'s own
   * `case Run(f) => f()` / `case Await(reg) => cb.block(reg)...`),
   * not a program built and run.
   */
  override def iterator[W](s: A ! Writer % W + G)(using H: Handler[G]): Iterator[W] =
    import !.*
    import scala.annotation.tailrec
    new Iterator[W]:
      private var cur: A ! Writer % W + G = s
      private var ready = false
      private var ended = false
      private var elem: W = scala.compiletime.uninitialized

      @tailrec private def advance(): Unit = cur match
        case Free.Return(_) => ended = true
        // Writer tested first, for `map`'s reason
        case Inject(e) =>
          split[Writer % W, G](e)(
            w0 => (w0: @unchecked) match
              case Writer.Say(w) => { elem = w; ready = true; ended = true }
          )(
            g => { val _ = H.handle(g); ended = true }
          )
        case Bind(Inject(e), k) =>
          split[Writer % W, G](e)(
            w0 => (w0: @unchecked) match
              case Writer.Say(w) => { elem = w; ready = true; cur = Writer.toldThen(k) }
          )(
            g => { cur = k(H.handle(g)); advance() }
          )
        case _ =>
          cur = cur.resume
          advance()

      def hasNext: Boolean =
        if !ready && !ended then advance()
        ready

      def next(): W =
        if !hasNext then throw java.util.NoSuchElementException("empty writer stream")
        ready = false
        elem

