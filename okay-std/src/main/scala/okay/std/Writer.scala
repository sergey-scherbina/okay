package okay.std

import okay.{!, +, %, Answers, Handler, Pure, Run, TypeableK, effect, pure, typeableK}
import okay.core.{Eff, Row, Split}
import okay.{Cps, />>}
import okay.{Fold, FoldUntil}
import okay.one.Cont
import okay.one.handler.{Stateful, Handler as OneHandler}
import okay.one.row.{In, Pure as OnePure}
import okay.one.machine.Machine.Head
import scala.annotation.tailrec


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
  /**
   * RECORD what a program asks for without answering any of it: each operation of `F` is told to a `Writer`
   * and then performed as before, so the program keeps `F` and gains `Writer % W`.
   *
   *     Writer.tracing(prog)([X] => (e: Users[X]) => e.toString)   // A ! Writer % String + Users + G
   *
   * It records before anything is interpreted, so it sees the program's own asks whatever answers them.
   * Re-emitting `e` does not loop: each operation is TRANSLATED (the kernel's `Translating`), its program run
   * outside the translating handler, where `F` is the handler of `F` around.
   */
  def tracing[A, F[_], W, Es[_]](prog: A ! Es)(show: [X] => F[X] => W)(using s: Split[Es, F], k: okay.KeyOf[F])
  : A ! Writer % W + F + s.Rest =
    prog.translate[F, Writer % W + F]([X] => (e: F[X]) => tell(show(e)) >> effect[F, X](e))


  /** handle-frames-loops: what a Writer operation told, read off an erased operation — `Say` is the only
   * constructor, so this is that `w` */
  @scala.annotation.publicInBinary private[okay] def told[W](op: Any): W = op.asInstanceOf[Say[W, Unit]].w


  /** the operation: telling w, answering nothing — the ONLY
   * constructor, which is what makes the answer type recoverable */
  inline def apply[W](w: W): Writer[W, Unit] = Say(w)

  /** tell w: emit it as an operation, which answers NOTHING */
  inline def tell[W](w: W): Unit ! Writer % W = effect[Writer % W, Unit](Say(w))

  /**
   * The continuation after a value a stream view has just HANDED OVER,
   * applied now, except that a throw from it is held back as the rest:
   * a program that throws when run. Applying it bare lost the value
   * being handed over whenever the source's next step threw while
   * being built (`Source.of` over a stream whose `uncons` throws; found
   * by source-merge-via-ready, `TestWriterToldBeforeThrow`). The views
   * here no longer need it — a stepped program's rest is resumed when
   * it is next pulled, so a throw from it comes at that pull — it stays
   * for a view of one's own that applies a continuation at once.
   */
  private[okay] inline def toldThen[A, Es[_]](k: Unit => A ! Es): A ! Es =
    try k(()) catch case e: Throwable => okay.defer[A, Es](throw e)


  /**
   * Fold everything told into any Fold algebra, wherever `Writer % W`
   * is among the program's effects, the rest forwarded. The fold is
   * the kernel's `Stateful` handler: its accumulator a register of the
   * machine, each tell answered in place — no capture, no loop of its
   * own.
   *
   * It used to dispatch on the accumulator (`Fold.OfLong`, ...) to keep
   * a `Long` unboxed through its own loop; the machine's state is
   * generic, so a dispatch would buy nothing here and is gone.
   */
  def fold[W, S, A, Es[_]](a: A ! Es)(using s: Split[Es, Writer % W], K: Fold[W, S]): (S, A) ! s.Rest =
    loopWith[W, S, A, (S, A), Es](a)(K.init)((st, w) => K.add(st, w))((st, x) => (st, x))

  /** The fold with the step given at the call site: the state and the program's answer at its end. */
  def foldWith[W, S, A, Es[_]](a: A ! Es)(z: S)(step: (S, W) => S)(using s: Split[Es, Writer % W]): (S, A) ! s.Rest =
    loopWith[W, S, A, (S, A), Es](a)(z)(step)((st, x) => (st, x))

  /**
   * The fold with a `finish` applied to the accumulator AND the
   * program's answer where the PROGRAM ends — the handler's `ret`,
   * never a `.map` over the residual. The answer reaches the finisher
   * so that a drain wanting only the accumulator (`Source.runCollect`),
   * or the accumulator reshaped (`Source.concat`), finishes here too
   * instead of mapping the tuple away afterwards (writer-collect-loops).
   */
  def loopWith[W, S, A, R, Es[_]](a: A ! Es)(z: S)(step: (S, W) => S)(finish: (S, A) => R)
                                 (using s: Split[Es, Writer % W]): R ! s.Rest =
    Run.install[Writer % W, A, R, Es, s.Rest](a)(new Stateful[Writer % W, OnePure, S, A, R]:
      def initial: S = z
      def ret(st: S, x: A) = Cont.pure(finish(st, x))
      def value[X](st: S, op: Writer[W, X]): X = op match { case Say(_) => () }
      def next[X](st: S, op: Writer[W, X]): S = op match { case Say(w) => step(st, w) })

  /**
   * A fold that STOPS (specs/fold-until.md): the program STEPPED (the
   * kernel's `Machine.Head`), each tell added to the state, the moment
   * the state is done the paused program dropped — which is what stops the
   * producer: nothing past the satisfying tell runs, and an operation
   * that would have followed it is never performed. The other effects'
   * operations met on the way are performed where the fold is, the
   * rest resumed with their answers. Answers `R` alone — a fold that
   * stopped early never saw the program's answer, and a signature
   * promising it would have to invent one.
   */
  def foldUntil[W, S, A, R, Es[_]](a: A ! Es)(using s: Split[Es, Writer % W], K: FoldUntil[W, S, R]): R ! s.Rest =
    if K.done(K.init) then pure(K.end(K.init))
    else Run.of(Cont.defer(Stepping.until(Stepping.start(a), Telling.key, K.init, Telling[W](), K)))

  /** the handler as a value: `p.handle(Writer.log)` answers what was told, in order, and `A` — the kernel's
   * `Stateful`, a cons a tell, reversed once at the end */
  def log[W]: Handler[Writer % W, [A] =>> (Seq[W], A)] = new Handler[Writer % W, [A] =>> (Seq[W], A)]:
    def kernel[T <: Row, A]: OneHandler[Writer % W, T, A, (Seq[W], A)] = new Stateful[Writer % W, T, List[W], A, (Seq[W], A)]:
      def initial: List[W] = Nil
      def ret(st: List[W], a: A): Eff[T, (Seq[W], A)] = Cont.pure((st.reverse, a))
      def value[X](st: List[W], op: Writer[W, X]): X = op match { case Say(_) => () }
      def next[X](st: List[W], op: Writer[W, X]): List[W] = op match { case Say(w) => w :: st }

  /** collect everything told, in order, forwarding the rest of the effects */
  def run[W, A, Es[_]](a: A ! Es)(using s: Split[Es, Writer % W]): (Seq[W], A) ! s.Rest =
    // a List built by prepending and reversed ONCE at the end, not a
    // Vector appended per tell: either-scalarised (2026-09-09) measured
    // the whole price of `Writer.run` at `:+` (~150 B per append). A
    // cons is 24 B, the reverse is one pass.
    a.handle(log[W])

  /**
   * `run` answering a `Vector`, with the answer KEPT — which a
   * `Blob.getSource` needs because its answer is the outcome. A cons
   * per tell and one reverse, not a `Vector :+` per tell
   * (writer-collect-loops).
   */
  def collect[W, A, Es[_]](a: A ! Es)(using s: Split[Es, Writer % W]): (Vector[W], A) ! s.Rest =
    loopWith[W, List[W], A, (Vector[W], A), Es](a)(Nil)((st, w) => w :: st)((st, x) => (st.reverse.toVector, x))

  /**
   * Map the told values, keeping the PROGRAM.
   *
   * `Stream.map` exists already and lands in LazyList — which is the
   * right answer for a pure stream and the wrong one for a stream
   * that still has effects to perform: the elements would be pulled
   * by whoever forces the list, not by whoever consumes it. This one
   * transforms the telling in place and forwards the other operations
   * untouched, in order, so the result is a source like the input.
   *
   * Each tell is TRANSLATED (the kernel's `Translating`): `tell(f(w))`
   * run outside, at the `Writer % V` of the place — answered in place,
   * nothing captured, no walk. It is also what makes two DIFFERENTLY
   * typed sources mergeable: re-telling at a common type (see `widen`
   * below for the case with no transform).
   */
  def map[W, V, A, Es[_]](a: A ! Es)(using s: Split[Es, Writer % W])(f: W => V): A ! Writer % V + s.Rest =
    a.translate[Writer % W, Writer % V]([X] => (op: Writer[W, X]) => op match
      case Say(w) => tell(f(w)))

  /** the values told one by one, from `i` */
  private def tellAll[W](ws: IndexedSeq[W], i: Int): Unit ! Writer % W =
    if i >= ws.length then pure[Unit, Writer % W](()) else tell(ws(i)) >> tellAll(ws, i + 1)

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
   * This translates each tell into the values `f` makes of it, told
   * directly at the place, so no element crosses a coroutine boundary.
   *
   * `f` may return any number of values, including none — an empty
   * result drops the told value, which makes this a filter as well as
   * an expansion.
   */
  def expand[W, V, A, Es[_]](a: A ! Es)(using s: Split[Es, Writer % W])(f: W => IndexedSeq[V]): A ! Writer % V + s.Rest =
    a.translate[Writer % W, Writer % V]([X] => (op: Writer[W, X]) => op match
      case Say(w) => tellAll(f(w), 0))

  /** what `listen` heard: one told value, recorded beside its re-telling */
  private enum Heard[W, X]:
    case Hear(w: W) extends Heard[W, Unit]

  /** the values heard, newest first: a register of the machine (the kernel's `Stateful`) */
  private def hearing[W]: Handler[Heard % W, [A] =>> (List[W], A)] = new Handler[Heard % W, [A] =>> (List[W], A)]:
    def kernel[T <: Row, A]: OneHandler[Heard % W, T, A, (List[W], A)] = new Stateful[Heard % W, T, List[W], A, (List[W], A)]:
      def initial: List[W] = Nil
      def ret(st: List[W], a: A): Eff[T, (List[W], A)] = Cont.pure((st, a))
      def value[X](st: List[W], op: Heard[W, X]): X = op match { case Heard.Hear(_) => () }
      def next[X](st: List[W], op: Heard[W, X]): List[W] = op match { case Heard.Hear(w) => w :: st }

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
   * the scope's, in the answer: each tell TRANSLATED to a record and
   * the same tell outside, the records kept by a `Stateful` around.
   */
  def listen[W, A, Es[_]](a: A ! Es)(using s: Split[Es, Writer % W]): (A, Seq[W]) ! Writer % W + s.Rest =
    a.translate[Writer % W, Heard % W + Writer % W]([X] => (op: Writer[W, X]) => op match
        case Say(w) => effect[Heard % W, Unit](Heard.Hear(w)) >> tell(w))
      .handle(hearing[W])
      .map((heard, x) => (x, heard.reverse))

  /**
   * A PART OF THE PROGRAM'S WHOLE OUTPUT, REWRITTEN — mtl's `censor`
   * (specs/core-gaps.md stage 2): `f` sees everything the scope told
   * and answers what to tell in its place.
   *
   *     Writer.censor(step)(ws => if ws.size > 1 then Seq(s"\${ws.size} steps") else ws)
   *
   * Seeing it all means WAITING for all of it: the scope's tells are
   * held back and told at the scope's END, as `f` of them — so a raise
   * inside the scope drops them, and they come after every other
   * effect the scope performed. That is the definition, not a defect.
   * A rewrite of each told value on its own keeps every tell in its
   * place, and it already exists: `map` (one to one) and `expand` (one
   * to many, and a filter) over the same scope.
   */
  def censor[W, A, Es[_]](a: A ! Es)(f: Seq[W] => Seq[W])(using s: Split[Es, Writer % W]): A ! Writer % W + s.Rest =
    collect[W, A, Es](a).flatMap((ws, x) => tellAll(f(ws).toIndexedSeq, 0).map(_ => x).at[Writer % W + s.Rest])

  /**
   * Re-tell at a WIDER element type with NO transform — `map`'s
   * identity case, and free: `Writer[+W, +A]` is covariant, so the
   * told OPERATION (`Say(w): Writer[W, Unit]`) already IS a
   * `Writer[V, Unit]` for any `V >: W`, and the program is the same
   * program — `f` never runs, nothing is walked or rebuilt.
   */
  def widen[W, V >: W, A, G[_]](a: A ! Writer % W + G): A ! Writer % V + G =
    // THE CAST, and why it is sound: a program is a function of the PATHS of its effects, and a path names a place,
    // its effect's type phantom in it. At the place of `Writer % V` this program performs `Say(w)`, which is a
    // `Writer[V, Unit]` by the enum's covariance — what that place's handler takes. `Run` is invariant in its
    // effects (they are a function's argument), so the compiler cannot see it
    a.asInstanceOf[A ! Writer % V + G]

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
   * Lazy: nothing is pulled until the result is run, one element per
   * pull, and the F-operations stay in the program rather than being
   * run behind the caller's back. The first pull is deferred
   * (`defer`); each next one sits inside the previous tell's bind,
   * which is deferral enough.
   */
  /** a COLLECTION's values told in order: a program needing `Writer % A` alone — a collection has no effect of its
   * own to carry, so no `+ Pure` for a caller to drop. A linear sequence is walked by its tail (a `LazyList` stays
   * lazy: forced as it is told, an infinite one too), an indexed one by its index, any other copied once */
  def of[A](xs: Iterable[A]): Unit ! Writer % A = xs match
    case ls: collection.LinearSeq[A] @unchecked => okay.defer(tellLinear(ls))
    case is: IndexedSeq[A] @unchecked => okay.defer(tellAll(is, 0))
    case other => okay.defer(tellAll(other.toIndexedSeq, 0))

  /** the values of a linear sequence told one by one, from its head */
  private def tellLinear[W](ws: collection.LinearSeq[W]): Unit ! Writer % W =
    if ws.isEmpty then pure[Unit, Writer % W](()) else tell(ws.head) >> tellLinear(ws.tail)

  def of[S[_], F[_], A](s: S[A])(using St: Stream[S, F]): Unit ! Writer % A + F =
    okay.defer(ofLoop[S, F, A](s))

  private def ofLoop[S[_], F[_], A](s: S[A])(using St: Stream[S, F]): Unit ! Writer % A + F =
    St.uncons(s).flatMap[Unit, Writer % A + F]:
      case Some((a, rest)) => tell(a) >> ofLoop[S, F, A](rest)
      case None => pure[Unit, Writer % A + F](())

  /**
   * The observation of the writer as codata: the same shape as
   * Stream.uncons but with a richer functor — Either[A, (W, rest)]
   * instead of Option[(W, rest)]. The told values come out one by one
   * through Right, on demand; when they end, Left carries the
   * program's own answer (nothing is lost — by then every told value
   * has already been observed). run/fold are this loop with a Fold
   * accumulating the Rights.
   *
   * The program is STEPPED (the kernel's `Machine.Head`) to its next
   * tell; the rest is the paused program, data — resumed when the rest is next
   * observed, not when this value is handed over, so a throw after the
   * tell comes at the next pull (TestWriterToldBeforeThrow) and nothing
   * past a value is computed before it is asked for.
   */
  def uncons[W, A](a: A ! Writer % W): Either[A, (W, A ! Writer % W)] =
    Stepping.nextPure(Stepping.start(a), Telling.key, Telling[W]())

  /**
   * The same observation for a writer program performing ARBITRARY
   * effects G alongside its telling: the next told value arrives
   * inside G — the G-operations met on the way performed where the
   * observation runs, the paused program resumed with their answers. Any
   * structured effect handler (State, Reader, Throws, ...) forwards the
   * telling, so it can be run over the program FIRST — handlers are
   * stream transformers — and the Answers-able residue (Async, say) is
   * what the consumer pays at each pull.
   *
   * The rest handed out is the paused program, tied to the place the
   * observation ran at: observed again there, or anywhere its effects
   * are at the same paths, it goes on from where it was; run elsewhere,
   * its operations are re-placed by the paths read side by side, and
   * one whose effect is not where it was is refused loudly
   * (`IllegalStateException`) — as `Logic.msplit`'s rest.
   */
  def uncons[W, A, G[_]](a: A ! Writer % W + G): Either[A, (W, A ! Writer % W + G)] ! G =
    Run.of(Cont.defer(Stepping.next(Stepping.start(a), Telling.key, Telling[W]())))

  /**
   * `TypeableK` by the CLASS of `Say` alone — evidence for code that
   * still splits by class (okay-freer's walks); the root's programs
   * find an effect by its path and need none.
   *
   * IN Writer'S OWN COMPANION: a `given` here is in the IMPLICIT SCOPE
   * of every `TypeableK[Writer % W]` query, from any package, with no
   * import.
   */
  given writerK[W]: TypeableK[Writer % W] = typeableK[Writer % W](classOf[Writer.Say[?, ?]])

  /**
   * The finer test, opt-in: the told value's own class as well, which
   * is what separates `Writer % String + Writer % Int` in one row on
   * okay-freer's road (by path on the root's, no test at all). It needs
   * a `Typeable[W]`, and for a parameterised W that is an erased test
   * and an E092 warning at the call site.
   */
  object byValue:
    given writerK[W](using t: scala.reflect.Typeable[W]): TypeableK.ByValue[Writer % W] = new:
      // `Typeable.unapply` answers an Option for the told value's own
      // test (the JDK's Typeable has no boolean form)
      def test(x: Any): Boolean = x match
        case s: Writer.Say[?, ?] => t.unapply(s.w).isDefined
        case _ => false
}

/** what a stepped `Writer % W` operation hands over — its told value — and what it answers: nothing */
private[std] object Telling:
  /** the key of every `Writer % W`: its constructor's */
  val key: AnyRef = summon[okay.KeyOf[Writer % Any]].ref
private[std] final class Telling[W] extends Stepping.Told[Writer % W, W]:
  def element[X](op: Writer[W, X]): W = op match { case Writer.Say(w) => w }
  def answer[X](op: Writer[W, X]): X = op match { case Writer.Say(_) => () }

/**
 * A PROGRAM AT ITS HEAD (the kernel's `Machine.Head`): the views that peel the next element off a program — a
 * writer's told value, a producer's production — and the folds that stop. `Machine.head(p)` runs the program to its
 * first operation and hands it over PAUSED, one object, the rest the machine's own frames: an operation of the
 * effect viewed (`E`, at the head of the row) is an element; one of the others is performed where the view runs,
 * or answered here, and the paused program resumed with its answer. What okay-freer's `resume`-based walks were,
 * with the machine's frames: no rotation, constant stack. No view here cancels a program: one not resumed is
 * dropped (a guard's finalizer inside it does not run).
 */
private[std] object Stepping:
  /** the walking machinery is the root's (`okay.Walk`): one for every walk of the platform */
  type Paused[A] = okay.Walk.Paused[A]
  export okay.Walk.{own, placed, start}
  def Rest[Es[_], A](next: () => Head[OnePure, A]): A ! Es = okay.Walk.rest[Es, A](next)
  def resume[A](at: Paused[A], v: Any): Head[OnePure, A] = okay.Walk.resumeAt(at, v)

  /** what an operation of the viewed effect hands over, and what it answers */
  abstract class Told[E[_], W]:
    def element[X](op: E[X]): W
    def answer[X](op: E[X]): X

  /** an operation of an effect outside the viewed one, answered here */
  abstract class Other:
    def apply[F[_], X](op: F[X], in: In[F, OnePure]): X

  object Other:
    /** no effect outside: none there */
    def none: Other = new Other:
      def apply[F[_], X](op: F[X], in: In[F, OnePure]): X =
        throw IllegalStateException(s"an operation of no effect of the program: $op")
    /** `G` outside, each operation answered by `H` — a single operation's interpretation, no program built */
    def answers[G[_]](H: Answers[G])(using k: okay.KeyOf[G]): Other = new Other:
      def apply[F[_], X](op: F[X], in: In[F, OnePure]): X =
        if own(in, k.ref) then H.handle(op.asInstanceOf[G[X]]) else none(op, in)

  /** the next element and the rest, of a program with no other effect */
  def nextPure[Es[_], E[_], A, W](st: Head[OnePure, A], key: AnyRef, told: Told[E, W]): Either[A, (W, A ! Es)] =
    st match
      case Head.Done(a) => Left(a)
      case p: Head.Paused[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        if !own(p.in, key) then throw IllegalStateException(s"an operation of no effect of the program: ${p.op}")
        else
          val op = p.op.asInstanceOf[E[Any]]
          Right((told.element(op), Rest[Es, A](() => resume(p, told.answer(op)))))

  /** the next element and the rest, the other effects' operations met on the way performed where the view runs */
  def next[Es[_], E[_], A, W](st: Head[OnePure, A], key: AnyRef, told: Told[E, W])
  : okay.one.![Either[A, (W, A ! Es)], OnePure] = st match
    case Head.Done(a) => Cont.pure(Left(a))
    case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] =>
      if !own(p.in, key) then Cont.perform[f, OnePure, x](p.op)(using placed(p.in)).flatMap(v => next(p.resume(v), key, told))
      else
        val op = p.op.asInstanceOf[E[Any]]
        Cont.pure(Right((told.element(op), Rest[Es, A](() => resume(p, told.answer(op))))))

  /** a fold that stops: each element added, the paused program dropped once the state is done; the other
   * effects' operations performed where the fold runs */
  def until[E[_], A, W, S, R](st0: Head[OnePure, A], key: AnyRef, s0: S, told: Told[E, W], K: FoldUntil[W, S, R])
  : okay.one.![R, OnePure] =
    def again(st: Head[OnePure, A], s: S): okay.one.![R, OnePure] = loop(st, s)
    @tailrec def loop(st: Head[OnePure, A], s: S): okay.one.![R, OnePure] = st match
      case Head.Done(_) => Cont.pure(K.end(s))
      case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] =>
        if !own(p.in, key) then Cont.perform[f, OnePure, x](p.op)(using placed(p.in)).flatMap(v => again(p.resume(v), s))
        else
          val op = p.op.asInstanceOf[E[Any]]
          val s2 = K.add(s, told.element(op))
          if K.done(s2) then Cont.pure(K.end(s2)) else loop(resume(p, told.answer(op)), s2)
    loop(st0, s0)

  /**
   * THE LINEAR VIEW: the program stepped element by element — no `Option`, no `Either`, no program built per
   * element, one object (the paused program) a step; an operation of the row outside answered by `other` in place.
   * The paused program after an element is resumed when the next one is asked for. A throw — the program's, or
   * `other`'s — ends the iterator.
   */
  final class Iter[E[_], W, A](first: () => Head[OnePure, A], key: AnyRef, told: Told[E, W], other: Other)
      extends Iterator[W]:
    // the element last handed over, paused, and its answer: resumed for the next; `null` before the first
    private var at: Paused[A] | Null = null
    private var answer: Any = null
    private var ready = false
    private var ended = false
    private var elem: W = scala.compiletime.uninitialized

    @tailrec private def advance(st: Head[OnePure, A]): Unit = st match
      case Head.Done(_) => ended = true; at = null
      case p: Head.Paused[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        if !own(p.in, key) then advance(resume(p, other(p.op, p.in)))
        else
          val op = p.op.asInstanceOf[E[Any]]
          elem = told.element(op)
          ready = true
          at = p
          answer = told.answer(op)

    def hasNext: Boolean =
      if !ready && !ended then
        val p = at
        try advance(if p == null then first() else resume(p, answer))
        catch case e: Throwable => { ended = true; at = null; throw e }
      ready

    def next(): W =
      if !hasNext then throw java.util.NoSuchElementException("empty stream")
      ready = false
      elem

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
  final override inline def put[W](w: W): Unit />> Feed[W] =
    Cps.shift(k => Writer.tell(w).flatMap(_ => k(())))
  /** each element told, the next step deferred into the bind */
  override def unfold[A, B](a: A)(f: A => B)(g: A => A): Feed[B] =
    Writer.tell(f(a)).flatMap(_ => okay.defer(unfold(g(a))(f)(g)))

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
   * The specialized linear view of the PURE writer stream: the
   * program stepped tell by tell (`Stepping.Iter`) — no `Option`, no
   * `Either`, no program built per step, which the DEFAULT
   * `Iterator.unfold(s)(uncons(_).runWith)` pays once per element —
   * which on a chunked stream is once per CHUNK, and was the whole of
   * the ~2x stage 0 measured on `Chunks.fold`
   * (producer-to-writer-carrier, `## Results`).
   */
  override def iterator[W](s: A ! Writer % W)(using Answers[Pure]): Iterator[W] =
    Stepping.Iter[Writer % W, W, A](() => Stepping.start(s), Telling.key, Telling[W](), Stepping.Other.none)

/**
 * And a writer program performing ARBITRARY effects G is a stream in
 * G: the same observation, with the G-operations met on the way
 * performed where the observation runs. This is the instance the
 * concurrent combinators ask for — `Channel.merge` needs a `Stream`,
 * and an asynchronous source (`Unit ! Writer % W + Async`) had none.
 * `G` is ONE effect here (a place of the row): the `Answers[G]` the
 * consumers run it by answers one.
 */
given writerStreamIn[A, G[_]](using okay.KeyOf[G]): Stream[[W] =>> A ! Writer % W + G, G] = new:
  def uncons[W](s: A ! Writer % W + G): Option[(W, A ! Writer % W + G)] ! G =
    Writer.uncons[W, A, G](s).map(_.toOption)

  /**
   * The specialized linear view: the program stepped at `Writer % W`
   * before `G`, a G-operation answered directly by `Answers[G].handle`
   * — a COMONADIC, single-operation interpretation
   * (`Answers[Async]`'s own `case Run(f) => f()`), not a program built
   * and run; no `Option`, no `Either` a step (see Chunks.scala's
   * `foldWriter` doc and [[escape-analysis-box-elimination-boundary]]).
   */
  override def iterator[W](s: A ! Writer % W + G)(using H: Answers[G]): Iterator[W] =
    Stepping.Iter[Writer % W, W, A](() => Stepping.start(s), Telling.key, Telling[W](),
      Stepping.Other.answers(H))
