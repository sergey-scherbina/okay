package okay

import okay.freer.{Free, Freer}

import okay.Row.{at, plus}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

/**
 * Extensible effects, founded on the continuation paramonad. A computation `A ! F` is a freer-monad tree over
 * the signature `F`; its meaning is its image in `Cont` (`foldCont`), where a handler `F !> S` is an
 * interpretation into continuations. `Effects` is the final-tagless interface; `Free` (the initial encoding)
 * and `Eager` (Eager.scala) are its instances, and `object Effects` (also `!`) is the toolkit over `Free`.
 * `reflect` and `reify` move programs between encodings.
 *
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * https://blog.higher-order.com/assets/trampolines.pdf
 */

/** a computation of A performing the operations of F: A ! F */
infix type ![A, F[+_]] = Free[F, A]

/** the toolkit's short name, as in `!.run`; `Effects` is the same object */
val ! = Effects

/** a value as a computation */
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)

/** an operation as a computation */
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

/**
 * An operation performed, postfix: `Users.Find(7).perform : Option[String] ! Users`. The answer type comes
 * from the case (`Find` extends `Users[Option[String]]`), so nothing is written twice. Named constructors
 * are still the better API for an effect others will use. It applies to any `F[A]`: `List(1, 2).perform` is
 * nondeterminism, which `runSeq` (Choice.scala) handles.
 */
extension [F[+_], A](op: F[A])
  inline def perform: A ! F = effect(op)

/** the union of two signatures: F + G */
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/** the empty signature: a computation over it is pure, with nothing to perform; the zero of `+` */
type Pure[+A] = Nothing

/** fix the parameter of a binary signature: State % S, Throws % E */
infix type %[F[_, _], S] = F[S, *]

/**
 * A partial function, infix: `Request |=> Response ! Async`. The spelling is forced by `!`: an infix type's
 * precedence comes from its first character, and anything binding tighter than `!` (`~>`, `-?>`, `=?>`)
 * parses `A ~> B ! F` as `(A ~> B) ! F`. A union on the left binds first, so `Get | Post |=> Res` reads as
 * it looks.
 */
infix type |=>[A, B] = PartialFunction[A, B]

/** Final tagless interface of extensible effects: `M[F, A]` computes `A` performing the signature `F`. Its
 * meaning is its image in the continuation paramonad (`foldCont`), on which `run` and `handle` are founded */
trait Effects[M[_[+_], _]]:
  def pure[F[+_], A](a: A): M[F, A]
  def perform[F[+_], A](e: F[A]): M[F, A]
  /** a bind whose left side is deferred, forced only when the encoding's interpreter reaches it, so that
   * mutually recursive functions returning `M[F, A]` call each other in tail position with no JVM frame each */
  def defer[F[+_], A, B](thunk: () => M[F, A])(f: A => M[F, B]): M[F, B]
  /** a tail call to a mutually recursive function, for code written over any `M: Effects` (`!.tailcall` on `Free`) */
  def tailcall[F[+_], A](thunk: => M[F, A]): M[F, A] = defer(() => thunk)(pure)

  extension [F[+_], A](m: M[F, A])
    def flatMap[B](f: A => M[F, B]): M[F, B]
    inline def map[B](f: A => B): M[F, B] = m.flatMap(a => pure(f(a)))
    /** `foldMap` into `Cont`: the program's fold, each operation answered by
     * `h` as a continuation (`Static.foldMap` is the same fold into any
     * `Selective`). The result is still waiting for its LAST continuation:
     * `/ identity` when `S` is the answer (`runWith`), `/ ret` to finish
     * into `S` (`handle`). TestFoldCont and docs/contract.md show three `S`. */
    def foldCont[S](h: F !> S): A /> S
    /** run all the effects by a comonadic Answers (the foldCont definition; encodings may override with an equivalent fast path) */
    def runWith(using Answers[F]): A = m.foldCont(handler[F, A]) / identity
    /**
     * The program folded into any `Monad` `G`, each operation translated by `nt`: `G`'s own `tailRecM`, as
     * cats' `Free.foldMap` is, so the fold is exactly as stack-safe as the carrier's loop
     * (specs/eager-carrier-depth.md). Each step resumes the tree once and answers the rest or the value.
     */
    def foldMap[G[_]](nt: F ==> G)(using G: Monad[G], R: TailRecM[G]): G[A] =
      R.tailRecM[A ! F, A](reify[M, F, A](m)(using Effects.this)) { p =>
        (p.resume: @unchecked) match
          case Free.Return(a) => G.pure(Right(a))
          case Free.Inject(e) => G.fmap(nt(e), a => Right(a))
          case Free.Bind(Free.Inject(e), k) => G.fmap(nt(e), x => Left(k(x)))
      }

  /** handle the effect F by h (and the values by ret), forwarding the
   * effects G; for mass tail-resumption prefer !.relay (measured) */
  def handle[F[+_], G[+_]](using TypeableK[F])[A, B](m: M[F + G, A])
                          (ret: A => M[G, B])
                          (h: F !> M[G, B]): M[G, B] =
    m.foldCont[M[G, B]]([X] => e => split[F, G](e)(e => h(e))(e => Cont.shift(k => perform(e).flatMap(k)))) / ret

  // LEVEL 1 (specs/shift-effect.md): continuations and ready handlers in any encoding, through the tree by
  // default; `Effects[Free]` overrides them with the top-level functions themselves

  /** Danvy-Filinski's capture (the top-level `shift`) */
  def shift[R, A, F[+_]](f: (A => M[Shift % R + F, R]) => M[Shift % R + F, R])(using k: Shift.Key[R], at: At): M[Shift % R + F, A] =
    reflect[M, Shift % R + F, A](okay.shift[R, A, F](kk =>
      reify[M, Shift % R + F, R](f(a => reflect[M, Shift % R + F, R](kk(a))(using this)))(using this)))(using this)

  /** the capture whose body runs outside its `reset` (the top-level `shift0`) */
  def shift0[R, A, F[+_]](f: (A => M[F, R]) => M[F, R])(using k: Shift.Key[R], at: At): M[Shift % R + F, A] =
    reflect[M, Shift % R + F, A](okay.shift0[R, A, F](kk =>
      reify[M, F, R](f(a => reflect[M, F, R](kk(a))(using this)))(using this)))(using this)

  /** delimit (the top-level `reset`) */
  def reset[R, F[+_]](body: M[Shift % R + F, R])(using Shift.Key[R], Distinct[Shift % R + F], Shift.Machine[F]): M[F, R] =
    reflect[M, F, R](okay.reset[R, F](reify[M, Shift % R + F, R](body)(using this)))(using this)

  /** take a ready handler's effect off the row (the program's `p.handle(h)`; two arguments in one list, so the
   * level-2 `handle(m)(ret)(clause)` above stays its own overload) */
  def handle[A, G[+_], E[+_], I, O[_], N[_[+_]], F[+_]](m: M[G, A], h: Handler.Full[E, I, O, N])
            (using row: (A ! G) =:= (A ! E + F), ok: A <:< I, d: Distinct[E + F], n: N[F]): M[F, O[A]] =
    reflect[M, F, O[A]](h.run[A, F](row(reify[M, G, A](m)(using this))))(using this)

  /** a program with no effect left, to its value (the program's `run`) */
  def run[A](m: M[Pure, A]): A = reify[M, Pure, A](m)(using this).run

/** the freer monad, the initial encoding: `Inject` is a suspended shift, given its meaning by `foldCont`.
 * Choose it when the program is a thing — to step, inspect or relay it — stack-safe on any bind shape */
given Effects[Free] with
  override inline def pure[F[+_], A](a: A): Free[F, A] = Free.Return(a)
  override inline def perform[F[+_], A](e: F[A]): Free[F, A] = Free.Inject(e)
  override inline def defer[F[+_], A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    Free.defer(thunk)(f)
  /** the tree has a node for exactly this */
  override def tailcall[F[+_], A](thunk: => Free[F, A]): Free[F, A] = Free.delay(() => thunk)

  extension [F[+_], A](m: Free[F, A])
    override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = m.flatMap(f)
    override def foldCont[S](h: F !> S): A /> S =
      Free.fold(m)(Cont.Pure(_))([X] => e => k => h(e).flatMap(k(_).foldCont(h)))
    /** the same answer as the foldCont definition, in one pass instead of two */
    override def runWith(using Answers[F]): A = runFree(m)

  // level 1: the top-level functions themselves
  override def shift[R, A, F[+_]](f: (A => R ! Shift % R + F) => R ! Shift % R + F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
    okay.shift[R, A, F](f)
  override def shift0[R, A, F[+_]](f: (A => R ! F) => R ! F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
    okay.shift0[R, A, F](f)
  override def reset[R, F[+_]](body: R ! Shift % R + F)(using Shift.Key[R], Distinct[Shift % R + F], Shift.Machine[F]): R ! F =
    okay.reset[R, F](body)
  override def run[A](m: A ! Pure): A = m.runWith

  @tailrec private def runFree[F[+_], A](m: Free[F, A])(using H: Answers[F]): A =
    (m.resume: @unchecked) match
      case Free.Return(a) => a
      case Free.Inject(e) => H.handle(e)
      case Free.Bind(Free.Inject(e), f) => runFree(f(H.handle(e)))

  /**
   * `Effects.handle`'s definition in one loop over the tree. The definition answers EVERY operation in `Cont`,
   * so a forwarded one costs a capture spent on copying (+112.7 B and 1.51x against `relay`, docs/benchmarks.md
   * §2, `hd-*`). Here a forwarded operation is re-emitted on the `G` side as `relay` does, and `Cont` is
   * entered only for an operation the handler claims. The same function: a forwarded operation is already
   * committed to the `G` program, which a later abort cannot un-perform under the definition either
   * (TestHandleForward pins aborting and multi-shot forwarding, red first against a wrong arm).
   *
   * A handler that answers with `Cont.Pure` continues the loop with one tail call; only a real capture
   * reifies the rest, under a `Delay` so a deep program trampolines. A `Delay` on EVERY handled operation
   * cost 59 µs of a 61 µs gap (`hff-*`): its `Pure` continuation rotates into a left-nested `Bind`.
   */
  override def handle[F[+_], G[+_]](using TypeableK[F])[A, B](m: Free[F + G, A])
                                   (ret: A => Free[G, B])
                                   (h: F !> Free[G, B]): Free[G, B] =
    // NOT @tailrec: the deferring arms mention `loop` inside a closure, which the annotation reads as a non-tail
    // call; the answered arm is a real tail call, and TestHandleForward's stack-safety tests guard the depth.
    // The terminal and capturing arms live in their own methods, as `relay`'s `last`, to keep this loop
    // small: `Free.resume` (323 bytes, under FreqInlineSize 325) is pasted into it, and a loop that is itself
    // too big to inline lost 15% on handlePrebuilt and handleCapture (`de-*`).
    def last(e: F[A] | G[A]): Free[G, B] =
      split[F, G](e)(e => h(e) / ret)(e => Free.Inject(e).flatMap(ret))

    def capture[X](d: Int)(c: Cont[X, Free[G, B], Free[G, B]], k: X => Free[F + G, A]): Free[G, B] =
      c / (x => Free.delay(() => loop(d)(k(x))))

    // a call from inside flatMap cannot be a jump; `again` takes it, so the walk stays a checked loop
    def again(d: Int)(x: Free[F + G, A]): Free[G, B] = loop(d)(x)

    // the forwarding arm in its own method too, to keep the loop under FreqInlineSize
    def forward[X](d: Int)(i: Free[F + G, X], k: X => Free[F + G, A]): Free[G, B] =
      forwarded[F, G](i).flatMap(x => again(d)(k(x)))
    @tailrec def loop(d: Int)(x: Free[F + G, A]): Free[G, B] = (x.resumeRun: @unchecked) match
      case Free.Return(a) => ret(a)
      case Free.Inject(e) => last(e)
      case Free.Bind(i @ Free.Inject(e), k) =>
        split[F, G](e)
          // `h` is asked ONCE: the answered test and the fallback both
          // read the same program, and a handler is not assumed pure
            (e => {
              val c = h(e)
              Cont.onAnswer(c)(a => loop(d)(k(a)))(capture(d)(c, k))
            })
          (_ => forward(d)(i, k))
      // a run nested here: forced — its fold below HandleFrames.Limit, its frame on a machine at it
      case y => loop(d)(HandleFrames.shallow(y, d))

    // a value: run by whoever forces it, a frame for a machine that meets it
    Free.delay(new HandleFrames.Run[B, G]:
      def at(d: Int): Free[G, B] = loop(d)(m)
      def program: Shift.U[G, B] = HandleFrames.control[F, A, B, G](ret, h, summon[TypeableK[F]])(m))

/**
 * Any Effects program in any other encoding: the interface's initiality as a function. An encoding is fixed
 * by `pure` and `perform` and `foldCont` is the fold, so there is one structure-preserving way across.
 * `reify` and `reflect` are this at the two ends.
 */
inline def convert[M[_[+_], _] : Effects,
  N[_[+_], _] : Effects as N, F[+_], A](m: M[F, A]): N[F, A] =
  m.foldCont[N[F, A]]([X] => e => Cont.shift(k =>
    N.perform(e).flatMap(k))) / (a => N.pure(a))

/** `M.tailRecM(a)(f)`: `TailRecM[F]`'s loop, the carrier's own (specs/eager-carrier-depth.md) */
extension [F[_]](M: Monad[F])
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]])(using R: TailRecM[F]): F[B] = R.tailRecM(a)(f)

/** any Effects program as a `Free` tree: building the syntax is itself an interpretation */
inline def reify[M[_[+_], _] : Effects, F[+_], A](m: M[F, A]): A ! F =
  convert[M, Free, F, A](m)

/**
 * A `Free` tree read INTO any encoding — `Eager`, or one of your own — where `reify` observes an encoding as
 * syntax (for a debugger, a rewriter, `Pipeline`'s optimizer). Together they round-trip (TestReflect). Inside
 * package `okay` the name shadows `scala.reflect`: write `scala.reflect.X` there.
 */
def reflect[M[_[+_], _] : Effects as M, F[+_], A](m: A ! F): M[F, A] =
  // straight into the target: a tree is already syntax, so no continuation is reified on the way
  Free.fold(m)(M.pure)([X] => e => k => M.perform(e).flatMap(x => reflect[M, F, A](k(x))))

object Effects {
  export Free.*

  /** level 1, any encoding in direct style: `M[F, *]` as a monad, for `direct[[A] =>> M[F, A]]` over `Effects[M]` */
  def monad[M[_[+_], _], F[+_]](using E: Effects[M]): Monad[[A] =>> M[F, A]] = new Monad[[A] =>> M[F, A]]:
    def pure[A](a: A): M[F, A] = E.pure(a)
    extension [A](a: M[F, A])
      def flatMap[B](f: A => M[F, B]): M[F, B] = E.flatMap(a)(f)

  /** the staging entry for effect programs: `Effects[Free]`, `Effects[Eager]`, or any `M` with an instance
   * in scope; with `trait Effects` it forms one door, as a class and its companion do */
  transparent inline def apply[M[_[+_], _]]: Effects[M] =
    compiletime.summonInline[Effects[M]]

  extension [F[+_], A](self: A ! F) {

    /** step through the next n operations by the Answers */
    @tailrec def next(steps: Long = 1)(using H: Answers[F]): A ! F = (self.resume: @unchecked) match
      case Bind(Inject(e), k) if steps > 0 => k(H.handle(e)).next(steps - 1)
      case a => a

    /** peek the nearest answer: the value, or the first operation handled by the `Answers` */
    @tailrec def peek: Answers[F] ?=> ? = self match
      case Bind(a, _) => a.peek
      case Inject(e) => summon[Answers[F]].handle(e)
      case Return(a) => a
      // forced, as `Bind(a, _)` above drops its continuation
      case Delay(t) => t().peek
  }

  /** run a closed computation */
  inline def run[A](e: A ! Pure): A = e.runWith

  /** a tail call to a mutually recursive function returning `A ! F`: the interpreter trampolines it. A `Delay`,
   * not `defer` with `pure`, which would push a `.flatMap(pure)` down every hop (`Cont.delay` on the Cont side) */
  inline def tailcall[F[+_], A](thunk: => A ! F): A ! F =
    Free.delay(() => thunk)

  // `loop`, `tailRecM` for programs (specs/fold-until.md), is `Free.loop`: exported above, as the four names are

  /**
   * A fold that performs, BUILT RIGHT-NESTED: `f` runs on each element in order, with the answer of the one
   * before as its accumulator.
   *
   *     !.foldM(orders)(0L)((total, o) => price(o).map(total + _))
   *
   * Not `xs.foldLeft(pure(z))((m, x) => m.flatMap(...))`: that builds a left-nested chain `resume` rotates one
   * bind at a time (32 µs against 13 for 1000 State/Writer operations, specs/handler-fusion.md). Here each
   * step's continuation builds the next, so nothing is rotated. Stack-safe as `loop`; a value that runs again
   * from the start.
   */
  def foldM[X, B, F[+_]](xs: Iterable[X])(z: B)(f: (B, X) => B ! F): B ! F =
    // indexed and FLAT: `v(i)` runs once a step, and a Vector's radix walk there was 3.3 µs of 7.3 over the
    // hand-written loop; an `ArraySeq` passes through, anything else is copied once
    val v = ArraySeq.untagged.from(xs)
    val n = v.length
    // a step written `op.map(g)` arrives as `Bind(op, Mapped(g))` and becomes `Bind(op, y => go(i + 1, g(y)))`,
    // one bind a step instead of two. Safe here only because `go` builds the rest and calls no continuation
    // (`flatMap` cannot do this in general, specs/map-fusion.md). The one type claim is the one `Mapped`
    // licenses: the continuation of a `Bind[F, x, B]` that is a `Mapped` IS a `Mapped[F, x, B]`.
    def go(i: Int, acc: B): B ! F =
      if i >= n then Return(acc)
      else f(acc, v(i)) match
        case b: Freer.Bind[Unary[F], Unit, Unit, Unit, x, B] @unchecked => b.f match
          case k: Freer.Mapped[Unary[F], Unit, x, B] @unchecked => Bind(b.a, (y: x) => go(i + 1, k.f(y)))
          case _ => Bind(b, (y: B) => go(i + 1, y))
        case m => Bind(m, (y: B) => go(i + 1, y))
    // DELAYED, so `f` runs when the program does, never at build time
    Free.delay(() => go(0, z))

  /**
   * A fold whose step is ONE bind: `f` is each element's program, and `combine` folds its answer into the
   * accumulator purely.
   *
   *     !.foldEach(orders)(0L)(o => price(o))(_ + _)
   *
   * `foldM` with `(acc, x) => f(x).map(combine(acc, _))` folds the map away, but only after it built its Bind
   * and `Mapped`; here nothing is built but the bind. Delayed, indexed and stack-safe as `foldM`.
   */
  def foldEach[X, A, B, F[+_]](xs: Iterable[X])(z: B)(f: X => A ! F)(combine: (B, A) => B): B ! F =
    val v = ArraySeq.untagged.from(xs) // flat, as foldM's: the read is per step
    val n = v.length
    def go(i: Int, acc: B): B ! F =
      if i >= n then Return(acc)
      else f(v(i)).flatMap(a => go(i + 1, combine(acc, a)))
    Free.delay(() => go(0, z))

  /** `f` on each element, in order, for its effects: `foldM` with no
   * accumulator, and the same right-nested build */
  def each[X, F[+_]](xs: Iterable[X])(f: X => Unit ! F): Unit ! F =
    foldM[X, Unit, F](xs)(())((_, x) => f(x))

  /** run p at most once under `Once.run`: the by-need word, an effect —
   * `Once.once`, here because `!.tailcall` (by-name) is its sibling */
  inline def once[A, F[+_]](p: => A ! Once + F): A ! Once + F = Once.once(p)

  /**
   * The same program in a wider row: effect subsumption, as a COERCION. `Free` is invariant in its row by a
   * measured choice, so the type system cannot see that a program at `F` is one at `F + G`; `Row.into` says it
   * once (an `Inject(e)` with `e: F[X]` IS a `(F + G)[X]`, the row being a union). Nothing is forced or walked;
   * the walk is `normalize`.
   */
  def widen[A, F[+_], G[+_]](p: A ! F): A ! F + G = Row.into[A, F, F + G](p)

  /**
   * The WALK: resume the head and rebuild the tree into the wider row, one re-injected node per operation,
   * deferred as it goes — a normalisation, not an upcast. It pays when a runner would otherwise rotate per
   * pull (`Source.merge` keeps `Writer.widen`'s walk: 5.3% without it). A deferred head stays deferred, so
   * the walk begins only when the program runs.
   */
  def normalize[A, F[+_], G[+_]](p: A ! F): A ! F + G = p match
    case Free.Delay(t) => Free.Delay(() => normalize[A, F, G](t()))
    case Bind(Free.Delay(t), f) => Free.defer(() => normalize(t()))(x => normalize[A, F, G](f(x)))
    case _ => (p.resume: @unchecked) match
      case Return(a) => Return(a)
      case Inject(e) => Inject(e)
      case Bind(Inject(e), k) => Inject(e).flatMap(x => normalize[A, F, G](k(x)))

  /**
   * `translate` with the widening done for you, for a target row BIGGER than the source's: `A ! Users + F`
   * into `A ! State % Store + Writer % String + F`, the rest `F` carried through, every row solved by the
   * expected type.
   *
   *     def tracked[A, F[+_]](p: A ! Users + F): A ! Tracked + F =
   *       !.interpret(p):
   *         [X] => (e: Users[X]) => e match
   *           case Users.Find(id) => ...   // a PROGRAM in Tracked + F
   */
  def interpret[A, F[+_] : TypeableK, G[+_], H[+_]](prog: A ! F + H)(using Distinct[F + G + H])
                                                   (h: F ==> ([X] =>> X ! G + H))
  : A ! G + H =
    translate[A, F, G + H](prog.plus[G])(h)

  /**
   * RECORD what a program asks for without answering any of it: each operation of `F` is told to a `Writer`
   * and then performed as before, so the row keeps `F` and gains `Writer % W`.
   *
   *     !.tracing(prog)([X] => (e: Users[X]) => e.toString)   // A ! Users + Writer % String + G
   *
   * It records before anything is interpreted, so it sees the program's own asks whatever answers them.
   * Re-emitting `e` does not loop: `translate` walks the SOURCE program only.
   */
  def tracing[A, F[+_] : TypeableK, W, G[+_]](prog: A ! F + G)
                                             (show: [X] => F[X] => W)
  : A ! F + Writer % W + G =
    type R = F + Writer % W + G
    interpret[A, F, Writer % W, F + G](prog):
      [X] => (e: F[X]) =>
        Writer.tell(show(e)).at[R].flatMap(_ => effect[R, X](e))

  /**
   * Interpret `F` into ANOTHER ROW rather than into a value: a handler valued in a program,
   * `F ==> ([X] =>> X ! G)`, so an operation may answer with more computation. Between `F ==> Id` (`runWith`,
   * which must answer and so cannot suspend) and `F !> S` (`Effects.handle`, abort and multi-shot through
   * `Cont`), this is the tail-resumptive middle: one walk, no `Cont`, `G` forwarded.
   */
  def translate[A, F[+_] : TypeableK, G[+_]](prog: A ! F + G)(using Distinct[F + G])
                                            (h: F ==> ([X] =>> X ! G)): A ! G =
    // every step suspends under a flatMap (the answer is a program), so the recursion lives in closures, not
    // on the stack, and no @tailrec belongs here
    def go(d: Int)(p: A ! F + G): A ! G = (p.resumeRun: @unchecked) match
      case Return(a) => Return(a)
      case i @ Inject(e) => split[F, G](e)(f => h(f))(_ => forwarded[F, G](i))
      case Bind(i @ Inject(e), k) =>
        // the Bind node types e and k together
        split[F, G](e)
          (f => h(f).flatMap(x => go(d)(k(x))))
          (_ => forwarded[F, G](i).flatMap(x => go(d)(k(x))))
      // a run nested here: forced — its fold below HandleFrames.Limit, its frame on a machine at it
      case y => go(d)(HandleFrames.shallow(y, d))
    // a value: run by whoever forces it, a frame for a machine that meets it
    Free.delay(new HandleFrames.Run[A, G]:
      def at(d: Int): A ! G = go(d)(prog)
      def program: Shift.U[G, A] = intoFrame[A, F, G](h)(prog))

  /** `translate` as a frame: an operation is its program, then the continuation */
  private def intoFrame[A, F[+_] : TypeableK, G[+_]](h: F ==> ([X] =>> X ! G))(x: A ! F + G): Shift.U[G, A] =
    HandleFrames.control[F, A, A, G](pure(_), [X] => (e: F[X]) => Cont.shift[X, A ! G, A ! G](k => h(e).flatMap(k)),
      summon[TypeableK[F]])(x)

  /**
   * handle_relay (Kiselyov): tail-resumptive handling. `g` is answer-polymorphic, so by parametricity it must
   * resume the continuation exactly once, which keeps the loop tail-recursive and stack-safe on any number of
   * handled operations. Since `handle` took this loop's forwarding arm the two cost the same (1.03x, the same
   * bytes; docs/benchmarks.md §2, `hd-*`/`hff-*`); what `relay` adds is the CLAIM its type makes — `g` can
   * neither abort nor perform `G`. For handlers that do, use `Effects.handle`.
   */
  def relay[A, B, F[+_] : TypeableK, G[+_]](a: A ! F + G)(using Distinct[F + G])(f: A => B ! G)
                                           (g: [X, Y] => F[X] => X /> Y): B ! G =
    // a value: run by whoever forces it, a frame for a machine that meets it
    Free.delay(new HandleFrames.Run[B, G]:
      def at(d: Int): B ! G = new Relaying[A, B, F, G](d, f, g).loop(a)
      def program: Shift.U[G, B] =
        HandleFrames.control[F, A, B, G](f, [X] => (e: F[X]) => g[X, B ! G](e), summon[TypeableK[F]])(a))

  /**
   * `relay`'s walk, an object per run: its depth for `HandleFrames.shallow` is a field, read in the cold arm
   * only. Threaded through the loop as a parameter it cost relayPrebuilt 1.19x; one allocation a run is cheaper.
   */
  private final class Relaying[A, B, F[+_], G[+_]](depth: Int, f: A => B ! G, g: [X, Y] => F[X] => X /> Y)
                                                  (using TypeableK[F]):
      /** the terminal case, at most once a program, out of the hot loop: `split`'s inline arms expand into the
       * loop, which must stay under HotSpot's FreqInlineSize (325 bytes) to be inlined into `relay` — 24 bytes
       * past it cost the lane over 10%. Extracted, the loop is 244 bytes */
      def last(e: F[A] | G[A]): B ! G =
        split[F, G](e)(e => g(e) / f)(e => Inject(e).flatMap(f))

      // a call from inside flatMap cannot be a jump; `again` takes it
      def again(x: A ! F + G): B ! G = loop(x)
      def forward[X](i: Free[F + G, X], k: X => A ! F + G): B ! G =
        forwarded[F, G](i).flatMap(x => again(k(x)))

      @tailrec final def loop(x: A ! F + G): B ! G = (x.resumeRun: @unchecked) match
        // `g(e) / k`: the Cont carrier's application
        case Bind(i @ Inject(e), k) => split[F, G](e)(e => loop(g(e) / k))(_ => forward(i, k))
        case Inject(e) => last(e)
        case Return(a) => f(a)
        // a run nested here: forced — its fold below HandleFrames.Limit, its frame on a machine at it
        case y => loop(HandleFrames.shallow(y, depth))

}
