package okay.freer

import okay.{Answers, Control, Monad, TailRecM, TypeableK, ==>}
import okay.freer.Row.{plus}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

/**
 * THE CLASSIC AS A TYPECLASS: what every encoding of the TREE implements — `Free`, the tree itself, and `Eager`
 * — over the carrier `C` it folds into, a `Control`: the machine's `Carrier` (the default) or the CPS `Cps`
 * (`cps`), so that a handler `F !> S` is what it always was. Level 0 is the monad and the fold (`pure`,
 * `perform`, `flatMap`, `foldCont`, `runWith`, `handle` by a clause); level 1 (specs/shift-effect.md) is
 * continuations (`Shift` in the row) and ready handlers (`Handler.Full`), the top-level functions through the
 * typeclass — by default through the tree, `reify` and `reflect`; `Classic[Free]` overrides them with the
 * functions themselves. A program written once over `Classic[M]` runs in `Free` and in `Eager` alike
 * (TestEffectsLevel1). The core's `Effects` is the interface over ROWS, every encoding's — the machine's and
 * this tree's under it (`Rowed`); this is the classic's own, over its union signatures (stage 47).
 */
trait Classic[M[_[+_], _]]:
  /** the continuation carrier `foldCont` folds into: `(A => S) => R` as the encoding has it */
  type C[_, _, _]
  /** the carrier's `shift` and `/` */
  def control: Control[C]

  def pure[F[+_], A](a: A): M[F, A]
  def perform[F[+_], A](e: F[A]): M[F, A]
  /** a bind whose left side is deferred, forced only when the encoding's interpreter reaches it, so that
   * mutually recursive functions returning `M[F, A]` call each other in tail position with no JVM frame each */
  def defer[F[+_], A, B](thunk: () => M[F, A])(f: A => M[F, B]): M[F, B]
  /** a tail call to a mutually recursive function, for code written over any `M: Classic` (`!.tailcall` on `Free`) */
  def tailcall[F[+_], A](thunk: => M[F, A]): M[F, A] = defer(() => thunk)(pure)

  extension [F[+_], A](m: M[F, A])
    def flatMap[B](f: A => M[F, B]): M[F, B]
    inline def map[B](f: A => B): M[F, B] = m.flatMap(a => pure(f(a)))
    /** `foldMap` into the carrier: the program's fold, each operation answered by `h` as a continuation
     * (`Static.foldMap` is the same fold into any `Selective`). The result is still waiting for its LAST
     * continuation: `/ identity` when `S` is the answer (`runWith`), `/ ret` to finish into `S` (`handle`).
     * TestFoldCont and docs/contract.md show three `S`. At `C = Cps` the handler is `F !> S` and the fold `A />> S` */
    def foldCont[S](h: Interpr[F, C, S]): C[A, S, S]
    /** run all the effects by a comonadic Answers (the foldCont definition; encodings may override with an equivalent fast path) */
    def runWith(using Answers[F]): A = control./(m.foldCont(interpr[C, F, A](using control, summon[Answers[F]])))(identity)
  /** handle the effect F by h (and the values by ret), forwarding the effects G; for mass tail-resumption
   * prefer !.relay (measured) */
  def handle[F[+_], G[+_]](using TypeableK[F])[A, B](m: M[F + G, A])
                          (ret: A => M[G, B])
                          (h: Interpr[F, C, M[G, B]]): M[G, B] =
    control./(m.foldCont[M[G, B]]([X] => e => split[F, G](e)(e => h(e))(e => control.shift(k => perform(e).flatMap(k)))))(ret)

  // LEVEL 1 (specs/shift-effect.md): continuations and ready handlers in any encoding, through the tree by
  // default; `Classic[Free]` overrides them with the top-level functions themselves

  /** Danvy-Filinski's capture (the top-level `shift`) */
  def shift[R, A, F[+_]](f: (A => M[Shift % R + F, R]) => M[Shift % R + F, R])(using k: Shift.Key[R], at: At): M[Shift % R + F, A] =
    reflect[M, Shift % R + F, A](okay.freer.shift[R, A, F](kk =>
      reify[M, Shift % R + F, R](f(a => reflect[M, Shift % R + F, R](kk(a))(using this)))(using this)))(using this)

  /** the capture whose body runs outside its `reset` (the top-level `shift0`) */
  def shift0[R, A, F[+_]](f: (A => M[F, R]) => M[F, R])(using k: Shift.Key[R], at: At): M[Shift % R + F, A] =
    reflect[M, Shift % R + F, A](okay.freer.shift0[R, A, F](kk =>
      reify[M, F, R](f(a => reflect[M, F, R](kk(a))(using this)))(using this)))(using this)

  /** delimit (the top-level `reset`) */
  def reset[R, F[+_]](body: M[Shift % R + F, R])(using Shift.Key[R], Distinct[Shift % R + F], Shift.Machine[F]): M[F, R] =
    reflect[M, F, R](okay.freer.reset[R, F](reify[M, Shift % R + F, R](body)(using this)))(using this)

  /** take a ready handler's effect off the row (the program's `p.handle(h)`; two arguments in one list, so the
   * core's `handle(m)(ret)(clause)` stays its own overload) */
  def handle[A, G[+_], E[+_], I, O[_], N[_[+_]], F[+_]](m: M[G, A], h: Handler.Full[E, I, O, N])
            (using row: (A ! G) =:= (A ! E + F), ok: A <:< I, d: Distinct[E + F], n: N[F]): M[F, O[A]] =
    reflect[M, F, O[A]](h.run[A, F](row(reify[M, G, A](m)(using this))))(using this)

  /** a program with no effect left, to its value (the program's `run`) */
  def run[A](m: M[Pure, A]): A = reify[M, Pure, A](m)(using this).run

  /** `foldMap` into any monad, through the tree (the program's `foldMap`, below in `Free`) */
  def foldMap[F[+_], A, G[_]](m: M[F, A])(nt: F ==> G)(using Monad[G], TailRecM[G]): G[A] =
    reify[M, F, A](m)(using this).foldMap(nt)

/** the instance, a class over its CARRIER `C0` — whatever has a `Control`: the machine's (the default), the CPS
 * `Cps` (`cps`) — its type naming the carrier (`Classic.Aux`), so a handler's type is known wherever the
 * instance is reached by its type — `Classic[Free]`, `summon`, a `using` — and not only through the given */
final class FreeEffectsAt[C0[_, _, _]](val control: Control[C0]) extends Classic[Free]:
  type C = C0

  override inline def pure[F[+_], A](a: A): Free[F, A] = Free.Return(a)
  override inline def perform[F[+_], A](e: F[A]): Free[F, A] = Free.Inject(e)
  override inline def defer[F[+_], A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    Free.defer(thunk)(f)
  /** the tree has a node for exactly this */
  override def tailcall[F[+_], A](thunk: => Free[F, A]): Free[F, A] = Free.delay(() => thunk)

  extension [F[+_], A](m: Free[F, A])
    override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = m.flatMap(f)
    override def foldCont[S](h: Interpr[F, C0, S]): C0[A, S, S] =
      Free.fold(m)(control.pure(_))([X] => e => k => control.flatMap(h(e))(k(_).foldCont(h)))
    /** the same answer as the foldCont definition, in one pass instead of two */
    override def runWith(using Answers[F]): A = runFree(m)

  // level 1: the top-level functions themselves
  override def shift[R, A, F[+_]](f: (A => R ! Shift % R + F) => R ! Shift % R + F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
    okay.freer.shift[R, A, F](f)
  override def shift0[R, A, F[+_]](f: (A => R ! F) => R ! F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
    okay.freer.shift0[R, A, F](f)
  override def reset[R, F[+_]](body: R ! Shift % R + F)(using Shift.Key[R], Distinct[Shift % R + F], Shift.Machine[F]): R ! F =
    okay.freer.reset[R, F](body)
  override def handle[A, G[+_], E[+_], I, O[_], N[_[+_]], F[+_]](m: Free[G, A], h: Handler.Full[E, I, O, N])
                     (using row: (A ! G) =:= (A ! E + F), ok: A <:< I, d: Distinct[E + F], n: N[F]): O[A] ! F =
    h.run[A, F](row(m))
  override def run[A](m: A ! Pure): A = m.runWith

  @tailrec private def runFree[F[+_], A](m: Free[F, A])(using H: Answers[F]): A =
    (m.resume: @unchecked) match
      case Free.Return(a) => a
      case Free.Inject(e) => H.handle(e)
      case Free.Bind(Free.Inject(e), f) => runFree(f(H.handle(e)))

  /**
   * `Effects.handle`'s definition in one loop over the tree. The definition answers EVERY operation in `Cps`,
   * so a forwarded one costs a capture spent on copying (+112.7 B and 1.51x against `relay`, docs/benchmarks.md
   * §2, `hd-*`). Here a forwarded operation is re-emitted on the `G` side as `relay` does, and `Cps` is
   * entered only for an operation the handler claims. The same function: a forwarded operation is already
   * committed to the `G` program, which a later abort cannot un-perform under the definition either
   * (TestHandleForward pins aborting and multi-shot forwarding, red first against a wrong arm).
   *
   * A handler that answers with a `pure` continues the loop with one tail call; only a real capture
   * reifies the rest, under a `Delay` so a deep program trampolines. A `Delay` on EVERY handled operation
   * cost 59 µs of a 61 µs gap (`hff-*`): its `Pure` continuation rotates into a left-nested `Bind`.
   */
  override def handle[F[+_], G[+_]](using TypeableK[F])[A, B](m: Free[F + G, A])
                                   (ret: A => Free[G, B])
                                   (h: Interpr[F, C0, Free[G, B]]): Free[G, B] =
    // NOT @tailrec: the deferring arms mention `loop` inside a closure, which the annotation reads as a non-tail
    // call; the answered arm is a real tail call, and TestHandleForward's stack-safety tests guard the depth.
    // The terminal and capturing arms live in their own methods, as `relay`'s `last`, to keep this loop
    // small: `Free.resume` (323 bytes, under FreqInlineSize 325) is pasted into it, and a loop that is itself
    // too big to inline lost 15% on handlePrebuilt and handleCapture (`de-*`).
    def last(e: F[A] | G[A]): Free[G, B] =
      split[F, G](e)(e => control./(h(e))(ret))(e => Free.Inject(e).flatMap(ret))

    def capture[X](d: Int)(c: C0[X, Free[G, B], Free[G, B]], k: X => Free[F + G, A]): Free[G, B] =
      control./(c)(x => Free.delay(() => loop(d)(k(x))))

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
              if control.isAnswer(c) then loop(d)(k(control.answerOf(c))) else capture(d)(c, k)
            })
          (_ => forward(d)(i, k))
      // a run nested here: forced — its fold below HandleFrames.Limit, its frame on a machine at it
      case y => loop(d)(HandleFrames.shallow(y, d))

    // a value: run by whoever forces it, a frame for a machine that meets it
    Free.delay(new HandleFrames.Run[B, G]:
      def at(d: Int): Free[G, B] = loop(d)(m)
      def program: Shift.U[G, B] = HandleFrames.control[F, A, B, G, C0](control, ret, h, summon[TypeableK[F]])(m))

/**
 * Any Effects program in any other encoding: the interface's initiality as a function. An encoding is fixed
 * by `pure` and `perform` and `foldCont` is the fold, so there is one structure-preserving way across.
 * `reify` and `reflect` are this at the two ends.
 */
inline def convert[M[_[+_], _] : Classic as M,
  N[_[+_], _] : Classic as N, F[+_], A](m: M[F, A]): N[F, A] =
  M.control./(m.foldCont[N[F, A]]([X] => e => M.control.shift(k =>
    N.perform(e).flatMap(k))))(a => N.pure(a))

/** `M.tailRecM(a)(f)`: `TailRecM[F]`'s loop, the carrier's own (specs/eager-carrier-depth.md) */
extension [F[_]](M: Monad[F])
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]])(using R: TailRecM[F]): F[B] = R.tailRecM(a)(f)

/** any Effects program as a `Free` tree: building the syntax is itself an interpretation */
inline def reify[M[_[+_], _] : Classic, F[+_], A](m: M[F, A]): A ! F =
  convert[M, Free, F, A](m)

/**
 * A `Free` tree read INTO any encoding — `Eager`, or one of your own — where `reify` observes an encoding as
 * syntax (for a debugger, a rewriter, `Pipeline`'s optimizer). Together they round-trip (TestReflect). Inside
 * package `okay` the name shadows `scala.reflect`: write `scala.reflect.X` there.
 */
def reflect[M[_[+_], _] : Classic as M, F[+_], A](m: A ! F): M[F, A] =
  // straight into the target: a tree is already syntax, so no continuation is reified on the way
  Free.fold(m)(M.pure)([X] => e => k => M.perform(e).flatMap(x => reflect[M, F, A](k(x))))


/** THE CLASSIC AS A TOOLKIT, `!` for short: the functions over the tree itself — `!.run`, `!.relay`, `!.foldM`
 * — and `Free`'s constructors exported, the companion of the typeclass above */
object Classic {
  /** an encoding WITH ITS CARRIER NAMED: what an instance's given declares (`given Classic.Aux[Free, Cps]`), so
   * that `foldCont`'s handler type is concrete wherever the instance is reached by its type, not only by the
   * given's own object */
  type Aux[M[_[+_], _], C0[_, _, _]] = Classic[M] { type C = C0 }

  /** level 1, any encoding in direct style: `M[F, *]` as a monad, for `direct[[A] =>> M[F, A]]` over `Classic[M]` */
  def monad[M[_[+_], _], F[+_]](using E: Classic[M]): Monad[[A] =>> M[F, A]] = new Monad[[A] =>> M[F, A]]:
    def pure[A](a: A): M[F, A] = E.pure(a)
    extension [A](a: M[F, A])
      def flatMap[B](f: A => M[F, B]): M[F, B] = E.flatMap(a)(f)

  /** the staging entry for tree programs: `Classic[Free]`, `Classic[Eager]`, or any `M` with an instance in
   * scope; with `trait Classic` it forms one door, as a class and its companion do. Summoned WITH ITS CARRIER:
   * the pattern binds `c` to what the instance declares (`Classic.Aux`), so `Classic[Free].handle(…)(h)` takes
   * the handler at `Cps` — `summonInline[Classic[M]]` answered at `Classic[M]`, the carrier unknown — and
   * `summonFrom` still defers the search to where an inline program is expanded (`sprog[Free]`, TestEffects) */
  transparent inline def apply[M[_[+_], _]] =
    compiletime.summonFrom { case e: Classic.Aux[M, c] => e }

  // the tree's constructors and `loop`; not `pure`, the top-level function's (a file importing both `okay.freer.*`
  // and `!.*` would find it twice)
  export Free.{pure as _, *}

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
   * not `defer` with `pure`, which would push a `.flatMap(pure)` down every hop (`Cps.delay` on the Cps side) */
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
   * Interpret `F` into ANOTHER ROW rather than into a value: a handler valued in a program,
   * `F ==> ([X] =>> X ! G)`, so an operation may answer with more computation. Between `F ==> Id` (`runWith`,
   * which must answer and so cannot suspend) and `F !> S` (`Effects.handle`, abort and multi-shot through
   * `Cps`), this is the tail-resumptive middle: one walk, no `Cps`, `G` forwarded.
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
    HandleFrames.control[F, A, A, G, Cps](summon[Control[Cps]], pure(_), [X] => (e: F[X]) => Cps.shift[X, A ! G, A ! G](k => h(e).flatMap(k)),
      summon[TypeableK[F]])(x)

  /**
   * handle_relay (Kiselyov): tail-resumptive handling. `g` is answer-polymorphic, so by parametricity it must
   * resume the continuation exactly once, which keeps the loop tail-recursive and stack-safe on any number of
   * handled operations. Since `handle` took this loop's forwarding arm the two cost the same (1.03x, the same
   * bytes; docs/benchmarks.md §2, `hd-*`/`hff-*`); what `relay` adds is the CLAIM its type makes — `g` can
   * neither abort nor perform `G`. For handlers that do, use `Effects.handle`.
   */
  def relay[A, B, F[+_] : TypeableK, G[+_]](a: A ! F + G)(using Distinct[F + G])(f: A => B ! G)
                                           (g: [X, Y] => F[X] => X />> Y): B ! G =
    // a value: run by whoever forces it, a frame for a machine that meets it
    Free.delay(new HandleFrames.Run[B, G]:
      def at(d: Int): B ! G = new Relaying[A, B, F, G](d, f, g).loop(a)
      def program: Shift.U[G, B] =
        HandleFrames.control[F, A, B, G, Cps](summon[Control[Cps]], f, [X] => (e: F[X]) => g[X, B ! G](e), summon[TypeableK[F]])(a))

  /**
   * `relay`'s walk, an object per run: its depth for `HandleFrames.shallow` is a field, read in the cold arm
   * only. Threaded through the loop as a parameter it cost relayPrebuilt 1.19x; one allocation a run is cheaper.
   */
  private final class Relaying[A, B, F[+_], G[+_]](depth: Int, f: A => B ! G, g: [X, Y] => F[X] => X />> Y)
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
        // `g(e) / k`: the Cps carrier's application
        case Bind(i @ Inject(e), k) => split[F, G](e)(e => loop(g(e) / k))(_ => forward(i, k))
        case Inject(e) => last(e)
        case Return(a) => f(a)
        // a run nested here: forced — its fold below HandleFrames.Limit, its frame on a machine at it
        case y => loop(HandleFrames.shallow(y, depth))

}
