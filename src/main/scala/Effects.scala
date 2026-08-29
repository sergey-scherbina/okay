package okay

import scala.annotation.tailrec
import scala.reflect.Typeable

/**
 * Extensible effects, founded on the continuation paramonad.
 *
 * A computation A ! F is a freer-monad tree over the signature F; its
 * meaning is its image in Cont, given by foldCont, where a handler is
 * an interpretation F !> S = F ==> ([X] =>> X /> S) — that is,
 * handlers are continuations. The Effects interface is final tagless
 * with two encodings (Free — initial, Eff — final, Church), and the
 * object ! is the concrete toolkit over the Free encoding: stepping
 * (resume, next, ?), running, and the tail-resumptive relay.
 *
 * Choosing an encoding: the tree is for tools — stepping, staged
 * relay, stack safety on any bind shape; the function is for speed —
 * fused build-and-run pipelines with no tree at all; the interface is
 * for not choosing too early. fromFree and reify move programs
 * between the encodings.
 *
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * https://blog.higher-order.com/assets/trampolines.pdf
 */

/** fix the parameter of a binary signature: State % S, Throws % E */
infix type %[F[_, _], S] = F[S, *]

/** the union of two signatures: F + G */
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/** a computation of A performing the operations of F: A ! F */
infix type ![A, F[+_]] = Free[F, A]

/** a value as a computation */
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)

/** an operation as a computation */
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

/** an interpretation of F into any Control carrier C, with the answers S */
type Interpr[F[_], C[_, _, _], S] = F ==> C[*, S, S]

/**
 * A handler of the operations F, with the answers S, is an interpretation
 * of F in the continuation paramonad: the natural transformation
 *
 * F ==> ([X] =>> X /> S)
 *
 * That is, handlers are continuations.
 */
infix type !>[F[_], S] = Interpr[F, Cont, S]

/** A comonadic handler interprets each operation by its own value */
trait Handler[F[_]]:
  def handle[A](a: F[A]): A

/** A comonadic (per-operation) Handler at every answer type. */
inline def handler[F[_] : Handler as H, S]: F !> S =
  [X] => e => Cont.Pure(H.handle(e))

/** the same, at any Control carrier */
inline def interpr[C[_, _, _] : Control as C, F[_] : Handler as H, S]: Interpr[F, C, S] =
  [X] => e => C.pure(H.handle(e))

given [F[_] : Comonad]: Handler[F] with
  inline def handle[A](a: F[A]): A = a.extract

/** Nothing has no operations left to handle */
given Handler[Nothing] with
  inline def handle[A](a: Nothing): A = a

/**
 * Final tagless interface of extensible effects: M[F, A] computes A
 * performing the operations of the signature F. The meaning of a
 * computation is its image in the continuation paramonad, given by
 * foldCont; run and handle are founded on it.
 */
trait Effects[M[_[+_], _]]:
  def pure[F[+_], A](a: A): M[F, A]
  def perform[F[+_], A](e: F[A]): M[F, A]

  extension [F[+_], A](m: M[F, A])
    def flatMap[B](f: A => M[F, B]): M[F, B]
    inline def map[B](f: A => B): M[F, B] = m.flatMap(a => pure(f(a)))
    /** interpret the operations, i.e. reflect the computation into Cont */
    def foldCont[S](h: F !> S): A /> S
    /** the same at any Control carrier (foldCont is its Cont fast path) */
    def foldIn[C[_, _, _] : Control, S](h: Interpr[F, C, S]): C[A, S, S]
    /** run all the effects by a comonadic Handler (the foldCont definition; encodings may override with an equivalent fast path) */
    def runWith(using Handler[F]): A = m.foldCont(handler[F, A]) / identity
    /**
     * run at a chosen Control carrier. Cont is the stack-safe default;
     * Func composes closures at run time — measured no faster than
     * Cont here (see specs/staged-effects.md): true staged effects are
     * inline handler-passing programs over Control, not a carrier.
     */
    inline def runIn[C[_, _, _]](using Handler[F], Control[C]): A =
      m.foldIn[C, A](interpr[C, F, A]) / identity

  /** handle the effect F by h (and the values by ret), forwarding the effects G */
  def handle[F[+_] : TypeableK, G[+_], A, B](m: M[F + G, A])
                                            (ret: A => M[G, B])
                                            (h: F !> M[G, B]): M[G, B] =
    m.foldCont[M[G, B]]([X] => e => <|>[F, G](e) match
      case Left(e) => h(e)
      case Right(e) => shift(k => perform(e).flatMap(k))
    ) / ret

/** the staging entry for effect programs, as staged is for Control */
transparent inline def Effects[M[_[+_], _]]: Effects[M] =
  compiletime.summonInline[Effects[M]]

/** ∀X, the runtime test for F[X], by the erasure of F */
trait TypeableK[F[_]]:
  def unapply[A](x: Any): Option[x.type & F[A]]

given [F[+_]](using t: Typeable[F[Nothing]]): TypeableK[F] = new:
  // no cast: sound by covariance, F[Nothing] <: F[X] for every X
  // (the erasure trust lives in the compiler-synthesized class
  // test behind Typeable[F[Nothing]])
  def unapply[A](x: Any): Option[x.type & F[A]] = t.unapply(x)

/**
 * Split the union by testing only the F side (the erasure of F, by
 * TypeableK), taking G by exclusion: a type test on an abstract G
 * would erase to an always-true test.
 */
inline def <|>[F[+_] : TypeableK as T, G[+_]]: [A] => (F[A] | G[A]) => Either[F[A], G[A]] =
  [A] => e => e match
    case T(e) => Left(e)
    // the trusted kernel, sound by the excluded middle of the union:
    // a value of F[A] | G[A] that is not an F[A] is a G[A]
    case e => Right(e.asInstanceOf[G[A]])

/**
 * The freer monad is the initial (defunctionalized) encoding of Effects:
 * Inject is a suspended shift, given its meaning by foldCont's !> interpretation.
 * Choose Free when the program is a thing: to step it, inspect it,
 * relay it in stages — and stay stack-safe on any bind shape.
 */
given Effects[Free] with
  override inline def pure[F[+_], A](a: A): Free[F, A] = Free.Pure(a)
  override inline def perform[F[+_], A](e: F[A]): Free[F, A] = Free.Inject(e)

  extension [F[+_], A](m: Free[F, A])
    override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = m.flatMap(f)
    override def foldCont[S](h: F !> S): A /> S =
      m.fold(Cont.Pure(_))([X] => e => k => h(e).flatMap(k(_).foldCont(h)))
    override def foldIn[C[_, _, _], S](h: Interpr[F, C, S])(using C: Control[C]): C[A, S, S] =
      m.fold(C.pure)([X] => e => k => C.flatMap(h(e))(x => k(x).foldIn[C, S](h)))
    /** the same answer as the foldCont definition, in one pass instead of two */
    override def runWith(using Handler[F]): A = runFree(m)

  @tailrec private def runFree[F[+_], A](m: Free[F, A])(using H: Handler[F]): A = m match
    case Free.Pure(a) => a
    case Free.Inject(e) => H.handle(e)
    case Free.Bind(Free.Bind(a, f), g) => runFree(Free.Bind(a, f(_).flatMap(g)))
    case Free.Bind(Free.Pure(a), f) => runFree(f(a))
    case Free.Bind(Free.Inject(e), f) => runFree(f(H.handle(e)))

/**
 * Effects are continuation programs, literally: Eff is the final
 * (Church) encoding of the interface — a computation as the function
 * of its handler, where foldCont is the program itself. This is how
 * extensible effects were first defined (Kiselyov–Sabry–Swords 2013,
 * by continuations), before the freer tree of 2015 — so Eff and Free
 * reenact the history, and "Free and Eff agree" is the claim that the
 * two papers describe one thing. Choose Eff when the program is a
 * pipeline: built once and run, the handler fusing into the closures
 * with no tree materialized at all. (Unlike Free, Eff is not
 * stack-safe on a left-nested flatMap, and cannot be stepped.)
 */
type Eff[F[+_], A] = [S] => F !> S => A /> S

given Effects[Eff] with
  override inline def pure[F[+_], A](a: A): Eff[F, A] =
    [S] => (_: F !> S) => Cont.Pure(a)
  override inline def perform[F[+_], A](e: F[A]): Eff[F, A] =
    [S] => (h: F !> S) => h(e)

  extension [F[+_], A](m: Eff[F, A])
    override inline def flatMap[B](f: A => Eff[F, B]): Eff[F, B] =
      [S] => (h: F !> S) => m[S](h).flatMap(a => f(a)[S](h))
    override inline def foldCont[S](h: F !> S): A /> S = m[S](h)
    /** Eff is committed to Cont; changing the carrier reifies the tree first */
    override inline def foldIn[C[_, _, _], S](h: Interpr[F, C, S])(using Control[C]): C[A, S, S] =
      (m[A ! F]([X] => e => shift(k => effect(e).flatMap(k))) / (a => Free.pure(a))).foldIn[C, S](h)

/**
 * Free is initial: the tree interprets uniquely into every Effects
 * instance (fromFree). Eff is final: every instance observes into it
 * by its own foldCont (toEff). So all the encodings live between the
 * tree and its behavior, and reify closes the circle: any program
 * materializes back as syntax.
 */
def fromFree[M[_[+_], _] : Effects as E, F[+_], A](m: A ! F): M[F, A] =
  m.fold(E.pure)([X] => e => k => E.perform(e).flatMap(x => fromFree[M, F, A](k(x))))

/** every Effects instance observes into Eff, by its own foldCont */
inline def toEff[M[_[+_], _] : Effects, F[+_], A](m: M[F, A]): Eff[F, A] =
  [S] => (h: F !> S) => m.foldCont(h)

/**
 * any Effects program materializes back as a Free tree: building
 * the syntax is itself an interpretation !>, with the answers A ! F
 */
inline def reify[M[_[+_], _] : Effects, F[+_], A](m: M[F, A]): A ! F =
  m.foldCont[A ! F]([X] => e => shift(k => effect(e).flatMap(k))) / (a => pure(a))

object ! {
  export Free.*

  import Free.*

  /** the domain name of Inject: an operation node */
  type Effect[F[+_], A] = Inject[F, A]
  val Effect = Inject

  extension [F[+_], A](self: A ! F) {

    /** normalize to a head form: Pure, Effect, or Bind(Effect, k) */
    @tailrec def resume: A ! F = self match
      case Bind(Bind(a, h), k) => a.flatMap(h(_).flatMap(k)).resume
      case Bind(Pure(a), k) => k(a).resume
      case a => a

    /** step through the next n operations by the Handler */
    @tailrec def next(steps: Long = 1)(using H: Handler[F]): A ! F = self.resume match
      case Bind(Effect(e), k) if steps > 0 => k(H.handle(e)).next(steps - 1)
      case a => a

    /** peek the nearest answer: the value, or the first operation handled */
    @tailrec def ? : Handler[F] ?=> ? = self match
      case Bind(a, _) => a.?
      case Effect(e) => summon[Handler[F]].handle(e)
      case Pure(a) => a
  }

  /** run a closed computation */
  inline def run[A](e: A ! Nothing): A = e.runWith

  /**
   * handle_relay (Kiselyov): tail-resumptive handling. g is
   * answer-polymorphic, so by parametricity it must resume the
   * continuation (exactly once), which keeps the loop tail-recursive,
   * i.e. stack-safe on any number of handled operations. For handlers
   * that abort or perform G, use Effects.handle instead.
   */
  def relay[A, B, F[+_] : TypeableK, G[+_]](a: A ! F + G)(f: A => B ! G)
                                           (g: [X, Y] => F[X] => X /> Y): B ! G = {
    @tailrec def loop(x: A ! F + G): B ! G = x.resume match
      case Bind(Effect(e), k) => <|>[F, G](e) match
        case Left(e) => loop(g(e)(k))
        case Right(e) => Effect(e).flatMap(x => relay[A, B, F, G](k(x))(f)(g))
      case Effect(e) => <|>[F, G](e) match
        case Left(e) => g(e)(f)
        case Right(e) => Effect(e).flatMap(f)
      case Pure(a) => f(a)

    loop(a)
  }

}
