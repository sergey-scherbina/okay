package okay

import scala.annotation.tailrec
import scala.reflect.Typeable

/**
 * https://okmij.org/ftp/Haskell/extensible/more.pdf
 * https://blog.higher-order.com/assets/trampolines.pdf
 */

infix type ![A, F[+_]] = Free[F, A]
inline def pure[F[+_], A](a: A): A ! F = Free.pure(a)
inline def effect[F[+_], A](a: F[A]): A ! F = Free.inject(a)

infix type %[F[_, _], S] = F[S, *]
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]

/** ∀X. Typeable[F[X]], by the erasure of F */
trait TypeableK[F[_]]:
  def apply[X]: Typeable[F[X]]

given [F[_]](using t: Typeable[F[Any]]): TypeableK[F] = new:
  def apply[X]: Typeable[F[X]] = t.asInstanceOf[Typeable[F[X]]]

/**
 * Split the union by testing only the F side (the erasure of F, by
 * TypeableK), taking G by exclusion: a type test on an abstract G
 * would erase to an always-true test.
 */
inline def <|>[F[+_] : TypeableK as tf, G[+_]]: [A] => (F[A] | G[A]) => Either[F[A], G[A]] =
  [A] => e => tf[A].unapply(e) match
    case Some(e) => Left(e)
    case None => Right(e.asInstanceOf[G[A]])

/** A comonadic handler interprets each operation by its own value */
trait Handler[F[_]]:
  def handle[A](a: F[A]): A

given [F[_] : Comonad]: Handler[F] with
  inline def handle[A](a: F[A]): A = a.extract

/** Nothing has no operations left to handle */
given Handler[Nothing] with
  inline def handle[A](a: Nothing): A = a

/**
 * A handler of the operations F, with the answers S, is an interpretation
 * of F in the continuation paramonad: the natural transformation
 * F ==> ([X] =>> X /> S). That is, handlers are continuations.
 */
type Handling[F[_], S] = F ==> ([X] =>> X /> S)

/** A comonadic (per-operation) Handler is a Handling at every answer type. */
inline def handling[F[_] : Handler as H, S]: Handling[F, S] =
  [X] => e => Cont.Pure(H.handle(e))

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
    def foldCont[S](h: Handling[F, S]): A /> S
    /** run all the effects by a comonadic Handler */
    inline def runWith(using Handler[F]): A = m.foldCont(handling[F, A]) / identity

  /** handle the effect F by h (and the values by ret), forwarding the effects G */
  def handle[F[+_] : TypeableK, G[+_], A, B](m: M[F + G, A])
                                            (ret: A => M[G, B])
                                            (h: Handling[F, M[G, B]]): M[G, B] =
    m.foldCont[M[G, B]]([X] => e => <|>[F, G](e) match
      case Left(e) => h(e)
      case Right(e) => shift(k => perform(e).flatMap(k))
    ) / ret

/**
 * The freer monad is the initial (defunctionalized) encoding of Effects:
 * Inject is a suspended shift, given its meaning by foldCont's Handling.
 */
given Effects[Free] with
  override inline def pure[F[+_], A](a: A): Free[F, A] = Free.Pure(a)
  override inline def perform[F[+_], A](e: F[A]): Free[F, A] = Free.Inject(e)

  extension [F[+_], A](m: Free[F, A])
    override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = m.flatMap(f)
    override def foldCont[S](h: Handling[F, S]): A /> S =
      m.fold(Cont.Pure(_))([X] => e => k => h(e).flatMap(k(_).foldCont(h)))

/**
 * Effects are continuation programs, literally: the final (Church)
 * encoding of the same interface, where foldCont is the program itself.
 * (Unlike Free, not stack-safe on a left-nested flatMap.)
 */
type EffC[F[+_], A] = [S] => Handling[F, S] => A /> S

given Effects[EffC] with
  override def pure[F[+_], A](a: A): EffC[F, A] =
    [S] => (_: Handling[F, S]) => Cont.Pure(a)
  override def perform[F[+_], A](e: F[A]): EffC[F, A] =
    [S] => (h: Handling[F, S]) => h(e)

  extension [F[+_], A](m: EffC[F, A])
    override def flatMap[B](f: A => EffC[F, B]): EffC[F, B] =
      [S] => (h: Handling[F, S]) => m[S](h).flatMap(a => f(a)[S](h))
    override def foldCont[S](h: Handling[F, S]): A /> S = m[S](h)

object ! {
  export Free.*

  import Free.*

  type Effect[F[+_], A] = Inject[F, A]
  val Effect = Inject

  extension [F[+_], A](self: A ! F) {

    @tailrec def resume: A ! F = self match
      case Bind(Bind(a, h), k) => a.flatMap(h(_).flatMap(k)).resume
      case Bind(Pure(a), k) => k(a).resume
      case a => a

    @tailrec def next(steps: Long = 1)(using H: Handler[F]): A ! F = self.resume match
      case Bind(Effect(e), k) if steps > 0 => k(H.handle(e)).next(steps - 1)
      case a => a

    @tailrec def ? : Handler[F] ?=> ? = self match
      case Bind(a, _) => a.?
      case Effect(e) => summon[Handler[F]].handle(e)
      case Pure(a) => a
  }

  inline def run[A](e: A ! Nothing): A = e.runWith

  /**
   * Tail-resumptive handling: g is answer-polymorphic, so by parametricity
   * it must resume the continuation (exactly once), which keeps the loop
   * tail-recursive, i.e. stack-safe on any number of handled operations.
   * For handlers that abort or perform G, use Effects.handle instead.
   */
  def handle[A, B, F[+_] : TypeableK, G[+_]](a: A ! F + G)(f: A => B ! G)
                                            (g: [X, Y] => F[X] => X /> Y): B ! G = {
    @tailrec def loop(x: A ! F + G): B ! G = x.resume match
      case Bind(Effect(e), k) => <|>[F, G](e) match
        case Left(e) => loop(g(e)(k))
        case Right(e) => Effect(e).flatMap(x => handle[A, B, F, G](k(x))(f)(g))
      case Effect(e) => <|>[F, G](e) match
        case Left(e) => g(e)(f)
        case Right(e) => Effect(e).flatMap(f)
      case Pure(a) => f(a)

    loop(a)
  }

}
