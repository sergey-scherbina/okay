package okay

import okay.core.{Both, Eff, NoEffects, Needs, Paths, Split}
import okay.one.Cont
import okay.one.handler.Handler as OneHandler
import okay.one.row.{In, Removed, Pure as OnePure}
import scala.language.implicitConversions

/**
 * THE PLATFORM'S PROGRAM, A VALUE — THE KERNEL'S TREE ITSELF (specs/keyed-root.md): `A ! Es`, a program of `A`
 * needing the effects `Es` (`Reader % Int + Writer % String`, in any order, among any others), is okay-core's `Cont`
 * at no row, each operation performed at a place BY KEY — its effect's constructor (`KeyOf`) — and each handler
 * saying the key is its own. So a bind is one node of the kernel's, a program needing fewer effects IS one needing
 * more (`at`, `widen`: nothing at run time), and the effects `Es` are the compiler's alone: `Join` (a bind's),
 * `Within` (a widening's), `Split` (a runner's) prove, at compile time, and carry nothing.
 *
 * `A !! Es` is the facade's program, a context function of the place (`okay.core.!`); `p.ctx`, `Program.of(q)` cross.
 */
object Run:
  /** the kernel's program at no row: its operations at places by key — `Es` the compiler's, phantom */
  opaque type Prog[Es[_], +A] <: AnyRef = okay.one.![A, OnePure]

  /** the kernel's program behind it, at any row: a place by key is in every row */
  inline def kernel[A, Es[_], R <: okay.core.Row](p: A ! Es): Eff[R, A] = p.asInstanceOf[Eff[R, A]]
  /** a kernel program over no row as a program needing `Es`: its operations at places by key (THE TRUSTED STEP:
   * the caller says they are of `Es`) */
  inline def of[A, Es[_]](e: okay.one.![A, OnePure]): A ! Es = e

  /** a handler of the kernel's, `h`, installed at `E`'s key around `p`: what a runner written with the kernel's
   * forms (a `Stateful` of its own) does — the effects of the answer the caller's (`Out`, its `Split`'s rest) */
  inline def install[E[_], A, B, Es[_], Out[_]](p: A ! Es)(h: OneHandler[E, OnePure, A, B])(using k: KeyOf[E]): B ! Out =
    Cont.handle(p)(h.keyed(k.ref))(using Removed.Absent[E, OnePure]())

  extension [A, Es[_]](p: A ! Es)
    /** the program at the row of a place: itself — its places are by key (kept for code that fitted paths) */
    inline def fit[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = p.asInstanceOf[Eff[R, A]]
    /** the bind: one node of the kernel's; the continuation's effects joined to these at compile time (`Join`) */
    inline def flatMap[B, Es3[_]](f: A => B ! Es3)(using j: Join[Es, Es3]): B ! j.Out =
      Cont.Bind[OnePure, Nothing, Nothing, Nothing, A, B](p, f)
    inline def map[B](f: A => B): B ! Es = Cont.Map[OnePure, Nothing, Nothing, A, B](p, f)
    /** `p` then `q`, `p`'s value dropped */
    inline def >>[B, Es3[_]](q: => B ! Es3)(using j: Join[Es, Es3]): B ! j.Out =
      Cont.Bind[OnePure, Nothing, Nothing, Nothing, A, B](p, _ => q)
    /** the same program needing more effects, `Es` among `Es2`: itself */
    inline def at[Es2[_]](using w: Within[Es, Es2]): A ! Es2 = p
    /** `h`'s effect taken off wherever it is among `Es`: `h` installed at its key, a program needing the rest */
    inline def handle[E[_], O[_]](h: Handler[E, O])(using s: Split[Es, E], k: KeyOf[E]): O[A] ! s.Rest =
      Cont.handle(p)(h.kernel[OnePure, A].keyed(k.ref))(using Removed.Absent[E, OnePure]())
    /** `h` INTERCEPTING `E` for this program, `E` kept: `h` answers the program's operations of `E`; its own
     * clauses' go to the handler of `E` outside (a translating clause runs outside it) */
    inline def intercept[E[_]](h: Handler[E, [x] =>> x])(using w: Within[E, Es], k: KeyOf[E]): A ! Es =
      Cont.handle(p)(h.kernel[OnePure, A].keyed(k.ref))(using Removed.Absent[E, OnePure]())
    /** `E` TRANSLATED: each operation of `E` a program needing `F`, run where it is performed (outside the
     * translating handler), the rest resumed with its value */
    inline def translate[E[_], F[_]](f: [X] => E[X] => X ! F)(using s: Split[Es, E], k: KeyOf[E]): A ! okay.core.+[F, s.Rest] =
      Cont.handle(p)(Run.translating[E, F, A](f).keyed(k.ref))(using Removed.Absent[E, OnePure]())
    /** `E` TRANSLATED INTO `F`, an effect the program needs already */
    inline def translateInto[E[_], F[_]](f: [X] => E[X] => X ! F)(using s: Split[Es, E], w: Within[F, s.Rest], k: KeyOf[E]): A ! s.Rest =
      Cont.handle(p)(Run.translating[E, F, A](f).keyed(k.ref))(using Removed.Absent[E, OnePure]())
    /** an error thrown in `p` given to `f`: its program in `p`'s place (the kernel's guard) */
    def recover[B >: A](f: Throwable => B ! Es): B ! Es = Cont.recover(p: okay.one.![B, OnePure])(f)
    /** `fin` after `p`, whatever way it is left */
    def ensure(fin: Unit ! Es): A ! Es = Cont.ensure(p)(fin)
    /** a cancel at a step of `p` kept for when it is left */
    def uncancelable: A ! Es = Cont.uncancelable(p)
    /** the facade's form of the same program: at the place's row, itself */
    def ctx: A !! Es = (c: Needs[Es]) ?=> p.asInstanceOf[Eff[c.R, A]]
    /** RUN with each effect ANSWERED in place by its `Answers` (a sum's assembled from its parts) */
    inline def runWith(using a: Answers[Es], ks: KeysOf[Es]): A = Program.runAll(p, a, ks)

  extension [A, E[_]](p: A ! E)
    /** `h` around a program whose ONE effect is `E`: every operation by key that comes to it answered by `h`
     * (the kernel's `Handler.anyKey`) — no key asked for, so `E` may be abstract */
    def handleAll[O[_]](h: Handler[E, O]): O[A] ! Pure =
      Cont.handle(p)(h.kernel[OnePure, A].keyed(okay.one.handler.Handler.anyKey))(using Removed.Absent[E, OnePure]())

  extension [A](p: A ! Pure)
    /** a program needing nothing: its value */
    def run: A = Cont.run(p)

  /** the translating handler of `f` (the kernel's `Translating`) */
  def translating[E[_], F[_], A](f: [X] => E[X] => X ! F): okay.one.handler.Translating[E, OnePure, A, A] =
    new okay.one.handler.Translating[E, OnePure, A, A]:
      def ret(a: A) = Cont.pure(a)
      def apply[X](op: E[X]): okay.one.![X, OnePure] = f(op)

/** `A ! Es`: a program of `A` needing `Es` — a value, the kernel's tree */
infix type ![+A, Es[_]] = Run.Prog[Es, A]

/** `A !! Es`: the facade's program, a context function of the place */
infix type !![+A, Es[_]] = okay.core.![A, Es]

/** no effects: a program needing nothing, run by `run` */
type Pure[X] = NoEffects[X]

/** THE KEYS OF A SUM of effects: each part's, by the sum's shape — what `runWith` and `Program.of` place by */
sealed abstract class KeysOf[Es[_]]
object KeysOf:
  private[okay] final class One[E[_]](val key: KeyOf[E]) extends KeysOf[E]
  private[okay] final class Two[F[_], G[_]](val left: KeysOf[F], val right: KeysOf[G]) extends KeysOf[okay.core.+[F, G]]
  private[okay] object NoneK extends KeysOf[Pure]
  import scala.compiletime.summonFrom
  inline given derived[Es[_]]: KeysOf[Es] = summonFrom {
    case _: (Es[Any] =:= NoEffects[Any]) => NoneK.asInstanceOf[KeysOf[Es]]
    case s: Evidence.Sum[Es] => Two[s.L, s.R](derived[s.L], derived[s.R]).asInstanceOf[KeysOf[Es]]
    case k: KeyOf[Es] => One(k)
  }

object Program:
  /** a program of the kernel over no row (a `k`'s answer inside a clause) as one needing nothing more */
  private[okay] inline def lift[A, T <: okay.core.Row](e: Eff[T, A]): A ! Pure = Run.of(e.asInstanceOf[okay.one.![A, OnePure]])
  /** `p` run, each effect answered by its part of `a` at its key */
  def runAll[A, Es[_]](p: A ! Es, a: Answers[Es], ks: KeysOf[Es]): A =
    var e: okay.one.![A, OnePure] = Run.kernel(p)
    // BOUNDED by the sum as written in the program's type: one call a `+`
    def install(x: Answers[?], k: KeysOf[?]): Unit = (x, k) match
      case (Answers.NoneA, _) => ()
      case (s: Answers.Sum[?, ?], t: KeysOf.Two[?, ?]) => install(s.left, t.left); install(s.right, t.right)
      case (l, o: KeysOf.One[?]) => e = answered(e, l.asInstanceOf[Answers[[x] =>> Any]], o.key.ref)
      case _ => throw IllegalStateException(s"runWith: the answers and the keys of $x do not meet")
    install(a, ks)
    Cont.run(e)
  private def answered[E[_], A](e: okay.one.![A, OnePure], a: Answers[E], key: AnyRef): okay.one.![A, OnePure] =
    Cont.handle(e)(new okay.one.handler.Answering[E, OnePure, A, A]:
      def ret(x: A) = Cont.pure(x)
      def value[X](op: E[X]): X = a.handle(op)
    .keyed(key))(using Removed.Absent[E, OnePure]())
  /** A PROGRAM OF THE KERNEL at the row `E` alone as the root's value needing `E`: each operation, at the head of
   * that row, performed again at `E`'s key */
  def fromKernel[E[_], A](e: Eff[E +: okay.core.Pure, A])(using k: KeyOf[E]): A ! E =
    Run.of(Cont.handle(e)(new okay.one.handler.Translating[E, OnePure, A, A]:
      def ret(a: A) = Cont.pure(a)
      def apply[X](op: E[X]): okay.one.![X, OnePure] = Cont.perform[E, OnePure, X](op)(using k.in)))
  /** from the facade's form: the same program, a value — fitted to the places by key of its effects */
  def of[A, Es[_]](q: A !! Es)(using ks: KeysOf[Es]): A ! Es =
    Run.of(q(using Needs.of(paths[Es](ks))).asInstanceOf[okay.one.![A, OnePure]])
  private def paths[Es[_]](ks: KeysOf[?]): Paths[Es, okay.core.Row] = (ks match
    case KeysOf.NoneK => Paths.none
    case t: KeysOf.Two[?, ?] => Paths.Both(paths(t.left), paths(t.right))
    case o: KeysOf.One[?] => Paths.One(o.key.at[okay.core.Row])).asInstanceOf[Paths[Es, okay.core.Row]]

/** a value, needing nothing — at any effects */
inline def pure[A, Es[_]](a: A): A ! Es = Run.of(Cont.Return[OnePure, Nothing, A](a))
/** an operation: a program needing its effect, performed at the effect's key */
inline def effect[E[_], X](op: E[X])(using k: KeyOf[E]): X ! E = Run.of(Cont.perform[E, OnePure, X](op)(using k.in))
/** `p` built when the run gets to it: a tail call in constant stack */
inline def defer[A, Es[_]](p: => A ! Es): A ! Es = Run.of(Cont.Defer[OnePure, Nothing, Nothing, A](() => Run.kernel(p)))
inline def tailcall[A, Es[_]](p: => A ! Es): A ! Es = defer(p)

/** A HANDLER AS A VALUE, of `E`, polymorphic in the body's value: its answer `O[A]` — the kernel's form at a row */
trait Handler[E[_], O[_]]:
  def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, O[A]]

object Handler:
  /** each operation ANSWERED in place by `value`: nothing captured (the kernel's `Answering`) */
  def answering[E[_]](answer: [X] => E[X] => X): Handler[E, [A] =>> A] = new Handler[E, [A] =>> A]:
    def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, A] = new okay.one.handler.Answering[E, T, A, A]:
      def ret(a: A) = Cont.pure(a)
      def value[X](op: E[X]): X = answer(op)
  /** a STATE from `s0`: each operation its value and the next state (the kernel's `Stateful`) */
  def state[E[_], S](s0: S)(step: [X] => (S, E[X]) => (S, X)): Handler[E, [A] =>> (S, A)] = new Handler[E, [A] =>> (S, A)]:
    def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, (S, A)] = new okay.one.handler.Stateful[E, T, S, A, (S, A)]:
      // the machine asks `value`, then `next`, of one operation: the step once, kept between the two
      private var lastOp: AnyRef | Null = null
      private var lastStep: (S, Any) | Null = null
      private def stepped[X](s: S, op: E[X]): (S, X) =
        val l = lastStep
        if (lastOp eq op.asInstanceOf[AnyRef]) && l != null then
          lastOp = null; lastStep = null; l.asInstanceOf[(S, X)]
        else
          val r = step(s, op); lastOp = op.asInstanceOf[AnyRef]; lastStep = r; r
      def initial = s0
      def ret(s: S, a: A) = Cont.pure((s, a))
      def value[X](s: S, op: E[X]): X = stepped(s, op)._2
      def next[X](s: S, op: E[X]): S = stepped(s, op)._1
  /** a clause given `k`, the rest under the handler — dropped, resumed, resumed again (the kernel's `Deep`) */
  def deep[E[_], O[_]](ret0: [A] => A => O[A])(clause: [A, X] => (E[X], X => O[A] ! Pure) => O[A] ! Pure): Handler[E, O] =
    new Handler[E, O]:
      def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, O[A]] = new okay.one.handler.Deep[E, T, A, O[A]]:
        def ret(a: A) = Cont.pure(ret0(a))
        def apply[X](op: E[X], k: X => okay.one.![O[A], T]) =
          Run.kernel[O[A], Pure, T](clause[A, X](op, x => Program.lift(k(x))))
  /** an operation ENDS the body: its answer in the body's place, the rest dropped (the kernel's `Aborting`) */
  def aborting[E[_], O[_]](ret0: [A] => A => O[A])(stop: [A, X] => E[X] => O[A]): Handler[E, O] = new Handler[E, O]:
    def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, O[A]] = new okay.one.handler.Aborting[E, T, A, O[A]]:
      def ret(a: A) = Cont.pure(ret0(a))
      def abort[X](op: E[X]) = Cont.pure(stop[A, X](op))

/** `Es` AMONG `Es2`: proved at compile time, nothing at run time — a program needing `Es` is one needing `Es2` */
sealed abstract class Within[Es[_], Es2[_]]
object Within:
  private object It extends Within[[x] =>> Any, [x] =>> Any]
  /** the one evidence (it holds nothing) */
  def it[Es[_], Es2[_]]: Within[Es, Es2] = It.asInstanceOf[Within[Es, Es2]]
  def same[Es[_]]: Within[Es, Es] = it
  def none[Es2[_]]: Within[Pure, Es2] = it
  def pair[F[_], G[_], Es2[_]](f: Within[F, Es2], g: Within[G, Es2]): Within[okay.core.+[F, G], Es2] = it
  /** proved where it is asked for */
  inline given derived[Es[_], Es2[_]]: Within[Es, Es2] =
    inline if Evidence.among[Es, Es2] then it[Es, Es2]
    else scala.compiletime.error("an effect of the program is not among the effects expected")

/** the effects of a bind: the continuation's if it has the program's among them, the program's if it has the
 * continuation's, else the two summed — at compile time; nothing at run time */
sealed abstract class Join[Es[_], Es3[_]]:
  type Out[_]
object Join extends JoinSum:
  private object It extends Join[[x] =>> Any, [x] =>> Any]:
    type Out[x] = Any
  private def it[Es[_], Es3[_], O[_]]: Join[Es, Es3] { type Out[x] = O[x] } =
    It.asInstanceOf[Join[Es, Es3] { type Out[x] = O[x] }]
  def same[Es[_]]: Join[Es, Es] { type Out[x] = Es[x] } = it[Es, Es, Es]
  def sum[Es[_], Es3[_]]: Join[Es, Es3] { type Out[x] = Both[Es, Es3, x] } = it[Es, Es3, [x] =>> Both[Es, Es3, x]]
  def within[Es[_], Es3[_]](w: Within[Es, Es3]): Join[Es, Es3] { type Out[x] = Es3[x] } = it[Es, Es3, Es3]
  def contains[Es[_], Es3[_]](w: Within[Es3, Es]): Join[Es, Es3] { type Out[x] = Es[x] } = it[Es, Es3, Es]
  /** the same effects */
  given sameGiven[Es[_]]: (Join[Es, Es] { type Out[x] = Es[x] }) = same[Es]
  /** the program's effects among the continuation's */
  given withinGiven[Es[_], Es3[_]](using a: Evidence.All[Es, Es3]): (Join[Es, Es3] { type Out[x] = Es3[x] }) =
    it[Es, Es3, Es3]

trait JoinSum extends JoinSumLow:
  /** the continuation's effects among the program's (a `! Pure` continuation, or one needing fewer) */
  given containsGiven[Es[_], Es3[_]](using a: Evidence.All[Es3, Es]): (Join[Es, Es3] { type Out[x] = Es[x] }) =
    Join.contains[Es, Es3](Within.it)

trait JoinSumLow:
  /** else the two summed */
  given sumGiven[Es[_], Es3[_]]: (Join[Es, Es3] { type Out[x] = Both[Es, Es3, x] }) = Join.sum[Es, Es3]

/** THE EVIDENCE OF ROWS, decided at compile time: membership by givens of no value */
object Evidence:
  import scala.compiletime.summonFrom
  /** `E` among the effects `Es`: decided, nothing at run time */
  sealed trait Among[E[_], Es[_]]
  object Among extends AmongLeft:
    inline given here[E[_]]: Among[E, E] = null
  trait AmongLeft extends AmongRight:
    inline given left[E[_], F[_], G[_]](using inline a: Among[E, F]): Among[E, okay.core.+[F, G]] = null
  trait AmongRight:
    inline given right[E[_], F[_], G[_]](using inline a: Among[E, G]): Among[E, okay.core.+[F, G]] = null
  /** every effect of `Es` among `Es2`, as an ordinary given — found or not, so a priority can fall through */
  sealed trait All[Es[_], Es2[_]]
  object All extends AllSum:
    given same[Es[_]]: All[Es, Es] = null
    given none[Es2[_]]: All[Pure, Es2] = null
  trait AllSum extends AllOne:
    given sum[F[_], G[_], Es2[_]](using All[F, Es2], All[G, Es2]): All[okay.core.+[F, G], Es2] = null
  trait AllOne:
    given one[E[_], Es2[_]](using Among[E, Es2]): All[E, Es2] = null
  /** a sum taken apart: its sides, as types */
  sealed trait Sum[Es[_]]:
    type L[_]
    type R[_]
  object Sum:
    inline given both[F[_], G[_]]: (Sum[okay.core.+[F, G]] { type L[x] = F[x]; type R[x] = G[x] }) = null

  /** every effect of `Es` among `Es2` */
  transparent inline def among[Es[_], Es2[_]]: Boolean = summonFrom {
    case _: (Es[Any] =:= Es2[Any]) => true
    case _: (Es[Any] =:= NoEffects[Any]) => true
    case s: Sum[Es] => among[s.L, Es2] && among[s.R, Es2]
    case _: Among[Es, Es2] => true
    case _ => false
  }
  inline def within[Es[_], Es2[_]]: Within[Es, Es2] = Within.derived[Es, Es2]

/** a program needing fewer effects where one needing more is expected: itself */
given widen[A, Es[_], Es2[_]](using w: Within[Es, Es2]): Conversion[A ! Es, A ! Es2] = p => p.at[Es2]

/** THE TYPECLASSES of a program, by import (`import okay.Programs.given`): NOT in the program's implicit scope, where
 * the Monad's own `flatMap` (one row) would be found beside the program's (rows joined) */
object Programs:
  /** a program needing `Es` is a Monad (generic `traverse`, `sequence`): the bind keeps the effects */
  given monad[Es[_]]: Monad[[A] =>> A ! Es] with
    def pure[A](a: A): A ! Es = okay.pure(a)
    extension [A](a: A ! Es)
      def flatMap[B](f: A => B ! Es): B ! Es = Run.flatMap(a)(f)(using Join.same[Es])
  /** a loop in the program: each turn deferred, the recursion in the run's binds, never on the caller's stack */
  given tailRecM[Es[_]]: TailRecM[[A] =>> A ! Es] with
    def tailRecM[A, B](a: A)(f: A => (Either[A, B] ! Es)): B ! Es =
      Run.flatMap(f(a))(e => e match
        case Left(a2) => defer(tailRecM(a2)(f))
        case Right(b) => okay.pure[B, Es](b))(using Join.same[Es])
