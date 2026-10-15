package okay

import okay.core.{Both, Eff, NoEffects, Needs, Paths, Req, Split}
import okay.one.handler.Handler as OneHandler
import scala.language.implicitConversions

/**
 * THE PLATFORM'S PROGRAM, A VALUE (specs/freer-on-core.md): `A ! Es`, a program of `A` needing the effects `Es`
 * (`Reader % Int + Writer % String`, in any order, among any others) — the words okay-freer had (`flatMap`, `map`,
 * `p.handle(h)`, `p.run`, `p.at[Es2]`), over okay-core's one monad: a program is a function of the PATHS of its
 * effects in the row of the place it is fitted to, one object a node, and at a place it is the kernel's `Eff`, run
 * by the machine. The effects of a bind are the continuation's if it has the program's among them, else the two
 * summed (`Join`); a program needing fewer effects fits where more are expected (`widen`, each found by its `Req`); a
 * runner takes its effect off wherever it is (`Split`).
 *
 * `A !! Es` is the facade's program, a CONTEXT FUNCTION of the place (`okay.core.!`): no object a node, a bind as the
 * kernel's — about twice as fast on binds — but no method can be called on it outside a program (it is applied
 * first): `handle(h)(p)`, `run(p)`. The two meet both ways: `p.ctx`, `Program.of(q)`.
 */
abstract class Run[Es[_], +A]:
  /** the program at the paths of a place */
  def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A]

/** the words on a program, found with it (its implicit scope): no import */
object Run:
  extension [A, Es[_]](p: A ! Es)
    /** fitted to a place: the program at its row */
    inline def fit[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = p.on(ps)
    /** the bind: the continuation's effects joined to these (`Join`) */
    def flatMap[B, Es3[_]](f: A => B ! Es3)(using j: Join[Es, Es3]): B ! j.Out = new Run[j.Out, B]:
      def on[R <: okay.core.Row](ps: Paths[j.Out, R]): Eff[R, B] =
        val rs = j.right(ps)
        p.fit(j.left(ps)).flatMap(a => f(a).fit(rs))
    def map[B](f: A => B): B ! Es = new Run[Es, B]:
      def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, B] = p.fit(ps).map(f)
    /** `p` then `q`, `p`'s value dropped */
    def >>[B, Es3[_]](q: => B ! Es3)(using j: Join[Es, Es3]): B ! j.Out = p.flatMap(_ => q)
    /** the same program needing more effects: `Es` among `Es2` */
    def at[Es2[_]](using w: Within[Es, Es2]): A ! Es2 = new Run[Es2, A]:
      def on[R <: okay.core.Row](ps: Paths[Es2, R]): Eff[R, A] = p.fit(w.paths(ps))
    /** `h`'s effect taken off wherever it is among `Es`: a program needing the rest */
    def handle[E[_], O[_]](h: Handler[E, O])(using s: Split[Es, E]): O[A] ! s.Rest = new Run[s.Rest, O[A]]:
      def on[R <: okay.core.Row](ps: Paths[s.Rest, R]): Eff[R, O[A]] = okay.one.Cont.handle(p.fit(s.paths(ps)))(h.kernel[R, A])
    /** `h` INTERCEPTING `E` for this program: `E` among `Es` and kept there — `h` answers the program's operations of
     * `E`, its own clauses' go to the handler of `E` outside (okay-core's `Removed.again`: `local`, `handleWith`) */
    def intercept[E[_]](h: Handler[E, [x] =>> x])(using w: Within[E, Es]): A ! Es = new Run[Es, A]:
      def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] =
        okay.one.Cont.handle(p.fit(ps))(h.kernel[R, A])(using okay.one.row.Removed.again(w.paths(ps).find(Req.only)))
    /** `E` TRANSLATED: each of `p`'s operations of `E` a program needing `F`, run where it is performed, the rest
     * resumed with its value — wherever `E` is among `Es` (the kernel's `Translating`) */
    def translate[E[_], F[_]](f: [X] => E[X] => X ! F)(using s: Split[Es, E]): A ! okay.core.+[F, s.Rest] =
      new Run[okay.core.+[F, s.Rest], A]:
        def on[R <: okay.core.Row](ps: Paths[okay.core.+[F, s.Rest], R]): Eff[R, A] =
          val fps = ps.sub(Req.Left[F, F, s.Rest](Req.Only[F]()))
          val rps = ps.sub(Req.Right[s.Rest, F, s.Rest](Req.Only[s.Rest]()))
          okay.one.Cont.handle(p.fit(s.paths(rps)))(new okay.one.handler.Translating[E, R, A, A]:
            def ret(a: A) = okay.one.Cont.pure(a)
            def apply[X](op: E[X]): Eff[R, X] = f(op).fit(fps))
    /** the facade's form of the same program */
    def ctx: A !! Es = (c: Needs[Es]) ?=> p.fit(c.paths)

  extension [A](p: A ! Pure)
    /** a program needing nothing: its value */
    def run: A = p.fit[okay.core.Pure](Paths.none).run



/** `A ! Es`: a program of `A` needing `Es` — a value */
infix type ![+A, Es[_]] = Run[Es, A]

/** `A !! Es`: the facade's program, a context function of the place — the fast form, no methods outside a program */
infix type !![+A, Es[_]] = okay.core.![A, Es]

/** no effects: a program needing nothing, run by `run` */
type Pure[X] = NoEffects[X]

object Program:
  /** a program of the kernel at a row, seen as needing nothing more than that row holds — a `k`'s answer inside a
   * clause, which the clause only binds and returns */
  private[okay] def lift[A, T <: okay.core.Row](e: Eff[T, A]): A ! Pure = new Run[Pure, A]:
    def on[R <: okay.core.Row](ps: Paths[Pure, R]): Eff[R, A] = e.asInstanceOf[Eff[R, A]]
  /** from the facade's form: the same program, a value */
  def of[A, Es[_]](q: A !! Es): A ! Es = new Run[Es, A]:
    def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = q(using Needs.of(ps))

/** a value, needing nothing — at any effects */
def pure[A, Es[_]](a: A): A ! Es = new Run[Es, A]:
  def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = okay.one.Cont.pure[R, A](a)
/** an operation: a program needing its effect */
def effect[E[_], X](op: E[X]): X ! E = new Run[E, X]:
  def on[R <: okay.core.Row](ps: Paths[E, R]): Eff[R, X] = okay.one.Cont.perform[E, R, X](op)(using ps.find(Req.only))
/** `p` built when the run gets to it: a tail call in constant stack */
def defer[A, Es[_]](p: => A ! Es): A ! Es = new Run[Es, A]:
  def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = okay.one.Cont.defer(p.fit(ps))
def tailcall[A, Es[_]](p: => A ! Es): A ! Es = defer(p)

/** A HANDLER AS A VALUE, of `E`, polymorphic in the body's value: its answer `O[A]` — the kernel's form at a row */
trait Handler[E[_], O[_]]:
  def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, O[A]]

object Handler:
  /** each operation ANSWERED in place by `value`: nothing captured (the kernel's `Answering`) */
  def answering[E[_]](answer: [X] => E[X] => X): Handler[E, [A] =>> A] = new Handler[E, [A] =>> A]:
    def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, A] = new okay.one.handler.Answering[E, T, A, A]:
      def ret(a: A) = okay.one.Cont.pure(a)
      def value[X](op: E[X]): X = answer(op)
  /** a STATE from `s0`: each operation its value and the next state; the answer the last state and the value — a
   * register of the machine, a `k` taken past it with the state as it was then (the kernel's `Stateful`) */
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
      def ret(s: S, a: A) = okay.one.Cont.pure((s, a))
      def value[X](s: S, op: E[X]): X = stepped(s, op)._2
      def next[X](s: S, op: E[X]): S = stepped(s, op)._1
  /** a clause given `k`, the rest under the handler — dropped, resumed, resumed again (the kernel's `Deep`); its
   * programs need nothing but the rest of the row */
  def deep[E[_], O[_]](ret0: [A] => A => O[A])(clause: [A, X] => (E[X], X => O[A] ! Pure) => O[A] ! Pure): Handler[E, O] =
    new Handler[E, O]:
      def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, O[A]] = new okay.one.handler.Deep[E, T, A, O[A]]:
        def ret(a: A) = okay.one.Cont.pure(ret0(a))
        def apply[X](op: E[X], k: X => okay.one.![O[A], T]) =
          clause[A, X](op, x => Program.lift(k(x))).fit(Paths.none.asInstanceOf[Paths[Pure, T]])
  /** an operation ENDS the body: its answer in the body's place, the rest dropped (the kernel's `Aborting`) */
  def aborting[E[_], O[_]](ret0: [A] => A => O[A])(stop: [A, X] => E[X] => O[A]): Handler[E, O] = new Handler[E, O]:
    def kernel[T <: okay.core.Row, A]: OneHandler[E, T, A, O[A]] = new okay.one.handler.Aborting[E, T, A, O[A]]:
      def ret(a: A) = okay.one.Cont.pure(ret0(a))
      def abort[X](op: E[X]) = okay.one.Cont.pure(stop[A, X](op))

/** `Es` AMONG `Es2`: the paths of `Es` from the paths of `Es2` — each effect found by its `Req` */
trait Within[Es[_], Es2[_]]:
  def paths[R <: okay.core.Row](ps: Paths[Es2, R]): Paths[Es, R]
object Within extends WithinTwo:
  given same[Es[_]]: Within[Es, Es] with
    def paths[R <: okay.core.Row](ps: Paths[Es, R]): Paths[Es, R] = ps
  /** no effects are among any */
  given none[Es2[_]]: Within[Pure, Es2] with
    def paths[R <: okay.core.Row](ps: Paths[Es2, R]): Paths[Pure, R] = Paths.none
trait WithinTwo extends WithinOne:
  given two[F[_], G[_], Es2[_]](using f: Within[F, Es2], g: Within[G, Es2]): Within[okay.core.+[F, G], Es2] with
    def paths[R <: okay.core.Row](ps: Paths[Es2, R]): Paths[okay.core.+[F, G], R] = Paths.Both(f.paths(ps), g.paths(ps))
trait WithinOne:
  given one[E[_], Es2[_]](using r: Req[E, Es2]): Within[E, Es2] with
    def paths[R <: okay.core.Row](ps: Paths[Es2, R]): Paths[E, R] = ps.sub(r)

/** the effects of a bind: the continuation's if it has the program's among them, else the two summed */
trait Join[Es[_], Es3[_]]:
  type Out[_]
  def left[R <: okay.core.Row](ps: Paths[Out, R]): Paths[Es, R]
  def right[R <: okay.core.Row](ps: Paths[Out, R]): Paths[Es3, R]
object Join extends JoinWithin:
  private type K[x] = Any
  private object SameJ extends Join[K, K]:
    type Out[x] = Any
    def left[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = ps
    def right[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = ps
  /** the same effects: one shared instance */
  given same[Es[_]]: (Join[Es, Es] { type Out[x] = Es[x] }) = SameJ.asInstanceOf[Join[Es, Es] { type Out[x] = Es[x] }]
trait JoinWithin extends JoinSum:
  given within[Es[_], Es3[_]](using w: Within[Es, Es3]): (Join[Es, Es3] { type Out[x] = Es3[x] }) = new Join[Es, Es3]:
    type Out[x] = Es3[x]
    def left[R <: okay.core.Row](ps: Paths[Es3, R]): Paths[Es, R] = w.paths(ps)
    def right[R <: okay.core.Row](ps: Paths[Es3, R]): Paths[Es3, R] = ps
trait JoinSum:
  given sum[Es[_], Es3[_]]: (Join[Es, Es3] { type Out[x] = Both[Es, Es3, x] }) = new Join[Es, Es3]:
    type Out[x] = Both[Es, Es3, x]
    def left[R <: okay.core.Row](ps: Paths[okay.core.+[Es, Es3], R]): Paths[Es, R] = ps.sub(Req.Left[Es, Es, Es3](Req.Only[Es]()))
    def right[R <: okay.core.Row](ps: Paths[okay.core.+[Es, Es3], R]): Paths[Es3, R] = ps.sub(Req.Right[Es3, Es, Es3](Req.Only[Es3]()))

/** a program needing fewer effects where one needing more is expected: each found among them */
given widen[A, Es[_], Es2[_]](using w: Within[Es, Es2]): Conversion[A ! Es, A ! Es2] = _.at[Es2]
