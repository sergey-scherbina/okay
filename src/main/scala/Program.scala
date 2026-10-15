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

/** `A ! Es`: a program of `A` needing `Es` — a value */
infix type ![+A, Es[_]] = Run[Es, A]

/** `A !! Es`: the facade's program, a context function of the place — the fast form, no methods outside a program */
infix type !![+A, Es[_]] = okay.core.![A, Es]

/** no effects: a program needing nothing, run by `run` */
type Pure[X] = NoEffects[X]

object Program:
  /** from the facade's form: the same program, a value */
  def of[A, Es[_]](q: A !! Es): A ! Es = new Run[Es, A]:
    def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = q(using Needs.of(ps))

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
  /** the facade's form of the same program */
  def ctx: A !! Es = (c: Needs[Es]) ?=> p.fit(c.paths)

extension [A](p: A ! Pure)
  /** a program needing nothing: its value */
  def run: A = p.fit[okay.core.Pure](Paths.none).run

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
