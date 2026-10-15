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

/** A BIND, kept as one node: fitted at a place it is the kernel's bind; a chain nested to the LEFT (a `foldLeft` of
 * binds) is fitted by a loop down its left spine (`Run.fitLeft`), not a host frame a bind */
private[okay] final class Bound[Es[_], Es3[_], A, B, O[_]](val p: A ! Es, val f: A => B ! Es3, val j: Join[Es, Es3])
    extends Run[O, B]:
  def on[R <: okay.core.Row](ps: Paths[O, R]): Eff[R, B] = p match
    case _: (Bound[?, ?, ?, ?, ?] | Mapped[?, ?, ?]) => Run.fitLeft(this, ps)
    case _ =>
      // unchecked: `O` is `j.Out`, the type this node was made at
      val out = ps.asInstanceOf[Paths[j.Out, R]]
      val rs = j.right(out)
      p.fit(j.left(out)).flatMap(a => f(a).fit(rs))

/** a map, kept as one node, fitted as `Bound` */
private[okay] final class Mapped[Es[_], A, B](val p: A ! Es, val f: A => B) extends Run[Es, B]:
  def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, B] = p match
    case _: (Bound[?, ?, ?, ?, ?] | Mapped[?, ?, ?]) => Run.fitLeft(this, ps)
    case _ => p.fit(ps).map(f)

/** the words on a program, found with it (its implicit scope): no import */
object Run:
  private type K[x] = Any
  /** A LEFT-NESTED CHAIN FITTED BY A LOOP: down the left spine of binds and maps, each one's continuation fitted at
   * its own paths and kept; the leftmost program fitted; then the kept continuations bound back on, innermost first.
   * THE CASTS, and why they are sound: each node is taken apart at the type it was made at — its `Join`'s paths
   * applied to the paths the node was fitted at — so the program below and its continuation get their own */
  private[okay] def fitLeft[R <: okay.core.Row, B](top: Run[?, B], ps: Paths[?, R]): Eff[R, B] =
    val kept = java.util.ArrayList[AnyRef]()
    var cur: Run[?, ?] = top
    var cps: Paths[?, R] = ps
    var more = true
    while more do cur match
      case b: Bound[?, ?, ?, ?, ?] =>
        val bb = b.asInstanceOf[Bound[K, K, Any, Any, K]]
        val out = cps.asInstanceOf[Paths[K, R]]
        val rs = bb.j.right(out.asInstanceOf[Paths[bb.j.Out, R]])
        val f = bb.f
        kept.add((a: Any) => f(a).fit(rs))
        cps = bb.j.left(out.asInstanceOf[Paths[bb.j.Out, R]])
        cur = bb.p
      case m: Mapped[?, ?, ?] =>
        kept.add(MapK(m.asInstanceOf[Mapped[K, Any, Any]].f))
        cur = m.p
      case _ => more = false
    var e: Eff[R, Any] = cur.asInstanceOf[Run[K, Any]].on(cps.asInstanceOf[Paths[K, R]])
    var i = kept.size - 1
    while i >= 0 do
      e = kept.get(i) match
        case MapK(f) => e.map(f)
        case k => e.flatMap(k.asInstanceOf[Any => Eff[R, Any]])
      i -= 1
    e.asInstanceOf[Eff[R, B]]
  private final case class MapK(f: Any => Any)

  extension [A, Es[_]](p: A ! Es)
    /** fitted to a place: the program at its row */
    inline def fit[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = p.on(ps)
    /** the bind: the continuation's effects joined to these (`Join`) */
    def flatMap[B, Es3[_]](f: A => B ! Es3)(using j: Join[Es, Es3]): B ! j.Out = Bound[Es, Es3, A, B, j.Out](p, f, j)
    def map[B](f: A => B): B ! Es = Mapped[Es, A, B](p, f)
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
          val fps = Sides.left(ps)
          val rps = Sides.right(ps)
          okay.one.Cont.handle(p.fit(s.paths(rps)))(new okay.one.handler.Translating[E, R, A, A]:
            def ret(a: A) = okay.one.Cont.pure(a)
            def apply[X](op: E[X]): Eff[R, X] = f(op).fit(fps))
    /** `E` TRANSLATED INTO `F`, an effect the program needs already: each operation of `E` a program needing `F`,
     * run where it is performed — the program needing the rest, `F` among it (a `Once` answered by `Async` in a
     * program that has `Async`: one place, not two) */
    def translateInto[E[_], F[_]](f: [X] => E[X] => X ! F)(using s: Split[Es, E], w: Within[F, s.Rest]): A ! s.Rest =
      new Run[s.Rest, A]:
        def on[R <: okay.core.Row](ps: Paths[s.Rest, R]): Eff[R, A] =
          val fps = w.paths(ps)
          okay.one.Cont.handle(p.fit(s.paths(ps)))(new okay.one.handler.Translating[E, R, A, A]:
            def ret(a: A) = okay.one.Cont.pure(a)
            def apply[X](op: E[X]): Eff[R, X] = f(op).fit(fps))
    /** an error thrown in `p` given to `f`: its program in `p`'s place (the kernel's guard, inside the handlers `p`
     * is under — a failed operation raised where it was is seen here) */
    def recover[B >: A](f: Throwable => B ! Es): B ! Es = new Run[Es, B]:
      def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, B] =
        okay.one.Cont.recover(p.fit(ps): Eff[R, B])(e => f(e).fit(ps))
    /** `fin` after `p`, whatever way it is left: an end, an error, an abort, a cancel */
    def ensure(fin: Unit ! Es): A ! Es = new Run[Es, A]:
      def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = okay.one.Cont.ensure(p.fit(ps))(fin.fit(ps))
    /** a cancel at a step of `p` kept for when it is left */
    def uncancelable: A ! Es = new Run[Es, A]:
      def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, A] = okay.one.Cont.uncancelable(p.fit(ps))
    /** the facade's form of the same program */
    def ctx: A !! Es = (c: Needs[Es]) ?=> p.fit(c.paths)
    /** RUN with each effect ANSWERED in place by its `Answers` (okay-freer's `runWith`): one place and one
     * `Answering` handler an effect — a sum's `Answers` assembled from its parts' (`Answers.sum`) */
    def runWith(using a: Answers[Es]): A = Program.runAll(p, a)

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
  private type K[x] = Any
  /** `p` run, each effect answered by its part of `a`: the parts in order, the first at the head of the row */
  private[okay] def runAll[A, Es[_]](p: A ! Es, a: Answers[Es]): A = a match
    case Answers.NoneA => p.asInstanceOf[A ! Pure].run
    case _: Answers.Sum[?, ?] =>
      val leaves = scala.collection.mutable.ArrayBuffer[Answers[K]]()
      // BOUNDED by the sum as written in the program's type: one call a `+`
      def paths(x: Answers[?]): Paths[K, okay.core.Row] = x match
        case Answers.NoneA => Paths.none.asInstanceOf[Paths[K, okay.core.Row]]
        case s: Answers.Sum[?, ?] => Paths.Both(paths(s.left), paths(s.right)).asInstanceOf[Paths[K, okay.core.Row]]
        case l =>
          leaves += l.asInstanceOf[Answers[K]]
          var in: okay.core.In[K, ?] = okay.one.row.In.Here[K, okay.core.Row]()
          var i = leaves.size - 1
          while i > 0 do { in = okay.one.row.In.There(in.asInstanceOf[okay.core.In[K, okay.core.Row]]); i -= 1 }
          Paths.One(in.asInstanceOf[okay.core.In[K, okay.core.Row]])
      val ps = paths(a)
      var e: Eff[okay.core.Row, A] = p.fit(ps.asInstanceOf[Paths[Es, okay.core.Row]])
      for l <- leaves do e = handleHead[K, okay.core.Row, A](e.asInstanceOf[Eff[K +: okay.core.Row, A]], l)
      e.asInstanceOf[Eff[okay.core.Pure, A]].run
    case leaf =>
      val e = p.fit(Paths.One(okay.one.row.In.Here[Es, okay.core.Pure]()))
      handleHead[Es, okay.core.Pure, A](e, leaf).run
  /** the effect at the head answered in place */
  private def handleHead[E[_], T <: okay.core.Row, A](e: Eff[E +: T, A], a: Answers[E]): Eff[T, A] =
    okay.one.Cont.handle(e)(new okay.one.handler.Answering[E, T, A, A]:
      def ret(x: A) = okay.one.Cont.pure(x)
      def value[X](op: E[X]): X = a.handle(op))
  /** A PROGRAM OF THE KERNEL at the row `E` alone, as the root's value needing `E`: run by the machine to each
   * operation, which is performed at the place it is fitted to (the walk's rest, re-placed) — the bridge for code
   * written at a kernel row (`okay.one`) where a root program is wanted */
  def fromKernel[E[_], A](e: Eff[E +: okay.core.Pure, A]): A ! E =
    Walk.Rest[E, E, okay.core.Pure, A](Paths.One(okay.one.row.In.Here[E, okay.core.Pure]()),
      () => okay.one.machine.Machine.head(e))
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

/** `Es` AMONG `Es2`: the paths of `Es` from the paths of `Es2`. Made where it is needed, at COMPILE time: the place
 * of an effect in a sum is a constant, a path of left and right steps (`Code`, interned — no object a use); a sum's
 * a pair of its sides'. So a bind, a widening, a program placed in a bigger row allocate no evidence */
abstract class Within[Es[_], Es2[_]]:
  def paths[R <: okay.core.Row](ps: Paths[Es2, R]): Paths[Es, R]
object Within:
  private type K[x] = Any
  /** the same effects */
  private object Same extends Within[K, K]:
    def paths[R <: okay.core.Row](ps: Paths[K, R]): Paths[K, R] = ps
  /** no effects, among any */
  private object NoneW extends Within[K, K]:
    def paths[R <: okay.core.Row](ps: Paths[K, R]): Paths[K, R] = Paths.none.asInstanceOf[Paths[K, R]]
  /** one effect at the path `code`: its steps from the high bit down, below a leading 1; 0 left, 1 right */
  private final class Code(val code: Long) extends Within[K, K]:
    private val top = 62 - java.lang.Long.numberOfLeadingZeros(code)
    def paths[R <: okay.core.Row](ps: Paths[K, R]): Paths[K, R] =
      var p: Paths[?, R] = ps
      var i = top
      while i >= 0 do
        p = (if ((code >>> i) & 1L) == 0L then Sides.left(p.asInstanceOf[Paths[okay.core.+[K, K], R]])
             else Sides.right(p.asInstanceOf[Paths[okay.core.+[K, K], R]]))
        i -= 1
      p.asInstanceOf[Paths[K, R]]
  private final class Pair(f: Within[K, K], g: Within[K, K]) extends Within[K, K]:
    def paths[R <: okay.core.Row](ps: Paths[K, R]): Paths[K, R] =
      Paths.Both(f.paths(ps), g.paths(ps)).asInstanceOf[Paths[K, R]]
  private val codes: Array[Code] = Array.tabulate(1024)(i => Code(i.toLong))

  def same[Es[_]]: Within[Es, Es] = Same.asInstanceOf[Within[Es, Es]]
  def none[Es2[_]]: Within[Pure, Es2] = NoneW.asInstanceOf[Within[Pure, Es2]]
  def code[E[_], Es2[_]](c: Long): Within[E, Es2] =
    (if c < 1024 then codes(c.toInt) else Code(c)).asInstanceOf[Within[E, Es2]]
  def pair[F[_], G[_], Es2[_]](f: Within[F, Es2], g: Within[G, Es2]): Within[okay.core.+[F, G], Es2] =
    Pair(f.asInstanceOf[Within[K, K]], g.asInstanceOf[Within[K, K]]).asInstanceOf[Within[okay.core.+[F, G], Es2]]

  /** made at compile time where it is asked for */
  inline given derived[Es[_], Es2[_]]: Within[Es, Es2] = Evidence.within[Es, Es2]

/** the effects of a bind: the continuation's if it has the program's among them, else the two summed */
sealed abstract class Join[Es[_], Es3[_]]:
  type Out[_]
  def left[R <: okay.core.Row](ps: Paths[Out, R]): Paths[Es, R]
  def right[R <: okay.core.Row](ps: Paths[Out, R]): Paths[Es3, R]
object Join extends JoinSum:
  private type K[x] = Any
  private object SameJ extends Join[K, K]:
    type Out[x] = Any
    def left[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = ps
    def right[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = ps
  private object SumJ extends Join[K, K]:
    type Out[x] = Both[K, K, x]
    def left[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = Sides.left(ps)
    def right[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = Sides.right(ps)
  private final class WithinJ(w: Within[K, K]) extends Join[K, K]:
    type Out[x] = Any
    def left[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = w.paths(ps)
    def right[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = ps
  private val withins = new java.util.concurrent.ConcurrentHashMap[Within[?, ?], WithinJ]()

  def same[Es[_]]: Join[Es, Es] { type Out[x] = Es[x] } = SameJ.asInstanceOf[Join[Es, Es] { type Out[x] = Es[x] }]
  def sum[Es[_], Es3[_]]: Join[Es, Es3] { type Out[x] = Both[Es, Es3, x] } =
    SumJ.asInstanceOf[Join[Es, Es3] { type Out[x] = Both[Es, Es3, x] }]
  /** `Es` among `Es3`: the continuation's effects — one per evidence, kept */
  def within[Es[_], Es3[_]](w: Within[Es, Es3]): Join[Es, Es3] { type Out[x] = Es3[x] } =
    withins.computeIfAbsent(w, ww => WithinJ(ww.asInstanceOf[Within[K, K]])).asInstanceOf[Join[Es, Es3] { type Out[x] = Es3[x] }]

  private final class ContainsJ(w: Within[K, K]) extends Join[K, K]:
    type Out[x] = Any
    def left[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = ps
    def right[R <: okay.core.Row](ps: Paths[Out, R]): Paths[K, R] = w.paths(ps)
  private val containers = new java.util.concurrent.ConcurrentHashMap[Within[?, ?], ContainsJ]()
  /** `Es3` among `Es`: the program's effects — one per evidence, kept */
  def contains[Es[_], Es3[_]](w: Within[Es3, Es]): Join[Es, Es3] { type Out[x] = Es[x] } =
    containers.computeIfAbsent(w, ww => ContainsJ(ww.asInstanceOf[Within[K, K]])).asInstanceOf[Join[Es, Es3] { type Out[x] = Es[x] }]

  /** the same effects, the same tree */
  given sameGiven[Es[_]]: (Join[Es, Es] { type Out[x] = Es[x] }) = same[Es]
  /** the program's effects among the continuation's: the evidence made at compile time, the `Join` kept per evidence */
  given withinGiven[Es[_], Es3[_]](using a: Evidence.All[Es, Es3], w: Within[Es, Es3]): (Join[Es, Es3] { type Out[x] = Es3[x] }) =
    within(w)

trait JoinSum extends JoinSumLow:
  /** the continuation's effects among the program's (a `! Pure` continuation, or one needing fewer): the program's */
  given containsGiven[Es[_], Es3[_]](using a: Evidence.All[Es3, Es], w: Within[Es3, Es]): (Join[Es, Es3] { type Out[x] = Es[x] }) =
    Join.contains(w)

trait JoinSumLow:
  /** else the two summed */
  given sumGiven[Es[_], Es3[_]]: (Join[Es, Es3] { type Out[x] = Both[Es, Es3, x] }) = Join.sum[Es, Es3]

/** THE EVIDENCE OF ROWS, decided at compile time: membership by givens of no value (`Among`, `Sum`), the place of an
 * effect a constant (`code`) */
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
  /** the path of `E` in `Es`, `acc` the steps so far */
  inline def code[E[_], Es[_]](acc: Long): Long = summonFrom {
    case _: (Es[Any] =:= E[Any]) => acc
    case s: Sum[Es] => inline if among[E, s.L] then code[E, s.L](acc << 1) else code[E, s.R]((acc << 1) | 1L)
  }
  inline def within[Es[_], Es2[_]]: Within[Es, Es2] = summonFrom {
    case _: (Es[Any] =:= Es2[Any]) => Within.same[Es].asInstanceOf[Within[Es, Es2]]
    case _: (Es[Any] =:= NoEffects[Any]) => Within.none[Es2].asInstanceOf[Within[Es, Es2]]
    case s: Sum[Es] => Within.pair[s.L, s.R, Es2](within[s.L, Es2], within[s.R, Es2]).asInstanceOf[Within[Es, Es2]]
    case _ => inline if among[Es, Es2] then Within.code[Es, Es2](code[Es, Es2](1L))
              else scala.compiletime.error("an effect of the program is not among the effects expected")
  }

/** THE PATHS OF A PART of the effects, by the sides of `Both` itself. Not `ps.sub(r)` alone: that ends by asking the
 * part for itself by `Req.Only`, and a part of no effects (`Paths.None`: a program needing nothing, a runner's rest
 * when it took the only effect) refuses it with a `MatchError` (okay-core's `Paths.None.sub`) */
private[okay] object Sides:
  /** `ps.sub(r)`, a part of no effects answered with itself: the walk as deep as `r`, a sum written in a type */
  def sub[E[_], Es[_], R <: okay.core.Row](ps: Paths[Es, R], r: Req[E, Es]): Paths[E, R] = r match
    case _: Req.Only[e] => ps
    case l: Req.Left[e, f, g] => sub(left[f, g, R](ps), l.r)
    case g: Req.Right[e, f, h] => sub(right[f, h, R](ps), g.r)
  // unchecked: the `Both` of `Paths[F + G, R]` is made only as `Paths.Both(left: Paths[F, R], right: Paths[G, R])`
  def left[F[_], G[_], R <: okay.core.Row](ps: Paths[okay.core.+[F, G], R]): Paths[F, R] = ps match
    case b: Paths.Both[F, G, R] @unchecked => b.left
    case _ => ps.sub(Req.Left[F, F, G](Req.Only[F]()))
  def right[F[_], G[_], R <: okay.core.Row](ps: Paths[okay.core.+[F, G], R]): Paths[G, R] = ps match
    case b: Paths.Both[F, G, R] @unchecked => b.right
    case _ => ps.sub(Req.Right[G, F, G](Req.Only[G]()))

/** a program needing fewer effects where one needing more is expected: each found among them */
given widen[A, Es[_], Es2[_]](using w: Within[Es, Es2]): Conversion[A ! Es, A ! Es2] = _.at[Es2]

/** THE TYPECLASSES of a program, by import (`import okay.Programs.given`): NOT in `Run`'s implicit scope, where the
 * Monad's own `flatMap` (one row) would be found beside `Run.flatMap` (rows joined) and take a bind away from it */
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
