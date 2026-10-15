package okay

import okay.core.{+:, Eff, Paths, Row, Split}
import okay.one.Cont
import okay.one.row.In
import okay.one.machine.Machine
import okay.one.machine.Machine.Head

/**
 * WALKING A PROGRAM, okay-freer's `resume` on the kernel (specs/freer-on-core-howto.md, "Walks over the tree"): a
 * program at its next operation of `E` — its value (`Done`), or the operation and THE REST, a program again
 * (`Op`). Code that walked the tree (`p.resume match { case Return(a) …; case Bind(Inject(e), k) … }`: a driver, an
 * interop, a stepped dialog, a merge) walks by this instead, one way for all of them:
 *
 *     def drive[A](p: A ! Async): … = Walk.view(p) match
 *       case Walk.Done(a) => …
 *       case o: Walk.Op[Async, x, A, Async] => o.op match
 *         case Async.Run(f) => drive(o.resume(f()))
 *
 * `next` walks `E` among other effects: their operations met on the way are performed where the walk is (the rest
 * of the place), as okay-freer's `relay` forwarded them. `view` walks a program needing `E` alone, with no effect
 * of its own. Underneath, the program is run by the machine to its next operation, handed out as ONE object
 * (`Machine.head`); the rest resumes it — any number of times, from any thread — and walked again at the same
 * place it goes on from where it was, nothing re-fitted; run anywhere else, its operations are re-placed by
 * reading the paths of the two places side by side.
 */
object Walk:
  /** the program at its next operation of `E` */
  sealed abstract class Step[E[_], A, Es[_]]
  /** it answered */
  final case class Done[E[_], A, Es[_]](value: A) extends Step[E, A, Es]
  /** it performs `op`; `resume(x)` is the rest given the answer, `fail(e)` the rest given a failure raised where the
   * operation was */
  final class Op[E[_], X, A, Es[_]] private[Walk] (val op: E[X], resume0: X => A ! Es, fail0: Throwable => A ! Es)
      extends Step[E, A, Es]:
    def resume(x: X): A ! Es = resume0(x)
    def fail(e: Throwable): A ! Es = fail0(e)
    override def toString: String = s"Walk.Op($op)"

  /** `p` to its next operation of `E`, wherever `E` is among its effects; the others' operations performed here */
  def next[E[_], A, Es[_]](p: A ! Es)(using s: Split[Es, E]): Step[E, A, Es] ! s.Rest = new Run[s.Rest, Step[E, A, Es]]:
    def on[T <: Row](ps: Paths[s.Rest, T]): Eff[T, Step[E, A, Es]] =
      val template = s.paths(ps)
      Cont.defer(walk(start(p, template), template))

  /** `p`, needing `E` alone, at its next operation: no effect of its own */
  def view[E[_], A](p: A ! E): Step[E, A, E] =
    val template = Paths.One(In.Here[E, okay.core.Pure]())
    viewed(start(p, template), template)

  // ---- the machinery (okay-std's Writer streams walk by it too)

  /** a program paused at an operation of `R`, its machine-side types unnamed */
  type Paused[R <: Row, A] = Head.Paused[R, A, ?, ?, ?, ?, ?, ?, ?]

  /** `at` resumed with `v`. THE CAST, and why it is sound: `v` is the paused operation's answer, of its type — the
   * caller read the operation and answered it */
  def resumeAt[R <: Row, A](at: Paused[R, A], v: Any): Head[R, A] =
    at.asInstanceOf[Head.Paused[R, A, [X] =>> Any, Any, Any, Any, Any, Any, Any]].resume(v)

  private def walk[Es[_], E[_], T <: Row, A](st: Head[E +: T, A], template: Paths[Es, E +: T]): Eff[T, Step[E, A, Es]] =
    st match
      case Head.Done(a) => Cont.pure(Done(a))
      case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] => own(p.in) match
        // another effect's: performed here, the walk on with its answer — a step of the machine, not a host frame
        case null => Cont.perform[f, T, x](p.op)(using outside(p.in)).flatMap(v => walk(p.resume(v), template))
        case same: In.Same[f, E] => Cont.pure(op[Es, E, T, A, x](same(p.op), p, template))

  private def viewed[E[_], A](st: Head[E +: okay.core.Pure, A], template: Paths[E, E +: okay.core.Pure]): Step[E, A, E] =
    st match
      case Head.Done(a) => Done(a)
      case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] => own(p.in) match
        case null => In.absurd(outside(p.in))
        case same: In.Same[f, E] => op[E, E, okay.core.Pure, A, x](same(p.op), p, template)

  private def op[Es[_], E[_], T <: Row, A, X](o: E[X], p: Paused[E +: T, A], template: Paths[Es, E +: T]): Op[E, X, A, Es] =
    Op[E, X, A, Es](o, x => Rest[Es, E, T, A](template, () => resumeAt(p, x)),
      e => Rest[Es, E, T, A](template, () => p.fail(e)))

  /** the operation at `in` the walked effect's own — its place the head — or `null` */
  def own[F[_], E[_], T <: Row](in: In[F, E +: T]): In.Same[F, E] | Null = In.same(in, In.Here[E, T]())

  /** a place past the head, in the row outside it */
  def outside[F[_], E[_], T <: Row](in: In[F, E +: T]): In[F, T] = in match
    case In.There(in2) => in2
    case at: In.At[f, ?] => at.again[T]
    case _ => throw IllegalStateException("Walk: an operation of the walked effect where another was expected")

  def moved(): Nothing = throw IllegalStateException(
    "Walk: the rest of a walked program run where its effects are not as they were where it was walked — " +
      "an effect of it not among those of the place; run the rest where the walk's answer is used")

  /**
   * THE REST of a program walked at the place `template`, past an operation it handed over: a program again —
   * `next` the paused program resumed, run when the rest is (as often as it is). Walked again at the same paths
   * (`start`), it goes on from where it was; run anywhere else, its operations are performed at the place's paths,
   * each re-placed by reading the two side by side (`relocate`).
   */
  final class Rest[Es[_], E[_], T <: Row, A](val template: Paths[Es, E +: T], next: () => Head[E +: T, A])
      extends Run[Es, A]:
    /** the rest stepped on at a place whose paths are the template's. THE CAST, and why it is sound: the program
     * reads its row only through its operations' paths, and those are the template's at the new place as well */
    def resumed[T2 <: Row]: Head[E +: T2, A] = next().asInstanceOf[Head[E +: T2, A]]

    def on[R <: Row](ps: Paths[Es, R]): Eff[R, A] = Cont.defer(replay(next(), ps))

    private def replay[R <: Row](st: Head[E +: T, A], ps: Paths[Es, R]): Eff[R, A] = st match
      case Head.Done(a) => Cont.pure(a)
      case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] =>
        Cont.perform[f, R, x](p.op)(using relocate(p.in, template, ps)).flatMap(v => replay(p.resume(v), ps))

  /** the same paths, leaf for leaf: a walk as deep as the sum of effects written in the program's type, a bound of
   * the source text */
  def samePaths(a: Paths[?, ?], b: Paths[?, ?]): Boolean = (a eq b) || ((a, b) match
    case (x: Paths.One[?, ?], y: Paths.One[?, ?]) => x.head == y.head
    case (x: Paths.Both[?, ?, ?], y: Paths.Both[?, ?, ?]) => samePaths(x.left, y.left) && samePaths(x.right, y.right)
    case (_: Paths.None[?], _: Paths.None[?]) => true
    case _ => false)

  /**
   * The place in `now` of the operation at `in` under `template`: the leaf of `template` whose path is `in`, read
   * at the same leaf of `now`. THE CAST, and why it is sound: the two are the paths of one `Es`, leaf for leaf, so
   * that leaf of `now` is the place of the same effect, `F`. The walk is as deep as the sum of effects written in
   * the program's type, a bound of the source text.
   */
  def relocate[F[_], Es[_], E[_], T <: Row, R <: Row](in: In[F, E +: T], template: Paths[Es, E +: T],
      now: Paths[Es, R]): In[F, R] =
    var found: In[?, ?] | Null = null
    def walk(t: Paths[?, ?], n: Paths[?, ?]): Unit = (t, n) match
      case (to: Paths.One[?, ?], no: Paths.One[?, ?]) => if found == null && to.head == in then found = no.head
      case (tb: Paths.Both[?, ?, ?], nb: Paths.Both[?, ?, ?]) => walk(tb.left, nb.left); walk(tb.right, nb.right)
      case _ => ()
    walk(template, now)
    found match
      case null => moved()
      case f => f.asInstanceOf[In[F, R]]

  /** `p` at its head at the place `template`: a rest walked at the same paths before, on from where it was; any
   * other program from its start */
  def start[Es[_], E[_], T <: Row, A](p: A ! Es, template: Paths[Es, E +: T]): Head[E +: T, A] = p match
    // unchecked: a `Rest` that is this `A ! Es` has its `Es` and `A`; its `E` is the template's head, which the
    // paths compared say
    case r: Rest[Es, E, ?, A] @unchecked if samePaths(r.template, template) => r.resumed[T]
    case _ => Machine.head(p.fit(template))
