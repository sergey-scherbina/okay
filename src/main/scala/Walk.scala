package okay

import okay.one.Cont
import okay.one.row.{In, Pure as OnePure}
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
 * place it goes on from where it was; run anywhere else, its operations are performed again at their places —
 * by key, the same in every program.
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
  def next[E[_], A, Es[_]](p: A ! Es)(using s: okay.core.Split[Es, E], k: KeyOf[E]): Step[E, A, Es] ! s.Rest =
    Run.of(Cont.defer(walk[Es, E, A](start(p), k.ref)))

  /** `p`, needing `E` alone, at its next operation: no effect of its own */
  def view[E[_], A](p: A ! E)(using k: KeyOf[E]): Step[E, A, E] = viewed[E, A](start(p), k.ref)

  /**
   * `p` INTO ANOTHER MONAD, each operation by `nt` (okay-freer's `foldMap`): the one interop word — a program of `E`
   * as a cats `IO`, a ZIO, a kyo computation, a `Future`. A loop in `G`'s `tailRecM` (okay-freer's foldMap took it
   * too): stack-safe for any `G` that has one, a strict `Option` as a lazy `IO` (`TailRecM.deferring` for those).
   */
  def foldMap[E[_], A, G[_]](p: A ! E)(nt: [X] => E[X] => G[X])(using G: Monad[G], R: TailRecM[G], k: KeyOf[E]): G[A] =
    R.tailRecM[A ! E, A](p)(q => view(q) match
      case Done(a) => G.pure(Right(a))
      case o: Op[E, x, A, E] => G.fmap(nt(o.op), (y: x) => Left(o.resume(y))))

  // ---- the machinery (okay-std's Writer streams walk by it too)

  /** a program paused at an operation, its machine-side types unnamed */
  type Paused[A] = Head.Paused[OnePure, A, ?, ?, ?, ?, ?, ?, ?]

  /** `at` resumed with `v`. THE CAST, and why it is sound: `v` is the paused operation's answer, of its type — the
   * caller read the operation and answered it */
  def resumeAt[A](at: Paused[A], v: Any): Head[OnePure, A] =
    at.asInstanceOf[Head.Paused[OnePure, A, [X] =>> Any, Any, Any, Any, Any, Any, Any]].resume(v)

  /** the operation at `in` the walked effect's — its key `key` — or not */
  inline def own(in: In[?, ?], key: AnyRef): Boolean = in match
    case at: In.At[?, ?] => at.ref eq key
    case _ => false

  /** the place of an operation, at the row of any program: a place by key is in every row */
  def placed[F[_]](in: In[F, OnePure]): In[F, OnePure] = in match
    case at: In.At[F, ?] => at.again[OnePure]
    case _ => throw IllegalStateException("Walk: an operation at a place not by key")

  private def walk[Es[_], E[_], A](st: Head[OnePure, A], key: AnyRef): okay.one.![Step[E, A, Es], OnePure] =
    st match
      case Head.Done(a) => Cont.pure(Done(a))
      case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] =>
        if own(p.in, key) then Cont.pure(op[Es, E, A, x](p.op.asInstanceOf[E[x]], p))
        // another effect's: performed here, the walk on with its answer — a step of the machine, not a host frame
        else Cont.perform[f, OnePure, x](p.op)(using placed(p.in)).flatMap(v => walk(p.resume(v), key))

  private def viewed[E[_], A](st: Head[OnePure, A], key: AnyRef): Step[E, A, E] = st match
    case Head.Done(a) => Done(a)
    case p: Head.Paused[?, ?, ?, x, ?, ?, ?, ?, ?] =>
      if own(p.in, key) then op[E, E, A, x](p.op.asInstanceOf[E[x]], p)
      else throw IllegalStateException(s"Walk.view: an operation of another effect than the walked one: ${p.op}")

  private def op[Es[_], E[_], A, X](o: E[X], p: Paused[A]): Op[E, X, A, Es] =
    Op[E, X, A, Es](o, x => rest[Es, A](() => resumeAt(p, x)), e => rest[Es, A](() => p.fail(e)))

  /**
   * THE REST of a walked program, past an operation it handed over: a program again — `next` the paused program
   * resumed, run when the rest is (as often as it is). Walked again (`start`), it goes on from where it was; run
   * anywhere else, its operations are performed again at their places, by key.
   */
  final class Rest[A](val next: () => Head[OnePure, A]) extends (() => okay.one.![A, OnePure]):
    def apply(): okay.one.![A, OnePure] = replay(next())

  def rest[Es[_], A](next: () => Head[OnePure, A]): A ! Es = Run.of(Cont.Defer(Rest(next)))

  private def replay[A](st: Head[OnePure, A]): okay.one.![A, OnePure] = st match
    case Head.Done(a) => Cont.pure(a)
    case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] =>
      Cont.perform[f, OnePure, x](p.op)(using placed(p.in)).flatMap(v => replay(p.resume(v)))

  /** `p` at its head: a rest walked before, on from where it was; any other program from its start */
  def start[A, Es[_]](p: A ! Es): Head[OnePure, A] = Run.kernel[A, Es, OnePure](p) match
    case Cont.Defer(r: Rest[A] @unchecked) => r.next()
    case e => Machine.head(e)
