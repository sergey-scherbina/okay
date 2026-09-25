package okay2

import scala.annotation.tailrec

/**
 * Final tagless interface of delimited control: the parameterised
 * continuation monad with the shift operator of Danvy and Filinski,
 * with answer-type modification. M[A, S, R] means (A => S) => R, which
 * `run` eliminates.
 */
trait Control[M[_, _, _]] extends ParaMonad[M] {
  def shift[A, S, R](f: (A => S) => R): M[A, S, R]
  def run[A, S, R](m: M[A, S, R])(k: A => S): R
  def reset[A, R](m: M[A, A, R]): R = run(m)(identity)
}

object Control {
  def apply[M[_, _, _]](implicit C: Control[M]): Control[M] = C

  /** the stack-safe data instance: the default carrier */
  implicit val cont: Control[Cont.Rep] = new Control[Cont.Rep] {
    def pure[A, R](a: A): Cont[A, R, R] = Cont.Pure(a)
    def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shift(f)
    def flatMap[A, B, S, S2, R](m: Cont[A, S, R])(f: A => Cont[B, S2, S]): Cont[B, S2, R] = Cont.bind(m)(f)
    // absorbed in its own right, not `flatMap(pure)`: see `Leaf.Mapped`
    override def map[A, B, S, R](m: Cont[A, S, R])(f: A => B): Cont[B, S, R] = Cont.mapped(m)(f)
    def run[A, S, R](m: Cont[A, S, R])(k: A => S): R = Cont.run(m)(k)
  }

  /** the reference function instance: fast, fused, not stack-safe */
  implicit val func: Control[Func] = new Control[Func] {
    def pure[A, R](a: A): Func[A, R, R] = k => k(a)
    def shift[A, S, R](f: (A => S) => R): Func[A, S, R] = f
    def flatMap[A, B, S, S2, R](m: Func[A, S, R])(f: A => Func[B, S2, S]): Func[B, S2, R] = k => m(a => f(a)(k))
    override def map[A, B, S, R](m: Func[A, S, R])(f: A => B): Func[B, S, R] = k => m(x => k(f(x)))
    def run[A, S, R](m: Func[A, S, R])(k: A => S): R = m(k)
  }
}

/**
 * The parameterised continuation monad, as a FACADE over the freer
 * tree: `Rep[A, S, R]` computes A and, applied by `run` to a
 * continuation A => S, makes an answer R. `S` and `R` are PHANTOM to
 * the tree — it is `Free[Shift, A]`, and answer-type modification
 * lives entirely in the signatures of this module.
 *
 * `Rep` is ABSTRACT here, and `okay2.Cont` is a value of this type:
 * that is Scala 2's opaque type. Only `ContImpl` sees `Free[Shift, A]`
 * under it, so nothing outside can put a leaf in a `Cont` or match its
 * tree — the discipline the Scala 3 core's `opaque type Rep` enforces.
 */
sealed abstract class ContModule {
  type Rep[A, S, R]

  /** a finished value */
  def Pure[A, R](a: A): Rep[A, R, R]

  /** a computation as a function of its continuation — the shift of Danvy and Filinski */
  def shift[A, S, R](f: (A => S) => R): Rep[A, S, R]

  /** a bind whose LEFT side is deferred into the runner's own loop:
   * mutual tail recursion without a JVM frame per call */
  def defer[A, B, S, T, R](thunk: () => Rep[A, T, R])(f: A => Rep[B, S, T]): Rep[B, S, R]

  /** `defer` with nothing to do afterwards */
  def delay[A, S, R](thunk: () => Rep[A, S, R]): Rep[A, S, R]

  /** flatMap, in prefix form */
  def bind[A, B, S, S2, R](c: Rep[A, S, R])(f: A => Rep[B, S2, S]): Rep[B, S2, R]

  /** map, absorbed in its own right */
  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R]

  /** apply to a continuation, as the function (A => S) => R it means */
  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R

  /** is this program already an ANSWER? A handler that does not
   * capture builds exactly `Pure`, and a caller that can go on from
   * the answer with a tail call then needs no trampoline node at all
   * (`Effects.handle`). Two methods rather than one taking the two
   * branches as functions: the Scala 3 core's `onAnswer` is inline, so
   * its branch IS the caller's tail call; a branch passed as a closure
   * is a frame per handled operation, measured as a stack overflow at
   * 1M (specs/okay2.md). */
  def isAnswer[A, S](c: Rep[A, S, S]): Boolean

  /** the answer of a program `isAnswer` said yes to */
  def answerOf[A, S](c: Rep[A, S, S]): A
}

private[okay2] object ContImpl extends ContModule {
  import Free.{Return, Inject, Bind, Delay}

  /**
   * The leaf, as the tree stores it: a shift with its answer types
   * FORGOTTEN. `(X => S) => R <: (X => Nothing) => Any` for every S and
   * R, so this is the one supertype every typed shift conforms to.
   * `Shift.at` is THE cast back: the facade typed this leaf when it was
   * built and every combinator since has threaded those types through
   * its own signature, so a runner handed a `Rep[A, S, R]` and a
   * `k: A => S` knows the leaf it reaches is the function the facade
   * said it was. Erased on the JVM, costs nothing.
   */
  sealed trait Shift extends Row { type Op[+X] = (X => Nothing) => Any }

  private def at[X, S, R](s: Shift#Op[X])(k: X => S): R = s.asInstanceOf[(X => S) => R](k)

  type Rep[A, S, R] = Free[Shift, A]

  def Pure[A, R](a: A): Rep[A, R, R] = Return(a)

  def shift[A, S, R](f: (A => S) => R): Rep[A, S, R] = Inject[Shift, A](f.asInstanceOf[Shift#Op[A]])

  def defer[A, B, S, T, R](thunk: () => Rep[A, T, R])(f: A => Rep[B, S, T]): Rep[B, S, R] = Free.defer(thunk)(f)

  def delay[A, S, R](thunk: () => Rep[A, S, R]): Rep[A, S, R] = Free.delay(thunk)

  /**
   * A leaf that has ALREADY absorbed one continuation. Absorption is a
   * single bit, so the state is the CASE. WHY EXACTLY ONE absorption
   * (the Scala 3 core measured it, history.tsv `fuse0-*`, `fuse1-*`):
   * absorption itself pays 12–25%, one step is the whole of that, and
   * depth COSTS — each further step nests one more closure call per
   * run.
   */
  private sealed abstract class Leaf[A, S, R] extends ((A => S) => R) {
    /** a leaf called by the user's own function: the runner's room when
     * `k` is one of its continuations, the first room otherwise */
    def apply(k: A => S): R = k match {
      case r: Reentry[_, _, _, _] => applyAt(k, r.room)
      case _ => applyAt(k, StackSwitch.firstRoom)
    }
    /** the leaf re-enters the runner with the room the runner has left
     * (specs/cont-stack.md Layer 2) */
    def applyAt(k: A => S, room: Int): R
  }
  /** flatMap's absorption: the continuation enters the leaf */
  private final case class Absorbed[A, B, S, T, R](s: Shift#Op[A], g: A => Rep[B, S, T]) extends Leaf[B, S, R] {
    def applyAt(k: B => S, room: Int): R = at[A, T, R](s)(new Reentry[A, B, S, T](g, k, room - 1))
  }
  /** the same for `map`, its own case rather than `Absorbed` over
   * `a => Return(f(a))`: that spelling allocates a `Return` per element
   * at RUN time */
  private final case class Mapped[A, B, S, R](s: Shift#Op[A], g: A => B) extends Leaf[B, S, R] {
    def applyAt(k: B => S, room: Int): R = at[A, S, R](s)(a => callK(k, g(a), room - 1))
  }

  def bind[A, B, S, S2, R](c: Rep[A, S, R])(f: A => Rep[B, S2, S]): Rep[B, S2, R] = c match {
    case Inject(s) => s match {
      // already absorbed one — see `Leaf` for why never twice
      case _: Leaf[_, _, _] => Bind(c, f)
      case _ => Inject[Shift, B](Absorbed[A, B, S2, S, R](shiftOp[A](s), f))
    }
    // Pure receivers build a node too: fusing `pure(a).flatMap(f)` at
    // CONSTRUCTION would run `def forever = pure(()).flatMap(_ =>
    // forever)` at construction and diverge
    case _ => Bind(c, f)
  }

  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] = c match {
    case Inject(s) => s match {
      case _: Leaf[_, _, _] => Bind(c, (a: A) => Return[Shift, B](f(a)))
      case _ => Inject[Shift, B](Mapped[A, B, S, R](shiftOp[A](s), f))
    }
    case _ => Bind(c, (a: A) => Return[Shift, B](f(a)))
  }

  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R = step(c)(k)(StackSwitch.firstRoom)

  /**
   * What a run's stack looked like at its last GRANT (specs/cont-stack.md
   * Layer 3): the stack it was on, the pointer then, the levels granted,
   * the most bytes one level has taken in this run. Attached at the
   * root of the continuation chain on the run's first exhaustion, never
   * allocated before — the Scala 3 core's `Gauge`, in Scala 2.
   */
  private[okay2] final class Gauge {
    var top: Long = 0L
    var mark: Long = 0L
    var granted: Int = 0
    var worst: Long = StackSwitch.coldBytesPerLevel
  }

  /** the chain's root once it has a gauge: the user's `k` and the gauge */
  private final class Gauged[B, S](val k: B => S, val gauge: Gauge) extends (B => S) {
    def apply(b: B): S = k(b)
  }

  /** the gauge behind a continuation: the root's, attached now if it
   * has none; a fresh unattached one for a chain with no `Reentry` at
   * all (conservative, never wrong) */
  @tailrec private def gaugeOf(k: Any): Gauge = k match {
    case r: Reentry[_, _, _, _] => r.k match {
      case inner: Reentry[_, _, _, _] => gaugeOf(inner)
      case _ => r.gauge
    }
    case g: Gauged[_, _] => g.gauge
    case _ => new Gauge
  }

  /**
   * THE CONTINUATION A SHIFT'S BODY RECEIVES, when calling it re-enters
   * this runner — and the room left on this stack, as a FIELD (no
   * ThreadLocal). At zero the stack is asked how much it really has
   * (`StackSwitch.more`), and only a stack with nothing left switches.
   * The rare road is `exhausted`, out of `enter` so `enter` inlines.
   */
  private final class Reentry[X, B, S, T](f: X => Rep[B, S, T], var k: B => S, val room: Int) extends (X => T) {
    def apply(x: X): T = enter(x, room)

    def enter(x: X, here: Int): T = {
      val r = if (here < room) here else room
      if (r > 0) step(f(x))(k)(r) else exhausted(x)
    }

    private def exhausted(x: X): T = {
      val more = StackSwitch.more(gaugeOf(k))
      if (more > 0) step(f(x))(k)(more)
      else StackSwitch.fresh(fresh => step(f(x))(k)(fresh))
    }

    /** this chain root's gauge, attached on the first ask */
    def gauge: Gauge = k match {
      case g: Gauged[_, _] => g.gauge
      case _ =>
        val g = new Gauge
        k = new Gauged(k, g)
        g
    }
  }

  /** call a continuation from inside the runner, with the room HERE */
  private def callK[A, S](k: A => S, a: A, room: Int): S = k match {
    case r: Reentry[_, _, _, _] => r.asInstanceOf[Reentry[A, Any, Any, S]].enter(a, room) // a Reentry[X, ..., T] IS an X => T: the class test says so, the erased indexes do not
    case _ => k(a)
  }

  def isAnswer[A, S](c: Rep[A, S, S]): Boolean = c match {
    case Return(_) => true
    case _ => false
  }

  def answerOf[A, S](c: Rep[A, S, S]): A = c match {
    case Return(a) => a
    case other => throw new IllegalStateException("answerOf on a program that is not an answer: " + other)
  }

  /** a `Return` reached through a `Rep[A, S, R]` was built by `Pure[A,
   * R']`, whose signature is `Rep[A, R', R']` — so the facade already
   * fixed S = R' = R, and the tree, which keeps no answer type, cannot
   * say it. `Shift.at`'s claim at the other node. */
  private def pinned[S, R](s: S): R = s.asInstanceOf[R]

  /** the operation of a Cont tree, at its type: `Inject` holds it as
   * `Any` since stage 8 (a row's `#Op` is not a type to read at), and
   * a Cont tree's row is the single signature `Shift`, so every
   * operation in it IS a `Shift#Op` — nothing else is ever injected */
  private def shiftOp[A](s: Any): Shift#Op[A] = s.asInstanceOf[Shift#Op[A]]

  /**
   * The loop: rotation and elimination interleaved. The single
   * non-tail case re-enters through `run`, because a shift's body may
   * invoke its continuation, and that frame is direct style's own cost.
   * The inner `run`'s answer type is PINNED to `Any`: left to
   * inference scalac 2 makes it `Nothing` and puts a `checkcast
   * Nothing$` on the call, which throws (measured, specs/okay2.md).
   */
  /** `at`, with the runner's room handed to a leaf: a leaf re-enters
   * the runner through ITS continuation, so the room must reach it
   * here (the Scala 3 core's `leafAt`) */
  private def leafAt[X, S, R](s: Shift#Op[X], k: X => S, room: Int): R = s match {
    case l: Leaf[_, _, _] => l.asInstanceOf[Leaf[X, S, R]].applyAt(k, room)
    case _ => at[X, S, R](s)(k)
  }

  @tailrec private def step[A, S, R](c: Rep[A, S, R])(k: A => S)(room: Int): R = c match {
    case Return(a) => pinned[S, R](callK(k, a, room))
    case Inject(s) => leafAt[A, S, R](shiftOp[A](s), k, room)
    case Bind(Inject(s), f) => at[Any, R, R](shiftOp[Any](s))(new Reentry[Any, A, S, R](f, k, room - 1))
    case Bind(Bind(a, f), g) => step(Bind(a, (x: Any) => bind(f(x))(g)))(k)(room)
    case Bind(Return(a), f) => step(f(a))(k)(room)
    case Delay(t) => step(t())(k)(room)
    case Bind(Delay(t), g) => step(Bind(t(), g))(k)(room)
  }
}
