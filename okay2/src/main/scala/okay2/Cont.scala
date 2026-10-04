package okay2

import scala.annotation.{tailrec, unused}
import scala.language.experimental.macros

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
    def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shiftLeaf(f)
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

  /** a computation as a function of its continuation — the shift of Danvy and Filinski. A MACRO
   * (cont-stack-okay2-macro): a tail-shaped body is its value, read when the runner reaches it (`tailShift`), any
   * other body the leaf (`shiftLeaf`). Inside okay2's own core, which a macro cannot expand in, write `shiftLeaf` */
  def shift[A, S, R](f: (A => S) => R): Rep[A, S, R] = macro ContMacro.shift[A, S, R]

  /** a body run as it is, its `k` the runner's continuation */
  def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R]

  /** a tail body `k => { stats; k(v) }` as its value `v`, evaluated when the runner reaches it; `S <: R` (the
   * evidence) makes the body's answer the shift's. Public for the macro's expansion; not an API */
  def tailShift[A, S, R](v: () => A)(implicit ev: S <:< R): Rep[A, S, R]

  /** the same with no thunk, for a literal */
  def tailPure[A, S, R](v: A)(implicit ev: S <:< R): Rep[A, S, R]

  /** an answer-using body after the macro's selective CPS transform (Layer 1 B): walked by the runner in its own
   * loop. Public for the macro's expansion; not an API */
  def cps[A, S, R](c: ContCps.Cps[A, S, R]): Rep[A, S, R]

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

  /** delimit: run the computation with the identity continuation */
  final def reset[A, R](c: Rep[A, A, R]): R = run(c)(identity)

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

  def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] = Inject[Shift, A](f.asInstanceOf[Shift#Op[A]])

  // the tree keeps no answer type (`Rep` is `Free[Shift, A]`), so the evidence is the facade's alone
  def tailShift[A, S, R](v: () => A)(implicit @unused ev: S <:< R): Rep[A, S, R] = Free.delay(() => Return[Shift, A](v()))

  def tailPure[A, S, R](v: A)(implicit @unused ev: S <:< R): Rep[A, S, R] = Return(v)

  // a `Cps` IS an `(A => S) => R`, so it is a leaf as `shiftLeaf` stores one; the runner tells it by its class
  def cps[A, S, R](c: ContCps.Cps[A, S, R]): Rep[A, S, R] = shiftLeaf[A, S, R](c)

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
      // a walked body is never absorbed: the runner walks it in-loop, a leaf would apply it as a function
      case _: Leaf[_, _, _] | _: ContCps.Cps[_, _, _] => Bind(c, f)
      case _ => Inject[Shift, B](Absorbed[A, B, S2, S, R](shiftOp[A](s), f))
    }
    // Pure receivers build a node too: fusing `pure(a).flatMap(f)` at
    // CONSTRUCTION would run `def forever = pure(()).flatMap(_ =>
    // forever)` at construction and diverge
    case _ => Bind(c, f)
  }

  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] = c match {
    case Inject(s) => s match {
      case _: Leaf[_, _, _] | _: ContCps.Cps[_, _, _] => Bind(c, (a: A) => Return[Shift, B](f(a)))
      case _ => Inject[Shift, B](Mapped[A, B, S, R](shiftOp[A](s), f))
    }
    case _ => Bind(c, (a: A) => Return[Shift, B](f(a)))
  }

  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R = step(c)(k)(StackSwitch.firstRoom)(NoPending)(null)

  /**
   * The explicit stack of pending body parts (Layer 1 B, the Scala 3 core's cont-stack-layer1-b): what is left to
   * do with the answer of a call of `k`, pushed when the call is made, popped and fed when the program's answer
   * arrives. `NoPending` is the empty stack; a run with no walked body never allocates one.
   */
  private final class Pending[S, R](val rest: S => ContCps.Body[R], val next: Pending[_, _]) {
    /** `Shift.at`'s claim once more: the answer the runner reached is the `S` the facade typed this part for */
    def deliver(s: Any): ContCps.Body[R] = rest(pinned[Any, S](s))
  }
  private val NoPending: Pending[_, _] = new Pending[Any, Nothing](_ => throw new IllegalStateException("empty"), null)

  /** the program and continuation a walk starts with — never looked at: a walked body answers through its pending
   * parts, and a program it continues carries its own */
  private val noProgram: Rep[Any, Any, Any] = Return(())
  private val noK: Any => Any = _ => throw new IllegalStateException("a walked body answers through its pending parts, never through k")

  /** the runner's loop from a body: what a `Cps` does when applied as the function it means */
  private[okay2] def walk[R](b: ContCps.Body[R]): R = step[Any, Any, R](noProgram)(noK)(StackSwitch.firstRoom)(NoPending)(b)

  /** a `Call` through the runner's own continuation: the rest of the program in-loop, at THIS stack's room. The
   * claim `callK` makes: a `Reentry[X, ..., T]` IS an `X => T`, and the call's argument is that `X` */
  private def reentryOf(k: Any): Reentry[Any, Any, Any, Any] = k.asInstanceOf[Reentry[Any, Any, Any, Any]]

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
  private final class Reentry[X, B, S, T](val f: X => Rep[B, S, T], var k: B => S, val room: Int) extends (X => T) {
    def apply(x: X): T = enter(x, room)

    def enter(x: X, here: Int): T = {
      val r = if (here < room) here else room
      if (r > 0) step(f(x))(k)(r)(NoPending)(null) else exhausted(x)
    }

    private def exhausted(x: X): T = {
      val more = StackSwitch.more(gaugeOf(k))
      if (more > 0) step(f(x))(k)(more)(NoPending)(null)
      else StackSwitch.fresh(fresh => step(f(x))(k)(fresh)(NoPending)(null))
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

  /**
   * WITH LAYER 1 B'S PENDING STACK: every exit that returned an answer — the `Return` case's `callK`, an opaque
   * leaf, an opaque body under a `Bind` — now hands it to the part on top when one is pending, and walks on. The
   * body being walked is the loop's last parameter (`null` when none), so the loop stays one `@tailrec` method: a
   * `Call` continues the program through the `Reentry`'s fields in-loop. A nested runner (`Reentry.enter`, an
   * opaque body's own call of `k`) starts with nothing pending and returns as before. Scala 2 has no `inline`, so
   * the hand-off is written out at each exit.
   */
  @tailrec private def step[A, S, R](c: Rep[A, S, R])(k: A => S)(room: Int)(pending: Pending[_, _])(b: ContCps.Body[_]): R =
    if (b ne null) b match {
      case ContCps.Done(r) =>
        if (pending eq NoPending) pinned[Any, R](r) else step[A, S, R](c)(k)(room)(pending.next)(pending.deliver(r))
      case call: ContCps.Call[_, _, _] => call.k match {
        case _: Reentry[_, _, _, _] =>
          val re = reentryOf(call.k)
          step[Any, Any, R](re.f(call.a))(re.k)(room)(new Pending(call.rest, pending))(null)
        case kk => step[A, S, R](c)(k)(room)(pending)(call.rest(kk(call.a)))
      }
    }
    else c match {
      case Return(a) =>
        val r = pinned[S, R](callK(k, a, room))
        if (pending eq NoPending) r else step[A, S, R](c)(k)(room)(pending.next)(pending.deliver(r))
      case Inject(s) => s match {
        case cps: ContCps.Cps[_, _, _] => step[A, S, R](c)(k)(room)(pending)(cps.walkWith(k))
        case _ =>
          val r = leafAt[A, S, R](shiftOp[A](s), k, room)
          if (pending eq NoPending) r else step[A, S, R](c)(k)(room)(pending.next)(pending.deliver(r))
      }
      case Bind(Inject(s), f) => s match {
        case cps: ContCps.Cps[_, _, _] => step[A, S, R](c)(k)(room)(pending)(cps.walkWith(new Reentry[Any, A, S, R](f, k, room - 1)))
        case _ =>
          val r = at[Any, R, R](shiftOp[Any](s))(new Reentry[Any, A, S, R](f, k, room - 1))
          if (pending eq NoPending) r else step[A, S, R](c)(k)(room)(pending.next)(pending.deliver(r))
      }
      case Bind(Bind(a, f), g) => step(Bind(a, (x: Any) => bind(f(x))(g)))(k)(room)(pending)(null)
      case Bind(Return(a), f) => step(f(a))(k)(room)(pending)(null)
      case Delay(t) => step(t())(k)(room)(pending)(null)
      case Bind(Delay(t), g) => step(Bind(t(), g))(k)(room)(pending)(null)
    }
}

/**
 * LAYER 1 B's DATA (cont-stack-okay2-macro, the Scala 3 core's cont-stack-layer1-b): a body that USES the answer of
 * `k` — `k(1) + k(10)`, `a :: k(x)`, a `val` bound to `k(x)` — after `ContMacro`'s selective CPS transform (Rompf,
 * Maier & Odersky, ICFP 2009). Every `k(e)` becomes a `Call` naming what is left, the body is then DATA the runner
 * walks in its own loop with the pending parts on an explicit stack, and a call of `k` continues the program
 * in-loop: no body frame, no room counted, no switch, `k` multi-shot as before. Public because the macro's
 * expansion at the user's call site builds it (an anonymous subclass per shift); not an API.
 */
object ContCps {
  sealed abstract class Body[R]
  /** the body's answer */
  final case class Done[R](r: R) extends Body[R]
  /** `k(a)`, then `rest` of the answer */
  final case class Call[A, S, R](k: A => S, a: A, rest: S => Body[R]) extends Body[R]

  /** a CPS-transformed body, as the `(A => S) => R` it still means */
  abstract class Cps[A, S, R] extends ((A => S) => R) {
    def body(k: A => S): Body[R]
    final def apply(k: A => S): R = ContImpl.walk(body(k))
    /** `Shift.at`'s door out, for the runner: the continuation it built for this leaf is the one the facade typed
     * the leaf with */
    private[okay2] def walkWith[X](k: X => Any): Body[_] = body(k.asInstanceOf[A => S])
  }
}
