package okay.java

import okay.{Free, TypeableK, effect}
import java.util.function.{BiFunction, Function as JFunction, Supplier, UnaryOperator}
import Rows.{Top, answered}

/**
 * A CAPABILITY: the right to perform the operations of `O`, made by one
 * handler and handed to its body (specs/java-capabilities.md).
 *
 * Java cannot spell a row, but it can spell a method's PARAMETERS — so the
 * row becomes them: `static Eff<Integer> two(Counter c)` cannot be called
 * without a `Counter`, and a `Counter` exists only inside the handler that
 * made it. That is capability-passing style (Brachthäuser, Schuster,
 * Ostermann, "Effects as Capabilities", OOPSLA 2020; Effekt as a Scala
 * library, JFP 2020), here over the core's own handlers: an operation
 * performed through a capability is TAGGED with it, and the handler that
 * made it takes exactly its own operations — by the capability's identity,
 * not the operation's class, so two instances of one effect (two states,
 * two readers) are told apart, which a class test cannot do (`Distinct`).
 *
 * What Java cannot prevent is a capability leaving its scope (stored in a
 * field, returned in a lambda). An operation performed through it then
 * reaches no handler of its own, and `run()` refuses it by name.
 */
final class Cap[O] private[java] (cls: Class[O]) {

  /** perform one operation of this capability's effect; the handler that made it answers */
  def perform[R](op: Op[R]): Eff[R] =
    if !cls.isInstance(op) then
      throw IllegalArgumentException(s"okay.java: ${Rows.named(op)} is not an operation of $this")
    via[R](op)

  /** an operation tagged with this capability, at the answer type its maker states */
  private[java] def via[R](op: Any): Eff[R] = new Eff(effect[Top, R](Cap.Tagged(this, op)))

  override def toString: String = s"Cap(${cls.getName})@${Integer.toHexString(System.identityHashCode(this))}"
}

object Cap {

  /** an operation and the capability it was performed through */
  final class Tagged(val cap: Cap[?], val op: Any) {
    override def toString: String = s"$op via $cap"
  }

  /** this capability's operations, by identity */
  private def test(cap: Cap[?]): TypeableK[Top] = {
    case t: Tagged => t.cap eq cap
    case _ => false
  }

  /** the operation inside a tag, once `test` has said the tag is the handler's own */
  private def inner(x: Any): Any = x match
    case t: Tagged => t.op
    case other => throw IllegalStateException(s"okay.java: ${Rows.named(other)} reached a capability's handler untagged")

  /** a fresh capability for each run of the handler, handed to `body`, its operations taken by `handle` */
  private def scoped[O, A, B](cls: Class[O], body: JFunction[Cap[O], Eff[A]])
                             (handle: (TypeableK[Top], Eff[A]) => Eff[B]): Eff[B] =
    new Eff(Free.delay[Top, B] { () =>
      val cap = new Cap[O](cls)
      handle(test(cap), body.apply(cap)).program
    })

  // the four forms of `Handler`/`StateHandler`/`Control`, each with the capability it makes passed to `body`

  /** form 1: each operation answered by `f` */
  def answer[O, A](cls: Class[O], f: JFunction[? >: O, ?], body: JFunction[Cap[O], Eff[A]]): Eff[A] =
    scoped(cls, body)((t, e) => Handler.answerBy(t, op => f.apply(cls.cast(inner(op)))).run(e))

  /** form 2: a state threaded through the operations: `(s, op) -> Stated.of(s2, answer)` */
  def state[O, S, A](cls: Class[O], init: S, step: BiFunction[S, ? >: O, Stated[S, ?]],
                     body: JFunction[Cap[O], Eff[A]]): Eff[Stated[S, A]] =
    scoped(cls, body)((t, e) => StateHandler.stateBy[S](t, init, (s, op) => step.apply(s, cls.cast(inner(op)))).run(e))

  /** form 3: each operation a program in the capabilities in scope */
  def into[O, A](cls: Class[O], f: JFunction[? >: O, Eff[?]], body: JFunction[Cap[O], Eff[A]]): Eff[A] =
    scoped(cls, body)((t, e) => Handler.intoBy(t, op => f.apply(cls.cast(inner(op)))).run(e))

  /** form 4: the continuation in hand: `(op, k) -> k.resume(x)` once, twice or never */
  def control[O, A, B](cls: Class[O], ret: JFunction[? >: A, Eff[B]], clause: Clause[? >: O, B],
                       body: JFunction[Cap[O], Eff[A]]): Eff[B] =
    scoped(cls, body)((t, e) => new Control[A, B](t, ret, (op, k) => clause.apply(cls.cast(inner(op)), k)).run(e))
}

/**
 * A mutable cell as a capability: `Var.run(0, v -> v.modify(n -> n + 1))`.
 * Each operation is a step `S => S` whose result is both the new state and
 * the answer — `get` is the identity — so one handler clause serves all three.
 */
final class Var[S] private (cap: Cap[Var.Step]) {
  def get(): Eff[S] = cap.via(Var.Step(s => s))
  /** replace the state, answering the new one */
  def put(s: S): Eff[S] = cap.via(Var.Step(_ => s))
  /** apply `f` to the state, answering the new one */
  def modify(f: UnaryOperator[S]): Eff[S] = cap.via(Var.Step(s => f.apply(answered[S](s))))
}

object Var {
  private[java] final case class Step(f: Any => Any)

  /** the cell from `init`, its last state beside `body`'s answer */
  def run[S, A](init: S, body: JFunction[Var[S], Eff[A]]): Eff[Stated[S, A]] =
    Cap.state[Step, S, A](classOf[Step], init, (s, op) => { val n = answered[S](op.f(s)); Stated(n, n) },
      c => body.apply(new Var[S](c)))
}

/** an environment as a capability: `Env.run(cfg, env -> env.ask()…)` */
final class Env[E] private (cap: Cap[Env.Ask]) {
  def ask(): Eff[E] = cap.via(Env.ask)
}

object Env {
  private[java] final class Ask
  private val ask = new Ask

  /** every `ask` in `body` answered with `env` */
  def run[E, A](env: E, body: JFunction[Env[E], Eff[A]]): Eff[A] =
    Cap.answer[Ask, A](classOf[Ask], _ => env, c => body.apply(new Env[E](c)))
}

/** failure with an `E` as a capability: `Raise.recover(e -> fallback, err -> … err.raise(e) …)` */
final class Raise[E] private (cap: Cap[Raise.Failed]) {
  /** stop with `e`: the rest of the program up to `recover` is dropped */
  def raise[A](e: E): Eff[A] = cap.via(Raise.Failed(e))
}

object Raise {
  private[java] final case class Failed(e: Any)

  /** `body`'s answer, or `onError` of what it raised */
  def recover[E, A](onError: JFunction[? >: E, ? <: A], body: JFunction[Raise[E], Eff[A]]): Eff[A] =
    Cap.control[Failed, A, A](classOf[Failed], a => Eff.pure(a), (op, _) => Eff.pure(onError.apply(answered[E](op.e))),
      c => body.apply(new Raise[E](c)))
}

/**
 * The platform's `Async` as a capability, given only by `Io.run` — so a
 * program that sleeps or blocks says so in its parameters, and `Io.run` is
 * where it runs (on this thread, blocking).
 */
final class Io private () {
  /** `a.get()` when the program runs, possibly blocking */
  def async[A](a: Supplier[? <: A]): Eff[A] = Eff.async(a)
  /** park for `millis` on the platform timer */
  def sleep(millis: Long): Eff[Void] = Eff.sleep(millis)
}

object Io {
  def run[A](main: JFunction[Io, Eff[A]]): A = main.apply(new Io).runAsync()
}
