package okay.java

import okay.{Answers, Async, Foreign, TypeableK, typeableK}

import okay.freer.{Distinct, Classic, Row}
import okay.freer.{+}
import okay.freer.{!, Free, Member, effect}
import okay.given
import okay.freer.given
import java.util.function.{BiFunction, Function as JFunction, Supplier, UnaryOperator}

/**
 * okay programs, effects and handlers FROM JAVA (specs/java-effects.md).
 *
 * okay's row is a union of type constructors, which Java cannot spell, and
 * okay-scala2's way around it — a phantom intersection `Eff[R, A]` — is not
 * open to Java either: an intersection is legal there only as a bound,
 * never as a type argument. So a Java program is `Eff<A>`, stored at an
 * erased row as okay-scala2 stores its own, and the row is checked when
 * the program RUNS: an operation nothing handled is refused by name.
 * Everything else is the core's own machinery — the handlers below are
 * `Handler.answerOf`, `stateOf`, `intoOf` and `Effects.handle`, each told
 * which operations are its own by the `Class` a Java caller names.
 *
 * {{{
 * sealed interface Counter<R> extends Op<R> {
 *   record Next() implements Counter<Integer> {}
 * }
 * Eff<Integer> p = Eff.perform(new Counter.Next())
 *     .flatMap(a -> Eff.perform(new Counter.Next()).map(b -> a + b));
 * StateHandler<Integer> h = StateHandler.of(Counter.class, 0, (n, op) -> Stated.of(n + 1, n));
 * p.handle(h).run();                                  // Stated(2, 1)
 * }}}
 */

/** an operation of a Java effect, answering `R`: `record Next() implements Op<Integer> {}` */
trait Op[+R]

private[java] object Rows {

  /** the row every `Eff` is stored at */
  type Top[+X] = Any

  /**
   * THE ONE ROW CAST, made here once (okay-scala2's `Rows.coerce`, for the
   * same reason): Java has no row to follow, so a program is stored at
   * `Top` and re-typed at the concrete row a core handler takes. It is
   * right because `Free` never reads its row at run time — handlers split
   * operations by their class — and an operation nobody took is refused by
   * name when the program runs (`Eff.unhandled`).
   */
  def coerce[F[+_], G[+_], A](p: Free[F, A]): Free[G, A] = p.asInstanceOf[Free[G, A]]

  /**
   * A Java clause's answer at its operation's answer type. A Java lambda
   * cannot be polymorphic in `X`, so it answers `Object`; `X` is erased, so
   * this checks nothing, and a wrong answer is a ClassCastException where
   * the performing Java code uses it — as with any generic Java code.
   */
  def answered[X](a: Any): X = a.asInstanceOf[X]

  /** the test a handler splits by: the class its Java caller named */
  def test(cls: Class[?]): TypeableK[Top] = typeableK[Top](cls)

  /** every row here is distinct by construction: one Java handler takes one class */
  def distinct[F[+_]]: Distinct[F] = Distinct.unchecked[F]()

  def named(x: Any): String = if x == null then "null" else s"${x.getClass.getName} ($x)"
}

import Rows.{Top, answered, coerce}

/** a program answering `A`, performing operations nothing in Java's types records */
final class Eff[A] private[java] (private[java] val program: Free[Top, A]) {

  def map[B](f: JFunction[? >: A, ? <: B]): Eff[B] = new Eff(program.map(a => f.apply(a)))

  def flatMap[B](f: JFunction[? >: A, Eff[B]]): Eff[B] = new Eff(program.flatMap(a => f.apply(a).program))

  /** this, then `next`, this program's answer dropped */
  def andThen[B](next: Eff[B]): Eff[B] = new Eff(program.flatMap(_ => next.program))

  /** take one effect off: forms 1 and 3, the answer unchanged */
  def handle(h: Handler): Eff[A] = h.run(this)

  /** take one effect off, a state threaded through it: form 2 */
  def handle[S](h: StateHandler[S]): Eff[Stated[S, A]] = h.run(this)

  /** take one effect off with its continuation in hand: form 4 */
  def handle[B](h: Control[A, B]): Eff[B] = h.run(this)

  /** the core `Throws`: a `raise` answered by `f` of what was raised */
  def recover(f: JFunction[Any, ? <: A]): Eff[A] =
    new Eff(coerce(okay.std.runEither[A, Top, Any](coerce(program))(using Rows.distinct)).map(_.fold(e => f.apply(e), a => a)))

  /** every operation handled: the answer. One left is refused by name */
  def run(): A = program.runWith(using Eff.unhandled)

  /** only the core `Async` left: run it on this thread, blocking */
  def runAsync(): A = coerce[Top, Async + Top, A](program).runWith(using Eff.asyncOnly)

  /**
   * This program for Scala, in the row `F`: each operation is checked to be
   * one of F's as it is performed, and refused by name if not.
   */
  def toScala[F[+_]](using m: Member[F]): A ! F =
    // the context bound's evidence comes LAST, after the translation
    !.translate[A, Top, F](coerce(program))(using Rows.distinct)([X] => (e: Top[X]) =>
      m.operation(Foreign.obj(e)) match
        case Some(op) => effect[F, Any](op).map(answered[X])
        case None => throw IllegalArgumentException(s"okay.java: ${Rows.named(e)} is not an operation of the row"))(
      using Eff.everything)
}

object Eff {

  def pure[A](a: A): Eff[A] = new Eff(okay.freer.pure(a))

  /** perform one operation of a Java effect */
  def perform[R](op: Op[R]): Eff[R] = new Eff(coerce(effect[Op, R](op)))

  /**
   * `next()` when the program reaches here, not now: the tail call that keeps
   * a recursive Java program stack-safe — `loop(n)` returning
   * `Eff.defer(() -> loop(n - 1))` runs in constant stack at any depth.
   */
  def defer[A](next: Supplier[Eff[A]]): Eff[A] = new Eff(Free.delay(() => next.get().program))

  /** a Scala program, for Java: its operations are handled by Java's handlers or by `run`'s refusal */
  def from[A, F[+_]](p: A ! F): Eff[A] = new Eff(coerce(p))

  // ------------------------------------------------- the core effects

  /** `Reader`: the environment, which `Handler.reader(env)` supplies */
  def ask[E](): Eff[E] = from(okay.std.Reader.ask[E])

  /** `State`: the state, which `StateHandler.state(s0)` threads */
  def get[S](): Eff[S] = from(okay.std.State.get[S])

  /** `State`: replace it, answering the new state */
  def put[S](s: S): Eff[S] = from(okay.std.State.set(s))

  /** `State`: apply `f` to it, answering the new state */
  def modify[S](f: UnaryOperator[S]): Eff[S] = from(okay.std.State.modify[S](s => f.apply(s)))

  /** `Throws`: stop with `e`, which `recover` answers */
  def raise[A](e: Any): Eff[A] = from(okay.std.raise[Any, A](e))

  /** `Async`: `a.get()` when the program runs, possibly blocking */
  def async[A](a: Supplier[? <: A]): Eff[A] = from(okay.async[A](a.get()))

  /** `Async`: park for `millis` on the platform timer */
  def sleep(millis: Long): Eff[Void] = from(okay.Async.sleep(millis).map(_ => null: Void))

  // ------------------------------------------------- run's refusal

  /** what `run` answers an operation that reached it with: a refusal by name, never the operation as a value */
  private[java] val unhandled: Answers[Top] = new Answers[Top]:
    def handle[X](op: Top[X]): X = op match
      case t: Cap.Tagged => throw IllegalStateException(
        s"okay.java: ${t.op} was performed through ${t.cap} outside its handler — the capability escaped its scope")
      case _ => throw IllegalStateException(s"okay.java: no handler took ${Rows.named(op)}")

  private[java] val asyncOnly: Answers[Async + Top] =
    Row.union[Async, Top](using summon[TypeableK[Async]], summon[Answers[Async]], unhandled)(using Rows.distinct)

  /** `toScala` re-types every operation */
  private[java] val everything: TypeableK[Top] = _ => true
}

/** a state and the program's answer beside it */
final case class Stated[S, A](state: S, value: A)

object Stated {
  def of[S, A](state: S, value: A): Stated[S, A] = Stated(state, value)
}

/** a handler that keeps the program's answer: `Handler.answer`, `Handler.into`, `Handler.reader` */
final class Handler private (private val take: [A] => Free[Top, A] => Free[Top, A]) {
  private[java] def run[A](e: Eff[A]): Eff[A] = new Eff(take[A](e.program))
}

object Handler {

  /** form 1: each operation of `cls` answered by `f`, and the program goes on */
  def answer[O](cls: Class[O], f: JFunction[? >: O, ?]): Handler = answerBy(Rows.test(cls), op => f.apply(cls.cast(op)))

  /** form 3: each operation of `cls` becomes the program `f` makes of it, in whatever effects remain */
  def into[O](cls: Class[O], f: JFunction[? >: O, Eff[?]]): Handler = intoBy(Rows.test(cls), op => f.apply(cls.cast(op)))

  // the forms over any test: by class above, by a capability's identity in `Cap`; `f` reads the raw operation

  private[java] def answerBy(test: TypeableK[Top], f: Any => Any): Handler =
    val h = okay.freer.Handler.answerOf[Top]([X] => (op: Top[X]) => answered[X](f(op)))(using test)
    new Handler([A] => (p: Free[Top, A]) => coerce[Top, Top, A](
      h.run[A, Top](coerce(p))(using summon, Rows.distinct, okay.freer.Handler.Nothing.any)))

  private[java] def intoBy(test: TypeableK[Top], f: Any => Eff[?]): Handler =
    val h = okay.freer.Handler.intoOf[Top, Top]([X] => (op: Top[X]) => f(op).program.map(answered[X]))(using test)
    new Handler([A] => (p: Free[Top, A]) =>
      h.run[A, Top](coerce(p))(using summon, Rows.distinct, summon))

  /** the core `Reader`: every `Eff.ask()` answered with `env` */
  def reader[E](env: E): Handler =
    new Handler([A] => (p: Free[Top, A]) => coerce[Top, Top, A](okay.std.Reader.run[E, A, Top](env)(coerce(p))(using Rows.distinct)))
}

/** a handler threading a state `S`: the program's answer arrives beside the last state */
final class StateHandler[S] private (private val take: [A] => Free[Top, A] => Free[Top, (S, A)]) {
  private[java] def run[A](e: Eff[A]): Eff[Stated[S, A]] = new Eff(take[A](e.program).map((s, a) => Stated(s, a)))
}

object StateHandler {

  /** form 2: each operation of `cls` answered from the state, which it may replace: `(s, op) -> Stated.of(s2, answer)` */
  def of[O, S](cls: Class[O], init: S, step: BiFunction[S, ? >: O, Stated[S, ?]]): StateHandler[S] =
    stateBy(Rows.test(cls), init, (s, op) => step.apply(s, cls.cast(op)))

  private[java] def stateBy[S](test: TypeableK[Top], init: S, step: (S, Any) => Stated[S, ?]): StateHandler[S] =
    val h = okay.freer.Handler.stateOf[Top, S](init)([X] => (s: S, op: Top[X]) =>
      val next = step(s, op)
      (next.state, answered[X](next.value)))(using test)
    new StateHandler([A] => (p: Free[Top, A]) =>
      h.run[A, Top](coerce(p))(using summon, Rows.distinct, okay.freer.Handler.Nothing.any))

  /** the core `State`, from `init`: `Eff.get()`, `put`, `modify` */
  def state[S](init: S): StateHandler[S] =
    new StateHandler([A] => (p: Free[Top, A]) => okay.std.State.handle(init)[A, Top](coerce(p))(using Rows.distinct))
}

/** the rest of the program after an operation, as a `Control` clause holds it */
trait Resume[B] {
  /** answer the operation with `x` and run the rest; callable once, several times, or never */
  def resume(x: Any): Eff[B]
}

/** a `Control` clause: the operation and the rest of the program */
trait Clause[O, B] {
  def apply(op: O, k: Resume[B]): Eff[B]
}

/** form 4: a handler with the continuation in hand, its return clause turning the answer `A` into `B` */
final class Control[A, B] private[java] (test: TypeableK[Top], ret: JFunction[? >: A, Eff[B]], clause: Clause[Any, B]) {
  private[java] def run(e: Eff[A]): Eff[B] =
    val E = Classic[Free]
    new Eff(E.handle[Top, Top](using test)[A, B](coerce(e.program))(a => ret.apply(a).program)(
      [X] => (op: Top[X]) =>
        E.control.shift[X, Free[Top, B], Free[Top, B]](k => clause(op, x => new Eff(k(answered[X](x)))).program)))
}

object Control {

  /** `ret` for the program's answer, `clause` for each operation of `cls`: `(op, k) -> k.resume(true)` */
  def of[O, A, B](cls: Class[O], ret: JFunction[? >: A, Eff[B]], clause: Clause[? >: O, B]): Control[A, B] =
    new Control[A, B](Rows.test(cls), ret, (op, k) => clause.apply(cls.cast(op), k))
}
