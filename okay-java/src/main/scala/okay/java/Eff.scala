package okay.java

import okay.{!, +, %, Answers, Async, Erased, Foreign, Run, effect}
import okay.core.{Paths, Req}
import okay.std.{Reader, State, Throws}
import okay.given
import okay.one.row.In
import java.util.function.{BiFunction, Function as JFunction, Supplier, UnaryOperator}

/**
 * okay programs, effects and handlers FROM JAVA (specs/java-effects.md).
 *
 * okay's row is a union of type constructors, which Java cannot spell, and
 * okay-scala2's way around it — a phantom intersection `Eff[R, A]` — is not
 * open to Java either: an intersection is legal there only as a bound,
 * never as a type argument. So a Java program is `Eff<A>`, a program
 * needing ONE effect, `Top`, whose operations are any values, and the row
 * is checked when the program RUNS: an operation nothing handled is
 * refused by name. Everything else is the root's own words — a Java
 * handler translates `Top` into `Top` (`translate`), answering its own
 * operations and performing the others outside; a state or a core effect
 * first moves its own operations to an effect of their own beside `Top`
 * and is then the ordinary handler of it (`Handler.state`, `Reader.run`).
 * Each Java handler is told which operations are its own by the `Class` a
 * Java caller names.
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

  /** the one effect every `Eff` needs: its operations are any values. Opaque, so no evidence of rows mistakes it
   * for another effect (an alias of `Any` would equal every other such alias) */
  opaque type Top[X] = Any

  /** the operations a Java handler took off `Top`, as an effect of their own beside it (`Own + Top`) */
  opaque type Own[X] = Any

  /** a value performed as an operation of `Top`, answering `X` */
  def top[X](op: Any): Top[X] = op

  /** the value an operation of `Top` is */
  def raw[X](op: Top[X]): Any = op

  /** an operation of `Top` that a handler's test said is its own */
  def own[X](op: Top[X]): Own[X] = op

  /** the value an operation of `Own` is */
  def rawOwn[X](op: Own[X]): Any = op

  /**
   * A Java clause's answer at its operation's answer type. A Java lambda
   * cannot be polymorphic in `X`, so it answers `Object`; `X` is erased, so
   * this checks nothing, and a wrong answer is a ClassCastException where
   * the performing Java code uses it — as with any generic Java code.
   */
  def answered[X](a: Any): X = a.asInstanceOf[X]

  def named(x: Any): String = if x == null then "null" else s"${x.getClass.getName} ($x)"
}

import Rows.{Own, Top, answered, raw, rawOwn, top}

private[java] object Moves {

  /** a Java handler's own operations moved to `Own` beside `Top` (`test` says which); the others stay */
  def own[A](p: A ! Top, test: Any => Boolean): A ! Own + Top =
    p.translate[Top, Own + Top]([X] => (op: Top[X]) =>
      if test(raw(op)) then effect[Own, X](Rows.own(op)).at[Own + Top] else effect[Top, X](op).at[Own + Top]
    ).at[Own + Top]

  /** a Scala effect's operations moved to it beside `Top` (its `Member` says which); the others stay */
  def scala[E[_], A](p: A ! Top)(using m: Foreign.Member[E]): A ! E + Top =
    p.translate[Top, E + Top]([X] => (op: Top[X]) => m.operation(Foreign.obj(raw(op))) match
      case Some(q) => q.map(answered[X]).at[E + Top]
      case None => effect[Top, X](op).at[E + Top]
    ).at[E + Top]
}

/** a program answering `A`, performing operations nothing in Java's types records */
final class Eff[A] private[java] (private[java] val program: A ! Top) {

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
    new Eff(okay.std.runEither[A, Throws % Any + Top, Any](Moves.scala[Throws % Any, A](program))
      .map(_.fold(e => f.apply(e), a => a)))

  /** every operation handled: the answer. One left is refused by name */
  def run(): A = program.runWith(using Eff.unhandled)

  /** only the core `Async` left: run it on this thread, blocking */
  def runAsync(): A = program.runWith(using Eff.asyncOnly)

  /**
   * This program for Scala, in the row `F`: each operation is checked to be
   * one of F's as it is performed, and refused by name if not.
   */
  def toScala[F[_]](using m: Foreign.Member[F]): A ! F =
    program.translate[Top, F]([X] => (op: Top[X]) => m.operation(Foreign.obj(raw(op))) match
      case Some(q) => q.map(answered[X])
      case None => throw IllegalArgumentException(s"okay.java: ${Rows.named(raw(op))} is not an operation of the row")
    ).at[F]
}

object Eff {

  def pure[A](a: A): Eff[A] = new Eff(okay.pure[A, Top](a))

  /** perform one operation of a Java effect */
  def perform[R](op: Op[R]): Eff[R] = new Eff(effect[Top, R](top[R](op)))

  /**
   * `next()` when the program reaches here, not now: the tail call that keeps
   * a recursive Java program stack-safe — `loop(n)` returning
   * `Eff.defer(() -> loop(n - 1))` runs in constant stack at any depth.
   */
  def defer[A](next: Supplier[Eff[A]]): Eff[A] = new Eff(okay.defer(next.get().program))

  /** a Scala program, for Java: its operations are handled by Java's handlers or by `run`'s refusal */
  def from[A, F[_]](p: A ! F)(using e: Erased[F]): Eff[A] = new Eff(Erased.erase[A, F, Top](p))

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
    def handle[X](op: Top[X]): X = raw(op) match
      case t: Cap.Tagged => throw IllegalStateException(
        s"okay.java: ${t.op} was performed through ${t.cap} outside its handler — the capability escaped its scope")
      case other => throw IllegalStateException(s"okay.java: no handler took ${Rows.named(other)}")

  /** `Async`'s operations answered on this thread, every other refused as `run` refuses it */
  private[java] val asyncOnly: Answers[Top] = new Answers[Top]:
    private val async = Foreign.Member.of[Async]
    private val answers = summon[Answers[Async]]
    def handle[X](op: Top[X]): X = async.operation(Foreign.obj(raw(op))) match
      case Some(q) => answered[X](q.runWith(using answers))
      case None => unhandled.handle(op)
}

/** a state and the program's answer beside it */
final case class Stated[S, A](state: S, value: A)

object Stated {
  def of[S, A](state: S, value: A): Stated[S, A] = Stated(state, value)
}

/** a handler that keeps the program's answer: `Handler.answer`, `Handler.into`, `Handler.reader` */
final class Handler private (private val take: [A] => (A ! Top) => A ! Top) {
  private[java] def run[A](e: Eff[A]): Eff[A] = new Eff(take[A](e.program))
}

object Handler {

  /** form 1: each operation of `cls` answered by `f`, and the program goes on */
  def answer[O](cls: Class[O], f: JFunction[? >: O, ?]): Handler = answerBy(cls.isInstance, op => f.apply(cls.cast(op)))

  /** form 3: each operation of `cls` becomes the program `f` makes of it, in whatever effects remain */
  def into[O](cls: Class[O], f: JFunction[? >: O, Eff[?]]): Handler = intoBy(cls.isInstance, op => f.apply(cls.cast(op)))

  // the forms over any test: by class above, by a capability's identity in `Cap`; `f` reads the raw operation

  private[java] def answerBy(test: Any => Boolean, f: Any => Any): Handler =
    intoBy(test, op => Eff.pure(f(op)))

  private[java] def intoBy(test: Any => Boolean, f: Any => Eff[?]): Handler =
    new Handler([A] => (p: A ! Top) =>
      p.translate[Top, Top]([X] => (op: Top[X]) =>
        if test(raw(op)) then f(raw(op)).program.map(answered[X]) else effect[Top, X](op)
      ).at[Top])

  /** the core `Reader`: every `Eff.ask()` answered with `env` */
  def reader[E](env: E): Handler =
    new Handler([A] => (p: A ! Top) => okay.std.Reader.run[E, A, Reader % E + Top](env)(Moves.scala[Reader % E, A](p)))
}

/** a handler threading a state `S`: the program's answer arrives beside the last state */
final class StateHandler[S] private (private val take: [A] => (A ! Top) => (S, A) ! Top) {
  private[java] def run[A](e: Eff[A]): Eff[Stated[S, A]] = new Eff(take[A](e.program).map((s, a) => Stated(s, a)))
}

object StateHandler {

  /** form 2: each operation of `cls` answered from the state, which it may replace: `(s, op) -> Stated.of(s2, answer)` */
  def of[O, S](cls: Class[O], init: S, step: BiFunction[S, ? >: O, Stated[S, ?]]): StateHandler[S] =
    stateBy(cls.isInstance, init, (s, op) => step.apply(s, cls.cast(op)))

  private[java] def stateBy[S](test: Any => Boolean, init: S, step: (S, Any) => Stated[S, ?]): StateHandler[S] =
    val h = okay.Handler.state[Own, S](init)([X] => (s: S, op: Own[X]) =>
      val next = step(s, rawOwn(op))
      (next.state, answered[X](next.value)))
    new StateHandler([A] => (p: A ! Top) => Moves.own(p, test).handle(h))

  /** the core `State`, from `init`: `Eff.get()`, `put`, `modify` */
  def state[S](init: S): StateHandler[S] =
    new StateHandler([A] => (p: A ! Top) => okay.std.State.handle(init)[A, State % S + Top](Moves.scala[State % S, A](p)))
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

/** form 4: a handler with the continuation in hand, its return clause turning the answer `A` into `B` — the
 * kernel's `Deep` written out, its clauses' programs at `Top`'s place outside it */
final class Control[A, B] private[java] (test: Any => Boolean, ret: JFunction[? >: A, Eff[B]], clause: Clause[Any, B]) {
  private[java] def run(e: Eff[A]): Eff[B] =
    val p = Moves.own(e.program, test)
    new Eff(Run.install[Own, A, B, Own + Top, Top](p)(new okay.one.handler.Deep[Own, okay.one.row.Pure, A, B]:
      def ret(a: A) = Run.kernel(Control.this.ret.apply(a).program)
      def apply[X](op: Own[X], k: X => okay.one.![B, okay.one.row.Pure]) =
        Run.kernel(clause(rawOwn(op), x => new Eff(okay.Program.lift(k(answered[X](x))).at[Top])).program)))
}

object Control {

  /** `ret` for the program's answer, `clause` for each operation of `cls`: `(op, k) -> k.resume(true)` */
  def of[O, A, B](cls: Class[O], ret: JFunction[? >: A, Eff[B]], clause: Clause[? >: O, B]): Control[A, B] =
    new Control[A, B](cls.isInstance, ret, (op, k) => clause.apply(cls.cast(op), k))
}
