package okay.cluster.foreign

import okay.{!, +, pure}
import okay.given
import okay.Row.plus
import okay.codec.Schema
import okay.frege.Prog.TProg
import okay.foreign.{Jvm, Py, Shape}

/**
 * PROGRAMS AS DATA FROM THE JVM'S OWN LANGUAGES, through the facade
 * (foreign-jvm-programs, specs/foreign-one.md Decision 8): a Clojure
 * `(step op k)` and a Frege `Step op k` are walked by `okay.Foreign` in
 * this process, and the operations they perform are the caller's callbacks
 * — the `Cb`s every wire language's program is offered — so a job written
 * against `Programs[M]` runs a Clojure or Frege program as it runs a Python
 * one. Nothing crosses a wire: `Op` is `Pure`, and `run` just runs.
 *
 * A Clojure module is a namespace: its function is called with the argument
 * as JVM data (a record is a `java.util.Map`) and answers the program.
 */
final case class CljModule(ns: String)

object CljModule:
  given programs: Programs[CljModule] = new:
    type Op[+A] = Nothing
    def name = "clojure"
    def program[Arg: Schema, Out: Schema, F[+_]](module: CljModule, fn: String, cbs: Vector[Cb[F]])(a: Arg)
    : Either[Batcher.Failed, Out] ! (F + Op) =
      okay.clojure.Clj.fn(module.ns, fn) match
        case Left(why) => pure[F + Op, Either[Batcher.Failed, Out]](Left(Batcher.Failed("LookupError", why)))
        case Right(f) =>
          given okay.Member[F] = JvmPrograms.callbacksOnly[F]
          okay.clojure.Program.run[F, AnyRef](f.invoke(JvmPrograms.arg(a)), s"clojure ${module.ns}/$fn",
            JvmPrograms.calls(cbs)).map(JvmPrograms.answer[Out]).plus[Op]
    def run[A](module: CljModule)(prog: A ! Op): A = prog.runWith(using JvmPrograms.none)

/**
 * A Frege module: each program registered by name, a Scala function from
 * the argument (JVM data) to the compiled Frege program — the glue a Frege
 * function needs anyway, since its arguments are lazy on the JVM.
 */
final class FregeModule private (val name: String, programs: Map[String, AnyRef => TProg[?]]):
  def program(fn: String)(make: AnyRef => TProg[?]): FregeModule = new FregeModule(name, programs.updated(fn, make))
  private[foreign] def get(fn: String): Option[AnyRef => TProg[?]] = programs.get(fn)

object FregeModule:
  def apply(name: String): FregeModule = new FregeModule(name, Map.empty)

  given programs: Programs[FregeModule] = new:
    type Op[+A] = Nothing
    def name = "frege"
    def program[Arg: Schema, Out: Schema, F[+_]](module: FregeModule, fn: String, cbs: Vector[Cb[F]])(a: Arg)
    : Either[Batcher.Failed, Out] ! (F + Op) =
      module.get(fn) match
        case None => pure[F + Op, Either[Batcher.Failed, Out]](Left(Batcher.Failed("LookupError", s"no program named '$fn' in the Frege module ${module.name}")))
        case Some(make) =>
          given okay.Member[F] = JvmPrograms.callbacksOnly[F]
          okay.frege.Frege.run[F, AnyRef](make(JvmPrograms.arg(a)), s"frege ${module.name}.$fn",
            JvmPrograms.calls(cbs)).map(JvmPrograms.answer[Out]).plus[Op]
    def run[A](module: FregeModule)(prog: A ! Op): A = prog.runWith(using JvmPrograms.none)

private[foreign] object JvmPrograms:
  private given Shape = Shape.python

  /** the argument as JVM data: a record a `java.util.Map`, numbers boxed */
  def arg[A: Schema](a: A): AnyRef = Jvm.jvm(summon[Shape].encode(a))

  /** the facade's callbacks, as the walker asks them */
  def calls[F[+_]](cbs: Vector[Cb[F]]): okay.Foreign.Calls[F] =
    Jvm.calls(Py.callbacks[F](cbs.map(c => Py.callback[c.Arg, c.Res](c.name)(using c.arg, c.res)(c.run))*))

  /** a row whose own operations a facade program does not perform: through
   * the facade a program performs its CALLBACKS, as a wire language's does,
   * so an operation of the row is refused by name, never cast */
  def callbacksOnly[F[+_]]: okay.Member[F] = _ => false

  /** the handler of a row with no operations: what is left once the
   * caller has handled its callbacks' effects */
  val none: okay.Handler[[A] =>> Nothing] = new:
    def handle[A](e: Nothing): A = e

  /** the program's answer, read at the type the caller asked for */
  def answer[Out: Schema](x: AnyRef): Either[Batcher.Failed, Out] =
    Jvm.value(x).left.map(Batcher.Failed("NotAValue", _))
      .flatMap(v => summon[Shape].decode[Out](v).left.map(Language.failed))
