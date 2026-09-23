package okay.ts

import scala.scalajs.js
import okay.{!, Async, pure}
import okay.codec.{Json, Schema, Stubs}

/**
 * TypeScript programs INSIDE okay, on Scala.js (specs/typescript.md,
 * stage 2), and okay handed to TypeScript as a Promise (stage 3).
 *
 * A TypeScript program is written with the `done`, `perform` and `then`
 * of okay's TypeScript library (the `okay.ts` okay-py's worker ships;
 * its `call` is the worker's and is not needed here) — plain objects
 * `{tag: "done", value}` and `{tag: "perform", name, args, k}`. `run`
 * walks one in the SAME JavaScript runtime, with no process: each named
 * operation is a callback, an okay program run under the caller's
 * handlers, and `k` is a JS function, so a `Choice` handler may call it
 * twice.
 *
 * Values cross through okay's JSON codec — `JSON.stringify` on the way in,
 * `JSON.parse` on the way out — which is the shape `Stubs.typescript`
 * declares: a sum is `{ "Case": {...} }`, `None` is `null`.
 */
object Ts:

  /** a JavaScript exception, or a value of the wrong shape, as data */
  final case class Failure(kind: String, message: String)

  /** a Scala value as the JS value okay's JSON codec writes */
  def toJs[A](a: A)(using s: Schema[A]): js.Any = js.JSON.parse(Json.encode(s)(a))

  /** a JS value read back through okay's JSON codec */
  def fromJs[A](v: js.Any)(using s: Schema[A]): Either[Failure, A] =
    json(v).flatMap(j => Json.decode(s)(j).left.map(Failure("Decode", _)))

  private def json(v: js.Any): Either[Failure, Json] =
    // typed String, but `undefined` stringifies to undefined: read it as Any
    (js.JSON.stringify(v): Any) match
      case s: String => Right(Json.parse(s))
      case _ => Right(Json.JNull)

  /** a named operation of a TypeScript program: an okay program in F */
  final class Callback[F[+_]](val name: String, val run: Json => Either[Failure, Json] ! F)

  final class Callbacks[F[+_]](val all: Vector[Callback[F]]):
    def get(name: String): Option[Callback[F]] = all.find(_.name == name)
    def names: Vector[String] = all.map(_.name)

  def callbacks[F[+_]](cbs: Callback[F]*): Callbacks[F] = Callbacks(cbs.toVector)

  /** `perform(name, x)` in TypeScript runs `f(x)` in okay; several
   * arguments arrive as an array */
  def callback[Arg: Schema, Res: Schema](name: String): CallbackOf[Arg, Res] = CallbackOf(name)

  final class CallbackOf[Arg, Res](name: String)(using arg: Schema[Arg], res: Schema[Res]):
    def apply[F[+_]](f: Arg => Res ! F): Callback[F] = Callback(name, args =>
      val in = args match
        case Json.JArr(Vector(one)) => one
        case many => many
      Json.decode(arg)(in) match
        case Left(why) => pure[F, Either[Failure, Json]](Left(Failure("Decode", s"$name: $why")))
        case Right(a) => f(a).map(r => Right(Json.parse(Json.encode(res)(r)))))

  /** a JavaScript exception, by its name and message */
  private def caught(e: Any): Failure = e match
    case err: js.Error => Failure(err.name, err.message)
    case other => Failure("Error", String.valueOf(other))

  /**
   * Walk a TypeScript program to its answer. Each step is one okay
   * program node, so a long program is a loop, not a stack.
   */
  def run[F[+_], Out](program: js.Any, cbs: Callbacks[F])(using out: Schema[Out]): Either[Failure, Out] ! F =
    walk(program, cbs, fromJs[Out](_))

  /** `run` whose answer is the program's final value as JSON, whatever
   * its shape (okay-ts-npm's `run`, typescript-types T9) */
  def runJson[F[+_]](program: js.Any, cbs: Callbacks[F]): Either[Failure, Json] ! F =
    walk(program, cbs, json)

  private def walk[F[+_], Out](program: js.Any, cbs: Callbacks[F], finish: js.Any => Either[Failure, Out]): Either[Failure, Out] ! F =
    def go(p: js.Dynamic): Either[Failure, Out] ! F =
      (p.selectDynamic("tag"): Any) match
        case "done" => pure[F, Either[Failure, Out]](finish(p.selectDynamic("value")))
        case "perform" =>
          val name = String.valueOf(p.selectDynamic("name"))
          json(p.selectDynamic("args")) match
            case Left(f) => pure[F, Either[Failure, Out]](Left(f))
            case Right(args) => cbs.get(name) match
              case None => pure[F, Either[Failure, Out]](Left(Failure("NoCallback",
                s"'$name' is not among this program's callbacks (${cbs.names.mkString(", ")})")))
              case Some(cb) => cb.run(args).flatMap {
                case Left(f) => pure[F, Either[Failure, Out]](Left(f))
                case Right(answer) =>
                  val next =
                    try Right(p.applyDynamic("k")(js.JSON.parse(Json.print(answer))))
                    catch case js.JavaScriptException(e) => Left(caught(e))
                  next match
                    case Left(f) => pure[F, Either[Failure, Out]](Left(f))
                    case Right(n) => go(n)
              }
        case other => pure[F, Either[Failure, Out]](Left(Failure("TypeError",
          s"a program answers done(v) or perform(name, ...), got a tag of $other")))
    try go(js.Dynamic.global.Object(program))
    catch case js.JavaScriptException(e) => pure(Left(caught(e)))

  /**
   * okay called FROM TypeScript (stage 3): run an `A ! Async` and hand
   * TypeScript a `Promise` of its value, as okay's JSON codec writes it.
   * An `@JSExportTopLevel` function returning this is an okay program a
   * TypeScript caller simply `await`s.
   */
  def promise[A](program: A ! Async)(using s: Schema[A]): js.Promise[js.Any] =
    import scala.scalajs.js.JSConverters.*
    given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue
    Async.runAsync(program).map(toJs(_)).toJSPromise

  /**
   * One okay function for a TypeScript caller (typescript-types T6): the
   * argument read with okay's JSON codec, the answer written with it, as a
   * `Promise`. A wrong argument rejects the promise with a `TypeError`
   * naming the function and the reason. (`export` is a Scala 3 keyword,
   * hence `expose`.)
   */
  def expose[A, B](name: String)(f: A => B ! Async)(using a: Schema[A], b: Schema[B]): Exposed =
    val call: js.Function1[js.Any, js.Promise[js.Any]] = (in: js.Any) =>
      fromJs[A](in) match
        case Right(x) => promise(f(x))
        case Left(why) => js.Promise.reject(js.TypeError(s"$name: ${why.message}"))
    Exposed(name, a, b, call)

  /** a function `expose` made: its name, its types and the JS function */
  final class Exposed private[Ts] (val name: String, val in: Schema[?], val out: Schema[?],
                                   val fn: js.Function1[js.Any, js.Promise[js.Any]])

  /**
   * Exposed functions as one module for TypeScript: `js` is the object to
   * export (`@JSExportTopLevel("tasks") val tasks: js.Object = m.js`), and
   * `declaration` is the `.d.ts` TypeScript reads for it. The declaration
   * is written from the same Schemas that encode the values, so the
   * declared types match what is sent.
   */
  def module(name: String)(exposed: Exposed*): TsModule = TsModule(name, exposed.toVector)

  final class TsModule private[Ts] (val name: String, val exposed: Vector[Exposed]):
    def js: scala.scalajs.js.Object & scala.scalajs.js.Dynamic =
      val o = scala.scalajs.js.Dynamic.literal()
      exposed.foreach(e => o.updateDynamic(e.name)(e.fn))
      o

    def declaration: String =
      val types = Stubs.typescript(exposed.flatMap(e => Vector(e.in, e.out))*)
      val sigs = exposed.map(e =>
        s"  ${e.name}(input: ${Stubs.typescriptType(e.in)}): Promise<${Stubs.typescriptType(e.out)}>;")
      types + s"\nexport declare const $name: {\n${sigs.mkString("\n")}\n};\n"
