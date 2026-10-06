package okay.semantic.data

import okay.{!, Async, Source}
import okay.codec.Json
import okay.semantic.{Model, Plan, Result}

/** An application can expose this contract through HTTP, a notebook or a tool transport. */
trait Endpoint:
  def describe: Json
  def explain(request: Json): Json
  def query(request: Json): Json ! Async
object Api:
  def apply[A](model: Model[A])(execute: Plan[A] => Either[Vector[String], Result] ! Async): Endpoint = new Endpoint:
    def describe: Json = Wire.describe(model)
    private def plan(request: Json): Either[Vector[String], Plan[A]] = Wire.request(request).flatMap(model.plan)
    def explain(request: Json): Json = plan(request) match
      case Left(es) => Wire.failure(es)
      case Right(p) => Json.JObj(Vector("explain" -> Json.JStr(p.explain)))
    def query(request: Json): Json ! Async = plan(request) match
      case Left(es) => okay.pure(Wire.failure(es))
      case Right(p) => execute(p).map(Wire.response)
  def local[A](model: Model[A], rows: () => IterableOnce[A]): Endpoint =
    apply(model)(plan => okay.pure(plan.run(rows())))
  def source[A](model: Model[A], rows: () => Source[A]): Endpoint =
    apply(model)(plan => Data.source(plan, rows()))

  def apply[A](model: okay.semantic.ossie.ExpressionModel[A])(
      execute: okay.semantic.ossie.ExpressionPlan[A] => Either[Vector[String], Result] ! Async): Endpoint = new Endpoint:
    def describe: Json = model.document.raw
    private def plan(request: Json): Either[Vector[String], okay.semantic.ossie.ExpressionPlan[A]] = Wire.request(request).flatMap(model.plan(_))
    def explain(request: Json): Json = plan(request) match
      case Left(es) => Wire.failure(es)
      case Right(p) => Json.JObj(Vector("explain" -> Json.JStr(p.explain)))
    def query(request: Json): Json ! Async = plan(request) match
      case Left(es) => okay.pure(Wire.failure(es))
      case Right(p) => execute(p).map(Wire.response)
  def local[A](model: okay.semantic.ossie.ExpressionModel[A], rows: () => IterableOnce[A]): Endpoint =
    apply(model)(plan => okay.pure(plan.run(rows())))
  def source[A](model: okay.semantic.ossie.ExpressionModel[A], rows: () => Source[A]): Endpoint =
    apply(model)(plan => plan.source(rows()))
