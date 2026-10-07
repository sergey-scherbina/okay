package okay.zio

import okay.{TypeableK, typeableK}
import okay.freer.{!, Free, effect}
import okay.freer.!.*
import _root_.zio.{ZEnvironment, ZIO}

/**
 * ZIO AS AN EFFECT OF THE TREE (specs/foreign-effects-in-tree.md). The row member is ZIO's own type —
 * `z.perform : Int ! ZIO[Db, DbErr, *]`, or an alias of ZIO's (`UIO`, `Task`) — so a ZIO is kept as one
 * operation and run when the program is handled. A program may hold ZIO steps with DIFFERENT environments and
 * errors, one member each (`ZIO[Db, DbErr, *] + ZIO[Log, LogErr, *]`), and the handlers below read `R` and `E`
 * off the whole row BY SUBTYPING — `R[Any] <:< ZIO[Env, E, Any]` — which is what ZIO's own `flatMap` bounds do:
 * the environments meet (`Db & Log`), the errors join (`AppErr`). `p.toZIO` answers the type ZIO's own `for`
 * would. These handlers take a row of ZIO members only, and walk it without `split`: two ZIO members are one
 * class at run time and no test tells them apart (`Distinct` refuses such a row to every handler that splits).
 */
given zioTypeable[R, E]: TypeableK[[X] =>> ZIO[R, E, X]] = typeableK[[X] =>> ZIO[R, E, X]](classOf[ZIO[?, ?, ?]])

extension [A, R[+_]](p: A ! R)
  /** the program's ZIO steps as ONE ZIO */
  def toZIO[Env, E](using R[Any] <:< ZIO[Env, E, Any]): ZIO[Env, E, A] = ZioEffect.run[A, R, Env, E](p)

  /** every step given the environment: the row's `Env` is gone, as ZIO's `provideEnvironment` takes it */
  def provideEnvironment[Env, E](env: ZEnvironment[Env])(using R[Any] <:< ZIO[Env, E, Any]): A ! ([X] =>> ZIO[Any, E, X]) =
    ZioEffect.mapSteps[A, R, Env, E, [X] =>> ZIO[Any, E, X]](p)([X] => (z: ZIO[Env, E, X]) => z.provideEnvironment(env))

  /** every step's typed failure mapped, as ZIO's `mapError` */
  def mapError[Env, E, Err](using R[Any] <:< ZIO[Env, E, Any])(f: E => Err): A ! ([X] =>> ZIO[Env, Err, X]) =
    ZioEffect.mapSteps[A, R, Env, E, [X] =>> ZIO[Env, Err, X]](p)([X] => (z: ZIO[Env, E, X]) => z.mapError(f))

object ZioEffect:
  /**
   * THE ONE CLAIM: an operation of the row read as the ZIO the row's evidence names. `R[Any] <:< ZIO[Env, E, Any]`
   * says it at `Any` (`Row.Sub`'s approximation, said out loud there); every member of the row is a ZIO whose
   * value type is the operation's answer type, so the operation at `X` is a `ZIO[Env, E, X]`.
   */
  private def step[R[+_], Env, E, X](e: R[X]): ZIO[Env, E, X] = e.asInstanceOf[ZIO[Env, E, X]]

  /** a walk: one `flatMap` a bind, the recursion inside ZIO's own continuation (stack-safe) */
  private[zio] def run[A, R[+_], Env, E](p: A ! R): ZIO[Env, E, A] = (p.resume: @unchecked) match
    case Return(a) => ZIO.succeed(a)
    case Inject(e) => step[R, Env, E, A](e)
    case Bind(Inject(e), k) => runBind(e, k)

  private def runBind[A, R[+_], Env, E, X](e: R[X], k: X => A ! R): ZIO[Env, E, A] =
    step[R, Env, E, X](e).flatMap(x => run[A, R, Env, E](k(x)))

  /** each step rebuilt by `f`, lazily: the rest is mapped only when an interpreter reaches it */
  private[zio] def mapSteps[A, R[+_], Env, E, G[+_]](p: A ! R)(f: [X] => ZIO[Env, E, X] => G[X]): A ! G =
    (p.resume: @unchecked) match
      case Return(a) => Return(a)
      case Inject(e) => effect[G, A](f(step[R, Env, E, A](e)))
      case Bind(Inject(e), k) => mapBind[A, R, Env, E, G](f)(e, k)

  private def mapBind[A, R[+_], Env, E, G[+_]](f: [Y] => ZIO[Env, E, Y] => G[Y])[X](e: R[X], k: X => A ! R): A ! G =
    Free.Bind(effect[G, X](f(step[R, Env, E, X](e))), x => mapSteps[A, R, Env, E, G](k(x))(f))
