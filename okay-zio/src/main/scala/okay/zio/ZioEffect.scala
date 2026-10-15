package okay.zio

import okay.{!, +, Walk, Within, effect}
import _root_.zio.{ZEnvironment, ZIO}

/**
 * ZIO AS AN EFFECT OF THE TREE (specs/foreign-effects-in-tree.md). The row member is ZIO's own type —
 * `z.perform : Int ! ZIO[Db, DbErr, *]`, or an alias of ZIO's (`UIO`, `Task`) — so a ZIO is kept as one
 * operation and run when the program is handled. A program may hold ZIO steps with DIFFERENT environments and
 * errors, one member each (`ZIO[Db, DbErr, *] + ZIO[Log, LogErr, *]`), and the handlers below read the row's
 * `Env` and `E` off its members ([[ZioEffect.Members]]) the way ZIO's own `flatMap` bounds join them: the
 * environments meet (`Db & Log`), the errors join (`DbErr | LogErr`, an `AppErr` where that is their parent).
 * `p.toZIO` answers the type ZIO's own `for` would. These handlers take a row of ZIO members only: the whole row
 * is seen as ONE effect, `ZIO[Env, E, *]`, and walked (okay's `Walk`) or translated as that one.
 */
extension [A, R[_]](p: A ! R)
  /** the program's ZIO steps as ONE ZIO */
  def toZIO(using m: ZioEffect.Members[R]): ZIO[m.Env, m.E, A] =
    ZioEffect.run[A, m.Env, m.E](p.at[ZioEffect.Z[m.Env, m.E]](using m.within))

  /** every step given the environment: the row's `Env` is gone, as ZIO's `provideEnvironment` takes it */
  def provideEnvironment(using m: ZioEffect.Members[R])(env: ZEnvironment[m.Env]): A ! ZioEffect.Z[Any, m.E] =
    ZioEffect.mapSteps[A, m.Env, m.E, ZioEffect.Z[Any, m.E]](p.at[ZioEffect.Z[m.Env, m.E]](using m.within))(
      [X] => (z: ZIO[m.Env, m.E, X]) => z.provideEnvironment(env))

  /** every step's typed failure mapped, as ZIO's `mapError` */
  def mapError[Err](using m: ZioEffect.Members[R])(f: m.E => Err): A ! ZioEffect.Z[m.Env, Err] =
    ZioEffect.mapSteps[A, m.Env, m.E, ZioEffect.Z[m.Env, Err]](p.at[ZioEffect.Z[m.Env, m.E]](using m.within))(
      [X] => (z: ZIO[m.Env, m.E, X]) => z.mapError(f))

object ZioEffect:
  /** the one effect a row of ZIO members is seen as */
  type Z[Env, E] = [X] =>> ZIO[Env, E, X]

  /**
   * A ROW OF ZIO MEMBERS ONLY, and what its steps need and raise: `Env` the members' environments met, `E` their
   * errors joined. `within` sees the whole row as ONE effect, `Z[Env, E]` — each member's place the one place of
   * it: a sum's evidence the pair of its sides', a member's the identity (`leaf`, the one claim).
   */
  sealed trait Members[R[_]]:
    type Env
    type E
    def to[One[_]]: Within[R, One]
    final def within: Within[R, Z[Env, E]] = to[Z[Env, E]]

  object Members:
    given leaf[Env0, E0]: (Members[Z[Env0, E0]] { type Env = Env0; type E = E0 }) = new Members[Z[Env0, E0]]:
      type Env = Env0
      type E = E0
      // THE CAST, and why it is sound: a ZIO of `Env0` and `E0` is a ZIO of any `Env` it is part of and any `E`
      // it is part of (ZIO is contravariant in its environment, covariant in its error) — the one effect `One`
      // this row is seen as is that ZIO, at the meet and the join of the row's members
      def to[One[_]]: Within[Z[Env0, E0], One] = Within.same[One].asInstanceOf[Within[Z[Env0, E0], One]]

    given sum[F[_], G[_]](using f: Members[F], g: Members[G]): (Members[F + G] { type Env = f.Env & g.Env; type E = f.E | g.E }) =
      new Members[F + G]:
        type Env = f.Env & g.Env
        type E = f.E | g.E
        def to[One[_]]: Within[F + G, One] = Within.pair[F, G, One](f.to[One], g.to[One])

  /** the program into ZIO, each operation its own ZIO value: the root's `Walk.foldMap` with ZIO's monad (the walk on
   * inside ZIO's own continuation, stack-safe as ZIO's loop is) */
  private[zio] def run[A, Env, E](p: A ! Z[Env, E]): ZIO[Env, E, A] =
    Walk.foldMap[Z[Env, E], A, [X] =>> ZIO[Env, E, X]](p)([X] => (z: ZIO[Env, E, X]) => z)(using zioMonad[Env, E])

  /** each step rebuilt by `f`, in place: the operation translated where it is performed */
  private[zio] def mapSteps[A, Env, E, G[_]](p: A ! Z[Env, E])(f: [X] => ZIO[Env, E, X] => G[X]): A ! G =
    p.translate[Z[Env, E], G]([X] => (z: ZIO[Env, E, X]) => effect[G, X](f(z))).at[G]
