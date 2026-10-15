package okay.cats

import okay.{!, +, Async, Walk}
import _root_.cats.effect.IO

/**
 * A PROGRAM OF `E` AND `Async` AS ONE IO, walked (okay's `Walk`, okay-freer's `resume` on the kernel): `E` walked
 * among the program's effects, the `Async` operations met on the way walked outside it — each operation its IO,
 * the next step inside IO's own `flatMap`, so the walk is as stack-safe as IO's loop.
 *
 * The walk has no "last operation" form: the last operation is followed by one `flatMap` into `IO.pure`, where
 * okay-freer's walk ended on the operation itself.
 */
private[cats] object Walking:
  def toIO[E[_], A](p: A ! E + Async)(step: [X] => E[X] => IO[X]): IO[A] =
    outer(Walk.view(Walk.next[E, A, E + Async](p)), step)

  private def outer[E[_], A](s: Walk.Step[Async, Walk.Step[E, A, E + Async], Async], step: [X] => E[X] => IO[X]): IO[A] =
    s match
      case Walk.Done(st) => st match
        case Walk.Done(a) => IO.pure(a)
        case o: Walk.Op[E, x, A, E + Async] => step(o.op).flatMap(x => toIO(o.resume(x))(step))
      case o: Walk.Op[Async, x, Walk.Step[E, A, E + Async], Async] =>
        CatsEffect.asyncIO(o.op).flatMap(x => outer(Walk.view(o.resume(x)), step))
