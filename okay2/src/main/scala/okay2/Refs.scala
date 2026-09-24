package okay2

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}
import Split.split

/**
 * STATE CELLS MADE AT RUN TIME. `State[S]` puts a state in the ROW, so
 * the type says which states exist; that is impossible when the cells
 * come from somewhere — one per request, one per node of a walk, one
 * per element of a list nobody has read yet — because a type cannot
 * list what does not exist yet. `Refs` says only "this program uses
 * refs": one row member however many there are, `ref(init)` makes a new
 * cell at run time, and identity is the cell itself.
 *
 * {{{
 *   for { a <- Refs.ref(1); b <- Refs.ref("ada"); _ <- Refs.write(a, 2) } yield ()
 * }}}
 *
 * The handler is `State.handle`'s loop over a HEAP: the next free slot
 * and the slots themselves, threaded rather than mutated, so the
 * residual program stays re-runnable.
 *
 * NO CAST, where the Scala 3 core needs one. There, the heap is keyed by
 * identity and a read must turn an `Any` back into the cell's `S`. Here
 * every handler runs at the answer type `Any` (a `Bind`'s continuation
 * takes `Any`, stage 8), so the value read out of a slot goes straight
 * into the continuation that the `Read[S]` operation's own answer type
 * describes — the claim the core's cast makes is the one the tree
 * already made when `read[S]` was built.
 */
sealed trait Refs extends Row { type Op[+A] = Refs.Op[A] }

object Refs {
  sealed trait Op[+A]
  final case class New[S](init: S) extends Op[Ref[S]]
  final case class Read[S](c: Ref[S]) extends Op[S]
  final case class Write[S](c: Ref[S], s: S) extends Op[S]

  /** a slot in the handler's heap: its number, which only the handler
   * reads, and no way to make one but `ref` */
  final class Ref[S] private[Refs] (private[Refs] val slot: Int) extends AnyVal {
    override def toString: String = "Ref#" + slot
  }

  implicit val effect: Effect[Refs] = Effect.of[Refs]

  /** a new cell, holding init */
  def ref[S](init: S): Ref[S] ! Refs = Free.inject[Refs, Ref[S]](New(init))

  /** what the cell holds */
  def read[S](c: Ref[S]): S ! Refs = Free.inject[Refs, S](Read(c))

  /** replace it, answering the new value */
  def write[S](c: Ref[S], s: S): S ! Refs = Free.inject[Refs, S](Write(c, s))

  /** the handler, the rest of the row forwarded */
  def handle[A, R <: Row](p: Free[Refs with R, A]): A ! R = {
    type Heap = Map[Int, Any]
    def _loop(n: Int, h: Heap)(x: Free[Refs with R, A]): A ! R = loop(n, h)(x)

    @tailrec def loop(n: Int, h: Heap)(x: Free[Refs with R, A]): A ! R = Free.resume(x) match {
      case Return(a) => Return(a)
      // a lone operation is a Bind with a pure continuation (package.scala)
      case Inject(e) => loop(n, h)(Bind(Inject[Refs with R, A](e), (v: A) => Return[Refs with R, A](v)))
      case Bind(Inject(e), k) =>
        split[Refs, R, Any, Either[(Int, Heap, Free[Refs with R, A]), A ! R]](e) {
          case New(init) => Left((n + 1, h.updated(n, init), k(new Ref[Any](n))))
          case Read(c) => Left((n, h, k(h(c.slot))))
          case Write(c, s) => Left((n, h.updated(c.slot, s), k(s)))
        } { g => Right(Inject[R, Any](g).flatMap(v => _loop(n, h)(k(v)))) } match {
          case Left((n2, h2, next)) => loop(n2, h2)(next)
          case Right(done) => done
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(0, Map.empty)(p)
  }

  /** run a program that uses cells, and nothing else */
  def run[A](p: Free[Refs, A]): A = Effects.run(handle[A, Pure](p))
}
