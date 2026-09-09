package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * STATE CELLS MADE AT RUN TIME — the other half of the pair `Keyed`
 * opens, and the same trade the library already makes between `HMap`
 * and `TMap`.
 *
 * `Keyed` puts the states in the ROW: `Keyed.At["count", Int] +
 * Keyed.At["name", String]` says which states exist and at what
 * types, nothing is created, and nothing casts. That is impossible
 * when the cells come from somewhere — one per request, one per node
 * of a walk, one per element of a list nobody has read yet — because
 * a type cannot list what does not exist yet.
 *
 * So `Refs` says only "this program uses refs". One row member
 * however many there are, `cell(init)` makes a new one at run time,
 * and identity is the cell itself:
 *
 *     val p: Int ! Refs = direct {
 *       val a = Refs.ref(1).!?
 *       val b = Refs.ref(10).!?
 *       Refs.write(a, Refs.read(a).!? + Refs.read(b).!?).!?
 *     }
 *
 * The price, stated: the row no longer says which states there are,
 * and the handler's heap is keyed by identity, so reading a cell
 * returns an `Any` that ONE cast turns back into its type. That cast
 * is sound because a `Ref[S]` is only ever made by `New(init: S)`
 * and only ever written by `Write(c: Ref[S], s: S)`, so what comes
 * out of a slot is what the same S put in. It is the cast `TMap`
 * makes, for the same reason, and it is the whole difference between
 * the two halves.
 */
enum Refs[+A]:
  case New[S](init: S) extends Refs[Refs.Ref[S]]
  case Read[S](c: Refs.Ref[S]) extends Refs[S]
  case Write[S](c: Refs.Ref[S], s: S) extends Refs[S]

object Refs:
  /** a slot in the handler's heap. Opaque, and declared HERE so that
   * the handler below — and only it — can see the number. */
  opaque type Ref[S] = Int

  given okay.Effect[Refs] = okay.Effect.of(typeableK(classOf[Refs[?]]))

  /** a new cell, holding init */
  inline def ref[S](init: S): Ref[S] ! Refs = effect(New(init))

  /** what the cell holds */
  inline def read[S](c: Ref[S]): S ! Refs = effect(Read(c))

  /** replace it, answering the new value */
  inline def write[S](c: Ref[S], s: S): S ! Refs = effect(Write(c, s))

  /**
   * The handler is `State.handle`'s loop over a HEAP: the next free
   * slot and the slots themselves, threaded rather than mutated, so
   * the residual program stays re-runnable.
   *
   * THE ONE CAST is `slot`: a heap keyed by identity cannot be typed,
   * and every value in it was put there by an operation at the same S
   * as the one now reading it (see the class comment). Isolated here
   * so there is one place to check that claim.
   */
  def handle[A, F[+_]](p: A ! (Refs + F)): A ! F =
    def slot[S](h: Map[Int, Any], c: Ref[S]): S = h(c).asInstanceOf[S]

    def _loop(n: Int, h: Map[Int, Any])(x: A ! (Refs + F)): A ! F = loop(n, h)(x)

    @tailrec def loop(n: Int, h: Map[Int, Any])(x: A ! (Refs + F)): A ! F =
      (x.resume: @unchecked) match
        case Pure(a) => Pure(a)
        case Effect(e) => <|>[Refs, F](e) match
          case Left(New(init)) => Pure(n)
          case Left(Read(c)) => Pure(slot(h, c))
          case Left(Write(_, s)) => Pure(s)
          case Right(e) => Effect(e)
        case Bind(Effect(e), k) => <|>[Refs, F](e) match
          case Left(New(init)) => loop(n + 1, h.updated(n, init))(k(n))
          case Left(Read(c)) => loop(n, h)(k(slot(h, c)))
          case Left(Write(c, s)) => loop(n, h.updated(c, s))(k(s))
          case Right(e) => Effect(e).flatMap(x => _loop(n, h)(k(x)))

    loop(0, Map.empty)(p)

  /** run a program that uses cells, and nothing else */
  inline def run[A](p: A ! Refs): A = !.run(handle[A, okay.Pure](p))
