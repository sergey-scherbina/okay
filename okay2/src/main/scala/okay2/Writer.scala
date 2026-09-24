package okay2

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}
import Split.split

/**
 * The Writer effect IS a stream: telling w emits w, and a writer
 * program is already the stream — run is a fold over it. One
 * constructor, `Say(w): Op[W, Unit]`, because a tell answers NOTHING;
 * and the class of `Say` is distinct from everything, so a row
 * forwards any other effect beside it. `Writer % W` is `Writer[W]`.
 */
sealed trait Writer[W] extends Row { type Op[+A] = Writer.Op[W, A] }

object Writer {
  sealed trait Op[+W, +A]
  final case class Say[W](w: W) extends Op[W, Unit]

  implicit def effect[W]: Effect[Writer[W]] = Effect.of[Writer[W]]

  /** tell w: emit it as an operation, which answers NOTHING */
  def tell[W](w: W): Unit ! Writer[W] = Free.inject[Writer[W], Unit](Say(w))

  /**
   * The loop itself, with a `finish` applied to the accumulator AND
   * the program's answer where the PROGRAM ends — inside the loop,
   * never as a `.map` over the residual (a map wrapped around a
   * program that still forwards effects makes every forwarded node
   * left-nested under it).
   */
  def loopWith[W, S, A, R, F <: Row](a: A ! (Writer[W] + F))(z: S)(step: (S, W) => S)(finish: (S, A) => R): R ! F = {
    def _loop(s: S)(x: A ! (Writer[W] + F)): R ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! (Writer[W] + F)): R ! F = Free.resume(x) match {
      case Return(a) => Return(finish(s, a))
      case Inject(e) => loop(s)(Bind(Inject[Writer[W] + F, A](e), (x: A) => Return[Writer[W] + F, A](x)))
      case Bind(Inject(e), k) =>
        split[Writer[W], F, Any, Either[(S, A ! (Writer[W] + F)), R ! F]](e) {
          case Say(v) => Left((step(s, v), k(())))
        } { e => Right(Inject[F, Any](e).flatMap(x => _loop(s)(k(x)))) } match {
          case Left((s2, next)) => loop(s2)(next)
          case Right(done) => done
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(z)(a)
  }

  /** fold everything told, forwarding the rest of the row */
  def foldWith[W, S, A, R <: Row](a: A ! R)(z: S)(step: (S, W) => S)(implicit rm: Remove[Writer[W], R]): (S, A) ! rm.Out =
    loopWith[W, S, A, (S, A), rm.Out](rm.split(a))(z)(step)((s, a) => (s, a))

  /** collect everything told, in order, forwarding the rest of the row
   * — a List built by prepending and reversed ONCE at the end */
  def run[W, A, R <: Row](a: A ! R)(implicit rm: Remove[Writer[W], R]): (Seq[W], A) ! rm.Out =
    loopWith[W, List[W], A, (Seq[W], A), rm.Out](rm.split(a))(Nil)((s, w) => w :: s)((s, a) => (s.reverse, a))

  /** `run` answering a Vector */
  def collect[W, A, R <: Row](a: A ! R)(implicit rm: Remove[Writer[W], R]): (Vector[W], A) ! rm.Out =
    loopWith[W, List[W], A, (Vector[W], A), rm.Out](rm.split(a))(Nil)((s, w) => w :: s)((s, a) => (s.reverse.toVector, a))

  /** map the told values, keeping the PROGRAM: the telling is
   * transformed in place and the G-operations forwarded untouched */
  def map[W, V, A, R <: Row](a: A ! R)(f: W => V)(implicit rm: Remove[Writer[W], R]): A ! (Writer[V] + rm.Out) =
    mapAt[W, V, A, rm.Out](rm.split(a))(f)

  /** `map` at the handler's own shape */
  def mapAt[W, V, A, G <: Row](a: A ! (Writer[W] + G))(f: W => V): A ! (Writer[V] + G) =
    Free.resume(a) match {
      case Return(x) => Return(x)
      case Inject(e) => mapAt[W, V, A, G](Bind(Inject[Writer[W] + G, A](e), (x: A) => Return[Writer[W] + G, A](x)))(f)
      case Bind(Inject(e), k) =>
        // the split tests the Writer side (the class of `Say`); G is taken by exclusion
        split[Writer[W], G, Any, A ! (Writer[V] + G)](e) {
          case Say(w) => tell(f(w)).at[Writer[V] + G].flatMap(_ => mapAt[W, V, A, G](k(()))(f))
        } { g =>
          // a forwarded operation is re-injected at G and the PROGRAM widened
          Free.inject[G, Any](g).at[Writer[V] + G].flatMap(x => mapAt[W, V, A, G](k(x))(f))
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
}
