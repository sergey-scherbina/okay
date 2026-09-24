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

  /**
   * The observation of the writer as codata: the next told value and
   * the rest through Right, on demand; when they end, Left carries the
   * program's own answer. `Say` is the only constructor, so the match
   * refines the continuation's argument to Unit — no cast.
   */
  def uncons[W, A](a: A ! Writer[W]): Either[A, (W, A ! Writer[W])] = Free.resume(a) match {
    case Return(x) => Left(x)
    case Inject(Say(w)) => Right((w, Return(())))
    case Bind(Inject(Say(w)), k) => Right((w, k(())))
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /** the same observation for a writer program performing ARBITRARY
   * effects G: the next told value arrives inside G — the G-operations
   * met on the way are carried into the answer (deferred, not run) */
  def unconsIn[W, A, G <: Row](a: A ! (Writer[W] + G)): Either[A, (W, A ! (Writer[W] + G))] ! G = Free.resume(a) match {
    case Return(x) => pure(Left(x))
    case Inject(e) =>
      split[Writer[W], G, A, Either[A, (W, A ! (Writer[W] + G))] ! G](e) {
        case Say(w) => pure(Right((w, Return(()))))
      } { g => Inject[G, A](g).map(x => Left(x)) }
    case Bind(Inject(e), k) =>
      split[Writer[W], G, Any, Either[A, (W, A ! (Writer[W] + G))] ! G](e) {
        case Say(w) => pure(Right((w, k(()))))
      } { g => Inject[G, Any](g).flatMap(x => unconsIn[W, A, G](k(x))) }
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /** fold everything told into a Fold algebra, forwarding the rest of
   * the row; dispatched on the accumulator as `Stream.fold` is */
  def fold[W, S, A, R <: Row](a: A ! R)(fo: Fold[W, S])(implicit rm: Remove[Writer[W], R]): (S, A) ! rm.Out =
    foldAt[W, S, A, rm.Out](rm.split(a))(fo)

  def foldAt[W, S, A, F <: Row](a: A ! (Writer[W] + F))(fo: Fold[W, S]): (S, A) ! F = fo match {
    // the four primitive shapes keep the accumulator unboxed across
    // the loop; the result is the same `(S, A)` the fold's own type
    // says, which the type test cannot tell the compiler
    case l: Fold.OfLong[W @unchecked] => loopWith[W, Long, A, (Long, A), F](a)(l.initLong)((s, w) => l.addLong(s, w))((s, a) => (s, a)).asInstanceOf[(S, A) ! F]
    case i: Fold.OfInt[W @unchecked] => loopWith[W, Int, A, (Int, A), F](a)(i.initInt)((s, w) => i.addInt(s, w))((s, a) => (s, a)).asInstanceOf[(S, A) ! F]
    case d: Fold.OfDouble[W @unchecked] => loopWith[W, Double, A, (Double, A), F](a)(d.initDouble)((s, w) => d.addDouble(s, w))((s, a) => (s, a)).asInstanceOf[(S, A) ! F]
    case b: Fold.OfBoolean[W @unchecked] => loopWith[W, Boolean, A, (Boolean, A), F](a)(b.initBoolean)((s, w) => b.addBoolean(s, w))((s, a) => (s, a)).asInstanceOf[(S, A) ! F]
    case _ => loopWith[W, S, A, (S, A), F](a)(fo.init)((s, w) => fo.add(s, w))((s, a) => (s, a))
  }

  /**
   * A fold that STOPS: `loopWith`'s walk with an early `Return(end(s))`
   * the moment the state is done. The `Bind(Inject(Say), k)` arm does
   * not call `k` then, which is what stops the producer: nothing past
   * the satisfying tell is built, and an F operation that would have
   * followed it is never performed.
   */
  def foldUntil[W, S, A, R, Rw <: Row](a: A ! Rw)(fo: FoldUntil[W, S, R])(implicit rm: Remove[Writer[W], Rw]): R ! rm.Out =
    foldUntilAt[W, S, A, R, rm.Out](rm.split(a))(fo)

  def foldUntilAt[W, S, A, R, F <: Row](a: A ! (Writer[W] + F))(fo: FoldUntil[W, S, R]): R ! F = {
    def _loop(s: S)(x: A ! (Writer[W] + F)): R ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! (Writer[W] + F)): R ! F =
      if (fo.done(s)) Return(fo.end(s))
      else Free.resume(x) match {
        case Return(_) => Return(fo.end(s))
        case Inject(e) => loop(s)(Bind(Inject[Writer[W] + F, A](e), (x: A) => Return[Writer[W] + F, A](x)))
        case Bind(Inject(e), k) =>
          split[Writer[W], F, Any, Either[(S, A ! (Writer[W] + F)), R ! F]](e) {
            case Say(v) => Left((fo.add(s, v), k(())))
          } { e => Right(Inject[F, Any](e).flatMap(x => _loop(s)(k(x)))) } match {
            case Left((s2, next)) => loop(s2)(next)
            case Right(done) => done
          }
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    loop(fo.init)(a)
  }

  /**
   * ANY stream as a writer program: its elements told one by one, its
   * own effects F performed at each pull. Lazy: nothing is pulled until
   * the result is consumed, one element per pull.
   */
  def of[S[_], F <: Row, A](s: S[A])(implicit St: Stream[S, F]): Unit ! (Writer[A] + F) =
    pure[Writer[A] + F, Unit](()).flatMap(_ => ofLoop[S, F, A](s))

  private def ofLoop[S[_], F <: Row, A](s: S[A])(implicit St: Stream[S, F]): Unit ! (Writer[A] + F) =
    St.uncons(s).at[Writer[A] + F].flatMap {
      case Some((a, rest)) => tell(a).at[Writer[A] + F].flatMap(_ => ofLoop[S, F, A](rest))
      case None => pure(())
    }

  /** re-tell at a WIDER element type with no transform: `Say[W]` IS a
   * `Say[V]` for `V >: W` (Op is covariant), and the row erases — one
   * cast, by the same argument as `Member.coerce` */
  def widen[W, V >: W, A, G <: Row](a: A ! (Writer[W] + G)): A ! (Writer[V] + G) = a.asInstanceOf[A ! (Writer[V] + G)]

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
