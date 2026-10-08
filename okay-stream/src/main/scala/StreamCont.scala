package okay

import okay.AsyncCont.{awaitEither, fork, join}
import okay.cont.Free
import okay.std.Writer
import okay.freer.Row.plus
import java.util.concurrent.atomic.AtomicBoolean

/**
 * THE STREAM ON THE MACHINE, A TWIN (stream-twin): beside the classic `Source[W] = Unit ! Writer % W + Async`, a
 * stream of the machine's `A ! R` — to learn what streaming costs and looks like there before anything moves.
 *
 * A PULL, NOT A PUSH. The classic's stream is a program that TELLS (Writer), and a transformation is a handler
 * that tells again downstream. On the machine a handler's clause has no capability for the rest of the row, so a
 * handler cannot re-perform downstream; the machine's own shape is the pull: a stream is an Async program that
 * answers its first element and the rest, or that it ended (`Step`). Its transformations are functions over that
 * answer — no handler at all — and its effects are Async's alone. A stream is re-runnable when what it was built
 * from is (a `List`, a `range`; an `Iterator` is read once).
 */
object StreamCont:
  type R = Async +: Pure

  /** where a stream stands when pulled */
  enum Step[+W]:
    case Done
    case Next(w: W, rest: Src[W])

  /** a stream: the program that pulls it */
  final class Src[+W](val pull: Free[R, Step[W]]):
    def map[V](f: W => V): Src[V] = Src(pull.map {
      case Step.Done => Step.Done
      case Step.Next(w, r) => Step.Next(f(w), r.map(f))
    })

    def filter(p: W => Boolean): Src[W] = Src(pull.flatMap {
      case Step.Done => Free.pure(Step.Done)
      case Step.Next(w, r) => if p(w) then Free.pure(Step.Next(w, r.filter(p))) else r.filter(p).pull
    })

    /** the first `n` elements; the producer is not pulled past them */
    def take(n: Int): Src[W] =
      if n <= 0 then StreamCont.empty
      else Src(pull.map {
        case Step.Done => Step.Done
        case Step.Next(w, r) => Step.Next(w, r.take(n - 1))
      })

    /** this, then `that` */
    def ++[V >: W](that: => Src[V]): Src[V] = Src(pull.flatMap {
      case Step.Done => that.pull
      case Step.Next(w, r) => Free.pure(Step.Next(w, r ++ that))
    })

    /** each element's stream, in order */
    def flatMap[V](f: W => Src[V]): Src[V] = Src(pull.flatMap {
      case Step.Done => Free.pure(Step.Done)
      case Step.Next(w, r) => (f(w) ++ r.flatMap(f)).pull
    })

    /** an Async step per element */
    def evalMap[V](f: W => V ! R): Src[V] = Src(pull.flatMap {
      case Step.Done => Free.pure(Step.Done)
      case Step.Next(w, r) => f(w).map(v => Step.Next(v, r.evalMap(f)))
    })

    def foldLeft[B](z: B)(f: (B, W) => B): B ! R = pull.flatMap {
      case Step.Done => Free.pure(z)
      case Step.Next(w, r) => r.foldLeft(f(z, w))(f)
    }

    def toVector: Vector[W] ! R = foldLeft(Vector.newBuilder[W])((b, w) => b += w).map(_.result())

    def runForeach(f: W => Unit ! R): Unit ! R = pull.flatMap {
      case Step.Done => Free.pure(())
      case Step.Next(w, r) => f(w).flatMap(_ => r.runForeach(f))
    }

    /**
     * both streams at once, each element as it comes: each side's next pull on its own fiber, the first answer
     * taken and its side pulled again, the other side's fiber kept. Not yet: a consumer that stops early leaves
     * the pending pulls running (the classic's merge closes them by a cancel scope)
     */
    def merge[V >: W](that: Src[V])(using Scheduler): Src[V] =
      Src(fork[Step[V]](this.pull).flatMap(fa => fork[Step[V]](that.pull).flatMap(fb => StreamCont.merging[V](fa, fb).pull)))

  private def merging[W](fa: Fiber[Step[W]], fb: Fiber[Step[W]])(using Scheduler): Src[W] =
    Src(first(fa, fb).flatMap {
      case (Step.Done, other) => join(other).map(s => s)
      case (Step.Next(w, r), other) =>
        Free.pure(Step.Next(w, Src(fork(r.pull).flatMap(again => merging(again, other).pull))))
    })

  /** which of two fibers answers first: its answer, and the other fiber */
  private def first[W](fa: Fiber[Step[W]], fb: Fiber[Step[W]]): (Step[W], Fiber[Step[W]]) ! R =
    awaitEither[(Step[W], Fiber[Step[W]])] { k =>
      val won = AtomicBoolean(false)
      fa.onComplete(r => if !won.getAndSet(true) then k(r.map(s => (s, fb))))
      fb.onComplete(r => if !won.getAndSet(true) then k(r.map(s => (s, fa))))
      () => ()
    }.at

  def empty[W]: Src[W] = Src(Free.pure(Step.Done))

  /** pulled again only when its rest is pulled: a tail call between mutually recursive streams costs no frame */
  def defer[W](s: => Src[W]): Src[W] = Src(Free.delay(() => s.pull))

  def emit[W](w: W): Src[W] = Src(Free.pure(Step.Next(w, empty)))

  def apply[W](ws: W*): Src[W] = fromList(ws.toList)

  def fromList[W](ws: List[W]): Src[W] = ws match
    case Nil => empty
    case w :: more => Src(Free.pure(Step.Next(w, defer(fromList(more)))))

  /** read once: the iterator is made when the stream is first pulled */
  def fromIterator[W](it: => Iterator[W]): Src[W] = Src(Free.delay(() => reading(it).pull))
  private def reading[W](it: Iterator[W]): Src[W] =
    Src(Free.delay(() => if it.hasNext then Free.pure(Step.Next(it.next(), reading(it))) else Free.pure(Step.Done)))

  def range(from: Long, until: Long): Src[Long] =
    if from >= until then empty else Src(Free.pure(Step.Next(from, defer(range(from + 1, until)))))

  /** one Async step as a stream of its answer */
  def eval[W](op: W ! R): Src[W] = Src(op.map(w => Step.Next(w, empty)))

  // ---- bridges to the classic Source, per element

  /** the classic stream pulled from the machine: `Writer.uncons` per element, each one Await of the machine */
  def fromSource[W](s: Source[W]): Src[W] =
    Src(AsyncCont.fromClassic(Writer.uncons[W, Unit, Async](s)).at[R].map {
      case Left(()) => Step.Done
      case Right((w, rest)) => Step.Next(w, fromSource(rest))
    })

  /** the machine's stream as a classic one: each pull a classic program (`toClassic`), each element told */
  def toSource[W](s: Src[W]): Source[W] =
    AsyncCont.toClassic(s.pull).plus[okay.freer.%[Writer, W]].flatMap {
      case Step.Done => okay.freer.pure(())
      case Step.Next(w, r) => Writer.tell(w).plus[Async].flatMap(_ => toSource(r))
    }
