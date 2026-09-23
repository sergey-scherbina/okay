package okay.clojure

import okay.{%, Free, Stage, Take, Writer, pure, split}
import okay.Free.{Bind, Inject, Return}
import clojure.lang.{AFn, IFn, RT, Reduced}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * A `Stage` IS a Clojure transducer (specs/clojure.md).
 *
 * Hickey's transducer \[Hickey 2014\] is a function of a reducing
 * function `rf` answering another, with three arities: `()` init,
 * `(acc x)` step, `(acc)` completion. A step may answer `(reduced acc)`
 * to stop the process; a stateful transducer keeps a `volatile!` made
 * when it is applied to `rf` and flushes it in the completion arity.
 * okay's `Stage[I, O, A]` — `A ! (Take % I + Writer % O)` — is that
 * written as a program: a `tell` is a call of `rf`, a stage that
 * ANSWERS is a step answering reduced, and the flush is what the stage
 * tells after its last `await` answered `None`. This is okay-java's
 * `Gather` (the JDK's gatherers) again, with Clojure's accumulator
 * threaded through `rf` where the JDK had a `Downstream`.
 *
 * TYPES AT THE SEAM. Clojure hands `Object`s: an element is tested
 * against the stage's input class (a `ClassTag` — Clojure's integers
 * are `java.lang.Long`, which a `ClassTag[Long]` accepts) and refused
 * by name when it is something else, rather than failing later as a
 * `ClassCastException` inside the stage.
 */
object Transducers {

  /** where a driven stage stands between two calls from Clojure */
  enum Pos[I, O, A]:
    case Fresh(stage: Stage[I, O, A])
    case Waiting(k: Option[I] => Stage[I, O, A])
    case Done()

  /**
   * An erased generic value handed to Clojure, which takes `Object`:
   * at run time it already IS one (a primitive `O` arrives boxed), so
   * the ascription checks nothing and cannot fail — the one place this
   * file says so.
   */
  private def boxed(a: Any): AnyRef = a.asInstanceOf[AnyRef]

  /** an element from Clojure, as the type the okay side declared */
  private def as[T](x: AnyRef, what: String)(using ct: ClassTag[T]): T = x match
    case ct(t) => t
    case other => throw IllegalArgumentException(
      s"okay.clojure.Transducers: $what expected ${ct.runtimeClass.getName}, got " +
        (if other == null then "nil" else other.getClass.getName))

  private def unreduced(x: AnyRef): AnyRef = x match
    case r: Reduced => r.deref()
    case v => v

  /**
   * Run the stage until it awaits or answers, feeding each tell to
   * `rf`. `ended`: the input is over — every await answers `None`
   * (the completion arity). A reduced answer from `rf` ends the drive:
   * the downstream wants nothing more, and the stage's continuation
   * past that tell is never built.
   */
  @tailrec private def drive[I, O, A](p: Stage[I, O, A], rf: IFn, acc: AnyRef,
                                      ended: Boolean): (Pos[I, O, A], AnyRef) =
    (p.resume: @unchecked) match
      case Return(_) => (Pos.Done(), acc)
      case Inject(e) => split[Take % I, Writer % O](e)
        { case Take.Await() => (Pos.Done[I, O, A](), acc) }
        { case Writer.Say(o) => (Pos.Done[I, O, A](), rf.invoke(acc, boxed(o))) }
      case Bind(Inject(e), k) => split[Take % I, Writer % O](e)
        { case Take.Await() =>
            if ended then drive(k(None), rf, acc, ended) else (Pos.Waiting[I, O, A](k), acc) }
        { w0 => (w0: @unchecked) match
            case Writer.Say(o) =>
              val next = rf.invoke(acc, boxed(o))
              if RT.isReduced(next) then (Pos.Done[I, O, A](), next)
              else drive(k(()), rf, next, ended) }

  /** one transducing process: the stage, fresh, bound to one `rf` */
  private final class Process[I: ClassTag, O, A](rf: IFn, stage: Stage[I, O, A]) extends AFn:
    private var pos: Pos[I, O, A] = Pos.Fresh(stage)

    override def invoke(): AnyRef = rf.invoke()

    override def invoke(acc: AnyRef, x: AnyRef): AnyRef =
      val i = as[I](x, "a transduced element")
      val (at, acc1) = pos match
        case Pos.Fresh(p) => drive(p, rf, acc, ended = false)
        case other => (other, acc)
      if RT.isReduced(acc1) then { pos = Pos.Done(); acc1 }
      else at match
        case Pos.Waiting(k) =>
          val (next, acc2) = drive(k(Some(i)), rf, acc1, ended = false)
          pos = next
          if RT.isReduced(acc2) then acc2
          else next match
            case Pos.Waiting(_) => acc2
            case _ => Reduced(acc2)   // the stage answered: stop the process
        case _ => pos = Pos.Done(); Reduced(acc1)

    override def invoke(acc: AnyRef): AnyRef =
      val (_, flushed) = pos match
        case Pos.Fresh(p) => drive(p, rf, acc, ended = true)
        case Pos.Waiting(k) => drive(k(None), rf, acc, ended = true)
        case done => (done, acc)
      pos = Pos.Done()
      rf.invoke(unreduced(flushed))

  /**
   * A stage as a Clojure transducer. Each application to a reducing
   * function starts the stage afresh — one process, one state, as a
   * `volatile!` would be — so the value composes with `comp` and runs
   * in `into`, `transduce`, `sequence`, `eduction` or a core.async
   * channel like any transducer of Clojure's own.
   */
  def of[I: ClassTag, O, A](stage: Stage[I, O, A]): IFn = new AFn:
    override def invoke(rf: AnyRef): AnyRef = rf match
      case f: IFn => Process[I, O, A](f, stage)
      case other => throw IllegalArgumentException(
        s"okay.clojure.Transducers.of: a transducer takes a reducing function, got ${other.getClass.getName}")

  /**
   * A Clojure transducer as a stage: `xf` is applied to a collecting
   * reducing function when the stage STARTS (a `Free.delay`), so each
   * run has its own `volatile!`s. A reduced step ends the stage after
   * the completion arity has flushed — `(take 3)` stops an okay
   * pipeline pulling, `(partition-all 3)` still hands over its tail.
   *
   * ONE-SHOT once built, as `okay.java.Gather.stage` is and for the
   * same reason: `through` runs a stage eagerly to its first output,
   * so the program it answers holds this run's transducer state, and
   * a `volatile!` cannot be snapshotted. A second run of the same
   * built program is refused by name; build it again instead.
   */
  def stage[I, O: ClassTag](xf: IFn): Stage[I, O, Unit] =
    Free.delay { () =>
      val out = ArrayBuffer.empty[O]
      val collect: IFn = new AFn:
        override def invoke(): AnyRef = scala.runtime.BoxedUnit.UNIT
        override def invoke(acc: AnyRef): AnyRef = acc
        override def invoke(acc: AnyRef, x: AnyRef): AnyRef = { out += as[O](x, "a transducer's output"); acc }
      val xrf: IFn = xf.invoke(collect) match
        case f: IFn => f
        case other => throw IllegalArgumentException(
          s"okay.clojure.Transducers.stage: applying the transducer answered ${other.getClass.getName}, not a function")
      var finished = false

      def live(): Unit =
        if finished then throw IllegalStateException(
          "okay.clojure.Transducers.stage: this pipeline already ran, and its transducer's state " +
            "is spent (a program built by `through` holds the state of its first run). " +
            "Build the pipeline again to run it again.")

      def flush(): Stage[I, O, Unit] =
        val batch = out.toVector
        out.clear()
        batch.foldLeft(pure(()): Stage[I, O, Unit])((p, o) => p.flatMap(_ => Stage.tell[I, O](o)))

      def finish(acc: AnyRef): Stage[I, O, Unit] =
        live()
        finished = true
        xrf.invoke(acc): Unit
        flush()

      def loop(acc: AnyRef): Stage[I, O, Unit] = Stage.await[I, O].flatMap {
        case Some(i) =>
          live()
          val r = xrf.invoke(acc, boxed(i))
          flush().flatMap(_ => if RT.isReduced(r) then finish(unreduced(r)) else loop(r))
        case None => finish(acc)
      }

      loop(xrf.invoke())
    }
}
