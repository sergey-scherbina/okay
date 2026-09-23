package okay.clojure

import okay.{Foreign, Free, Push, Stage, pure}
import clojure.lang.{AFn, IFn, RT, Reduced}
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

  import okay.Push.Pos

  private def boxed(a: Any): AnyRef = Foreign.obj(a)

  /** an element from Clojure, as the type the okay side declared */
  private def as[T: ClassTag](x: AnyRef, what: String): T = Foreign.as[T](x, what, "okay.clojure.Transducers")

  private def unreduced(x: AnyRef): AnyRef = x match
    case r: Reduced => r.deref()
    case v => v

  /**
   * One transducing process: the stage, fresh, bound to one `rf`, driven
   * by okay-stream's `Push` (interop-shared). The accumulator is threaded
   * by the process's `emit`: each tell is a call of `rf`, and a reduced
   * answer refuses the push — the downstream wants nothing more, and the
   * stage's continuation past that tell is never built.
   */
  private final class Process[I: ClassTag, O, A](rf: IFn, stage: Stage[I, O, A]) extends AFn:
    private var pos: Pos[I, O, A] = Pos.Fresh(stage)
    private var acc: AnyRef = null
    private val emit: O => Boolean = o =>
      acc = rf.invoke(acc, boxed(o))
      !RT.isReduced(acc)

    override def invoke(): AnyRef = rf.invoke()

    override def invoke(acc0: AnyRef, x: AnyRef): AnyRef =
      val i = as[I](x, "a transduced element")
      acc = acc0
      pos = Push.offer(pos, i, emit)
      if RT.isReduced(acc) then { pos = Pos.Done(); acc }
      else pos match
        case Pos.Waiting(_) => acc
        case _ => Reduced(acc)   // the stage answered: stop the process

    override def invoke(acc0: AnyRef): AnyRef =
      acc = acc0
      pos = Push.end(pos, emit)
      rf.invoke(unreduced(acc))

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
   * A program BUILT over it is a value too, as over
   * `okay.java.Gather.stage`: `through` starts its drive when the
   * program runs (windows-stage-rerun-loses-pane), so each run of the
   * built program applies `xf` afresh. What cannot be replayed is a
   * continuation from INSIDE a run, resumed again after the run
   * finished — a `volatile!` cannot be snapshotted — and that is
   * refused by name, before the spent state is stepped.
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
            "is spent (a continuation from inside that run was resumed after it finished). " +
            "Run the built program from its start to run it again.")

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
