package okay.java

import okay.{Free, Push, Stage, pure}
import java.util.stream.Gatherer
import scala.collection.mutable.ArrayBuffer

/**
 * A `Stage` IS a `Gatherer` (JDK 24, JEP 485).
 *
 * `Collect` says an okay `Aggregator` is a JDK `Collector` — the
 * terminal fold, both sides the same four parts. This is the same
 * statement one step earlier in the pipeline. A `Gatherer[T, A, R]` is
 * a user-defined INTERMEDIATE operation: an initializer, an integrator
 * `(state, element, downstream) => boolean` that may push any number of
 * elements downstream and answer `false` to end the stream early, and a
 * finisher that may push what is left. okay's `Stage[I, O, A]` —
 * `A ! Take % I + Writer % O` — is exactly that, written as a
 * program: every `await` is where the integrator returns, every `tell`
 * is a `push`, and a stage that ANSWERS is a gatherer that
 * short-circuits.
 *
 * The state a Gatherer threads is therefore the stage itself,
 * suspended at its next `await` — the continuation `k` of that await.
 * The JDK hands it back with the next element; `k(Some(i))` resumes the
 * program, which runs to its next await, pushing as it tells. The
 * finisher resumes it with `None` — the end of the input — until it
 * answers.
 *
 * SEQUENTIAL, and on purpose: no combiner. A suspended program is a
 * position in the stream; two of them cannot be merged, because a
 * parallel split holds a RANGE and not a prefix (the finding
 * `Windowed`'s collector records, where the combiner had to throw).
 * A Gatherer can say this properly — JEP 485 evaluates a combiner-less
 * gatherer in encounter order even inside a `.parallel()` stream.
 *
 * Needs JDK 24+ at RUN time. okay-java still loads on 17/21: the JVM
 * links `Gatherer` only when something in this object is called.
 */
object Gather {

  import okay.Push.Pos

  /** the Gatherer's mutable state: one cell per evaluation; the stage is
   * driven by okay-stream's `Push` (interop-shared), a refused
   * `Downstream.push` ending the drive */
  final class Cell[I, O, A](var pos: Pos[I, O, A])

  /**
   * A stage as a Gatherer: `stream.gather(Gather.gatherer(stage))`.
   * Each element resumes the stage with `Some(i)`; a stage that
   * answers makes the integrator return `false`, so the upstream is
   * not pulled again — an infinite `Stream.iterate` ends where a
   * `transduceUntil` does.
   */
  def gatherer[I, O, A](stage: Stage[I, O, A]): Gatherer[I, Cell[I, O, A], O] =
    Gatherer.ofSequential[I, Cell[I, O, A], O](
      () => Cell(Pos.Fresh(stage)),
      (cell: Cell[I, O, A], i: I, ds: Gatherer.Downstream[? >: O]) => {
        cell.pos = Push.offer(cell.pos, i, o => ds.push(o))
        cell.pos match
          case Pos.Waiting(_) => !ds.isRejecting
          case _ => false
      },
      (cell: Cell[I, O, A], ds: Gatherer.Downstream[? >: O]) => {
        cell.pos = Push.end(cell.pos, o => ds.push(o))
      })

  /**
   * The other direction: a JDK Gatherer as a stage, so the JDK's own
   * `Gatherers.windowSliding`, `scan`, `fold` — or any third-party
   * gatherer — runs inside an okay pipeline (`through`, `pipe`).
   *
   * The Gatherer's state is made when the stage STARTS, not when this
   * is called (a `Free.delay`): the same stage value run twice must
   * not share one mutable state. The integrator pushes into a buffer
   * the stage then tells, element by element; when it answers `false`
   * the stage stops awaiting — `through` then pulls nothing more from
   * upstream — and, as in a JDK stream, the finisher still runs.
   *
   * A program BUILT over it is a value too: `through` starts its
   * drive when the program runs, not when it is built (Pipe.scala,
   * windows-stage-rerun-loses-pane, 2026-09-23), so each run of the
   * built program reaches this `delay` and makes its own state. It
   * used to drive the stage eagerly to its first output, and the
   * built program then held this run's gatherer — measured, a second
   * run NPE'd inside the JDK's `windowFixed` (its finisher nulls its
   * array). What CANNOT be replayed is a continuation from inside a
   * run — the rest after some output, resumed again after the run
   * finished (a Free continuation is multi-shot): the JDK's state is
   * an opaque mutable object that cannot be snapshotted, so that is
   * REFUSED by name below, before the JDK's state is touched.
   */
  def stage[I, S, O](g: Gatherer[I, S, O]): Stage[I, O, Unit] =
    val init = g.initializer
    val integrator = g.integrator
    val finisher = g.finisher
    Free.delay { () =>
      val state = init.get()
      val out = ArrayBuffer.empty[O]
      val ds: Gatherer.Downstream[O] = o => { out += o; true }
      // the gatherer is finished: anything after this is a continuation
      // of this run resumed a second time (see above)
      var finished = false

      def live(): Unit =
        if finished then throw IllegalStateException(
          "okay.java.Gather.stage: this pipeline already ran, and its JDK gatherer's state " +
            "is spent (a continuation from inside that run was resumed after it finished). " +
            "Run the built program from its start to run it again.")

      def flush(): Stage[I, O, Unit] =
        val batch = out.toVector
        out.clear()
        batch.foldLeft(pure(()): Stage[I, O, Unit])((p, o) => p.flatMap(_ => Stage.tell[I, O](o)))

      def finish(): Stage[I, O, Unit] =
        live()
        finished = true
        finisher.accept(state, ds)
        flush()

      def loop: Stage[I, O, Unit] = Stage.await[I, O].flatMap {
        case Some(i) =>
          live()
          val more = integrator.integrate(state, i, ds)
          flush().flatMap(_ => if more then loop else finish())
        case None => finish()
      }

      loop
    }
}
