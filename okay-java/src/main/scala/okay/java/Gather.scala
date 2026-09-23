package okay.java

import okay.{!, %, +, Free, Stage, Take, Writer, pure, split}
import okay.Free.{Bind, Inject, Return}
import java.util.stream.Gatherer
import scala.annotation.tailrec
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
 * `A ! (Take % I + Writer % O)` — is exactly that, written as a
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

  /** where a driven stage stands, between two calls from the JDK */
  enum Pos[I, O, A]:
    /** not started: the stage may tell before its first await, and
     * there is no downstream to push to until the first element (or
     * the finisher, on an empty stream) supplies one */
    case Fresh(stage: Stage[I, O, A])
    /** suspended at an await; the JDK's next element resumes it */
    case Waiting(k: Option[I] => Stage[I, O, A])
    /** answered, or a push was refused: nothing more to do */
    case Done()

  /** the Gatherer's mutable state: one cell per evaluation */
  final class Cell[I, O, A](var pos: Pos[I, O, A])

  /**
   * Run the stage until it awaits or answers, pushing every tell.
   * `ended`: the input is over, so every await is answered `None` and
   * the run goes on to the answer — the finisher's mode.
   *
   * A refused push ends the drive: `Downstream.push` answering false
   * means nothing downstream wants more (a `limit`, a `findFirst`),
   * and the stage's own continuation past that tell is never built.
   * A final bare `await` (the stage's answer IS the next element) ends
   * it too: nothing can be told after it.
   */
  @tailrec def drive[I, O, A](p: Stage[I, O, A], ds: Gatherer.Downstream[? >: O],
                              ended: Boolean): Pos[I, O, A] =
    (p.resume: @unchecked) match
      case Return(_) => Pos.Done()
      case Inject(e) => split[Take % I, Writer % O](e)
        { case Take.Await() => Pos.Done[I, O, A]() }
        { case Writer.Say(o) => ds.push(o): Unit; Pos.Done[I, O, A]() }
      case Bind(Inject(e), k) => split[Take % I, Writer % O](e)
        { case Take.Await() =>
            if ended then drive(k(None), ds, ended) else Pos.Waiting[I, O, A](k) }
        { w0 => (w0: @unchecked) match
            case Writer.Say(o) =>
              if ds.push(o) then drive(k(()), ds, ended) else Pos.Done[I, O, A]() }

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
        val at = cell.pos match
          case Pos.Fresh(p) => drive(p, ds, ended = false)
          case other => other
        cell.pos = at match
          case Pos.Waiting(k) => drive(k(Some(i)), ds, ended = false)
          case other => other
        cell.pos match
          case Pos.Waiting(_) => !ds.isRejecting
          case _ => false
      },
      (cell: Cell[I, O, A], ds: Gatherer.Downstream[? >: O]) => {
        cell.pos = cell.pos match
          case Pos.Fresh(p) => drive(p, ds, ended = true)
          case Pos.Waiting(k) => drive(k(None), ds, ended = true)
          case done => done
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
   * ONE-SHOT once built, and it says so. `through(p)(stage)` runs the
   * stage eagerly up to its first output, so the program it answers
   * already holds this run's gatherer state; running THAT program a
   * second time would feed the same state again — measured, the JDK's
   * `windowFixed` then fails with an NPE inside its own array (its
   * finisher nulls it). A JDK gatherer's state is an opaque mutable
   * object that cannot be snapshotted, so the second run is REFUSED
   * with a message instead. Build the pipeline again (call `through`
   * again) to run it again; that is always fine.
   */
  def stage[I, S, O](g: Gatherer[I, S, O]): Stage[I, O, Unit] =
    val init = g.initializer
    val integrator = g.integrator
    val finisher = g.finisher
    Free.delay { () =>
      val state = init.get()
      val out = ArrayBuffer.empty[O]
      val ds: Gatherer.Downstream[O] = o => { out += o; true }
      // the gatherer is finished: anything after this is a second run
      // of a program that already consumed this state (see above)
      var finished = false

      def live(): Unit =
        if finished then throw IllegalStateException(
          "okay.java.Gather.stage: this pipeline already ran, and its JDK gatherer's state " +
            "is spent (a program built by `through` holds the state of its first run). " +
            "Build the pipeline again to run it again.")

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
