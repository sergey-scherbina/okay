package okay.foreign.workflow

import okay.{!, +, At, Delim, Wf, effect}
import okay.codec.Schema
import okay.foreign.{Condition, ForeignEval, PyNode, PyValue, Shape, ToPy, Wire}

/** a foreign call as a workflow QUESTION: the function's address and its
 * arguments (specs/foreign-workflow.md) */
final case class ForeignCall(address: String, args: Vector[PyValue])

/**
 * Foreign calls as workflow ACTIVITIES (foreign-workflow stage 1). An
 * activity is a question and its remembered answer (`Wf.perform`), so a
 * foreign call needs no mechanism of its own: the call is the question,
 * the worker is the oracle, and the journal, replay, crash-resume,
 * versioning and races of the durable layers apply as they are.
 *
 * The journalled answer is the wire's own written form of
 * `Either[Condition, PyValue]` — the format `Durable` journals a foreign
 * call in — so a FAILURE (an exception, a timeout, a dead worker) is a
 * recorded answer too, and a replay does not call the far side again.
 *
 * {{{
 * def trip(using w: Wf.Asks[ForeignCall, String, String, Pure]) = direct:
 *   val price = !ForeignActivity.call[Double]("shop:price")("tea")
 *   ...
 * dialogue.runWorkflowIn(ForeignActivity.oracle)   // under a worker's handler
 * }}}
 */
object ForeignActivity:

  /** the conditions that are the WIRE's, not the function's: the far
   * side died, went silent, could not be reached. None is an answer. */
  val transport: Set[String] = Set("WorkerDied", "timeout", "WorkerUnavailable", "WireError")

  /** an activity whose worker stayed unreachable through every attempt: the
   * step is left UNANSWERED, so nothing is journalled and the next run of
   * the workflow tries it again */
  final class Unreachable(val address: String, val last: Condition) extends IllegalStateException(
    s"the activity '$address' could not reach its worker (${last.kind}: ${last.message}); " +
      "nothing was journalled, so the workflow tries it again when it is run again")

  /**
   * The oracle: each question a call on whatever `ForeignEval` handler is
   * installed — a worker, a supervised worker, a pool — answered in the
   * wire's written form.
   *
   * What is journalled is the FUNCTION's answer: its value, or its own
   * exception. A failure of the WIRE (a dead worker, a deadline, no
   * connection) is not an answer, and recording it would make a network
   * blip the permanent history of the workflow. So it is retried, up to
   * `attempts` times — on a fresh worker when the handler is a supervisor
   * — and after that the oracle throws `Unreachable`, leaving the step
   * unanswered for the next run. That is at-least-once, the promise every
   * activity of the durable layers makes.
   */
  def oracle: ForeignCall => String ! ForeignEval = oracle(attempts = 3)

  /** the run ids this activity's programs are started under */
  private val runs = java.util.concurrent.atomic.AtomicLong()

  def oracle(attempts: Int): ForeignCall => String ! ForeignEval = c =>
    // a PROGRAM, not a `call`: every language serves it (Go's and Rust's
    // functions are direct-style). An activity offers no callbacks, so an
    // okay_call from the far side is ANSWERED with a refusal — its frame
    // must not be left waiting — and the function's own answer is the
    // activity's. DIRECT: a start that died is the retry loop's to redo.
    def settle(run: Long)(node: Either[Condition, PyNode]): Either[Condition, PyValue] ! ForeignEval = node match
      case Left(cond) => okay.pure[ForeignEval, Either[Condition, PyValue]](Left(cond))
      case Right(PyNode.Done(v)) => okay.pure[ForeignEval, Either[Condition, PyValue]](Right(v))
      case Right(PyNode.Perform(cb, _, k, _)) =>
        effect[ForeignEval, Either[Condition, PyNode]](ForeignEval.Continue(run, k, Left(Condition("NoCallback",
          s"the activity '${c.address}' called okay_call('$cb'), and an activity offers no callbacks")))).flatMap(settle(run))
    def attempt(left: Int): Either[Condition, PyValue] ! ForeignEval =
      val run = runs.incrementAndGet()
      effect[ForeignEval, Either[Condition, PyNode]](ForeignEval.Program(run, c.address, c.args, Vector.empty, direct = true))
        .flatMap(settle(run)).flatMap {
          case Left(cond) if transport(cond.kind) =>
            if left > 1 then attempt(left - 1) else throw Unreachable(c.address, cond)
          case answer => okay.pure[ForeignEval, Either[Condition, PyValue]](answer)
        }
    attempt(math.max(1, attempts)).map(answer => Wire.written(answer.map(Wire.enc)))

  /** what a journalled answer says, back as a value or a condition */
  def answer(written: String): Either[Condition, PyValue] = Wire.read(written).map(Wire.dec)

  /** a TYPED activity: the call is journalled, the answer decoded by
   * `Out`'s Schema; a far answer of the wrong shape is a `Left` */
  def call[Out: Schema](address: String)(using Shape): Call[Out] = Call(address)

  final class Call[Out: Schema](address: String)(using shape: Shape):
    def apply[R, F[+_]]()(using Wf.Asks[ForeignCall, String, R, F], At): Either[Condition, Out] ! Delim + F =
      go(Vector.empty)
    def apply[A: ToPy, R, F[+_]](a: A)(using Wf.Asks[ForeignCall, String, R, F], At): Either[Condition, Out] ! Delim + F =
      go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy, R, F[+_]](a: A, b: B)
                                         (using Wf.Asks[ForeignCall, String, R, F], At): Either[Condition, Out] ! Delim + F =
      go(Vector(ToPy(a), ToPy(b)))
    private def go[R, F[+_]](args: Vector[PyValue])
                            (using w: Wf.Asks[ForeignCall, String, R, F], at: At): Either[Condition, Out] ! Delim + F =
      w.perform(ForeignCall(address, args)).map(s => answer(s).flatMap(shape.decode[Out]))
