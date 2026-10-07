package okay.agent

import okay.freer.{+}
import okay.freer.{!, Aggregator}
/**
 * The context handler that THREADS its state instead of holding it
 * (mirrors State.handle, and for the same reason): the compaction
 * accumulator is carried through the walk as a value, so a
 * continuation invoked twice — a multi-shot Choose branch, a
 * backtracking search — sees the context as it was AT THAT POINT,
 * not as some other branch left it. A mutable handler would leak one
 * branch's turns into its sibling; this one cannot.
 *
 * The pay-off is that handler ORDER now says what you mean:
 * `Memory.handle` inside the search gives every branch its own
 * conversation (speculative exploration), outside it gives one shared
 * conversation across branches (a transcript of everything tried).
 */
object Memory {

  /**
   * Handle Context with the policy, threading its accumulator; the
   * residual program keeps whatever effects F remain. Answers the
   * final state beside the value, like State.handle does.
   */
  def handle[S, A, F[+_]](policy: Aggregator[Turn, S, Seq[Turn]])
                                     (init: S)(prog: A ! Context + F): (S, A) ! F = {
    def answer[X](s: S, e: Context[X]): (S, X) = e match
      case Context.Remember(t) => (policy.add(s, t), ())
      case Context.Recall() => (s, policy.present(s))
      case Context.Mark() => (s, Snapshot(s))
      case Context.Restore(m) => (m.stateAs[S], ())

    // one step on the state engine, both faces (handler-one-step)
    okay.freer.HandleFrames.stateRun[Context, S, A, (S, A), F](summon[okay.TypeableK[Context]], (s, a) => okay.freer.pure((s, a)))(
      (s, op) => answer(s, op.asInstanceOf[Context[Any]]))(init, prog)
  }

  /** the common case: start empty, keep the answer only */
  def run[S, A, F[+_]](policy: Aggregator[Turn, S, Seq[Turn]])
                                  (prog: A ! Context + F): A ! F =
    handle(policy)(policy.init)(prog).map(_._2)

  /** …and the transcript beside it, for inspection or a next turn */
  def runWithState[S, A, F[+_]](policy: Aggregator[Turn, S, Seq[Turn]])
                                           (prog: A ! Context + F): (S, A) ! F =
    handle(policy)(policy.init)(prog)
}
