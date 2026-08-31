package okay.agent

import okay.{!, +, Aggregator, Handler}
import okay.given
import okay.lex.Scan
import okay.lex.Bpe

/**
 * Where the policy lives. Each handler answers ONE effect, so an
 * agent program is re-targeted by swapping handlers, not by editing
 * it: the same program runs against a live model or a canned script,
 * executes tools or asks a human, keeps everything in context or
 * compacts it hard. This is also why an agent needs no mocking
 * framework — a different handler IS the mock.
 */
object Handlers {

  // ---------------------------------------------------------------- context

  /**
   * The context handler: a compaction policy plus its state. Recall
   * presents the policy's view, so the view is ALWAYS within budget
   * and the agent never mentions compaction. Mark/Restore hand out
   * and take back the state — a persistent value, so backtracking
   * over the conversation is free.
   */
  final class ContextState[S](policy: Aggregator[Turn, S, Seq[Turn]]):
    private var acc: S = policy.init

    def remember(t: Turn): Unit = acc = policy.add(acc, t)

    def recall: Seq[Turn] = policy.present(acc)

    def mark: Snapshot = Snapshot(acc)

    def restore(s: Snapshot): Unit = acc = s.state.asInstanceOf[S]

    /** the whole history the policy kept, uncompacted by present */
    def state: S = acc

  def context[S](policy: Aggregator[Turn, S, Seq[Turn]]): (ContextState[S], Handler[Context]) =
    val st = ContextState(policy)
    (st, new Handler[Context]:
      def handle[A](e: Context[A]): A = e match
        case Context.Remember(t) => st.remember(t)
        case Context.Recall() => st.recall
        case Context.Mark() => st.mark
        case Context.Restore(s) => st.restore(s))

  // ---------------------------------------------------------------- tools

  /** execute tools from a table; an unknown tool is an ANSWER, not a
   * fault — the model must be able to recover from its own mistake */
  def tools(table: Map[String, ToolCall => String]): Handler[Tool] = new:
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) => table.get(c.name) match
        case Some(f) =>
          try f(c)
          catch case ex: Throwable => s"error: ${ex.getMessage}"
        case None => s"error: no such tool '${c.name}'"

  /** approve every call through a gate first (human in the loop, a
   * sandbox, a rate limiter — the same shape) */
  def gated(table: Map[String, ToolCall => String])
           (approve: ToolCall => Boolean): Handler[Tool] = new:
    private val inner = tools(table)
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) =>
        if approve(c) then inner.handle(e)
        else "denied"

  /** record every call, then delegate — replay is the same table */
  def recording(inner: Handler[Tool])(log: scala.collection.mutable.Buffer[ToolCall])
  : Handler[Tool] = new:
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) => log += c; inner.handle(e)

  /**
   * Tools as a program transformer rather than a comonadic handler:
   * the operations are tail-resumptive, so a relay answers them and
   * hands the program back with the row one effect shorter. This is
   * what a platform with no threads needs — `runWith` wants a handler
   * for the WHOLE row, Async included, and on JS there is none.
   */
  def relayTools[A, F[+_] : okay.TypeableK](table: Map[String, ToolCall => String])
                                           (prog: A ! (Tool + F)): A ! F =
    def answer(c: ToolCall): String = table.get(c.name) match
      case Some(f) =>
        try f(c) catch case ex: Throwable => s"error: ${ex.getMessage}"
      case None => s"error: no such tool '${c.name}'"

    okay.!.translate[A, Tool, F](prog) {
      [X] => (e: Tool[X]) => e match
        case Tool.Call(c) => okay.pure(answer(c).asInstanceOf[X])
    }

  // ---------------------------------------------------------------- model

  /** the local tokenizer: counting needs no provider */
  def counter(bpe: Bpe): String => Int = s =>
    Scan.all(bpe)(s).tokens.count(_.channel == okay.lex.Channel.Syntax)

  /**
   * A scripted model: the canned replies in order, then a final
   * answer. Everything the tests need, and the shape any real
   * provider handler has (okay-llm's Anthropic client plugs in here).
   */
  def scripted(replies: Seq[Reply], count: String => Int = _.length / 4)
  : Handler[Model] = new:
    private var rest = replies.toList
    def handle[A](e: Model[A]): A = e match
      case Model.Complete(_, _) => rest match
        case r :: t => rest = t; r
        case Nil => Reply("done", Nil)
      case Model.Count(text) => count(text)

  /** a model that sees the context — for asserting WHAT was sent */
  def observing(replies: Seq[Reply], seen: scala.collection.mutable.Buffer[Seq[Turn]],
                count: String => Int = _.length / 4): Handler[Model] = new:
    private val inner = scripted(replies, count)
    def handle[A](e: Model[A]): A = e match
      case c @ Model.Complete(ctx, _) => seen += ctx; inner.handle(c)
      case other => inner.handle(other)
}
