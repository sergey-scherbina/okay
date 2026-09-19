package okay.agent

import okay.*

/**
 * The stepper (specs/llm-agentic.md, "The stepper"): run an agent
 * program under a debugger's hand — it PAUSES at every tool call,
 * the operator inspects the call (and may perform it, fake it, or
 * edit its result), and the program resumes none the wiser.
 *
 * Delim is the foundation, and earns it twice over. The pause is a
 * `shift` to a typed prompt: the captured continuation IS "the rest
 * of the agent run", reified as an ordinary program value — which is
 * exactly what `resume` hands back. And because Delim's captured
 * continuations are multi-shot (the machine reifies segments into
 * programs, closing over no interpreter state), one pause can be
 * resumed MORE THAN ONCE: fork the run at a tool call, feed two
 * different results, compare the futures. A debugger that can ask
 * "what if the tool had said X instead" is the staged-relay promise
 * made concrete.
 *
 * ── IT IS A DIALOGUE, AND SAYING SO DELETED HALF OF IT
 * (delim-patterns-in-modules, 2026-09-17). This file was written
 * before `Delim` had the named patterns, so it carried its own
 * `Stepping` enum — a `Paused(call, resume)` beside a `Done(a)` —
 * and its own driver. That is `Delim.Paused` and `Delim.drive`,
 * exactly, and a stepping run is a dialogue whose questions are tool
 * calls and whose answers are their results.
 *
 * WHAT IT DOES NOT BUY, and this was written down the other way
 * round first. The backlog entry that asked for this rewrite said the
 * stepper would gain `Delim.replay` — a session that survives the
 * process — for free. It does not, and the type system says why:
 * replay requires `Replayable[Delim + Rest]`, and `Rest` is
 * `Context + (Model + Async)`. Re-running a stepping session would
 * ASK THE MODEL AGAIN. A stepping run is a dialogue in shape and a
 * live one in substance; to make it durable, the model's replies have
 * to become answers in the journal too, which is a different feature
 * (specs/durable-workflow.md) and not a consequence of this one.
 *
 * What it does buy: the deletion, one driver instead of two, and
 * `Paused` and `drive` maintained in one place for every consumer.
 */
object Stepper {

  /** the agent row with Tool stepped away */
  type Rest = Context + (Model + Async)

  /**
   * A paused run: the call to inspect, and the rest of the run as a
   * function of the result you choose to give it — which is what a
   * `Delim.Dialogue` is, with `ToolCall` for the question and the
   * tool's result for the answer.
   */
  type Stepping[A] = Delim.Dialogue[ToolCall, String, A, Rest]

  /** the program, stepping: every Tool.Call becomes a pause */
  def stepped[A](prog: A ! Agent): Stepping[A] ! Rest =
    Delim.resumable[ToolCall, String, A, Rest]:
      val widened: A ! (Tool + (Delim + Rest)) =
        !.widen[A, Tool + Rest, Delim](prog)
      !.translate[A, Tool, Delim + Rest](widened):
        [X] => (t: Tool[X]) => t match
          case Tool.Call(c) =>
            // `ask`, not `pause`: a natural transformation is not a
            // direct block, and this is the door that lane added
            Delim.ask[ToolCall, String, A, Rest](c)
              .map(s => s: X)   // Tool.Call refines X >: String; Free is invariant

  /** drive a stepping run: `onCall` decides each pause's answer —
   * consult the real tool table, edit its output, or fabricate */
  def drive[A](s: Stepping[A] ! Rest)(onCall: ToolCall => String ! Rest): A ! Rest =
    s.flatMap(Delim.drive[ToolCall, String, A, Rest](_)(onCall))

  /** the transparent driver: every pause performs the real tool —
   * stepping with nobody watching equals not stepping */
  def transparent[A](s: Stepping[A] ! Rest)
                    (table: Map[String, ToolCall => String]): A ! Rest =
    drive(s)(c => pure(table.get(c.name).fold(s"no tool: ${c.name}")(_(c))))
}
