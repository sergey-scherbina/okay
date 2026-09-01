package okay.agent

import okay.*
import okay.given

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
 */
object Stepper {

  /** the agent row with Tool stepped away */
  type Rest = Context + (Model + Async)

  /** a paused run: the call to inspect, and the rest of the run as a
   * function of the result you choose to give it */
  enum Stepping[A]:
    case Paused(call: ToolCall, resume: String => Stepping[A] ! Rest)
    case Done(a: A)

  /** the program, stepping: every Tool.Call becomes a pause */
  def stepped[A](prog: A ! Agent): Stepping[A] ! Rest =
    type DR = Delim + Rest
    val p = Delim.prompt[Stepping[A]]
    val widened: A ! (Tool + DR) =
      !.widen[A, Tool + Rest, Delim](prog)
    val translated: A ! DR =
      !.translate[A, Tool, DR](widened):
        [X] => (t: Tool[X]) => t match
          case Tool.Call(c) =>
            Delim.shift[Stepping[A], String, Rest](p) { k =>
              pure(Stepping.Paused(c, r => Delim.run(k(r))))
            }.map(s => s: X)   // Tool.Call refines X >: String; Free is invariant
    Delim.run(Delim.push(p)(translated.map(Stepping.Done(_))))

  /** drive a stepping run: `onCall` decides each pause's answer —
   * consult the real tool table, edit its output, or fabricate */
  def drive[A](s: Stepping[A] ! Rest)(onCall: ToolCall => String ! Rest): A ! Rest =
    s.flatMap {
      case Stepping.Done(a) => pure(a)
      case Stepping.Paused(c, resume) =>
        onCall(c).flatMap(r => drive(resume(r))(onCall))
    }

  /** the transparent driver: every pause performs the real tool —
   * stepping with nobody watching equals not stepping */
  def transparent[A](s: Stepping[A] ! Rest)
                    (table: Map[String, ToolCall => String]): A ! Rest =
    drive(s)(c => pure(table.get(c.name).fold(s"no tool: ${c.name}")(_(c))))
}
