package okay.agent

import okay.Answers

/** Source-compatible agent names over the neutral implementation.
 * Recompile clients when upgrading: aliases do not preserve JVM class names.
 */
type OpTrace = okay.durable.OpTrace
type Journalled[Op[_]] = okay.codec.Journalled[Op]

object Durable:
  export okay.durable.Durable.{OnRepeat, Entry, Journal, MemoryJournal,
    Drift, Unresolved, Awaiting, argsOf, awaiting, over, replayingOver}

  /** The Tool wire field, owned by the Tool adapter. */
  val KeyField = "idempotency_key"

  def keyFor(seq: Int, c: ToolCall): String =
    okay.durable.Durable.keyFor(seq, Tool.Call(c))

  /**
   * The durable TOOL handler: `over` at `Tool`, and the signature the
   * callers had before there was an `over`. Every behaviour is the
   * instance's — the `idempotency_key` field, the span attributes,
   * the `Awaiting` carrying the call's arguments — so this is a
   * spelling, not a second implementation.
   */
  def tools(inner: Answers[Tool], journal: Journal)
           (policy: String => OnRepeat = _ => OnRepeat.Fail,
            reconcile: (ToolCall, String) => Option[String] = (_, _) => None,
            escalate: (ToolCall, String) => Option[String] = (_, _) => None,
            trace: Option[OpTrace] = None)
  : Answers[Tool] =
    def onCall(f: (ToolCall, String) => Option[String]): [X] => (Tool[X], String) => Option[String] =
      [X] => (op: Tool[X], key: String) => op match { case Tool.Call(c) => f(c, key) }
    over[Tool](inner, journal)(policy, onCall(reconcile), onCall(escalate), trace)

  /** Replay at Tool, preserving the original agent convenience API. */
  def replaying(journal: Journal, trace: Option[OpTrace] = None): Answers[Tool] =
    replayingOver[Tool](journal, trace)
