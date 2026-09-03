package okay.demo

import okay.given
import okay.agent.{ToolCall, ToolSpec}
import okay.codec.Json
import okay.mcp.{Mcp, Server, Stdio}

import java.io.File
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicReference

/**
 * Bounded execution state (specs/llm-agentic.md, "Bounded execution
 * state") offered to any MCP client — Claude Code by name — as three
 * tools over one durable JSON value, Sigma, persisted to a file.
 *
 *   CP=$(sbt -batch --error "export okayDemo/Runtime/fullClasspath" | tail -1)
 *   java -cp "$CP" okay.demo.StateMcp /path/to/state.json
 *
 * Register it with Claude Code (`.mcp.json` in a project, or
 * `claude mcp add`):
 *
 *   {"mcpServers": {"state": {"command": "java",
 *     "args": ["-cp", "<CP>", "okay.demo.StateMcp", ".claude/state.json"]}}}
 *
 * The point this makes concrete: Claude Code's own conversation is
 * not ours to intercept — it keeps its transcript, compacts it its
 * own way, and nothing here changes that. What CAN cross that
 * boundary is an external, typed, PATCHED fact store that survives
 * `/clear` and `/compact` because it never lived in the transcript at
 * all — call `get_state` after either to recover exactly where a task
 * stood, `update_state` whenever something worth keeping is learned.
 *
 * `Compact.skillState`/`Turn.StatePatch` (the skill-state lane) stay
 * where they belong — OUR agents' own `Context` loop, built on our
 * `Turn`/`Aggregator`. Claude Code is not built on that, so this
 * server reuses only the format-level primitive both would need
 * either way: `Json.mergePatch`, RFC 7396, unconditionally. Schema
 * validation before a patch lands (`Compact.validatePatch`'s role in
 * our own agents) needs a compile-time `Schema[S]` for a KNOWN shape;
 * a tool a different, arbitrary project points at cannot assume one,
 * so this accepts any JSON OBJECT patch and lets the state's actual
 * shape be a convention between the caller and whoever reads the
 * file — the same trust boundary any hand-edited JSON file already
 * has. A project that wants typed validation defines its own `Schema`
 * and calls `Compact.validatePatch` in its own copy of `updateState`;
 * the hook is a four-line function, not a fork.
 */
object StateMcp {

  /** the one thing this server has: Sigma, and where it lives */
  final class Store(file: File):
    private val sigma: AtomicReference[Json] = AtomicReference(load())

    /** the parser is total and lossless (specs/codecs.md): damage
     * survives as a `JErr` leaf, or — inside an object's fields, a
     * garbled key-value pair — as a synthetic `"<message>"` key
     * (`Json.scala`'s own `pairs`), so both are checked recursively */
    private def damaged(j: Json): Boolean = j match
      case _: Json.JErr => true
      case Json.JObj(fs) => fs.exists((k, v) => k.startsWith("<") || damaged(v))
      case Json.JArr(vs) => vs.exists(damaged)
      case _ => false

    private def load(): Json =
      if !file.exists() then Json.JObj(Vector.empty)
      else
        val parsed = Json.parse(Files.readString(file.toPath))
        if damaged(parsed) then
          System.err.println(s"state-mcp: damaged state file, starting empty (${Json.print(parsed)})")
          Json.JObj(Vector.empty)
        else parsed

    private def persist(j: Json): Unit =
      Option(file.getAbsoluteFile.getParentFile).foreach(_.mkdirs())
      Files.writeString(file.toPath, Json.print(j)): Unit

    def get: Json = sigma.get()

    /** RFC 7396 only — no shape is assumed, none is enforced here;
     * see the module doc for where per-project validation belongs */
    def merge(patch: Json): Json =
      val next = sigma.updateAndGet(cur => Json.mergePatch(cur, patch))
      persist(next)
      next

    def reset(): Json =
      val empty = Json.JObj(Vector.empty)
      sigma.set(empty)
      persist(empty)
      empty

  private def objectSchema(extra: (String, Json)*): Json =
    Json.JObj(Vector("type" -> Json.JStr("object"), "additionalProperties" -> Json.JBool(true)) ++ extra)

  def tools: Seq[ToolSpec] = Seq(
    ToolSpec("get_state",
      "Read the durable task state (a JSON object), independent of the conversation. " +
        "Call this at the start of a task, and always right after a compaction or a " +
        "fresh session, to recover exactly where the task stood — the conversation " +
        "history may be gone or summarized; this is not.",
      objectSchema()),
    ToolSpec("update_state",
      "Merge a JSON Merge Patch (RFC 7396) into the durable task state and persist it. " +
        "An object field merges recursively; setting a field to null DELETES it; any " +
        "other value replaces it wholesale. Omit fields that did not change. Call this " +
        "whenever a fact is learned or a decision is made that must survive to the next " +
        "turn or a fresh session — do not rely on the conversation to carry it.",
      objectSchema()),
    ToolSpec("reset_state",
      "Clear the durable task state back to an empty object. Call this only when " +
        "starting a genuinely new task, not between steps of the same one.",
      objectSchema()))

  def handlers(store: Store): Map[String, ToolCall => String] = Map(
    "get_state" -> { (_: ToolCall) => Json.print(store.get) },
    "update_state" -> { (c: ToolCall) =>
      c.args match
        case _: Json.JObj => Json.print(store.merge(c.args))
        case other => throw RuntimeException(
          s"update_state expects a JSON object patch (RFC 7396), got: ${Json.print(other)}")
    },
    "reset_state" -> { (_: ToolCall) => Json.print(store.reset()) })

  def main(args: Array[String]): Unit =
    val file = File(args.headOption.getOrElse(
      sys.env.getOrElse("STATE_MCP_FILE", ".claude/state.json")))
    val store = Store(file)
    System.err.println(s"state-mcp: state file ${file.getAbsolutePath}")

    Server.run(Stdio.std, Mcp.Info("okay-state", "0.1"), tools, handlers(store)).runWith
}
