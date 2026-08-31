package okay.agent

import okay.Handler
import okay.codec.Schema
import scala.collection.mutable

/**
 * Tool results that do not fit — the context hog every agent meets
 * (a file, a query, a log). The doctrine is the one the retrieval
 * layer already uses: LOSSY IN THE VIEW, LOSSLESS IN THE LINEAGE.
 * The model sees a projection and a handle; the whole value is kept
 * where it can be re-read, and widening it costs a tool call rather
 * than a prompt that was inflated in advance for something the model
 * may never look at.
 *
 * This is a wrapper around any tool handler, so no tool has to know
 * about it — the policy lives in the handler, as always.
 */
object Large {

  /** what the model asks when it wants more of a kept result */
  final case class Expand(handle: String, from: Option[Int], length: Option[Int])
  given Schema[Expand] = Schema.derived

  val ExpandTool = "expand"

  /** the declaration to hand the model alongside its own tools */
  val spec: ToolSpec = ToolSpec[Expand](ExpandTool,
    "read more of a large tool result by its handle")

  /** where the whole values live — a session's worth of lineage */
  final class Store:
    private val kept = mutable.LinkedHashMap[String, String]()
    private var n = 0

    def put(text: String): String =
      n += 1
      val id = s"result-$n"
      kept(id) = text
      id

    def get(id: String): Option[String] = kept.get(id)
    def handles: Seq[String] = kept.keys.toSeq
    def size: Int = kept.size

  /**
   * Wrap a tool handler. A result at or under `limit` passes through
   * untouched; a longer one is stored whole and replaced by its first
   * `window` characters, the handle, and the total size — enough for
   * the model to decide whether it needs more.
   */
  def projecting(inner: Handler[Tool], store: Store,
                 limit: Int = 1000, window: Int = 300): Handler[Tool] = new:
    def handle[A](e: Tool[A]): A = e match
      case Tool.Call(c) if c.name == ExpandTool =>
        (ToolSpec.args[Expand](c) match
          case Left(err) => s"error: $err"
          case Right(a) => store.get(a.handle) match
            case None => s"error: no such result '${a.handle}'"
            case Some(full) =>
              val from = a.from.getOrElse(0).max(0).min(full.length)
              val len = a.length.getOrElse(window).max(0)
              val to = (from + len).min(full.length)
              val more = full.length - to
              val tail = if more > 0 then s"\n[$more more characters]" else ""
              full.substring(from, to) + tail
        ).asInstanceOf[A]

      case Tool.Call(_) =>
        val full = inner.handle(e).asInstanceOf[String]
        (if full.length <= limit then full
        else
          val id = store.put(full)
          s"[$id: ${full.length} characters, showing the first $window; " +
            s"use the $ExpandTool tool with handle=$id for more]\n" +
            full.take(window)).asInstanceOf[A]
}
