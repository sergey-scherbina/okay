package okay

/**
 * Server-sent events framing: lines in, each event's joined `data:`
 * fields out; a partial trailing event flushes.
 *
 * Here beside `Lines` rather than in okay-llm, where it was written for
 * streamed completions: okay-http's SSE client needs it too, and
 * reaching it there made every HTTP user carry the LLM client (and,
 * through okay-mcp, an agent and RAG) — http-mcp-agent-edge,
 * 2026-09-25. `okay.llm.Sse.events` is this.
 */
object Sse:
  def events: Stage[String, String, Unit] =
    def flush(buf: List[String]): Stage[String, String, List[String]] =
      if buf.isEmpty then pure(Nil)
      else Stage.tell[String, String](buf.reverse.mkString("\n")).map(_ => Nil)

    // named rather than inlined into the `.map`: as the receiver of a
    // call the transduce gets no expected type, and its input type
    // has nothing else to be inferred from
    val framed: Stage[String, String, List[String]] =
      Stage.transduce(List.empty[String])((buf, line) =>
        if line.isEmpty then flush(buf)
        else if line.startsWith("data:") then pure(line.drop(5).trim :: buf)
        else pure(buf),   // comments, event:, id: — framing we do not need yet
        flush)

    framed.map(_ => ())
