package okay.demo

import okay.given
import okay.agent.Turn
import okay.mcp.{Mcp, Server, Stdio}

/**
 * This repository, as an MCP server: everything the demo agent knows
 * about this codebase, offered to any MCP client — an editor, another
 * agent, Claude Code.
 *
 *   CP=$(sbt -batch --error "export okayDemo/Runtime/fullClasspath" | tail -1)
 *   java -cp "$CP" okay.demo.RepoMcp /path/to/repo
 *
 * All three capabilities, and each is the demo's own data wearing the
 * protocol's clothes:
 *
 *   - TOOLS are the agent's own two (a definition by name, a file by
 *     path), schemas derived from the same `Schema` that parses their
 *     arguments;
 *   - RESOURCES are the indexed files, so a client can read the
 *     source it is being told about;
 *   - a PROMPT is a conversation opening about one definition, which
 *     is `Seq[Turn]` — the same thing an agent's context is made of.
 *
 * It is wiring and nothing else. Note the one rule stdio imposes:
 * only protocol goes to stdout, so the indexing report goes to
 * stderr, where a server's logs belong.
 */
object RepoMcp {

  private val scheme = "okay://"

  def main(args: Array[String]): Unit =
    val root = java.io.File(
      args.headOption.getOrElse(sys.env.getOrElse("OKAY_REPO", ".")))
    val sources = RepoAgent.load(root)
    val repo = RepoAgent.index(sources)
    System.err.println(
      s"okay-repo: indexed ${sources.length} files, ${repo.index.names.size} definitions")

    val explain = Mcp.Prompt("explain", "explain a definition in this repository",
      Seq(Mcp.Prompt.Arg("name", "the name to explain", required = true)))

    Server.run(Stdio.std, Server.Serving(
      info = Mcp.Info("okay-repo", "0.1"),
      tools = Seq(RepoAgent.definitionTool, RepoAgent.readTool),
      call = RepoAgent.tools(repo),
      resources = sources.map(s =>
        Mcp.Resource(scheme + s.id, s.id, "a file of this repository",
          Some("text/plain"))),
      read = uri => repo.corpus.sources.get(uri.stripPrefix(scheme)).map(_.text),
      prompts = Seq(explain),
      prompt = (name, args) => Option.when(name == "explain") {
        val what = args.getOrElse("name", "")
        val found = repo.index.definition(what).headOption
          .flatMap(sym => repo.corpus.sources.get(sym.source)
            .map(src => s"${sym.source}:${sym.span.line + 1}\n" +
              okay.rag.Symbols.segment(sym, src).text))
        Seq(
          Turn.System("You explain this codebase. Cite file and line. Be brief."),
          Turn.User(found.fold(s"What is '$what' in this repository?")(src =>
            s"Explain '$what':\n\n$src")))
      })).runWith
}
