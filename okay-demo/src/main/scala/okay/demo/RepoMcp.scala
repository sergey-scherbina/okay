package okay.demo

import okay.given
import okay.mcp.{Mcp, Server, Stdio}


/**
 * This repository, as an MCP server: the two tools the demo agent
 * already has — a definition by name, a file by path — served over
 * stdio, so any MCP client (an editor, another agent, Claude Code)
 * can ask this codebase about itself.
 *
 *   sbt "okayDemo/runMain okay.demo.RepoMcp"
 *
 * It is four lines of wiring on purpose. The tools are the SAME
 * `Map[String, ToolCall => String]` the local agent uses, and their
 * schemas are the same derived ones — serving them is a transport
 * decision, not a rewrite.
 *
 * Note the one rule stdio imposes: nothing but protocol may go to
 * stdout. The indexing report goes to stderr, where a server's logs
 * belong.
 */
object RepoMcp {

  def main(args: Array[String]): Unit =
    val root = java.io.File(
      args.headOption.getOrElse(sys.env.getOrElse("OKAY_REPO", ".")))
    val sources = RepoAgent.load(root)
    val repo = RepoAgent.index(sources)
    System.err.println(
      s"okay-repo: indexed ${sources.length} files, ${repo.index.names.size} definitions")

    Server.run(Stdio.std, Mcp.Info("okay-repo", "0.1"),
      Seq(RepoAgent.definitionTool, RepoAgent.readTool),
      RepoAgent.tools(repo)).runWith
}
