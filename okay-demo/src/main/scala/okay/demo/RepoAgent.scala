package okay.demo

import okay.{!, +, Async, Handler, given}
import okay.agent.*
import okay.codec.{Json, Schema}
import okay.llm.{Transport, Transports}
import okay.rag.*

/**
 * A coding agent over a repository — written the way a USER of this
 * library would write it, from outside, with nothing added to the
 * library to make it possible.
 *
 * It exists to be a different kind of consumer than a test suite: the
 * tests were written by the author of the code and agree with it by
 * construction, while an application has to be assembled from the
 * public surface and either falls together or does not.
 *
 * What it uses, in order: okay-rag to index the source (parsing it,
 * so chunks are whole definitions and citations are exact byte
 * ranges), okay-agent for the loop and the context budget, grounded
 * recall so the relevant code is already in context, two tools for
 * when the agent wants to steer, and okay-llm against any
 * OpenAI-compatible endpoint.
 */
object RepoAgent {

  final case class Repo(corpus: Corpus, index: Index, keyword: Postings,
                        sources: Seq[Source])

  /**
   * Every source file under a root. Which files those are is decided
   * by `Language`, not by this demo — any language the library learns
   * to parse is indexed here without a change, and markdown comes
   * along as prose (no definers, so it splits by size).
   */
  def load(dir: java.io.File, limit: Int = 400): Seq[Source] =
    // canonicalize first: the root is skipped by its own hidden-name
    // rule otherwise, and "." is a hidden name — which is exactly how
    // the first run of this demo indexed nothing at all
    val root = dir.getCanonicalFile

    def skip(f: java.io.File): Boolean =
      val n = f.getName
      n == "target" || n == ".git" || (n.startsWith(".") && n.length > 1)

    def walk(f: java.io.File): Seq[java.io.File] =
      if f.isDirectory then
        if f != root && skip(f) then Seq.empty
        else Option(f.listFiles).toSeq.flatten.flatMap(walk)
      else if Language.of(f.getName).isDefined || f.getName.endsWith(".md") then Seq(f)
      else Seq.empty

    walk(root).take(limit).map { f =>
      val text = scala.io.Source.fromFile(f, "UTF-8").mkString
      Source(root.toPath.relativize(f.toPath).toString, text)
    }

  /** parse everything once: the symbol index, the keyword index, and
   * the corpus that makes passages lineage rather than copies */
  def index(sources: Seq[Source], budget: Int = 600): Repo =
    val segments = sources.flatMap(s => Ingest.segment(s, budget)(_.length))
    Repo(Corpus.of(sources), Symbols.project(sources),
      Keyword.index(segments), sources)

  // ---------------------------------------------------------------- tools

  final case class Definition(name: String)
  final case class ReadFile(path: String, from: Option[Int], lines: Option[Int])
  given Schema[Definition] = Schema.derived
  given Schema[ReadFile] = Schema.derived

  val definitionTool: ToolSpec =
    ToolSpec[Definition]("definition", "the source of a named definition")
  val readTool: ToolSpec =
    ToolSpec[ReadFile]("read_file", "read a file, optionally from a line")

  def tools(repo: Repo): Map[String, ToolCall => String] = Map(
    "definition" -> { c =>
      ToolSpec.args[Definition](c).fold(e => s"bad args: $e", a =>
        repo.index.definition(a.name).headOption
          .flatMap(sym => repo.corpus.sources.get(sym.source).map(src =>
            s"${sym.source}:${sym.span.line + 1}\n${Symbols.segment(sym, src).text}"))
          .getOrElse(s"no definition named '${a.name}'"))
    },
    "read_file" -> { c =>
      ToolSpec.args[ReadFile](c).fold(e => s"bad args: $e", a =>
        repo.corpus.sources.get(a.path) match
          case None => s"no such file '${a.path}'"
          case Some(src) =>
            val all = src.text.linesIterator.toVector
            val from = a.from.getOrElse(1).max(1)
            val n = a.lines.getOrElse(60).max(1)
            all.slice(from - 1, from - 1 + n).zipWithIndex
              .map((l, i) => f"${from + i}%5d  $l").mkString("\n"))
    })

  // ---------------------------------------------------------------- run

  /**
   * One question, answered with the repository in context. The
   * retriever is hybrid — exact symbols beside BM25 — and grounded
   * recall puts what it finds under the SAME budget as the
   * conversation, so no tool call is needed for the common case.
   */
  def ask(repo: Repo, question: String,
          url: String, model: String, key: String = "none",
          budget: Int = 6000)
         (using Handler[Async]): (String, Seq[Turn]) =
    // what the model saw, not what the conversation held: the two
    // differ precisely because retrieval joins at recall time
    val seen = scala.collection.mutable.Buffer[Seq[Turn]]()
    val retriever = Retrieve.hybrid[okay.Pure](Seq(
      Retrieve.symbols(repo.index, repo.corpus.sources),
      Retrieve.keyword(repo.keyword)))

    val provider = Provider.openAi(Transports.http(), key, model, url,
      maxTokens = Some(700))
    val (state, ctx) = Grounded.context(
      Compact.window(budget)(Compact.chars), retriever,
      budget = budget, share = 0.6, k = 4,
      onRecall = v => seen += v)(Compact.chars)

    given Handler[Model] = provider
    given Handler[Tool] = Handlers.tools(tools(repo))
    given Handler[Context] = ctx
    given rowMA: Handler[Model + Async] = okay.Handler.union[Model, Async]
    given rowCMA: Handler[Context + (Model + Async)] =
      okay.Handler.union[Context, Model + Async]
    given rowAll: Handler[Agent] = okay.Handler.union[Tool, Context + (Model + Async)]

    val prog = Agent.remember(Turn.System(
      "You answer questions about a codebase. Relevant source is " +
        "already provided in the context. Cite file names. Be brief."))
      .flatMap(_ => Agent.converse(question, Seq(definitionTool, readTool)))

    (prog.runWith, seen.lastOption.getOrElse(state.recall))

  def main(args: Array[String]): Unit =
    val root = java.io.File(sys.env.getOrElse("OKAY_REPO", "."))
    val url = sys.env.getOrElse("OKAY_LLM_URL",
      "http://127.0.0.1:8089/v1/chat/completions")
    val model = sys.env.getOrElse("OKAY_LLM_MODEL",
      "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
    val question =
      if args.nonEmpty then args.mkString(" ")
      else "What does the Compact.window policy do when the budget is exceeded?"

    val sources = load(root)
    val repo = index(sources)
    val byLang = sources.groupBy(s => Language.of(s.id).map(_.name).getOrElse("text"))
      .view.mapValues(_.size).toSeq.sortBy(-_._2)
    println(s"indexed ${sources.length} files, ${repo.index.names.size} definitions" +
      byLang.map((n, c) => s"$n $c").mkString(" (", ", ", ")"))

    val (answer, context) = ask(repo, question, url, model)
    println(s"\n--- context the model saw (${context.length} turns) ---")
    context.foreach(t => println(s"  ${Compact.text(t).take(90).replace("\n", " ")}"))
    println(s"\n--- answer ---\n$answer")
}
