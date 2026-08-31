package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.rag.*

/**
 * Retrieval measured (P10). The cases are chosen to make the design's
 * two bets falsifiable rather than to look fast:
 *
 *  1. **Parsing is affordable.** Structural chunking claims to be
 *     better than sliding a window over characters — but it costs a
 *     parse. `indexSymbols` against `splitWindows` is that price, in
 *     microseconds, on the same file.
 *
 *  2. **An edit costs its own size, not the file's.** `reindexEdit`
 *     against `indexFull` is the O(damage) claim at the code layer;
 *     it is the same claim okay-parse makes for JSON, one level up,
 *     and it is what makes a live index of an edited repository
 *     possible at all.
 *
 * The retrieval side measures what actually runs per query: an exact
 * symbol lookup (a map), BM25 over the postings, and the fusion of
 * the two. Nothing here calls an embedding service — that is the
 * point of having a half of retrieval that needs no vectors, and the
 * numbers say what that half costs.
 *
 * There is no third-party comparison in this file, deliberately: no
 * Scala library ships this shape, and comparing against a Python
 * stack across a process boundary would measure the boundary.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class RagBenchmark {

  /** ~6KB of Scala: 30 definitions, doc comments, strings, nesting */
  val scalaText: String = (0 until 30).map(i =>
    s"""/** what member $i is for, said in a doc comment. */
       |final case class Member$i(name: String, count: Int) {
       |  def describe: String = s"member $i called $$name"
       |  private val tag = "member-$i"
       |  def combine(that: Member$i): Member$i =
       |    Member$i(name + that.name, count + that.count)
       |}
       |""".stripMargin).mkString("package bench\n\n", "\n", "\n")

  val pythonText: String = (0 until 30).map(i =>
    s"""class Member$i:
       |    \"\"\"what member $i is for.\"\"\"
       |    def __init__(self, name, count):
       |        self.name = name
       |        self.count = count
       |
       |    def describe(self):
       |        return f"member $i called {self.name}"
       |""".stripMargin).mkString("\n")

  val scalaSrc: Source = Source("Bench.scala", scalaText)
  val pythonSrc: Source = Source("bench.py", pythonText)

  /** a small project: the same file 20 times over, distinctly named */
  val project: Seq[Source] =
    (0 until 20).map(i => Source(s"f$i.scala", scalaText.replace("Member", s"M${i}x")))

  val segments: Seq[Segment] = project.flatMap(s => Ingest.segment(s, 600)(_.length))
  val postings: Postings = Keyword.index(segments)
  val index: Index = Symbols.project(project)
  val sources: Map[String, Source] = Corpus.of(project).sources

  // the vector side at PROVIDER dimension, over the whole corpus
  val store: MemoryStore =
    val st = MemoryStore()
    val f = Vectors.hashing(1536)
    st.upsert(segments.map(s => (s, f(s.text)))).runWith
    st
  val probe: Embedding = Vectors.hashing(1536)("describe a member by name")

  val symbolRetriever: Retriever[Pure] = Retrieve.symbols(index, sources)
  val keywordRetriever: Retriever[Pure] = Retrieve.keyword(postings)
  val hybridRetriever: Retriever[Pure] =
    Retrieve.hybrid[Pure](Seq(symbolRetriever, keywordRetriever))

  // a session to reparse from, and a one-character edit in the middle
  val session: okay.parse.Parse.Parsed[Code.K, Code.S, Code.D] =
    Code.parse(scalaText, 64, Language.scala)
  val editAt: Int = scalaText.length / 2
  val edited: String = scalaText.take(editAt) + "x" + scalaText.drop(editAt)

  // ---- indexing: what a parse costs, and what a window costs instead

  @Benchmark
  def indexFull: Int = Symbols.source(scalaSrc).defs.size

  @Benchmark
  def indexFullPython: Int = Symbols.source(pythonSrc).defs.size

  /** the incremental path: one character changed in a 6KB file */
  @Benchmark
  def reindexEdit: Int =
    Code.reparse(session, scalaText, edited, editAt, editAt, editAt + 1, 64,
      Language.scala).lexed.tokens.length

  /** the same file parsed from scratch — the baseline the above beats */
  @Benchmark
  def reindexFull: Int = Code.parse(edited, 64, Language.scala).lexed.tokens.length

  // ---- chunking: structural (parsed) against windows (unparsed)

  @Benchmark
  def splitStructural: Int = Ingest.segment(scalaSrc, 600)(_.length).size

  /**
   * What a conventional RAG stack does: slide a window, no parse. The
   * budget is in TOKENS here and in characters above, chosen so both
   * produce about ten chunks of the same file — otherwise the
   * comparison would be of chunk counts, not of method.
   */
  @Benchmark
  def splitWindows: Int =
    Split.windows(scalaSrc, Code.scanner(Language.scala), 200, overlap = 20).size

  // ---- retrieval: per query, with no embedding service in play

  @Benchmark
  def retrieveSymbols: Int = symbolRetriever.retrieve("describe", 8).runWith.size

  /** the segments a keyword index is built from, split once */
  val kwSegs: Seq[Segment] = Ingest.segment(scalaSrc, 600)(_.length)

  /**
   * BUILDING the keyword index, as opposed to searching it.
   *
   * `Keyword.fold` makes a whole one-segment `Postings` per segment
   * and merges it into the accumulated one, so every segment rebuilds
   * map paths and concatenates posting vectors. That is the shape that
   * turned out to cost 3x to 580x in the sketches; whether it costs
   * anything here is what this lane is for.
   */
  /** the tokenization alone, to see whether the merge is the cost or
   * whether it is just reading the words */
  @Benchmark
  def keywordTerms: Int = kwSegs.map(s => Keyword.terms(s.text).length).sum

  @Benchmark
  def indexKeyword: Int = Keyword.index(kwSegs).docs.length

  @Benchmark
  def retrieveKeyword: Int =
    keywordRetriever.retrieve("member called name", 8).runWith.size

  @Benchmark
  def retrieveHybrid: Int =
    hybridRetriever.retrieve("describe member name", 8).runWith.size

  /**
   * The vector side through the REAL store: `Aggregator.topK`, the
   * `Scored` allocations and the segment records included, at
   * provider dimension over a realistic corpus. What the loop
   * benchmark measures in isolation, priced where a user meets it.
   */
  @Benchmark
  def searchVectors: Int = store.search(probe, 8).runWith.size

  /** the whole per-query cost an agent actually pays */
  @Benchmark
  def retrieveAndAssemble: Int =
    hybridRetriever.retrieve("describe member name", 4).runWith
      .map(_.segment.text).mkString("\n").length
}
