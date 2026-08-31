package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.lex.{Scan, Json as JsonLex}
import okay.parse.{Cst, JsonParse, Parse}
import okay.codec.{Cbor, Json, Schema}
import okay.lex.Bpe
import io.circe.syntax.*

/**
 * The text stack (P5) measured: total streaming lexing (element-wise
 * vs chunked), total parsing, INCREMENTAL reparse after an edit (the
 * O(damage) claim as a number), the two Schema algebras (JSON text,
 * CBOR binary) against circe on the same value, and BPE tokenization.
 * Honest scope: okay's JSON path pays for totality and losslessness
 * (a full CST with trivia, damage as data) — circe parses to a
 * lighter AST and may win raw speed; the interesting number is the
 * PRICE of the stronger contract.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class TextBenchmark {

  // ~2.5KB, 50 members, newlines between them (reconvergence points)
  val doc: String = (0 until 50)
    .map(i => s""" "k$i": {"a": [1, 2, $i], "s": "value $i"}""")
    .mkString("{\n", ",\n", "\n}")

  // ---- lexing

  @Benchmark
  def lexElementwise: Int = Scan.all(JsonLex.scan)(doc).tokens.length

  @Benchmark
  def lexChunked: Long =
    Chunks.fold(Scan.chunks(JsonLex.scan)(Chunks.fromIterator(doc.iterator, 64)))(
      using Fold.count)

  /** the same chunked lex, but over UNBOXED char chunks — the
   * hypothesis the three-size probe left standing */
  @Benchmark
  def lexChunkedChars: Long =
    Chunks.fold(Scan.chunks(JsonLex.scan)(Chunks.ofChars(doc, 64)))(using Fold.count)

  // the same work at 8x and 1/8x the chunk size: if per-chunk
  // overhead dominates, bigger chunks win; if the boxing of chars
  // into Array[AnyRef] dominates, the size barely matters
  @Benchmark
  def lexChunked512: Long =
    Chunks.fold(Scan.chunks(JsonLex.scan)(Chunks.fromIterator(doc.iterator, 512)))(
      using Fold.count)

  @Benchmark
  def lexChunked8: Long =
    Chunks.fold(Scan.chunks(JsonLex.scan)(Chunks.fromIterator(doc.iterator, 8)))(
      using Fold.count)

  // ---- parsing, full and incremental

  val session = Parse.full(JsonLex.scan, JsonParse.instrs)(doc, 64)
  val at = doc.indexOf("value 25")
  val edited = doc.replace("value 25", "VALUE 25")   // same length, one member

  /** the instruction stream the builder folds, computed once */
  val instrs: Vector[okay.parse.Instr[JsonLex.K]] =
    Scan.all(JsonLex.scan)(doc).tokens.flatMap(JsonParse.instrs).toVector

  /**
   * The BUILDER alone, over that stream.
   *
   * `Parse.full` is lex plus drive plus build, and the section-10
   * numbers say the relex dominates. This isolates the third part, to
   * see whether its accumulator is worth the same treatment the
   * sketches just got: `Building` carries a `List` stack of tuples
   * whose third field is a `Vector`, and every token does `kids :+ c`
   * plus a fresh tuple, a fresh cons cell and a fresh `Building`.
   */
  @Benchmark
  def buildOnly: Int =
    val fold = Parse.build[JsonLex.K]
    Parse.present(instrs.foldLeft(fold.init)(fold.add)) match
      case Cst.Node(_, kids) => kids.length
      case _ => 0

  @Benchmark
  def parseFull: Cst[JsonLex.K] =
    Parse.full(JsonLex.scan, JsonParse.instrs)(doc).tree

  @Benchmark
  def reparseIncremental: Cst[JsonLex.K] =
    Parse.reparse(JsonLex.scan, JsonParse.instrs)(
      session, doc, edited, at, at + 8, at + 8, 64).tree

  // ---- codecs: one Schema, two wires, circe as the ecosystem line

  case class Person(name: String, age: Int, tags: List[String], boss: Option[Person])
  given Schema[Person] = Schema.derived
  given io.circe.Codec[Person] = io.circe.generic.semiauto.deriveCodec

  val person = Person("ann", 41, List("a", "b"), Some(Person("boss", 60, Nil, None)))
  val personJson = Json.write(person)
  val personCbor = Cbor.write(person)

  @Benchmark
  def okayJsonWrite: String = Json.write(person)

  @Benchmark
  def okayJsonRead: Either[String, Person] = Json.read[Person](personJson)

  // ---- where the JSON read time actually goes
  //
  // The 16x against circe is explained in docs as the price of the
  // pipeline: chars -> total scanner -> total driver -> lossless CST
  // -> projection -> Schema fold, against circe parsing straight into
  // its AST. These three lanes check that the explanation is the whole
  // story, by charging each stage separately.

  val personCst = Json.cst(personJson)
  val personTree: okay.codec.Json = Json.value(personCst)

  /** stage 1+2: lex and parse, to a lossless CST */
  @Benchmark
  def jsonToCst: Cst[?] = Json.cst(personJson)

  /** stage 3: the CST projected to a Json value */
  @Benchmark
  def jsonProject: okay.codec.Json = Json.value(personCst)

  /** stage 4: the Schema fold over that value */
  @Benchmark
  def jsonDecode: Either[String, Person] = Json.decode(summon[Schema[Person]])(personTree)

  @Benchmark
  def okayCborWrite: Array[Byte] = Cbor.write(person)

  @Benchmark
  def okayCborRead: Either[String, Person] = Cbor.read[Person](personCbor)

  @Benchmark
  def circeWrite: String = person.asJson.noSpaces

  @Benchmark
  def circeRead: Either[io.circe.Error, Person] =
    io.circe.parser.decode[Person](personJson)

  // ---- BPE

  val bpe = Bpe(List(("h", "e"), ("l", "l"), ("he", "ll"), ("hell", "o"),
    ("w", "o"), ("r", "l"), ("wo", "rl"), ("worl", "d"), ("e", "r")))
  val corpus = List.fill(100)("hello world her herd worldly hold").mkString(" ")

  @Benchmark
  def bpeScan: Int = Scan.all(bpe)(corpus).tokens.length
}
