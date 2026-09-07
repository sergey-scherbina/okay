package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.codec.{Cbor, Json, Schema}
import okay.codec.Json.*

/**
 * Step 0 of specs/codecs.md's STAGED fold mode: what the interpreted
 * Schema fold costs today, against two floors — a hand-written
 * encoder/decoder over the same Json AST (the shape a staged fold
 * would emit), and circe as the external reference.
 */
object CodecFixture:
  final case class Address(city: String, zip: String, line: Option[String])
  final case class Order(id: Long, user: String, amount: Double, active: Boolean,
                         tags: List[String], addr: Address, note: Option[String])
  given Schema[Address] = Schema.derived
  given Schema[Order] = Schema.derived

  val order = Order(42L, "ada", 12.5, true, List("new", "vip"),
    Address("Kyiv", "01001", None), Some("leave at door"))
  val text: String = Json.encode(summon[Schema[Order]])(order)
  val ast: Json = Json.parse(text)

  /** schema-thunks-once: a SUM whose cases have no given of their own
   * — the shape whose case schemas the interpreter re-derived per value */
  enum Pet derives Schema:
    case Dog(name: String, age: Int)
    case Cat(name: String)
    case Rock
  final case class Owner(name: String, pet: Pet, pets: List[Pet]) derives Schema
  val owner = Owner("bo", Pet.Dog("rex", 3), List(Pet.Cat("tom"), Pet.Rock, Pet.Dog("ace", 1)))
  val ownerText: String = Json.encode(summon[Schema[Owner]])(owner)
  val ownerAst: Json = Json.parse(ownerText)
  val ownerCbor: Array[Byte] = Cbor.write(owner)

  // ---- the hand-written floor: straight-line field access ----
  def handEncode(o: Order): String =
    val sb = new java.lang.StringBuilder(128)
    sb.append("{\"id\":").append(o.id)
      .append(",\"user\":\"").append(Json.escape(o.user)).append('"')
      .append(",\"amount\":").append(o.amount)
      .append(",\"active\":").append(o.active)
      .append(",\"tags\":[")
    var first = true
    for t <- o.tags do
      if !first then sb.append(','): Unit
      first = false
      sb.append('"').append(Json.escape(t)).append('"'): Unit
    sb.append("],\"addr\":{\"city\":\"").append(Json.escape(o.addr.city))
      .append("\",\"zip\":\"").append(Json.escape(o.addr.zip))
      .append("\",\"line\":")
    o.addr.line match
      case Some(l) => sb.append('"').append(Json.escape(l)).append('"')
      case None => sb.append("null")
    sb.append("},\"note\":")
    o.note match
      case Some(n) => sb.append('"').append(Json.escape(n)).append('"')
      case None => sb.append("null")
    sb.append('}').toString

  private def str(j: Json, f: String): Either[String, String] = j match
    case JStr(s) => Right(s)
    case other => Left(s"$f: expected string, got $other")
  private def optStr(j: Json, f: String): Either[String, Option[String]] = j match
    case JNull => Right(None)
    case JStr(s) => Right(Some(s))
    case other => Left(s"$f: expected string or null, got $other")
  private def get(fs: Vector[(String, Json)], name: String): Either[String, Json] =
    var i = 0
    while i < fs.length do
      if fs(i)._1 == name then return Right(fs(i)._2)
      i += 1
    Left(s"missing field '$name'")

  def handDecodeAddress(j: Json): Either[String, Address] = j match
    case JObj(fs) =>
      for
        city <- get(fs, "city").flatMap(str(_, "city"))
        zip <- get(fs, "zip").flatMap(str(_, "zip"))
        line <- get(fs, "line").fold(_ => Right(None), optStr(_, "line"))
      yield Address(city, zip, line)
    case other => Left(s"expected object, got $other")

  def handDecode(j: Json): Either[String, Order] = j match
    case JObj(fs) =>
      for
        id <- get(fs, "id").flatMap { case JNum(n) => Right(n.toLong); case o => Left(s"id: $o") }
        user <- get(fs, "user").flatMap(str(_, "user"))
        amount <- get(fs, "amount").flatMap { case JNum(n) => Right(n); case o => Left(s"amount: $o") }
        active <- get(fs, "active").flatMap { case JBool(b) => Right(b); case o => Left(s"active: $o") }
        tags <- get(fs, "tags").flatMap {
          case JArr(vs) => vs.foldRight(Right(Nil): Either[String, List[String]]) { (v, acc) =>
            acc.flatMap(xs => str(v, "tags").map(_ :: xs)) }
          case o => Left(s"tags: $o") }
        addr <- get(fs, "addr").flatMap(handDecodeAddress)
        note <- get(fs, "note").fold(_ => Right(None), optStr(_, "note"))
      yield Order(id, user, amount, active, tags, addr, note)
    case other => Left(s"expected object, got $other")

  // ---- circe, the external reference ----
  import io.circe.{Encoder, Decoder}
  import io.circe.generic.semiauto.*
  given Encoder[Address] = deriveEncoder
  given Decoder[Address] = deriveDecoder
  given Encoder[Order] = deriveEncoder
  given Decoder[Order] = deriveDecoder
  val circeAst: io.circe.Json = io.circe.parser.parse(text).toOption.get

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class CodecBenchmark {
  import CodecFixture.*

  val staged = okay.codec.Staged.json[Order]
  val stagedCbor = okay.codec.Staged.cbor[Order]
  /** staged-runtime: the same fold over the schema as a VALUE, generated
   * by the compiler in the running process at first use */
  val runtimeStaged = okay.staging.RuntimeStaged.json(summon[Schema[Order]])
  /** a fresh identity each call, so every call is a first use: the
   * generation cost (`generate`), the number a caller pays once per schema */
  def freshSchema(): Schema[Order] = summon[Schema[Order]] match
    case p: Schema.SProduct[Order] => p.copy()
    case other => other
  val cborBytes: Array[Byte] = Cbor.write(order)

  @Benchmark def encodeInterp(): String = Json.encode(summon[Schema[Order]])(order)
  @Benchmark def encodeSumInterp(): String = Json.encode(summon[Schema[Owner]])(owner)
  @Benchmark def decodeSumInterpAst(): Either[String, Owner] = Json.decode(summon[Schema[Owner]])(ownerAst)
  @Benchmark def cborEncodeSumInterp(): Array[Byte] = Cbor.write(owner)(using summon[Schema[Owner]])
  @Benchmark def encodeStaged(): String = staged.encode(order)
  @Benchmark def decodeStagedAst(): Either[String, Order] = staged.decode(ast)
  @Benchmark def encodeRuntimeStaged(): String = runtimeStaged.encode(order)
  /** staging-seam: a generic door's cost — the seam with the default
   * interpreter (a volatile read and a small wrapper over the fold) */
  @Benchmark def encodeSeam(): String = okay.codec.Codecs.writeJson(order)
  @Benchmark def decodeSeamAst(): Either[String, Order] = okay.codec.Codecs.json(summon[Schema[Order]]).decode(ast)
  /** the same door with okay-staging installed (its own fork: the
   * provider is process-global) — a cache lookup by identity, then the
   * generated code */
  @Benchmark def encodeSeamStaged(st: CodecBenchmark.Installed): String = okay.codec.Codecs.writeJson(order)
  @Benchmark def decodeSeamStagedAst(st: CodecBenchmark.Installed): Either[String, Order] = okay.codec.Codecs.json(summon[Schema[Order]]).decode(ast)
  @Benchmark def decodeRuntimeStagedAst(): Either[String, Order] = runtimeStaged.decode(ast)
  @Benchmark @Fork(1) @Warmup(iterations = 1, time = 5) @Measurement(iterations = 3, time = 5)
  def generateRuntimeStaged(): okay.codec.JsonCodec[Order] = okay.staging.RuntimeStaged.json(freshSchema())
  @Benchmark def encodeHand(): String = handEncode(order)
  @Benchmark def encodeCirce(): String = summon[io.circe.Encoder[Order]](order).noSpaces

  @Benchmark def cborEncodeInterp(): Array[Byte] = Cbor.write(order)(using summon[Schema[Order]])
  @Benchmark def cborEncodeStaged(): Array[Byte] = stagedCbor.encode(order)
  @Benchmark def cborDecodeInterp(): Either[String, Order] = Cbor.read(cborBytes)(using summon[Schema[Order]])
  @Benchmark def cborDecodeStaged(): Either[String, Order] = stagedCbor.decode(cborBytes)

  @Benchmark def parseOnly(): Json = Json.parse(text)
  @Benchmark def parseValueOnly(): Json = Json.parse(text)
  @Benchmark def textToOrderStaged(): Either[String, Order] = staged.decode(Json.parse(text))
  @Benchmark def textToOrderCirce(): Either[?, Order] = io.circe.parser.decode[Order](text)
  /** json-fast-read: the strict door -- characters straight into the
   * Schema, no tokens, no CST, no Json tree; the same answer as
   * `Json.read` on this (complete, well-formed) text */
  @Benchmark def textToOrderStrict(): Either[String, Order] = Json.readStrict[Order](text)
  /** json-strict-staged: the same strict reader, generated for Order
   * at compile time -- fields into locals, no name lookup, no erased
   * parts; the interpreted door's answer, without the walk */
  // fully qualified: `okay.Staged` (core) shadows `okay.codec.Staged` in this package
  private val strictStaged: okay.codec.StrictJsonCodec[Order] = okay.codec.Staged.strict[Order]
  @Benchmark def textToOrderStrictStaged(): Either[String, Order] = strictStaged.decode(text)
  /** staged-strict: the same strict reader, generated at RUN time
   * from the schema as a value — what a generic strict door gets */
  private val strictRuntime: okay.codec.StrictJsonCodec[Order] =
    okay.staging.RuntimeStaged.strict(summon[Schema[Order]])
  @Benchmark def textToOrderStrictRuntimeStaged(): Either[String, Order] = strictRuntime.decode(text)
  /** the lossless door, for the same text: the price list's 10.3 */
  @Benchmark def textToOrderLossless(): Either[String, Order] = Json.read[Order](text)
  @Benchmark def decodeInterpAst(): Either[String, Order] = Json.decode(summon[Schema[Order]])(ast)
  @Benchmark def decodeHandAst(): Either[String, Order] = handDecode(ast)
  @Benchmark def decodeCirceAst(): Either[?, Order] = summon[io.circe.Decoder[Order]].decodeJson(circeAst)
  @Benchmark def parseCirce(): io.circe.Json = io.circe.parser.parse(text).toOption.get
}

object CodecBenchmark:
  /** staging-seam: installs the run-time staged provider for the fork
   * that uses this state, so the seam lanes measure the door with and
   * without it */
  @State(Scope.Benchmark)
  class Installed:
    @Setup def up(): Unit = { okay.staging.RuntimeStaged.install(); () }

