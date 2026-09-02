package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.codec.{Json, Schema}
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

  @Benchmark def encodeInterp(): String = Json.encode(summon[Schema[Order]])(order)
  @Benchmark def encodeStaged(): String = staged.encode(order)
  @Benchmark def decodeStagedAst(): Either[String, Order] = staged.decode(ast)
  @Benchmark def encodeHand(): String = handEncode(order)
  @Benchmark def encodeCirce(): String = summon[io.circe.Encoder[Order]](order).noSpaces

  @Benchmark def parseOnly(): Json = Json.parse(text)
  @Benchmark def decodeInterpAst(): Either[String, Order] = Json.decode(summon[Schema[Order]])(ast)
  @Benchmark def decodeHandAst(): Either[String, Order] = handDecode(ast)
  @Benchmark def decodeCirceAst(): Either[?, Order] = summon[io.circe.Decoder[Order]].decodeJson(circeAst)
  @Benchmark def parseCirce(): io.circe.Json = io.circe.parser.parse(text).toOption.get
}
