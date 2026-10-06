package okay.semantic.ossie

import okay.codec.Json
import okay.parse.Cst
import okay.lex.Json.K

/** Storage syntax is optional and independent of business/execution validation. */
trait Syntax:
  def name: String
  def unavailable: Option[String] = None
  def readJson(text: String): Either[Vector[String], Json]
  def readYaml(text: String): Either[Vector[String], Json]
  def writeJson(value: Json): String
  def writeYaml(value: Json): String
object Syntax:
  given Syntax = PortableSyntax
  def byName(name: String)(using candidate: Syntax): Either[String, Syntax] =
    if name == "portable" then Right(PortableSyntax)
    else if name == candidate.name then candidate.unavailable.toLeft(candidate)
    else Left(s"syntax $name: import the optional platform adapter explicitly")

/** JSON is also YAML 1.2 flow syntax; block YAML requires a platform adapter. */
object PortableSyntax extends Syntax:
  val name = "portable"
  def readJson(text: String): Either[Vector[String], Json] =
    val errors = Vector.newBuilder[String]
    var work = List(Json.cst(text))
    while work.nonEmpty do
      val node = work.head; work = work.tail
      node match
        case Cst.Node(_, children) => work = children.toList ::: work
        case Cst.Err(_, why) => errors += s"JSON: $why"
        case Cst.Leaf(token) if token.kind == K.Num =>
          Exact.number(token.lexeme).left.foreach(errors += _)
        case _ => ()
    val found = errors.result()
    if found.nonEmpty then Left(found) else Right(Json.parse(text))
  def readYaml(text: String): Either[Vector[String], Json] =
    if text.trim.startsWith("{") || text.trim.startsWith("[") then readJson(text)
    else readJson(text).left.map(why => Vector("portable YAML requires JSON flow syntax; import SnakeYaml for block YAML") ++ why)
  def writeJson(value: Json): String = Json.print(value)
  def writeYaml(value: Json): String = Json.print(value)

private[ossie] object Exact:
  def number(text: String): Either[String, Json] =
    scala.util.Try(BigDecimal(text)).toEither.left.map(_ => s"invalid numeric metadata $text").flatMap { exact =>
      val projected = exact.toDouble
      if !projected.isNaN && !projected.isInfinity && BigDecimal(projected.toString) == exact then Right(Json.JNum(projected))
      else Left(s"numeric metadata $text would lose precision; encode it as a string")
    }
