package okay.semantic.ossie

import okay.codec.Json
import Json.*

final case class DialectExpression(dialect: String, expression: String)
final case class Expression(dialects: Vector[DialectExpression]):
  def in(dialect: String): Either[String, String] =
    dialects.filter(_.dialect == dialect) match
      case Vector(one) => Right(one.expression)
      case Vector() => Left(s"missing dialect $dialect")
      case _ => Left(s"ambiguous dialect $dialect")
final case class Extension(vendor: String, data: String)
final case class Field(name: String, expression: Expression, datatype: Option[String],
                       isTime: Boolean, description: Option[String], aiContext: Option[Json], extensions: Vector[Extension])
final case class Dataset(name: String, source: String, fields: Vector[Field], primaryKey: Vector[String],
                         uniqueKeys: Vector[Vector[String]], description: Option[String], aiContext: Option[Json], extensions: Vector[Extension])
final case class Relationship(name: String, from: String, to: String, fromColumns: Vector[String], toColumns: Vector[String],
                              aiContext: Option[Json], extensions: Vector[Extension])
final case class Metric(name: String, expression: Expression, description: Option[String], datatype: Option[String],
                        aiContext: Option[Json], extensions: Vector[Extension])

/** Immutable source tree preserves optional-property presence and opaque metadata. */
final class Document private (val raw: Json, val name: String, val datasets: Vector[Dataset],
                             val relationships: Vector[Relationship], val metrics: Vector[Metric]) extends Serializable:
  val version: String = Document.version
  def description: Option[String] = Read.optionalString(raw, "description")
  def aiContext: Option[Json] = Read.get(raw, "ai_context")
  def extensions: Vector[Extension] = Read.extensions(raw)
  def json(using syntax: Syntax): String = syntax.writeJson(raw)
  def yaml(using syntax: Syntax): String = syntax.writeYaml(raw)
object Document:
  val version: String = Pinned.version
  val revision: String = Pinned.revision
  def readJson(text: String)(using syntax: Syntax): Either[Vector[String], Document] = syntax.readJson(text).flatMap(fromJson)
  def readYaml(text: String)(using syntax: Syntax): Either[Vector[String], Document] = syntax.readYaml(text).flatMap(fromJson)
  def fromJson(raw: Json): Either[Vector[String], Document] =
    val shape = Validate.shape(raw)
    if shape.nonEmpty then Left(shape)
    else
      val datasets = Read.array(raw, "datasets").map { d =>
        Dataset(Read.string(d,"name"), Read.string(d,"source"), Read.array(d,"fields").map { f =>
          val datatype = Read.optionalString(f,"datatype")
          val temporal = Read.get(f,"dimension").flatMap(Read.get(_,"is_time")) match
            case Some(JBool(b)) => b
            case _ => datatype.exists(Set("Date","Time","DateTime","DateTimeTz"))
          Field(Read.string(f,"name"), Read.expression(f), datatype, temporal,
            Read.optionalString(f,"description"), Read.get(f,"ai_context"), Read.extensions(f))
        }, Read.strings(d,"primary_key"), Read.array(d,"unique_keys").map(Read.stringArray),
          Read.optionalString(d,"description"), Read.get(d,"ai_context"), Read.extensions(d))
      }
      val relations = Read.array(raw,"relationships").map(r => Relationship(Read.string(r,"name"), Read.string(r,"from"), Read.string(r,"to"),
        Read.strings(r,"from_columns"), Read.strings(r,"to_columns"), Read.get(r,"ai_context"), Read.extensions(r)))
      val metrics = Read.array(raw,"metrics").map(m => Metric(Read.string(m,"name"), Read.expression(m),
        Read.optionalString(m,"description"), Read.optionalString(m,"datatype"), Read.get(m,"ai_context"), Read.extensions(m)))
      val document = new Document(raw, Read.string(raw,"name"), datasets, relations, metrics)
      val errors = Validate.references(document)
      if errors.nonEmpty then Left(errors) else Right(document)

private[ossie] object Read:
  def get(value: Json, key: String): Option[Json] = value match
    case JObj(fields) => fields.find(_._1 == key).map(_._2)
    case _ => None
  def array(value: Json, key: String): Vector[Json] = get(value,key) match
    case Some(JArr(vs)) => vs
    case _ => Vector.empty
  def stringArray(value: Json): Vector[String] = value match
    case JArr(vs) => vs.collect { case JStr(s) => s }
    case _ => Vector.empty
  def strings(value: Json,key: String): Vector[String] = get(value,key).toVector.flatMap(stringArray)
  def optionalString(value: Json,key: String): Option[String] = get(value,key).collect { case JStr(s) => s }
  def string(value: Json,key: String): String = optionalString(value,key).getOrElse("")
  def expression(value: Json): Expression = Expression(get(value,"expression").toVector.flatMap(array(_,"dialects"))
    .map(d => DialectExpression(string(d,"dialect"),string(d,"expression"))))
  def extensions(value: Json): Vector[Extension] = array(value,"custom_extensions").map(e => Extension(string(e,"vendor_name"),string(e,"data")))
