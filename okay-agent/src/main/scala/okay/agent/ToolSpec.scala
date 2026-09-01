package okay.agent

import okay.codec.{Json, Schema}

/**
 * A tool declaration, derived — not written. The JSON Schema a
 * provider wants is the FOURTH algebra over Schema[A] (after JSON,
 * CBOR and YAML): one derivation per type serves the wire format,
 * the argument decode and now the tool declaration, so a tool's
 * signature cannot drift from its parser.
 */
final case class ToolSpec(name: String, description: String, schema: Json)

object ToolSpec {

  /** the algebra: a datatype's shape as a JSON Schema value */
  def jsonSchema[A](s: Schema[A]): Json = s match
    case Schema.SInt | Schema.SLong => obj("type" -> Json.JStr("integer"))
    case Schema.SDouble => obj("type" -> Json.JStr("number"))
    case Schema.SBool => obj("type" -> Json.JStr("boolean"))
    case Schema.SString => obj("type" -> Json.JStr("string"))
    case Schema.SChar => obj("type" -> Json.JStr("string"),
      "minLength" -> Json.JNum(1), "maxLength" -> Json.JNum(1))
    // the JSON Schema vocabulary for bytes, and it matches what the
    // Json algebra actually writes — a tool taking binary input tells
    // the model exactly how to send it
    case Schema.SBytes => obj("type" -> Json.JStr("string"),
      "contentEncoding" -> Json.JStr("base64"))
    case Schema.SOption(of) => jsonSchema(of())   // optionality is in `required`
    case Schema.SList(of) => obj(
      "type" -> Json.JStr("array"),
      "items" -> jsonSchema(of()))
    case Schema.SVector(of) => obj(
      "type" -> Json.JStr("array"),
      "items" -> jsonSchema(of()))
    case p: Schema.SProduct[A] =>
      val props = p.fields.map((n, f) => (n, jsonSchema(f())))
      val required = p.fields.collect {
        case (n, f) if !f().isInstanceOf[Schema.SOption[?]] => Json.JStr(n)
      }
      obj(
        "type" -> Json.JStr("object"),
        "properties" -> Json.JObj(props),
        "required" -> Json.JArr(required))
    case su: Schema.SSum[A] =>
      // a sum is one-of, each case tagged by its name (the same
      // encoding Json and Cbor use, so decode round-trips)
      obj("oneOf" -> Json.JArr(su.cases.map { (n, c) =>
        obj(
          "type" -> Json.JStr("object"),
          "properties" -> Json.JObj(Vector((n, jsonSchema(c())))),
          "required" -> Json.JArr(Vector(Json.JStr(n))))
      }))

  private def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

  /** declare a tool from its argument type — nothing hand-written */
  def apply[A](name: String, description: String)(using s: Schema[A]): ToolSpec =
    ToolSpec(name, description, jsonSchema(s))

  /** decode a call's arguments with the SAME Schema that declared it */
  def args[A](call: ToolCall)(using s: Schema[A]): Either[String, A] =
    Json.decode(s)(call.args)
}
