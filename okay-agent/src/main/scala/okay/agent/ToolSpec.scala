package okay.agent

import okay.codec.{Json, JsonSchema, Schema}

/**
 * A tool declaration, derived — not written. The JSON Schema a
 * provider wants is the FOURTH algebra over Schema[A] (after JSON,
 * CBOR and YAML): one derivation per type serves the wire format,
 * the argument decode and now the tool declaration, so a tool's
 * signature cannot drift from its parser.
 */
final case class ToolSpec(name: String, description: String, schema: Json)

object ToolSpec {

  /** the algebra, now `okay.codec.JsonSchema` — kept here as a
   * delegation so no caller of a tool declaration notices the move */
  def jsonSchema[A](s: Schema[A]): Json = JsonSchema.of(s)

  /** declare a tool from its argument type — nothing hand-written */
  def apply[A](name: String, description: String)(using s: Schema[A]): ToolSpec =
    ToolSpec(name, description, jsonSchema(s))

  /** decode a call's arguments with the SAME Schema that declared it */
  def args[A](call: ToolCall)(using s: Schema[A]): Either[String, A] =
    Json.decode(s)(call.args)
}
