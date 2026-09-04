package okay.codec

/**
 * JSON Schema: the FOURTH algebra over `Schema[A]`.
 *
 * After the Json, Cbor and YAML algebras, this is the one a model or a
 * tool declaration wants — the same derivation serving the wire
 * format, the argument decode and now the declaration, so a tool's
 * signature cannot drift from its parser.
 *
 * It lived in `okay.agent.ToolSpec` until the classification tiers
 * moved to their own module and only this function tied them back to
 * the agent loop. It was never about agents: it is a fold over
 * `Schema`, and it belongs beside the folds it is a sibling of.
 * `ToolSpec.jsonSchema` still exists and delegates here.
 */
object JsonSchema {

  /** a datatype's shape as a JSON Schema value */
  def of[A](s: Schema[A]): Json = s match
    // a wrapper does not exist to the tool schema — a Secret is a string
    case Schema.SIso(u, _, _) => of(u())
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
    case Schema.SOption(inner) => of(inner())   // optionality is in `required`
    case Schema.SList(inner) => obj(
      "type" -> Json.JStr("array"),
      "items" -> of(inner()))
    case Schema.SVector(inner) => obj(
      "type" -> Json.JStr("array"),
      "items" -> of(inner()))
    case p: Schema.SProduct[A] =>
      // a DEFAULTED field is not required (the model may omit it —
      // decode falls back to the declaration) and advertises its
      // default, encoded by the field's own schema
      def defaulted(i: Int) = p.defaults.lift(i).flatten
      val props = p.fields.zipWithIndex.map { case ((n, f), i) =>
        val base = of(f())
        (n, defaulted(i) match
          case Some(_) => base match
            case Json.JObj(fs) => Json.JObj(fs :+
              ("default" -> Json.parse(p.defaultAt(i)([X] => (sc: Schema[X], x: X) => Json.encode(sc)(x)).get)))
            case other => other
          case None => base)
      }
      val required = p.fields.zipWithIndex.collect {
        case ((n, f), i) if !f().isInstanceOf[Schema.SOption[?]]
          && defaulted(i).isEmpty => Json.JStr(n)
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
          "properties" -> Json.JObj(Vector((n, of(c())))),
          "required" -> Json.JArr(Vector(Json.JStr(n))))
      }))

  private def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

}
