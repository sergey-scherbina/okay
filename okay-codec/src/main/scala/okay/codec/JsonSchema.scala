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
 *
 * Since schema-fold stage 1 it is literally that: `Schema.fold` with
 * this file's `Algebra`, no `match` on the GADT here. Byte-for-byte
 * what the hand-rolled version answered on every non-recursive
 * schema; on a RECURSIVE one the hand-rolled version descended for
 * ever, and this answers `$defs`/`$ref` — the back edge the fold
 * hands the algebra as `ref(name)` becomes `{"$ref": "#/$defs/name"}`,
 * and every name referenced that way is declared once under `$defs`
 * at the root.
 */
object JsonSchema {

  /** a datatype's shape as a JSON Schema value. `vocabularies` says
   * whether an enumeration's values are declared (`enum`): true for a
   * contract or a tool declaration, which is what they are for; false
   * for a PROMPT, where the same words rendered in the schema cost a
   * 4B model 1.7 macro-F1 points on the fixture, deterministically
   * (codec-jsonschema-refinement-enum) — the prompt states the
   * vocabulary in prose, once, where it was measured to help */
  def of[A](s: Schema[A], vocabularies: Boolean = true): Json =
    val alg = Algebra(vocabularies)
    val root = Schema.fold(s)(alg)
    if alg.defs.isEmpty then root
    else root match
      case Json.JObj(fs) => Json.JObj(fs :+ ("$defs" -> Json.JObj(alg.defs.toVector)))
      case other => other

  /** the constant carrier: every node answers a Json, whatever its A */
  private type K[A] = Json

  private final class Algebra(vocabularies: Boolean) extends Schema.Algebra[K]:
    /** the named nodes some back edge pointed at, in first-seen order */
    val defs = scala.collection.mutable.LinkedHashMap.empty[String, Json]
    private val referenced = scala.collection.mutable.LinkedHashSet.empty[String]

    def int = obj("type" -> Json.JStr("integer"))
    def long = obj("type" -> Json.JStr("integer"))
    def double = obj("type" -> Json.JStr("number"))
    def bool = obj("type" -> Json.JStr("boolean"))
    def string = obj("type" -> Json.JStr("string"))
    def char = obj("type" -> Json.JStr("string"),
      "minLength" -> Json.JNum(1), "maxLength" -> Json.JNum(1))
    // the JSON Schema vocabulary for bytes, and it matches what the
    // Json algebra actually writes — a tool taking binary input tells
    // the model exactly how to send it
    def bytes = obj("type" -> Json.JStr("string"),
      "contentEncoding" -> Json.JStr("base64"))
    def option[A](of: () => Json) = of()   // optionality is in `required`
    def list[A](of: () => Json) = obj("type" -> Json.JStr("array"), "items" -> of())
    def vector[A](of: () => Json) = obj("type" -> Json.JStr("array"), "items" -> of())

    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[K, Any])]) =
      // a DEFAULTED field is not required (the model may omit it —
      // decode falls back to the declaration) and advertises its
      // default, encoded by the field's own schema
      def defaulted(i: Int) = p.defaults.lift(i).flatten
      val props = fields.zipWithIndex.map { case ((n, e), i) =>
        val base = e()
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
      declared(p.name, obj(
        "type" -> Json.JStr("object"),
        "properties" -> Json.JObj(props),
        "required" -> Json.JArr(required)))

    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[K, A])]) =
      // a sum is one-of, each case tagged by its name (the same
      // encoding Json and Cbor use, so decode round-trips)
      declared(su.name, obj("oneOf" -> Json.JArr(cases.map { (n, c) =>
        obj(
          "type" -> Json.JStr("object"),
          "properties" -> Json.JObj(Vector((n, c()))),
          "required" -> Json.JArr(Vector(Json.JStr(n))))
      })))

    // a wrapper does not exist to the tool schema — a Secret is a string —
    // unless it names its vocabulary (`Schema.enumeration`), which the
    // declaration carries as `enum` beside the underlying type
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Json) = iso.vocabulary match
      case Some(vs) if vocabularies => under() match
        case Json.JObj(fs) => Json.JObj(fs :+ ("enum" -> Json.JArr(vs.map(v => Json.parse(Json.encode(iso.under())(v))))))
        case other => other
      case _ => under()

    def ref[A](name: String) =
      referenced += name
      obj("$ref" -> Json.JStr(s"#/$$defs/$name"))

    /** a named node's finished schema: inline where it stands (so a
      * non-recursive schema is byte-for-byte what it was), and ALSO
      * under `$defs` if a back edge pointed at it while it was built */
    private def declared(name: String, built: Json): Json =
      if referenced.contains(name) then defs(name) = built
      built

  private def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

}
