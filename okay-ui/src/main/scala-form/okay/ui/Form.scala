package okay.ui

import okay.codec.{Json, Schema}

/**
 * The fifth algebra over Schema (after JSON, CBOR, YAML and JSON
 * Schema): a FORM — rendered from the same Schema that decodes it, so
 * a form cannot drift from its parser, for the reason ToolSpec's
 * declaration cannot.
 *
 * v1 renders what MCP elicitation is allowed to ask for — a flat
 * product of primitives (the elicitation spec restricts requested
 * schemas to exactly that) — plus Option for the unrequired. A
 * nested product, list or sum renders as a Text naming itself, which
 * is the honest v1 answer.
 *
 * The form's STATE is the partial Json value; keys are field names;
 * `edit` folds one event in, coercing by the field's schema; `decode`
 * is the same decoder the wire uses.
 */
object Form {

  /** the form of A, over its partial value */
  def of[A](using s: Schema[A]): Json => Ui = j => render(s, j)

  def render[A](s: Schema[A], value: Json): Ui = s match
    case p: Schema.SProduct[A] =>
      Ui.Column(p.fields.map((name, f) => field(name, f(), get(value, name))).toVector)
    case _ => Ui.Text(s"unsupported form: $s")

  private def field(name: String, s: Schema[?], v: Option[Json]): Ui = s match
    case Schema.SOption(of) => field(name + " (optional)", of(), v) match
      case Ui.Input(value, _, label) => Ui.Input(value, key = name, label)
      case Ui.Check(on, _, label) => Ui.Check(on, key = name, label)
      case other => other
    case Schema.SBool => Ui.Check(v.contains(Json.JBool(true)), key = name, label = name)
    case Schema.SInt | Schema.SLong | Schema.SDouble => Ui.Input(v.collect {
      case Json.JNum(n) => Json.print(Json.JNum(n)) }.getOrElse(""), key = name, label = name)
    case Schema.SString => Ui.Input(v.collect {
      case Json.JStr(x) => x }.getOrElse(""), key = name, label = name)
    case other => Ui.Text(s"unsupported field: $name")

  /** fold one event into the partial value, typed by the schema */
  def edit[A](using s: Schema[A])(value: Json, e: Event): Json = s match
    case p: Schema.SProduct[A] => e match
      case Event.Edited(key, text) =>
        p.fields.collectFirst { case (n, f) if n == key => f() } match
          case None => value
          case Some(fs) => set(value, key, coerce(fs, text))
      case Event.Toggled(key, on) => set(value, key, Json.JBool(on))
      case _ => value
    case _ => value

  private def coerce(s: Schema[?], text: String): Json = s match
    case Schema.SOption(of) => coerce(of(), text)
    case Schema.SInt | Schema.SLong =>
      text.toLongOption.fold(Json.JStr(text))(n => Json.JNum(n.toDouble))
    case Schema.SDouble =>
      text.toDoubleOption.fold(Json.JStr(text))(Json.JNum(_))
    case _ => Json.JStr(text)

  /** the SAME decoder the wire uses — that is the whole point */
  def decode[A](using s: Schema[A]): Json => Either[String, A] = Json.decode(s)

  // ---- the dynamic side: a JSON Schema, as elicitation carries one

  /** a form from a JSON Schema value (flat object of primitives —
   * exactly what elicitation may request) */
  def ofSchema(schema: Json): Json => Ui = value =>
    Ui.Column(properties(schema).map((name, p) => dynField(name, p, get(value, name))).toVector)

  private def dynField(name: String, prop: Json, v: Option[Json]): Ui =
    str(prop, "type").getOrElse("string") match
      case "boolean" => Ui.Check(v.contains(Json.JBool(true)), key = name, label = name)
      case _ => prop match
        case _ if field0(prop, "enum").isDefined =>
          val opts = field0(prop, "enum") match
            case Some(Json.JArr(vs)) => vs.collect { case Json.JStr(x) => x }
            case _ => Vector.empty
          val sel = v.collect { case Json.JStr(x) => opts.indexOf(x) }.getOrElse(-1)
          Ui.Select(opts, math.max(sel, 0), key = name)
        case _ => Ui.Input(v.collect {
          case Json.JStr(x) => x
          case Json.JNum(n) => Json.print(Json.JNum(n)) }.getOrElse(""),
          key = name, label = name)

  def editSchema(schema: Json, value: Json, e: Event): Json = e match
    case Event.Edited(key, text) =>
      val t = properties(schema).collectFirst { case (n, p) if n == key => str(p, "type") }.flatten
      val coerced = t match
        case Some("number") | Some("integer") =>
          text.toDoubleOption.fold(Json.JStr(text))(Json.JNum(_))
        case _ => Json.JStr(text)
      set(value, key, coerced)
    case Event.Toggled(key, on) => set(value, key, Json.JBool(on))
    case Event.Chosen(key, i) =>
      val opts = properties(schema).collectFirst { case (n, p) if n == key => field0(p, "enum") }.flatten
      opts match
        case Some(Json.JArr(vs)) => vs.lift(i).fold(value)(set(value, key, _))
        case _ => value
    case _ => value

  private def properties(schema: Json): Vector[(String, Json)] = field0(schema, "properties") match
    case Some(Json.JObj(fs)) => fs
    case _ => Vector.empty

  // ---- small Json helpers (the codec keeps objects as Vectors)
  private def field0(j: Json, name: String): Option[Json] = j match
    case Json.JObj(fs) => fs.collectFirst { case (n, v) if n == name => v }
    case _ => None
  private def str(j: Json, name: String): Option[String] =
    field0(j, name).collect { case Json.JStr(s) => s }
  private def get(value: Json, name: String): Option[Json] = field0(value, name)
  private def set(value: Json, name: String, v: Json): Json = value match
    case Json.JObj(fs) =>
      if fs.exists(_._1 == name) then Json.JObj(fs.map((n, old) => if n == name then (n, v) else (n, old)))
      else Json.JObj(fs :+ (name, v))
    case _ => Json.JObj(Vector(name -> v))
}
