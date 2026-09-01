package okay.ui

import okay.!
import okay.codec.{Json, Schema}

/**
 * The fifth algebra over Schema (after JSON, CBOR, YAML and JSON
 * Schema): a FORM — rendered from the same Schema that decodes it, so
 * a form cannot drift from its parser, for the reason ToolSpec's
 * declaration cannot.
 *
 * v2 (specs/ui-toolkit.md) is total over the algebra: a nested
 * product is a titled section, a sum is a Select of its cases plus
 * the chosen case's subform, a list edits in place with add/remove.
 * Field keys are DOTTED PATHS (`addr.city`, `tags[2]`, `pet.$case`) —
 * events carry one string, the edit site parses it, and the Ui tree
 * itself stays flat, which leaves the diff/patch machinery untouched.
 * Errors are data: each failing field's message renders under that
 * field, and cross-field checks run on the DECODED value.
 *
 * The form's STATE is the partial Json value in the codec's own
 * shapes — `{"Case": {...}}` for a sum, an array for a list — so
 * what the form submits is literally what the wire decoder reads.
 */
object Form {

  /** the form of A, over its partial value */
  def of[A](using s: Schema[A]): Json => Ui = j => render(s, j, Vector.empty, "")

  /** the form with per-field errors shown under their fields */
  def ofWith[A](errors: Vector[(String, String)])(using s: Schema[A]): Json => Ui =
    j => render(s, j, errors, "")

  private def key(prefix: String, name: String): String =
    if prefix.isEmpty then name else s"$prefix.$name"

  private def errorsUnder(errors: Vector[(String, String)], k: String): Vector[Ui] =
    errors.collect { case (`k`, msg) => Ui.Text(s"! $msg", Style(bold = true)) }

  def render[A](s: Schema[A], value: Json, errors: Vector[(String, String)],
                prefix: String): Ui = s match
    case p: Schema.SProduct[A] =>
      Ui.Column(p.fields.flatMap { (name, f) =>
        val k = key(prefix, name)
        field(name, k, f(), get(value, name), errors) +: errorsUnder(errors, k)
      }.toVector)
    case su: Schema.SSum[?] => sumUi(su, value, errors, prefix, label = "")
    case other => Ui.Text(s"unsupported form: $other")

  /** a sum: the case Select, then the chosen case's subform */
  private def sumUi(su: Schema.SSum[?], value: Json,
                    errors: Vector[(String, String)], path: String,
                    label: String): Ui =
    val names = su.cases.map(_._1)
    val chosen = value match
      case Json.JObj(Vector((n, _))) => math.max(names.indexOf(n), 0)
      case _ => 0
    val inner = value match
      case Json.JObj(Vector((_, v))) => v
      case _ => Json.JObj(Vector.empty)
    val caseSchema = su.cases(chosen)._2()
    val head = Ui.Select(names.toVector, chosen, key = s"$path.$$case".stripPrefix("."))
    val body = caseSchema match
      case p: Schema.SProduct[?] if p.fields.nonEmpty =>
        Vector(render(p, inner, errors, path))
      case _ => Vector.empty
    Ui.Column((if label.isEmpty then Vector(head)
               else Vector(Ui.Text(label, Style(bold = true)), head)) ++ body)

  private def field(name: String, k: String, s: Schema[?], v: Option[Json],
                    errors: Vector[(String, String)]): Ui = s match
    case Schema.SIso(u, _, _) => field(name, k, u(), v, errors)
    case Schema.SOption(of) => field(name + " (optional)", k, of(), v, errors) match
      case Ui.Input(value, _, label) => Ui.Input(value, key = k, label)
      case Ui.Check(on, _, label) => Ui.Check(on, key = k, label)
      case other => other
    case Schema.SBool => Ui.Check(v.contains(Json.JBool(true)), key = k, label = name)
    case Schema.SInt | Schema.SLong | Schema.SDouble => Ui.Input(v.collect {
      case Json.JNum(n) => Json.print(Json.JNum(n)) }.getOrElse(""), key = k, label = name)
    case Schema.SString => Ui.Input(v.collect {
      case Json.JStr(x) => x }.getOrElse(""), key = k, label = name)
    case p: Schema.SProduct[?] =>
      // the titled section: the nested fields carry the dotted prefix
      Ui.Column(Ui.Text(name, Style(bold = true)) +:
        render(p, v.getOrElse(Json.JObj(Vector.empty)), errors, k).asInstanceOf[Ui.Column].children)
    case su: Schema.SSum[?] => sumUi(su, v.getOrElse(Json.JObj(Vector.empty)), errors, k, name)
    case Schema.SList(of) => listUi(name, k, of(), v, errors)
    case Schema.SVector(of) => listUi(name, k, of(), v, errors)
    case other => Ui.Text(s"unsupported field: $name")

  /** items in order, each with its remover, and the adder at the end */
  private def listUi(name: String, k: String, item: Schema[?],
                     v: Option[Json], errors: Vector[(String, String)]): Ui =
    val items = v match
      case Some(Json.JArr(vs)) => vs
      case _ => Vector.empty
    Ui.Column(Ui.Text(name, Style(bold = true)) +:
      items.zipWithIndex.flatMap { (iv, i) =>
        val ik = s"$k[$i]"
        Vector(Ui.Row(Vector(
          field(s"$name $i", ik, item, Some(iv), errors),
          Ui.Button("-", key = s"$ik$$del")))) ++ errorsUnder(errors, ik)
      } :+ Ui.Button("+", key = s"$k$$add"))

  // ---- editing: one event in, routed by its path -------------------

  /** fold one event into the partial value, typed by the schema */
  def edit[A](using s: Schema[A])(value: Json, e: Event): Json =
    val (k, ev) = e match
      case Event.Edited(k, t) => (k, Edit.Set(t))
      case Event.Toggled(k, on) => (k, Edit.Flag(on))
      case Event.Chosen(k, i) => (k, Edit.Choose(i))
      case Event.Pressed(k) if k.endsWith("$add") => (k.dropRight(4), Edit.Add)
      case Event.Pressed(k) if k.endsWith("$del") => (k.dropRight(4), Edit.Del)
      case _ => ("", Edit.None)
    if k.isEmpty && ev == Edit.None then value
    else editAt(s, value, Path.parse(k), ev)

  private enum Edit:
    case Set(text: String)
    case Flag(on: Boolean)
    case Choose(i: Int)
    case Add, Del, None

  /** one step of a dotted path: a field, an index, or the case knob */
  private enum Seg:
    case Field(name: String)
    case Index(name: String, i: Int)
    case Case

  private object Path:
    def parse(k: String): List[Seg] = k.split('.').toList.map {
      case "$case" => Seg.Case
      case s if s.endsWith("]") && s.contains('[') =>
        val at = s.lastIndexOf('[')
        Seg.Index(s.take(at), s.slice(at + 1, s.length - 1).toInt)
      case s => Seg.Field(s)
    }

  private def editAt(s: Schema[?], value: Json, path: List[Seg], ev: Edit): Json =
    s match
      case Schema.SIso(u, _, _) => editAt(u(), value, path, ev)
      case _ => (s, path) match
        // the case knob: swap to the chosen case's empty object
        case (su: Schema.SSum[?], Seg.Case :: Nil) => ev match
          case Edit.Choose(i) if i >= 0 && i < su.cases.length =>
            Json.JObj(Vector(su.cases(i)._1 -> Json.JObj(Vector.empty)))
          case _ => value
        // routing INTO the chosen case
        case (su: Schema.SSum[?], _) =>
          val (name, cs) = value match
            case Json.JObj(Vector((n, _))) =>
              su.cases.find(_._1 == n).getOrElse(su.cases.head)
            case _ => su.cases.head
          val inner = value match
            case Json.JObj(Vector((_, v))) => v
            case _ => Json.JObj(Vector.empty)
          Json.JObj(Vector(name -> editAt(cs(), inner, path, ev)))
        case (p: Schema.SProduct[?], Seg.Field(n) :: rest) =>
          p.fields.find(_._1 == n) match
            case None => value
            case Some((_, fs)) =>
              // rest empty = the event addresses the field itself: a
              // scalar Set/Flag, or a list's Add — leaf serves both
              if rest.isEmpty then set(value, n, leaf(fs(), ev, get(value, n)))
              else set(value, n, editAt(fs(), get(value, n).getOrElse(empty(fs())), rest, ev))
        case (p: Schema.SProduct[?], Seg.Index(n, i) :: rest) =>
          p.fields.find(_._1 == n) match
            case None => value
            case Some((_, fs)) => itemSchema(fs()) match
              case None => value
              case Some(item) =>
                val arr = get(value, n) match
                  case Some(Json.JArr(vs)) => vs
                  case _ => Vector.empty
                if i < 0 || i >= arr.length then value
                else if rest.isEmpty && ev == Edit.Del then
                  set(value, n, Json.JArr(arr.patch(i, Nil, 1)))
                else if rest.isEmpty && !isList(item) && !isComposite(item) then
                  set(value, n, Json.JArr(arr.updated(i, leaf(item, ev, Some(arr(i))))))
                else set(value, n, Json.JArr(arr.updated(i, editAt(item, arr(i), rest, ev))))
        // list add/del addressed at the FIELD itself
        case (p: Schema.SProduct[?], Nil) => value
        case _ => value

  private def isList(s: Schema[?]): Boolean = s match
    case Schema.SIso(u, _, _) => isList(u())
    case Schema.SList(_) | Schema.SVector(_) => true
    case _ => false

  private def isComposite(s: Schema[?]): Boolean = s match
    case Schema.SIso(u, _, _) => isComposite(u())
    case _: Schema.SProduct[?] | _: Schema.SSum[?] => true
    case _ => false

  private def itemSchema(s: Schema[?]): Option[Schema[?]] = s match
    case Schema.SIso(u, _, _) => itemSchema(u())
    case Schema.SList(of) => Some(of())
    case Schema.SVector(of) => Some(of())
    case _ => None

  private def empty(s: Schema[?]): Json = s match
    case Schema.SIso(u, _, _) => empty(u())
    case Schema.SList(_) | Schema.SVector(_) => Json.JArr(Vector.empty)
    case su: Schema.SSum[?] => Json.JObj(Vector(su.cases.head._1 -> Json.JObj(Vector.empty)))
    case _ => Json.JObj(Vector.empty)

  /** a leaf edit: Set/Flag against the field's own schema; Add on a
   * list appends that item's empty */
  private def leaf(s: Schema[?], ev: Edit, old: Option[Json]): Json = (s, ev) match
    case (Schema.SIso(u, _, _), _) => leaf(u(), ev, old)
    case (Schema.SOption(of), _) => leaf(of(), ev, old)
    case (_, Edit.Flag(on)) => Json.JBool(on)
    case (Schema.SInt | Schema.SLong, Edit.Set(t)) =>
      t.toLongOption.fold(Json.JStr(t))(n => Json.JNum(n.toDouble))
    case (Schema.SDouble, Edit.Set(t)) =>
      t.toDoubleOption.fold(Json.JStr(t))(Json.JNum(_))
    case (_, Edit.Set(t)) => Json.JStr(t)
    case (Schema.SList(of), Edit.Add) =>
      Json.JArr((old match { case Some(Json.JArr(vs)) => vs; case _ => Vector.empty })
        :+ leafEmpty(of()))
    case (Schema.SVector(of), Edit.Add) =>
      Json.JArr((old match { case Some(Json.JArr(vs)) => vs; case _ => Vector.empty })
        :+ leafEmpty(of()))
    case _ => old.getOrElse(Json.JNull)

  private def leafEmpty(s: Schema[?]): Json = s match
    case Schema.SIso(u, _, _) => leafEmpty(u())
    case Schema.SString => Json.JStr("")
    case Schema.SBool => Json.JBool(false)
    case Schema.SInt | Schema.SLong | Schema.SDouble => Json.JStr("")
    case other => empty(other)

  // ---- errors as data ----------------------------------------------

  /** per-field failures, dotted paths — each renders under its field */
  def errors[A](value: Json)(using s: Schema[A]): Vector[(String, String)] =
    errorsOf(s, value, "")

  private def errorsOf(s: Schema[?], value: Json, prefix: String): Vector[(String, String)] =
    s match
      case Schema.SIso(u, _, _) => errorsOf(u(), value, prefix)
      case p: Schema.SProduct[?] =>
        p.fields.toVector.flatMap { (name, f) =>
          val k = key(prefix, name)
          val fs = f()
          get(value, name) match
            case None => fieldError(fs, None, k)
            case Some(v) => fs match
              case np: Schema.SProduct[?] => errorsOf(np, v, k)
              case su: Schema.SSum[?] => errorsOf(su, v, k)
              case Schema.SList(of) => listErrors(of(), v, k)
              case Schema.SVector(of) => listErrors(of(), v, k)
              case other => fieldError(other, Some(v), k)
        }
      case su: Schema.SSum[?] => value match
        case Json.JObj(Vector((n, v))) =>
          su.cases.find(_._1 == n) match
            case Some((_, cs)) => errorsOf(cs(), v, prefix)
            case None => Vector(prefix -> s"unknown case '$n'")
        case _ => Vector(key(prefix, "$case") -> "choose one")
      case other => fieldError(other, Some(value), prefix)

  private def listErrors(item: Schema[?], v: Json, k: String): Vector[(String, String)] =
    v match
      case Json.JArr(vs) => vs.zipWithIndex.flatMap { (iv, i) =>
        item match
          case p: Schema.SProduct[?] => errorsOf(p, iv, s"$k[$i]")
          case su: Schema.SSum[?] => errorsOf(su, iv, s"$k[$i]")
          case other => fieldError(other, Some(iv), s"$k[$i]")
      }
      case _ => Vector.empty

  private def fieldError(s: Schema[?], v: Option[Json], k: String): Vector[(String, String)] =
    decodeField(s, v) match
      case Left(msg) => Vector(k -> msg)
      case Right(_) => Vector.empty

  private def decodeField(s: Schema[?], v: Option[Json]): Either[String, Any] = s match
    case Schema.SIso(u, to, _) =>
      decodeField(u(), v).flatMap(x => to.asInstanceOf[Any => Either[String, Any]](x))
    case Schema.SOption(of) => v match
      case None | Some(Json.JNull) | Some(Json.JStr("")) => Right(None)
      case Some(x) => decodeField(of(), Some(x)).map(Some(_))
    case other => v match
      case None => Left("required")
      case Some(x) => Json.decode(other.asInstanceOf[Schema[Any]])(x)

  /** the SAME decoder the wire uses — that is the whole point */
  def decode[A](using s: Schema[A]): Json => Either[String, A] = Json.decode(s)

  // ---- the dynamic side: a JSON Schema, as elicitation carries one
  // (flat by elicitation's own spec — v1 by design, specs/ui-toolkit.md)

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

  // ---- flows: a form as a SCENARIO (show, edit, submit, retry)

  /** a cross-field rule: reads the DECODED value, answers failures
   * as (field path | "" for the form, message) */
  type Check[A] = A => Vector[(String, String)]

  /**
   * Ask for an A: the typed form as a Dialog program. Ok submits —
   * and an invalid value does NOT: per-field errors render under
   * their fields, cross-field failures (run only once the decode
   * succeeds) name theirs, and the flow continues. Cancel (or the
   * host closing) answers None.
   */
  def ask[A](message: String, checks: Check[A]*)(using s: Schema[A]): Option[A] ! Dialog =
    def loop(j: Json, errs: Vector[(String, String)]): Option[A] ! Dialog =
      Dialog.show(asked(message, errs.collect { case ("", m) => m },
        ofWith[A](errs).apply(j))).flatMap {
        case Event.Pressed("$ok") =>
          val fieldErrs = errors[A](j)
          if fieldErrs.nonEmpty then loop(j, fieldErrs)
          else decode[A].apply(j) match
            case Left(err) => loop(j, Vector("" -> err))
            case Right(a) =>
              val crossErrs = checks.toVector.flatMap(_(a))
              if crossErrs.isEmpty then okay.pure(Some(a))
              else loop(j, crossErrs)
        case Event.Pressed("$cancel") | Event.Closed => okay.pure(None)
        case e => loop(edit[A](j, e), Vector.empty)
      }

    loop(Json.JObj(Vector.empty), Vector.empty)

  /** the same flow over a JSON Schema — what elicitation asks with */
  def askSchema(message: String, schema: Json): Option[Json] ! Dialog =
    def loop(j: Json, error: Option[String]): Option[Json] ! Dialog =
      Dialog.show(asked(message, error.toVector, ofSchema(schema)(j))).flatMap {
        case Event.Pressed("$ok") => okay.pure(Some(j))
        case Event.Pressed("$cancel") | Event.Closed => okay.pure(None)
        case e => loop(editSchema(schema, j, e), None)
      }

    loop(Json.JObj(Vector.empty), None)

  private def asked(message: String, formErrors: Vector[String], form: Ui): Ui =
    Ui.Column(Vector(Ui.Text(message)) ++
      formErrors.map(e => Ui.Text(s"! $e", Style(bold = true))) ++
      Vector(form, Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))

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
