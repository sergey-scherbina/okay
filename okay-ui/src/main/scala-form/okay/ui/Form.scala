package okay.ui

import okay.{!, +, pure, Pure, Cont, reset, />}
import okay.given
import okay.codec.{Codecs, Json, Schema}

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

  /**
   * The form, as a fold (specs/schema-fold.md, stage 2): `Schema.fold`
   * with `Render` below, the value walk on `Schema.Step`. What used to
   * be four mutually recursive functions (`render`/`field`/`sumUi`/
   * `listUi`) and their four trampoline twins is one algebra: a node
   * renders itself from its ENVIRONMENT — the errors, its dotted key,
   * and its label — and the parent chooses the child's environment.
   * The root has no label, which is exactly how a root product differs
   * from a nested one (a titled section) and how a root leaf is
   * "unsupported form" where a field leaf is an Input.
   *
   * The value walked is a `Json` (the form's partial value), not the
   * schema's own `A` — so the carrier is constant in `A`, and
   * `Step.node` (the algebra names each child's env and value) is the
   * road, not `Step.fields`. Depth logic lives in `Step`, not here;
   * this door's own copy (`renderC` and friends) is deleted.
   */
  def render[A](s: Schema[A], value: Json, errors: Vector[(String, String)],
                prefix: String): Ui =
    Schema.Step.walk(renderer(s), RenderEnv(errors, prefix, ""), Some(value))

  private final case class RenderEnv(errors: Vector[(String, String)], key: String, name: String):
    def root: Boolean = name.isEmpty
    def field(n: String): RenderEnv = RenderEnv(errors, Form.key(key, n), n)

  private type Render[A] = Schema.Step[RenderEnv, Option[Json], Ui]
  private val renderer = Schema.Folded[Render](new Schema.Algebra[Render]:
    import Schema.Step
    private def unsupported(e: RenderEnv, node: String): Ui =
      if e.root then Ui.Text(s"unsupported form: $node") else Ui.Text(s"unsupported field: ${e.name}")
    private def number(node: String) = Step.leaf[RenderEnv, Option[Json], Ui]((e, v) =>
      if e.root then unsupported(e, node)
      else Ui.Input(v.collect { case Json.JNum(n) => Json.print(Json.JNum(n)) }.getOrElse(""),
        key = e.key, label = e.name, kind = InputKind.Number))
    def int = number("SInt")
    def long = number("SLong")
    def double = number("SDouble")
    def bool = Step.leaf((e, v: Option[Json]) =>
      if e.root then unsupported(e, "SBool")
      else Ui.Check(v.contains(Json.JBool(true)), key = e.key, label = e.name))
    def string = Step.leaf((e, v: Option[Json]) =>
      if e.root then unsupported(e, "SString")
      else Ui.Input(v.collect { case Json.JStr(x) => x }.getOrElse(""), key = e.key, label = e.name))
    def char = Step.leaf((e, _: Option[Json]) => unsupported(e, "SChar"))
    def bytes = Step.leaf((e, _: Option[Json]) => unsupported(e, "SBytes"))

    /** "(optional)" on the label, the same key, the same depth */
    def option[A](o: Schema.SOption[A], of: () => Render[A]) = Step.adapt[RenderEnv, Option[Json], Ui](
      e => if e.root then e else e.copy(name = e.name + " (optional)"),
      identity, of)

    private def items(each: () => Render[?]): Step[RenderEnv, Option[Json], Ui] =
      Step.node[RenderEnv, Option[Json], Ui, Vector[Ui]](
        (_, _) => Vector.empty,
        (e, v) =>
          val vs = v match
            case Some(Json.JArr(xs)) => xs
            case _ => Vector.empty
          vs.zipWithIndex.map((iv, i) => Step.Kid(each(), RenderEnv(e.errors, s"${e.key}[$i]", s"${e.name} $i"), Some(iv))),
        _ :+ _,
        (e, _, uis) =>
          if e.root then unsupported(e, "a list")
          else Ui.Column(Ui.Text(e.name, Style(bold = true)) +:
            uis.zipWithIndex.flatMap { (ui, i) =>
              val ik = s"${e.key}[$i]"
              Vector(Ui.Row(Vector(ui, Ui.Button("-", key = s"$ik$$del")))) ++ errorsUnder(e.errors, ik)
            } :+ Ui.Button("+", key = s"${e.key}$$add")))
    def list[A](l: Schema.SList[A], of: () => Render[A]) = items(of)
    def vector[A](vs: Schema.SVector[A], of: () => Render[A]) = items(of)

    /** a product: its fields in order, each with its errors under it;
      * titled by its label unless it is the root */
    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Render, Any])]) =
      Step.node[RenderEnv, Option[Json], Ui, Vector[Ui]](
        (_, _) => Vector.empty,
        (e, v) =>
          val value = v.getOrElse(Json.JObj(Vector.empty))
          fields.map((n, edge) => Step.Kid(edge(), e.field(n), get(value, n))),
        _ :+ _,
        (e, _, uis) =>
          val children = fields.zip(uis).flatMap((nf, ui) => ui +: errorsUnder(e.errors, Form.key(e.key, nf._1)))
          if e.root then Ui.Column(children) else Ui.Column(Ui.Text(e.name, Style(bold = true)) +: children))

    /** a sum: the case Select, then the chosen case's subform — only
      * when that case is a product with fields, as before */
    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Render, A])]) =
      val names = su.cases.map(_._1)
      def chosen(value: Json): Int = value match
        case Json.JObj(Vector((n, _))) => math.max(names.indexOf(n), 0)
        case _ => 0
      def inner(value: Json): Json = value match
        case Json.JObj(Vector((_, v))) => v
        case _ => Json.JObj(Vector.empty)
      Step.node[RenderEnv, Option[Json], Ui, Vector[Ui]](
        (e, v) =>
          val head = Ui.Select(names.toVector, chosen(v.getOrElse(Json.JObj(Vector.empty))),
            key = s"${e.key}.$$case".stripPrefix("."))
          if e.root then Vector(head) else Vector(Ui.Text(e.name, Style(bold = true)), head),
        (e, v) =>
          val value = v.getOrElse(Json.JObj(Vector.empty))
          val c = chosen(value)
          su.cases(c)._2() match
            case p: Schema.SProduct[?] if p.fields.nonEmpty =>
              Vector(Step.Kid(cases(c)._2(), RenderEnv(e.errors, e.key, ""), Some(inner(value))))
            case _ => Vector.empty,
        _ :+ _,
        (_, _, all) => Ui.Column(all))

    /** a wrapper does not exist to the form */
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Render[B]) =
      Step.via[RenderEnv, Option[Json], Option[Json], Ui](identity, under)
    def ref[A](name: String) =
      throw IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  )

  // ---- editing: one event in, routed by its path -------------------

  /** the hybrid's one event (specs/frontend.md stage 2): the edits a
   * client folded locally, folded here through the SAME `edit` a live
   * edit takes — so a submitted form cannot decode differently from
   * one typed over the wire */
  def submitted[A](using s: Schema[A])(value: Json, e: Event): Json = e match
    case Event.Submitted(_, edits) => edits.foldLeft(value)(edit[A])
    case other => edit[A](value, other)

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

  /**
   * `editAt` recurses once per PATH SEGMENT — and a path is a dotted
   * STRING an `Event` carries, whatever length whoever built it
   * chose (`submitted`'s own doc: "the edits a client folded locally
   * ... folded here through the SAME edit a live edit takes" — a
   * remote submission road exists). Same fix, same shape
   * (form-recursive-depth-safety): `Codecs.NativeThreshold`-then-
   * `Cont.defer`, mirroring `mergePatch`/`mergePatchC` (okay-codec's
   * `Json.scala`) — a value-returning fold, not a side-effecting one.
   */
  private def editAt(s: Schema[?], value: Json, path: List[Seg], ev: Edit): Json =
    editAtAt(s, value, path, ev, 0)

  private def editAtAt(s: Schema[?], value: Json, path: List[Seg], ev: Edit, open: Int): Json =
    if open >= Codecs.NativeThreshold then reset(editAtC[Json](s, value, path, ev, open))
    else editAtNative(s, value, path, ev, open)

  private def editAtNative(s: Schema[?], value: Json, path: List[Seg], ev: Edit, open: Int): Json =
    s match
      case Schema.SIso(u, _, _) => editAtAt(u(), value, path, ev, open)
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
          Json.JObj(Vector(name -> editAtAt(cs(), inner, path, ev, open + 1)))
        case (p: Schema.SProduct[?], Seg.Field(n) :: rest) =>
          p.fields.find(_._1 == n) match
            case None => value
            case Some((_, fs)) =>
              // rest empty = the event addresses the field itself: a
              // scalar Set/Flag, or a list's Add — leaf serves both
              if rest.isEmpty then set(value, n, leaf(fs(), ev, get(value, n)))
              else set(value, n, editAtAt(fs(), get(value, n).getOrElse(empty(fs())), rest, ev, open + 1))
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
                else set(value, n, Json.JArr(arr.updated(i, editAtAt(item, arr(i), rest, ev, open + 1))))
        // list add/del addressed at the FIELD itself
        case (_: Schema.SProduct[?], Nil) => value
        case _ => value

  private def editAtC[R](s: Schema[?], value: Json, path: List[Seg], ev: Edit, open: Int): Json /> R =
    s match
      case Schema.SIso(u, _, _) => Cont.defer(() => editAtC[R](u(), value, path, ev, open))(v => Cont.Pure(v))
      case _ => (s, path) match
        case (su: Schema.SSum[?], Seg.Case :: Nil) => ev match
          case Edit.Choose(i) if i >= 0 && i < su.cases.length =>
            Cont.Pure(Json.JObj(Vector(su.cases(i)._1 -> Json.JObj(Vector.empty))))
          case _ => Cont.Pure(value)
        case (su: Schema.SSum[?], _) =>
          val (name, cs) = value match
            case Json.JObj(Vector((n, _))) => su.cases.find(_._1 == n).getOrElse(su.cases.head)
            case _ => su.cases.head
          val inner = value match
            case Json.JObj(Vector((_, v))) => v
            case _ => Json.JObj(Vector.empty)
          Cont.defer(() => editAtC[R](cs(), inner, path, ev, open + 1)) { v =>
            Cont.Pure(Json.JObj(Vector(name -> v)))
          }
        case (p: Schema.SProduct[?], Seg.Field(n) :: rest) =>
          p.fields.find(_._1 == n) match
            case None => Cont.Pure(value)
            case Some((_, fs)) =>
              if rest.isEmpty then Cont.Pure(set(value, n, leaf(fs(), ev, get(value, n))))
              else Cont.defer(() => editAtC[R](fs(), get(value, n).getOrElse(empty(fs())), rest, ev, open + 1)) { v =>
                Cont.Pure(set(value, n, v))
              }
        case (p: Schema.SProduct[?], Seg.Index(n, i) :: rest) =>
          p.fields.find(_._1 == n) match
            case None => Cont.Pure(value)
            case Some((_, fs)) => itemSchema(fs()) match
              case None => Cont.Pure(value)
              case Some(item) =>
                val arr = get(value, n) match
                  case Some(Json.JArr(vs)) => vs
                  case _ => Vector.empty
                if i < 0 || i >= arr.length then Cont.Pure(value)
                else if rest.isEmpty && ev == Edit.Del then
                  Cont.Pure(set(value, n, Json.JArr(arr.patch(i, Nil, 1))))
                else if rest.isEmpty && !isList(item) && !isComposite(item) then
                  Cont.Pure(set(value, n, Json.JArr(arr.updated(i, leaf(item, ev, Some(arr(i)))))))
                else Cont.defer(() => editAtC[R](item, arr(i), rest, ev, open + 1)) { v =>
                  Cont.Pure(set(value, n, Json.JArr(arr.updated(i, v))))
                }
        case (_: Schema.SProduct[?], Nil) => Cont.Pure(value)
        case _ => Cont.Pure(value)

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

  /**
   * Validation, as a fold — the same shape as `render`'s. A node
   * answers its errors from its dotted prefix and an OPTIONAL value
   * (absent is the parent's finding, the child's rule: an option is
   * fine absent, everything else is "required"). The rules are read
   * off the old `errorsOf` exactly: a nested product, sum, list or
   * vector is walked field by field; a leaf, a wrapper (`SIso`) or an
   * option is handed whole to the decoder that reads the wire, and
   * its one message is the error — which is why `iso` and `option`
   * here are leaves, not descents.
   */
  private def errorsOf(s: Schema[?], value: Json, prefix: String): Vector[(String, String)] =
    Schema.Step.walk(validator(s), prefix, Some(value))

  private type Errors = Vector[(String, String)]
  private type Validate[A] = Schema.Step[String, Option[Json], Errors]
  private val validator = Schema.Folded[Validate](new Schema.Algebra[Validate]:
    import Schema.Step
    private def whole(sc: Schema[?]) = Step.leaf[String, Option[Json], Errors]((k, v) => fieldError(sc, v, k))
    def int = whole(Schema.SInt)
    def long = whole(Schema.SLong)
    def double = whole(Schema.SDouble)
    def bool = whole(Schema.SBool)
    def string = whole(Schema.SString)
    def char = whole(Schema.SChar)
    def bytes = whole(Schema.SBytes)
    def option[A](o: Schema.SOption[A], of: () => Validate[A]) = whole(o)
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Validate[B]) = whole(iso)

    private def elements(sc: Schema[?], each: () => Validate[?]) =
      Step.node[String, Option[Json], Errors, Errors](
        (_, _) => Vector.empty,
        (k, v) => v match
          case Some(Json.JArr(vs)) => vs.zipWithIndex.map((iv, i) => Step.Kid(each(), s"$k[$i]", Some(iv)))
          case _ => Vector.empty,
        _ ++ _,
        (k, v, found) => v match
          case None => fieldError(sc, None, k)
          case _ => found)
    def list[A](l: Schema.SList[A], of: () => Validate[A]) = elements(l, of)
    def vector[A](vs: Schema.SVector[A], of: () => Validate[A]) = elements(vs, of)

    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Validate, Any])]) =
      Step.node[String, Option[Json], Errors, Errors](
        (_, _) => Vector.empty,
        (k, v) => v match
          case Some(value) => fields.map((n, edge) => Step.Kid(edge(), Form.key(k, n), get(value, n)))
          case None => Vector.empty,
        _ ++ _,
        (k, v, found) => v match
          case None => fieldError(p, None, k)
          case _ => found)

    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Validate, A])]) =
      Step.node[String, Option[Json], Errors, Errors](
        (_, _) => Vector.empty,
        (k, v) => v match
          case Some(Json.JObj(Vector((n, inner)))) =>
            val i = su.cases.indexWhere(_._1 == n)
            if i < 0 then Vector.empty else Vector(Step.Kid(cases(i)._2(), k, Some(inner)))
          case _ => Vector.empty,
        _ ++ _,
        (k, v, found) => v match
          case None => fieldError(su, None, k)
          case Some(Json.JObj(Vector((n, _)))) =>
            if su.cases.exists(_._1 == n) then found else Vector(k -> s"unknown case '$n'")
          case Some(_) => Vector(Form.key(k, "$case") -> "choose one"))
    def ref[A](name: String) =
      throw IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  )

  private def fieldError(s: Schema[?], v: Option[Json], k: String): Vector[(String, String)] =
    decodeField(s, v) match
      case Left(msg) => Vector(k -> msg)
      case Right(_) => Vector.empty

  private def decodeField[X](s: Schema[X], v: Option[Json]): Either[String, X] = s match
    case Schema.SIso(u, to, _) => decodeField(u(), v).flatMap(to)
    case o: Schema.SOption[?] => v match
      case None | Some(Json.JNull) | Some(Json.JStr("")) => Right(None)
      case Some(x) => decodeField(o.of(), Some(x)).map(Some(_))
    case other => v match
      case None => Left("required")
      case Some(x) => Json.decode(other)(x)

  /** the SAME decoder the wire uses — that is the whole point */
  def decode[A](using s: Schema[A]): Json => Either[String, A] = okay.codec.Codecs.json(s).decode

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

  /** an invalid submit, as a CONDITION (ui-direct): the errors and
   * which attempt this is — the policy decides how forgiving the
   * wizard is */
  final case class InvalidSubmit(errors: Vector[(String, String)], attempt: Int)
  /** the typed pair: an InvalidSubmit is answered with the forced A —
   * declared per ask via the local instance below */

  private enum Outcome[A]:
    case Done(a: A)
    case Retry()
    case Gave()

  /** the reask restart invoked — the default policy's answer, i.e.
   * exactly what `ask` always did */
  val forgiving: (Any, Vector[String]) => okay.Condition.Decision =
    case (_: InvalidSubmit, menu) if menu.contains("reask") =>
      okay.Condition.Decision.Invoke("reask", ())
    case _ => okay.Condition.Decision.Fail

  /** give up after n attempts, reask before that */
  def patience(n: Int): (Any, Vector[String]) => okay.Condition.Decision =
    case (InvalidSubmit(_, k), menu) =>
      if k >= n && menu.contains("giveup") then okay.Condition.Decision.Invoke("giveup", ())
      else if menu.contains("reask") then okay.Condition.Decision.Invoke("reask", ())
      else okay.Condition.Decision.Fail
    case _ => okay.Condition.Decision.Fail

  /**
   * `ask`, with the retry POLICY lifted out (the condition road): an
   * invalid submit SIGNALS InvalidSubmit — the reask restart repeats
   * (the default forgiving policy makes this exactly `ask`), the
   * giveup restart answers None (a patience policy), and a repairing
   * policy may Resume with a forced value at the live signal point.
   * A valid submit never consults the policy. The condition machine
   * runs PER SUBMIT over a tiny pure program — the dialog loop
   * itself is ask's own.
   */
  def askWith[A](message: String, checks: Check[A]*)
                (policy: (Any, Vector[String]) => okay.Condition.Decision)
                (using s: Schema[A], ct: scala.reflect.ClassTag[A]): Option[A] ! Dialog =
    import okay.Condition
    // the answer type rides the instance; a policy Resume of the
    // wrong type is BadResume at the point where it acts
    given ans: Condition.Answers[InvalidSubmit, A] = Condition.Answers.of[InvalidSubmit, A]
    def verdict(errs: Vector[(String, String)], n: Int): Outcome[A] =
      Condition.run[Outcome[A], Pure](policy)(
        Condition.within[Outcome[A], Pure]("giveup")(
          Condition.within[Outcome[A], Pure]("reask")(
            !.widen[A, Condition.Op, Pure](
              Condition.raiseC(InvalidSubmit(errs, n))(using ans)).map(Outcome.Done(_))
          )(_ => Outcome.Retry()))(_ => Outcome.Gave())).runWith

    def loop(j: Json, errs: Vector[(String, String)], n: Int): Option[A] ! Dialog =
      Dialog.show(asked(message, errs.collect { case ("", m) => m },
        ofWith[A](errs).apply(j))).flatMap {
        case Event.Pressed("$ok") =>
          val fieldErrs = errors[A](j)
          val submit: Either[Vector[(String, String)], A] =
            if fieldErrs.nonEmpty then Left(fieldErrs)
            else decode[A].apply(j) match
              case Left(err) => Left(Vector("" -> err))
              case Right(a) =>
                val crossErrs = checks.toVector.flatMap(_(a))
                if crossErrs.isEmpty then Right(a) else Left(crossErrs)
          submit match
            case Right(a) => pure(Some(a))          // the policy is never consulted
            case Left(errs2) => verdict(errs2, n) match
              case Outcome.Done(forced) => pure(Some(forced))
              case Outcome.Gave() => pure(None)
              case Outcome.Retry() => loop(j, errs2, n + 1)
        case Event.Pressed("$cancel") | Event.Closed => pure(None)
        case e => loop(edit[A](j, e), Vector.empty, n)
      }

    loop(Json.JObj(Vector.empty), Vector.empty, 1)

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
