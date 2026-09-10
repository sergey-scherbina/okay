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
   * `render`/`field`/`sumUi`/`listUi` recurse on the VALUE's own
   * depth for a RECURSIVE schema, not a fixed schema shape — the same
   * exposure `okay-codec`'s decode and write sides both had
   * (form-recursive-depth-safety, the same defect shape as
   * remove-codecs-maxdepth/encode-side-depth-safety, one layer up):
   * `Form.of`/`ofWith` take an arbitrary `Json` directly, and nothing
   * bounds how deep a value assembled through many folded edits (a
   * remote `Event.Submitted` batch) can get. Same
   * `Codecs.NativeThreshold`-then-`Cont.defer` split, mirroring
   * `intoC`/`pairsC` (okay-codec's `Json.scala`): each of the four
   * mutually-recursive functions gets a `Native`/`C` pair, and every
   * call from one into another past the threshold is `Cont.defer`red.
   * No mutable/ordered side effect lives in this pipeline (unlike
   * `Cbor.putC`'s `Out` buffer) — it only COMBINES immutable `Ui`
   * values — so building several fields'/items' `Cont` values ahead of
   * sequencing them is safe here; `loop`'s accumulator still runs them
   * one at a time, in order, for the trampoline's own sake.
   */
  def render[A](s: Schema[A], value: Json, errors: Vector[(String, String)],
                prefix: String): Ui = renderAt(s, value, errors, prefix, 0)

  private def renderAt(s: Schema[?], value: Json, errors: Vector[(String, String)],
                       prefix: String, open: Int): Ui =
    if open >= Codecs.NativeThreshold then reset(renderC[Ui](s, value, errors, prefix, open))
    else renderNative(s, value, errors, prefix, open)

  private def renderNative(s: Schema[?], value: Json, errors: Vector[(String, String)],
                           prefix: String, open: Int): Ui = s match
    case p: Schema.SProduct[?] =>
      Ui.Column(p.fields.flatMap { (name, f) =>
        val k = key(prefix, name)
        fieldAt(name, k, f(), get(value, name), errors, open + 1) +: errorsUnder(errors, k)
      }.toVector)
    case su: Schema.SSum[?] => sumUiAt(su, value, errors, prefix, "", open + 1)
    case other => Ui.Text(s"unsupported form: $other")

  /** a sum: the case Select, then the chosen case's subform */
  private def sumUiAt(su: Schema.SSum[?], value: Json, errors: Vector[(String, String)],
                      path: String, label: String, open: Int): Ui =
    if open >= Codecs.NativeThreshold then reset(sumUiC[Ui](su, value, errors, path, label, open))
    else sumUiNative(su, value, errors, path, label, open)

  private def sumUiNative(su: Schema.SSum[?], value: Json, errors: Vector[(String, String)],
                          path: String, label: String, open: Int): Ui =
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
        Vector(renderAt(p, inner, errors, path, open + 1))
      case _ => Vector.empty
    Ui.Column((if label.isEmpty then Vector(head)
               else Vector(Ui.Text(label, Style(bold = true)), head)) ++ body)

  private def fieldAt(name: String, k: String, s: Schema[?], v: Option[Json],
                      errors: Vector[(String, String)], open: Int): Ui =
    if open >= Codecs.NativeThreshold then reset(fieldC[Ui](name, k, s, v, errors, open))
    else fieldNative(name, k, s, v, errors, open)

  private def fieldNative(name: String, k: String, s: Schema[?], v: Option[Json],
                          errors: Vector[(String, String)], open: Int): Ui = s match
    case Schema.SIso(u, _, _) => fieldAt(name, k, u(), v, errors, open)
    case Schema.SOption(of) => fieldAt(name + " (optional)", k, of(), v, errors, open) match
      case i: Ui.Input => i.copy(key = k)
      case Ui.Check(on, _, label) => Ui.Check(on, key = k, label)
      case other => other
    case Schema.SBool => Ui.Check(v.contains(Json.JBool(true)), key = k, label = name)
    case Schema.SInt | Schema.SLong | Schema.SDouble => Ui.Input(v.collect {
      case Json.JNum(n) => Json.print(Json.JNum(n)) }.getOrElse(""), key = k, label = name,
      kind = InputKind.Number)
    case Schema.SString => Ui.Input(v.collect {
      case Json.JStr(x) => x }.getOrElse(""), key = k, label = name)
    case p: Schema.SProduct[?] =>
      // the titled section: the nested fields carry the dotted prefix
      Ui.Column(Ui.Text(name, Style(bold = true)) +: (renderAt(p, v.getOrElse(Json.JObj(Vector.empty)), errors, k, open + 1) match
        case c: Ui.Column => c.children     // a product renders as a column
        case other => Vector(other)))
    case su: Schema.SSum[?] => sumUiAt(su, v.getOrElse(Json.JObj(Vector.empty)), errors, k, name, open + 1)
    case Schema.SList(of) => listUiAt(name, k, of(), v, errors, open + 1)
    case Schema.SVector(of) => listUiAt(name, k, of(), v, errors, open + 1)
    case _ => Ui.Text(s"unsupported field: $name")

  /** items in order, each with its remover, and the adder at the end */
  private def listUiAt(name: String, k: String, item: Schema[?], v: Option[Json],
                       errors: Vector[(String, String)], open: Int): Ui =
    if open >= Codecs.NativeThreshold then reset(listUiC[Ui](name, k, item, v, errors, open))
    else listUiNative(name, k, item, v, errors, open)

  private def listUiNative(name: String, k: String, item: Schema[?], v: Option[Json],
                           errors: Vector[(String, String)], open: Int): Ui =
    val items = v match
      case Some(Json.JArr(vs)) => vs
      case _ => Vector.empty
    Ui.Column(Ui.Text(name, Style(bold = true)) +:
      items.zipWithIndex.flatMap { (iv, i) =>
        val ik = s"$k[$i]"
        Vector(Ui.Row(Vector(
          fieldAt(s"$name $i", ik, item, Some(iv), errors, open + 1),
          Ui.Button("-", key = s"$ik$$del")))) ++ errorsUnder(errors, ik)
      } :+ Ui.Button("+", key = s"$k$$add"))

  // ---- the trampoline: mirrors renderNative/sumUiNative/fieldNative/
  // listUiNative exactly, each recursive call into a sibling function
  // deferred through Cont.defer, forced one at a time inside `/`'s
  // own loop ----

  private def renderC[R](s: Schema[?], value: Json, errors: Vector[(String, String)],
                         prefix: String, open: Int): Ui /> R = s match
    case p: Schema.SProduct[?] =>
      def loop(rest: Vector[(String, () => Schema[?])], acc: Vector[Ui]): Ui /> R =
        if rest.isEmpty then Cont.Pure(Ui.Column(acc))
        else
          val (name, f) = rest.head
          val k = key(prefix, name)
          Cont.defer(() => fieldC[R](name, k, f(), get(value, name), errors, open + 1)) { fui =>
            loop(rest.tail, acc ++ (fui +: errorsUnder(errors, k)))
          }
      loop(p.fields, Vector.empty)
    case su: Schema.SSum[?] =>
      Cont.defer(() => sumUiC[R](su, value, errors, prefix, "", open + 1))(ui => Cont.Pure(ui))
    case other => Cont.Pure(Ui.Text(s"unsupported form: $other"))

  private def sumUiC[R](su: Schema.SSum[?], value: Json, errors: Vector[(String, String)],
                        path: String, label: String, open: Int): Ui /> R =
    val names = su.cases.map(_._1)
    val chosen = value match
      case Json.JObj(Vector((n, _))) => math.max(names.indexOf(n), 0)
      case _ => 0
    val inner = value match
      case Json.JObj(Vector((_, v))) => v
      case _ => Json.JObj(Vector.empty)
    val caseSchema = su.cases(chosen)._2()
    val head = Ui.Select(names.toVector, chosen, key = s"$path.$$case".stripPrefix("."))
    val heading = if label.isEmpty then Vector(head) else Vector(Ui.Text(label, Style(bold = true)), head)
    caseSchema match
      case p: Schema.SProduct[?] if p.fields.nonEmpty =>
        Cont.defer(() => renderC[R](p, inner, errors, path, open + 1))(rendered => Cont.Pure(Ui.Column(heading :+ rendered)))
      case _ => Cont.Pure(Ui.Column(heading))

  private def fieldC[R](name: String, k: String, s: Schema[?], v: Option[Json],
                        errors: Vector[(String, String)], open: Int): Ui /> R = s match
    case Schema.SIso(u, _, _) => Cont.defer(() => fieldC[R](name, k, u(), v, errors, open))(ui => Cont.Pure(ui))
    case Schema.SOption(of) =>
      Cont.defer(() => fieldC[R](name + " (optional)", k, of(), v, errors, open)) { result =>
        Cont.Pure(result match
          case i: Ui.Input => i.copy(key = k)
          case Ui.Check(on, _, label) => Ui.Check(on, key = k, label)
          case other => other)
      }
    case Schema.SBool => Cont.Pure(Ui.Check(v.contains(Json.JBool(true)), key = k, label = name))
    case Schema.SInt | Schema.SLong | Schema.SDouble => Cont.Pure(Ui.Input(v.collect {
      case Json.JNum(n) => Json.print(Json.JNum(n)) }.getOrElse(""), key = k, label = name,
      kind = InputKind.Number))
    case Schema.SString => Cont.Pure(Ui.Input(v.collect { case Json.JStr(x) => x }.getOrElse(""), key = k, label = name))
    case p: Schema.SProduct[?] =>
      Cont.defer(() => renderC[R](p, v.getOrElse(Json.JObj(Vector.empty)), errors, k, open + 1)) { rendered =>
        Cont.Pure(Ui.Column(Ui.Text(name, Style(bold = true)) +: (rendered match
          case c: Ui.Column => c.children
          case other => Vector(other))))
      }
    case su: Schema.SSum[?] =>
      Cont.defer(() => sumUiC[R](su, v.getOrElse(Json.JObj(Vector.empty)), errors, k, name, open + 1))(ui => Cont.Pure(ui))
    case Schema.SList(of) => Cont.defer(() => listUiC[R](name, k, of(), v, errors, open + 1))(ui => Cont.Pure(ui))
    case Schema.SVector(of) => Cont.defer(() => listUiC[R](name, k, of(), v, errors, open + 1))(ui => Cont.Pure(ui))
    case _ => Cont.Pure(Ui.Text(s"unsupported field: $name"))

  private def listUiC[R](name: String, k: String, item: Schema[?], v: Option[Json],
                         errors: Vector[(String, String)], open: Int): Ui /> R =
    val items = v match
      case Some(Json.JArr(vs)) => vs
      case _ => Vector.empty
    def loop(rest: Vector[(Json, Int)], acc: Vector[Ui]): Ui /> R =
      if rest.isEmpty then Cont.Pure(Ui.Column(Ui.Text(name, Style(bold = true)) +: (acc :+ Ui.Button("+", key = s"$k$$add"))))
      else
        val (iv, i) = rest.head
        val ik = s"$k[$i]"
        Cont.defer(() => fieldC[R](s"$name $i", ik, item, Some(iv), errors, open + 1)) { fui =>
          loop(rest.tail, acc ++ (Vector(Ui.Row(Vector(fui, Ui.Button("-", key = s"$ik$$del")))) ++ errorsUnder(errors, ik)))
        }
    loop(items.zipWithIndex, Vector.empty)

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

  /** same shape, same fix as `render`'s pipeline above — recurses on
    * the VALUE's own depth for a recursive schema, validating a
    * submitted form */
  private def errorsOf(s: Schema[?], value: Json, prefix: String): Vector[(String, String)] =
    errorsOfAt(s, value, prefix, 0)

  private def errorsOfAt(s: Schema[?], value: Json, prefix: String, open: Int): Vector[(String, String)] =
    if open >= Codecs.NativeThreshold then reset(errorsOfC[Vector[(String, String)]](s, value, prefix, open))
    else errorsOfNative(s, value, prefix, open)

  private def errorsOfNative(s: Schema[?], value: Json, prefix: String, open: Int): Vector[(String, String)] =
    s match
      case Schema.SIso(u, _, _) => errorsOfAt(u(), value, prefix, open)
      case p: Schema.SProduct[?] =>
        p.fields.toVector.flatMap { (name, f) =>
          val k = key(prefix, name)
          val fs = f()
          get(value, name) match
            case None => fieldError(fs, None, k)
            case Some(v) => fs match
              case np: Schema.SProduct[?] => errorsOfAt(np, v, k, open + 1)
              case su: Schema.SSum[?] => errorsOfAt(su, v, k, open + 1)
              case Schema.SList(of) => listErrorsAt(of(), v, k, open + 1)
              case Schema.SVector(of) => listErrorsAt(of(), v, k, open + 1)
              case other => fieldError(other, Some(v), k)
        }
      case su: Schema.SSum[?] => value match
        case Json.JObj(Vector((n, v))) =>
          su.cases.find(_._1 == n)
            .map((_, cs) => errorsOfAt(cs(), v, prefix, open + 1))
            .getOrElse(Vector(prefix -> s"unknown case '$n'"))
        case _ => Vector(key(prefix, "$case") -> "choose one")
      case other => fieldError(other, Some(value), prefix)

  private def listErrorsAt(item: Schema[?], v: Json, k: String, open: Int): Vector[(String, String)] =
    if open >= Codecs.NativeThreshold then reset(listErrorsC[Vector[(String, String)]](item, v, k, open))
    else listErrorsNative(item, v, k, open)

  private def listErrorsNative(item: Schema[?], v: Json, k: String, open: Int): Vector[(String, String)] =
    v match
      case Json.JArr(vs) => vs.zipWithIndex.flatMap { (iv, i) =>
        item match
          case p: Schema.SProduct[?] => errorsOfAt(p, iv, s"$k[$i]", open + 1)
          case su: Schema.SSum[?] => errorsOfAt(su, iv, s"$k[$i]", open + 1)
          case other => fieldError(other, Some(iv), s"$k[$i]")
      }
      case _ => Vector.empty

  // ---- the trampoline: mirrors errorsOfNative/listErrorsNative
  // exactly ----

  private def errorsOfC[R](s: Schema[?], value: Json, prefix: String, open: Int): Vector[(String, String)] /> R =
    s match
      case Schema.SIso(u, _, _) => Cont.defer(() => errorsOfC[R](u(), value, prefix, open))(v => Cont.Pure(v))
      case p: Schema.SProduct[?] =>
        def loop(rest: Vector[(String, () => Schema[?])], acc: Vector[(String, String)]): Vector[(String, String)] /> R =
          if rest.isEmpty then Cont.Pure(acc)
          else
            val (name, f) = rest.head
            val k = key(prefix, name)
            val fs = f()
            get(value, name) match
              case None => loop(rest.tail, acc ++ fieldError(fs, None, k))
              case Some(v) => fs match
                case np: Schema.SProduct[?] =>
                  Cont.defer(() => errorsOfC[R](np, v, k, open + 1))(es => loop(rest.tail, acc ++ es))
                case su: Schema.SSum[?] =>
                  Cont.defer(() => errorsOfC[R](su, v, k, open + 1))(es => loop(rest.tail, acc ++ es))
                case Schema.SList(of) =>
                  Cont.defer(() => listErrorsC[R](of(), v, k, open + 1))(es => loop(rest.tail, acc ++ es))
                case Schema.SVector(of) =>
                  Cont.defer(() => listErrorsC[R](of(), v, k, open + 1))(es => loop(rest.tail, acc ++ es))
                case other => loop(rest.tail, acc ++ fieldError(other, Some(v), k))
        loop(p.fields.toVector, Vector.empty)
      case su: Schema.SSum[?] => value match
        case Json.JObj(Vector((n, v))) =>
          val found = su.cases.find(_._1 == n)
          if found.isEmpty then Cont.Pure(Vector(prefix -> s"unknown case '$n'"))
          else Cont.defer(() => errorsOfC[R](found.get._2(), v, prefix, open + 1))(es => Cont.Pure(es))
        case _ => Cont.Pure(Vector(key(prefix, "$case") -> "choose one"))
      case other => Cont.Pure(fieldError(other, Some(value), prefix))

  private def listErrorsC[R](item: Schema[?], v: Json, k: String, open: Int): Vector[(String, String)] /> R =
    v match
      case Json.JArr(vs) =>
        def loop(rest: Vector[(Json, Int)], acc: Vector[(String, String)]): Vector[(String, String)] /> R =
          if rest.isEmpty then Cont.Pure(acc)
          else
            val (iv, i) = rest.head
            item match
              case p: Schema.SProduct[?] =>
                Cont.defer(() => errorsOfC[R](p, iv, s"$k[$i]", open + 1))(es => loop(rest.tail, acc ++ es))
              case su: Schema.SSum[?] =>
                Cont.defer(() => errorsOfC[R](su, iv, s"$k[$i]", open + 1))(es => loop(rest.tail, acc ++ es))
              case other => loop(rest.tail, acc ++ fieldError(other, Some(iv), s"$k[$i]"))
        loop(vs.zipWithIndex, Vector.empty)
      case _ => Cont.Pure(Vector.empty)

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
