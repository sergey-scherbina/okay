package okay.ui

import okay.{!, +, Pure, Cont, reset, />}
import okay.given
import okay.codec.{Codecs, Json, Schema}
import scala.annotation.tailrec

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

  /** the form of A, with a caller-stated LABEL per field (form-labels):
   * a dotted path (`addr.city`) or, failing that, a bare field name
   * (`city`), overriding the schema's own name in what a reader sees.
   * An overload rather than a defaulted parameter on `of` itself,
   * since `of` has no explicit parameter list today and every
   * existing call site relies on that (specs/form-labels.md) */
  def of[A](labels: Map[String, String])(using s: Schema[A]): Json => Ui =
    j => render(s, j, Vector.empty, "", labels)

  /**
   * THE VALUE A FORM STARTS FROM: every `Check` false, every `Select`
   * on its first option — a sum's case knob included, since that is a
   * `Select` like any other.
   *
   * It exists because a form SHOWS those answers before anybody
   * touches them, and until form-blank the value behind them did not
   * hold them. On the scriptless road `Html.events` sends an event
   * only for a field whose post DIFFERS from what was shown, so a
   * submit that changed nothing said nothing, and the decode refused a
   * field the user could see was filled in. Two copies of this
   * function already existed outside okay-ui — okay-script's
   * `Forms.defaults` (Checks only, so a sum was still refused) and
   * okay-watch's `Analyst.blank` — which is the other half of the
   * reason it belongs here.
   *
   * DERIVED FROM THE TREE, not from the Schema a second time: the
   * blank is the fold of the shown form's own widgets through the same
   * `edit` a user's event takes, so what a form starts from cannot
   * disagree with what a form draws. An `Option` field stays ABSENT —
   * absent is what "not required" means.
   *
   * A LIST IS THE THIRD WIDGET THAT SHOWS AN ANSWER, and it shows
   * "none". The decoder wants the key there (an absent array is a
   * missing field to `Form.errors` and to the codec alike — measured,
   * they agree), and the tree has no event meaning "be empty". It has
   * two that compose into one: the `+` this form draws, and the `-` on
   * the item it just made. That keeps the blank inside the edit
   * vocabulary rather than writing JSON at a path, which is the whole
   * reason it cannot drift.
   */
  def blank[A](using Schema[A]): Json =
    val start = Json.JObj(Vector.empty)
    Ui.focusable(of[A](start)).foldLeft(start: Json) { (j, u) =>
      u match
        case Ui.Select(_, _, k) => edit[A](j, Event.Chosen(k, 0))
        case Ui.Check(_, k, _) => edit[A](j, Event.Toggled(k, false))
        case Ui.Button(_, k, _) if k.endsWith("$add") =>
          val list = k.dropRight(4)
          edit[A](edit[A](j, Event.Pressed(k)), Event.Pressed(s"$list[0]$$del"))
        case _ => j
    }

  /** the form with per-field errors shown under their fields */
  def ofWith[A](errors: Vector[(String, String)])(using s: Schema[A]): Json => Ui =
    j => render(s, j, errors, "")

  /** `ofWith`, with the same caller-stated labels `of` takes */
  def ofWith[A](errors: Vector[(String, String)], labels: Map[String, String])
               (using s: Schema[A]): Json => Ui =
    j => render(s, j, errors, "", labels)

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
                prefix: String, labels: Map[String, String] = Map.empty): Ui =
    Schema.Step.walk(renderer(s), RenderEnv(errors, prefix, "", labels), Some(value))

  private final case class RenderEnv(errors: Vector[(String, String)], key: String, name: String,
                                      labels: Map[String, String] = Map.empty):
    def root: Boolean = name.isEmpty
    def field(n: String): RenderEnv =
      val k = Form.key(key, n)
      RenderEnv(errors, k, labels.getOrElse(k, labels.getOrElse(n, n)), labels)

  private type Render[A] = Schema.Step[RenderEnv, Option[Json], Ui]
  private val renderer = Schema.Folded[Render](RenderAlgebra(drill = false))
  /** the same algebra ONE LEVEL deep (specs/form-drill.md): a composite
   * below the root is an `into` button, not a subform */
  private val drillRenderer = Schema.Folded[Render](RenderAlgebra(drill = true))

  /**
   * One algebra, two modes. `drill` changes exactly three arms — a
   * product, a list and a sum that are NOT the root render as a
   * `Button(name ›, key = s"$key$$into")` and produce no kids — and one
   * more: a root LIST renders its items (a list can be the focus of a
   * drill, where a flat form's root is never a list). Every leaf, the
   * option label, the case Select, `$add`/`$del` and the error
   * placement are shared by construction.
   */
  private final class RenderAlgebra(drill: Boolean) extends Schema.Algebra[Render]:
    import Schema.Step
    /** the one-level stop: the composite's name as the way in */
    private def into(e: RenderEnv): Ui = Ui.Button(s"${e.name} ›", key = s"${e.key}$$into")
    /** a field's errors: its own, and in drill mode those BELOW it —
     * the way in is where a reader looks for what is wrong inside */
    private def errorsFor(errors: Vector[(String, String)], k: String): Vector[Ui] =
      if !drill then errorsUnder(errors, k)
      else errorsUnder(errors, k) ++ errors.collect {
        case (ek, msg) if ek.startsWith(k + ".") || ek.startsWith(k + "[") => Ui.Text(s"! ${ek.drop(k.length + 1)}: $msg", Style(bold = true))
      }
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
    // a TEXT input, not a number one (schema-bigint): what comes back
    // must be the digits as a string, the one form a uint64 survives
    def bigInt = Step.leaf((e, v: Option[Json]) =>
      if e.root then unsupported(e, "SBigInt")
      else Ui.Input(v.collect { case Json.JStr(x) => x; case Json.JNum(n) => Json.print(Json.JNum(n)) }.getOrElse(""),
        key = e.key, label = e.name))

    /** "(optional)" on the label, the same key, the same depth */
    def option[A](o: Schema.SOption[A], of: () => Render[A]) = Step.adapt[RenderEnv, Option[Json], Ui](
      e => if e.root then e else e.copy(name = e.name + " (optional)"),
      identity, of)

    private def items(each: () => Render[?]): Step[RenderEnv, Option[Json], Ui] =
      Step.node[RenderEnv, Option[Json], Ui, Vector[Ui]](
        (_, _) => Vector.empty,
        (e, v) =>
          if drill && !e.root then Vector.empty
          else
            val vs = v match
              case Some(Json.JArr(xs)) => xs
              case _ => Vector.empty
            vs.zipWithIndex.map((iv, i) => Step.Kid(each(), RenderEnv(e.errors, s"${e.key}[$i]", s"${e.name} $i", e.labels), Some(iv))),
        _ :+ _,
        (e, _, uis) =>
          if drill && !e.root then into(e)
          else if e.root && !drill then unsupported(e, "a list")
          else
            val rows = uis.zipWithIndex.flatMap { (ui, i) =>
              val ik = s"${e.key}[$i]"
              Vector(Ui.Row(Vector(ui, Ui.Button("-", key = s"$ik$$del")))) ++ errorsFor(e.errors, ik)
            } :+ Ui.Button("+", key = s"${e.key}$$add")
            if e.root then Ui.Column(rows) else Ui.Column(Ui.Text(e.name, Style(bold = true)) +: rows))
    def list[A](l: Schema.SList[A], of: () => Render[A]) = items(of)
    def vector[A](vs: Schema.SVector[A], of: () => Render[A]) = items(of)

    /** a product: its fields in order, each with its errors under it;
      * titled by its label unless it is the root */
    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Render, Any])]) =
      Step.node[RenderEnv, Option[Json], Ui, Vector[Ui]](
        (_, _) => Vector.empty,
        (e, v) =>
          if drill && !e.root then Vector.empty
          else
            val value = v.getOrElse(Json.JObj(Vector.empty))
            fields.map((n, edge) => Step.Kid(edge(), e.field(n), get(value, n))),
        _ :+ _,
        (e, _, uis) =>
          if drill && !e.root then into(e)
          else
            val children = fields.zip(uis).flatMap((nf, ui) => ui +: errorsFor(e.errors, Form.key(e.key, nf._1)))
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
          if drill && !e.root then Vector.empty
          else
            val head = Ui.Select(names.toVector, chosen(v.getOrElse(Json.JObj(Vector.empty))),
              key = s"${e.key}.$$case".stripPrefix("."))
            if e.root then Vector(head) else Vector(Ui.Text(e.name, Style(bold = true)), head),
        (e, v) =>
          if drill && !e.root then Vector.empty
          else
            val value = v.getOrElse(Json.JObj(Vector.empty))
            val c = chosen(value)
            su.cases(c)._2() match
              case p: Schema.SProduct[?] if p.fields.nonEmpty =>
                Vector(Step.Kid(cases(c)._2(), RenderEnv(e.errors, e.key, "", e.labels), Some(inner(value))))
              case _ => Vector.empty,
        _ :+ _,
        (e, _, all) => if drill && !e.root then into(e) else Ui.Column(all))

    /** a wrapper does not exist to the form */
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Render[B]) =
      Step.via[RenderEnv, Option[Json], Option[Json], Ui](identity, under)
    def ref[A](name: String) =
      throw IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")

  // ---- the drill: one level at a time (specs/form-drill.md) --------

  /**
   * The form of the value AT `path`, one level deep: the focus's
   * scalars as widgets, its composites and items as `<key>$into`
   * buttons, every key prefixed by the path — so an event from this
   * view folds through `edit`/`submitted` exactly as one from the
   * flat form does. A path that is not on the schema, or past a list's
   * end, renders the root: total, like `edit`.
   */
  def renderAt[A](value: Json, path: String, errors: Vector[(String, String)] = Vector.empty,
                  labels: Map[String, String] = Map.empty)(using s: Schema[A]): Ui =
    val segs = if path.isEmpty then Nil else Path.parse(path)
    focusAt(s, Some(value), segs) match
      case Some((sub, v)) => Schema.Step.walk(drillRenderer(sub), RenderEnv(errors, path, "", labels), v)
      case None => Schema.Step.walk(drillRenderer(s), RenderEnv(errors, "", "", labels), Some(value))

  /** the schema and the value at a path — `editAtNative`'s routing,
   * reading instead of writing: sums go into their chosen case
   * without consuming a segment, isos and options are transparent,
   * the case knob is not a place */
  // a tail loop over the path (stack-safety-ui): a dotted path is a string
  // an Event carries, whatever length whoever built it chose — `editAt`
  // trampolines for the same reason, and this read-only twin recursed
  // once per segment inside a `flatMap` until TestUiDepth caught it
  @tailrec
  private def focusAt(s: Schema[?], value: Option[Json], path: List[Seg]): Option[(Schema[?], Option[Json])] =
    s match
      case Schema.SIso(u, _, _) => focusAt(u(), value, path)
      case Schema.SOption(of) => focusAt(of(), value, path)
      case _ => path match
        case Nil => Some((s, value))
        case seg :: rest => (s, seg) match
          case (_: Schema.SSum[?], Seg.Case) => None
          case (su: Schema.SSum[?], _) =>
            val (_, cs) = value match
              case Some(Json.JObj(Vector((n, _)))) => su.cases.find(_._1 == n).getOrElse(su.cases.head)
              case _ => su.cases.head
            val inner = value match
              case Some(Json.JObj(Vector((_, v)))) => v
              case _ => Json.JObj(Vector.empty)
            focusAt(cs(), Some(inner), path)
          case (p: Schema.SProduct[?], Seg.Field(n)) => p.fields.find(_._1 == n) match
            case None => None
            case Some((_, fs)) => focusAt(fs(), value.flatMap(get(_, n)), rest)
          case (p: Schema.SProduct[?], Seg.Index(n, i)) =>
            p.fields.find(_._1 == n).flatMap((_, fs) => itemSchema(fs())) match
              case None => None
              case Some(item) => value.flatMap(get(_, n)) match
                case Some(Json.JArr(vs)) if vs.isDefinedAt(i) => focusAt(item, Some(vs(i)), rest)
                case _ => None
          case _ => None

  /** the drill screen's state: the whole partial value, where the
   * view is, and the errors shown since the last `done` */
  private final case class Drilling(value: Json, path: String, errors: Vector[(String, String)])

  /**
   * The drill-down screen: `into` pushes the path, `out` pops it,
   * every other event folds through `submitted` — the value is the
   * WHOLE partial document, so a move is invisible to it. `done`
   * answers `Some(value)` when `errors` is empty and otherwise shows
   * them under their fields and stays; `cancel` answers `None`.
   */
  def drill[A](value: Json)(done: Option[Json] => Nav)(using s: Schema[A]): Screen =
    drill[A](value, "")(done)

  /** the same, opened AT a path — the dotted key a typed cursor's
   * `pathKey` answers, or one a caller wrote; a key not on the schema
   * shows the root, as `renderAt` does */
  def drill[A](value: Json, at: String)(done: Option[Json] => Nav)(using s: Schema[A]): Screen =
    Nav.screen(Drilling(value, at, Vector.empty))(drillView[A])((d, e) => drillStep[A](d, e, done))

  /** a drill over a typed cursor's ROOT, opened at its focus: the
   * position the code chose, the navigation the user does from there
   * (specs/form-drill.md; specs/zipper.md stage 5). `None` where the
   * cursor took a frame by lens rather than by name — no key exists */
  def drillAt[S, A, Z <: okay.TypedZipper[S, A, Z]](z: okay.TypedZipper[S, A, Z])(done: Option[S] => Nav)
                                                   (using s: Schema[S]): Option[Screen] =
    z.pathKey.map(k => drill[S](encoded(z.root), k)(j => done(j.flatMap(decode[S].apply(_).toOption))))

  /** the same over an existing `A`: in through the codec, out through
   * the decode (the drift law of the second order, TestFormOptic) */
  def drillValue[A](a: A)(done: Option[A] => Nav)(using s: Schema[A]): Screen =
    drill[A](encoded(a))(j => done(j.flatMap(decode[A].apply(_).toOption)))

  private def drillView[A](d: Drilling)(using Schema[A]): Ui =
    // the focus's OWN errors (a missing sub-record is an error at its
    // key, which no field inside it can show) and the form's ("")
    val own = d.errors.collect { case (k, m) if k == d.path => Ui.Text(s"! $m", Style(bold = true)) }
    Ui.Column(Vector(
      Ui.Text(if d.path.isEmpty then "/" else d.path, Style(dim = true))) ++ own ++ Vector(
      renderAt[A](d.value, d.path, d.errors),
      Ui.Row(Vector(Ui.Button("out", "$out"), Ui.Button("done", "$done", Role.Primary), Ui.Button("cancel", "$cancel")))),
      key = "$drill")

  private def drillStep[A](d: Drilling, e: Event, done: Option[Json] => Nav)(using s: Schema[A]): Nav | Drilling =
    e match
      case Event.Pressed(k) if k.endsWith("$into") => d.copy(path = k.dropRight(5))
      case Event.Pressed("$out") =>
        d.copy(path = if d.path.isEmpty then "" else Path.show(Path.parse(d.path).init))
      case Event.Pressed("$done") =>
        val errs = errors[A](d.value)
        if errs.isEmpty then done(Some(d.value)) else d.copy(errors = errs)
      case Event.Pressed("$cancel") | Event.Closed => done(None)
      case other => d.copy(value = submitted[A](d.value, other), errors = Vector.empty)

  private def encoded[A](a: A)(using s: Schema[A]): Json = Json.parse(Json.write(a))

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
    /** the dotted string a list of segments came from */
    def show(segs: List[Seg]): String = segs.map {
      case Seg.Field(n) => n
      case Seg.Index(n, i) => s"$n[$i]"
      case Seg.Case => "$case"
    }.mkString(".")
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

  @tailrec private def isList(s: Schema[?]): Boolean = s match
    case Schema.SIso(u, _, _) => isList(u())
    case Schema.SList(_) | Schema.SVector(_) => true
    case _ => false

  @tailrec private def isComposite(s: Schema[?]): Boolean = s match
    case Schema.SIso(u, _, _) => isComposite(u())
    case _: Schema.SProduct[?] | _: Schema.SSum[?] => true
    case _ => false

  private def itemSchema(s: Schema[?]): Option[Schema[?]] = s match
    case Schema.SIso(u, _, _) => itemSchema(u())
    case Schema.SList(of) => Some(of())
    case Schema.SVector(of) => Some(of())
    case _ => None

  @tailrec private def empty(s: Schema[?]): Json = s match
    case Schema.SIso(u, _, _) => empty(u())
    case Schema.SList(_) | Schema.SVector(_) => Json.JArr(Vector.empty)
    case su: Schema.SSum[?] => Json.JObj(Vector(su.cases.head._1 -> Json.JObj(Vector.empty)))
    case _ => Json.JObj(Vector.empty)

  /** a leaf edit: Set/Flag against the field's own schema; Add on a
   * list appends that item's empty */
  @tailrec private def leaf(s: Schema[?], ev: Edit, old: Option[Json]): Json = (s, ev) match
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

  @tailrec private def leafEmpty(s: Schema[?]): Json = s match
    case Schema.SIso(u, _, _) => leafEmpty(u())
    case Schema.SString => Json.JStr("")
    case Schema.SBool => Json.JBool(false)
    case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBigInt => Json.JStr("")
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
    def bigInt = whole(Schema.SBigInt)
    /**
     * An ABSENT option is fine and says nothing; a PRESENT one is
     * WALKED, not handed whole to the decoder.
     *
     * Handing it whole put the message at the option's own key
     * (`address`), and a form renders an error under the key of a
     * FIELD — `address.city`, `address.zip`. So the one thing the user
     * needed to read rendered nowhere at all. Found by comparing this
     * walk against `Validate`'s on one schema (FormErrorsProbe).
     */
    def option[A](o: Schema.SOption[A], of: () => Validate[A]) =
      Step.node[String, Option[Json], Errors, Errors](
        (_, _) => Vector.empty,
        (k, v) => v match
          case None | Some(Json.JNull) | Some(Json.JStr("")) => Vector.empty
          case Some(inner) => Vector(Step.Kid(of(), k, Some(inner))),
        _ ++ _,
        (_, _, found) => found)
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
        // AN ABSENT FIELD THE SCHEMA DEFAULTS IS NOT AN ERROR. It used
        // to be walked like any other, so its child said "required" —
        // and the form held a submit that `Form.decode` (and the wire's
        // own decoder, which applies the default) would have ACCEPTED.
        // A user cannot see that a field they never touched is the one
        // being refused, which is what made it worth finding.
        (k, v) => v match
          case Some(value) => fields.zipWithIndex.collect {
            case ((n, edge), i) if !(get(value, n).isEmpty && p.defaults.lift(i).flatten.isDefined) =>
              Step.Kid(edge(), Form.key(k, n), get(value, n))
          }
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
    asking(message, checks, blank[A])

  /** `ask`, started from a VALUE rather than from the blank — the seam
   * `TestFormOptic` encoded by hand; the codec is the one encoder */
  def askFrom[A](message: String, initial: A, checks: Check[A]*)(using s: Schema[A]): Option[A] ! Dialog =
    asking(message, checks, encoded(initial))

  /** the form of a typed cursor's focus, seeded from it: the answer is
   * the cursor with the focus replaced, `up` and `root` as before —
   * a program written against the part, parked in the whole
   * (specs/zipper.md, stage 2's consumer; specs/form-drill.md) */
  def askAt[S, A, Z <: okay.TypedZipper[S, A, Z]](z: okay.TypedZipper[S, A, Z], message: String, checks: Check[A]*)
                                                   (using s: Schema[A]): Option[Z] ! Dialog =
    askFrom(message, z.focus, checks*).map(_.map(a => z.set(a)))

  private def asking[A](message: String, checks: Seq[Check[A]], start: Json)(using s: Schema[A]): Option[A] ! Dialog =
    // FROM THE BLANK, not from `{}` (form-ask-blank): a `Select` the
    // user never touches still shows an option, and the value has to
    // hold what the screen says or `ok` submits a form with no answer
    // where the screen shows one. The same door `Live.form` and
    // okay-watch's page had, counted here by writing the guide.
    //
    // One `!.loop` (loop-audit): the state is the draft and its errors,
    // `Right` is the answer, every `Left` is "show again with this".
    !.loop((start, Vector.empty[(String, String)])) { (j, errs) =>
      Dialog.show(asked(message, errs.collect { case ("", m) => m },
        ofWith[A](errs).apply(j))).map {
        case Event.Pressed("$ok") =>
          val fieldErrs = errors[A](j)
          if fieldErrs.nonEmpty then Left((j, fieldErrs))
          else decode[A].apply(j) match
            case Left(err) => Left((j, Vector("" -> err)))
            case Right(a) =>
              val crossErrs = checks.toVector.flatMap(_(a))
              if crossErrs.isEmpty then Right(Some(a)) else Left((j, crossErrs))
        case Event.Pressed("$cancel") | Event.Closed => Right(None)
        case e => Left((edit[A](j, e), Vector.empty))
      }
    }

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

    // the policy road has its own loop, and the same start: the state
    // carries the attempt number the policy is asked with
    !.loop((blank[A], Vector.empty[(String, String)], 1)) { (j, errs, n) =>
      Dialog.show(asked(message, errs.collect { case ("", m) => m },
        ofWith[A](errs).apply(j))).map {
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
            case Right(a) => Right(Some(a))          // the policy is never consulted
            case Left(errs2) => verdict(errs2, n) match
              case Outcome.Done(forced) => Right(Some(forced))
              case Outcome.Gave() => Right(None)
              case Outcome.Retry() => Left((j, errs2, n + 1))
        case Event.Pressed("$cancel") | Event.Closed => Right(None)
        case e => Left((edit[A](j, e), Vector.empty, n))
      }
    }

  /** the same flow over a JSON Schema — what elicitation asks with */
  def askSchema(message: String, schema: Json): Option[Json] ! Dialog =
    !.loop((Json.JObj(Vector.empty): Json, Option.empty[String])) { (j, error) =>
      Dialog.show(asked(message, error.toVector, ofSchema(schema)(j))).map {
        case Event.Pressed("$ok") => Right(Some(j))
        case Event.Pressed("$cancel") | Event.Closed => Right(None)
        case e => Left((editSchema(schema, j, e), None))
      }
    }

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
