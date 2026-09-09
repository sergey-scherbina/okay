package okay.ui

import okay.*
import okay.given

/**
 * The view is a VALUE (specs/ui.md): no functions inside, so it has
 * equality (the diff), pure rendering (the tests), and nothing that
 * cannot cross a wire. Widgets carry KEYS; events name keys; the
 * application's update interprets — where Elm puts a message in the
 * attribute and React puts a closure, this puts a name.
 */
enum Ui:
  // ---- level L, the layout vocabulary: small and CLOSED (specs/frontend.md)
  case Text(s: String, style: Style = Style.none)
  case Row(children: Vector[Ui], key: String = "")
  case Column(children: Vector[Ui], key: String = "")
  /** the general container: Row/Column with weights (a child's share
   * of the main axis), a gap between children and padding inside */
  case Box(children: Vector[Ui], dir: Dir, weights: Vector[Int] = Vector.empty,
           gap: Int = 0, pad: Int = 0, key: String = "")
  case Image(src: String, alt: String)
  case Button(label: String, key: String, role: Role = Role.Plain)
  case Input(value: String, key: String, label: String = "",
             kind: InputKind = InputKind.Text, live: Boolean = false)
  case Check(on: Boolean, key: String, label: String = "")
  case Select(options: Vector[String], selected: Int, key: String)
  case Scroll(child: Ui, key: String = "")
  /** fields and a submit button; `submit` is the button's label, the
   * form's key is the button's key. Form is LEVEL L — every client
   * draws it — because the hybrid rule lives on it (ui-hybrid): the
   * fields fold on the client, the button sends ONE
   * `Event.Submitted(key, edits)`; an input marked `live` sends its
   * `Edited` as it happens */
  case Form(fields: Vector[Ui], submit: String, key: String)
  // ---- level S, the semantic vocabulary: OPEN, each node DEFINED by
  // its lowering (`Ui.lower`) — a client that does not claim a node
  // receives the lowering, and cannot tell (`Ui.keys` is the law).
  // A CLAIMED Tabs or Disclosure switches on the client with no line
  // on the wire; lowered, its buttons round-trip as Pressed
  /** a keyed list of items — `List` is Scala's name, so `Items` */
  case Items(items: Vector[Ui], key: String)
  case Table(header: Vector[String], rows: Vector[Vector[Ui]], key: String)
  /** tab i's button is keyed `<key>$tab<i>`; only the selected page
   * is shown, so only its keys are capabilities */
  case Tabs(labels: Vector[String], selected: Int, pages: Vector[Ui], key: String)
  case Modal(title: String, body: Ui, key: String)
  /** a titled subtree shown when `open`; the title is its toggle,
   * keyed by the node's key */
  case Disclosure(title: String, open: Boolean, body: Ui, key: String)

enum Dir:
  case Horizontal, Vertical

/** how a button reads — a host maps a role to its idiom */
enum Role:
  case Plain, Primary, Danger, Active

enum InputKind:
  case Text, Secret, Multiline, Number

/** style is TOKENS, not pixels: a host maps a tone to its own idiom
 * (bold/dim are v1's two, kept as they were) */
enum Tone:
  case Plain, Emphasis, Muted, Danger

enum Size:
  case Small, Normal, Large

final case class Style(bold: Boolean = false, dim: Boolean = false,
                       tone: Tone = Tone.Plain, size: Size = Size.Normal)
object Style:
  val none = Style()

/** what the user did — keys name the widget it happened to */
enum Event:
  case Pressed(key: String)
  case Edited(key: String, value: String)
  case Toggled(key: String, on: Boolean)
  case Chosen(key: String, index: Int)
  case Key(ch: Char)
  case Resized(w: Int, h: Int)
  case Closed
  /** a Form's ONE event: the field edits the client folded locally,
   * sent together when its button was pressed (ui-hybrid). The server
   * folds them through the same `Form.edit` a live edit takes */
  case Submitted(key: String, edits: Vector[Event])

/**
 * The seam, in the form React taught everyone: a HOST is handed the
 * whole tree and draws it however it likes — React reconciles, a
 * terminal repaints, a test host keeps it as a value. Two functions,
 * like Link, and for the same reason: the program above cannot tell
 * hosts apart, which is what makes it portable between them.
 */
trait Host:
  def render(ui: Ui): Unit ! Async
  def events: Source[Event]

/** a patch-consuming target (raw DOM, a native toolkit): the core
 * diff turns it into a Host — diffing is OUR job, not every backend's */
trait Backend:
  def apply(p: Patch): Unit ! Async
  def events: Source[Event]

/** what the diff says: the smallest change at the deepest path */
enum Patch:
  case Replace(path: List[Int], ui: Ui)
  case SetText(path: List[Int], s: String)
  case SetValue(path: List[Int], s: String)
  case SetChecked(path: List[Int], on: Boolean)
  case SetSelected(path: List[Int], index: Int)
  /** children ops, applied IN ORDER: removals (desc), one reorder,
   * insertions (asc) — the sequencing the keyed diff emits */
  case Remove(path: List[Int], index: Int)
  case Reorder(path: List[Int], order: Vector[Int])
  case Insert(path: List[Int], index: Int, ui: Ui)

object Ui {

  /**
   * The difference between two trees, as patches. Positional in v1
   * (keyed reordering can learn later without an API change): equal
   * nodes yield nothing, a changed leaf yields its narrow Set, a
   * changed shape replaces at the highest node that differs.
   */
  /** a child's identity, when it has one — what keyed matching keys on */
  def keyOf(ui: Ui): Option[String] = ui match
    case Row(_, k) if k.nonEmpty => Some(k)
    case Column(_, k) if k.nonEmpty => Some(k)
    case Box(_, _, _, _, _, k) if k.nonEmpty => Some(k)
    case Button(_, k, _) if k.nonEmpty => Some(k)
    case Input(_, k, _, _, _) if k.nonEmpty => Some(k)
    case Check(_, k, _) if k.nonEmpty => Some(k)
    case Select(_, _, k) if k.nonEmpty => Some(k)
    case Scroll(_, k) if k.nonEmpty => Some(k)
    case Form(_, _, k) if k.nonEmpty => Some(k)
    case Items(_, k) if k.nonEmpty => Some(k)
    case Table(_, _, k) if k.nonEmpty => Some(k)
    case Tabs(_, _, _, k) if k.nonEmpty => Some(k)
    case Modal(_, _, k) if k.nonEmpty => Some(k)
    case Disclosure(_, _, _, k) if k.nonEmpty => Some(k)
    case _ => None

  def diff(old: Ui, next: Ui): Vector[Patch] =
    def go(a: Ui, b: Ui, path: List[Int]): Vector[Patch] = (a, b) match
      case (x, y) if x == y => Vector.empty
      case (Text(_, s1), Text(t, s2)) if s1 == s2 =>
        Vector(Patch.SetText(path.reverse, t))
      case (Input(_, k1, l1, kd1, lv1), Input(v, k2, l2, kd2, lv2))
        if k1 == k2 && l1 == l2 && kd1 == kd2 && lv1 == lv2 =>
        Vector(Patch.SetValue(path.reverse, v))
      case (Check(_, k1, l1), Check(on, k2, l2)) if k1 == k2 && l1 == l2 =>
        Vector(Patch.SetChecked(path.reverse, on))
      case (Select(o1, _, k1), Select(o2, i, k2)) if k1 == k2 && o1 == o2 =>
        Vector(Patch.SetSelected(path.reverse, i))
      case (Row(c1, k1), Row(c2, k2)) if k1 == k2 => children(b, c1, c2, path)
      case (Column(c1, k1), Column(c2, k2)) if k1 == k2 => children(b, c1, c2, path)
      case (Box(c1, d1, w1, g1, p1, k1), Box(c2, d2, w2, g2, p2, k2))
        if d1 == d2 && w1 == w2 && g1 == g2 && p1 == p2 && k1 == k2 => children(b, c1, c2, path)
      case (Scroll(x, k1), Scroll(y, k2)) if k1 == k2 => go(x, y, 0 :: path)
      // semantic nodes: their children are at the paths their
      // LOWERING puts them at (the diff commutes with lowering — the
      // law TestVocab asserts), so a client holding either tree walks
      // the same indices
      case (Form(f1, s1, k1), Form(f2, s2, k2)) if s1 == s2 && k1 == k2 => children(b, f1, f2, path)
      case (Items(i1, k1), Items(i2, k2)) if k1 == k2 => children(b, i1, i2, path)
      case (Modal(t1, x, k1), Modal(t2, y, k2)) if t1 == t2 && k1 == k2 => go(x, y, 1 :: path)
      case (Disclosure(t1, o1, x, k1), Disclosure(t2, o2, y, k2)) if t1 == t2 && o1 == o2 && k1 == k2 =>
        go(x, y, 1 :: path)
      case _ => Vector(Patch.Replace(path.reverse, b))

    /**
     * Children: keyed matching when EVERY child on both sides has a
     * distinct key — then a moved child is a move, not a Replace —
     * and the positional walk otherwise. The keyed patches come in
     * the order `patch` applies them: removals (descending), one
     * Reorder of the survivors, insertions (ascending), then content
     * recursion at the settled positions.
     */
    def children(b: Ui, c1: Vector[Ui], c2: Vector[Ui], path: List[Int]): Vector[Patch] =
      val k1 = c1.map(keyOf)
      val k2 = c2.map(keyOf)
      val keyed = k1.forall(_.isDefined) && k2.forall(_.isDefined) &&
        k1.distinct.length == k1.length && k2.distinct.length == k2.length
      if !keyed then
        // the positional walk of v1: same length recurses, different
        // lengths replace the container — unkeyed children have no
        // identity to move by
        if c1.length == c2.length then
          c1.indices.flatMap(i => go(c1(i), c2(i), i :: path)).toVector
        else Vector(Patch.Replace(path.reverse, b))
      else
        val at = path.reverse
        val oldKeys = k1.map(_.get)
        val newKeys = k2.map(_.get)
        val newSet = newKeys.toSet
        // 1. removals, descending, of keys that vanished
        val removals = oldKeys.zipWithIndex.collect {
          case (k, i) if !newSet(k) => i }.sorted(using Ordering[Int].reverse)
          .map(i => Patch.Remove(at, i))
        val survivors = oldKeys.filter(newSet)
        // 2. one reorder of the survivors into the new relative order
        val targetOrder = newKeys.filter(survivors.contains)
        val order = targetOrder.map(k => survivors.indexOf(k))
        val reorder =
          if order == survivors.indices.toVector then Vector.empty
          else Vector(Patch.Reorder(at, order))
        // 3. insertions, ascending, of keys that appeared
        val oldSet = oldKeys.toSet
        val insertions = newKeys.zipWithIndex.collect {
          case (k, i) if !oldSet(k) => Patch.Insert(at, i, c2(i)) }
        // 4. content recursion at the settled positions
        val oldByKey = oldKeys.zip(c1).toMap
        val content = c2.zipWithIndex.flatMap { (child, i) =>
          oldByKey.get(newKeys(i)).toVector.flatMap(o => go(o, child, i :: path))
        }
        removals ++ reorder ++ insertions ++ content

    go(old, next, Nil)

  /** a patch applied to a tree — what a Backend does to its own
   * representation, done to the value: the test that diff-then-apply
   * equals the next tree is what keeps the diff honest */
  def patch(ui: Ui, p: Patch): Ui =
    def at(u: Ui, path: List[Int], f: Ui => Ui): Ui = path match
      case Nil => f(u)
      case i :: rest => u match
        case Row(c, k) => Row(c.updated(i, at(c(i), rest, f)), k)
        case Column(c, k) => Column(c.updated(i, at(c(i), rest, f)), k)
        case b: Box => b.copy(children = b.children.updated(i, at(b.children(i), rest, f)))
        case Scroll(c, k) if i == 0 => Scroll(at(c, rest, f), k)
        case Form(c, s, k) => Form(c.updated(i, at(c(i), rest, f)), s, k)
        case Items(c, k) => Items(c.updated(i, at(c(i), rest, f)), k)
        case Modal(t, c, k) if i == 1 => Modal(t, at(c, rest, f), k)
        case Disclosure(t, o, c, k) if i == 1 => Disclosure(t, o, at(c, rest, f), k)
        case other => other   // a path into a leaf: the diff never makes one
    def kids(u: Ui, f: Vector[Ui] => Vector[Ui]): Ui = u match
      case Row(c, k) => Row(f(c), k)
      case Column(c, k) => Column(f(c), k)
      case b: Box => b.copy(children = f(b.children))
      case Form(c, s, k) => Form(f(c), s, k)
      case Items(c, k) => Items(f(c), k)
      case other => other
    p match
      case Patch.Replace(path, b) => at(ui, path, _ => b)
      case Patch.SetText(path, s) => at(ui, path, { case Text(_, st) => Text(s, st); case u => u })
      case Patch.SetValue(path, v) => at(ui, path, { case i: Input => i.copy(value = v); case u => u })
      case Patch.SetChecked(path, on) => at(ui, path, { case Check(_, k, l) => Check(on, k, l); case u => u })
      case Patch.SetSelected(path, i) => at(ui, path, { case Select(o, _, k) => Select(o, i, k); case u => u })
      case Patch.Remove(path, i) => at(ui, path, kids(_, c => c.patch(i, Nil, 1)))
      case Patch.Reorder(path, order) => at(ui, path, kids(_, c => order.map(c)))
      case Patch.Insert(path, i, b) => at(ui, path, kids(_, c => c.patch(i, Seq(b), 0)))

  /** every interactive widget, in tab order — focus is a position in
   * this list, and it is the HOST's business, not the tree's */
  def focusable(ui: Ui): Vector[Ui] = ui match
    case Row(c, _) => c.flatMap(focusable)
    case Column(c, _) => c.flatMap(focusable)
    case Box(c, _, _, _, _, _) => c.flatMap(focusable)
    case Scroll(c, _) => focusable(c)
    case _: Text | _: Image => Vector.empty
    case _: Button | _: Input | _: Check | _: Select => Vector(ui)
    case Form(fields, submit, k) => fields.flatMap(focusable) :+ Button(submit, k, Role.Primary)
    case semantic => focusable(lower(semantic, Set.empty))

  /** the CAPABILITY LIST: every key an event may name. Structural on
   * purpose — a semantic node lists its own keys, and TestVocab
   * asserts they equal its lowering's (`keys(s) == keys(lower(s))`),
   * which is what lets `update` not know how the client drew it */
  def keys(ui: Ui): Set[String] = ui match
    case Row(c, _) => c.flatMap(keys).toSet
    case Column(c, _) => c.flatMap(keys).toSet
    case Box(c, _, _, _, _, _) => c.flatMap(keys).toSet
    case Scroll(c, _) => keys(c)
    case _: Text | _: Image => Set.empty
    case Button(_, k, _) => Set(k)
    case Input(_, k, _, _, _) => Set(k)
    case Check(_, k, _) => Set(k)
    case Select(_, _, k) => Set(k)
    case Form(fields, _, k) => fields.flatMap(keys).toSet + k
    case Items(items, _) => items.flatMap(keys).toSet
    case Table(_, rows, _) => rows.flatten.flatMap(keys).toSet
    case Tabs(labels, selected, pages, k) =>
      labels.indices.map(i => tabKey(k, i)).toSet ++ pages.lift(selected).map(keys).getOrElse(Set.empty)
    case Modal(_, body, _) => keys(body)
    case Disclosure(_, open, body, k) => (if open then keys(body) else Set.empty) + k

  def tabKey(key: String, i: Int): String = s"$key$$tab$i"

  /** every Form on the tree: its key, and the keys of its fields —
   * what a `Submitted` may name (the hybrid's capability list) */
  def forms(ui: Ui): Map[String, Set[String]] = ui match
    case Row(c, _) => c.flatMap(forms).toMap
    case Column(c, _) => c.flatMap(forms).toMap
    case Box(c, _, _, _, _, _) => c.flatMap(forms).toMap
    case Scroll(c, _) => forms(c)
    case Form(fields, _, k) => fields.flatMap(forms).toMap + (k -> fields.flatMap(keys).toSet)
    case Items(items, _) => items.flatMap(forms).toMap
    case Table(_, rows, _) => rows.flatten.flatMap(forms).toMap
    case Tabs(_, selected, pages, _) => pages.lift(selected).map(forms).getOrElse(Map.empty)
    case Modal(_, body, _) => forms(body)
    case Disclosure(_, open, body, _) => if open then forms(body) else Map.empty
    case _ => Map.empty

  /**
   * The HYBRID rule, on the client (ui-hybrid): an event that stays
   * local answers the tree it changes; None means "send it". Local:
   * an edit to a field of a Form whose input is not `live` (the tree
   * keeps the typed value, so the host re-renders it), a tab switch
   * when the client claims `tabs`, a disclosure toggle when it claims
   * `disclosure`. The server stays the truth: its later SetValue
   * lands on the same tree and wins.
   */
  def foldLocal(tree: Ui, e: Event, vocab: Set[String]): Option[Ui] =
    val fieldOf = forms(tree).flatMap((f, ks) => ks.map(_ -> f))
    def live(k: String): Boolean = focusable(tree).exists { case Input(_, `k`, _, _, l) => l; case _ => false }
    // the optics say what each of these reaches (specs/optics.md stage
    // 2): `key(k)` is every node that key names, and the guard above
    // it is the capability check `forms` already performs
    def at(k: String)(f: Ui => Ui): Ui = key(k).modify(f)(tree)
    e match
      case Event.Edited(k, v) if fieldOf.contains(k) && !live(k) =>
        Some(at(k) { case i: Input => i.copy(value = v); case u => u })
      case Event.Toggled(k, on) if fieldOf.contains(k) =>
        Some(at(k) { case c: Check => c.copy(on = on); case u => u })
      case Event.Chosen(k, i) if fieldOf.contains(k) =>
        Some(at(k) { case s: Select => s.copy(selected = i); case u => u })
      case Event.Pressed(k) if vocab(Vocab.tabs) && tabOf(tree, k).isDefined =>
        val (tk, i) = tabOf(tree, k).get
        Some(at(tk) { case t: Tabs => t.copy(selected = i); case u => u })
      case Event.Pressed(k) if vocab(Vocab.disclosure) && everywhere.toVector(tree).exists {
          case Disclosure(_, _, _, `k`) => true; case _ => false } =>
        Some(at(k) { case d: Disclosure => d.copy(open = !d.open); case u => u })
      case _ => None

  /** the Form's one event, from the values its fields hold now */
  def submit(tree: Ui, formKey: String): Option[Event] =
    // the SHOWN traversal, in pre-order: a form inside a hidden tab or
    // a closed disclosure is not on screen and cannot be submitted,
    // which is the same reading `forms` and `keys` take
    def find(u: Ui): Option[Form] =
      shown.toVector(u).collectFirst { case f: Form if f.key == formKey => f }
    find(tree).map { f =>
      Event.Submitted(formKey, f.fields.flatMap(focusable).collect {
        case Input(v, k, _, _, _) => Event.Edited(k, v)
        case Check(on, k, _) => Event.Toggled(k, on)
        case Select(_, i, k) => Event.Chosen(k, i)
      })
    }

  private def tabOf(tree: Ui, k: String): Option[(String, Int)] =
    everywhere.toVector(tree).collectFirst {
      case t: Tabs if t.labels.indices.exists(i => tabKey(t.key, i) == k) =>
        t.key -> t.labels.indices.find(i => tabKey(t.key, i) == k).get
    }

  // ---------------------------------------------------------------- the tree's optics (specs/optics.md stage 2)

  /**
   * Every node, in pre-order: `f` sees the node, and the children it
   * had are traversed and put back into what `f` answered.
   *
   * TOP-DOWN, AND IT HAS TO BE. `Ui.map` rewrites bottom-up — children
   * first, then `f` on the REBUILT node — and that is not a traversal
   * at all: applying `f` to a rebuilt node means binding the effect
   * (`F[Ui] >>= f`), and a traversal has only an Applicative. So the
   * two agree on every `f` that keeps a node's children (which is
   * every call site in this file), and `TestUiOptic` both asserts that
   * agreement and names an `f` for which they differ.
   */
  def everywhere: Traversal[Ui, Ui, Ui, Ui] =
    Traversal([F[_]] => (F: Applicative[F]) ?=> (f: Ui => F[Ui]) => (u: Ui) => walk(f, u, structural = true))

  /**
   * Every node the user can SEE: the selected tab's page only, an open
   * disclosure's body only — the reading `keys`, `forms` and
   * `focusable` take, and the reason a form in a hidden tab cannot be
   * submitted.
   */
  def shown: Traversal[Ui, Ui, Ui, Ui] =
    Traversal([F[_]] => (F: Applicative[F]) ?=> (f: Ui => F[Ui]) => (u: Ui) => walk(f, u, structural = false))

  /**
   * Every node a key names — a TRAVERSAL, not an affine. On a
   * well-formed tree there is exactly one: `Ui.keys` treats keys as a
   * SET and the capability rule reads them that way. But nothing
   * enforces it — a view is an ordinary function and may build the
   * same key twice — and this is the operation `foldLocal` used to
   * perform with `map`, which rewrote every match. A traversal keeps
   * that behaviour exactly instead of quietly picking the first.
   */
  def key(k: String): Traversal[Ui, Ui, Ui, Ui] =
    Traversal([F[_]] => (F: Applicative[F]) ?=> (f: Ui => F[Ui]) => (u: Ui) =>
      walk((n: Ui) => if keyOf(n).contains(k) then f(n) else F.pure(n), u, structural = true))

  /**
   * The node at an index path — THE PATCH CONVENTION, index for index
   * with `Ui.patch`'s own walk (a Scroll's child at 0, a Modal's and a
   * Disclosure's body at 1), so `path(p).preview` names exactly the
   * node a `Patch` at `p` touches. An affine: a path into a leaf
   * previews nothing, which is that walk's `case other => other`.
   */
  def path(is: List[Int]): Affine[Ui, Ui, Ui, Ui] =
    is.foldLeft(Affine[Ui, Ui, Ui, Ui](Right(_), (_, v) => v))((acc, i) => acc.andThen(child(i)))

  private def child(i: Int): Affine[Ui, Ui, Ui, Ui] =
    Affine(u => childAt(u, i).map(_._1).toRight(u), (u, v) => childAt(u, i).map(_._2(v)).getOrElse(u))

  /** the i-th child by the patch convention, and how to put one back */
  private def childAt(u: Ui, i: Int): Option[(Ui, Ui => Ui)] = u match
    case Row(c, k) if c.isDefinedAt(i) => Some((c(i), v => Row(c.updated(i, v), k)))
    case Column(c, k) if c.isDefinedAt(i) => Some((c(i), v => Column(c.updated(i, v), k)))
    case b: Box if b.children.isDefinedAt(i) => Some((b.children(i), v => b.copy(children = b.children.updated(i, v))))
    case Scroll(c, k) if i == 0 => Some((c, v => Scroll(v, k)))
    case Form(fs, sub, k) if fs.isDefinedAt(i) => Some((fs(i), v => Form(fs.updated(i, v), sub, k)))
    case Items(items, k) if items.isDefinedAt(i) => Some((items(i), v => Items(items.updated(i, v), k)))
    case Modal(t, body, k) if i == 1 => Some((body, v => Modal(t, v, k)))
    case Disclosure(t, o, body, k) if i == 1 => Some((body, v => Disclosure(t, o, v, k)))
    case _ => None

  /** a node's children — all of them, or only those on screen */
  private def kidsOf(u: Ui, structural: Boolean): Vector[Ui] = u match
    case Row(c, _) => c
    case Column(c, _) => c
    case b: Box => b.children
    case Scroll(c, _) => Vector(c)
    case Form(fs, _, _) => fs
    case Items(items, _) => items
    case Table(_, rows, _) => rows.flatten
    case Tabs(_, sel, pages, _) => if structural then pages else pages.lift(sel).toVector
    case Modal(_, body, _) => Vector(body)
    case Disclosure(_, open, body, _) => if structural || open then Vector(body) else Vector.empty
    case _ => Vector.empty

  /** children back into a node, by position; a node whose shape `f`
   * changed keeps what `f` made of it */
  private def withKids(u: Ui, cs: Vector[Ui], structural: Boolean): Ui =
    if cs.length != kidsOf(u, structural).length then u
    else u match
      case Row(_, k) => Row(cs, k)
      case Column(_, k) => Column(cs, k)
      case b: Box => b.copy(children = cs)
      case Scroll(_, k) => Scroll(cs.head, k)
      case Form(_, sub, k) => Form(cs, sub, k)
      case Items(_, k) => Items(cs, k)
      case Table(h, rows, k) => Table(h, regroup(cs, rows.map(_.length)), k)
      case Tabs(l, sel, pages, k) =>
        if structural then Tabs(l, sel, cs, k)
        else Tabs(l, sel, cs.headOption.fold(pages)(c => pages.updated(sel, c)), k)
      case Modal(t, _, k) => Modal(t, cs.head, k)
      case Disclosure(t, o, body, k) => Disclosure(t, o, cs.headOption.getOrElse(body), k)
      case leaf => leaf

  private def walk[F[_]](f: Ui => F[Ui], u: Ui, structural: Boolean)(using F: Applicative[F]): F[Ui] =
    val kids = kidsOf(u, structural)
    val traversed = kids.foldLeft(F.pure(Vector.empty[Ui]))((acc, x) =>
      F.fmap(acc, (out: Vector[Ui]) => (y: Ui) => out :+ y).app(walk(f, x, structural)))
    F.fmap(f(u), (n: Ui) => (cs: Vector[Ui]) => withKids(n, cs, structural)).app(traversed)

  private def regroup(flat: Vector[Ui], lengths: Vector[Int]): Vector[Vector[Ui]] =
    lengths.foldLeft((flat, Vector.empty[Vector[Ui]])) { case ((rest, out), n) =>
      val (row, more) = rest.splitAt(n)
      (more, out :+ row)
    }._2

  /** a bottom-up rewrite of every node */
  def map(ui: Ui, f: Ui => Ui): Ui =
    val u = ui match
      case Row(c, k) => Row(c.map(map(_, f)), k)
      case Column(c, k) => Column(c.map(map(_, f)), k)
      case b: Box => b.copy(children = b.children.map(map(_, f)))
      case Scroll(c, k) => Scroll(map(c, f), k)
      case Form(fields, s, k) => Form(fields.map(map(_, f)), s, k)
      case Items(items, k) => Items(items.map(map(_, f)), k)
      case Table(h, rows, k) => Table(h, rows.map(_.map(map(_, f))), k)
      case Tabs(l, s, pages, k) => Tabs(l, s, pages.map(map(_, f)), k)
      case Modal(t, body, k) => Modal(t, map(body, f), k)
      case Disclosure(t, o, body, k) => Disclosure(t, o, map(body, f), k)
      case leaf => leaf
    f(u)

  /** the names a client claims in its `hello` — a semantic node is
   * sent as itself only to a client that named it */
  object Vocab:
    val items = "items"; val table = "table"
    val tabs = "tabs"; val modal = "modal"; val disclosure = "disclosure"
    val all: Set[String] = Set(items, table, tabs, modal, disclosure)

  /**
   * The LOWERING: every semantic node the vocabulary does not claim,
   * rewritten as level L — what the node MEANS. Total; level L is a
   * fixed point; the result contains only claimed nodes. The keys are
   * preserved exactly (the law), and children keep their indices so
   * patch paths survive.
   */
  def lower(ui: Ui, vocab: Set[String]): Ui =
    def go(u: Ui): Ui = u match
      case Row(c, k) => Row(c.map(go), k)
      case Column(c, k) => Column(c.map(go), k)
      case b: Box => b.copy(children = b.children.map(go))
      case Scroll(c, k) => Scroll(go(c), k)
      case _: Text | _: Image | _: Button | _: Input | _: Check | _: Select => u
      case Form(fields, submit, k) => Form(fields.map(go), submit, k)   // level L since ui-hybrid
      case Items(items, k) =>
        if vocab(Vocab.items) then Items(items.map(go), k)
        else Box(items.map(go), Dir.Vertical, key = k)
      case Table(header, rows, k) =>
        if vocab(Vocab.table) then Table(header, rows.map(_.map(go)), k)
        else
          val head = Box(header.map(h => Text(h, Style(tone = Tone.Emphasis))), Dir.Horizontal,
            weights = Vector.fill(header.length)(1))
          Box(head +: rows.map(r => Box(r.map(go), Dir.Horizontal, weights = Vector.fill(r.length)(1))),
            Dir.Vertical, key = k)
      case Tabs(labels, selected, pages, k) =>
        if vocab(Vocab.tabs) then Tabs(labels, selected, pages.map(go), k)
        else
          val bar = Box(labels.zipWithIndex.map { (l, i) =>
            Button(l, tabKey(k, i), if i == selected then Role.Active else Role.Plain) },
            Dir.Horizontal)
          Box(bar +: pages.lift(selected).map(go).toVector, Dir.Vertical, key = k)
      case Modal(title, body, k) =>
        if vocab(Vocab.modal) then Modal(title, go(body), k)
        else Box(Vector(Text(title, Style(tone = Tone.Emphasis)), go(body)), Dir.Vertical, pad = 1, key = k)
      case Disclosure(title, open, body, k) =>
        if vocab(Vocab.disclosure) then Disclosure(title, open, go(body), k)
        else Box(Button(title, k, if open then Role.Active else Role.Plain) +: (if open then Vector(go(body)) else Vector.empty),
          Dir.Vertical, key = k)
    go(ui)

  /**
   * The loop: pure update, the world merged in as sources. The state
   * is the fold's parameter; a frame is rendered when the view
   * actually changed; `Closed` answers the final state. Subscriptions
   * are not a Cmd type — an application spawns its own programs and
   * feeds a source, and `merge` is the subscription mechanism this
   * library already had.
   */
  def run[S](init: S)(view: S => Ui)(update: (S, Event) => S)
            (host: Host, external: Source[Event] = pure(()))
            (using Scheduler, CanBlock): S ! Async =
    runCmd(init)(view)((s, e) => (update(s, e), Vector.empty))(host, external)

  /** `runCmd`'s closing decision, as one cell (stm-ui-close,
   * specs/stm.md): commands still in flight, events handed to the
   * channel but not yet folded, and whether the upstream has ended —
   * `ready` is the composite condition the loop closes on */
  private[ui] final case class CloseState(pending: Int = 0, unprocessed: Int = 0,
                                          upstreamDone: Boolean = false):
    def ready: Boolean = upstreamDone && pending == 0 && unprocessed == 0

  /**
   * The loop WITH the effect slot (specs/ui.md, "The effect slot"):
   * update also answers COMMANDS — programs whose Event answers
   * re-enter this same fold through the merge, which was always the
   * subscription mechanism. The commands are DATA out of a pure
   * update; the loop is the only thing that runs them. A command
   * encodes its own failure as an event or is dropped on a raw
   * throw — stated, not hidden.
   */
  def runCmd[S](init: S)(view: S => Ui)
               (update: (S, Event) => (S, Vector[Event ! Async]))
               (host: Host, external: Source[Event] = pure(()))
               (using Scheduler, CanBlock): S ! Async =
    // ONE channel carries everything: a feeder drains the merged
    // upstream into it, command answers join it, and it CLOSES when
    // the upstream has ended and no command is still in flight — so
    // the commandless loop keeps v1's exact ending (host ends, loop
    // ends), and a pending command's answer is waited for, not lost.
    val events = Channel[Event]()
    // the close decision, as ONE cell (stm-ui-close, specs/stm.md):
    // `pending` commands in flight, `unprocessed` events handed to
    // the channel but not yet folded by the loop, `upstreamDone` once
    // the merged feed ends. v1 held these as three atomics and read
    // them one at a time in `maybeClose` — a command launched from a
    // buffered event could land in the window between two of those
    // reads and its answer was lost (found as a flaky TestCmd). A
    // single `TRef[CloseState]` makes "mutate, then decide" ONE step
    // through `modify`: nothing can observe a state this cell never
    // held.
    val closeState = TRef(Ui.CloseState())
    // `f` is pure and may run more than once (TRef.modify's contract);
    // the close is the side effect, run only when THIS call is the one
    // that made the state ready — never inside `f` itself
    def bump(f: Ui.CloseState => Ui.CloseState): Unit =
      if closeState.modify(s => { val s2 = f(s); (s2, s2.ready) }) then events.close()
    def offer(e: Event): Unit =
      closeState.modify(s => (s.copy(unprocessed = s.unprocessed + 1), ()))
      events.offer(e): Unit

    def drain(src: Source[Event]): Unit ! Async =
      Writer.uncons[Event, Unit, Async](src).flatMap {
        case Left(_) => pure(())
        case Right((e, more)) => async(offer(e)).flatMap(_ => drain(more))
      }
    Async.spawn(drain(host.events merge external)).onComplete { _ =>
      bump(_.copy(upstreamDone = true))
    }

    def launch(cmds: Vector[Event ! Async]): Unit =
      cmds.foreach { prog =>
        closeState.modify(s => (s.copy(pending = s.pending + 1), ()))
        Async.spawn(prog).onComplete { r =>
          r match
            case Right(ev) => offer(ev)
            case Left(_) => ()   // a command encodes its failure as an event, or forfeits it
          bump(s => s.copy(pending = s.pending - 1))
        }
      }

    def loop(s: S, shown: Ui, rest: Source[Event]): S ! Async =
      Writer.uncons[Event, Unit, Async](rest).flatMap {
        case Left(_) => pure(s)
        case Right((Event.Closed, _)) => pure(s)
        case Right((e, more)) =>
          val (s2, cmds) = update(s, e)
          launch(cmds)
          // the event is folded and its commands are COUNTED (by
          // `launch`, above) before this decrement can see a zero
          bump(s => s.copy(unprocessed = s.unprocessed - 1))
          val u2 = view(s2)
          (if u2 == shown then pure(()) else host.render(u2))
            .flatMap(_ => loop(s2, u2, more))
      }

    val first = view(init)
    host.render(first).flatMap(_ => loop(init, first, Writer.of(events)))

  /** a patch consumer as a Host: the core diff, one kept tree. A
   * Backend is a level-L consumer (raw DOM, a native toolkit), so the
   * tree is LOWERED before the diff — a patch path names a node the
   * backend actually built */
  def diffing(b: Backend, vocab: Set[String] = Set.empty): Host = new Host:
    private var last: Option[Ui] = None
    def events: Source[Event] = b.events
    def render(full: Ui): Unit ! Async =
      val ui = lower(full, vocab)
      val ps = last match
        case None => Vector(Patch.Replace(Nil, ui))
        case Some(old) => diff(old, ui)
      last = Some(ui)
      def send(rest: Vector[Patch]): Unit ! Async = rest match
        case p +: more => b.apply(p).flatMap(_ => send(more))
        case _ => pure(())
      send(ps)
}
