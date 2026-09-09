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
  // ---- level S, the semantic vocabulary: OPEN, each node DEFINED by
  // its lowering (`Ui.lower`) — a client that does not claim a node
  // receives the lowering, and cannot tell (`Ui.keys` is the law)
  /** fields and a submit button; `submit` is the button's label, the
   * form's key is the button's key (Pressed(key) until ui-hybrid's
   * Submitted) */
  case Form(fields: Vector[Ui], submit: String, key: String)
  /** a keyed list of items — `List` is Scala's name, so `Items` */
  case Items(items: Vector[Ui], key: String)
  case Table(header: Vector[String], rows: Vector[Vector[Ui]], key: String)
  /** tab i's button is keyed `<key>$tab<i>`; only the selected page
   * is shown, so only its keys are capabilities */
  case Tabs(labels: Vector[String], selected: Int, pages: Vector[Ui], key: String)
  case Modal(title: String, body: Ui, key: String)

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

  def tabKey(key: String, i: Int): String = s"$key$$tab$i"

  /** the names a client claims in its `hello` — a semantic node is
   * sent as itself only to a client that named it */
  object Vocab:
    val form = "form"; val items = "items"; val table = "table"
    val tabs = "tabs"; val modal = "modal"
    val all: Set[String] = Set(form, items, table, tabs, modal)

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
      case Form(fields, submit, k) =>
        if vocab(Vocab.form) then Form(fields.map(go), submit, k)
        else Box(fields.map(go) :+ Button(submit, k, Role.Primary), Dir.Vertical, key = k)
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
