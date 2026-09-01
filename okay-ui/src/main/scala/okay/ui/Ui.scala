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
  case Text(s: String, style: Style = Style.none)
  case Row(children: Vector[Ui], key: String = "")
  case Column(children: Vector[Ui], key: String = "")
  case Button(label: String, key: String)
  case Input(value: String, key: String, label: String = "")
  case Check(on: Boolean, key: String, label: String = "")
  case Select(options: Vector[String], selected: Int, key: String)

final case class Style(bold: Boolean = false, dim: Boolean = false)
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

object Ui {

  /**
   * The difference between two trees, as patches. Positional in v1
   * (keyed reordering can learn later without an API change): equal
   * nodes yield nothing, a changed leaf yields its narrow Set, a
   * changed shape replaces at the highest node that differs.
   */
  def diff(old: Ui, next: Ui): Vector[Patch] =
    def go(a: Ui, b: Ui, path: List[Int]): Vector[Patch] = (a, b) match
      case (x, y) if x == y => Vector.empty
      case (Text(_, s1), Text(t, s2)) if s1 == s2 =>
        Vector(Patch.SetText(path.reverse, t))
      case (Input(_, k1, l1), Input(v, k2, l2)) if k1 == k2 && l1 == l2 =>
        Vector(Patch.SetValue(path.reverse, v))
      case (Check(_, k1, l1), Check(on, k2, l2)) if k1 == k2 && l1 == l2 =>
        Vector(Patch.SetChecked(path.reverse, on))
      case (Select(o1, _, k1), Select(o2, i, k2)) if k1 == k2 && o1 == o2 =>
        Vector(Patch.SetSelected(path.reverse, i))
      case (Row(c1, k1), Row(c2, k2)) if k1 == k2 && c1.length == c2.length =>
        c1.indices.flatMap(i => go(c1(i), c2(i), i :: path)).toVector
      case (Column(c1, k1), Column(c2, k2)) if k1 == k2 && c1.length == c2.length =>
        c1.indices.flatMap(i => go(c1(i), c2(i), i :: path)).toVector
      case _ => Vector(Patch.Replace(path.reverse, b))

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
        case other => other   // a path into a leaf: the diff never makes one
    p match
      case Patch.Replace(path, b) => at(ui, path, _ => b)
      case Patch.SetText(path, s) => at(ui, path, { case Text(_, st) => Text(s, st); case u => u })
      case Patch.SetValue(path, v) => at(ui, path, { case Input(_, k, l) => Input(v, k, l); case u => u })
      case Patch.SetChecked(path, on) => at(ui, path, { case Check(_, k, l) => Check(on, k, l); case u => u })
      case Patch.SetSelected(path, i) => at(ui, path, { case Select(o, _, k) => Select(o, i, k); case u => u })

  /** every interactive widget, in tab order — focus is a position in
   * this list, and it is the HOST's business, not the tree's */
  def focusable(ui: Ui): Vector[Ui] = ui match
    case Row(c, _) => c.flatMap(focusable)
    case Column(c, _) => c.flatMap(focusable)
    case _: Text => Vector.empty
    case leaf => Vector(leaf)

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
    val events: Source[Event] = host.events merge external

    def loop(s: S, shown: Ui, rest: Source[Event]): S ! Async =
      Writer.uncons[Event, Unit, Async](rest).flatMap {
        case Left(_) => pure(s)
        case Right((Event.Closed, _)) => pure(s)
        case Right((e, more)) =>
          val s2 = update(s, e)
          val u2 = view(s2)
          (if u2 == shown then pure(()) else host.render(u2))
            .flatMap(_ => loop(s2, u2, more))
      }

    val first = view(init)
    host.render(first).flatMap(_ => loop(init, first, events))

  /** a patch consumer as a Host: the core diff, one kept tree */
  def diffing(b: Backend): Host = new Host:
    private var last: Option[Ui] = None
    def events: Source[Event] = b.events
    def render(ui: Ui): Unit ! Async =
      val ps = last match
        case None => Vector(Patch.Replace(Nil, ui))
        case Some(old) => diff(old, ui)
      last = Some(ui)
      def send(rest: Vector[Patch]): Unit ! Async = rest match
        case p +: more => b.apply(p).flatMap(_ => send(more))
        case _ => pure(())
      send(ps)
}
