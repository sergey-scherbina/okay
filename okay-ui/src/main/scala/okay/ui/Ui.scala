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
    case Button(_, k) if k.nonEmpty => Some(k)
    case Input(_, k, _) if k.nonEmpty => Some(k)
    case Check(_, k, _) if k.nonEmpty => Some(k)
    case Select(_, _, k) if k.nonEmpty => Some(k)
    case _ => None

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
      case (Row(c1, k1), Row(c2, k2)) if k1 == k2 => children(b, c1, c2, path)
      case (Column(c1, k1), Column(c2, k2)) if k1 == k2 => children(b, c1, c2, path)
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
          case (k, i) if !newSet(k) => i }.sorted(Ordering[Int].reverse)
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
        case other => other   // a path into a leaf: the diff never makes one
    def kids(u: Ui, f: Vector[Ui] => Vector[Ui]): Ui = u match
      case Row(c, k) => Row(f(c), k)
      case Column(c, k) => Column(f(c), k)
      case other => other
    p match
      case Patch.Replace(path, b) => at(ui, path, _ => b)
      case Patch.SetText(path, s) => at(ui, path, { case Text(_, st) => Text(s, st); case u => u })
      case Patch.SetValue(path, v) => at(ui, path, { case Input(_, k, l) => Input(v, k, l); case u => u })
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
    runCmd(init)(view)((s, e) => (update(s, e), Vector.empty))(host, external)

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
    val pending = java.util.concurrent.atomic.AtomicInteger(0)
    // events HANDED OVER but not yet folded by the loop: the close
    // decision must count them, or a command launched from a
    // buffered event races the close and its answer is lost (found
    // as a flaky TestCmd: the loop read the last buffered event,
    // the closer saw pending == 0 in the window before launch ran,
    // and the loop met a closed, drained channel before the
    // answers came back)
    val unprocessed = java.util.concurrent.atomic.AtomicInteger(0)
    val upstreamDone = java.util.concurrent.atomic.AtomicBoolean(false)
    def maybeClose(): Unit =
      if upstreamDone.get && pending.get == 0 && unprocessed.get == 0 then
        events.close()
    def offer(e: Event): Unit =
      unprocessed.incrementAndGet()
      events.send(e)

    def drain(src: Source[Event]): Unit ! Async =
      Writer.uncons[Event, Unit, Async](src).flatMap {
        case Left(_) => pure(())
        case Right((e, more)) => async(offer(e)).flatMap(_ => drain(more))
      }
    Async.spawn(drain(host.events merge external)).onComplete { _ =>
      upstreamDone.set(true); maybeClose()
    }

    def launch(cmds: Vector[Event ! Async]): Unit =
      cmds.foreach { prog =>
        pending.incrementAndGet()
        Async.spawn(prog).onComplete { r =>
          r match
            case Right(ev) => offer(ev)
            case Left(_) => ()   // a command encodes its failure as an event, or forfeits it
          pending.decrementAndGet()
          maybeClose()
        }
      }

    def loop(s: S, shown: Ui, rest: Source[Event]): S ! Async =
      Writer.uncons[Event, Unit, Async](rest).flatMap {
        case Left(_) => pure(s)
        case Right((Event.Closed, _)) => pure(s)
        case Right((e, more)) =>
          val (s2, cmds) = update(s, e)
          launch(cmds)
          // the event is folded and its commands are COUNTED before
          // the close decision may see a zero
          unprocessed.decrementAndGet()
          maybeClose()
          val u2 = view(s2)
          (if u2 == shown then pure(()) else host.render(u2))
            .flatMap(_ => loop(s2, u2, more))
      }

    val first = view(init)
    host.render(first).flatMap(_ => loop(init, first, Writer.of(events)))

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
