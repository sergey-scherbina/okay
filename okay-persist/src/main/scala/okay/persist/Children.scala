package okay.persist

import okay.codec.Schema

/**
 * WHAT A CHILD RUN TELLS ITS PARENT (workflow-children, 2026-09-17).
 *
 * `Wf.awaitChild(id)` has existed since the suspended driver landed:
 * the program asks `Sys.Child(id)`, the runtime declines with
 * `Wait.Child(id)`, and the drive ends. What was missing is the half
 * that answers it — somewhere for a finished run to leave its result
 * where a waiting parent will find it. This is that, and it is the
 * same shape as `Signals` because it is the same problem: a fact from
 * outside the parent's journal has to become an answer INSIDE it,
 * exactly once.
 *
 * ── WHY IT DOES NOT SPAWN. A parent does not start its child here,
 * and that is deliberate rather than unfinished. A `Worker` is built
 * for ONE program — one journal topic, one body, one set of types —
 * so a parent's worker has no way to run a different program's code,
 * and giving it one would mean a registry of erased bodies and a cast
 * at every spawn. STARTING A CHILD IS AN ACTIVITY: the parent asks
 * its own question, the oracle calls the child's worker, and the
 * child id comes back as an ordinary answer, journalled like any
 * other. The parent then waits on it. No new machinery, and the spawn
 * is idempotent in `(id, index)` like every other activity.
 *
 * ── DELIVERY. A child completes once and its result does not change,
 * so unlike a signal there is no cursor and no mailbox: the parent's
 * `expect` is the whole of the guard. Two workers that both notice
 * the same finished child both append, and exactly one is accepted.
 *
 * ── WHY THE CHANNEL IS A STRING. Because `SysA.Got` is, and that is
 * the library's question channel, which no consumer's `match` may be
 * made to grow. A child that owes its parent a structure encodes it,
 * the same way an activity's answer does.
 *
 * Lose this whole topic and no journal is wrong: parents that would
 * have woken simply keep waiting, which is the failure mode the rest
 * of the engine also prefers.
 */
final class Children(val snapshots: Snapshots):

  /** record that this run is a child of that one. Optional — the
   * parent's wait does not need it — and kept for the tree view,
   * which is the question an operator actually asks. */
  def link(child: String, parent: String, program: String): Unit =
    val _ = snapshots.putValue(key("l", child), Children.Link(parent, program))

  /** a run finished with this result: the only thing a waiting parent
   * reads */
  def completed(child: String, result: String): Unit =
    val _ = snapshots.putValue(key("d", child), Children.Done(result))

  /** what a finished run left behind, if it has finished */
  def resultOf(child: String): Option[String] =
    snapshots.latestValue[Children.Done](key("d", child))
      .flatMap(_._2.toOption).map(_.result)

  /** who this run's parent is, if it was linked */
  def parentOf(child: String): Option[Children.Link] =
    snapshots.latestValue[Children.Link](key("l", child))
      .flatMap(_._2.toOption)

  /** every linked child of one parent, with its result if it has one.
   * Scans what compaction left, the same honest-for-thousands bound
   * `Timers.armed` carries. */
  def of(parent: String): List[(String, Children.Link, Option[String])] =
    val links = scala.collection.mutable.LinkedHashMap.empty[String, Option[Children.Link]]
    val t = snapshots.topic
    var p = 0
    while p < t.partitions do
      var from = t.begin(p)
      var going = true
      while going do
        t.read(p, from, 512) match
          case Topic.Read.TooEarly(b) => from = b
          case Topic.Read.Records(rs) =>
            if rs.isEmpty then going = false
            else
              rs.foreach: r =>
                val k = new String(r.key, "UTF-8")
                if k.startsWith("l|") then
                  val child = k.substring(2)
                  if r.value.isEmpty then links(child) = None
                  else links(child) = okay.codec.Codecs.readCbor[Children.Link](r.value).toOption
              from = rs.last.offset + 1
      p += 1
    links.collect:
      case (child, Some(l)) if l.parent == parent => (child, l, resultOf(child))
    .toList

  private def key(kind: String, id: String): Array[Byte] =
    s"$kind|$id".getBytes("UTF-8")

object Children:

  /** who started this run, and what program it runs */
  final case class Link(parent: String, program: String)
  given Schema[Link] = Schema.derived

  /** what a finished run left for whoever is waiting */
  final case class Done(result: String)
  given Schema[Done] = Schema.derived

  /** plain CBOR, no `Typed` envelope — a snapshot topic's convention */
  def over(store: Store, name: String = "__children"): Children =
    new Children(Snapshots(store, name))
