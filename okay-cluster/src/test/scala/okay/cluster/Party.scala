package okay.cluster

import okay.Chunks
import okay.codec.Schema
import okay.persist.{Ack, MemoryStore, Policy, Streams}
import scala.collection.concurrent.TrieMap
import scala.util.DynamicVariable

/**
 * A PARTY (specs/federation.md, stage 1).
 *
 * A process that OWNS one log and computes only over it. Which party
 * this process is comes from `-Dokay.party=N` at start — not from
 * the job's parameters, which every worker receives identically, and
 * not from the coordinator, which is the point: the coordinator says
 * which partition it wants and the party decides whether that is
 * its own. Partition `i` of a federated job IS party `i`'s log.
 *
 * A party asked for a partition it does not hold REFUSES — a
 * `Refused`, answered as `Resp.Failed`, which the coordinator does
 * not carry to another worker. The refusal comes BEFORE any read:
 * a store that answered "no records" for a foreign partition would
 * let a misplaced partition compute an empty share in silence, and
 * that silence is exactly what federation must not have.
 *
 * `current` is a dynamic variable so that one JVM can be several
 * parties for a test without sockets (`Party.as`): the same code
 * runs whether the identity came from the command line or from the
 * `Serve` that wrapped the request.
 */
object Party {
  import Feeds.*

  /** which party this process was started as; `-1` is "no party",
   * and no party holds any partition */
  private val configured: Int =
    Option(System.getProperty("okay.party")).map(_.toInt).getOrElse(-1)
  val current: DynamicVariable[Int] = DynamicVariable(configured)

  /** an in-process worker that IS party `n` for the length of each
   * request — several of these in one JVM are several parties */
  def as(n: Int): Cluster.Serve = req => current.withValue(n)(Cluster.local(req))

  Jobs.register(PartyJob)
  def install(): Unit = ()

  /** this party's logs, one per (feed, cut) — a one-partition topic
   * each, because there is nothing else in a party's store */
  private val logs = TrieMap.empty[(Int, Feed, Int), SeekStore.Counting]

  /** the log party `me` holds for `feed` cut `parts` ways: the
   * `me`-th contiguous slice, which is the cut `Flow.slices` makes,
   * so the union of the parties' logs is the whole feed in order */
  def log(me: Int, feed: Feed, parts: Int): SeekStore.Counting =
    logs.getOrElseUpdate((me, feed, parts), {
      val t = SeekStore.Counting(MemoryStore().topic(s"party-$me", 1, Policy(compact = false)))
      val evs = events(feed)
      val n = evs.length
      val from = (n.toLong * me / parts).toInt
      val until = (n.toLong * (me + 1) / parts).toInt
      for i <- from until until do
        t.append(0, Array.emptyByteArray, SeekStore.codec.encode(evs(i)), Ack.Durable): Unit
      t
    })

  /** the bytes party `me`'s log holds — what a record costs, to set
   * against what crossed */
  def held(me: Int, feed: Feed, parts: Int): Long =
    val evs = events(feed)
    val n = evs.length
    val from = (n.toLong * me / parts).toInt
    val until = (n.toLong * (me + 1) / parts).toInt
    (from until until).map(i => SeekStore.codec.encode(evs(i)).length.toLong).sum

  /** partition `p` of `parts`, as this party sees it: its own log if
   * `p` is its own, a refusal otherwise */
  def partition(feed: Feed, parts: Int, p: Int): Chunks[Ev] =
    val me = current.value
    if me < 0 then throw Cluster.Refused(s"partition $p: this process is no party and holds no log")
    if p != me then throw Cluster.Refused(
      s"partition $p belongs to party $p; this is party $me, and a party does not compute another's share")
    Chunks.map(Streams.chunks(log(me, feed, parts), 0, 0L))(r =>
      SeekStore.codec.decode(r.value).fold(why => throw IllegalStateException(why), identity))
}

/** the tumbling job of `WindowJob`, over the parties' logs — panes
 * cross, records do not */
object PartyJob extends Job[Feed, Feeds.Sum] {
  import Feeds.*
  type A = Ev
  def name: String = "test.party"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] =
    Flow.of(Vector.tabulate(parts)(p => () => Party.partition(f, parts, p)))
  def sink(f: Feed): Wire[Ev, Sum] =
    Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
}
