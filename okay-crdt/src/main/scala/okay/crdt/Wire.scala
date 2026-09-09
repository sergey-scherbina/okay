package okay.crdt

import okay.{Hlc, Uid}
import okay.codec.{Json, Schema}

/**
 * A REPLICA AS DATA (specs/coordination-free.md, the item stage 3
 * left open): `Schema` instances, so a CRDT ships through okay-codec
 * — JSON, CBOR, a Sql column, anything the algebra already reaches.
 *
 * THE DECISION THIS FILE TURNS ON: **equal values encode to equal
 * bytes.**
 *
 * A CRDT's state is maps and sets, and their ITERATION ORDER is not
 * part of their value. Two replicas that have converged — that are
 * `==` to each other — would otherwise ship different bytes, and then
 * everything downstream that compares encodings sees a difference
 * that is not there: a cache key that misses, a digest that differs,
 * a dedup check that lets a duplicate through, a test that fails on
 * Tuesday. So every collection here is written SORTED, and that is a
 * decision about the format, not a detail of the implementation.
 *
 * `Schema` has no Map and no Set — it has scalars, `SOption`,
 * `SList`, `SVector`, `SProduct`, `SSum` and `SIso`. That absence is
 * what forced the choice into the open, which is the better outcome:
 * a Map instance would have picked an order silently.
 *
 * WHAT SORTS BY WHAT. `NodeId` and `Uid` have orders of their own, so
 * counters and tag lists sort by those. `GSet` and `OrSet` hold an
 * ARBITRARY `A` with no `Ordering` to reach for, and they sort by the
 * ENCODING of each element — see `Sortable`.
 *
 * The wire shapes are ordinary case classes with `derives Schema`, so
 * the derivation writes the products and nothing here casts.
 */
object Wire:

  /** a node name travels as the string it is */
  given nodeIdSchema: Schema[NodeId] = Schema.SIso[NodeId, String](
    () => Schema.SString,
    s => Right(NodeId(s)),
    n => n.name)()

  /** a 128-bit id travels as its ULID: 26 characters that sort in the
   * same order the value does, so a sorted list of ids reads the same
   * as a sorted list of their text */
  given uidSchema: Schema[Uid] = Schema.SIso[Uid, String](
    () => Schema.SString,
    s => Uid.parseUlid(s).toRight(s"not a ULID: $s"),
    u => u.ulid)()

  /** a stamp is one Long and travels as one */
  given stampSchema: Schema[Hlc.Stamp] = Schema.SIso[Hlc.Stamp, Long](
    () => Schema.SLong,
    l => Right(Hlc.fromLong(l)),
    s => s.toLong)()

  /** one node's count. A named pair rather than a tuple, so the JSON
   * reads as something. */
  final case class Count(node: NodeId, count: Long) derives Schema

  given gcounterSchema: Schema[GCounter] = Schema.SIso[GCounter, List[Count]](
    () => summon[Schema[List[Count]]],
    entries => Right(GCounter(entries.map(e => e.node -> e.count).toMap)),
    g => g.counts.toList.sortBy(_._1.name).map((n, c) => Count(n, c)))()

  final case class UpsDowns(ups: GCounter, downs: GCounter) derives Schema

  given pncounterSchema: Schema[PNCounter] = Schema.SIso[PNCounter, UpsDowns](
    () => summon[Schema[UpsDowns]],
    w => Right(PNCounter(w.ups, w.downs)),
    p => UpsDowns(p.ups, p.downs))()

  given gsetSchema[A](using Schema[A], Sortable[A]): Schema[GSet[A]] =
    Schema.SIso[GSet[A], List[A]](
      () => summon[Schema[List[A]]],
      items => Right(GSet(items.toSet)),
      g => summon[Sortable[A]].sorted(g.items.toList))()

  /** an element and every tag it was added under */
  final case class Tagged[A](value: A, tags: List[Uid]) derives Schema

  final case class OrSetWire[A](adds: List[Tagged[A]], removed: List[Uid]) derives Schema

  given orSetSchema[A](using Schema[A], Sortable[A]): Schema[OrSet[A]] =
    Schema.SIso[OrSet[A], OrSetWire[A]](
      () => summon[Schema[OrSetWire[A]]],
      w => Right(OrSet(w.adds.map(t => t.value -> t.tags.toSet).toMap, w.removed.toSet)),
      o => OrSetWire(
        // BOTH levels sorted: the elements by their own order, and
        // each element's tags by id. One unsorted level is enough to
        // make two equal sets differ on the wire.
        summon[Sortable[A]].sorted(o.adds.keys.toList)
          .map(k => Tagged(k, o.adds(k).toList.sortBy(_.ulid))),
        o.removed.toList.sortBy(_.ulid)))()

  final case class LwwWire[A](value: A, at: Hlc.Stamp, by: NodeId) derives Schema

  given lwwSchema[A](using Schema[A]): Schema[LwwRegister[A]] =
    Schema.SIso[LwwRegister[A], LwwWire[A]](
      () => summon[Schema[LwwWire[A]]],
      w => Right(LwwRegister(w.value, w.at, w.by)),
      r => LwwWire(r.value, r.at, r.by))()

/**
 * How to order elements a CRDT holds when nothing else can.
 *
 * `GSet[A]` and `OrSet[A]` take an arbitrary `A`, so there is no
 * `Ordering[A]` to reach for and demanding one would narrow what the
 * types accept. `hashCode` is the obvious alternative and the wrong
 * one: it differs between JVM, JS and Native, so a replica encoded in
 * a browser would not match the same replica encoded on a server —
 * which is precisely the failure the sorting exists to prevent.
 *
 * So the order comes from the ENCODING: total, already present, and
 * identical on every platform.
 */
trait Sortable[A]:
  def sorted(as: List[A]): List[A]

object Sortable:
  /** order by each element's own JSON, which every platform agrees on */
  given byEncoding[A](using s: Schema[A]): Sortable[A] with
    def sorted(as: List[A]): List[A] =
      as.map(a => Json.write(a)(using s) -> a).sortBy(_._1).map(_._2)
