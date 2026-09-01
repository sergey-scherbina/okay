package okay.docs.mongo

import com.mongodb.MongoWriteException
import com.mongodb.client.{MongoClient, MongoClients, MongoCollection}
import com.mongodb.client.model.{Filters, FindOneAndUpdateOptions, IndexOptions, Indexes, ReturnDocument, Updates}
import okay.{!, +, Async, Chunk, ChunkBuf, Produce, async, effect}
import okay.codec.{Cbor, Schema}
import okay.docs.{Cond, Consistency, Docs, PutResult}
import org.bson.Document
import org.bson.types.Binary
import scala.jdk.CollectionConverters.*

/**
 * The Mongo adapter of the Docs seam (specs/data.md, FOREIGN
 * posture): their collection, our trait. The document travels as
 * CBOR bytes under `d` (Schema at the edge, the persist layering),
 * with the DECLARED index fields materialized beside it as
 * `ix_<field>` — queries walk real Mongo indexes, never a
 * collection scan wearing a query's hat. The version is a plain
 * counter under `ver`, advanced by `$inc`, and every conditional
 * write is ONE server-side operation (find-and-modify / a filtered
 * delete) — the engine's own CAS, not a read-then-write hope.
 *
 * Multi-document transactions stay out, per the seam's contract:
 * a multi-item change is a journaled sequence of these CAS calls.
 */
final class MongoDocs[A](coll: MongoCollection[Document],
                         indexes: Map[String, A => String])
                        (using Schema[A]) extends Docs[A]:

  /** declared indexes become real ones */
  def ensure(): Unit =
    indexes.keys.foreach { f =>
      coll.createIndex(Indexes.ascending(s"ix_$f"), IndexOptions()); ()
    }

  private def fieldsOf(id: String, a: A): List[org.bson.conversions.Bson] =
    Updates.set("d", Binary(Cbor.write(a))) ::
      indexes.toList.map((f, get) => Updates.set(s"ix_$f", get(a)))

  private def decode(doc: Document): Option[Docs.Versioned[A]] =
    val bytes = doc.get("d", classOf[Binary]).getData
    Cbor.read[A](bytes).toOption.map(a => Docs.Versioned(doc.getLong("ver"), a))

  private def currentVersion(id: String): Option[Long] =
    Option(coll.find(Filters.eq("_id", id)).first()).map(_.getLong("ver").longValue())

  def get(id: String): Option[Docs.Versioned[A]] ! Async = async {
    Option(coll.find(Filters.eq("_id", id)).first()).flatMap(decode)
  }

  def put(id: String, a: A, cond: Cond): PutResult ! Async = async {
    cond match
      case Cond.Always =>
        val after = coll.findOneAndUpdate(
          Filters.eq("_id", id),
          Updates.combine((Updates.inc("ver", 1L) :: fieldsOf(id, a)).asJava),
          FindOneAndUpdateOptions().upsert(true).returnDocument(ReturnDocument.AFTER))
        PutResult.Applied(after.getLong("ver"))
      case Cond.IfAbsent =>
        try
          val doc = Document("_id", id).append("ver", 1L)
          doc.append("d", Binary(Cbor.write(a)))
          indexes.foreach((f, get) => doc.append(s"ix_$f", get(a)))
          coll.insertOne(doc)
          PutResult.Applied(1L)
        catch case _: MongoWriteException => PutResult.Stale(currentVersion(id))
      case Cond.IfVersion(v) =>
        val after = coll.findOneAndUpdate(
          Filters.and(Filters.eq("_id", id), Filters.eq("ver", v)),
          Updates.combine((Updates.inc("ver", 1L) :: fieldsOf(id, a)).asJava),
          FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER))
        if after == null then PutResult.Stale(currentVersion(id))
        else PutResult.Applied(after.getLong("ver"))
  }

  def delete(id: String, cond: Cond): PutResult ! Async = async {
    cond match
      case Cond.Always =>
        coll.deleteOne(Filters.eq("_id", id))
        PutResult.Applied(0L)
      case Cond.IfAbsent =>
        currentVersion(id) match
          case None => PutResult.Applied(0L) // deleting the absent is a no-op that "applied"
          case some => PutResult.Stale(some)
      case Cond.IfVersion(v) =>
        val r = coll.deleteOne(Filters.and(Filters.eq("_id", id), Filters.eq("ver", v)))
        if r.getDeletedCount == 0 then PutResult.Stale(currentVersion(id))
        else PutResult.Applied(0L)
  }

  def query(field: String, equals: String, max: Int)
  : Chunk[(String, A)] ! (Produce + Async) =
    type F = Produce + Async
    if !indexes.contains(field) then
      throw IllegalArgumentException(
        s"field $field is not a declared index — refused (declared: ${indexes.keys.mkString(", ")})")
    effect[F, Chunk[(String, A)]](Async.Run { () =>
      ChunkBuf.of(coll.find(Filters.eq(s"ix_$field", equals)).limit(max)
        .iterator().asScala
        .flatMap(d => decode(d).map(v => (d.getString("_id"), v.value)))
        .toVector.sortBy(_._1))
    }).flatMap(c => effect[F, Chunk[(String, A)]](c))

  /** a standalone node answers Strong for everything it accepts;
   * on a replica set the deployment's read/write concerns decide,
   * and this adapter states rather than upgrades */
  def grants(requested: Consistency): Consistency = requested

object MongoDocs:
  def apply[A](client: MongoClient, db: String, collection: String,
               indexes: Map[String, A => String])(using Schema[A]): MongoDocs[A] =
    val m = new MongoDocs[A](client.getDatabase(db).getCollection(collection), indexes)
    m.ensure()
    m

  def client(uri: String): MongoClient = MongoClients.create(uri)
