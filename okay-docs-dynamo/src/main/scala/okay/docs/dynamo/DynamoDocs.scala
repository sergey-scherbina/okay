package okay.docs.dynamo

import okay.{!, +, Async, Chunk, ChunkBuf, Produce, async, effect}
import okay.given
import okay.codec.{Codecs, Json, Schema}
import okay.docs.{Cond, Consistency, Docs, PutResult}
import okay.blob.SigV4
import okay.http.{Body, Http, Method, Request}
import java.util.Base64
import scala.collection.immutable.ArraySeq

/**
 * The DynamoDB adapter of the Docs seam (specs/data.md, FOREIGN
 * posture; docs-dynamo): their table, our trait, and the second
 * engine the seam meets — the one with condition expressions and
 * eventual reads, which Mongo never exercised.
 *
 * The protocol is DynamoDB's JSON over HTTP (`X-Amz-Target:
 * DynamoDB_20120810.<Op>`), signed by okay-blob's SigV4 with service
 * `dynamodb` — no AWS SDK, the one http client. The document travels
 * as CBOR bytes under `d` (Schema at the edge, the persist layering),
 * the version is a number under `ver` advanced by `ADD ver :one`, and
 * every conditional write is ONE UpdateItem/DeleteItem carrying a
 * condition expression — the engine's own CAS. Declared index fields
 * are materialized as `ix_<field>` and become global secondary
 * indexes `ix_<field>-index`, so `query` walks a real index; GSI
 * reads are eventually consistent by DynamoDB's own rule, stated.
 *
 * `grants`: DynamoDB offers exactly two read modes, so `One` is an
 * eventually consistent read and `Quorum`/`Strong` are
 * `ConsistentRead` — a Quorum request is granted Strong, an upgrade
 * the adapter names rather than hides. `get` reads with what
 * `consistent` says (Strong by default).
 *
 * Multi-document transactions stay out, per the seam's contract:
 * DynamoDB advertises TransactWriteItems, and a multi-item change is
 * `okay.persist.Saga` over these CAS calls.
 */
final class DynamoDocs[A](http: Http, endpoint: String, region: String, creds: SigV4.Creds,
                          table: String, indexes: Map[String, A => String],
                          consistent: Boolean = true,
                          clock: () => java.time.Instant = () => java.time.Instant.now)
                         (using Schema[A]) extends Docs[A]:
  import DynamoDocs.*
  import Json.*

  private val hostHeader =
    val u = java.net.URI(endpoint)
    if u.getPort == -1 then u.getHost else s"${u.getHost}:${u.getPort}"

  private def stamp(): String =
    java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
      .withZone(java.time.ZoneOffset.UTC).format(clock())

  /** one operation: signed, sent, the answer parsed; an error answer
   * is `Left(__type)` — the caller reads the condition failure as data */
  private def call(op: String, body: Json): Either[String, Json] =
    val payload = Json.print(body).getBytes("UTF-8")
    val target = s"DynamoDB_20120810.$op"
    val contentType = "application/x-amz-json-1.0"
    val auth = SigV4.sign("POST", "/", Nil,
      Seq("host" -> hostHeader, "x-amz-target" -> target, "content-type" -> contentType),
      SigV4.sha256Hex(payload), region, stamp(), creds, service = "dynamodb")
    val req = Request(Method.Post, s"$endpoint/",
      Seq("x-amz-target" -> target, "content-type" -> contentType) ++ auth,
      Body.Bytes(ArraySeq.unsafeWrapArray(payload)))
    val (status, text) = !.run(Async.run[(Int, String), Nothing](
      http.send(req).flatMap(r => Http.text(r).map(t => (r.status, t)))))
    val json = Json.parse(text)
    if status == 200 then Right(json)
    else
      val tpe = jsonText(json, "__type").getOrElse(s"HTTP $status")
      Left(tpe.substring(tpe.lastIndexOf('#') + 1))

  private def must(op: String, body: Json): Json =
    call(op, body).fold(e => throw IllegalStateException(s"$op on '$table': $e"), identity)

  /** the table and its index GSIs, created when absent — the OWN half
   * of the posture; a foreign table is simply used */
  def ensure(): Unit =
    call("DescribeTable", obj("TableName" -> JStr(table))) match
      case Right(_) => ()
      case Left("ResourceNotFoundException") =>
        val attrs = ("id" +: indexes.keys.toVector.map(f => s"ix_$f"))
          .map(n => obj("AttributeName" -> JStr(n), "AttributeType" -> JStr("S")))
        val gsis = indexes.keys.toVector.map { f =>
          obj("IndexName" -> JStr(s"ix_$f-index"),
            "KeySchema" -> JArr(Vector(obj("AttributeName" -> JStr(s"ix_$f"), "KeyType" -> JStr("HASH")))),
            "Projection" -> obj("ProjectionType" -> JStr("ALL")))
        }
        val spec = Vector(
          "TableName" -> JStr(table),
          "AttributeDefinitions" -> JArr(attrs),
          "KeySchema" -> JArr(Vector(obj("AttributeName" -> JStr("id"), "KeyType" -> JStr("HASH")))),
          "BillingMode" -> JStr("PAY_PER_REQUEST")) ++
          (if gsis.isEmpty then Nil else Vector("GlobalSecondaryIndexes" -> JArr(gsis)))
        must("CreateTable", JObj(spec)): Unit
      case Left(e) => throw IllegalStateException(s"DescribeTable '$table': $e")

  private def keyOf(id: String): Json = obj("id" -> obj("S" -> JStr(id)))

  private def decode(item: Json): Option[Docs.Versioned[A]] =
    for
      b <- jsonText(jsonField(item, "d"), "B")
      v <- jsonText(jsonField(item, "ver"), "N").flatMap(_.toLongOption)
      a <- Codecs.readCbor[A](Base64.getDecoder.decode(b)).toOption
    yield Docs.Versioned(v, a)

  private def currentVersion(id: String): Option[Long] =
    val r = must("GetItem", obj("TableName" -> JStr(table), "Key" -> keyOf(id), "ConsistentRead" -> JBool(true)))
    jsonText(jsonField(jsonField(r, "Item"), "ver"), "N").flatMap(_.toLongOption)

  def get(id: String): Option[Docs.Versioned[A]] ! Async = async {
    val r = must("GetItem", obj("TableName" -> JStr(table), "Key" -> keyOf(id), "ConsistentRead" -> JBool(consistent)))
    jsonField(r, "Item") match
      case JObj(_) => decode(jsonField(r, "Item"))
      case _ => None
  }

  /** the write: SET the document and the index fields, ADD one to the
   * version, under the condition; ALL_NEW hands the version back */
  private def write(id: String, a: A, condition: Option[(String, Vector[(String, Json)])]): Either[String, Long] =
    val sets = ("d" -> obj("B" -> JStr(Base64.getEncoder.encodeToString(Codecs.writeCbor(a))))) +:
      indexes.toVector.map((f, g) => s"ix_$f" -> obj("S" -> JStr(g(a))))
    val names = sets.zipWithIndex.map((kv, i) => (s"#a$i", kv._1))
    val values = sets.zipWithIndex.map((kv, i) => (s":v$i", kv._2)) :+ (":one" -> obj("N" -> JStr("1")))
    val update = "SET " + names.zipWithIndex.map((n, i) => s"${n._1} = :v$i").mkString(", ") + " ADD ver :one"
    val body = Vector(
      "TableName" -> JStr(table), "Key" -> keyOf(id),
      "UpdateExpression" -> JStr(update),
      "ExpressionAttributeNames" -> JObj(names.map((n, real) => (n, JStr(real)))),
      "ExpressionAttributeValues" -> JObj(values ++ condition.map(_._2).getOrElse(Vector.empty)),
      "ReturnValues" -> JStr("ALL_NEW")) ++ condition.map(c => "ConditionExpression" -> JStr(c._1))
    call("UpdateItem", JObj(body)).map(r => jsonText(jsonField(jsonField(r, "Attributes"), "ver"), "N").flatMap(_.toLongOption).getOrElse(0L))

  def put(id: String, a: A, cond: Cond): PutResult ! Async = async {
    val condition = cond match
      case Cond.Always => None
      case Cond.IfAbsent => Some(("attribute_not_exists(id)", Vector.empty))
      case Cond.IfVersion(v) => Some(("ver = :ver", Vector(":ver" -> obj("N" -> JStr(v.toString)))))
    write(id, a, condition) match
      case Right(v) => PutResult.Applied(v)
      case Left("ConditionalCheckFailedException") => PutResult.Stale(currentVersion(id))
      case Left(e) => throw IllegalStateException(s"UpdateItem on '$table': $e")
  }

  def delete(id: String, cond: Cond): PutResult ! Async = async {
    cond match
      case Cond.Always =>
        must("DeleteItem", obj("TableName" -> JStr(table), "Key" -> keyOf(id))): Unit
        PutResult.Applied(0L)
      case Cond.IfAbsent =>
        currentVersion(id) match
          case None => PutResult.Applied(0L)
          case some => PutResult.Stale(some)
      case Cond.IfVersion(v) =>
        call("DeleteItem", obj("TableName" -> JStr(table), "Key" -> keyOf(id),
          "ConditionExpression" -> JStr("ver = :ver"),
          "ExpressionAttributeValues" -> obj(":ver" -> obj("N" -> JStr(v.toString))))) match
          case Right(_) => PutResult.Applied(0L)
          case Left("ConditionalCheckFailedException") => PutResult.Stale(currentVersion(id))
          case Left(e) => throw IllegalStateException(s"DeleteItem on '$table': $e")
  }

  def query(field: String, equals: String, max: Int): Chunk[(String, A)] ! (Produce + Async) =
    type F = Produce + Async
    if !indexes.contains(field) then
      throw IllegalArgumentException(
        s"field $field is not a declared index — refused (declared: ${indexes.keys.mkString(", ")})")
    effect[F, Chunk[(String, A)]](Async.Run { () =>
      val r = must("Query", obj("TableName" -> JStr(table), "IndexName" -> JStr(s"ix_$field-index"),
        "KeyConditionExpression" -> JStr("#f = :v"),
        "ExpressionAttributeNames" -> obj("#f" -> JStr(s"ix_$field")),
        "ExpressionAttributeValues" -> obj(":v" -> obj("S" -> JStr(equals))),
        "Limit" -> JNum(max.toDouble)))
      val items = jsonField(r, "Items") match
        case JArr(vs) => vs
        case _ => Vector.empty
      ChunkBuf.of(items.flatMap(it => for id <- jsonText(jsonField(it, "id"), "S"); v <- decode(it) yield (id, v.value)).sortBy(_._1))
    }).flatMap(c => effect[F, Chunk[(String, A)]](c))

  /** two read modes exist: eventual (One) and consistent (Strong);
   * a Quorum request is granted Strong — named, not hidden */
  def grants(requested: Consistency): Consistency = requested match
    case Consistency.One => Consistency.One
    case _ => Consistency.Strong

object DynamoDocs:
  import Json.*

  private[dynamo] def obj(fs: (String, Json)*): Json = JObj(fs.toVector)
  private[dynamo] def jsonField(j: Json, k: String): Json = j match
    case JObj(fs) => fs.find(_._1 == k).map(_._2).getOrElse(JNull)
    case _ => JNull
  private[dynamo] def jsonText(j: Json, k: String): Option[String] = jsonField(j, k) match
    case JStr(s) => Some(s)
    case _ => None

  /** the adapter over the platform's http client */
  def apply[A](endpoint: String, region: String, creds: SigV4.Creds, table: String,
               indexes: Map[String, A => String], consistent: Boolean = true)
              (using Schema[A]): DynamoDocs[A] =
    new DynamoDocs[A](okay.http.Transports.http(), endpoint, region, creds, table, indexes, consistent)
