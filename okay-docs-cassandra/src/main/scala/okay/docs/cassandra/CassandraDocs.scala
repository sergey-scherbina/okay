package okay.docs.cassandra

import okay.{!, +, Async, Chunk, ChunkBuf, Produce, async, effect}
import okay.codec.{Codecs, Schema}
import okay.docs.{Cond, Consistency, Docs, PutResult}
import com.datastax.oss.driver.api.core.{ConsistencyLevel, CqlSession}
import com.datastax.oss.driver.api.core.cql.{Row, SimpleStatement}
import java.nio.ByteBuffer
import scala.jdk.CollectionConverters.*

/**
 * The Cassandra adapter of the Docs seam (specs/data.md, FOREIGN
 * posture; docs-cassandra): the third engine the seam meets, and the
 * one where consistency is a per-request DIAL rather than a mode —
 * `grants(Quorum)` here means a quorum, not an upgrade.
 *
 * The document travels as CBOR bytes under `d` (Schema at the edge),
 * the version is a bigint under `ver`, and every conditional write
 * is ONE lightweight transaction: `INSERT … IF NOT EXISTS` for
 * IfAbsent, `UPDATE … IF ver = ?` for IfVersion, a `[applied]` row
 * answering with the CURRENT version when it refused — the engine's
 * own compare-and-set (Paxos under the hood), never a read-then-write
 * hope. `Cond.Always` is a bounded read-then-CAS loop: Cassandra has
 * no atomic increment on a regular column, and a plain overwrite
 * would lose the version's monotonicity the seam promises. Declared
 * index fields are materialized as `ix_<field>` with a secondary
 * index each, so `query` walks a real index (equality only — the
 * seam asks no more).
 *
 * Reads use the level `grants` maps: ONE, QUORUM, ALL; the LWTs use
 * the serial level the driver defaults to. Multi-document
 * transactions stay out, per the seam's contract (Cassandra's batches
 * are not them either); a multi-item change is `okay.persist.Saga`
 * over these CAS calls.
 */
final class CassandraDocs[A](session: CqlSession, keyspace: String, table: String,
                             indexes: Map[String, A => String],
                             reads: Consistency = Consistency.Quorum)
                            (using Schema[A]) extends Docs[A]:

  private val t = s"$keyspace.$table"
  private val ixCols = indexes.keys.toVector.sorted.map(f => s"ix_$f")

  private def level(c: Consistency): ConsistencyLevel = c match
    case Consistency.One => ConsistencyLevel.ONE
    case Consistency.Quorum => ConsistencyLevel.QUORUM
    case Consistency.Strong => ConsistencyLevel.ALL

  private def exec(cql: String, args: Any*): com.datastax.oss.driver.api.core.cql.ResultSet =
    session.execute(SimpleStatement.newInstance(cql, args.map(_.asInstanceOf[AnyRef])*)
      .setConsistencyLevel(level(reads)))

  /** the table and its indexes, created when absent — the OWN half of
   * the posture; a foreign table is simply used */
  def ensure(): Unit =
    val cols = ("id text PRIMARY KEY" +: "d blob" +: "ver bigint" +: ixCols.map(c => s"$c text")).mkString(", ")
    exec(s"CREATE TABLE IF NOT EXISTS $t ($cols)"): Unit
    ixCols.foreach(c => exec(s"CREATE INDEX IF NOT EXISTS ${table}_$c ON $t ($c)"): Unit)

  private def bytes(a: A): ByteBuffer = ByteBuffer.wrap(Codecs.writeCbor(a))
  private def ixValues(a: A): Vector[String] = indexes.toVector.sortBy(_._1).map((_, g) => g(a))

  private def decode(row: Row): Option[Docs.Versioned[A]] =
    val bb = row.getByteBuffer("d")
    if bb == null then None
    else
      val bs = new Array[Byte](bb.remaining); bb.duplicate().get(bs)
      Codecs.readCbor[A](bs).toOption.map(a => Docs.Versioned(row.getLong("ver"), a))

  private def currentVersion(id: String): Option[Long] =
    Option(exec(s"SELECT ver FROM $t WHERE id = ?", id).one()).map(_.getLong("ver"))

  def get(id: String): Option[Docs.Versioned[A]] ! Async = async {
    Option(exec(s"SELECT d, ver FROM $t WHERE id = ?", id).one()).flatMap(decode)
  }

  /** INSERT … IF NOT EXISTS at version 1 */
  private def insertIfAbsent(id: String, a: A): Either[Option[Long], Long] =
    val cols = ("id" +: "d" +: "ver" +: ixCols).mkString(", ")
    val marks = Vector.fill(3 + ixCols.length)("?").mkString(", ")
    val row = exec(s"INSERT INTO $t ($cols) VALUES ($marks) IF NOT EXISTS",
      (id +: bytes(a) +: java.lang.Long.valueOf(1L) +: ixValues(a))*).one()
    if row.getBoolean("[applied]") then Right(1L)
    else Left(Option(row.getLong("ver")))

  /** UPDATE … IF ver = expected, to expected + 1 */
  private def updateIfVersion(id: String, a: A, expected: Long): Either[Option[Long], Long] =
    val sets = ("d = ?" +: "ver = ?" +: ixCols.map(c => s"$c = ?")).mkString(", ")
    val row = exec(s"UPDATE $t SET $sets WHERE id = ? IF ver = ?",
      (bytes(a) +: java.lang.Long.valueOf(expected + 1) +: ixValues(a) :+ id :+ java.lang.Long.valueOf(expected))*).one()
    if row.getBoolean("[applied]") then Right(expected + 1)
    // the refusal carries what holds NOW: a row with ver, or nothing (deleted meanwhile)
    else Left(if row.getColumnDefinitions.contains("ver") && !row.isNull("ver") then Some(row.getLong("ver")) else None)

  def put(id: String, a: A, cond: Cond): PutResult ! Async = async {
    cond match
      case Cond.IfAbsent => insertIfAbsent(id, a).fold(PutResult.Stale(_), PutResult.Applied(_))
      case Cond.IfVersion(v) => updateIfVersion(id, a, v).fold(PutResult.Stale(_), PutResult.Applied(_))
      case Cond.Always =>
        // no atomic increment on a regular column: read, then CAS on
        // what was read, bounded — a concurrent writer moves the
        // version and this loop follows it
        var out: Option[PutResult] = None
        var tries = 0
        while out.isEmpty && tries < 8 do
          tries += 1
          val attempt = currentVersion(id) match
            case None => insertIfAbsent(id, a)
            case Some(cur) => updateIfVersion(id, a, cur)
          attempt match
            case Right(v) => out = Some(PutResult.Applied(v))
            case Left(_) => ()
        out.getOrElse(throw IllegalStateException(s"put '$id' on $t: the version moved $tries times in a row"))
  }

  def delete(id: String, cond: Cond): PutResult ! Async = async {
    cond match
      case Cond.Always =>
        exec(s"DELETE FROM $t WHERE id = ?", id): Unit
        PutResult.Applied(0L)
      case Cond.IfAbsent =>
        currentVersion(id) match
          case None => PutResult.Applied(0L)
          case some => PutResult.Stale(some)
      case Cond.IfVersion(v) =>
        val row = exec(s"DELETE FROM $t WHERE id = ? IF ver = ?", id, java.lang.Long.valueOf(v)).one()
        if row.getBoolean("[applied]") then PutResult.Applied(0L)
        else PutResult.Stale(if row.getColumnDefinitions.contains("ver") && !row.isNull("ver") then Some(row.getLong("ver")) else None)
  }

  def query(field: String, equals: String, max: Int): Chunk[(String, A)] ! (Produce + Async) =
    type F = Produce + Async
    if !indexes.contains(field) then
      throw IllegalArgumentException(
        s"field $field is not a declared index — refused (declared: ${indexes.keys.mkString(", ")})")
    effect[F, Chunk[(String, A)]](Async.Run { () =>
      val rows = exec(s"SELECT id, d, ver FROM $t WHERE ix_$field = ? LIMIT ?", equals, java.lang.Integer.valueOf(max))
        .all().asScala.toVector
      ChunkBuf.of(rows.flatMap(r => decode(r).map(v => (r.getString("id"), v.value))).sortBy(_._1))
    }).flatMap(c => effect[F, Chunk[(String, A)]](c))

  /** the dial itself: ONE, QUORUM, ALL — each granted as asked; the
   * deployment's replication factor decides what a quorum is */
  def grants(requested: Consistency): Consistency = requested

object CassandraDocs:
  /** a session on one contact point; the keyspace is created with
   * SimpleStrategy at the given factor when absent */
  def session(host: String, port: Int, datacenter: String, keyspace: String, replication: Int = 1,
              requestTimeoutSeconds: Long = 10L): CqlSession =
    // the driver's 2 s default is too short for DDL on a loaded node
    // (measured: CREATE TABLE + CREATE INDEX per test timed out under
    // four sbts on the box); the timeout is a session setting here
    val config = com.datastax.oss.driver.api.core.config.DriverConfigLoader.programmaticBuilder()
      .withDuration(com.datastax.oss.driver.api.core.config.DefaultDriverOption.REQUEST_TIMEOUT,
        java.time.Duration.ofSeconds(requestTimeoutSeconds))
      .build()
    val s = CqlSession.builder()
      .addContactPoint(java.net.InetSocketAddress(host, port))
      .withLocalDatacenter(datacenter)
      .withConfigLoader(config)
      .build()
    s.execute(s"CREATE KEYSPACE IF NOT EXISTS $keyspace WITH replication = " +
      s"{'class': 'SimpleStrategy', 'replication_factor': $replication}"): Unit
    s
