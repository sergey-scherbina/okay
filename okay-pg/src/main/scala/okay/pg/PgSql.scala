package okay.pg

import okay.{!, +, Async, Chunk, ChunkBuf, Chunks, Produce, async, effect}
import okay.sql.{Col, Granted, Isolation, Sql, SqlType, SqlValue}
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The Postgres v3 wire, natively (specs/sql.md): the direct road
 * the seam exists for — no java.sql, no JDBC driver, the protocol
 * itself behind the same `Sql` trait, so the typed layer runs over
 * it unchanged. Startup + SCRAM-SHA-256 (with the server-signature
 * verification most clients skip), the EXTENDED query protocol
 * whose portals ARE chunked streaming at the protocol level
 * (Execute maxRows + PortalSuspended = our fetch-size story with
 * no driver in between), text-format values both directions v1.
 *
 * JVM leg first: a blocking socket behind Async.Run — the same
 * honesty as the JDBC driver, virtual threads make it real. The
 * cross-platform transport (Node) arrives with a consumer, behind
 * this same class shape.
 *
 * One driver instance wraps ONE connection; use it from one
 * logical thread of control at a time (the JdbcSql contract).
 */
final class PgSql private (sock: Socket, in: DataInputStream, out: DataOutputStream)
  extends Sql:
  import PgSql.*

  private var inTx = false

  // ── the Sql seam ───────────────────────────────────────────────

  def describe(sql: String): Vector[Col] ! Async = async {
    send('P', str("") ++ str(sql) ++ i16(0))
    send('D', Array('S'.toByte) ++ str(""))
    sync()
    var cols = Vector.empty[(String, Int, Int, Int)] // label, oid, tableOid, attnum
    drainUntilReady {
      case ('T', body) => cols = rowDescription(body)
      case _ => ()
    }
    // RowDescription does not carry nullability; the catalog does.
    // One lookup per table column, at verify time — startup cost.
    cols.map { (label, oid, tableOid, attnum) =>
      val nullable =
        if tableOid == 0 then true // an expression: no home, no promise
        else attNotNull(tableOid, attnum).map(!_).getOrElse(true)
      Col(label, typeOf(oid), nullable)
    }
  }

  def query(sql: String, params: Vector[SqlValue])
  : Chunk[Vector[SqlValue]] ! (Produce + Async) =
    type F = Produce + Async

    // one chunk = one Execute against the open portal: rows until
    // PortalSuspended (more to come) or CommandComplete (the end)
    def readChunk(oids: Vector[Int]): (Chunk[Vector[SqlValue]], Boolean) =
      send('E', str("") ++ i32(fetchSize))
      flush()
      val buf = ChunkBuf[Vector[SqlValue]](fetchSize)
      var i = 0
      var more = false
      var going = true
      while going do
        val (tag, body) = receive()
        tag match
          case 'D' => buf(i) = dataRow(body, oids); i += 1
          case 's' => more = true; going = false          // PortalSuspended
          case 'C' => going = false                       // CommandComplete
          case 'E' =>
            // after an error the backend discards until Sync: reach
            // quiet first, so the connection survives the throw
            sync()
            val err = errorOf(body)
            drainUntilReady { case _ => () }
            throw err
          case _ => ()
      (buf.take(i), more)

    def go(oids: Vector[Int]): Chunk[Vector[SqlValue]] ! F =
      effect[F, (Chunk[Vector[SqlValue]], Boolean)](Async.Run(() => readChunk(oids))).flatMap {
        (c, more) =>
          if !more then
            effect[F, Unit](Async.Run(() => finishPortal())).flatMap { _ =>
              if c.isEmpty then okay.pure(Chunks.emptyChunk)
              else effect[F, Chunk[Vector[SqlValue]]](c)
            }
          else effect[F, Chunk[Vector[SqlValue]]](c).flatMap(_ => go(oids))
      }

    effect[F, Vector[Int]](Async.Run { () =>
      send('P', str("") ++ str(sql) ++ i16(0))
      bind(params)
      send('D', Array('P'.toByte) ++ str(""))
      flush()
      var oids = Vector.empty[Int]
      var going = true
      while going do
        val (tag, body) = receive()
        tag match
          case 'T' => oids = rowDescription(body).map(_._2); going = false
          case 'n' => going = false                       // NoData
          case 'E' =>
            sync()
            val err = errorOf(body)
            drainUntilReady { case _ => () }
            throw err
          case _ => ()
      oids
    }).flatMap(go)

  def update(sql: String, params: Vector[SqlValue]): Long ! Async = async {
    send('P', str("") ++ str(sql) ++ i16(0))
    bind(params)
    send('E', str("") ++ i32(0))
    sync()
    var count = 0L
    drainUntilReady {
      case ('C', body) => count = countOf(body)
      case _ => ()
    }
    count
  }

  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = async {
    send('P', str("") ++ str(sql) ++ i16(0))
    rows.foreach { r =>
      bind(r)
      send('E', str("") ++ i32(0))
    }
    sync()
    var count = 0L
    drainUntilReady {
      case ('C', body) => count += countOf(body)
      case _ => ()
    }
    count
  }

  def begin(isolation: Isolation): Granted ! Async = async {
    if inTx then throw IllegalStateException(
      "nested transaction: this connection is already in one — " +
        "refuse rather than silently flatten (specs/jdbc.md)")
    simple("BEGIN")
    simple(s"SET TRANSACTION ISOLATION LEVEL ${levelSql(isolation)}")
    inTx = true
    val granted = simpleValue("SHOW transaction_isolation") match
      case Some("serializable") => Isolation.Serializable
      case Some("repeatable read") => Isolation.RepeatableRead
      case _ => Isolation.ReadCommitted
    Granted(isolation, granted)
  }

  def commit(): Unit ! Async = async { simple("COMMIT"); inTx = false }

  def rollback(): Unit ! Async = async { simple("ROLLBACK"); inTx = false }

  // ── COPY: the bulk-load road (specs/sql.md, specs/data.md) ─────

  /** raw COPY IN over the simple protocol: CopyInResponse, then a
   * CopyData frame per row line, CopyDone, the count from
   * CommandComplete. Rows arrive already TEXT-ENCODED (see
   * `PgSql.copyRow`); the caller owns the statement text — the
   * bind-don't-model rule holds for bulk loads too. */
  def copyIn(sql: String, rows: Iterator[String]): Long ! Async = async {
    send('Q', str(sql))
    out.flush()
    var ok = false
    var going = true
    while going do
      val (tag, body) = receive()
      tag match
        case 'G' => ok = true; going = false          // CopyInResponse
        case 'E' =>
          val err = errorOf(body)
          drainUntilReady { case _ => () }
          throw err
        case 'Z' => going = false                     // refused before starting
        case _ => ()
    if !ok then throw PgError(s"the statement did not start a COPY: $sql")
    for line <- rows do
      send('d', (line + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))
    send('c', Array.empty)
    out.flush()
    var count = 0L
    drainUntilReady {
      case ('C', body) => count = countOf(body)
      case _ => ()
    }
    count
  }

  /** the sync emergency brake: blocking I/O anyway on this leg */
  def cancel(): Unit =
    if inTx then
      simple("ROLLBACK")
      inTx = false

  def close(): Unit =
    try { send('X', Array.empty); flush() } finally sock.close()

  // ── protocol plumbing ──────────────────────────────────────────

  private val fetchSize = 64

  private def send(tag: Char, body: Array[Byte]): Unit =
    out.writeByte(tag)
    out.writeInt(body.length + 4)
    out.write(body)

  private def flush(): Unit = { send('H', Array.empty); out.flush() }
  private def sync(): Unit = { send('S', Array.empty); out.flush() }

  private def receive(): (Char, Array[Byte]) =
    val tag = in.readByte().toChar
    val len = in.readInt() - 4
    val body = new Array[Byte](len)
    in.readFully(body)
    (tag, body)

  /** pump messages to ReadyForQuery; errors THROW after the pump
   * reaches quiet, so the connection stays usable */
  private def drainUntilReady(f: PartialFunction[(Char, Array[Byte]), Unit]): Unit =
    var err: PgError = null
    var going = true
    while going do
      val m = receive()
      m._1 match
        case 'Z' => going = false
        case 'E' => err = errorOf(m._2)
        case _ => if f.isDefinedAt(m) then f(m)
    if err != null then throw err

  private def bind(params: Vector[SqlValue]): Unit =
    val b = Array.newBuilder[Byte]
    b ++= str("") ++= str("")                 // portal, statement
    b ++= i16(0)                              // all params text
    b ++= i16(params.length)
    for p <- params do
      textOf(p) match
        case None => b ++= i32(-1)
        case Some(s) =>
          val bs = s.getBytes(UTF_8)
          b ++= i32(bs.length) ++= bs
    b ++= i16(0)                              // all results text
    send('B', b.result())

  private def finishPortal(): Unit =
    send('C', Array('P'.toByte) ++ str(""))
    sync()
    drainUntilReady { case _ => () }

  private def simple(sql: String): Unit =
    send('Q', str(sql)); out.flush()
    drainUntilReady { case _ => () }

  private def simpleValue(sql: String): Option[String] =
    send('Q', str(sql)); out.flush()
    var v: Option[String] = None
    drainUntilReady {
      case ('D', body) =>
        val n = ((body(0) & 0xff) << 8) | (body(1) & 0xff)
        if n >= 1 then
          val len = readI32(body, 2)
          if len >= 0 then v = Some(new String(body, 6, len, UTF_8))
      case _ => ()
    }
    v

  private def attNotNull(tableOid: Int, attnum: Int): Option[Boolean] =
    simpleValue(
      s"select attnotnull from pg_attribute where attrelid = $tableOid and attnum = $attnum")
      .map(_ == "t")

  private def rowDescription(body: Array[Byte]): Vector[(String, Int, Int, Int)] =
    var at = 0
    def i16r(): Int = { val v = ((body(at) & 0xff) << 8) | (body(at + 1) & 0xff); at += 2; v }
    def i32r(): Int = { val v = readI32(body, at); at += 4; v }
    def cstr(): String =
      val start = at
      while body(at) != 0 do at += 1
      val s = new String(body, start, at - start, UTF_8)
      at += 1
      s
    val n = i16r()
    Vector.fill(n) {
      val label = cstr()
      val tableOid = i32r()
      val attnum = i16r()
      val typeOid = i32r()
      i16r(); i32r(); i16r()                  // typlen, typmod, format
      (label, typeOid, tableOid, attnum)
    }

  private def dataRow(body: Array[Byte], oids: Vector[Int]): Vector[SqlValue] =
    var at = 2                                // column count, already known
    Vector.tabulate(oids.length) { i =>
      val len = readI32(body, at); at += 4
      if len < 0 then SqlValue.Null
      else
        val s = new String(body, at, len, UTF_8)
        at += len
        valueOf(oids(i), s)
    }

object PgSql:

  /** startup + SCRAM-SHA-256; answers a driver ready for the seam */
  def connect(host: String, port: Int, user: String, password: String,
              database: String): PgSql =
    val sock = Socket(host, port)
    sock.setTcpNoDelay(true)
    val in = DataInputStream(BufferedInputStream(sock.getInputStream))
    val out = DataOutputStream(BufferedOutputStream(sock.getOutputStream))

    // StartupMessage has no tag byte
    val params = str("user") ++ str(user) ++ str("database") ++ str(database) ++ Array(0.toByte)
    out.writeInt(params.length + 8)
    out.writeInt(196608)                      // protocol 3.0
    out.write(params)
    out.flush()

    def receive(): (Char, Array[Byte]) =
      val tag = in.readByte().toChar
      val len = in.readInt() - 4
      val body = new Array[Byte](len)
      in.readFully(body)
      (tag, body)

    def send(tag: Char, body: Array[Byte]): Unit =
      out.writeByte(tag); out.writeInt(body.length + 4); out.write(body); out.flush()

    // the SASL handshake as PHASE OBJECTS: the variable holds the
    // phase we are in, and a tag arriving out of order is a NAMED
    // refusal, not an accidental NPE (pg-scram-typestate)
    var sasl: AnyRef = null
    var ready = false
    while !ready do
      val (tag, body) = receive()
      tag match
        case 'R' =>
          readI32(body, 0) match
            case 0 => ()                      // AuthenticationOk
            case 10 =>                        // SASL: pick SCRAM-SHA-256
              val mechs = new String(body, 4, body.length - 4, UTF_8)
              if !mechs.contains("SCRAM-SHA-256") then
                throw PgError(s"server offers no SCRAM-SHA-256 (offered: $mechs)")
              val p0 = Scram.start(user, password)
              sasl = p0
              val first = p0.message
              send('p', str("SCRAM-SHA-256") ++ i32(first.length) ++ first)
            case 11 => sasl match             // SASLContinue
              case p: Scram.ClientFirst =>
                val next = p.serverFirst(body.drop(4))
                sasl = next
                send('p', next.message)
              case _ => throw PgError("SASLContinue out of order")
            case 12 => sasl match             // SASLFinal: verify the server
              case p: Scram.ClientFinal => p.serverFinal(body.drop(4))
              case _ => throw PgError("SASLFinal before SASLContinue")
            case other =>
              throw PgError(s"authentication method $other is not spoken here " +
                "(scram-sha-256 is; md5 and cleartext are deliberately not)")
        case 'E' => throw errorOf(body)
        case 'Z' => ready = true              // ReadyForQuery
        case _ => ()                          // ParameterStatus, BackendKeyData, notices
    new PgSql(sock, in, out)

  // ── shared byte helpers ────────────────────────────────────────

  private[pg] def str(s: String): Array[Byte] = s.getBytes(UTF_8) :+ 0.toByte
  private[pg] def i16(v: Int): Array[Byte] = Array((v >> 8).toByte, v.toByte)
  private[pg] def i32(v: Int): Array[Byte] =
    Array((v >> 24).toByte, (v >> 16).toByte, (v >> 8).toByte, v.toByte)
  private[pg] def readI32(bs: Array[Byte], at: Int): Int =
    ((bs(at) & 0xff) << 24) | ((bs(at + 1) & 0xff) << 16) |
      ((bs(at + 2) & 0xff) << 8) | (bs(at + 3) & 0xff)

  private[pg] def errorOf(body: Array[Byte]): PgError =
    // fields: a tag byte then a c-string, until a lone terminator
    var at = 0
    var msg = "backend error"
    var code = ""
    while at < body.length && body(at) != 0 do
      val tag = body(at).toChar
      at += 1
      val start = at
      while body(at) != 0 do at += 1
      val v = new String(body, start, at - start, UTF_8)
      at += 1
      tag match
        case 'M' => msg = v
        case 'C' => code = v
        case _ => ()
    PgError(if code.isEmpty then msg else s"$msg [$code]")

  /** CommandComplete's tag: "INSERT 0 5", "UPDATE 3", "SELECT 2" —
   * the affected count is the last token when it is a number */
  private[pg] def countOf(body: Array[Byte]): Long =
    val tag = new String(body, UTF_8).takeWhile(_ != '\u0000')
    tag.split(' ').lastOption.flatMap(_.toLongOption).getOrElse(0L)

  private def levelSql(i: Isolation): String = i match
    case Isolation.ReadCommitted => "READ COMMITTED"
    case Isolation.RepeatableRead => "REPEATABLE READ"
    case Isolation.Serializable => "SERIALIZABLE"

  /** type OIDs → the neutral vocabulary; numeric→F64 v1, stated
   * like the JDBC driver states it */
  private def typeOf(oid: Int): SqlType = oid match
    case 16 => SqlType.Bool
    case 21 | 23 => SqlType.I32
    case 20 => SqlType.I64
    case 700 | 701 | 1700 => SqlType.F64
    case 25 | 1043 | 18 | 19 => SqlType.Text
    case 17 => SqlType.Bytes
    case other => SqlType.Other(s"oid:$other")

  private def valueOf(oid: Int, s: String): SqlValue = oid match
    case 16 => SqlValue.Bool(s == "t")
    case 21 | 23 => SqlValue.I32(s.toInt)
    case 20 => SqlValue.I64(s.toLong)
    case 700 | 701 | 1700 => SqlValue.F64(s.toDouble)
    case 17 =>
      // bytea text format: \x followed by hex
      val hex = s.drop(2)
      val out = new Array[Byte](hex.length / 2)
      var i = 0
      while i < out.length do
        out(i) = Integer.parseInt(hex.substring(i * 2, i * 2 + 2), 16).toByte
        i += 1
      SqlValue.Bytes(out)
    case _ => SqlValue.Text(s)

  /** one row in COPY text format: tab-separated, NULL as \\N,
   * the backslash/tab/newline/return escapes the format demands */
  def copyRow(row: Vector[SqlValue]): String =
    row.map {
      case SqlValue.Null => "\\N"
      case v =>
        val s = textOf(v).get
        val sb = new StringBuilder(s.length)
        for c <- s do c match
          case '\\' => sb.append("\\\\")
          case '\t' => sb.append("\\t")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case other => sb.append(other)
        sb.result()
    }.mkString("\t")

  private[pg] def textOf(v: SqlValue): Option[String] = v match
    case SqlValue.Null => None
    case SqlValue.Bool(b) => Some(if b then "t" else "f")
    case SqlValue.I32(x) => Some(x.toString)
    case SqlValue.I64(x) => Some(x.toString)
    case SqlValue.F64(x) => Some(x.toString)
    case SqlValue.Text(s) => Some(s)
    case SqlValue.Bytes(bs) =>
      Some("\\x" + bs.map(b => f"${b & 0xff}%02x").mkString)
