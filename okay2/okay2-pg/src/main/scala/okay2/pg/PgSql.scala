package okay2.pg

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.tailrec
import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.crypto.Crypto
import okay2.platform.{Net, NetConn}
import okay2.sql.{Col, Granted, Isolation, Sql, SqlType, SqlValue, Temporal}
import okay2.stream.{Chunk, ChunkBuf, Source}

/**
 * The Postgres v3 wire, natively (okay-pg's PgSql.scala, specs/sql.md):
 * no java.sql, no JDBC driver, the protocol itself behind the same `Sql`
 * trait. The message pump PULLS bytes through the Net seam as a
 * sequential Async program, so the same driver runs over a blocking
 * socket on the JVM and over Node's `net` on Scala.js. Startup +
 * SCRAM-SHA-256 (server signature VERIFIED), the extended query protocol
 * with portals as the chunk mechanism, text format both directions,
 * errors drained to quiet so the session survives.
 *
 * `cancel` — the region's sync brake — MARKS the rollback and the next
 * operation on this connection performs it first. One driver instance =
 * one logical thread of control, the driver contract.
 */
final class PgSql private (conn: NetConn) extends Sql {
  import PgSql._

  private var inTx = false
  @volatile private var pendingRollback = false

  // named-composite type OID -> its field OIDs, in attribute order;
  // preloaded ONCE at connect from the catalog
  private val composites = scala.collection.mutable.HashMap.empty[Int, Vector[Int]]
  // an array type OID whose ELEMENT is a named composite -> that OID
  private val arrayElemDyn = scala.collection.mutable.HashMap.empty[Int, Int]

  /** the column type as verify speaks it: arrays and named composites
   * resolve through the same caches `decodeCell` reads */
  private def colType(oid: Int): SqlType =
    composites.get(oid) match {
      case Some(fieldOids) => SqlType.Row(fieldOids.map(colType))
      case None =>
        arrayElem.get(oid).orElse(arrayElemDyn.get(oid)) match {
          case Some(el) => SqlType.Arr(colType(el))
          case None => typeOf(oid)
        }
    }

  // ── the Sql seam ───────────────────────────────────────────────

  def describe(sql: String): Vector[Col] ! Async =
    settled {
      conn.write(concat(
        msg('P', str("") ++ str(sql) ++ i16(0)),
        msg('D', Array('S'.toByte) ++ str("")),
        msg('S', Array.empty[Byte]))).flatMap { _ =>
        collectReady(Vector.empty[(String, Int, Int, Int)]) {
          case (('T', body), _) => rowDescription(body)
          case (_, acc) => acc
        }.flatMap { cols =>
          // RowDescription has no nullability; the catalog does. A column
          // read inside flatMap continues from there, a call that cannot
          // be a jump; `again` takes it, so the walk stays a loop
          def again(rest: List[(String, Int, Int, Int)], acc: Vector[Col]): Vector[Col] ! Async = resolve(rest, acc)
          @tailrec def resolve(rest: List[(String, Int, Int, Int)], acc: Vector[Col]): Vector[Col] ! Async =
            rest match {
              case Nil => pure[Async, Vector[Col]](acc)
              case (label, oid, tableOid, attnum) :: more =>
                if (tableOid == 0) resolve(more, acc :+ Col(label, colType(oid), true))
                else
                  simpleValue(s"select attnotnull from pg_attribute where attrelid = $tableOid and attnum = $attnum")
                    .flatMap(v => again(more, acc :+ Col(label, colType(oid), !v.contains("t"))))
            }
          resolve(cols.toList, Vector.empty)
        }
      }
    }

  def query(sql: String, params: Vector[SqlValue]): Source[Chunk[Vector[SqlValue]]] = {
    type W = Chunk[Vector[SqlValue]]

    def openPortal: Vector[Int] ! Async =
      conn.write(concat(
        msg('P', str("") ++ str(sql) ++ i16(0)),
        bindMsg(params),
        msg('D', Array('P'.toByte) ++ str("")),
        msg('H', Array.empty[Byte]))).flatMap { _ =>
        // each message is pulled inside flatMap: trampolined
        def await: Vector[Int] ! Async = receive().flatMap {
          case ('T', body) => pure[Async, Vector[Int]](rowDescription(body).map(_._2))
          case ('n', _) => pure[Async, Vector[Int]](Vector.empty)
          case ('E', body) => failToQuiet[Vector[Int]](body)
          case _ => await
        }
        await
      }

    def readChunk(oids: Vector[Int]): (W, Boolean) ! Async =
      conn.write(concat(msg('E', str("") ++ i32(fetchSize)), msg('H', Array.empty[Byte]))).flatMap { _ =>
        def go(buf: Vector[Vector[SqlValue]]): (W, Boolean) ! Async =
          receive().flatMap {
            case ('D', body) => go(buf :+ dataRow(body, oids))
            case ('s', _) => pure[Async, (W, Boolean)]((ChunkBuf.of(buf), true))
            case ('C', _) => pure[Async, (W, Boolean)]((ChunkBuf.of(buf), false))
            case ('E', body) => failToQuiet[(W, Boolean)](body)
            case _ => go(buf)
          }
        go(Vector.empty)
      }

    def emit(oids: Vector[Int]): Source[W] =
      readChunk(oids).flatMap[Writer[W] with Async, Unit] { case (c, more) =>
        if (!more)
          finishPortal.flatMap[Writer[W] with Async, Unit] { _ =>
            if (c.isEmpty) pure[Writer[W] with Async, Unit](()) else Writer.tell[W](c)
          }
        else Writer.tell[W](c).flatMap(_ => emit(oids))
      }

    settled(pure[Async, Unit](())).flatMap(_ => openPortal).flatMap[Writer[W] with Async, Unit](emit)
  }

  def update(sql: String, params: Vector[SqlValue]): Long ! Async =
    settled {
      conn.write(concat(
        msg('P', str("") ++ str(sql) ++ i16(0)),
        bindMsg(params),
        msg('E', str("") ++ i32(0)),
        msg('S', Array.empty[Byte]))).flatMap { _ =>
        collectReady(0L) {
          case (('C', body), _) => countOf(body)
          case (_, acc) => acc
        }
      }
    }

  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async =
    settled {
      val msgs = Vector(msg('P', str("") ++ str(sql) ++ i16(0))) ++
        rows.toVector.flatMap(r => Vector(bindMsg(r), msg('E', str("") ++ i32(0)))) :+
        msg('S', Array.empty[Byte])
      conn.write(concat(msgs: _*)).flatMap { _ =>
        collectReady(0L) {
          case (('C', body), acc) => acc + countOf(body)
          case (_, acc) => acc
        }
      }
    }

  def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async =
    settled {
      if (inTx) throw new IllegalStateException(
        "nested transaction: this connection is already in one — " +
          "refuse rather than silently flatten (specs/jdbc.md)")
      val mode = if (readOnly) " READ ONLY" else ""
      simple("BEGIN").flatMap { _ =>
        simple(s"SET TRANSACTION ISOLATION LEVEL ${levelSql(isolation)}$mode").flatMap { _ =>
          inTx = true
          simpleValue("SHOW transaction_isolation").flatMap { v =>
            val granted: Isolation = v match {
              case Some("serializable") => Isolation.Serializable
              case Some("repeatable read") => Isolation.RepeatableRead
              case _ => Isolation.ReadCommitted
            }
            // read back, not assumed: pg enforces READ ONLY
            simpleValue("SHOW transaction_read_only").map(ro => Granted(isolation, granted, ro.contains("on")))
          }
        }
      }
    }

  /** COMMIT reads its COMMAND TAG: a transaction an earlier HANDLED error
   * aborted answers `ROLLBACK` with no ErrorResponse at all — the tag is
   * the only place the server says so (sql-commit-tag) */
  def commit(): Unit ! Async = settled(simpleTag("COMMIT").map { tag =>
    inTx = false
    if (tag.startsWith("ROLLBACK")) throw PgError(
      "COMMIT answered ROLLBACK: an earlier error aborted this transaction " +
        "and the server rolled it back — nothing in the region is committed")
  })

  def rollback(): Unit ! Async = settled(simple("ROLLBACK").map { _ => inTx = false })

  /** the server's SQLSTATE, carried on the ErrorResponse */
  override def sqlState(t: Throwable): Option[String] = t match {
    case PgError(_, code) if code.nonEmpty => Some(code)
    case _ => None
  }

  /** the sync brake: mark now, roll back before the next use */
  def cancel(): Unit =
    if (inTx) {
      pendingRollback = true
      inTx = false
    }

  def close(): Unit = conn.close()

  // ── COPY: the bulk-load road ───────────────────────────────────

  /** raw COPY IN over the simple protocol: rows arrive already
   * text-encoded (`PgSql.copyRow`) */
  def copyIn(sql: String, rows: Iterator[String]): Long ! Async =
    settled {
      conn.write(msg('Q', str(sql))).flatMap { _ =>
        def awaitCopy: Unit ! Async = receive().flatMap {
          case ('G', _) => pure[Async, Unit](())
          case ('E', body) => failToQuiet[Unit](body)
          case ('Z', _) => throw PgError(s"the statement did not start a COPY: $sql")
          case _ => awaitCopy
        }
        awaitCopy.flatMap { _ =>
          val payload = rows.map(l => msg('d', (l + "\n").getBytes(UTF_8))).toVector
          conn.write(concat((payload :+ msg('c', Array.empty[Byte])): _*)).flatMap { _ =>
            collectReady(0L) {
              case (('C', body), _) => countOf(body)
              case (_, acc) => acc
            }
          }
        }
      }
    }

  // ── the pump: sequential pulls over the Net seam ───────────────

  private val fetchSize = 64

  private def receive(): (Char, Array[Byte]) ! Async =
    conn.readFully(5).flatMap { h =>
      val tag = (h(0) & 0xff).toChar
      val len = ((h(1) & 0xff) << 24) | ((h(2) & 0xff) << 16) | ((h(3) & 0xff) << 8) | (h(4) & 0xff)
      if (len < 4 || len > 512 * 1024 * 1024) throw PgError(s"message length $len is not a message")
      conn.readFully(len - 4).map(body => (tag, body))
    }

  /** pump to ReadyForQuery, folding what the caller cares about; an
   * ErrorResponse is remembered and THROWN after quiet, so the session
   * survives. Each message is pulled inside flatMap: trampolined */
  private def collectReady[S](init: S)(f: ((Char, Array[Byte]), S) => S): S ! Async = {
    def go(acc: S, err: Option[PgError]): S ! Async =
      receive().flatMap {
        case ('Z', _) => err.fold(pure[Async, S](acc))(e => throw e)
        case ('E', body) => go(acc, err.orElse(Some(errorOf(body))))
        case m => go(f(m, acc), err)
      }
    go(init, None)
  }

  /** an error mid-conversation: reach quiet first, then throw */
  private def failToQuiet[A](body: Array[Byte]): A ! Async =
    conn.write(msg('S', Array.empty[Byte])).flatMap { _ =>
      val err = errorOf(body)
      collectReady(())((_, _) => ()).map[A](_ => throw err)
    }

  private def finishPortal: Unit ! Async =
    conn.write(concat(msg('C', Array('P'.toByte) ++ str("")), msg('S', Array.empty[Byte])))
      .flatMap(_ => collectReady(())((_, _) => ()))

  private def simple(sql: String): Unit ! Async =
    conn.write(msg('Q', str(sql))).flatMap(_ => collectReady(())((_, _) => ()))

  /** a simple-protocol statement whose command tag is the answer */
  private def simpleTag(sql: String): String ! Async =
    conn.write(msg('Q', str(sql))).flatMap { _ =>
      collectReady("") {
        case (('C', body), _) => new String(body, UTF_8).takeWhile(_ != '\u0000')
        case (_, acc) => acc
      }
    }

  private def simpleValue(sql: String): Option[String] ! Async =
    conn.write(msg('Q', str(sql))).flatMap { _ =>
      collectReady(Option.empty[String]) {
        case (('D', body), _) =>
          val n = ((body(0) & 0xff) << 8) | (body(1) & 0xff)
          if (n >= 1) {
            val len = readI32(body, 2)
            if (len >= 0) Some(new String(body, 6, len, UTF_8)) else None
          } else None
        case (_, acc) => acc
      }
    }

  /** the pending-rollback settle: cancel's mark performed before any next
   * use of this connection */
  private def settled[A](prog: => A ! Async): A ! Async =
    if (!pendingRollback) pure[Async, Unit](()).flatMap(_ => prog)
    else {
      pendingRollback = false
      simple("ROLLBACK").flatMap(_ => prog)
    }

  private def bindMsg(params: Vector[SqlValue]): Array[Byte] = {
    val b = Array.newBuilder[Byte]
    b ++= str("") ++= str("")
    b ++= i16(0)
    b ++= i16(params.length)
    for (p <- params) textOf(p) match {
      case None => b ++= i32(-1)
      case Some(s) =>
        val bs = s.getBytes(UTF_8)
        b ++= i32(bs.length) ++= bs
    }
    b ++= i16(0)
    msg('B', b.result())
  }

  private def rowDescription(body: Array[Byte]): Vector[(String, Int, Int, Int)] = {
    var at = 0
    def i16r(): Int = { val v = ((body(at) & 0xff) << 8) | (body(at + 1) & 0xff); at += 2; v }
    def i32r(): Int = { val v = readI32(body, at); at += 4; v }
    def cstr(): String = {
      val start = at
      while (body(at) != 0) at += 1
      val s = new String(body, start, at - start, UTF_8)
      at += 1
      s
    }
    val n = i16r()
    Vector.fill(n) {
      val label = cstr()
      val tableOid = i32r()
      val attnum = i16r()
      val typeOid = i32r()
      i16r(): Unit; i32r(): Unit; i16r(): Unit
      (label, typeOid, tableOid, attnum)
    }
  }

  private def dataRow(body: Array[Byte], oids: Vector[Int]): Vector[SqlValue] = {
    var at = 2
    Vector.tabulate(oids.length) { i =>
      val len = readI32(body, at); at += 4
      if (len < 0) SqlValue.Null
      else {
        val s = new String(body, at, len, UTF_8)
        at += len
        decodeCell(oids(i), s)
      }
    }
  }

  /** the connection-aware cell decode: a named composite types from the
   * preloaded field cache; an array decodes with this SAME function as the
   * element decoder; everything else falls to the static valueOf */
  private def decodeCell(oid: Int, s: String): SqlValue =
    composites.get(oid) match {
      case Some(fieldOids) => parseCompositeTyped(s, fieldOids, decodeCell)
      case None =>
        arrayElem.get(oid).orElse(arrayElemDyn.get(oid)) match {
          case Some(el) => parseArray(s, r => decodeCell(el, r))
          case None => valueOf(oid, s)
        }
    }

  /** which relations lend their row type to the preload: named composites
   * AND tables/views/matviews/partitioned tables — in the user's schemas
   * only */
  private val rowKinds = "c.relkind in ('c', 'r', 'v', 'm', 'p')"
  private val userSchemas =
    "n.nspname not in ('pg_catalog', 'information_schema') and n.nspname not like 'pg\\_toast%'"

  /** preload every named composite type's field OIDs once, at connect: a
   * simple 'Q' query in the ready state, safe because no portal is open */
  private def loadComposites(): Unit ! Async =
    compositeRows(
      "select ty.oid, a.atttypid from pg_type ty " +
        "join pg_class c on c.oid = ty.typrelid " +
        "join pg_namespace n on n.oid = c.relnamespace " +
        "join pg_attribute a on a.attrelid = c.oid " +
        s"where $rowKinds and $userSchemas and a.attnum > 0 and not a.attisdropped " +
        "order by ty.oid, a.attnum").flatMap { rows =>
      composites.clear()
      for ((typeOid, fieldOid) <- rows)
        composites.update(typeOid, composites.getOrElse(typeOid, Vector.empty) :+ fieldOid)
      loadArrayElems()
    }

  /** the arrays whose element is a named composite */
  private def loadArrayElems(): Unit ! Async =
    compositeRows(
      "select ty.oid, ty.typelem from pg_type ty " +
        "join pg_type el on el.oid = ty.typelem " +
        "join pg_class c on c.oid = el.typrelid " +
        "join pg_namespace n on n.oid = c.relnamespace " +
        s"where $rowKinds and $userSchemas").map { rows =>
      arrayElemDyn.clear()
      for ((arrayOid, elemOid) <- rows) arrayElemDyn.update(arrayOid, elemOid)
    }

  /** the first two int columns of every row of a simple query */
  private def compositeRows(sql: String): Vector[(Int, Int)] ! Async =
    conn.write(msg('Q', str(sql))).flatMap { _ =>
      collectReady(Vector.empty[(Int, Int)]) {
        case (('D', body), acc) =>
          val n = ((body(0) & 0xff) << 8) | (body(1) & 0xff)
          if (n >= 2) {
            var at = 2
            val l1 = readI32(body, at); at += 4
            val c1 = new String(body, at, l1, UTF_8); at += l1
            val l2 = readI32(body, at); at += 4
            val c2 = new String(body, at, l2, UTF_8)
            acc :+ ((c1.toInt, c2.toInt))
          } else acc
        case (_, acc) => acc
      }
    }
}

object PgSql {

  /** startup + SCRAM-SHA-256 as one Async program over the Net seam —
   * the same connect on the JVM and on Node */
  def connect(host: String, port: Int, user: String, password: String, database: String)
             (implicit net: Net, c: Crypto): PgSql ! Async =
    Net.connect(host, port).flatMap(conn => connectOver(conn, user, password, database))

  /** the startup + SCRAM handshake over an ALREADY-established connection
   * — plaintext or TLS-wrapped; this half never learns which */
  def connectOver(conn: NetConn, user: String, password: String, database: String)
                 (implicit c: Crypto): PgSql ! Async = {
    val params = str("user") ++ str(user) ++ str("database") ++ str(database) ++ Array(0.toByte)
    val startup = new Array[Byte](8 + params.length)
    writeI32(startup, 0, params.length + 8)
    writeI32(startup, 4, 196608)
    System.arraycopy(params, 0, startup, 8, params.length)

    def receive(): (Char, Array[Byte]) ! Async =
      conn.readFully(5).flatMap { h =>
        val len = ((h(1) & 0xff) << 24) | ((h(2) & 0xff) << 16) | ((h(3) & 0xff) << 8) | (h(4) & 0xff)
        conn.readFully(len - 4).map(body => ((h(0) & 0xff).toChar, body))
      }

    // each message is pulled inside flatMap: trampolined
    def auth(scram: Scram): PgSql ! Async = receive().flatMap {
      case ('R', body) => readI32(body, 0) match {
        case 0 => auth(scram)
        case 10 =>
          val mechs = new String(body, 4, body.length - 4, UTF_8)
          if (!mechs.contains("SCRAM-SHA-256")) throw PgError(s"server offers no SCRAM-SHA-256 (offered: $mechs)")
          val first = scram.clientFirst
          conn.write(msg('p', str("SCRAM-SHA-256") ++ i32(first.length) ++ first)).flatMap(_ => auth(scram))
        case 11 =>
          conn.write(msg('p', scram.clientFinal(body.drop(4)))).flatMap(_ => auth(scram))
        case 12 =>
          scram.verifyServerFinal(body.drop(4))
          auth(scram)
        case other =>
          throw PgError(s"authentication method $other is not spoken here " +
            "(scram-sha-256 is; md5 and cleartext are deliberately not)")
      }
      case ('E', body) => throw errorOf(body)
      case ('Z', _) =>
        // ready: preload named-composite field OIDs before handing over
        val db = new PgSql(conn)
        db.loadComposites().map(_ => db)
      case _ => auth(scram)
    }

    conn.write(startup).flatMap(_ => auth(new Scram(user, password, Scram.nonce())))
  }

  // ── shared byte helpers ────────────────────────────────────────

  private[pg] def msg(tag: Char, body: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](5 + body.length)
    out(0) = tag.toByte
    writeI32(out, 1, body.length + 4)
    System.arraycopy(body, 0, out, 5, body.length)
    out
  }

  private[pg] def concat(msgs: Array[Byte]*): Array[Byte] = {
    val out = new Array[Byte](msgs.map(_.length).sum)
    var at = 0
    for (m <- msgs) { System.arraycopy(m, 0, out, at, m.length); at += m.length }
    out
  }

  private def writeI32(out: Array[Byte], at: Int, v: Int): Unit = {
    out(at) = (v >> 24).toByte
    out(at + 1) = (v >> 16).toByte
    out(at + 2) = (v >> 8).toByte
    out(at + 3) = v.toByte
  }

  private[pg] def str(s: String): Array[Byte] = s.getBytes(UTF_8) :+ 0.toByte
  private[pg] def i16(v: Int): Array[Byte] = Array((v >> 8).toByte, v.toByte)
  private[pg] def i32(v: Int): Array[Byte] = Array((v >> 24).toByte, (v >> 16).toByte, (v >> 8).toByte, v.toByte)
  private[pg] def readI32(bs: Array[Byte], at: Int): Int =
    ((bs(at) & 0xff) << 24) | ((bs(at + 1) & 0xff) << 16) | ((bs(at + 2) & 0xff) << 8) | (bs(at + 3) & 0xff)

  private[pg] def errorOf(body: Array[Byte]): PgError = {
    var at = 0
    var m = "backend error"
    var code = ""
    while (at < body.length && body(at) != 0) {
      val tag = body(at).toChar
      at += 1
      val start = at
      while (body(at) != 0) at += 1
      val v = new String(body, start, at - start, UTF_8)
      at += 1
      tag match {
        case 'M' => m = v
        case 'C' => code = v
        case _ => ()
      }
    }
    PgError(if (code.isEmpty) m else s"$m [$code]", code)
  }

  /** CommandComplete's tag: the affected count is the last token */
  private[pg] def countOf(body: Array[Byte]): Long = {
    val tag = new String(body, UTF_8).takeWhile(_ != '\u0000')
    tag.split(' ').lastOption.flatMap(_.toLongOption).getOrElse(0L)
  }

  private def levelSql(i: Isolation): String = i match {
    case Isolation.ReadCommitted => "READ COMMITTED"
    case Isolation.RepeatableRead => "REPEATABLE READ"
    case Isolation.Serializable => "SERIALIZABLE"
  }

  /** type OIDs -> the neutral vocabulary */
  private def typeOf(oid: Int): SqlType = oid match {
    case 16 => SqlType.Bool
    case 21 | 23 => SqlType.I32
    case 20 => SqlType.I64
    case 700 | 701 => SqlType.F64
    case 1700 => SqlType.Num
    case 25 | 1043 | 18 | 19 => SqlType.Text
    case 17 => SqlType.Bytes
    case 1114 | 1184 => SqlType.Timestamp
    case 1082 => SqlType.Date
    case 1083 => SqlType.Time
    case 2950 => SqlType.Uuid
    case 114 | 3802 => SqlType.Json
    case other => SqlType.Other(vendorNames.getOrElse(other, s"oid:$other"))
  }

  /** the scalars that stay TEXT on purpose, named so verify can say what
   * it found — a String field fits any of them */
  private val vendorNames: Map[Int, String] = Map(
    142 -> "xml", 1266 -> "timetz", 1186 -> "interval",
    869 -> "inet", 650 -> "cidr", 829 -> "macaddr", 790 -> "money")

  private def valueOf(oid: Int, s: String): SqlValue = oid match {
    case 16 => SqlValue.Bool(s == "t")
    case 21 | 23 => SqlValue.I32(s.toInt)
    case 20 => SqlValue.I64(s.toLong)
    case 700 | 701 => SqlValue.F64(s.toDouble)
    // numeric is EXACT; pg's NaN/Infinity have no BigDecimal, so they
    // fall to the float they are
    case 1700 => s match {
      case "NaN" | "Infinity" | "-Infinity" => SqlValue.F64(s.toDouble)
      case _ => SqlValue.Num(BigDecimal(s))
    }
    case 17 =>
      val hex = s.drop(2)
      val out = new Array[Byte](hex.length / 2)
      var i = 0
      while (i < out.length) {
        out(i) = Integer.parseInt(hex.substring(i * 2, i * 2 + 2), 16).toByte
        i += 1
      }
      SqlValue.Bytes(out)
    // a form the parser does not know stays text — loud in the type,
    // never a wrong number
    case 1114 | 1184 => Temporal.parseTimestamp(s).fold[SqlValue](SqlValue.Text(s))(SqlValue.Timestamp(_))
    case 1082 => Temporal.parseDate(s).fold[SqlValue](SqlValue.Text(s))(SqlValue.Date(_))
    case 1083 => Temporal.parseTime(s).fold[SqlValue](SqlValue.Text(s))(SqlValue.Time(_))
    case 2950 => SqlValue.Uuid(java.util.UUID.fromString(s))
    case 114 | 3802 => SqlValue.Json(s)
    // ROW()/record and arrays decode into structure
    case 2249 => parseComposite(s)
    case a if arrayElem.contains(a) => parseArray(s, r => valueOf(arrayElem(a), r))
    case _ => SqlValue.Text(s)
  }

  // ── composite / array text decoding ────────────────────────────

  /** the common array OIDs -> their element OID */
  private val arrayElem: Map[Int, Int] = Map(
    1000 -> 16, 1005 -> 21, 1007 -> 23, 1016 -> 20,
    1021 -> 700, 1022 -> 701, 1231 -> 1700,
    1009 -> 25, 1015 -> 1043, 1014 -> 1042, 1002 -> 18,
    1001 -> 17, 1028 -> 26,
    1115 -> 1114, 1185 -> 1184, 1182 -> 1082, 1183 -> 1083,
    2951 -> 2950, 199 -> 114, 3807 -> 3802)

  /** split a composite/array body into top-level members, honouring
   * double-quoted values (both `""` and `\"`/`\\` escaping) and — for
   * arrays — brace nesting. Each member is (unescaped-text, quoted). */
  private def splitMembers(body: String, braces: Boolean): Vector[(String, Boolean)] = {
    val out = Vector.newBuilder[(String, Boolean)]
    val sb = new StringBuilder
    var i = 0; var depth = 0; var inQ = false; var quoted = false
    while (i < body.length) {
      val c = body(i)
      if (inQ) {
        if (c == '\\' && i + 1 < body.length) { sb.append(body(i + 1)); i += 2 }
        else if (c == '"' && i + 1 < body.length && body(i + 1) == '"') { sb.append('"'); i += 2 }
        else if (c == '"') { inQ = false; i += 1 }
        else { sb.append(c); i += 1 }
      } else c match {
        case '"' => inQ = true; quoted = true; i += 1
        case '{' if braces => depth += 1; sb.append(c); i += 1
        case '}' if braces => depth -= 1; sb.append(c); i += 1
        case ',' if depth == 0 => out += ((sb.result(), quoted)); sb.clear(); quoted = false; i += 1
        case _ => sb.append(c); i += 1
      }
    }
    out += ((sb.result(), quoted))
    out.result()
  }

  /** `{...}` -> Arr; each element decoded by `decodeElem`, nested arrays
   * with the SAME decoder; the literal NULL element is Null */
  private[pg] def parseArray(s: String, decodeElem: String => SqlValue): SqlValue = parseArray(s, decodeElem, 1)

  /** Postgres caps an array at MAXDIM = 6 dimensions (src/include/utils/
   * array.h) and refuses a deeper literal before it is stored: `dim` is
   * the BOUND of this recursion, and a literal past it is refused by name */
  private[pg] val MaxDim = 6

  private def parseArray(s: String, decodeElem: String => SqlValue, dim: Int): SqlValue = {
    if (dim > MaxDim)
      throw new IllegalStateException(
        s"an array literal nested deeper than $MaxDim dimensions, which Postgres itself refuses (MAXDIM)")
    val inner = s.stripPrefix("{").stripSuffix("}")
    if (inner.isEmpty) SqlValue.Arr(Vector.empty)
    else SqlValue.Arr(splitMembers(inner, braces = true).map { case (raw, quoted) =>
      if (!quoted && raw.equalsIgnoreCase("NULL")) SqlValue.Null
      else if (!quoted && raw.startsWith("{")) parseArray(raw, decodeElem, dim + 1)
      else decodeElem(raw)
    })
  }

  /** `(...)` -> Row; an UNquoted empty field is a SQL NULL, every other
   * field is Text (record field types are not on the wire) */
  private[pg] def parseComposite(s: String): SqlValue = {
    val inner = s.stripPrefix("(").stripSuffix(")")
    SqlValue.Row(splitMembers(inner, braces = false).map { case (raw, quoted) =>
      if (!quoted && raw.isEmpty) SqlValue.Null else SqlValue.Text(raw)
    })
  }

  /** a NAMED composite whose field OIDs are known: each field typed, an
   * unquoted empty field a SQL NULL; extra fields fall back to text */
  private[pg] def parseCompositeTyped(s: String, fieldOids: Vector[Int],
                                      field: (Int, String) => SqlValue = valueOf): SqlValue = {
    val inner = s.stripPrefix("(").stripSuffix(")")
    SqlValue.Row(splitMembers(inner, braces = false).zipWithIndex.map { case ((raw, quoted), i) =>
      if (!quoted && raw.isEmpty) SqlValue.Null
      else if (i < fieldOids.length) field(fieldOids(i), raw)
      else SqlValue.Text(raw)
    })
  }

  /** one row in COPY text format */
  def copyRow(row: Vector[SqlValue]): String =
    row.map {
      case SqlValue.Null => "\\N"
      case v =>
        val s = textOf(v).get
        val sb = new StringBuilder(s.length)
        for (c <- s) c match {
          case '\\' => sb.append("\\\\")
          case '\t' => sb.append("\\t")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case other => sb.append(other)
        }
        sb.result()
    }.mkString("\t")

  private[pg] def textOf(v: SqlValue): Option[String] = v match {
    case SqlValue.Null => None
    case SqlValue.Bool(b) => Some(if (b) "t" else "f")
    case SqlValue.I32(x) => Some(x.toString)
    case SqlValue.I64(x) => Some(x.toString)
    case SqlValue.F64(x) => Some(x.toString)
    case SqlValue.Num(x) => Some(x.toString)
    case SqlValue.Text(s) => Some(s)
    case SqlValue.Timestamp(us) => Some(Temporal.renderTimestamp(us))
    case SqlValue.Date(d) => Some(Temporal.renderDate(d))
    case SqlValue.Time(us) => Some(Temporal.renderTime(us))
    case SqlValue.Uuid(u) => Some(u.toString)
    case SqlValue.Json(j) => Some(j)
    case SqlValue.Bytes(bs) => Some("\\x" + bs.map(b => f"${b & 0xff}%02x").mkString)
    // the reverse of the decode: a structured value re-encodes to the pg
    // literal, so a decoded Arr/Row round-trips through copy/bind
    case SqlValue.Arr(elems) => Some(arrayLiteral(elems))
    case SqlValue.Row(fields) => Some(compositeLiteral(fields))
  }

  private def arrayLiteral(elems: Vector[SqlValue]): String =
    elems.map {
      case SqlValue.Null => "NULL"
      case a: SqlValue.Arr => arrayLiteral(a.elems)
      case v => "\"" + textOf(v).getOrElse("").replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    }.mkString("{", ",", "}")

  private def compositeLiteral(fields: Vector[SqlValue]): String =
    fields.map {
      case SqlValue.Null => ""
      case v => "\"" + textOf(v).getOrElse("").replace("\\", "\\\\").replace("\"", "\"\"") + "\""
    }.mkString("(", ",", ")")
}
