package okay.pg

import okay.{!, +, Async, Chunk, ChunkBuf, Chunks, Net, NetConn, Produce, async, effect, pure}
import okay.sql.{Col, Granted, Isolation, Sql, SqlType, SqlValue}
import okay.crypto.Crypto
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The Postgres v3 wire, natively (specs/sql.md): the direct road
 * the seam exists for — no java.sql, no JDBC driver, the protocol
 * itself behind the same `Sql` trait. CROSS-PLATFORM since
 * sql-pg-node: the message pump PULLS bytes through the Net seam
 * (specs/net.md) as a sequential Async program, so the same driver
 * runs over a blocking socket on the JVM and over Node's buffered
 * `net` events — and SCRAM speaks the per-platform PgCrypto given
 * on both. Startup + SCRAM-SHA-256 (server signature VERIFIED), the
 * extended query protocol with portals as the chunk mechanism,
 * text format both directions, errors drained to quiet so the
 * session survives.
 *
 * `cancel` — the region's sync brake — MARKS the rollback and the
 * next operation on this connection performs it first (program
 * order is server order, so the rollback happens-before any later
 * use); a connection dropped without a next use is rolled back by
 * the server, the abandoned-transaction truth every deployment
 * already relies on. One driver instance = one logical thread of
 * control, the driver contract.
 */
final class PgSql private (conn: NetConn) extends Sql:
  import PgSql.*

  private var inTx = false
  @volatile private var pendingRollback = false

  // ── the Sql seam ───────────────────────────────────────────────

  def describe(sql: String): Vector[Col] ! Async =
    settled {
      conn.write(concat(
        msg('P', str("") ++ str(sql) ++ i16(0)),
        msg('D', Array('S'.toByte) ++ str("")),
        msg('S', Array.empty))).flatMap { _ =>
        collectReady(Vector.empty[(String, Int, Int, Int)]) {
          case (('T', body), _) => rowDescription(body)
          case (_, acc) => acc
        }.flatMap { cols =>
          // RowDescription has no nullability; the catalog does
          def resolve(rest: List[(String, Int, Int, Int)],
                      acc: Vector[Col]): Vector[Col] ! Async = rest match
            case Nil => pure(acc)
            case (label, oid, tableOid, attnum) :: more =>
              if tableOid == 0 then
                resolve(more, acc :+ Col(label, typeOf(oid), true))
              else
                simpleValue(s"select attnotnull from pg_attribute " +
                  s"where attrelid = $tableOid and attnum = $attnum").flatMap { v =>
                  resolve(more, acc :+ Col(label, typeOf(oid), !v.contains("t")))
                }
          resolve(cols.toList, Vector.empty)
        }
      }
    }

  def query(sql: String, params: Vector[SqlValue])
  : Chunk[Vector[SqlValue]] ! (Produce + Async) =
    type F = Produce + Async

    def openPortal: Vector[Int] ! Async =
      conn.write(concat(
        msg('P', str("") ++ str(sql) ++ i16(0)),
        bindMsg(params),
        msg('D', Array('P'.toByte) ++ str("")),
        msg('H', Array.empty))).flatMap { _ =>
        def await: Vector[Int] ! Async = receive().flatMap {
          case ('T', body) => pure(rowDescription(body).map(_._2))
          case ('n', _) => pure(Vector.empty)
          case ('E', body) => failToQuiet(body)
          case _ => await
        }
        await
      }

    def readChunk(oids: Vector[Int]): (Chunk[Vector[SqlValue]], Boolean) ! Async =
      conn.write(concat(msg('E', str("") ++ i32(fetchSize)), msg('H', Array.empty)))
        .flatMap { _ =>
          def go(buf: Vector[Vector[SqlValue]]): (Chunk[Vector[SqlValue]], Boolean) ! Async =
            receive().flatMap {
              case ('D', body) => go(buf :+ dataRow(body, oids))
              case ('s', _) => pure((ChunkBuf.of(buf), true))
              case ('C', _) => pure((ChunkBuf.of(buf), false))
              case ('E', body) => failToQuiet(body)
              case _ => go(buf)
            }
          go(Vector.empty)
        }

    def emit(oids: Vector[Int]): Chunk[Vector[SqlValue]] ! F =
      !.widen[(Chunk[Vector[SqlValue]], Boolean), Async, Produce](readChunk(oids))
        .flatMap { (c, more) =>
          if !more then
            !.widen[Unit, Async, Produce](finishPortal).flatMap { _ =>
              if c.isEmpty then pure(Chunks.emptyChunk)
              else effect[F, Chunk[Vector[SqlValue]]](c)
            }
          else effect[F, Chunk[Vector[SqlValue]]](c).flatMap(_ => emit(oids))
        }

    !.widen[Unit, Async, Produce](settled(pure(())))
      .flatMap(_ => !.widen[Vector[Int], Async, Produce](openPortal))
      .flatMap(emit)

  def update(sql: String, params: Vector[SqlValue]): Long ! Async =
    settled {
      conn.write(concat(
        msg('P', str("") ++ str(sql) ++ i16(0)),
        bindMsg(params),
        msg('E', str("") ++ i32(0)),
        msg('S', Array.empty))).flatMap { _ =>
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
        msg('S', Array.empty)
      conn.write(concat(msgs*)).flatMap { _ =>
        collectReady(0L) {
          case (('C', body), acc) => acc + countOf(body)
          case (_, acc) => acc
        }
      }
    }

  def begin(isolation: Isolation): Granted ! Async =
    settled {
      if inTx then throw IllegalStateException(
        "nested transaction: this connection is already in one — " +
          "refuse rather than silently flatten (specs/jdbc.md)")
      simple("BEGIN").flatMap { _ =>
        simple(s"SET TRANSACTION ISOLATION LEVEL ${levelSql(isolation)}").flatMap { _ =>
          inTx = true
          simpleValue("SHOW transaction_isolation").map { v =>
            val granted = v match
              case Some("serializable") => Isolation.Serializable
              case Some("repeatable read") => Isolation.RepeatableRead
              case _ => Isolation.ReadCommitted
            Granted(isolation, granted)
          }
        }
      }
    }

  def commit(): Unit ! Async = settled(simple("COMMIT").map { _ => inTx = false })

  def rollback(): Unit ! Async = settled(simple("ROLLBACK").map { _ => inTx = false })

  /** the sync brake: mark now, roll back before the next use —
   * program order is server order, and an abandoned connection is
   * rolled back by the server anyway */
  def cancel(): Unit =
    if inTx then
      pendingRollback = true
      inTx = false

  def close(): Unit = conn.close()

  // ── COPY: the bulk-load road ───────────────────────────────────

  /** raw COPY IN over the simple protocol (see specs/sql.md): rows
   * arrive already text-encoded (`PgSql.copyRow`) */
  def copyIn(sql: String, rows: Iterator[String]): Long ! Async =
    settled {
      conn.write(msg('Q', str(sql))).flatMap { _ =>
        def awaitCopy: Unit ! Async = receive().flatMap {
          case ('G', _) => pure(())
          case ('E', body) => failToQuiet(body)
          case ('Z', _) => throw PgError(s"the statement did not start a COPY: $sql")
          case _ => awaitCopy
        }
        awaitCopy.flatMap { _ =>
          val payload = rows.map(l => msg('d', (l + "\n").getBytes(UTF_8))).toVector
          conn.write(concat((payload :+ msg('c', Array.empty))*)).flatMap { _ =>
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
      val len = ((h(1) & 0xff) << 24) | ((h(2) & 0xff) << 16) |
        ((h(3) & 0xff) << 8) | (h(4) & 0xff)
      if len < 4 || len > 512 * 1024 * 1024 then
        throw PgError(s"message length $len is not a message")
      conn.readFully(len - 4).map(body => (tag, body))
    }

  /** pump to ReadyForQuery, folding what the caller cares about;
   * an ErrorResponse is remembered and THROWN after quiet, so the
   * session survives */
  private def collectReady[S](init: S)(f: ((Char, Array[Byte]), S) => S): S ! Async =
    def go(acc: S, err: Option[PgError]): S ! Async =
      receive().flatMap {
        case ('Z', _) => err.fold(pure(acc))(e => throw e)
        case ('E', body) => go(acc, err.orElse(Some(errorOf(body))))
        case m => go(f(m, acc), err)
      }
    go(init, None)

  /** an error mid-conversation: reach quiet first, then throw */
  private def failToQuiet[A](body: Array[Byte]): A ! Async =
    conn.write(msg('S', Array.empty)).flatMap { _ =>
      val err = errorOf(body)
      collectReady(())((_, _) => ()).map(_ => throw err)
    }

  private def finishPortal: Unit ! Async =
    conn.write(concat(msg('C', Array('P'.toByte) ++ str("")), msg('S', Array.empty)))
      .flatMap(_ => collectReady(())((_, _) => ()))

  private def simple(sql: String): Unit ! Async =
    conn.write(msg('Q', str(sql))).flatMap(_ => collectReady(())((_, _) => ()))

  private def simpleValue(sql: String): Option[String] ! Async =
    conn.write(msg('Q', str(sql))).flatMap { _ =>
      collectReady(Option.empty[String]) {
        case (('D', body), _) =>
          val n = ((body(0) & 0xff) << 8) | (body(1) & 0xff)
          if n >= 1 then
            val len = readI32(body, 2)
            if len >= 0 then Some(new String(body, 6, len, UTF_8)) else None
          else None
        case (_, acc) => acc
      }
    }

  /** the pending-rollback settle: cancel's mark performed before
   * any next use of this connection */
  private def settled[A](prog: => A ! Async): A ! Async =
    if !pendingRollback then pure(()).flatMap(_ => prog)
    else
      pendingRollback = false
      simple("ROLLBACK").flatMap(_ => prog)

  private def bindMsg(params: Vector[SqlValue]): Array[Byte] =
    val b = Array.newBuilder[Byte]
    b ++= str("") ++= str("")
    b ++= i16(0)
    b ++= i16(params.length)
    for p <- params do
      textOf(p) match
        case None => b ++= i32(-1)
        case Some(s) =>
          val bs = s.getBytes(UTF_8)
          b ++= i32(bs.length) ++= bs
    b ++= i16(0)
    msg('B', b.result())

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
      i16r(); i32r(); i16r()
      (label, typeOid, tableOid, attnum)
    }

  private def dataRow(body: Array[Byte], oids: Vector[Int]): Vector[SqlValue] =
    var at = 2
    Vector.tabulate(oids.length) { i =>
      val len = readI32(body, at); at += 4
      if len < 0 then SqlValue.Null
      else
        val s = new String(body, at, len, UTF_8)
        at += len
        valueOf(oids(i), s)
    }

object PgSql:

  /** startup + SCRAM-SHA-256 as one Async program over the Net
   * seam — the same connect on the JVM and on Node */
  def connect(host: String, port: Int, user: String, password: String,
              database: String)(using Net, Crypto): PgSql ! Async =
    Net.connect(host, port).flatMap { conn =>
      val params = str("user") ++ str(user) ++ str("database") ++ str(database) ++
        Array(0.toByte)
      val startup = new Array[Byte](8 + params.length)
      writeI32(startup, 0, params.length + 8)
      writeI32(startup, 4, 196608)
      System.arraycopy(params, 0, startup, 8, params.length)

      def receive(): (Char, Array[Byte]) ! Async =
        conn.readFully(5).flatMap { h =>
          val len = ((h(1) & 0xff) << 24) | ((h(2) & 0xff) << 16) |
            ((h(3) & 0xff) << 8) | (h(4) & 0xff)
          conn.readFully(len - 4).map(body => ((h(0) & 0xff).toChar, body))
        }

      def auth(scram: Scram): PgSql ! Async = receive().flatMap {
        case ('R', body) => readI32(body, 0) match
          case 0 => auth(scram)
          case 10 =>
            val mechs = new String(body, 4, body.length - 4, UTF_8)
            if !mechs.contains("SCRAM-SHA-256") then
              throw PgError(s"server offers no SCRAM-SHA-256 (offered: $mechs)")
            val first = scram.clientFirst
            conn.write(msg('p', str("SCRAM-SHA-256") ++ i32(first.length) ++ first))
              .flatMap(_ => auth(scram))
          case 11 =>
            conn.write(msg('p', scram.clientFinal(body.drop(4))))
              .flatMap(_ => auth(scram))
          case 12 =>
            scram.verifyServerFinal(body.drop(4))
            auth(scram)
          case other =>
            throw PgError(s"authentication method $other is not spoken here " +
              "(scram-sha-256 is; md5 and cleartext are deliberately not)")
        case ('E', body) => throw errorOf(body)
        case ('Z', _) => pure(new PgSql(conn))
        case _ => auth(scram)
      }

      conn.write(startup).flatMap { _ =>
        auth(Scram(user, password, Scram.nonce()))
      }
    }

  // ── shared byte helpers ────────────────────────────────────────

  private[pg] def msg(tag: Char, body: Array[Byte]): Array[Byte] =
    val out = new Array[Byte](5 + body.length)
    out(0) = tag.toByte
    writeI32(out, 1, body.length + 4)
    System.arraycopy(body, 0, out, 5, body.length)
    out

  private[pg] def concat(msgs: Array[Byte]*): Array[Byte] =
    val out = new Array[Byte](msgs.map(_.length).sum)
    var at = 0
    for m <- msgs do { System.arraycopy(m, 0, out, at, m.length); at += m.length }
    out

  private def writeI32(out: Array[Byte], at: Int, v: Int): Unit =
    out(at) = (v >> 24).toByte
    out(at + 1) = (v >> 16).toByte
    out(at + 2) = (v >> 8).toByte
    out(at + 3) = v.toByte

  private[pg] def str(s: String): Array[Byte] = s.getBytes(UTF_8) :+ 0.toByte
  private[pg] def i16(v: Int): Array[Byte] = Array((v >> 8).toByte, v.toByte)
  private[pg] def i32(v: Int): Array[Byte] =
    Array((v >> 24).toByte, (v >> 16).toByte, (v >> 8).toByte, v.toByte)
  private[pg] def readI32(bs: Array[Byte], at: Int): Int =
    ((bs(at) & 0xff) << 24) | ((bs(at + 1) & 0xff) << 16) |
      ((bs(at + 2) & 0xff) << 8) | (bs(at + 3) & 0xff)

  private[pg] def errorOf(body: Array[Byte]): PgError =
    var at = 0
    var m = "backend error"
    var code = ""
    while at < body.length && body(at) != 0 do
      val tag = body(at).toChar
      at += 1
      val start = at
      while body(at) != 0 do at += 1
      val v = new String(body, start, at - start, UTF_8)
      at += 1
      tag match
        case 'M' => m = v
        case 'C' => code = v
        case _ => ()
    PgError(if code.isEmpty then m else s"$m [$code]")

  /** CommandComplete's tag: the affected count is the last token */
  private[pg] def countOf(body: Array[Byte]): Long =
    val tag = new String(body, UTF_8).takeWhile(_ != ' ')
    tag.split(' ').lastOption.flatMap(_.toLongOption).getOrElse(0L)

  private def levelSql(i: Isolation): String = i match
    case Isolation.ReadCommitted => "READ COMMITTED"
    case Isolation.RepeatableRead => "REPEATABLE READ"
    case Isolation.Serializable => "SERIALIZABLE"

  /** type OIDs → the neutral vocabulary; numeric→F64 v1, stated */
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
      val hex = s.drop(2)
      val out = new Array[Byte](hex.length / 2)
      var i = 0
      while i < out.length do
        out(i) = Integer.parseInt(hex.substring(i * 2, i * 2 + 2), 16).toByte
        i += 1
      SqlValue.Bytes(out)
    case _ => SqlValue.Text(s)

  /** one row in COPY text format (see specs/sql.md) */
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
