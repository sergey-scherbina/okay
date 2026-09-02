package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, Stream}
import okay.given
import okay.persist.{Ack, Policy, Record, Store, Topic}
import okay.sql.{Sql, SqlValue}

/**
 * The durable log over a SQL table (specs/persist.md, stage 3) —
 * the `Journal` doc-comment's oldest promise ("a file or a table
 * behind the same three methods"), kept through the Sql seam so
 * ANY driver serves it: H2 for tests, Postgres over the wire, a
 * warehouse if a business insists.
 *
 * OWN posture: two tables, created by `ensure()`. The `Topic` SPI
 * is sync, and this engine is Async underneath — so it BLOCKS,
 * honestly (CanBlock; virtual threads make it real), the JdbcSql
 * argument run in reverse. One store instance = one connection =
 * one writer, the driver's own contract.
 *
 * Retention is per record (the memory engine's granularity):
 * bytes are tracked and the front is deleted while over budget.
 * `begin` moves ONLY under retention and is state of its own (a
 * begins table) — compaction leaves holes but never the start.
 * Compaction is one DELETE keeping the latest per key. Offsets are
 * assigned dense from a cached end initialized at open — the table
 * is the truth, the caches are caches.
 */
final class SqlStore(db: Sql, prefix: String = "okay_persist") extends Store:
  import SqlStore.*

  private val records = s"${prefix}_records"
  private val topicsT = s"${prefix}_topics"

  /** own-posture DDL, once */
  def ensure(): Unit =
    run(db.update(
      s"""create table if not exists $records(
         topic varchar(128) not null,
         part int not null,
         off bigint not null,
         ts bigint not null,
         k varbinary(4096) not null,
         v varbinary(1048576) not null,
         primary key (topic, part, off))"""))
    run(db.update(
      s"""create table if not exists $topicsT(
         name varchar(128) primary key not null,
         parts int not null)"""))
    // begin moves ONLY under retention (compaction leaves holes but
    // never the start), so it is state of its own, not min(off)
    run(db.update(
      s"""create table if not exists ${prefix}_begins(
         topic varchar(128) not null,
         part int not null,
         begin_off bigint not null,
         primary key (topic, part))"""))
    ()

  private var open = Vector.empty[SqlTopic]

  def topic(name: String, partitions: Int, policy: Policy): Topic = synchronized {
    open.find(_.name == name) match
      case Some(t) =>
        if t.partitions != partitions then
          throw IllegalArgumentException(
            s"topic $name has ${t.partitions} partitions; asked for $partitions — " +
              "rerouting keys would break per-key order")
        t
      case None =>
        val stored = one(s"select parts from $topicsT where name = ?",
          Vector(SqlValue.Text(name))).map(intOf)
        stored match
          case Some(p) if p != partitions =>
            throw IllegalArgumentException(
              s"topic $name exists with $p partitions; asked for $partitions")
          case Some(_) => ()
          case None =>
            run(db.update(s"insert into $topicsT values (?, ?)",
              Vector(SqlValue.Text(name), SqlValue.I32(partitions))))
            ()
        val t = new SqlTopic(name, partitions, policy)
        open :+= t
        t
  }

  def topics: Vector[String] = synchronized {
    all(s"select name from $topicsT order by name").map {
      case SqlValue.Text(s) => s
      case other => other.toString
    }
  }

  def stats: Store.Stats = synchronized {
    Store.Stats(open.map { t =>
      Store.TopicStats(t.name, Vector.tabulate(t.partitions)(t.statsOf))
    })
  }

  // ── plumbing ───────────────────────────────────────────────────

  private def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  private def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async))
  : Vector[Vector[SqlValue]] =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    def go(rest: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] ! Async =
      S.uncons(rest).flatMap {
        case None => okay.pure(Vector.empty)
        case Some((c, more)) => go(more).map(c.toVector ++ _)
      }
    run(go(p))

  private def all(sql: String, params: Vector[SqlValue] = Vector.empty): Vector[SqlValue] =
    drain(db.query(sql, params)).map(_.head)

  private def one(sql: String, params: Vector[SqlValue]): Option[SqlValue] =
    all(sql, params).headOption

  private def longOf(v: SqlValue): Long = v match
    case SqlValue.I64(x) => x
    case SqlValue.I32(x) => x.toLong
    case SqlValue.Num(x) => x.toLong // H2 types SUM(expr) as NUMERIC
    case SqlValue.F64(x) => x.toLong
    case SqlValue.Null => 0L
    case other => throw IllegalStateException(s"expected a number, got $other")

  private def intOf(v: SqlValue): Int = longOf(v).toInt

  private def bytesOf(v: SqlValue): Array[Byte] = v match
    case SqlValue.Bytes(bs) => bs
    case SqlValue.Null => Array.empty
    case other => throw IllegalStateException(s"expected bytes, got $other")

  private val frameOverhead = 28L // agreed with the other engines

  private final class SqlTopic(val name: String, val partitions: Int,
                               policy: Policy) extends Topic:
    // the table is the truth; these are caches, filled at open
    // an aggregate over nothing answers a NULL row, not no row —
    // the empty partition starts at 0, not at NULL + 1
    private val next = Array.tabulate(partitions) { p =>
      one(s"select max(off) from $records where topic = ? and part = ?",
        Vector(SqlValue.Text(name), SqlValue.I32(p)))
        .filter(_ != SqlValue.Null).map(longOf).map(_ + 1).getOrElse(0L)
    }
    private val begins = Array.tabulate(partitions) { p =>
      one(s"select begin_off from ${prefix}_begins where topic = ? and part = ?",
        Vector(SqlValue.Text(name), SqlValue.I32(p)))
        .filter(_ != SqlValue.Null).map(longOf)
        .orElse(one(s"select min(off) from $records where topic = ? and part = ?",
          Vector(SqlValue.Text(name), SqlValue.I32(p)))
          .filter(_ != SqlValue.Null).map(longOf))
        .getOrElse(0L)
    }
    private val bytes = Array.tabulate(partitions) { p =>
      one(s"select sum(length(k) + length(v) + $frameOverhead) from $records " +
        "where topic = ? and part = ?",
        Vector(SqlValue.Text(name), SqlValue.I32(p))).map(longOf).getOrElse(0L)
    }

    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      SqlStore.this.synchronized {
        val off = next(partition)
        run(db.update(s"insert into $records values (?, ?, ?, ?, ?, ?)",
          Vector(SqlValue.Text(name), SqlValue.I32(partition), SqlValue.I64(off),
            SqlValue.I64(System.currentTimeMillis()),
            SqlValue.Bytes(key), SqlValue.Bytes(value))))
        next(partition) = off + 1
        bytes(partition) += key.length.toLong + value.length + frameOverhead
        if !policy.compact then retain(partition)
        off
      }

    private def retain(partition: Int): Unit =
      var moved = false
      while bytes(partition) > policy.retainBytes && countOf(partition) > 1 do
        val head = one(s"select min(off) from $records where topic = ? and part = ?",
          Vector(SqlValue.Text(name), SqlValue.I32(partition))).map(longOf).get
        val dropped = one(
          s"select length(k) + length(v) + $frameOverhead from $records " +
            "where topic = ? and part = ? and off = ?",
          Vector(SqlValue.Text(name), SqlValue.I32(partition), SqlValue.I64(head)))
          .map(longOf).getOrElse(0L)
        run(db.update(s"delete from $records where topic = ? and part = ? and off = ?",
          Vector(SqlValue.Text(name), SqlValue.I32(partition), SqlValue.I64(head))))
        bytes(partition) -= dropped
        begins(partition) = head + 1
        moved = true
      if moved then
        run(db.update(s"merge into ${prefix}_begins key (topic, part) values (?, ?, ?)",
          Vector(SqlValue.Text(name), SqlValue.I32(partition),
            SqlValue.I64(begins(partition)))))
        ()

    private def countOf(partition: Int): Long =
      one(s"select count(*) from $records where topic = ? and part = ?",
        Vector(SqlValue.Text(name), SqlValue.I32(partition))).map(longOf).getOrElse(0L)

    def read(partition: Int, from: Long, max: Int): Topic.Read =
      SqlStore.this.synchronized {
        val b = begin(partition)
        if from < b then Topic.Read.TooEarly(b)
        else
          val rows = drain(db.query(
            s"select off, ts, k, v from $records where topic = ? and part = ? " +
              s"and off >= ? order by off limit $max",
            Vector(SqlValue.Text(name), SqlValue.I32(partition), SqlValue.I64(from))))
          Topic.Read.Records(rows.map(r =>
            Record(longOf(r(0)), longOf(r(1)), bytesOf(r(2)), bytesOf(r(3)))))
      }

    def begin(partition: Int): Long = begins(partition)
    def end(partition: Int): Long = next(partition)

    def compact(partition: Int): Unit =
      SqlStore.this.synchronized {
        run(db.update(
          s"""delete from $records r where r.topic = ? and r.part = ? and exists (
             select 1 from $records r2 where r2.topic = r.topic and r2.part = r.part
             and r2.k = r.k and r2.off > r.off)""",
          Vector(SqlValue.Text(name), SqlValue.I32(partition))))
        bytes(partition) = one(
          s"select sum(length(k) + length(v) + $frameOverhead) from $records " +
            "where topic = ? and part = ?",
          Vector(SqlValue.Text(name), SqlValue.I32(partition))).map(longOf).getOrElse(0L)
      }

    def statsOf(partition: Int): Store.PartitionStats =
      Store.PartitionStats(partition, begin(partition), end(partition),
        bytes(partition), math.max(1, countOf(partition).toInt))

object SqlStore:
  /** open-and-ensure in one move */
  def apply(db: Sql, prefix: String = "okay_persist"): SqlStore =
    val s = new SqlStore(db, prefix)
    s.ensure()
    s
