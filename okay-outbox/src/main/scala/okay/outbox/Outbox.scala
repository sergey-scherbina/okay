package okay.outbox

import okay.*
import okay.codec.Schema
import okay.persist.{Ack, Store}
import okay.sql.{Sql, SqlValue, Typed}

/** the column type bytes take — the one thing SQL never agreed on */
enum Dialect:
  case H2, Postgres, Sqlite

  def bytes: String = this match
    case H2 => "varbinary(1000000)"
    case Postgres => "bytea"
    case Sqlite => "blob"

/** one outbox row: the event as the transaction wrote it */
final case class Entry(id: String, topic: String, part: Int,
                       msgKey: Array[Byte], msgValue: Array[Byte],
                       createdAt: Long) derives Schema

/**
 * The transactional outbox (specs/outbox.md): a business transaction
 * writes its event as a ROW beside its other rows, and a relay carries
 * rows into the log afterwards. The event and the rows share one
 * fate; the relay is at-least-once and says so — the row id is the
 * record KEY, so a downstream `Inbox` collapses a duplicate.
 */
final class Outbox(table: String = "okay_outbox",
                   clock: () => Long = () => System.currentTimeMillis,
                   ids: () => String = () => java.util.UUID.randomUUID.toString):

  /** the DDL, for our own database's migration scripts */
  def ddl(dialect: Dialect): String =
    s"""create table $table (
       |  id varchar(64) primary key,
       |  topic varchar(255) not null,
       |  part int not null,
       |  msg_key ${dialect.bytes},
       |  msg_value ${dialect.bytes} not null,
       |  created_at bigint not null,
       |  published_at bigint
       |)""".stripMargin

  /** INSIDE the caller's transaction: the event as a row; answers its id */
  def enqueue(db: Sql, topic: String, value: Array[Byte],
              key: Array[Byte] = Array.empty, part: Int = 0): String ! Async =
    val id = ids()
    Typed.update[Entry](db,
      s"insert into $table (id, topic, part, msg_key, msg_value, created_at) values (?, ?, ?, ?, ?, ?)")(
      Entry(id, topic, part, key, value, clock())).map(_ => id)

  /** unpublished rows, oldest first */
  private def unpublished(db: Sql, batch: Int): Vector[Entry] ! Async =
    Rows.all[Entry](db,
      s"select id, topic, part, msg_key, msg_value, created_at from $table where published_at is null order by created_at, id limit $batch")

  /**
   * One relay pass: each unpublished row appended to its topic (the
   * row id as the record key), then marked published. A crash between
   * the two re-appends the row next pass — the same key, twice: what
   * the inbox is for.
   */
  def relayOnce(db: Sql, store: Store, batch: Int = 256): Int ! Async =
    unpublished(db, batch).flatMap { rows =>
      def go(rest: List[Entry], n: Int): Int ! Async = rest match
        case Nil => pure(n)
        case e :: more =>
          okay.async {
            store.topic(e.topic).append(e.part, e.id.getBytes("UTF-8"), e.msgValue, Ack.Durable): Unit
          }.flatMap(_ => db.update(s"update $table set published_at = ? where id = ?",
              Vector(SqlValue.I64(clock()), SqlValue.Text(e.id))))
            .flatMap(_ => go(more, n + 1))
      go(rows.toList, 0)
    }

  /** the relay as a loop, every `everyMillis`, until cancelled */
  def relay(db: Sql, store: Store, everyMillis: Long, batch: Int = 256)(using Timer): Nothing ! Async =
    relayOnce(db, store, batch).flatMap(_ => Async.sleep(everyMillis)).flatMap(_ => relay(db, store, everyMillis, batch))

  /** unpublished rows, as of now */
  def pending(db: Sql): Long ! Async =
    Rows.all[Count](db, s"select count(*) as n from $table where published_at is null")
      .map(_.headOption.map(_.n).getOrElse(0L))

private final case class Count(n: Long) derives Schema
