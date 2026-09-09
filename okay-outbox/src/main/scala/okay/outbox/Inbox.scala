package okay.outbox

import okay.*
import okay.codec.Schema
import okay.sql.{Sql, SqlValue, Typed}

/**
 * The inbox (specs/outbox.md): a consumer with database effects
 * records the message id INSIDE its own transaction, and a duplicate
 * is refused by the primary key — the constraint is the machinery,
 * the select is the fast path. A body that fails rolls the id back
 * with it, so the record is tried again.
 */
final class Inbox(table: String = "okay_inbox",
                  clock: () => Long = () => System.currentTimeMillis):

  def ddl(dialect: Dialect): String =
    val _ = dialect   // no bytes column; the parameter keeps the two DDLs one shape
    s"""create table $table (
       |  id varchar(64) primary key,
       |  seen_at bigint not null
       |)""".stripMargin

  /** INSIDE the caller's transaction: true the first time an id is
    * seen (the row is inserted), false when it is already there. Two
    * transactions racing on one id both pass the select; the second
    * insert fails on the key and its transaction is cancelled. */
  def first(db: Sql, id: String): Boolean ! Async =
    Rows.all[Seen](db, s"select id, seen_at from $table where id = ?", Vector(SqlValue.Text(id)))
      .flatMap { rows =>
        if rows.nonEmpty then pure(false)
        else db.update(s"insert into $table (id, seen_at) values (?, ?)",
          Vector(SqlValue.Text(id), SqlValue.I64(clock()))).map(_ => true)
      }

  /** the whole shape: a transaction that records the id and runs the
    * body; a duplicate runs nothing and answers None */
  def once[A](db: Sql, id: String)(body: => A ! Async): Option[A] ! Async =
    Resource.run[Option[A], Async](
      Typed.transact[Option[A], Async](db) { _ =>
        !.widen[Boolean, Async, Resource](first(db, id)).flatMap { fresh =>
          if fresh then !.widen[A, Async, Resource](body).map(Some(_))
          else pure(None)
        }
      })

private final case class Seen(id: String, seenAt: Long) derives Schema
