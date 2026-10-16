package okay.tx

import language.experimental.captureChecking

import java.sql.Connection
import java.util.concurrent.atomic.AtomicBoolean

/** A BRACKET'S CONNECTION, GUARDED: what `Sessions` hands out inside a bracket instead of the raw `Connection`.
 * Closed when its bracket commits or rolls back, before the connection is: a statement on it after that is
 * `Session.Closed`, naming the leak — never a statement on a connection closed, or given back to its pool and maybe
 * in another bracket's hands. A `caps.SharedCapability`: a capture-checked caller holding a session as a value cannot
 * carry it out of its scope (okay-caps) */
final class Session private[tx] (connection: Connection) extends caps.SharedCapability:
  private val open = AtomicBoolean(true)

  private def live(sql: String): Connection =
    if !open.get then throw Session.Closed(sql)
    connection

  /** a statement */
  def execute(sql: String): Unit =
    val st = live(sql).createStatement()
    try st.execute(sql): Unit finally st.close()

  /** one value of a query */
  def queryInt(sql: String): Int =
    val st = live(sql).createStatement()
    try
      val rs = st.executeQuery(sql)
      rs.next(): Unit
      rs.getInt(1)
    finally st.close()

  /** the connection itself, for what the words above do not say — checked as they are, and not to be kept */
  def withConnection[A](f: Connection => A): A = f(live("withConnection"))

  private[tx] def close(): Unit = open.set(false)
  private[tx] def raw: Connection = connection

object Session:
  /** a statement on a session after its bracket ended: the session leaked out of it */
  final class Closed(sql: String) extends IllegalStateException(
    s"session closed: '$sql' after its bracket's commit or rollback — the session leaked out of the bracket")
