package okay.tx

import java.sql.Connection
import okay.one.*
import okay.one.Cont.{defer, pure}
import okay.one.row.*
import okay.one.std.{Reader, %}

// not capture-checked: the kernel's programs are not (specs/okay-caps.md, "The kernel"); `Session` is, in its file

/** A TRANSACTOR OVER JDBC, ITS RESOURCE A GUARDED `Session`: `Jdbc`'s begin, commit, rollback and retry, the
 * session closed first at the end of the bracket. `Sessions.execute`, `Sessions.queryInt` on the bracket's session */
final class Sessions[T <: Row](connect: () => Connection, isolation: Int = Connection.TRANSACTION_READ_COMMITTED,
    readOnly: Boolean = false, retries: Int = 0) extends Tx.Transactor[Session, T]:
  private val jdbc = Jdbc[T](connect, isolation, readOnly, retries)
  def begin: Session ! T = jdbc.begin.map(Session(_))
  def commit(s: Session): Unit ! T = defer { s.close(); jdbc.commit(s.raw) }
  def rollback(s: Session): Unit ! T = defer { s.close(); jdbc.rollback(s.raw) }
  override def retry(e: Throwable, attempt: Int): Boolean = jdbc.retry(e, attempt)

object Sessions:
  /** a statement on the bracket's session: `Sessions.execute(...)` inside `atomic` */
  def execute[R <: Row](sql: String)(using In[Reader % Session, R]): Unit ! R =
    Tx.resource[Session, R].flatMap(s => defer { s.execute(sql); pure(()) })

  /** one value of a query on the bracket's session */
  def queryInt[R <: Row](sql: String)(using In[Reader % Session, R]): Int ! R =
    Tx.resource[Session, R].flatMap(s => defer(pure(s.queryInt(sql))))
