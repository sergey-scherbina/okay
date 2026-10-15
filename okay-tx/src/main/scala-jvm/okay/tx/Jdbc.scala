package okay.tx

import java.sql.{Connection, SQLException}
import okay.one.*
import okay.one.Cont.{defer, pure}
import okay.one.row.*
import okay.one.std.{Reader, %}

/** A TRANSACTOR OVER JDBC: the resource a `Connection`, from `connect` (a pool's); begin is auto-commit off at the
 * isolation asked; commit and rollback the driver's, the connection closed — given back to its pool — after either;
 * a serialization failure or a deadlock (SQLSTATE 40001, 40P01) runs the bracket again, up to `retries` more times.
 * Synchronous: each step a program of effects that calls JDBC where it runs — nothing of a thread in the core */
final class Jdbc[T <: Row](connect: () => Connection, isolation: Int = Connection.TRANSACTION_READ_COMMITTED,
    readOnly: Boolean = false, retries: Int = 0) extends Tx.Transactor[Connection, T]:
  def begin: Connection ! T = defer {
    val c = connect()
    c.setAutoCommit(false)
    c.setTransactionIsolation(isolation)
    c.setReadOnly(readOnly)
    pure(c)
  }
  def commit(c: Connection): Unit ! T = defer { try c.commit() finally c.close(); pure(()) }
  def rollback(c: Connection): Unit ! T = defer { try c.rollback() finally c.close(); pure(()) }
  override def retry(e: Throwable, attempt: Int): Boolean = attempt <= retries && Jdbc.retryable(e)

object Jdbc:
  /** a failure the engine means as "run it again": a serialization failure, a deadlock */
  def retryable(e: Throwable): Boolean = e match
    case s: SQLException => s.getSQLState == "40001" || s.getSQLState == "40P01"
    case _ => false

  /** a statement on the bracket's connection: `sql(...)` inside `atomic` */
  def execute[R <: Row](sql: String)(using In[Reader % Connection, R]): Unit ! R =
    Tx.resource[Connection, R].flatMap(c => defer { val st = c.createStatement(); try st.execute(sql) finally st.close(); pure(()) })

  /** one value of a query on the bracket's connection */
  def queryInt[R <: Row](sql: String)(using In[Reader % Connection, R]): Int ! R =
    Tx.resource[Connection, R].flatMap(c => defer {
      val st = c.createStatement()
      try { val rs = st.executeQuery(sql); rs.next(); pure(rs.getInt(1)) } finally st.close()
    })
