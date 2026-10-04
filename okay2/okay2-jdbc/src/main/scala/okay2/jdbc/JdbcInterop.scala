package okay2.jdbc

import java.sql.{Connection, DriverManager, PreparedStatement, ResultSet}
import okay2.{!, Resource, Writer, pure}
import okay2.async.Async
import okay2.stream.{Chunk, ChunkBuf, Source}

/**
 * JDBC as chunked async streams (okay-jdbc's JdbcInterop.scala,
 * specs/external-systems.md): a query streams its result set fetch-size
 * rows per chunk — constant memory for any result size; the connection
 * lives under the Resource region; writes go a chunk per batch.
 */
object JdbcInterop {

  /** a connection under the Resource region */
  def connection(url: String, user: String = "", password: String = ""): Connection ! Resource =
    Resource.acquire(DriverManager.getConnection(url, user, password))(_.close())

  /**
   * A query as a chunked async stream: the statement opens at the first
   * pull, each chunk is up to fetchSize rows read inside one Async
   * operation, and the statement closes itself at exhaustion. (An
   * abandoned stream leaks the statement — consume it fully or scope
   * the CONNECTION, whose close closes its statements.)
   */
  def query[A](conn: Connection, sql: String, fetchSize: Int = 64)(f: ResultSet => A): Source[Chunk[A]] = {
    type W = Chunk[A]

    def readChunk(rs: ResultSet): W = {
      val buf = ChunkBuf[A](fetchSize)
      var i = 0
      while (i < fetchSize && rs.next()) {
        buf(i) = f(rs)
        i += 1
      }
      buf.take(i)
    }

    // each next chunk is deferred into the program's flatMap: the walk
    // over the result set holds no host stack (trampolined)
    def go(rs: ResultSet, st: java.sql.Statement): Source[W] =
      Async(readChunk(rs)).flatMap { (c: W) =>
        if (c.length < fetchSize)
          Async { rs.close(); st.close() }.flatMap { _ =>
            if (c.isEmpty) pure[Writer[W] with Async, Unit](()) else Writer.tell[W](c)
          }
        else Writer.tell[W](c).flatMap(_ => go(rs, st))
      }

    Async {
      val st = conn.createStatement()
      st.setFetchSize(fetchSize)
      (st.executeQuery(sql), st)
    }.flatMap[Writer[W] with Async, Unit] { case (rs, st) => go(rs, st) }
  }

  /** one chunk, one batch: bind each row, execute, count updates */
  def batch[A](conn: Connection, sql: String)(bind: (PreparedStatement, A) => Unit)(rows: Chunk[A]): Int ! Async =
    Async {
      val ps = conn.prepareStatement(sql)
      try {
        rows.foreach { a => bind(ps, a); ps.addBatch() }
        ps.executeBatch().sum
      } finally ps.close()
    }

  /** run a DDL/DML statement */
  def execute(conn: Connection, sql: String): Unit ! Async =
    Async {
      val st = conn.createStatement()
      try { st.execute(sql); () }
      finally st.close()
    }
}
