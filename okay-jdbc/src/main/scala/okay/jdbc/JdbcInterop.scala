package okay.jdbc

import okay.{!, +, Async, Chunk, Chunks, Produce, async, effect}
import java.sql.{Connection, DriverManager, PreparedStatement, ResultSet}

/**
 * JDBC as chunked async streams (specs/external-systems.md): a query
 * streams its result set fetch-size rows per chunk — constant memory
 * for any result size; the connection lives under the Resource region
 * (released on the scope's end, on handled aborts, on exceptions);
 * writes go a chunk per batch.
 */
object JdbcInterop {

  /** a connection under the Resource region */
  def connection(url: String, user: String = "", password: String = "")
  : Connection ! okay.Resource =
    okay.Resource.acquire(DriverManager.getConnection(url, user, password))(_.close())

  /**
   * A query as a chunked async stream: the statement opens at the
   * first pull, each chunk is up to fetchSize rows read inside one
   * Async operation, and the statement closes itself at exhaustion.
   * (An abandoned stream leaks the statement — consume it fully or
   * scope the CONNECTION, whose close closes its statements.)
   */
  def query[A](conn: Connection, sql: String, fetchSize: Int = 64)(f: ResultSet => A)
  : Chunk[A] ! (Produce + Async) =
    type F = Produce + Async

    def readChunk(rs: ResultSet): Chunk[A] =
      val buf = okay.ChunkBuf[A](fetchSize)
      var i = 0
      while i < fetchSize && rs.next() do
        buf(i) = f(rs)
        i += 1
      buf.take(i)

    def go(rs: ResultSet, st: java.sql.Statement): Chunk[A] ! F =
      effect[F, Chunk[A]](Async.Run(() => readChunk(rs))).flatMap { c =>
        if c.length < fetchSize then
          effect[F, Unit](Async.Run { () => rs.close(); st.close() }).flatMap { _ =>
            if c.isEmpty then okay.pure(Chunks.emptyChunk)
            else effect[F, Chunk[A]](c)
          }
        else effect[F, Chunk[A]](c).flatMap(_ => go(rs, st))
      }

    effect[F, (ResultSet, java.sql.Statement)](Async.Run { () =>
      val st = conn.createStatement()
      st.setFetchSize(fetchSize)
      (st.executeQuery(sql), st)
    }).flatMap(go)

  /** one chunk, one batch: bind each row, execute, count updates */
  def batch[A](conn: Connection, sql: String)(bind: (PreparedStatement, A) => Unit)
              (rows: Chunk[A]): Int ! Async =
    async {
      val ps = conn.prepareStatement(sql)
      try
        rows.foreach { a => bind(ps, a); ps.addBatch() }
        ps.executeBatch().sum
      finally ps.close()
    }

  /** run a DDL/DML statement */
  def execute(conn: Connection, sql: String): Unit ! Async =
    async {
      val st = conn.createStatement()
      try { st.execute(sql); () }
      finally st.close()
    }
}
