package okay.jdbc

import okay.{Async, Source}

import okay.freer.{+, %}
import okay.freer.{!, Chunk, effect, Stream, Throws, Writer}
import okay.given
import okay.freer.given
import JdbcInterop.*
import java.sql.DriverManager

/** H2 in memory: chunked queries, batched writes, the Resource region. */
class TestJdbcInterop extends munit.FunSuite {

  def withDb[A](f: java.sql.Connection => A): A =
    val c = DriverManager.getConnection("jdbc:h2:mem:t;DB_CLOSE_DELAY=-1")
    try f(c)
    finally c.close()

  test("a query streams fetch-size rows per chunk, in order") {
    withDb { c =>
      !.run(okay.Async.run[Unit, okay.freer.Pure](execute(c, "create table nums(n int)")))
      val rows = okay.ChunkBuf.of(1 to 250)
      val inserted = !.run(okay.Async.run[Int, okay.freer.Pure](
        batch(c, "insert into nums values (?)")((ps, n: Int) => ps.setInt(1, n))(rows)))
      assertEquals(inserted, 250)

      val all = collectChunks(query(c, "select n from nums order by n", 64)(_.getInt(1)))
      assertEquals(all.map(_.length), List(64, 64, 64, 58))
      assertEquals(all.flatten, (1 to 250).toList)
      !.run(okay.Async.run[Unit, okay.freer.Pure](execute(c, "drop table nums")))
    }
  }

  /** drain the effectful chunked stream into a list of chunks */
  def collectChunks[A](s: Source[Chunk[A]]): List[Chunk[A]] =
    summon[Stream[[W] =>> Unit ! Writer % W + Async, Async]].iterator(s).toList

  test("the Resource region closes the connection on a handled abort") {
    type F = Throws % String + okay.freer.Resource
    var conn: java.sql.Connection = null
    val prog2 = okay.freer.!.widen[java.sql.Connection, okay.freer.Resource, Throws % String](
      connection("jdbc:h2:mem:r2")).flatMap { c =>
      conn = c
      effect[F, Int](Throws("boom"))
    }
    val out = !.run(okay.freer.Resource.run[Either[String, Int], okay.freer.Pure](
      okay.freer.runEither[Int, okay.freer.Resource, String](prog2)))
    assertEquals(out, Left("boom"))
    assert(conn.isClosed, "the region must close the connection after the abort")
  }
}
