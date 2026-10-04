package okay2.jdbc

import okay2.{!, Pure, Resource, Throws}
import okay2.stream.ChunkBuf
import JdbcInterop._
import java.sql.DriverManager

/** H2 in memory (okay-jdbc's TestJdbcInterop): chunked queries, batched
 * writes, the Resource region. */
class TestJdbcInterop extends munit.FunSuite {

  def withDb[A](f: java.sql.Connection => A): A = {
    val c = DriverManager.getConnection("jdbc:h2:mem:t;DB_CLOSE_DELAY=-1")
    try f(c)
    finally c.close()
  }

  test("a query streams fetch-size rows per chunk, in order") {
    withDb { c =>
      Run(execute(c, "create table nums(n int)"))
      val rows = ChunkBuf.of(1 to 250)
      val inserted = Run(batch(c, "insert into nums values (?)")((ps, n: Int) => ps.setInt(1, n))(rows))
      assertEquals(inserted, 250)

      val all = Run.chunks(query(c, "select n from nums order by n", 64)(_.getInt(1)))
      assertEquals(all.map(_.length), List(64, 64, 64, 58))
      assertEquals(all.flatten, (1 to 250).toList)
      Run(execute(c, "drop table nums"))
    }
  }

  test("the Resource region closes the connection on a handled abort") {
    var conn: java.sql.Connection = null
    val prog: Int ! (Throws[String] with Resource) =
      connection("jdbc:h2:mem:r2").flatMap[Throws[String] with Resource, Int] { c =>
        conn = c
        Throws.raise[String, Int]("boom")
      }
    val out = !.run(Resource.run[Either[String, Int], Pure](Throws.runEither[Int, String, Resource](prog)))
    assertEquals(out, Left("boom"))
    assert(conn.isClosed, "the region must close the connection after the abort")
  }
}
