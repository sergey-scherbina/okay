package okay.jdbc

import okay.{!, %, +, Async, Chunk, Chunks, Produce, Throws, effect}
import okay.given
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
      !.run(okay.Async.run[Unit, Nothing](execute(c, "create table nums(n int)")))
      val rows = Chunks.wrap[Int]((1 to 250).map(Integer.valueOf(_): AnyRef).toArray)
      val inserted = !.run(okay.Async.run[Int, Nothing](
        batch(c, "insert into nums values (?)")((ps, n: Int) => ps.setInt(1, n))(rows)))
      assertEquals(inserted, 250)

      val all = collectChunks(query(c, "select n from nums order by n", 64)(_.getInt(1)))
      assertEquals(all.map(_.length), List(64, 64, 64, 58))
      assertEquals(all.flatten, (1 to 250).toList)
      !.run(okay.Async.run[Unit, Nothing](execute(c, "drop table nums")))
    }
  }

  /** drain the effectful chunked stream into a list of chunks */
  def collectChunks[A](s: Chunk[A] ! (Produce + Async)): List[Chunk[A]] =
    import okay.!.*
    def go(rest: Chunk[A] ! (Produce + Async), acc: List[Chunk[A]]): List[Chunk[A]] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc.reverse
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => summon[okay.Handler[Async]].handle(a); acc.reverse
          case Right(c) => (c.asInstanceOf[Chunk[A]] :: acc).reverse
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => go(k(summon[okay.Handler[Async]].handle(a)), acc)
          case Right(c) => go(k(c), c.asInstanceOf[Chunk[A]] :: acc)
    go(s, Nil)

  test("the Resource region closes the connection on a handled abort") {
    type F = Throws % String + okay.Resource
    var conn: java.sql.Connection = null
    val prog2 = okay.!.widen[java.sql.Connection, okay.Resource, Throws % String](
      connection("jdbc:h2:mem:r2")).flatMap { c =>
      conn = c
      effect[F, Int](Throws("boom"))
    }
    val out = !.run(okay.Resource.run[Either[String, Int], Nothing](
      okay.runEither[Int, okay.Resource, String](prog2)))
    assertEquals(out, Left("boom"))
    assert(conn.isClosed, "the region must close the connection after the abort")
  }
}
