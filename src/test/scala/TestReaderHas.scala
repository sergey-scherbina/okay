package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * Reading the environment BY THE TYPE READ (reader-read,
 * 2026-09-16): `Reader.read[E, T]` is `ask` with a projection, so the
 * row holds ONE Reader, `Reader.run` handles it, nothing casts, and a
 * type the environment does not hold does not compile.
 */
class TestReaderHas extends munit.FunSuite {

  case class Users(byToken: Map[String, String])
  case class Feeds(byUser: Map[Int, List[String]])
  case class Clock(nowMs: Long)

  type Env = (Users, Feeds, Clock)
  def read[T](using Reader.Has[Env, T]): T ! Reader % Env = Reader.read[Env, T]

  val env: Env = (Users(Map("a" -> "Ada")), Feeds(Map(1 -> List("x", "y"))), Clock(7L))

  test("each part of the environment is read by its type") {
    val prog: String ! Reader % Env = direct:
      val u = !read[Users]
      val f = !read[Feeds]
      val c = !read[Clock]
      s"${u.byToken("a")} ${f.byUser(1).size} ${c.nowMs}"
    assertEquals(!.run(Reader.run(env)(prog)), "Ada 2 7")
  }

  test("a component declares only what it reads, and runs in ANY environment holding it") {
    def banner[E](using Reader.Has[E, Users]): String ! Reader % E = direct:
      s"hello ${(!Reader.read[E, Users]).byToken("a")}"
    assertEquals(!.run(Reader.run(env)(banner[Env])), "hello Ada")
    // the same component, a smaller environment
    type Small = (Clock, Users)
    val small: Small = (Clock(1L), Users(Map("a" -> "Bob")))
    assertEquals(!.run(Reader.run(small)(banner[Small])), "hello Bob")
  }

  test("the environment itself, and a product's fields") {
    assertEquals(!.run(Reader.run(env)(Reader.read[Env, Env])), env)
    case class Db(users: Users, clock: Clock)
    val db = Db(Users(Map("a" -> "Cleo")), Clock(3L))
    assertEquals(!.run(Reader.run(db)(Reader.read[Db, Clock])), Clock(3L))
  }

  test("a type the environment does not hold does not compile") {
    val e = compileErrors("okay.Reader.read[(Int, Long), String]")
    assert(e.nonEmpty, "reading a String out of (Int, Long) compiled")
  }

  test("a mark on a program of a narrower row, and colourless too") {
    type Row = Writer % String + Reader % Env + State % Long
    val prog: Long ! Row = direct:
      "seen".tell                              // an operation of the row
      val u = !read[Users]                     // a program at Reader % Env
      val c: Clock = read[Clock]               // the same, colourless
      !State.modify[Long](_ + u.byToken.size + c.nowMs)
    val (log, out) = !.run(Reader.run(env)(Writer.run[String, (Long, Long), Reader % Env](
      State.handle[Long](0L)(prog))))
    assertEquals(log, Seq("seen"))
    assertEquals(out, (8L, 8L))
  }
}
