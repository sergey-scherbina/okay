package okay


import okay.std.*
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
    assertEquals((Reader.run(env)(prog)).run, "Ada 2 7")
  }

  test("a component declares only what it reads, and runs in ANY environment holding it") {
    def banner[E](using Reader.Has[E, Users]): String ! Reader % E = direct:
      s"hello ${(!Reader.read[E, Users]).byToken("a")}"
    assertEquals((Reader.run(env)(banner[Env])).run, "hello Ada")
    // the same component, a smaller environment
    type Small = (Clock, Users)
    val small: Small = (Clock(1L), Users(Map("a" -> "Bob")))
    assertEquals((Reader.run(small)(banner[Small])).run, "hello Bob")
  }

  test("the environment itself, and a product's fields") {
    assertEquals((Reader.run(env)(Reader.read[Env, Env])).run, env)
    case class Db(users: Users, clock: Clock)
    val db = Db(Users(Map("a" -> "Cleo")), Clock(3L))
    assertEquals((Reader.run(db)(Reader.read[Db, Clock])).run, Clock(3L))
  }

  test("ask with no type argument takes the environment from the block's row") {
    type Row = Writer % String + Reader % Env + State % Long
    val prog: String ! Row = direct:
      val (users, feeds, clock) = !Reader.ask[Env]   // the std's ask names its environment (the row no longer infers it)
      "seen".tell
      s"${users.byToken("a")} ${feeds.byUser(1).size} ${clock.nowMs}"
    val (log, out) = (Reader.run(env)(Writer.run(
      State.handle[Long](0L)(prog)))).run
    assertEquals(log, Seq("seen"))
    assertEquals(out._2, "Ada 2 7")
  }

  test("ask with a type argument still works, in a block and outside one") {
    assertEquals((Reader.run(env)(Reader.ask[Env])).run, env)
    val inBlock: Users ! Reader % Env = direct { (!Reader.ask[Env])._1 }
    assertEquals((Reader.run(env)(inBlock)).run, env._1)
  }

  test("a type the environment does not hold does not compile") {
    val e = compileErrors("okay.std.Reader.read[(Int, Long), String]")
    assert(e.nonEmpty, "reading a String out of (Int, Long) compiled")
  }

  test("a mark on a program of a narrower row, and colourless too") {
    type Row = Writer % String + Reader % Env + State % Long
    val prog: Long ! Row = direct:
      "seen".tell                              // an operation of the row
      val u = !read[Users]                     // a program at Reader % Env
      val c: Clock = read[Clock]               // the same, colourless
      !State.modify[Long](_ + u.byToken.size + c.nowMs)
    val (log, out) = (Reader.run(env)(Writer.run(
      State.handle[Long](0L)(prog)))).run
    assertEquals(log, Seq("seen"))
    assertEquals(out, (8L, 8L))
  }
}
