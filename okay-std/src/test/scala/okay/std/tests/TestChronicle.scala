package okay.std.tests

import okay.*
import okay.std.*

import okay.std.Chronicle.Verdict.*

/** specs/core-gaps.md stage 3: Chronicle, errors that accumulate */
class TestChronicle extends munit.FunSuite {
  type C = Chronicle % String
  def port(s: String): Int ! C = s.toIntOption.fold(Chronicle.confess[String, Int](s"port: '$s' is not a number"))(pure)
  def host(s: String): String ! C =
    if s.contains("_") then Chronicle.dictate(s"host: '$s' has an underscore").map(_ => s) else pure(s)

  test("nothing recorded is Clean, recorded and finished is Warned") {
    assertEquals(Chronicle.run(host("db")).run, Clean("db"))
    val both = for h <- host("my_db"); p <- port("5432") yield s"$h:$p"
    assertEquals(Chronicle.run(both).run, Warned("my_db:5432", Vector("host: 'my_db' has an underscore")))
    val two = for a <- host("a_1"); b <- host("b_2") yield a + b
    assertEquals(Chronicle.run(two).run, Warned("a_1b_2", Vector("host: 'a_1' has an underscore", "host: 'b_2' has an underscore")))
  }

  test("confess stops: what follows does not run, and every recorded error is kept") {
    val p = for h <- host("my_db"); p <- port("x"); q <- host("never_run") yield s"$h:$p:$q"
    assertEquals(Chronicle.run(p).run,
      Failed(Vector("host: 'my_db' has an underscore", "port: 'x' is not a number")))
    assertEquals(Chronicle.run(Chronicle.halt[String, Int]).run, Failed(Vector()))
  }

  test("all: one element's halt does not stop the next, and every error comes out in order") {
    val ports = Chronicle.all[String, String, Int, C](List("80", "x", "443", "y"))(port)
    assertEquals(Chronicle.run(ports).run,
      Failed(Vector("port: 'x' is not a number", "port: 'y' is not a number")))
    val hosts = Chronicle.all[String, String, String, C](List("a", "b_c", "d"))(host)
    assertEquals(Chronicle.run(hosts).run, Warned(Vector("a", "b_c", "d"), Vector("host: 'b_c' has an underscore")))
  }

  test("a row holds Chronicle AND Throws, and each handler answers its own") {
    type R = C + Throws % Int
    val p: String ! R = for h <- host("my_db").at[R]; code <- raise[Int, Int](503).at[R] yield s"$h$code"
    assertEquals(runEither(Chronicle.run(p)).run, Left(503))
    val q: String ! R = for h <- host("my_db").at[R]; n <- pure[Int, R](1) yield s"$h$n"
    assertEquals(runEither(Chronicle.run(q)).run, Right(Warned("my_db1", Vector("host: 'my_db' has an underscore"))))
  }

  test("stack-safe over 100 000 dictates") {
    val n = 100000
    val p = (1 to n).foldLeft(pure[Int, C](0))((acc, i) => acc.flatMap(s => Chronicle.dictate(i.toString).map(_ => s + 1)))
    Chronicle.run(p).run match
      case Warned(count, es) => assertEquals((count, es.size, es.last), (n, n, n.toString))
      case other => fail(s"expected Warned, got ${other.getClass}")
  }
}
