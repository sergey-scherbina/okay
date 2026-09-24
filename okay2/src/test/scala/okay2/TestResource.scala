package okay2

import Later.later
import Produce.produce

/** The resource region: releases at the end of the scope, no matter what. */
class TestResource extends munit.FunSuite {

  /** the test effect cannot fail on the outer handler, and says so */
  implicit val laterFailing: Failing[Later] = Failing.never[Later]

  def res(log: () => Unit, n: String, into: String => Unit): String ! Resource =
    Resource.acquire { into(s"open $n"); n } (r => into(s"close $r"))

  test("releases in reverse acquisition order at the end of the scope") {
    var log = List.empty[String]
    def res(n: String) = Resource.acquire { log ::= s"open $n"; n } (r => log ::= s"close $r")
    val prog = res("a").flatMap(a => res("b").map(b => a + b))
    assertEquals(Resource.scoped(prog), "ab")
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }

  test("an abort handled inside the scope still releases") {
    var released = false
    type F = Throws[String] + Resource
    val prog: Int ! F =
      Resource.acquire(())(_ => released = true).at[F].flatMap(_ => Throws.raise[String, Int]("boom").at[F])
    val either = Resource.scoped(Throws.runEither(prog))
    assertEquals(either, Left("boom"))
    assertEquals(released, true)
  }

  test("a JVM exception during a step still releases") {
    var released = false
    val prog: Int ! Resource = Resource.acquire(())(_ => released = true)
      .flatMap(_ => pure[Resource, Int](0).map(_ => throw new RuntimeException("boom")))
    val _ = intercept[RuntimeException](Resource.scoped(prog))
    assertEquals(released, true)
  }

  test("forwarded effects: finalizers travel with the residual") {
    var released = false
    type F = Resource + Later
    val prog: Int ! F =
      Resource.acquire(())(_ => released = true).at[F].flatMap(_ => later(41).at[F].map(_ + 1))
    val residual: Int ! Later = Resource.run(prog)
    assertEquals(released, false)
    assertEquals(residual.runWith, 42)
    assertEquals(released, true)
  }

  test("a throw in the continuation AFTER a forwarded effect still releases") {
    var released = false
    type F = Resource + Later
    val prog: Int ! F =
      Resource.acquire(())(_ => released = true).at[F].flatMap(_ => later(41).at[F].map(_ => throw new RuntimeException("boom")))
    val residual: Int ! Later = Resource.run(prog)
    assertEquals(released, false)
    val _ = intercept[RuntimeException](residual.runWith)
    assertEquals(released, true)
  }

  test("run finds Resource anywhere in the row: the residual is what is left") {
    var log = List.empty[String]
    type F = Later + Resource
    val prog: Int ! F =
      later { log ::= "before"; 1 }.at[F].flatMap(x =>
        Resource.acquire { log ::= "open"; x + 1 } (_ => log ::= "close").at[F]).flatMap(y =>
        later { log ::= "after"; y * 10 }.at[F])
    assertEquals(Resource.run(prog).runWith, 20)
    assertEquals(log.reverse, List("before", "open", "after", "close"))
  }

  test("open keeps the scope open; the closer releases in reverse order and is idempotent") {
    var log = List.empty[String]
    def res(n: String) = Resource.acquire { log ::= s"open $n"; n } (r => log ::= s"close $r")
    val (v, close) = Resource.open(res("a").flatMap(a => res("b").map(b => a + b)))
    assertEquals(v, "ab")
    assertEquals(log.reverse, List("open a", "open b"))
    close()
    close()
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }

  test("open: a throw during an acquisition releases what came before it, and rethrows") {
    var log = List.empty[String]
    def res(n: String) = Resource.acquire { log ::= s"open $n"; n } (r => log ::= s"close $r")
    val prog = res("a").flatMap(_ => Resource.acquire[String](throw new RuntimeException("no b"))(_ => ()))
    val _ = intercept[RuntimeException](Resource.open(prog))
    assertEquals(log.reverse, List("open a", "close a"))
  }

  test("bracket over any Handler-able row") {
    var released = 0
    assertEquals(bracket(41)(_ => released += 1)(r => later(r + 1)).runWith, 42)
    assertEquals(bracket(1)(_ => released += 1)(r => produce(r + 1)).runWith, 2)
    val _ = intercept[RuntimeException] {
      bracket(0)(_ => released += 1)(_ => later[Int](throw new RuntimeException("boom"))).runWith
    }
    assertEquals(released, 3)
  }

  test("a row with no Failing instance is refused, not silently unguarded") {
    val errors = compileErrors("Resource.run(Resource.acquire(1)(_ => ()).at[Resource + Produce])")
    assert(errors.contains("Failing"), errors)
  }
}
