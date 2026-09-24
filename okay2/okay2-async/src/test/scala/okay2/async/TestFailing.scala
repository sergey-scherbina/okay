package okay2.async

import okay2._

/**
 * A forwarded Async operation that fails OUT THERE still releases the
 * Resource scope that forwarded it: `Failing` is the hook, and the row
 * instance finds the Async operation by class at any nesting.
 */
class TestFailing extends munit.FunSuite {

  def outcome[A](f: scala.concurrent.Future[A]): Option[scala.util.Try[A]] = f.value

  test("a forwarded Async.Run that THROWS still releases") {
    var log = List.empty[String]
    type F = Resource + Async
    val prog = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close").at[F]
      .flatMap(_ => Async[Int] { log ::= "run"; throw new RuntimeException("boom") }.at[F])
    val out = outcome(Async.runAsync(Resource.run[Int, F, Async](prog)))
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "run", "close"))
  }

  test("a forwarded Async.Await whose callback answers Left still releases") {
    var log = List.empty[String]
    type F = Resource + Async
    val prog = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close").at[F]
      .flatMap(_ => Async.await[Int](k => { k(Left(new RuntimeException("no"))); () => () }).at[F])
    val out = outcome(Async.runAsync(Resource.run[Int, F, Async](prog)))
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "close"))
  }

  test("a ROW: Async + Throws — the Async half is found through the row instance and a throwing Run still releases") {
    var log = List.empty[String]
    type G = Throws[String]
    type F = Resource + (Async + G)
    val prog = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close").at[F]
      .flatMap(_ => Async[Int] { log ::= "run"; throw new RuntimeException("boom") }.at[F])
    val out = outcome(Async.runAsync(Throws.runEither[Int, String, Async + G](Resource.run[Int, F, Async + G](prog))))
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "run", "close"))
  }

  test("a ROW with Async on the RIGHT, nested: still guarded") {
    var log = List.empty[String]
    type G = Throws[String] + (State[Int] + Async)
    type F = Resource + G
    val prog = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close").at[F]
      .flatMap(_ => Async[Int] { log ::= "run"; throw new RuntimeException("boom") }.at[F])
    val handled = State.handle[Int, Either[String, Int], State[Int] + Async](0)(
      Throws.runEither[Int, String, G](Resource.run[Int, F, G](prog)))
    val out = outcome(Async.runAsync(handled))
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "run", "close"))
  }

  test("the happy path releases exactly once, after the forwarded operation") {
    var log = List.empty[String]
    type F = Resource + Async
    val prog = Resource.acquire { log ::= "open"; 1 } (_ => log ::= "close").at[F]
      .flatMap(a => Async[Int] { log ::= "run"; a + 41 }.at[F])
    val out = outcome(Async.runAsync(Resource.run[Int, F, Async](prog)))
    assertEquals(out.flatMap(_.toOption), Some(42))
    assertEquals(log.reverse, List("open", "run", "close"))
  }
}
