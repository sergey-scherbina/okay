package scala2probe

import okay.resilience.{Breaker, Bulkhead, Deadline, Limiter, Refused}
import okay.scala2._

import java.util.concurrent.atomic.AtomicInteger

/** okay-resilience from Scala 2.13 (specs/scala2-facade.md, stage 15.1) */
class TestResilienceFromScala2 extends munit.FunSuite {

  def outcome[A](e: Eff[Async, A]): Either[Throwable, A] = scala.util.Try(Eff.runAsync(e)).toEither

  test("a breaker opens after its failures and then refuses without running") {
    val b = new Breaker("pay", 2, 60000L)
    val runs = new AtomicInteger
    val failing = Async.delay[Int] { runs.incrementAndGet(); throw new IllegalStateException("down") }
    assert(outcome(Guards.breaker(b)(failing)).isLeft)
    assert(outcome(Guards.breaker(b)(failing)).isLeft)
    outcome(Guards.breaker(b)(failing)) match {
      case Left(r: Refused.BreakerOpen) => assertEquals(r.name, "pay")
      case other => fail("expected BreakerOpen, got " + other)
    }
    assertEquals(runs.get, 2)
  }

  test("a breaker can count a returned value as a failure") {
    val b = new Breaker("http", 1, 60000L)
    assert(outcome(Guards.breaker(b)(Async.delay(503), (r: Either[Throwable, Int]) => r.exists(_ >= 500))).isRight)
    assert(outcome(Guards.breaker(b)(Async.delay(200))).left.exists(_.isInstanceOf[Refused.BreakerOpen]))
  }

  test("a bulkhead of one refuses a second call while the first is in flight") {
    val b = new Bulkhead("db", 1, 0)
    // no sleeps: the first call says when it holds the permit, and the
    // second is tried only then, so the refusal cannot depend on timing
    val inside = new java.util.concurrent.CountDownLatch(1)
    val gate = new java.util.concurrent.CountDownLatch(1)
    val prog = for {
      first <- Async.fork(Guards.bulkhead(b)(Async.delay { inside.countDown(); gate.await(); 1 }))
      _ <- Async.delay(inside.await())
      second <- Async.delay(outcome(Guards.bulkhead(b)(Async.delay(2))))
      _ <- Async.delay(gate.countDown())
      one <- first.join
    } yield (one, second)
    val (one, second) = Eff.runAsync(prog)
    assertEquals(one, 1)
    assert(second.left.exists(_.isInstanceOf[Refused.BulkheadFull]), second.toString)
  }

  test("a limiter refuses past its burst, naming the key") {
    val l = new Limiter("api", 0.001, 1)
    assertEquals(outcome(Guards.limiter(l, "alice")(Async.delay(1))), Right(1))
    outcome(Guards.limiter(l, "alice")(Async.delay(2))) match {
      case Left(r: Refused.Exhausted) => assertEquals(r.key, "alice")
      case other => fail("expected Exhausted, got " + other)
    }
    assertEquals(outcome(Guards.limiter(l, "bob")(Async.delay(3))), Right(3))
  }

  test("retry runs again until it succeeds, and answers the last failure when the policy runs out") {
    val calls = new AtomicInteger
    val flaky = Async.delay { if (calls.incrementAndGet() < 3) throw new IllegalStateException("not yet") else "ok" }
    assertEquals(Eff.runAsync(Guards.retry(okay.Retry.immediate(5))(flaky)), "ok")
    assertEquals(calls.get, 3)
    val never = Async.delay[String](throw new IllegalStateException("never"))
    assert(outcome(Guards.retry(okay.Retry.immediate(2))(never)).left.exists(_.getMessage == "never"))
  }

  test("hedge: a slow first attempt is overtaken by a second") {
    val attempts = new AtomicInteger
    val prog = Async.delay(attempts.incrementAndGet()).flatMap { n =>
      if (n == 1) Async.sleep(5000).map(_ => "slow") else Eff.pure[String]("fast")
    }
    assertEquals(Eff.runAsync(Guards.hedge(50)(prog)), "fast")
  }

  test("a deadline cancels a run that outlives it") {
    val slow = Async.sleep(5000).map(_ => "late")
    assert(outcome(Guards.deadline(Deadline.in(50))(slow)).left.exists(_.isInstanceOf[Refused.DeadlineExceeded]))
    assertEquals(Eff.runAsync(Guards.deadline(Deadline.in(5000))(Async.delay("on time"))), "on time")
  }
}
