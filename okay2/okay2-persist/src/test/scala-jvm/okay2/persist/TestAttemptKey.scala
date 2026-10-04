package okay2.persist

import munit.FunSuite
import okay2.{!, Wf}
import okay2.async.{Async, Retry}
import okay2.platform._

/**
 * THE IDEMPOTENCY KEY REACHES THE ORACLE (okay-persist's TestAttemptKey;
 * worker-oracle-attempt): a question re-asked after a failure carries
 * the SAME position, because the position is the journal's own and
 * nothing was journalled. (Scala 3 hands the `Attempt` over as context;
 * here it is the oracle's second argument.)
 */
class TestAttemptKey extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  def two(w: W): String ! Rw = for {
    a <- w.pause("1?")
    b <- w.pause("2?")
  } yield s"$a$b"

  val xx: Worker.Progress[String] = Worker.Progress.Finished("xx")

  test("an oracle can ask where it is, and the answer is the journal's position") {
    val store = new MemoryStore
    var seen = List.empty[(String, String, Int)]
    val w = new Worker[String, String, String, P, Async](store.topic("keys"), "two/1", Timers.over(store),
      (q, at) => Async { seen = seen :+ ((q, at.id, at.index)); "x" })(two)

    assertEquals(drive(w.start("k-1")), xx)
    assertEquals(seen, List(("1?", "k-1", 0), ("2?", "k-1", 1)))
  }

  test("THE POINT: a question re-asked after a failure carries the SAME position") {
    val store = new MemoryStore
    val t = store.topic("keys")
    var seen = List.empty[Int]
    var down = true

    def worker = new Worker[String, String, String, P, Async](t, "two/1", Timers.over(store),
      (q, at) => Async {
        if (q == "2?") {
          seen = seen :+ at.index
          if (down) throw new RuntimeException("the service is down")
        }
        "x"
      })(two)

    intercept[RuntimeException](drive(worker.start("k-1")))
    assertEquals(seen, List(1))
    assertEquals(worker.dialogue("k-1").journal, List(Right("x")))

    down = false
    assertEquals(drive(worker.advance("k-1")), xx)
    assertEquals(seen, List(1, 1), "the re-ask carried a different position, so it is not a key")
  }

  test("every attempt at ONE question carries one position, retries included") {
    val store = new MemoryStore
    var seen = List.empty[Int]
    var left = 2
    val w = new Worker[String, String, String, P, Async](store.topic("keys"), "two/1", Timers.over(store),
      Worker.retrying(Retry.immediate(5))((q, at) => Async {
        if (q == "2?") {
          seen = seen :+ at.index
          if (left > 0) { left -= 1; throw new RuntimeException("flaky") }
        }
        "x"
      }))(two)

    assertEquals(drive(w.start("k-1")), xx)
    assertEquals(seen, List(1, 1, 1), "the retries did not share one key")
  }
}
