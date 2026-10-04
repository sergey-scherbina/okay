package okay2.persist

import okay2.{!, Pure, Wf}
import okay2.async.Async
import okay2.platform._

/** the JVM worker suites' fixtures: the activity row is `Async`, and
 * running it to a value needs `CanBlock`, which Scala.js does not have —
 * the model underneath stays cross-platform, these suites do not */
object WorkerFixtures {
  import DialogueFixtures._
  import WorkflowFixtures._

  /** the driver's row is NOT the program's: the workflow stays in the
   * replayable `Pure`, the ACTIVITIES live in `Async` */
  def drive[A](p: A ! Async): A = !.run(Async.run[A, Pure](p))

  /** an activity that answers `a`, in the activity row */
  def say(a: => String): (String, Dialogue.Attempt) => String ! Async = (_, _) => Async(a)

  /** answer, sleep a minute, finish */
  def nap(w: W): String ! Rw = for {
    who <- w.pause("who?")
    _ <- w.sleep(60000L)
  } yield s"$who woke"

  def worker(store: MemoryStore, t: Topic, answers: String = "ada")(implicit rt: Wf.Runtime): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](t, "nap/1", Timers.over(store), say(answers))(nap)
}
