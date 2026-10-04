package okay2.persist

import munit.FunSuite
import okay2.{!, Wf}
import okay2.async.Async

/**
 * A RUN THAT WAITS FOR ANOTHER RUN (okay-persist's TestChildren;
 * workflow-children): a child that finishes BEFORE its parent reaches
 * `awaitChild` must still be found; losing the registry costs only
 * waiting.
 */
class TestChildren extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  def kid(w: W): String ! Rw = w.pause("child?").map(v => s"child:$v")

  /** the parent's first question is the SPAWN: an ordinary activity that
   * starts the child and answers with its id */
  def parent(w: W): String ! Rw = for {
    id <- w.pause("start a child")
    got <- w.awaitChild(id)
  } yield s"parent got $got"

  def kidWorker(store: MemoryStore, kids: Option[Children]): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](store.topic("kids"), "child/1", Timers.over(store), say("v"), children = kids)(kid)

  def parentWorker(store: MemoryStore, kids: Option[Children], spawns: String = "k-1"): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](store.topic("parents"), "parent/1", Timers.over(store), say(spawns), children = kids)(parent)

  val waitingOnK1: Worker.Progress[String] = Worker.Progress.Waiting(Wf.Wait.Child("k-1"))
  val parentDone: Worker.Progress[String] = Worker.Progress.Finished("parent got child:v")

  test("the parent waits, the child finishes, the parent wakes with its result") {
    val store = new MemoryStore
    val kids = Children.over(store)
    val p = parentWorker(store, Some(kids))

    assertEquals(drive(p.start("p-1")), waitingOnK1)
    assertEquals(drive(kidWorker(store, Some(kids)).start("k-1")), Worker.Progress.Finished("child:v"): Worker.Progress[String])
    assertEquals(kids.resultOf("k-1"), Some("child:v"))
    assertEquals(drive(p.advance("p-1")), parentDone)
  }

  test("a child that finished FIRST is found when the parent gets there") {
    val store = new MemoryStore
    val kids = Children.over(store)
    val _ = drive(kidWorker(store, Some(kids)).start("k-1"))
    assertEquals(drive(parentWorker(store, Some(kids)).start("p-1")), parentDone)
  }

  test("two workers that both notice the finished child produce ONE journal") {
    val store = new MemoryStore
    val kids = Children.over(store)
    val a = parentWorker(store, Some(kids))
    val b = parentWorker(store, Some(kids))
    val _ = drive(a.start("p-1"))
    val _ = drive(kidWorker(store, Some(kids)).start("k-1"))

    assertEquals(drive(a.advance("p-1")), parentDone)
    assertEquals(drive(b.advance("p-1")), parentDone)

    val gots = a.dialogue("p-1").journal.count(_ == Left(Wf.SysA.Got("child:v")))
    assertEquals(gots, 1, s"the child's result was journalled twice: ${a.dialogue("p-1").journal}")
  }

  test("no registry: the parent keeps waiting, and nothing is wrong") {
    val store = new MemoryStore
    val p = parentWorker(store, None)
    assertEquals(drive(p.start("p-1")), waitingOnK1)
    val before = p.dialogue("p-1").journal
    assertEquals(drive(p.advance("p-1")), waitingOnK1)
    assertEquals(p.dialogue("p-1").journal, before, "a waiting parent gained an answer")
  }

  test("a run waiting on a CHILD is off the clock: time is not what wakes it") {
    val store = new MemoryStore
    val _ = drive(parentWorker(store, Some(Children.over(store))).start("p-1"))
    assertEquals(Timers.over(store).armed, Map.empty[String, Long])
  }

  test("the tree view: who started whom, and which of them are done") {
    val store = new MemoryStore
    val kids = Children.over(store)
    kids.link("k-1", "p-1", "child/1")
    kids.link("k-2", "p-1", "child/1")
    kids.link("k-3", "other", "child/1")
    val _ = drive(kidWorker(store, Some(kids)).start("k-1"))

    val mine = kids.of("p-1").map { case (id, _, done) => (id, done) }.sortBy(_._1)
    assertEquals(mine, List("k-1" -> Some("child:v"), "k-2" -> None))
    assertEquals(kids.parentOf("k-2").map(_.parent), Some("p-1"))
  }
}
