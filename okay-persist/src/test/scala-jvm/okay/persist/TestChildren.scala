package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * A RUN THAT WAITS FOR ANOTHER RUN (workflow-children, 2026-09-17).
 *
 * The second test is the one that decides the shape: a child that
 * finishes BEFORE its parent reaches `awaitChild` must still be found
 * — the same "it may already be here" that signals needed, and for
 * the same reason. The fifth says what losing the registry costs:
 * parents keep waiting, and nothing is wrong.
 */
class TestChildren extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  def kid(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val v = !w.pause("child?")
      s"child:$v"

  /** the parent's first question is the SPAWN: an ordinary activity
   * that starts the child and answers with its id */
  def parent(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val id = !w.pause("start a child")
      val got = !w.awaitChild(id)
      s"parent got $got"

  def kidWorker(store: MemoryStore, kids: Option[Children]) =
    Worker[String, String, String, Pure, Async](store.topic("kids"), "child/1",
      Timers.over(store), _ => okay.async("v"), children = kids)(kid)

  def parentWorker(store: MemoryStore, kids: Option[Children], spawns: String = "k-1") =
    Worker[String, String, String, Pure, Async](store.topic("parents"), "parent/1",
      Timers.over(store), _ => okay.async(spawns), children = kids)(parent)

  test("the parent waits, the child finishes, the parent wakes with its result") {
    val store = MemoryStore()
    val kids = Children.over(store)
    val p = parentWorker(store, Some(kids))

    assertEquals(drive(p.start("p-1")), Worker.Progress.Waiting(Wf.Wait.Child("k-1")))
    // ...the parent's process may die here, holding nothing

    assertEquals(drive(kidWorker(store, Some(kids)).start("k-1")),
      Worker.Progress.Finished("child:v"))
    assertEquals(kids.resultOf("k-1"), Some("child:v"))

    assertEquals(drive(p.advance("p-1")), Worker.Progress.Finished("parent got child:v"))
  }

  test("a child that finished FIRST is found when the parent gets there") {
    val store = MemoryStore()
    val kids = Children.over(store)
    val _ = drive(kidWorker(store, Some(kids)).start("k-1"))

    // the parent has never run, and goes straight through
    assertEquals(drive(parentWorker(store, Some(kids)).start("p-1")),
      Worker.Progress.Finished("parent got child:v"))
  }

  test("two workers that both notice the finished child produce ONE journal") {
    val store = MemoryStore()
    val kids = Children.over(store)
    val a = parentWorker(store, Some(kids))
    val b = parentWorker(store, Some(kids))
    val _ = drive(a.start("p-1"))
    val _ = drive(kidWorker(store, Some(kids)).start("k-1"))

    assertEquals(drive(a.advance("p-1")), Worker.Progress.Finished("parent got child:v"))
    assertEquals(drive(b.advance("p-1")), Worker.Progress.Finished("parent got child:v"))

    val gots = a.dialogue("p-1").journal.count(_ == Left(Wf.SysA.Got("child:v")))
    assertEquals(gots, 1, s"the child's result was journalled twice: ${a.dialogue("p-1").journal}")
  }

  test("no registry: the parent keeps waiting, and nothing is wrong") {
    val store = MemoryStore()
    val p = parentWorker(store, None)
    assertEquals(drive(p.start("p-1")), Worker.Progress.Waiting(Wf.Wait.Child("k-1")))
    val before = p.dialogue("p-1").journal
    assertEquals(drive(p.advance("p-1")), Worker.Progress.Waiting(Wf.Wait.Child("k-1")))
    assertEquals(p.dialogue("p-1").journal, before, "a waiting parent gained an answer")
  }

  test("a run waiting on a CHILD is off the clock: time is not what wakes it") {
    val store = MemoryStore()
    val p = parentWorker(store, Some(Children.over(store)))
    val _ = drive(p.start("p-1"))
    assertEquals(Timers.over(store).armed, Map.empty[String, Long])
  }

  test("the tree view: who started whom, and which of them are done") {
    val store = MemoryStore()
    val kids = Children.over(store)
    kids.link("k-1", "p-1", "child/1")
    kids.link("k-2", "p-1", "child/1")
    kids.link("k-3", "other", "child/1")
    val _ = drive(kidWorker(store, Some(kids)).start("k-1"))

    val mine = kids.of("p-1").map((id, _, done) => (id, done)).sortBy(_._1)
    assertEquals(mine, List("k-1" -> Some("child:v"), "k-2" -> None))
    assertEquals(kids.parentOf("k-2").map(_.parent), Some("p-1"))
  }
}
