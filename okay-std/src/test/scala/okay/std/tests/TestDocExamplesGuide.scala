package okay.std.tests

import okay.*
import okay.given
import scala.language.implicitConversions
import okay.std.*

import okay.core.Split

/**
 * docs/guide.md's core examples, VERBATIM (doc-snippet-debt): each
 * line as the page prints it, answer comment included, then asserted.
 * The stream examples are okay-stream's TestDocExamplesFoldUntil and
 * the generator ones okay-direct's TestDocExamplesGen.
 */
object GuideUsers:

  enum Users[+A] derives Effect:
    case Find(id: Long) extends Users[Option[String]]
    case Save(id: Long, name: String) extends Users[Unit]

  object Users:                       // optional: `Users.Find(id).perform`
    inline def find(id: Long): Option[String] ! Users = effect(Find(id))
    inline def save(id: Long, name: String): Unit ! Users = effect(Save(id, name))

  import Users.*

  type Store = Map[Long, String]

  object viaInterpret:
    def tracked[A, Es[_]](p: A ! Es)(using s: Split[Es, Users]): A ! (State % Store + Writer % String + s.Rest) =
      type R = State % Store + Writer % String
      p.translate[Users, R]:
        [X] => (e: Users[X]) => e match
          case Find(id) =>
            for
              store <- State.get[Store].at[R]
              _     <- Writer.tell(s"find($id)").at[R]
            yield store.get(id)
          case Save(id, name) =>
            for
              store <- State.get[Store].at[R]
              _     <- Writer.tell(s"save($id)").at[R]
              _     <- State.set(store + (id -> name)).at[R]
            yield ()

  /** the storage half the page names and does not print */
  def stored[A, Es[_]](p: A ! Es)(using s: Split[Es, Users]): A ! (State % Store + s.Rest) =
    p.translate[Users, State % Store]:
      [X] => (e: Users[X]) => e match
        case Find(id) => State.get[Store].map(_.get(id))
        case Save(id, name) => State.get[Store].flatMap(s => State.set(s + (id -> name))).map(_ => ())

  object viaTracing:
    def tracked[A, Es[_]](p: A ! Es)(using s: Split[Es, Users]): A ! (State % Store + Writer % String + s.Rest) =
      stored(Writer.tracing[A, Users, String, Es](p)([X] => (e: Users[X]) => e.toString))

class TestDocExamplesGuide extends munit.FunSuite:
  import GuideUsers.*

  val prog: Option[String] ! Users =
    Users.save(1, "ada").flatMap(_ => Users.find(1))

  test("guide: the Users signature, handled two ways, answers alike") {
    def runBoth(p: Option[String] ! (State % Store + Writer % String)) =
      Writer.run(State.handle[Store](Map.empty)(p)).run
    val (logA, (_, a)) = runBoth(viaInterpret.tracked(prog))
    val (logB, (_, b)) = runBoth(viaTracing.tracked(prog))
    assertEquals((a, b), (Some("ada"), Some("ada")))
    assertEquals(logA, Seq("save(1)", "find(1)"))
    assertEquals(logB, Seq("Save(1,ada)", "Find(1)"))
  }

  test("guide: Reader.local overrides ask for one block") {
    val p = Reader.local[Int, Int, Reader % Int](_ * 10)(Reader.ask[Int])
    assertEquals(Reader.run(5)(p).run, 50)
  }


  test("guide: !.each and !.foldM, a loop that performs on every element") {
    type W = Writer % String
    locally {
      val p: Unit ! W = !.each(List("a", "b", "c"))(s => Writer.tell(s))
      assertEquals(Writer.run(p).run, (Seq("a", "b", "c"), ()))
    }
    locally {
      val p: Int ! State % Int = !.foldM(1 to 3)(0)((acc, x) => State.modify[Int](_ + x).map(acc + _))
      assertEquals(State.handle[Int](0)(p).run._1, 6)
    }
  }
