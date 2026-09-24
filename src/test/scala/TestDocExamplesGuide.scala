package okay

import okay.Row.{at, plus}

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
    def tracked[A, F[+_]](p: A ! (Users + F)): A ! (State % Store + Writer % String + F) =
      type R = State % Store + Writer % String + F
      !.interpret(p):
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
  def stored[A, F[+_]](p: A ! (Users + F)): A ! (State % Store + F) =
    type R = State % Store + F
    !.interpret(p):
      [X] => (e: Users[X]) => e match
        case Find(id) => State.get[Store].at[R].map(_.get(id))
        case Save(id, name) => State.get[Store].at[R].flatMap(s => State.set(s + (id -> name)).at[R]).map(_ => ())

  object viaTracing:
    def tracked[A, F[+_]](p: A ! (Users + F)): A ! (State % Store + Writer % String + F) =
      stored[A, Writer % String + F](!.tracing(p)([X] => (e: Users[X]) => e.toString))

class TestDocExamplesGuide extends munit.FunSuite:
  import GuideUsers.*

  val prog: Option[String] ! Users =
    Users.save(1, "ada").flatMap(_ => Users.find(1))

  test("guide: the Users signature, handled two ways, answers alike") {
    def runBoth(p: Option[String] ! (State % Store + Writer % String + okay.Pure)) =
      !.run(Writer.run(State.handle[Store](Map.empty)(p)))
    val (logA, (_, a)) = runBoth(viaInterpret.tracked[Option[String], okay.Pure](prog))
    val (logB, (_, b)) = runBoth(viaTracing.tracked[Option[String], okay.Pure](prog))
    assertEquals((a, b), (Some("ada"), Some("ada")))
    assertEquals(logA, Seq("save(1)", "find(1)"))
    assertEquals(logB, Seq("Save(1,ada)", "Find(1)"))
  }

  test("guide: Reader.local overrides ask for one block") {
    val p = Reader.local[Int, Int, okay.Pure](_ * 10)(Reader.ask[Int])
    assertEquals(!.run(Reader.run(5)(p)), 50)
  }

  def bump(by: Int): Int ! State % Int =
    for
      n <- State.get[Int]
      _ <- State.set(n + by)
    yield n

  test("guide: one function, two tagged states") {
    type Small = Tag.Of["small", State % Int]
    type Big   = Tag.Of["big",   State % Int]

    val twice: (Int, Int) ! (Small + Big) =
      for
        a <- Tag.tag["small", State % Int](bump(1)).plus[Big]
        b <- Tag.tag["big",   State % Int][Int, okay.Pure](bump(10)).at[Small + Big]
      yield (a, b)

    val afterSmall = State.handle[Int](1)(Tag.untag["small", State % Int](twice))
    val (big, (small, answer)) = !.run(State.handle[Int](100)(Tag.untag["big", State % Int](afterSmall)))
    assertEquals((answer, small, big), ((1, 100), 2, 110))
  }
