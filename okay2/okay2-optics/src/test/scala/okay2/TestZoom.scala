package okay2

import okay2.Optic._
import OpticsFixtures._

/** a program over a PART of the state runs over the whole, and the
 * four-parameter lens zooms the parameterised state — the Scala 3
 * core's okay-optics TestZoom */
class TestZoom extends munit.FunSuite {

  val n = Lens[Counter](_.n)
  val counter = Lens[App](_.counter)
  val appN = counter.andThen(n)

  /** a program written knowing only about an Int */
  def tick(by: Int): Int ! State[Int] =
    for {
      cur <- State.get[Int]
      _ <- State.set(cur + by)
      out <- State.get[Int]
    } yield out

  test("zoom: a program over the part runs over the whole, and touches nothing else") {
    val app = App(Counter(1, "hits"), "ada")
    val (after, out) = State.run(app)(State.zoom[App, Int, Int, Pure](appN)(tick(41)))
    assertEquals(out, 42)
    assertEquals(after, App(Counter(42, "hits"), "ada"))
    assertEquals(after.counter.label, app.counter.label)
    assertEquals(after.user, app.user)
  }

  test("zoom composes with the optic, not with itself: one lens, two levels") {
    val app = App(Counter(0, "x"), "bob")
    val prog = State.zoom[App, Int, Int, Pure](counter.andThen(n))(tick(5))
    assertEquals(State.run(app)(prog)._1, App(Counter(5, "x"), "bob"))
    val label = counter.andThen(Lens[Counter](_.label))
    val rename: String ! State[String] = State.set("hits")
    assertEquals(State.run(app)(State.zoom[App, String, String, Pure](label)(rename))._1, App(Counter(0, "hits"), "bob"))
  }

  test("zoom leaves the rest of the row alone: a Writer beside the State passes through") {
    val app = App(Counter(0, "x"), "ada")
    val prog: Free[State[Int] with Writer[String], Int] =
      Writer.tell("before").flatMap(_ => tick(7)).flatMap(v => Writer.tell(s"after $v").map(_ => v))
    val zoomed: Free[State[App] with Writer[String], Int] = State.zoom[App, Int, Int, Writer[String]](appN)(prog)
    val (told, (after, v)) = !.run(Writer.run(State.handle(app)(zoomed)))
    assertEquals(v, 7)
    assertEquals(after, App(Counter(7, "x"), "ada"))
    assertEquals(told.toList, List("before", "after 7"))
  }

  test("zoom of a program that does nothing to the state is the identity on it") {
    val app = App(Counter(3, "x"), "ada")
    val p: Int ! State[Int] = pure(9)
    val (after, v) = State.run(app)(State.zoom[App, Int, Int, Pure](appN)(p))
    assertEquals(v, 9)
    assertEquals(after, app)
  }

  test("PState.zoom: the four-parameter lens zooms the parameterised state — the type CHANGES with it") {
    val item: Lens[Box[String], Box[Int], String, Int] = Lens[Box[String], Box[Int], String, Int](_.item, (b, i) => Box(i, b.tag))
    def parse[R]: Cont[Int, Int => R, String => R] =
      PState.get[String, R].flatMap(s => PState.set[String, Int, R](s.length).map(_ => s.length))
    val (after, out) = PState.run[Box[String], Box[Int], Int](Box("hello", "t"))(
      PState.zoom[Box[String], Box[Int], String, Int, Int, (Box[Int], Int)](item)(parse))
    assertEquals(out, 5)
    assertEquals(after, Box(5, "t"))
  }

  test("PState.zoom: misusing the state's new type does not compile") {
    val e = compileErrors("""
      import okay2.Optic._
      import okay2.OpticsFixtures.Box
      val item: Lens[Box[String], Box[Int], String, Int] = Lens[Box[String], Box[Int], String, Int](_.item, (b, i) => Box(i, b.tag))
      def parse[R]: okay2.Cont[Int, Int => R, String => R] =
        okay2.PState.get[String, R].flatMap(s => okay2.PState.set[String, Int, R](s.length).map(_ => s.length))
      okay2.PState.run[Box[String], Box[String], Int](Box("hello", "t"))(
        okay2.PState.zoom[Box[String], Box[Int], String, Int, Int, (Box[String], Int)](item)(parse))
    """)
    assert(e.nonEmpty, "the misused typestate compiled")
  }
}
