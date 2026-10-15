package okay

import okay.{Cps}




import okay.std.*
import okay.given
/**
 * specs/optics.md stage 3: a program over a PART of the state runs
 * over the whole, and the four-parameter lens zooms the parameterised
 * state — which is where the optic and Atkey's state turn out to be
 * one picture (theory textbook ch. 3).
 */
class TestZoom extends munit.FunSuite {

  final case class Counter(n: Int, label: String)
  final case class App(counter: Counter, user: String)

  val n = Lens[Counter](_.n)
  val counter = Lens[App](_.counter)
  val appN = counter.andThen(n)

  /** a program written knowing only about an Int */
  def tick(by: Int): Int ! State % Int =
    for
      cur <- State.get[Int]
      _ <- State.set(cur + by)
      out <- State.get[Int]
    yield out

  test("zoom: a program over the part runs over the whole, and touches nothing else") {
    val app = App(Counter(1, "hits"), "ada")
    val (after, out) = (State.handle(app)(State.zoom[App, Int, Int, State % Int](appN)(tick(41)))).run
    assertEquals(out, 42)
    assertEquals(after, App(Counter(42, "hits"), "ada"))
    // the label and the user are untouched: the lens is the whole story
    assertEquals(after.counter.label, app.counter.label)
    assertEquals(after.user, app.user)
  }

  test("zoom composes with the optic, not with itself: one lens, two levels") {
    val app = App(Counter(0, "x"), "bob")
    // the same program, zoomed by a composed lens
    val prog = State.zoom[App, Int, Int, State % Int](counter.andThen(n))(tick(5))
    assertEquals((State.handle(app)(prog)).run._1, App(Counter(5, "x"), "bob"))
    // and zoomed by a lens onto a different part of the same whole
    val label = counter.andThen(Lens[Counter](_.label))
    val rename: String ! State % String = State.set("hits")
    assertEquals((State.handle(app)(State.zoom[App, String, String, State % String](label)(rename))).run._1,
      App(Counter(0, "hits"), "bob"))
  }

  test("zoom leaves the rest of the row alone: a Writer beside the State passes through") {
    val app = App(Counter(0, "x"), "ada")
    // the effects are named by hand: `at` is how a program needing one
    // effect is seen as needing more
    type Row = [X] =>> (State % Int + Writer % String)[X]
    def tell(s: String): Unit ! Row = Writer.tell[String](s).at[Row]
    val prog: Int ! Row =
      tell("before")
        .flatMap(_ => tick(7).at[Row])
        .flatMap(v => tell(s"after $v").map(_ => v))
    val zoomed: Int ! State % App + Writer % String = State.zoom[App, Int, Int, Row](appN)(prog)
    val (told, (after, v)) = (Writer.run(State.handle(app)(zoomed))).run
    assertEquals(v, 7)
    assertEquals(after, App(Counter(7, "x"), "ada"))
    assertEquals(told.toList, List("before", "after 7"))
  }

  test("zoom of a program that does nothing to the state is the identity on it") {
    val app = App(Counter(3, "x"), "ada")
    val pure: Int ! State % Int = okay.pure(9)
    val (after, v) = (State.handle(app)(State.zoom[App, Int, Int, State % Int](appN)(pure))).run
    assertEquals(v, 9)
    assertEquals(after, app)
  }

  test("PState.zoom: the four-parameter lens zooms the parameterised state — the type CHANGES with it") {
    // the part goes String -> Int, so the whole must: the lens says so,
    // and nothing else in the program mentions the whole at all
    final case class Box[A](item: A, tag: String)
    val item: Lens[Box[String], Box[Int], String, Int] =
      Lens(_.item, (b, i) => Box(i, b.tag))

    // a typestate program over the PART: read a String, leave an Int
    def parse[R]: Cps[Int, Int => R, String => R] =
      PState.get[String, R].flatMap(s => PState.set[String, Int, R](s.length).map(_ => s.length))

    val (after, out) = PState.run[Box[String], Box[Int], Int](Box("hello", "t"))(
      PState.zoom[Box[String], Box[Int], String, Int, Int, (Box[Int], Int)](item)(parse))
    assertEquals(out, 5)
    assertEquals(after, Box(5, "t"))
    // the tag rode along untouched, which is what the lens's `set` promised
    assertEquals(after.tag, "t")
  }

  test("PState.Threaded.zoom: the same type-changing lens on the threaded road, no nested run") {
    final case class Box[A](item: A, tag: String)
    val item: Lens[Box[String], Box[Int], String, Int] =
      Lens(_.item, (b, i) => Box(i, b.tag))
    val parse: PState.Threaded[Int, Int, String] =
      PState.Threaded.get[String].flatMap(s => PState.Threaded.put[String, Int](s.length).map(_ => s.length))
    assertEquals(PState.Threaded.run(PState.Threaded.zoom(item)(parse))(Box("hello", "t")), (Box(5, "t"), 5))
  }

  test("PState.zoom: misusing the state's new type does not compile") {
    val e = compileErrors("""
      final case class Box[A](item: A, tag: String)
      val item: okay.Lens[Box[String], Box[Int], String, Int] =
        okay.Lens(_.item, (b, i) => Box(i, b.tag))
      // the inner program leaves an Int; asking for the whole back as
      // Box[String] is the typestate error the lens exists to catch
      def parse[R]: okay.Cps[Int, Int => R, String => R] =
        okay.std.PState.get[String, R].flatMap(s => okay.std.PState.set[String, Int, R](s.length).map(_ => s.length))
      okay.std.PState.run[Box[String], Box[String], Int](Box("hello", "t"))(
        okay.std.PState.zoom[Box[String], Box[Int], String, Int, Int, (Box[String], Int)](item)(parse))
    """)
    assert(e.nonEmpty, "the misused typestate compiled")
  }
}
